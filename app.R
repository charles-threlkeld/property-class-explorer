library(bslib)
library(here)
## library(ptaxsim)
library(shiny)
library(sf)
library(tidyverse)

###############
## Constants ##
###############

year <- 2023

## See https://prodassets.cookcountyassessoril.gov/s3fs-public/form_documents/Class_codes_definitions_12.16.24.pdf
residential_regression <- c(200, 201, 202, 203, 204, 205, 206, 207,
                            208, 209, 210, 211, 212, 234, 278, 295)
residential_nonregression <- c(200, 201, 213, 218, 219, 225)
multi_family <- c(313, 314, 315, 318, 391, 396, 399)

classes <- read_csv(here("data/class-descriptions.csv"))

ward_bounds <- st_read("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson")


######################
## Helper Functions ##
######################

query_db <- function(year=2023) {
    ##     ## Set up the DB connection
    ##     ptaxsim_db_conn <- DBI::dbConnect(
    ##                                 RSQLite::SQLite(),
    ##                                 here("data/ptaxsim.db")
    ##                             )
    
    ##     all_pins <- DBI::dbGetQuery(
    ##                          ptaxsim_db_conn, str_glue("
    ## SELECT p.year, p.pin, p.class, p.av_certified, p.tax_code_num, pg.longitude, pg.latitude
    ## FROM pin p
    ## INNER JOIN pin_geometry pg
    ## ON substr(p.pin, 1, 10) = pg.pin10
    ## AND p.year = pg.year
    ## WHERE substr(p.tax_code_num, 1, 1) = '7'
    ## AND p.year = {year}
    ## ")
    ## ) |>
    ##     filter(class %in% residential_regression |                    # Include only residental buildings
    ##            class %in% residential_nonregression |
    ##            class %in% multi_family)

    all_pins <- read_csv("data/residential_pins.csv") |>
        mutate(class = as.factor(class)) |>
        mutate(av_certified = as.numeric(av_certified))

    all_pins <-
        all_pins |>
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>  # Map lat/lon to point
        mutate(cash_value = av_certified * 10)                        # Apply residential multiplier
    
    ## DBI::dbDisconnect(ptaxsim_db_conn)

    return(all_pins)
}

## Given all Cook County, a ward number, and the geojson ward data
## return a subset of the property data pertaining only to that ward
get_ward <- function(all_pins, ward_num) {
    ## https://app.chicagoelections.gov/documents/general/Citywide%20Ward%20Map%202022.pdf
    ward_bound <- ward_bounds |>
        filter(ward == as.character(ward_num))
    all_pins |>
        filter(as.logical(st_within(geometry, ward_bound)))
}



#####################
## Shiny Functions ##
#####################

ui <- page_sidebar(
    title = "Chicago Real Estate Explorer",

    sidebar = sidebar(
        helpText("Select a ward."),

        sliderInput(
            "ward1", label = "First Ward",
            min = 1, max = 50, value = 47),
        
        sliderInput(
            "ward2", label = "Second Ward",
            min = 1, max = 50, value = 40),

        sliderInput(
            "nbins",
            label = "Number of bins:",
            min = 1,
            max = 200,
            value = 30
        ),
        
        sliderInput(
            "range",
            label = "Range of interest ($):",
            min = 0, 
            max = 4000000, 
            value = c(200000, 1000000)
        ),
        
        checkboxGroupInput(
            "checkGroup",
            "Select all that apply",
            choices = classes$class,
            selected = c("211")
        )

    ),

    card(
        card_header("Assesed Valuation History"),
        plotOutput("wardHist")
    ),

    card(
        tableOutput("table")
    )
)

server <- function(input, output) {

    residential_df <- query_db(year)

    ward1_df <- reactive({
        residential_df |>
            get_ward(input$ward1) |>
            mutate(ward = input$ward1)
    })

    ward2_df <- reactive({
        residential_df |>
            get_ward(input$ward2) |>
            mutate(ward = input$ward2)
    })

    combined_df <- reactive({
        if (input$ward1 == input$ward2) {
            ward1_df() |>
                mutate_at("ward", as.factor)
        } else {
            rows_append(ward1_df(), ward2_df()) |>
                mutate_at("ward", as.factor)
        }
    })

    class_subset_df <- reactive({
        combined_df() |>
            filter(class %in% input$checkGroup)
    })
    
    median_value <- reactive({
        median(class_subset_df()$cash_value)
    })

    output$wardHist <- renderPlot({
        ggplot(class_subset_df(), aes(cash_value, fill = ward)) +
            geom_histogram(
                bins=input$nbins,
                alpha=0.8,
                position = "dodge") +
            geom_vline(
                aes(xintercept = median_value()), linewidth = 1) +
            scale_y_continuous(
                "Number of Properties",
                labels = scales::label_number_auto()) +
            scale_x_continuous(
                "Fair Cash Value",
                labels = scales::label_currency(prefix="$"),
                n.breaks = 12,
                limits = c(input$range[1], input$range[2])) +
            labs(
                title = str_glue("Cash Value of Properties in {year}"))
    })

    output$table <- renderTable(
        combined_df() |>
        st_drop_geometry() |>
        group_by(class, ward) |>
        summarize(count = n()) |>
        pivot_wider(names_from = ward,
                    names_prefix = "Ward ",
                    values_from = count,
                    values_fill = 0) |>
        merge(classes) |>
        arrange(desc(pick(starts_with("Ward"))))
    )
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 4044))
