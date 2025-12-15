library(bslib)
library(here)
## library(ptaxsim)
library(scales)
library(shiny)
library(sf)
library(tidyverse)

###############
## Constants ##
###############

year <- 2023

classes <- read_csv(here("data/class-descriptions.csv")) |>
    mutate_at("class", as.factor)

## Alt: query_db()
all_pins <- read_csv("data/zoning_pins.csv") |>
    select(class, cash_value, location) |>
    mutate_at("class", as.factor)

locations <- all_pins |>
    distinct(location) |>
    arrange(location)

######################
## Helper Functions ##
######################

query_db <- function(year=2023) {
    ## We grab the neighborhood bounds from the City Data Portal
    neighborhood_bounds <- st_read("https://data.cityofchicago.org/resource/y6yq-dbs2.geojson")

    sf_use_s2(FALSE)
    municipality_bounds <- st_read("data/Municipality.geojson")

    ## See https://prodassets.cookcountyassessoril.gov/s3fs-public/form_documents/Class_codes_definitions_12.16.24.pdf
    residential_regression <- c(200, 201, 202, 203, 204, 205, 206, 207,
                                208, 209, 210, 211, 212, 234, 278, 295)
    residential_nonregression <- c(200, 201, 213, 218, 219, 225, 299)
    multi_family <- c(313, 314, 315, 318, 391, 396, 399)

    ## Set up the DB connection
    ## residential_pins <- DBI::dbGetQuery(
    ##          ptaxsim_db_conn, str_glue("
    ## SELECT p.year, p.pin, p.class, p.av_certified, pg.longitude, pg.latitude
    ## FROM pin p
    ## INNER JOIN pin_geometry pg
    ## ON substr(p.pin, 1, 10) = pg.pin10
    ## AND p.year = pg.year
    ## WHERE p.year = {year}
    ## ")
    ## ) |>
    ##     ## Include only residental buildings
    ##     filter(class %in% residential_regression |  
    ##            class %in% residential_nonregression |
    ##            class %in% multi_family) |>
    ##     ## Map lat/lon to point
    ##     st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    ##     ## Add in geographic data
    ##     st_join(neighborhood_bounds, st_within) |>
    ##     st_join(municipality_bounds, st_within) |>
    ##     st_drop_geometry() |>
    ##     as_tibble() |>
    ##     ## Apply residential multiplier
    ##     mutate(cash_value = av_certified * 10,
    ##            location = ifelse(is.na(pri_neigh), MUNICIPALITY, pri_neigh),
    ##            .keep = "unused") |>
    ##     select(class, cash_value, location) |>
    ##     mutate_at("class", as.factor)

    ## DBI::dbDisconnect(ptaxsim_db_conn)
    
    all_pins <- read_csv("data/zoning_pins.csv") |>
        select(class, cash_value, location)

    return(all_pins)
}

## Given all Cook County, a ward number, and the geojson ward data
## return a subset of the property data pertaining only to that ward
get_location <- function(all_pins, location_name) {
    all_pins |>
        filter(location == location_name)
}

#####################
## Shiny Functions ##
#####################

ui <- page_sidebar(
    title = "Chicago Real Estate Explorer",

    sidebar = sidebar(
        tags$a(href="https://charles-threlkeld-cook-county-real-estate-explorer.share.connect.posit.cloud/classApp.html", "See here for a motivating example."),

        selectInput(
            "location1", label = "First Neighborhood / Suburb",
            choices = locations),

        selectInput(
            "location2", label = "Second Neighborhood / Suburb",
            choices = locations),

        checkboxGroupInput(
            "checkGroup",
            "Select Property Classes for Graph",
            choices = classes$class,
            selected = c("211")
        )

    ),

    card(
        card_header("Location Comparison"),
        plotOutput("locComparison")
    ),

    card(
        tableOutput("table")
    )
)

server <- function(input, output) {

    loc_df <- reactive({
        all_pins |>
            filter(location == input$location1 | location == input$location2)
    })

    class_df <- reactive({
        loc_df() |>
            filter(class %in% input$checkGroup)
    })
    
    output$locComparison <- renderPlot({
        ggplot(class_df(), aes(cash_value, fill = location)) +
            geom_histogram(
                alpha=0.8,
                position = "dodge") +
            scale_y_continuous(
                "Number of Properties",
                labels = label_number_auto()) +
            scale_x_continuous(
                "Fair Cash Value",
                labels = label_currency(prefix="$", scale_cut=cut_short_scale()),
                n.breaks = 12,
                limits=c(0,2000000)) +
            labs(
                title = str_glue("Cash Value of Properties in {year}"))
    })

    output$table <- renderTable(
        loc_df() |>
        group_by(class, location) |>
        summarize(count = n()) |>
        arrange(desc(count)) |>
        pivot_wider(names_from = location,
                    values_from = count,
                    values_fill = 0) |>
        left_join(classes)
    )
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 4044))
