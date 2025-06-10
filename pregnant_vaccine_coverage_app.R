library(shiny) #creates app/dashboard
library(tidyverse)
library(janitor) #Clean data
library(readr)
library(DT) #HTML data table
library(ggplot2)
library(maps)
library(sf)
library(tigris) #shapefile
library(leaflet) #interactive map

# Load and clean the data
vax <- read_csv("pregnant_vaccine_coverage.csv") %>%
    clean_names() %>%
    mutate(
        estimate_percent = if_else(str_detect(estimate_percent, "^\\d"), 
                                   as.numeric(estimate_percent), NA_real_),
        year = survey_year_influenza_season
    )

# Load US states shapefile
states_map <- tigris::states(cb = TRUE, year = 2022) %>% st_as_sf()

# UI
ui <- fluidPage(
    titlePanel("Vaccination Coverage Among Pregnant Women"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year:", choices = sort(unique(vax$year)), selected = max(vax$year)),
            selectInput("vaccine", "Select Vaccine:", choices = unique(vax$vaccine), selected = "Influenza"),
            selectInput("dimension_type", "Select Dimension Type:", choices = unique(vax$dimension_type)),
            uiOutput("dimension_selector"),
            selectInput("geography_type", "Select Geography Type:", choices = unique(vax$geography_type)),
            checkboxInput("sort_desc", "Sort by Coverage (High to Low)", value = TRUE),
            downloadButton("download_data", "Download Filtered Table")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("coverage_plot")),
                tabPanel("Summary Table", DTOutput("summary_table")),
                tabPanel("Time Trend", plotOutput("time_trend_plot")),
                tabPanel("Map", plotOutput("coverage_map")),
                tabPanel("Interactive Map", leafletOutput("interactive_map", height = 600))
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    
    output$dimension_selector <- renderUI({
        available_dimensions <- vax %>%
            filter(dimension_type == input$dimension_type) %>%
            pull(dimension) %>%
            unique()
        
        selectInput("dimension", "Select Dimension:", choices = available_dimensions)
    })
#Filtering Data    
    filtered_data <- reactive({
        df <- vax %>%
            filter(
                year == input$year,
                vaccine == input$vaccine,
                dimension_type == input$dimension_type,
                dimension == input$dimension,
                geography_type == input$geography_type
            )
        
        if (input$sort_desc) {
            df <- df %>% arrange(desc(estimate_percent))
        } else {
            df <- df %>% arrange(estimate_percent)
        }
        
        df
    })
#Bar Plot - Vaccine Coverage by geography    
    output$coverage_plot <- renderPlot({
        req(filtered_data())
        
        ggplot(filtered_data(), aes(x = reorder(geography, estimate_percent), y = estimate_percent, fill = dimension)) +
            geom_col() +
            coord_flip() +
            labs(
                title = paste(input$vaccine, "Coverage in", input$year),
                x = "Geography",
                y = "Coverage (%)"
            ) +
            theme_minimal()
    })
    
#Summary Table    
    output$summary_table <- renderDT({
        datatable(
            filtered_data() %>%
                select(geography, estimate_percent, sample_size, x95_percent_ci_percent),
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                rowCallback = JS(
                    "function(row, data, index) {",
                    "  $('td', row).css('background-color', function(i) {",
                    "    const colors = ['#f7fcf0', '#e0f3db', '#ccebc5', '#a8ddb5', '#7bccc4', '#4eb3d3', '#2b8cbe'];",
                    "    return colors[index % colors.length];",
                    "  });",
                    "}"
                )
            ),
            rownames = FALSE
        )
    })
#Tab to download panel    
    output$download_data <- downloadHandler(
        filename = function() {
            paste("vaccine_coverage_", input$year, ".csv", sep = "")
        },
        content = function(file) {
            write_csv(filtered_data(), file)
        }
    )
#Time Trend Plot    
    output$time_trend_plot <- renderPlot({
        vax %>%
            filter(
                vaccine == input$vaccine,
                dimension_type == input$dimension_type,
                dimension == input$dimension,
                geography_type == input$geography_type
            ) %>%
            ggplot(aes(x = year, y = estimate_percent, group = geography)) +
            geom_line(alpha = 0.3) +
            stat_summary(fun = mean, geom = "line", color = "blue", size = 1.2) +
            labs(
                title = paste("Trend of", input$vaccine, "Coverage Over Time"),
                y = "Coverage (%)",
                x = "Year"
            ) +
            theme_minimal()
    })
#Regular Map    
    output$coverage_map <- renderPlot({
        map_data <- vax %>%
            filter(
                year == input$year,
                vaccine == input$vaccine,
                dimension_type == input$dimension_type,
                dimension == input$dimension,
                geography_type == "States"
            ) %>%
            mutate(NAME = geography)
        
        plot_data <- left_join(states_map, map_data, by = "NAME")
        
        ggplot(plot_data) +
            geom_sf(aes(fill = estimate_percent), color = "white") +
            scale_fill_viridis_c(name = "Coverage %", na.value = "grey90") +
            labs(
                title = paste(input$vaccine, "Coverage by State (", input$year, ")"),
                caption = "Source: CDC Vaccine Coverage Dataset"
            ) +
            theme_minimal()
    })
#Interactive Map
    output$interactive_map <- renderLeaflet({
        map_data <- vax %>%
            filter(
                year == input$year,
                vaccine == input$vaccine,
                dimension_type == input$dimension_type,
                dimension == input$dimension,
                geography_type == "States"
            ) %>%
            mutate(NAME = geography)
        
        map_join <- left_join(states_map, map_data, by = "NAME")
        
        pal <- colorNumeric("YlOrRd", domain = map_join$estimate_percent, na.color = "#f0f0f0")
        
        leaflet(map_join) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(
                fillColor = ~pal(estimate_percent),
                color = "#444444",
                weight = 1,
                smoothFactor = 0.2,
                opacity = 1.0,
                fillOpacity = 0.7,
                highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                label = ~paste(NAME, ": ", estimate_percent, "%"),
                labelOptions = labelOptions(direction = "auto")
            ) %>%
            addLegend("bottomright", pal = pal, values = ~estimate_percent,
                      title = "Coverage %",
                      opacity = 1)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
