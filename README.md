Title: Vaccination Coverage Among Pregnant Women - Shiny Dashboard
Description: Interactive dashboard for exploring CDC vaccination coverage 
              data among pregnant women. Includes plots, summary tables, 
              time trends, and interactive mapping by geography and demographic.
 
 Required Packages: shiny, tidyverse, janitor, readr, DT, ggplot2, maps, sf, 
                    tigris, leaflet



This Shiny dashboard explores CDC data on vaccination coverage among pregnant women in the U.S. It provides visualizations and interactive tools to filter by year, vaccine type, geography, and demographic dimensions.

 📊 Features

- Filterable bar plots of vaccine coverage by state and demographic groups
- Downloadable summary data table
- Time trend visualization across multiple years
- Static and interactive maps of U.S. state coverage
- Clean, modular UI using `shiny`, `leaflet`, and `ggplot2`

 🧰 Packages Used

- `shiny` – for building the dashboard
- `tidyverse`, `janitor` – for data wrangling
- `DT` – for interactive tables
- `ggplot2`, `sf`, `tigris` – for plotting and spatial mapping
- `leaflet` – for interactive map rendering

 📂 Files

- `pregnant_vaccine_coverage_app.R`: Main Shiny app script
- `pregnant_vaccine_coverage.csv`: Dataset (found on [CDC Database](https://data.cdc.gov/))


📦 Installation

To run the app locally:

```r
# Install required packages if needed
install.packages(c("shiny", "tidyverse", "janitor", "readr", "DT", "ggplot2", "maps", "sf", "tigris", "leaflet"))

# Run the app
shiny::runApp("pregnant_vaccine_coverage_app.R")
```
Step 1. 📦 Load Required Libraries. To create this dashboard you want to have these packages loaded. Here are some packages you may not be used to. The shiny package is used as the core freamwork for building the dashboard's UI and server logic. Janitor is going to standarize the column names. DT is what we use to create the scrollable summary table tab of the dashboard. sf is used to plot shapefiles in the US. Tigris will retrieve the current US state boundaries and leaflet is to build an interactive map with labels and color scales.

```
library(shiny)
library(tidyverse)
library(janitor)
library(readr)
library(DT)
library(ggplot2)
library(sf)
library(tigris)
library(leaflet)
```

Step 2. Clean Data
```
vax <- read_csv("pregnant_vaccine_coverage.csv") %>%
  clean_names() %>%
  mutate(
    estimate_percent = if_else(str_detect(estimate_percent, "^\\d"),
                               as.numeric(estimate_percent), NA_real_),
    year = survey_year_influenza_season
  )
```

Step 3.  Load US states shapefile. This will load US state boundaries from the Census Bureau using the tigris package. It will then convert the shapefile into an sf object to support spatial plotting in ggplot2 and leaflet
```
states_map <- tigris::states(cb = TRUE, year = 2022) %>% st_as_sf()
```

Step 4. The next step is to set up the User Interface (UI) by creating a sidebar and main panel. The Sidebar will include filters for year, Vaccine type, Dimension type (e.g. Age and Race), Geography type, Sort order toggle, and a Download Button
The main panel will have tabs for plots, tbales, and maps
```
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
```
Step 5. Server
```
server <- function(input, output, session) {
    
    output$dimension_selector <- renderUI({
        available_dimensions <- vax %>%
            filter(dimension_type == input$dimension_type) %>%
            pull(dimension) %>%
            unique()
        
        selectInput("dimension", "Select Dimension:", choices = available_dimensions)
    })
```
Step 6. This section allows us to create our filters based on the sidebar inputs and it will order the results by coverage percentage
```
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
```
Step 7 Bar Plot - Vaccine Coverage by geography    
```
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
```
Step 8 Summary Table will display a sortable, scrollable data table with geography, vaccine coverage %, and sample size  
```
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
```
Step 9. Tab to download panel    
```
    output$download_data <- downloadHandler(
        filename = function() {
            paste("vaccine_coverage_", input$year, ".csv", sep = "")
        },
        content = function(file) {
            write_csv(filtered_data(), file)
        }
    )
```
Step 10 Time Trend Plot  to show vaccine coverage over time
```
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
```
Step 11Regular Map    
 ```   
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
```
Step 12. Interactive Map
```
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
```

Step 13. After running the above lines, run the bottom code and you will have your dashboard! 
```
shinyApp(ui = ui, server = server)
```
