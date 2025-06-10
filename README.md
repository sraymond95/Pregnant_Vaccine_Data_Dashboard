# ------------------------------------------------------------------------------
# Title: Vaccination Coverage Among Pregnant Women - Shiny Dashboard
# Author: Shailla Raymond
# Description: Interactive dashboard for exploring CDC vaccination coverage 
#              data among pregnant women. Includes plots, summary tables, 
#              time trends, and interactive mapping by geography and demographic.
# 
# Required Packages: shiny, tidyverse, janitor, readr, DT, ggplot2, maps, sf, 
#                    tigris, leaflet
# ------------------------------------------------------------------------------

# Vaccination Coverage Among Pregnant Women - Shiny Dashboard

This Shiny dashboard explores CDC data on vaccination coverage among pregnant women in the U.S. It provides visualizations and interactive tools to filter by year, vaccine type, geography, and demographic dimensions.

## 📊 Features

- Filterable bar plots of vaccine coverage by state and demographic groups
- Downloadable summary data table
- Time trend visualization across multiple years
- Static and interactive maps of U.S. state coverage
- Clean, modular UI using `shiny`, `leaflet`, and `ggplot2`

## 🧰 Technologies Used

- `shiny` – for building the dashboard
- `tidyverse`, `janitor` – for data wrangling
- `DT` – for interactive tables
- `ggplot2`, `sf`, `tigris` – for plotting and spatial mapping
- `leaflet` – for interactive map rendering

## 📂 Files

- `pregnant_vaccine_coverage_app.R`: Main Shiny app script
- `pregnant_vaccine_coverage.csv`: Dataset (found on CDC Database)


## 📦 Installation

To run the app locally:

```r
# Install required packages if needed
install.packages(c("shiny", "tidyverse", "janitor", "readr", "DT", "ggplot2", "maps", "sf", "tigris", "leaflet"))

# Run the app
shiny::runApp("pregnant_vaccine_coverage_app.R")
