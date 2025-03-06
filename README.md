# The Role of Nuclear Energy in Reducing Carbon Emissions

Here is the link to the Shiny app: https://kohanchen.shinyapps.io/Nuclear_Energy_Role_in_Carbon_Emission/

This repository contains an interactive Shiny application that explores the relationship between nuclear energy production and carbon emissions across different countries and time periods.

## Overview

This visualization project examines three key metrics:
- Nuclear Power Generation per Person (measured in tonnes of oil equivalent per person)
- Electricity Use per Person (total electricity consumption per capita)
- CO2 Emissions per Capita (measured in tonnes)

The application allows users to explore global trends, compare countries, and examine how nuclear energy adoption relates to carbon emissions patterns worldwide.

## Features

- **Global Trends**: Interactive scatter plot showing the relationship between nuclear power generation and CO2 emissions across countries
- **Country Comparison**: Tools to compare nuclear power generation and CO2 emissions trends over time for selected countries
- **Nuclear Share Analysis**: Visualization of what percentage of each country's electricity comes from nuclear sources
- **Data Explorer**: Interactive table for exploring the full dataset

## Data Sources

The data used in this application is based on free material from GAPMINDER.ORG, CC-BY LICENSE.
- Nuclear power generation per person (data/nuclear_power_generation_per_person.csv)
- Electricity use per person (data/electricity_use_per_person.csv)
- CO2 emissions per capita (data/co2_pcap_cons.csv)

## Project Structure

- `app.R`: The main Shiny application code
- `data/`: Directory containing the datasets
- `run_app.R`: Run this script to run the Shiny app
- `www/`: Directory for the video

## Design Explanation

A detailed explanation of the design choices made in this visualization can be found in the "Design Explanation" tab of the application.
