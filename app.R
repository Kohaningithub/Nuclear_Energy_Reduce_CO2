library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)
library(scales)
library(ggrepel)

# Load and prepare data
nuclear_data <- read_csv("data/nuclear_power_generation_per_person.csv")
electricity_data <- read_csv("data/electricity_use_per_person.csv")
co2_data <- read_csv("data/co2_pcap_cons.csv")

# Convert all columns to numeric except for country
nuclear_data <- nuclear_data %>%
    mutate(across(-country, as.numeric)) 

electricity_data <- electricity_data %>%
    mutate(across(-country, as.numeric))

co2_data <- co2_data %>%
    mutate(across(-country, as.numeric))

# Convert to long format
nuclear_long <- nuclear_data %>%
    pivot_longer(cols = -country, names_to = "year", values_to = "nuclear_per_person")

electricity_long <- electricity_data %>%
    pivot_longer(cols = -country, names_to = "year", values_to = "electricity_per_person")

co2_long <- co2_data %>%
    pivot_longer(cols = -country, names_to = "year", values_to = "co2_per_capita")

# Combine the data
combined_data <- nuclear_long %>%
    inner_join(electricity_long, by = c("country", "year")) %>%
    inner_join(co2_long, by = c("country", "year")) %>%
    mutate(year = as.numeric(year))

# Calculate nuclear percentage of total electricity
combined_data <- combined_data %>%
    mutate(nuclear_percentage = (nuclear_per_person / electricity_per_person) * 100)

# Get list of countries for selection
countries <- sort(unique(combined_data$country))
years <- sort(unique(combined_data$year))
max_year <- max(years, na.rm = TRUE)
min_year <- min(years, na.rm = TRUE)

# UI
ui <- navbarPage(
    title = "The Role of Nuclear Energy in Reducing Carbon Emissions",
    theme = shinytheme("flatly"),
    
    # Introduction Tab
    tabPanel(
        "Introduction",
        fluidRow(
            column(
                width = 10, offset = 1,
                h2("The Role of Nuclear Energy in Reducing Carbon Emissions"),
                p("This interactive visualization explores the relationship between nuclear energy production and carbon emissions across different countries and time periods."),
                p("As the world grapples with climate change, understanding how different energy sources impact carbon emissions is crucial for developing effective policies. Nuclear energy, despite controversies, produces minimal direct carbon emissions during operation."),
                p("This analysis examines three key metrics:"),
                tags$ul(
                    tags$li(strong("Nuclear Power Generation per Person"), " - measured in tonnes of oil equivalent (toe) per person"),
                    tags$li(strong("Electricity Use per Person"), " - total electricity consumption per capita"),
                    tags$li(strong("CO2 Emissions per Capita"), " - measured in tonnes")
                ),
                p("Navigate through the tabs to explore different aspects of the data and discover insights about how nuclear energy adoption relates to carbon emissions patterns worldwide."),
                h3("Key Questions:"),
                tags$ol(
                    tags$li("Do countries with higher nuclear energy production have lower carbon emissions?"),
                    tags$li("How has the relationship between nuclear energy and emissions changed over time?"),
                    tags$li("Which countries have been most successful at using nuclear energy to reduce emissions?")
                ),
                    br(),
                    tags$video(
                        src = "AdobeStock_1025274185_Video_4K_Preview.mov",
                        type = "video/mp4",
                        controls = TRUE,
                        autoplay = TRUE,
                        loop = TRUE,
                        width = "100%"
                    )
            )
        )
    ),
    
    # Global Trends Tab
    tabPanel(
        "Global Trends",
        sidebarLayout(
            sidebarPanel(
                sliderInput("yearRange", "Select Year Range:",
                            min = min_year, max = max_year,
                            value = c(min_year, max_year), step = 1,
                            sep = ""),
                checkboxInput("showTrendline", "Show Trendline", TRUE),
                selectInput("highlightCountries", "Highlight Countries:",
                            choices = c("None", "Top Nuclear Producers", "Top CO2 Emitters", "G7 Countries"),
                            selected = "None"),
                width = 3
            ),
            mainPanel(
                h3("Global Relationship Between Nuclear Energy and CO2 Emissions"),
                p("This scatter plot shows the relationship between nuclear power generation per person and CO2 emissions per capita across countries."),
                p("Each point represents a country in a specific year. The size of the point indicates the total electricity consumption per person."),
                plotlyOutput("globalScatterPlot", height = "500px"),
                br(),
                p("Observations:"),
                p("Countries with higher nuclear energy production tend to have varying levels of CO2 emissions, suggesting that nuclear energy is just one factor among many that influence emissions."),
                p("The relationship between nuclear energy and emissions has evolved over time, with some countries showing significant reductions in emissions as they increased nuclear capacity.")
            )
        )
    ),
    
    # Country Comparison Tab
    tabPanel(
        "Country Comparison",
        sidebarLayout(
            sidebarPanel(
                style = "background-color: #ecf0f1; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                selectizeInput("selectedCountries", "Select Countries to Compare:",
                             choices = countries, multiple = TRUE,
                             selected = c("United States", "France", "Germany", "Japan", "China")),
                sliderInput("yearRangeCountry", "Select Year Range:",
                            min = min_year, max = max_year,
                            value = c(min_year, max_year), step = 1,
                            sep = ""),
                width = 3
            ),
            mainPanel(
                div(
                    style = "background-color: #fff; padding: 20px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                    tabsetPanel(
                        tabPanel("Nuclear vs CO2 Trends",
                                h3("Nuclear Power Generation and CO2 Emissions Over Time", style = "color: #2c3e50; border-bottom: 2px solid #18bc9c; padding-bottom: 10px;"),
                                p("This visualization shows how nuclear power generation and CO2 emissions have changed over time for selected countries."),
                                fluidRow(
                                    column(12, 
                                        h4("Nuclear Power Generation Over Time", style = "color: #18bc9c;"),
                                        plotlyOutput("nuclearTrendsPlot", height = "300px")
                                    )
                                ),
                                fluidRow(
                                    column(12, 
                                        h4("CO2 Emissions Over Time", style = "color: #18bc9c;"),
                                        plotlyOutput("co2TrendsPlot", height = "300px")
                                    )
                                )
                        ),
                        tabPanel("Nuclear Share of Electricity",
                                h3("Nuclear Energy as Percentage of Total Electricity", style = "color: #2c3e50; border-bottom: 2px solid #18bc9c; padding-bottom: 10px;"),
                                p("This chart shows what percentage of each country's electricity comes from nuclear sources."),
                                plotlyOutput("nuclearSharePlot", height = "500px")
                        )
                    )
                )
            )
        )
    ),
    
    # Data Explorer Tab
    tabPanel(
        "Data Explorer",
        fluidRow(
            column(
                width = 12,
                h3("Explore the Dataset"),
                p("This interactive table allows you to explore the full dataset used in this analysis."),
                p("You can sort, filter, and search through the data to find specific information about countries, years, or values."),
                DT::dataTableOutput("dataTable")
            )
        )
    ),
    
    # Design Choices Tab
    tabPanel(
        "Design Explanation",
        fluidRow(
            column(
                width = 10, offset = 1,
                h2("Design Choices Explanation"),
                h3("The Story"),
                p("This visualization explores the complex relationship between nuclear energy adoption and carbon emissions across different countries from 1960 to the present. The core narrative examines whether countries that embrace nuclear power tend to have lower carbon footprints, how these patterns have shifted over time, and which nations have most effectively used nuclear energy as part of their emissions reduction strategy."),
                p("The data reveals nuanced relationships rather than simple correlations. Some countries with high nuclear energy production maintain relatively high emissions due to other factors in their energy mix and industrial profile, while others have achieved significant emissions reductions alongside nuclear expansion."),
                
                h3("Graphical Form Selection"),
                tags$ul(
                    tags$li(strong("Scatter plots"), " - To show the relationship between nuclear energy and CO2 emissions, allowing users to see patterns and correlations across countries, while also showing the size of the country's electricity consumption."),
                    tags$li(strong("Line charts"), " - To visualize trends over time, showing how nuclear energy production and emissions have changed in parallel."),
                    tags$li(strong("Bar charts"), " - To compare nuclear energy's share of total electricity across countries, providing context for each country's energy mix.")
                ),
    
                
                h3("Role of Interactivity"),
                tags$ul(
                    tags$li("It allows users to explore the data at their own pace and focus on countries or time periods of interest."),
                    tags$li("Time sliders enable users to see how relationships have evolved over decades."),
                    tags$li("Country selection tools let users compare specific nations of interest."),
                    tags$li("Tooltips provide detailed information on demand, reducing visual clutter while making specific values accessible.")
                ),
                h3("Visual Design Principles"),
                p("The visualization employs several key design principles:"),
                tags$ul(
                    tags$li(" - Countries maintain the same color across different visualizations, helping users track specific nations across multiple views."),
                    tags$li(" - Each visualization includes explanatory text to help users interpret what they're seeing."),
                ),
                
                h3("Limitations and Considerations"),
                tags$ul(
                    tags$li("Correlation does not imply causation - lower emissions in countries with high nuclear energy use may be influenced by other factors."),
                    tags$li("The data doesn't account for lifecycle emissions of nuclear power (uranium mining, plant construction, etc.)."),
                    tags$li("Countries vary in their reporting methodologies and data quality."),
                    tags$li("The visualization focuses on per capita metrics, which may obscure total emissions impacts.")
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    
    # Filtered data based on year range
    filtered_data <- reactive({
        combined_data %>%
            filter(year >= input$yearRange[1] & year <= input$yearRange[2])
    })
    
    # Country filtered data
    country_filtered_data <- reactive({
        combined_data %>%
            filter(country %in% input$selectedCountries) %>%
            filter(year >= input$yearRangeCountry[1] & year <= input$yearRangeCountry[2])
    })
    
    # Global Scatter Plot
    output$globalScatterPlot <- renderPlotly({
        # Get filtered data
        data <- filtered_data()
        
        # Check if data is empty
        if(nrow(data) == 0) {
            return(NULL)
        }
        
        # Determine which countries to highlight
        highlighted_countries <- c()
        if(input$highlightCountries == "Top Nuclear Producers") {
            # Get top 5 nuclear producers in the most recent year
            recent_year <- max(data$year)
            top_nuclear <- data %>%
                filter(year == recent_year) %>%
                arrange(desc(nuclear_per_person)) %>%
                head(5) %>%
                pull(country)
            highlighted_countries <- top_nuclear
        } else if(input$highlightCountries == "Top CO2 Emitters") {
            # Get top 5 CO2 emitters in the most recent year
            recent_year <- max(data$year)
            top_emitters <- data %>%
                filter(year == recent_year) %>%
                arrange(desc(co2_per_capita)) %>%
                head(5) %>%
                pull(country)
            highlighted_countries <- top_emitters
        } else if(input$highlightCountries == "G7 Countries") {
            highlighted_countries <- c("United States", "Canada", "France", "Germany", 
                                      "Italy", "Japan", "United Kingdom")
        }
        
        # Create a new column for highlighting
        data$highlight <- ifelse(data$country %in% highlighted_countries, "Highlighted", "Not Highlighted")
        
        # Create a basic scatter plot using ggplot2 first, then convert to plotly
        g <- ggplot(data, aes(x = nuclear_per_person, y = co2_per_capita)) +
            geom_point(aes(size = electricity_per_person, 
                          color = highlight,
                          text = paste("Country:", country, 
                                      "<br>Year:", year,
                                      "<br>Nuclear Power:", round(nuclear_per_person, 2), "toe per person",
                                      "<br>CO2 Emissions:", round(co2_per_capita, 2), "tonnes per capita",
                                      "<br>Electricity Use:", round(electricity_per_person, 2), "kWh per person")),
                       alpha = 0.5) +
            scale_color_manual(values = c("Not Highlighted" = "#7f7f7f", "Highlighted" = "#18bc9c")) +
            scale_size(range = c(0.2, 7)) +
            labs(x = "Nuclear Power Generation (toe per person)",
                 y = "CO2 Emissions per Capita (tonnes)") +
            theme_minimal() +
            theme(
                panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA),
                legend.position = "bottom"
            )
        
        # Add trendline if selected
        if(input$showTrendline) {
            g <- g + geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", size = 1)
        }
        
        # Convert to plotly
        p <- ggplotly(g, tooltip = "text") %>%
            layout(legend = list(orientation = "h", y = -0.2))
        
        return(p)
    })
    
    # Country Trends Plot - Nuclear Power
    output$nuclearTrendsPlot <- renderPlotly({
        data <- country_filtered_data()
        
        if(nrow(data) == 0) {
            return(NULL)
        }
        
        # Create a custom color palette
        n_countries <- length(unique(data$country))
        custom_colors <- colorRampPalette(c("#18bc9c", "#2c3e50", "#e74c3c", "#f39c12", "#3498db"))(n_countries)
        
        # Create a plot for nuclear power
        plot_ly(data = data, x = ~year, y = ~nuclear_per_person, color = ~country,
                colors = custom_colors,
                type = "scatter", mode = "lines+markers",
                hoverinfo = "text",
                text = ~paste("Country:", country,
                             "<br>Year:", year,
                             "<br>Nuclear Power:", round(nuclear_per_person, 2), "toe per person")) %>%
            layout(
                xaxis = list(title = "Year"),
                yaxis = list(title = "Nuclear Power Generation (toe per person)"),
                legend = list(orientation = "h", y = -0.2),
                hovermode = "closest",
                paper_bgcolor = 'rgba(0,0,0,0)',
                plot_bgcolor = 'rgba(0,0,0,0)'
            )
    })
    
    # Country Trends Plot - CO2 Emissions
    output$co2TrendsPlot <- renderPlotly({
        data <- country_filtered_data()
        
        if(nrow(data) == 0) {
            return(NULL)
        }
        
        # Create a custom color palette
        n_countries <- length(unique(data$country))
        custom_colors <- colorRampPalette(c("#18bc9c", "#2c3e50", "#e74c3c", "#f39c12", "#3498db"))(n_countries)
        
        # Create a plot for CO2 emissions
        plot_ly(data = data, x = ~year, y = ~co2_per_capita, color = ~country,
                colors = custom_colors,
                type = "scatter", mode = "lines+markers",
                hoverinfo = "text",
                text = ~paste("Country:", country,
                             "<br>Year:", year,
                             "<br>CO2 Emissions:", round(co2_per_capita, 2), "tonnes per capita")) %>%
            layout(
                xaxis = list(title = "Year"),
                yaxis = list(title = "CO2 Emissions per Capita (tonnes)"),
                legend = list(orientation = "h", y = -0.2),
                hovermode = "closest",
                paper_bgcolor = 'rgba(0,0,0,0)',
                plot_bgcolor = 'rgba(0,0,0,0)'
            )
    })
    
    # Nuclear Share Plot
    output$nuclearSharePlot <- renderPlotly({
        data <- country_filtered_data()
        
        if(nrow(data) == 0) {
            return(NULL)
        }
        
        # Instead of using only the most recent year, calculate average over the selected year range
        summary_data <- data %>%
            group_by(country) %>%
            summarize(
                nuclear_percentage = mean(nuclear_percentage, na.rm = TRUE),
                nuclear_per_person = mean(nuclear_per_person, na.rm = TRUE),
                electricity_per_person = mean(electricity_per_person, na.rm = TRUE),
                min_year = min(year),
                max_year = max(year)
            ) %>%
            ungroup()
        
        # Create bar chart of nuclear percentage
        p <- ggplot(summary_data, aes(x = reorder(country, nuclear_percentage), 
                                    y = nuclear_percentage,
                                    fill = country,
                                    text = paste("Country:", country,
                                               "<br>Year Range:", min_year, "-", max_year,
                                               "<br>Nuclear Share (avg):", round(nuclear_percentage, 4), "%",
                                               "<br>Nuclear Power (avg):", round(nuclear_per_person, 4), "toe per person",
                                               "<br>Total Electricity (avg):", round(electricity_per_person, 2), "kWh per person"))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "",
                 y = "Nuclear Energy as % of Total Electricity",
                 title = paste("Nuclear Share of Electricity (", input$yearRangeCountry[1], "-", input$yearRangeCountry[2], ")", sep = "")) +
            theme_minimal() +
            theme(legend.position = "none")
        
        ggplotly(p, tooltip = "text")
    })
    
    # Data Table
    output$dataTable <- renderDT({
        combined_data %>%
            select(country, year, nuclear_per_person, electricity_per_person, co2_per_capita, nuclear_percentage) %>%
            rename(
                "Country" = country,
                "Year" = year,
                "Nuclear Power (toe per person)" = nuclear_per_person,
                "Electricity Use (kWh per person)" = electricity_per_person,
                "CO2 Emissions (tonnes per capita)" = co2_per_capita,
                "Nuclear Share (%)" = nuclear_percentage
            ) %>%
            datatable(options = list(
                pageLength = 10,
                scrollX = TRUE,
                order = list(list(1, 'desc'), list(0, 'asc'))
            )) %>%
            formatRound(columns = c("Nuclear Power (toe per person)", 
                                   "Electricity Use (kWh per person)", 
                                   "CO2 Emissions (tonnes per capita)",
                                   "Nuclear Share (%)"), digits = 2)
    })
}

# Run the app
shinyApp(ui = ui, server = server) 