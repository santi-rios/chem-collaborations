# =============================================================================
# Documentation for app.r - Chemical Space Collaboration Explorer Shiny App
# =============================================================================

#
# This Shiny application allows users to explore multi-country collaborations by visualizing interactive charts,
# maps, and data tables. The app reads collaboration data from a remote CSV, processes it, and presents it through
# a user-friendly interface using the 'shiny' and 'bslib' packages.

# -----------------------------------------------------------------------------
# 1) Library Imports and Data Loading
# -----------------------------------------------------------------------------
#
# The following libraries are loaded:
# - shiny: Provides the core framework for building interactive web applications.
# - bslib: Offers Bootstrap themes for a modern UI.
# - data.table: Enables efficient data manipulation and reading.
# - glue: Aids in string interpolation.
# - countrycode: Maps country codes to country names.
# - plotly: For generating interactive charts and maps.
# - DT: For rendering advanced data tables.
#
# The code then sets a remote data URL from a GitHub repository. It reads the CSV using 'fread' from data.table,
# which is an efficient function for handling large datasets.
#
# -----------------------------------------------------------------------------
# 2) Shiny UI Construction
# -----------------------------------------------------------------------------
#
# A custom Bootstrap theme is defined using 'bs_theme' with version 5 and the Morph bootswatch theme. Colors for
# primary and secondary elements are also specified.
#
# The user interface is constructed with 'page_fluid' to ensure responsiveness.
#
# Key Elements:
# - Header Section: Displays the application's title and a brief description with custom background styling.
#
# - Sidebar (Left Column): Contains two cards:
#   1. A card with selection inputs:
#      - A selectInput for choosing a collaboration group, populated with unique values from the data.
#      - A sliderInput for selecting a year within the data's range.
#
#   2. A card with a collaboration summary:
#      - The summaryText output shows information about the collaboration.
#      - The flagButtons output provides clickable flag icons that allow users to get country-specific details.
#
# - Main Panel (Right Column): Contains a card with two tabs:
#   1. "Charts" Tab:
#      - plotlyOutput for a line chart showing trends over time.
#      - plotlyOutput for a map displaying geographical locations of collaborations.
#      - An area (htmlOutput) to display detailed country information.
#
#   2. "Data Table" Tab:
#      - DTOutput presents the collaboration data in a user-friendly, paginated, searchable data table.
#
# - Footer: Provides the data source with a hyperlink directing users to the original study.

# -----------------------------------------------------------------------------
# 3) Shiny Server Logic
# -----------------------------------------------------------------------------
#
# The server function encapsulates the reactive application logic.
#
# Key Components:
#
# - Reactive Inputs:
#   - 'selected_year': Uses debounce to delay reactions to rapid slider movements, helping performance.
#   - 'collab_data': Filters the main data frame based on the chosen collaboration group.
#
# - Output renderers:
#
#   1. output$summaryText:
#      - Constructs a dynamic summary using HTML that shows the collaboration group's details.
#      - Splits the 'iso2c' column into individual country codes, converts these using 'countrycode' to names,
#        and creates clickable flag buttons. These buttons use inline JavaScript (via Shiny.setInputValue) to send
#        the selected country back to the server.
#
#   2. output$collabTable:
#      - Uses the DT package to render a data table showing the Year and a rounded Percentage value for the collaboration.
#
#   3. output$collabMap:
#      - Generates an interactive Plotly map.
#      - It filters the data using the throttled selected year, splits the 'iso2c' column for individual country markers,
#        and maps these markers to display the collaboration's geographic spread.
#
#   4. output$collabLinePlot:
#      - Produces an interactive line chart using Plotly.
#      - The chart visualizes a time series of collaboration 'Value' over the years, highlighting the selected collaboration group.
#      - Incorporates hover text to provide detailed insight on data points.
#
# - Finally, the 'shinyApp(ui, server)' call instantiates and launches the Shiny app.
#
# =============================================================================
# End of Documentation.
library(shiny)
library(bslib)
library(data.table)
library(glue)
library(countrycode)
library(plotly)
library(DT)  # DataTable for better tables

# Load Data Efficiently
# data_url <- "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/collabs_df_ds.csv"
data_url <- "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/collabs_sample.csv"


df <- fread(data_url)  # Efficiently read CSV, omitting NAs

################################################
## 2) Shiny UI Construction
################################################
app_theme <- bs_theme(
  version = 5,
  bootswatch = "morph",
  primary = "#2c3e50",
  secondary = "#aeddd4"
#   base_font = font_google("Roboto Mono"),
#   heading_font = font_google("Roboto Condensed")
)

ui <- page_fluid(
  theme = app_theme,
  
  tags$div(
    style = "background-color: #2c3e50; padding: 20px; border-radius: 4px; margin-bottom: 20px;",
    tags$h1("Collaboration Explorer", style = "color: #fff; text-align: center; margin: 0;"),
    tags$p("Explore multi-country collaborations over time.",
           style = "color: #eee; text-align: center; margin: 0;")
  ),
  
  fluidRow(
    column(
      width = 4,
      card(
        style = "margin-bottom: 20px;",
        card_header("Select a Collaboration Group and a Year", class = "bg-primary text-white"),
        card_body(
          selectInput(
            inputId  = "collabSelector",
            label    = "Search Collaboration Group:",
            choices  = sort(unique(df$CollabGroup)),
            selected = "China-United States",
            multiple = FALSE,
            width    = "100%",
            selectize = TRUE
          ),
          sliderInput(
            inputId = "year", 
            label   = "Year",
            min     = min(df$Year, na.rm = TRUE),
            max     = max(df$Year, na.rm = TRUE),
            value   = max(df$Year, na.rm = TRUE) - 1,
            step    = 1,
            ticks = FALSE,
            animate = FALSE,
            width   = "100%"
          )
        )
      ),
      
      card(
        style = "margin-bottom: 20px;",
        card_header("Collaboration Summary (Click Flag for Details)", class = "bg-primary text-white"),
        card_body(
          htmlOutput("summaryText"),
          uiOutput("flagButtons")  
        )
      )
    ),
    
    column(
      width = 8,
      card(
        style = "margin-bottom: 20px;",
        full_screen = TRUE,
        card_header("Interactive Visualizations", class = "bg-primary text-white"),
        card_body(
          tabsetPanel(
            tabPanel("Charts",
              plotlyOutput("collabLinePlot", height = "50vh"),
              tags$hr(),
              plotlyOutput("collabMap", height = "45vh"),
              htmlOutput("countryDetails")
            ),
            tabPanel("Data Table",
              DTOutput("collabTable")  # Improved DataTable
            )
          )
        )
      )
    )
  ),
  
  tags$footer(
    style = "background-color: #f8f9fa; padding: 15px; margin-top: 20px; border-top: 1px solid #ddd;",
    tags$div(class = "text-center", 
             "Data source: ", 
             tags$a(href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6", 
                    "China's rise in the chemical space and the decline of US influence", target = "_blank")) 
  )
)

################################################
## 3) Shiny Server Logic
################################################
server <- function(input, output, session) {
  selected_year <- debounce(reactive(input$year), 300)
  
  collab_data <- reactive({
    df[CollabGroup == input$collabSelector]
  })
  
  output$summaryText <- renderUI({
    data_subset <- collab_data()
    if (nrow(data_subset) == 0) return("No data for this collaboration group.")
    
    all_iso <- unique(unlist(strsplit(data_subset$iso2c, "-")))
    country_names <- countrycode(all_iso, "iso2c", "country.name", warn = FALSE)
    
    flag_urls <- paste0('<img src="https://flagcdn.com/16x12/', tolower(all_iso), '.png" width="16">')
    
    buttons <- paste0(
      '<button type="button" class="btn btn-outline-secondary btn-sm" onclick="Shiny.setInputValue(\'selectedCountry\', \'', all_iso, '\');">',
      flag_urls, " ", country_names, "</button>"
    )
    
    HTML(glue("Collaboration '{input$collabSelector}' involves {length(all_iso)} countries. 
              Data spans from {min(data_subset$Year)} to {max(data_subset$Year)}.<br> 
              <b>Countries:</b> {paste(buttons, collapse=' ')}"))
  })
  
  output$collabTable <- renderDT({
    datatable(
      collab_data()[, .(Year, Percentage = round(Percentage, 3))],
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
output$collabMap <- renderPlotly({
  req(collab_data())
  max_val <- max(collab_data()$Value, na.rm = TRUE)
  map_subset <- collab_data()[Value == max_val]

  # Split out iso2c and iso3c for each row
  map_expanded <- map_subset[, .(
    iso2c = unlist(strsplit(iso2c, "-")),
    iso3c = unlist(strsplit(iso3c, "-")),
    Value = rep(Value, sapply(strsplit(iso2c, "-"), length)),
    Year = rep(Year, sapply(strsplit(iso2c, "-"), length))
  )]

  # Use three-letter codes (iso3c) for plotting
  plot_geo(map_expanded, locationmode = "ISO-3") %>%
    add_trace(
      locations = ~iso3c,
      z = ~Value,
      color = ~Value,
      text = ~paste("Year:", Year, "<br>Value:", Value),
      hoverinfo = "text"
    ) %>%
    layout(
      title = paste("Map for Collaboration Max Value =", max_val, 
                    "| Year =", unique(map_expanded$Year)),
      geo = list(scope = "world")
    )
})
  output$collabLinePlot <- renderPlotly({
  data_subset <- collab_data()
  if (nrow(data_subset) == 0) return(plotly_empty())

  # Identify unique sources
  sources <- unique(data_subset$source)
  plot_list <- list()

  # Create a separate plotly bar chart for each source
  for (src in sources) {
    sub_df <- data_subset[source == src]
    p <- plot_ly(
      data = sub_df,
      x = ~Value,
      y = ~as.factor(Year),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0("Year: ", Year, "<br>Value: ", Value)
    ) %>%
      layout(
        yaxis = list(title = paste0("Year (Source: ", src, ")")),
        xaxis = list(title = "Value")
      )
    plot_list <- c(plot_list, list(p))
  }

  # Combine all bar charts into subplots
  subplot(plot_list, nrows = length(plot_list), shareX = TRUE, titleX = TRUE) %>%
    layout(title = paste("Horizontal Bar Plots by 'source' for:", input$collabSelector))
})
  
  output$flagButtons <- renderUI({
    req(input$selectedCountry)  # Proceed only if a country has been selected
    selected <- input$selectedCountry
    
    # Get subset of data for the selected country
    data_subset <- collab_data()[grepl(selected, iso2c)]
    
    if(nrow(data_subset) == 0)
      return(HTML("No detailed data available for this country."))
    
    # Calculate statistics; adjust column names as needed
    occurrence <- sum(sapply(strsplit(data_subset$iso2c, "-"), function(x) selected %in% x))
    mean_percentage <- round(mean(data_subset$Percentage, na.rm = TRUE), 3)
    mean_value <- round(mean(data_subset$Value, na.rm = TRUE), 3)
    
    # Convert ISO to full country name using countrycode
    country_name <- countrycode(selected, "iso2c", "country.name", warn = FALSE)
    
    # Find other collaborations in the full dataset for this country
    all_collabs_for_country <- df[grepl(selected, iso2c), unique(CollabGroup)]

    HTML(glue::glue("
      <h4>{country_name}</h4>
      <p><strong>Occurrences in '{input$collabSelector}':</strong> {occurrence}</p>
      <p><strong>Mean Percentage:</strong> {mean_percentage}</p>
      <p><strong>Mean Value:</strong> {mean_value}</p>
      <p><strong>Other collaborations for {selected}:</strong></p>
      <ul>
        {paste0('<li>', all_collabs_for_country, '</li>', collapse='')}
      </ul>
    "))
  })
}

shinyApp(ui, server)