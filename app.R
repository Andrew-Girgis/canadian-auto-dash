library(shiny)
library(tidyquant)
library(tidyverse)
library(scales)
library(plotly)
library(lubridate)
library(reactable)
library(bslib)
library(htmltools)
library(rnaturalearth)
library(leaflet)
library(sf)

# TSX ticker
tsx_ticker <- "XIC.TO"

# Load preprocessed data
combined_data <- readRDS("data/combined_data.rds")
returns_wide <- readRDS("data/returns_wide.rds")
auto_exports_all <- readRDS("data/auto_exports_all.rds")
canada_provinces <- readRDS("data/canada_provinces_cleaned.rds")

# Define the significant events
events_df <- tibble::tibble(
  event = c("COVID", "Ukraine War", "Tariffs"),
  date = as.Date(c("2020-03-01", "2022-02-24", "2024-11-25"))
)

# Define the colours for the stocks
stock_colors <- c(
  "MG.TO" = "#D6963C",
  "LNR.TO" = "#0693e3",
  "MRE.TO" = "#5533ff",
  "XTC.TO" = "#00d084",
  "ACQ.TO" = "#D13239",
  "XIC.TO" = "#000000"
)

# Define Company Names
company_names <- c(
  "MG.TO" = "Magna International",
  "LNR.TO" = "Linamar Corporation",
  "MRE.TO" = "Martinrea Intl.",
  "XTC.TO" = "Exco Technologies",
  "ACQ.TO" = "AutoCanada",
  "XIC.TO" = "TSX Index"
)

# Database of company descriptions
company_descriptions <- list(
  "MG.TO" = "Magna International is a global automotive supplier headquartered in Aurora, Ontario. It produces automotive systems, modules, and components.",
  "LNR.TO" = "Linamar Corporation is a diversified global manufacturing company based in Guelph, Ontario, specializing in driveline systems and precision products.",
  "MRE.TO" = "Martinrea International Inc. is a Canadian auto parts manufacturer headquartered in Vaughan, Ontario, focused on lightweight structures and propulsion systems.",
  "XTC.TO" = "Exco Technologies Limited is a designer, developer, and manufacturer of dies, molds, and components for the automotive and industrial sectors.",
  "ACQ.TO" = "AutoCanada Inc. operates automotive dealerships across Canada and the U.S., offering new and used vehicles, repair services, and financing.",
  "XIC.TO" = "The TSX Index (XIC.TO) is an ETF that tracks the performance of the broad Canadian equity market through the S&P/TSX Capped Composite Index."
)



color_return_cell <- function(value) {
  if (is.na(value)) return("")
  arrow <- if (value > 0) "↑" else if (value < 0) "↓" else ""
  bg <- if (value > 0) "#cce5cc" else if (value < 0) "repeating-linear-gradient(45deg, #f5c6cb, #f5c6cb 5px, #f8d7da 5px, #f8d7da 10px)" else "white"
  color <- if (value > 0) "#1a472a" else if (value < 0) "#721c24" else "black"
  
  div(style = paste("background:", bg, "; padding:4px; color:", color, "; font-weight: bold;"),
      paste0(value, "% ", arrow))
}


# UI
ui <- fluidPage(
  theme = bs_theme(),  
  titlePanel("Cumulative Returns of Auto Stocks vs TSX"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("dark_mode", "Dark Mode", value = FALSE),  
      selectInput("stock", "Choose an auto stock:",
                  choices = setNames(names(company_names), company_names)),
      dateRangeInput(
        "table_date_range",
        "Select Month Range:",
        start = as.Date("2018-02-01"),
        end = Sys.Date(),
        min = as.Date("2018-02-01"),
        max = Sys.Date(),
        format = "yyyy-mm",
        startview = "year"
      ),
      br(),
      tags$h5(textOutput("company_name")),
      tags$p(style = "font-size: 14px; font-style: italic; color: gray;",
             textOutput("company_description"))
    ),
    mainPanel(
      width = 9,
      tags$div(
        style = "max-width: 750px; margin-left: auto; margin-right: auto; text-align: justify;",
        tags$h4("Project Summary"),
        tags$p("This dashboard investigates how major events have impacted Canada’s automotive sector, focusing on stock performance and export activity. Using tidyquant, tidyverse, and Statistics Canada data, I track the cumulative and monthly returns of five automotive stocks versus the TSX Index. Key global disruptions; COVID-19, the Russia-Ukraine war, and Trump’s 2024 tariff announcement are marked on the timeline."),
        tags$p("I observe clear return dips around each event, with all auto stocks underperforming the TSX over the full period, illustrating the sector’s vulnerability. Our interactive heatmap of provincial exports to the U.S. reveals a notable drop in the month after the tariff announcement—despite no actual implementation—suggesting market hesitation. Unfortunately, due to data availability, I cannot yet assess the direct impact of the tariff enactment itself."),
        tags$p("A custom Manim animation further visualizes provincial export changes, showing relative size and direction of change. This multi-faceted tool merges financial and trade data to highlight how macro shocks ripple through Canada’s automotive landscape.")
      ),
      plotlyOutput("returnPlot"),
      h3("Monthly Return Tracker"),
      reactableOutput("monthlyTable"),
      h3("Heat Map of Auto Exports by Province"),
      sliderInput("map_month", "Select Month:",
                  min = min(auto_exports_all$ref_date),
                  max = max(auto_exports_all$ref_date),
                  value = max(auto_exports_all$ref_date),
                  timeFormat = "%b %Y",
                  step = 30,  
                  animate = animationOptions(interval = 2500, loop = TRUE)),
      leafletOutput("heatMap", height = "600px"),
    
    )
  )
)


# Server
server <- function(input, output, session) {
  observe({
    session$setCurrentTheme(
      if (input$dark_mode) {
        bs_theme(bootswatch = "darkly")  # Dark theme
      } else {
        bs_theme(bootswatch = "flatly")  # Light theme
      }
    )
  })
  
  output$company_name <- renderText({
    company_names[input$stock]
  })
  
  output$company_description <- renderText({
    company_descriptions[[input$stock]]
  })

  output$returnPlot <- renderPlotly({
    filtered_data <- combined_data %>%
      filter(symbol %in% c(input$stock, tsx_ticker))
    
    is_dark <- input$dark_mode
    
    p <- ggplot(filtered_data, aes(
      x = date,
      y = cumulative_return,
      color = company,
      group = company
    )) +
      geom_line(linewidth = 0.6) +
      
      # Vertical event lines
      geom_segment(data = events_df,
                   aes(x = date, xend = date, y = -0.5, yend = 1.0),
                   inherit.aes = FALSE, color = "grey50", linetype = "dashed") +
      
      # Event labels
      geom_text(data = events_df,
                aes(x = date, y = 1.02, label = event),
                inherit.aes = FALSE, angle = 90, vjust = 0, size = 3,
                color = if (is_dark) "white" else "black") +
      
      # Manual colors (including white TSX line in dark mode)
      scale_color_manual(values = {
        stock_colors_mod <- stock_colors
        if (is_dark) stock_colors_mod["XIC.TO"] <- "white"
        setNames(stock_colors_mod, company_names)
      }) +
      
      scale_y_continuous(labels = scales::percent) +
      
      labs(
        title = paste("Cumulative Return:", company_names[input$stock], "vs TSX"),
        subtitle = "Major global events shown with dashed lines",
        x = "Date", y = "Cumulative Return",
        color = "Company"
      ) +
      
      (if (is_dark) theme_dark() else theme_minimal()) +
      theme(
        plot.background = element_rect(fill = if (is_dark) "#343a40" else "white", color = NA),
        panel.background = element_rect(fill = if (is_dark) "#343a40" else "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(color = if (is_dark) "white" else "black"),
        axis.text = element_text(color = if (is_dark) "white" else "black"),
        legend.text = element_text(color = if (is_dark) "white" else "black"),
        legend.title = element_text(color = if (is_dark) "white" else "black"),
        plot.title = element_text(color = if (is_dark) "white" else "black"),
        plot.subtitle = element_text(color = if (is_dark) "white" else "black")
      )
    
    ggplotly(p) %>% layout(
      paper_bgcolor = if (is_dark) "#343a40" else "white",
      plot_bgcolor = if (is_dark) "#343a40" else "white",
      font = list(color = if (is_dark) "white" else "black"),
      legend = list(
        bgcolor = if (is_dark) "#343a40" else "white",
        font = list(color = if (is_dark) "white" else "black")
      )
    )
  })

  
  output$monthlyTable <- renderReactable({
    is_dark <- input$dark_mode
    
    reactable(
      returns_wide,
      columns = c(
        list(company = colDef(name = "Company")),
        setNames(
          lapply(
            setdiff(colnames(returns_wide), c("company", "symbol")),
            function(col) colDef(cell = color_return_cell, align = "center")
          ),
          setdiff(colnames(returns_wide), c("company", "symbol"))
        )
      ),
      defaultPageSize = 10,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      theme = reactableTheme(
        backgroundColor = if (is_dark) "#2b2b2b" else "white",
        color = if (is_dark) "#f1f1f1" else "black",
        borderColor = if (is_dark) "#444" else "#ddd",
        stripedColor = if (is_dark) "#383838" else "#f9f9f9",
        highlightColor = if (is_dark) "#3d3d3d" else "#eaeaea",
        inputStyle = list(backgroundColor = if (is_dark) "#3a3a3a" else "white")
      )
    )
  })
  
  
  output$heatMap <- renderLeaflet({
    is_dark <- input$dark_mode
    selected_month <- input$map_month
    
    # Summarize monthly export change by province
    selected_data <- auto_exports_all %>%
      filter(format(ref_date, "%Y-%m") == format(selected_month, "%Y-%m")) %>%
      select(geo, total_exports, pct_change) %>%
      group_by(geo) %>%
      summarise(
        total_exports = sum(total_exports, na.rm = TRUE) * 1000,
        pct_change = mean(pct_change[is.finite(pct_change)], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join to spatial map safely (drop and reattach geometry)
    province_geom <- sf::st_geometry(canada_provinces)
    joined <- left_join(st_drop_geometry(canada_provinces), selected_data, by = c("name" = "geo"))
    map_data <- st_sf(joined, geometry = province_geom)
    
    # Clean pct_change and clip extreme values
    map_data$pct_change <- as.numeric(map_data$pct_change)
    map_data$pct_change[!is.finite(map_data$pct_change)] <- NA
    map_data$pct_change <- pmax(pmin(map_data$pct_change, 50), -50)
    
    # Define static domain and color palette
    pal <- colorNumeric("RdBu", domain = c(-50, 50))
    
    # Leaflet map
    leaflet(map_data) %>%
      addProviderTiles(if (is_dark) "CartoDB.DarkMatter" else "CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(pct_change),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(
          name, "<br>",
          "Total Exports: $", scales::comma(total_exports), "<br>",
          "Percent Change: ", paste0(round(pct_change, 1), "%")
        ) %>% lapply(htmltools::HTML),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = c(-50, 0, 50),
        title = htmltools::HTML("% Change<br>(Capped at ±50%)"),
        position = "bottomright"
      )
  })
  
  
}


# Run app
shinyApp(ui, server)

