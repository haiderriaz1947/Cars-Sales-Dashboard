# Load libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RMariaDB)
library(plotly)
library(DT)
library(dplyr)

# Connect to MySQL
bts <- dbConnect(RMariaDB::MariaDB(),
                 user = 'Rania061',
                 password = "Rania061",
                 host = 'localhost',
                 dbname = 'Rania_db')

sales_data <- dbReadTable(bts, "sales_data") %>%
  mutate(
    SALES = as.numeric(SALES),
    PRICEEACH = as.numeric(PRICEEACH),
    QUANTITYORDERED = as.numeric(QUANTITYORDERED)
  ) %>%
  filter(!is.na(SALES) & !is.na(PRICEEACH) & !is.na(QUANTITYORDERED))

dbDisconnect(bts)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Interactive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Info", tabName = "datainfo", icon = icon("info-circle")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Geo Analysis", tabName = "geo", icon = icon("globe")),
      menuItem("Summary", tabName = "summary", icon = icon("clipboard-list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("datainfo",
              h3("About the Sales Dataset"),
              p("This dashboard presents interactive sales data from a MySQL database."),
              p("Key Columns: PRODUCTLINE, SALES, STATUS, DEALSIZE, COUNTRY, CUSTOMERNAME, PRICEEACH, QUANTITYORDERED.")
      ),
      tabItem("datatable",
              fluidRow(box(DTOutput("sales_table"), width = 12))
      ),
      tabItem("trends",
              fluidRow(
                column(12, align = "right",
                       actionButton("reset_btn_trend", "Refresh Selection", icon = icon("sync")))
              ),
              fluidRow(
                valueBoxOutput("total_sales_trend"),
                valueBoxOutput("avg_price_trend"),
                valueBoxOutput("total_orders_trend"),
                valueBoxOutput("unique_products_trend")
              ),
              fluidRow(
                box(plotlyOutput("line_chart", height = 300), width = 6),
                box(plotlyOutput("scatter_chart", height = 300), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("trend1", height = 300), width = 6),
                box(plotlyOutput("trend2", height = 300), width = 6)
              )
      ),
      tabItem("comparison",
              fluidRow(
                column(12, align = "right",
                       actionButton("reset_btn_comp", "Refresh Selection", icon = icon("sync")))
              ),
              fluidRow(
                valueBoxOutput("total_sales_comp"),
                valueBoxOutput("avg_sales_comp"),
                valueBoxOutput("high_deal_comp"),
                valueBoxOutput("status_types_comp")
              ),
              fluidRow(
                box(plotlyOutput("box_chart", height = 300), width = 6),
                box(plotlyOutput("density_chart", height = 300), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("comp1", height = 300), width = 6),
                box(plotlyOutput("comp2", height = 300), width = 6)
              )
      ),
      tabItem("geo",
              fluidRow(
                column(12, align = "right",
                       actionButton("reset_btn_geo", "Refresh Selection", icon = icon("sync")))
              ),
              fluidRow(
                valueBoxOutput("unique_countries"),
                valueBoxOutput("total_customers"),
                valueBoxOutput("avg_sales_geo"),
                valueBoxOutput("region_count")
              ),
              fluidRow(
                box(plotlyOutput("map_chart", height = 300), width = 6),
                box(plotlyOutput("geo2", height = 300), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("geo3", height = 300), width = 6),
                box(plotlyOutput("geo4", height = 300), width = 6)
              )
      ),
      tabItem("summary",
              fluidRow(
                box(
                  sliderInput("sales_filter", "Filter Sales Range:",
                              min = min(sales_data$SALES), max = max(sales_data$SALES),
                              value = c(min(sales_data$SALES), max(sales_data$SALES))),
                  sliderInput("price_filter", "Filter Price Each Range:",
                              min = min(sales_data$PRICEEACH), max = max(sales_data$PRICEEACH),
                              value = c(min(sales_data$PRICEEACH), max(sales_data$PRICEEACH))),
                  selectInput("product_filter", "Select Product Line:",
                              choices = c("All", unique(sales_data$PRODUCTLINE))),
                  width = 4
                ),
                box(
                  valueBoxOutput("summary_total_sales"),
                  valueBoxOutput("summary_avg_sales"),
                  valueBoxOutput("summary_orders"),
                  width = 8
                )
              ),
              fluidRow(
                box(plotlyOutput("summary_plot", height = 300), width = 12)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Create a reactive value to store the current filter
  current_filter <- reactiveValues(
    column = NULL,
    value = NULL
  )
  
  # Reset buttons for each tab
  observeEvent(input$reset_btn_trend, {
    current_filter$column <- NULL
    current_filter$value <- NULL
  })
  
  observeEvent(input$reset_btn_comp, {
    current_filter$column <- NULL
    current_filter$value <- NULL
  })
  
  observeEvent(input$reset_btn_geo, {
    current_filter$column <- NULL
    current_filter$value <- NULL
  })
  
  # Observe all plotly click events and update the current filter
  observe({
    # Line chart click
    line_event <- event_data("plotly_click", source = "line_chart")
    if (!is.null(line_event)) {
      current_filter$column <- "DEALSIZE"
      current_filter$value <- line_event$x
    }
    
    # Scatter chart click
    scatter_event <- event_data("plotly_click", source = "scatter_chart")
    if (!is.null(scatter_event)) {
      current_filter$column <- "QUANTITYORDERED"
      current_filter$value <- scatter_event$x
    }
    
    # Trend1 chart click
    trend1_event <- event_data("plotly_click", source = "trend1")
    if (!is.null(trend1_event)) {
      current_filter$column <- "STATUS"
      current_filter$value <- trend1_event$x
    }
    
    # Trend2 chart click
    trend2_event <- event_data("plotly_click", source = "trend2")
    if (!is.null(trend2_event)) {
      current_filter$column <- "DEALSIZE"
      current_filter$value <- trend2_event$x
    }
    
    # Box chart click
    box_event <- event_data("plotly_click", source = "box_chart")
    if (!is.null(box_event)) {
      current_filter$column <- "DEALSIZE"
      current_filter$value <- box_event$curveNumber + 1  # Adjust based on your data
    }
    
    # Density chart click (not typically interactive)
    
    # Comp1 chart click
    comp1_event <- event_data("plotly_click", source = "comp1")
    if (!is.null(comp1_event)) {
      current_filter$column <- "PRODUCTLINE"
      current_filter$value <- comp1_event$x
    }
    
    # Comp2 chart click
    comp2_event <- event_data("plotly_click", source = "comp2")
    if (!is.null(comp2_event)) {
      current_filter$column <- "STATUS"
      current_filter$value <- comp2_event$x
    }
    
    # Map chart click
    map_event <- event_data("plotly_click", source = "map_chart")
    if (!is.null(map_event)) {
      current_filter$column <- "COUNTRY"
      current_filter$value <- map_event$pointNumber + 1  # Adjust based on your data
    }
    
    # Geo2 chart click
    geo2_event <- event_data("plotly_click", source = "geo2")
    if (!is.null(geo2_event)) {
      current_filter$column <- "COUNTRY"
      current_filter$value <- geo2_event$x
    }
    
    # Geo3 chart click
    geo3_event <- event_data("plotly_click", source = "geo3")
    if (!is.null(geo3_event)) {
      current_filter$column <- "TERRITORY"
      current_filter$value <- geo3_event$x
    }
    
    # Geo4 chart click
    geo4_event <- event_data("plotly_click", source = "geo4")
    if (!is.null(geo4_event)) {
      current_filter$column <- "DEALSIZE"
      current_filter$value <- geo4_event$pointNumber + 1  # Adjust based on your data
    }
  })
  
  # Reactive filtered data based on current filter
  reactive_filtered_data <- reactive({
    data <- sales_data
    
    if (!is.null(current_filter$column) && !is.null(current_filter$value)) {
      if (current_filter$column %in% names(data)) {
        if (current_filter$column == "DEALSIZE") {
          data <- data[data$DEALSIZE == unique(data$DEALSIZE)[current_filter$value], ]
        } else if (current_filter$column == "STATUS") {
          data <- data[data$STATUS == current_filter$value, ]
        } else if (current_filter$column == "PRODUCTLINE") {
          data <- data[data$PRODUCTLINE == current_filter$value, ]
        } else if (current_filter$column == "TERRITORY") {
          data <- data[data$TERRITORY == current_filter$value, ]
        } else if (current_filter$column == "COUNTRY") {
          data <- data[data$COUNTRY == current_filter$value, ]
        } else if (current_filter$column == "QUANTITYORDERED") {
          data <- data[data$QUANTITYORDERED == current_filter$value, ]
        }
      }
    }
    data
  })
  
  # Trends ValueBoxes
  output$total_sales_trend <- renderValueBox({
    valueBox(sum(reactive_filtered_data()$SALES), "Total Sales", icon = icon("dollar-sign"), color = "green")
  })
  output$avg_price_trend <- renderValueBox({
    valueBox(round(mean(reactive_filtered_data()$PRICEEACH), 2), "Avg. Price Each", icon = icon("tag"), color = "blue")
  })
  output$total_orders_trend <- renderValueBox({
    valueBox(nrow(reactive_filtered_data()), "Total Orders", icon = icon("shopping-cart"), color = "purple")
  })
  output$unique_products_trend <- renderValueBox({
    valueBox(length(unique(reactive_filtered_data()$PRODUCTLINE)), "Product Categories", icon = icon("boxes"), color = "orange")
  })
  
  # Trend Charts
  output$line_chart <- renderPlotly({
    df <- reactive_filtered_data() %>%
      group_by(DEALSIZE) %>%
      summarise(Sales = sum(SALES))
    plot_ly(df, x = ~DEALSIZE, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            source = "line_chart") %>%
      layout(clickmode = 'event+select')
  })
  
  output$scatter_chart <- renderPlotly({
    plot_ly(reactive_filtered_data(), x = ~QUANTITYORDERED, y = ~PRICEEACH, type = 'scatter', mode = 'markers',
            source = "scatter_chart") %>%
      layout(clickmode = 'event+select')
  })
  
  output$trend1 <- renderPlotly({
    plot_ly(reactive_filtered_data(), x = ~STATUS, y = ~SALES, type = 'box',
            source = "trend1") %>%
      layout(clickmode = 'event+select')
  })
  
  output$trend2 <- renderPlotly({
    plot_ly(reactive_filtered_data(), x = ~DEALSIZE, y = ~SALES, type = 'box',
            source = "trend2") %>%
      layout(clickmode = 'event+select')
  })
  
  # Comparison ValueBoxes
  output$total_sales_comp <- renderValueBox({
    valueBox(sum(reactive_filtered_data()$SALES), "Total Sales", icon = icon("coins"), color = "teal")
  })
  output$avg_sales_comp <- renderValueBox({
    valueBox(mean(reactive_filtered_data()$SALES), "Avg Sales", icon = icon("chart-bar"), color = "lime")
  })
  output$high_deal_comp <- renderValueBox({
    valueBox(max(reactive_filtered_data()$SALES), "Highest Sale", icon = icon("star"), color = "yellow")
  })
  output$status_types_comp <- renderValueBox({
    valueBox(length(unique(reactive_filtered_data()$STATUS)), "Status Types", icon = icon("tags"), color = "maroon")
  })
  
  output$box_chart <- renderPlotly({
    plot_ly(reactive_filtered_data(), y = ~SALES, type = 'box', color = ~DEALSIZE,
            source = "box_chart") %>%
      layout(clickmode = 'event+select')
  })
  
  output$density_chart <- renderPlotly({
    d <- density(reactive_filtered_data()$SALES)
    plot_ly(x = d$x, y = d$y, type = 'scatter', mode = 'lines',
            source = "density_chart") %>%
      layout(clickmode = 'event+select')
  })
  
  output$comp1 <- renderPlotly({
    df <- reactive_filtered_data() %>%
      group_by(PRODUCTLINE) %>%
      summarise(Sales = sum(SALES))
    plot_ly(df, x = ~PRODUCTLINE, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            source = "comp1") %>%
      layout(clickmode = 'event+select')
  })
  
  output$comp2 <- renderPlotly({
    df <- reactive_filtered_data() %>%
      group_by(STATUS) %>%
      summarise(Count = n())
    plot_ly(df, x = ~STATUS, y = ~Count, type = 'scatter', mode = 'lines+markers',
            source = "comp2") %>%
      layout(clickmode = 'event+select')
  })
  
  # Geo ValueBoxes
  output$unique_countries <- renderValueBox({
    valueBox(length(unique(reactive_filtered_data()$COUNTRY)), "Countries", icon = icon("globe"), color = "aqua")
  })
  output$total_customers <- renderValueBox({
    valueBox(length(unique(reactive_filtered_data()$CUSTOMERNAME)), "Customers", icon = icon("users"), color = "navy")
  })
  output$avg_sales_geo <- renderValueBox({
    valueBox(mean(reactive_filtered_data()$SALES), "Avg Sales", icon = icon("chart-line"), color = "olive")
  })
  output$region_count <- renderValueBox({
    valueBox(length(unique(reactive_filtered_data()$TERRITORY)), "Regions", icon = icon("map"), color = "purple")
  })
  
  output$map_chart <- renderPlotly({
    plot_ly(reactive_filtered_data(), type = "scattergeo", mode = "markers",
            locations = ~COUNTRY, locationmode = "country names", text = ~CUSTOMERNAME,
            source = "map_chart") %>%
      layout(clickmode = 'event+select')
  })
  
  output$geo2 <- renderPlotly({
    df <- reactive_filtered_data() %>%
      group_by(COUNTRY) %>%
      summarise(Sales = sum(SALES))
    plot_ly(df, x = ~COUNTRY, y = ~Sales, type = "scatter", mode = "lines+markers",
            source = "geo2") %>%
      layout(clickmode = 'event+select')
  })
  
  output$geo3 <- renderPlotly({
    df <- reactive_filtered_data() %>%
      group_by(TERRITORY) %>%
      summarise(Sales = sum(SALES))
    plot_ly(df, x = ~TERRITORY, y = ~Sales, type = "scatter", mode = "lines+markers",
            source = "geo3") %>%
      layout(clickmode = 'event+select')
  })
  
  output$geo4 <- renderPlotly({
    df <- reactive_filtered_data() %>%
      count(DEALSIZE)
    plot_ly(df, labels = ~DEALSIZE, values = ~n, type = 'pie', hole = 0.5,
            source = "geo4") %>%
      layout(clickmode = 'event+select')
  })
  
  # Summary tab
  summary_data <- reactive({
    d <- sales_data %>%
      filter(SALES >= input$sales_filter[1], SALES <= input$sales_filter[2],
             PRICEEACH >= input$price_filter[1], PRICEEACH <= input$price_filter[2])
    if (input$product_filter != "All") {
      d <- d %>% filter(PRODUCTLINE == input$product_filter)
    }
    d
  })
  
  output$summary_total_sales <- renderValueBox({
    valueBox(sum(summary_data()$SALES), "Filtered Total Sales", icon = icon("dollar-sign"), color = "green")
  })
  output$summary_avg_sales <- renderValueBox({
    valueBox(mean(summary_data()$SALES), "Filtered Avg Sales", icon = icon("chart-line"), color = "blue")
  })
  output$summary_orders <- renderValueBox({
    valueBox(nrow(summary_data()), "Filtered Orders", icon = icon("shopping-cart"), color = "purple")
  })
  output$summary_plot <- renderPlotly({
    plot_ly(summary_data(), x = ~QUANTITYORDERED, y = ~SALES, type = 'scatter', mode = 'markers')
  })
  
  output$sales_table <- renderDT({
    datatable(sales_data)
  })
}

shinyApp(ui, server)