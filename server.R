#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load market backend and plots
source("market_backend.R")
source("economic_plots.R")

# Define server logic 
shinyServer(function(input, output) {
  
  outs <- eventReactive(input$submitBtn, {
  
    if (!is.null(input$demFile$datapath))  
      demand <- read_demand(input$demFile$datapath)
    else 
      demand <- read_demand("www/data/demand_02.csv")
    
    if (!is.null(input$supFile$datapath))  
      supply <- read_supply(input$supFile$datapath)
    else 
      supply <- read_supply("www/data/portfolios_01.csv")
    
    dem_curves <- get_dem_curves(demand)
    sup_curves <- get_sup_curves(supply)
    
    clearing_prices <- get_clearing_prices(dem_curves, sup_curves)
    deltas          <- get_deltas(clearing_prices)
    eq_prices       <- get_eq_prices(clearing_prices, deltas)
    
    profit_yr    <- get_profits(supply, eq_prices)
    full_profits <- get_full_profits(supply, eq_prices)
    profits      <- full_profits %>%
                      ungroup() %>% 
                      select(day, hour, group, name, location, fuel, capacity, 
                             marginal_cost, bid, assigned_dem, revenues, costs, 
                             carbon_tax, profit)
  
    # Outputs
    list(
      market_results = eq_prices, 
      profit_yr      = profit_yr, 
      profits        = profits, 
      full_profits   = full_profits,
      sup_curves     = sup_curves, 
      demand         = demand,
      supply         = supply
    )
  })
  
  # MARKET RESULTS  -------------------------------------------------------
  
  output$plot_prices <- renderPlotly(plot_prices(outs()$market_results))
  
  output$plot_demand <- renderPlotly(plot_demand(outs()$market_results))
  
  output$plot_flow   <- renderPlotly(plot_flow(outs()$market_results))
  
  output$plot_tax    <- renderPlotly(plot_tax(outs()$full_profits))
  
  output$res_market  <- renderDataTable(outs()$market_results)
  
  # P&L  ------------------------------------------------------------------
   
  output$res_yr     <- renderReactable(
    reactable(
      outs()$profit_yr,
      groupBy       = "group", 
      defaultSorted = c("day", "profit"),
      filterable    = T, 
      defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
      columns = list(
        revenues = colDef(name = "Revenues", 
                          aggregate = "sum",
                          footer = function(values) scales::dollar(sum(values)),
                          format = colFormat(prefix = "$",
                                             separators = T,
                                             digits = 0)), 
        fixed_costs = colDef(name = "Fixed costs", 
                       aggregate = "sum",
                       footer = function(values) scales::dollar(sum(values)),
                       format = colFormat(prefix = "$",
                                          separators = T,
                                          digits = 0)),
        var_costs = colDef(name = "Variable costs", 
                       aggregate = "sum",
                       footer = function(values) scales::dollar(sum(values)),
                       format = colFormat(prefix = "$",
                                          separators = T,
                                          digits = 0)),
        costs = colDef(name = "Total Costs", 
                       aggregate = "sum",
                       footer = function(values) scales::dollar(sum(values)),
                       format = colFormat(prefix = "$",
                                          separators = T,
                                          digits = 0)),
        emissions   = colDef(name = "Emissions", 
                       aggregate = "sum",
                       footer = function(values) scales::comma(sum(values)),
                       format = colFormat(prefix = "",
                                          separators = T,
                                          digits = 0)),
        
        carbon_tax = colDef(name = "Carbon Tax", 
                       aggregate = "sum",
                       footer = function(values) scales::dollar(sum(values)),
                       format = colFormat(prefix = "$",
                                          separators = T,
                                          digits = 0)),
        profit = colDef(name = "Profits", 
                       aggregate = "sum",
                       footer = function(values) scales::dollar(sum(values)),
                       format = colFormat(prefix = "$",
                                          separators = T,
                                          digits = 0))
      )
      ))
  
  output$downloadFullData <- downloadHandler(
    filename = "data.csv", 
    content  = function(file) write_csv(outs()$full_profits, file)
  )
  
  # DAY 1 -----------------------------------------------------------------
  output$day1_plot  <- renderPlotly(plot_day(1, 
                                             outs()$sup_curves, 
                                             outs()$demand, 
                                             outs()$supply,
                                             outs()$market_results))
  
  output$day1_data  <- renderDataTable(outs()$profits %>% filter(day == 1))
  
  # DAY 1 -----------------------------------------------------------------
  output$day2_plot  <- renderPlotly(plot_day(2, 
                                             outs()$sup_curves, 
                                             outs()$demand, 
                                             outs()$supply,
                                             outs()$market_results))
  
  output$day2_data  <- renderDataTable(outs()$profits %>% filter(day == 2))
  
  # DAY 1 -----------------------------------------------------------------
  output$day3_plot  <- renderPlotly(plot_day(3, 
                                             outs()$sup_curves, 
                                             outs()$demand, 
                                             outs()$supply,
                                             outs()$market_results))
  
  output$day3_data  <- renderDataTable(outs()$profits %>% filter(day == 3))
  
  # DAY 1 -----------------------------------------------------------------
  output$day4_plot  <- renderPlotly(plot_day(4, 
                                             outs()$sup_curves, 
                                             outs()$demand, 
                                             outs()$supply,
                                             outs()$market_results))
  
  output$day4_data  <- renderDataTable(outs()$profits %>% filter(day == 4))

})
