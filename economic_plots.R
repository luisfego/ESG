library(tidyverse)
library(plotly)


# Functions ---------------------------------------------------------------

# Supply and demand curves
plot_day <- function(plot_day, sup_curves, demand, supply, eq_prices) {
  
  sup_data <- sup_curves %>%
    unnest(cols = "data") %>%
    filter(day == plot_day)
  
  prices <- eq_prices %>% 
    filter(day == plot_day) 
  
  dem_data <- demand %>% 
    filter(day == plot_day) %>% 
    left_join(eq_prices)
  
  sup_data2 <- supply %>% 
    filter(day == plot_day)
  
  p <- ggplot(dem_data) + 
    geom_bar(data = sup_data2,
             aes(x     = (lower_bound + (capacity/2)),
                 width = capacity,
                 y     = bid,
                 fill  = group,
                 text  = paste0(name, " (", fuel, ")")), stat = "identity") +
    geom_abline(aes(slope = 1/slope, intercept=-intercept/slope), color = "lightgray") + 
    geom_abline(aes(slope = 1/slope, intercept=(root-intercept)/slope), color = "black") + 
    geom_line(data = sup_data, aes(x=x,y=y)) +
    geom_point(data = prices, aes(x = demand, y = price), color = "blue") +
    geom_point(data = prices, aes(x = new_dem, y = new_price), color = "red") +
    facet_grid(vars(hour), vars(location)) + 
    scale_x_continuous(labels = scales::comma) + 
    scale_y_continuous(labels = scales::dollar,
                       limits = c(0,max(sup_data$y,max(prices$price)))) + 
    xlab("Demand (Q) [MW]") + ylab("Price (P) (USD)") +
    theme_light()
  
  ggplotly(p)
  
}

# Clearing prices
plot_prices <- function(eq_prices) {
  
  data <- eq_prices %>% 
    group_by(day, hour) %>% 
    summarize(price = mean(new_price)) %>%
    mutate(location = "Equilibrium") %>% 
    bind_rows(eq_prices) %>% 
    mutate(
      day  = paste0('Day ', day),
    ) %>%
    select(day, hour, price, location) %>% 
    group_by(location) %>% 
    arrange(location) 
  
  # Refactored plot in native plotly
  fig <- plot_ly(data,
                 x     = list(~day, ~hour),
                 y     = ~price,
                 color = ~location, 
                 type  = 'scatter', 
                 mode  = 'lines+markers') %>% 
    layout(
      title = "Market prices [USD/MWh]", 
      xaxis = list(title = "Hour"), 
      yaxis = list(title = "Price", tickformat = "$"), 
      hovermode = "compare"
    )
  
  fig
    
}

# Total demand
plot_demand <- function(eq_prices) {
  
  data <- eq_prices %>% 
    mutate(day = paste0("Day ", day)) %>% 
    arrange(location)
  
  fig <- plot_ly(data, 
                 x          = list(~day, ~hour), 
                 y          = ~new_dem,
                 color      = ~location, 
                 type       = "scatter", 
                 mode       = "lines+markers", 
                 fill       = "viridis", 
                 stackgroup = "one") %>% 
    layout(
      title = "Total demand", 
      xaxis = list(title = "Hour"), 
      yaxis = list(title = "Demand [MW]", tickformat = ","), 
      hovermode = "compare"
    )
  
  fig
  
}

# Flow
plot_flow <- function(eq_prices) {
  data <- eq_prices %>% 
    mutate(
      direction = if_else(root < 0, "South-North", "North-South"), 
      day       = paste0("Day ", day)
    ) %>%
    filter(location == "North")
  
  fig <- plot_ly(data, 
                 x     = ~root, 
                 y     = list(~day, ~hour), 
                 type  = "bar", orientation = "h", name = "Flow"
                 ) %>% 
    layout(
      title = "Transmission North-South", 
      xaxis = list(title = "Energy [MWh]", tickformat = ","),
      yaxis = list(title = "Hour")
    ) %>% 
    add_trace(x =  ~trans_cap,
              type = "scatter",
              mode = "lines",
              line = list(shape = "vhv"), 
              name = "Trans. cap.", 
              color = I("red")) %>% 
    add_trace(x =  ~-trans_cap,
              type = "scatter",
              mode = "lines",
              line = list(shape = "vhv"), 
              name = "Trans. cap.", 
              color = I("red"), showlegend = FALSE)
  
  fig
}

# Taxes
plot_tax <- function(profits) {
  
  data <- profits %>% 
    mutate(
      emissions = co2e * assigned_dem,
      day       = paste0("Day ", day)
      ) %>%
    group_by(hour, day, location) %>%
    summarize(
      emissions = sum(emissions),
      tax_co2   = max(tax_co2)
    ) %>% arrange(location)
  
  
  fig <- plot_ly(data, 
                 x = list(~day, ~hour), 
                 y = ~emissions, 
                 type = "bar", 
                 color = ~location) %>%
    add_trace(data  = data %>% filter(location == "North") %>% arrange(day, hour),
              y     = ~tax_co2, 
              x     = list(~day, ~hour), 
              yaxis = "y2", 
              line  = list(shape = "hvh"), 
              color = "red", 
              name  = "Carbon Tax", 
              type  = "scatter", 
              mode  = "line") %>%
    layout(
      title = "CO2 Emissions", 
      xaxis = list(title = "Hour"),
      yaxis = list(title = "Emissions [Tons CO2]"), 
      yaxis2 = list(side = "right", 
                    title = "Carbon Tax", 
                    overlaying = "y", 
                    tickformat = "$", 
                    range = c(0,100)), 
      barmode = "stack"
    )
  
  fig
  
}


