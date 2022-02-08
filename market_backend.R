library(tidyverse)
library(purrr)
library(readr)


# Read data ---------------------------------------------------------------

#' Reads the demand file
#'
#' @param path, The path to the demand file
#' @return A tibble with the demand
#'
read_demand <- function(path) {
  out <- read_csv(path,
    col_types = cols(
      day = col_factor(),
      hour = col_factor()
    )
  )

  out
}

#' Reads the supply file
#'
#' @param path, The path to the supply file
#' @return A tibble with the supply
#'
read_supply <- function(path) {
  out <- read_csv(path) %>%
    pivot_longer(starts_with("bid"),
      names_to = "period",
      values_to = "bid"
    ) %>%
    mutate(period = stringr::str_remove(period, "bid_")) %>%
    separate(period, c("day", "hour"), sep = "_") %>%
    group_by(location, day, hour) %>%
    arrange(location, bid) %>%
    mutate(
      lower_bound = cumsum(capacity) - capacity,
      upper_bound = cumsum(capacity),
      om_day = parse_number(om_day)
    )

  out
}


# Helper functions --------------------------------------------------------

#' Calculates the x-intercept of two functions.
#'
#' @param a, First function (demand)
#' @param b, Second function (supply)
#' @param limits, A vector with the x-limits to search for an intersection
#' @return A double with the x-coord of the intersection between both functions
#'
fun_intersect <- function(a, b, limits = c(0, 15000)) {
  uniroot(function(x) a(x) - b(x), limits)$root # Not ideal but good enough

  # @TODO Step functions are not defined vertically. That's why intersections
  # here might no be calculated exactly. Currently there's up to a ~5% error
  # but could be refactored by using simple features or something else.

  # Look at this thread:
  # https://stackoverflow.com/questions/23830906/intersection-of-two-step-functions
}


#' Calculates the price difference between two regions given a certain
#' amount of arbitrage (transmission capacity)
#'
#' @param delta, Effective transmission to reach equilibrium prices (Arbitrage)
#' @param dn, Demand function for North Region
#' @param sn, Supply function for North Region
#' @param ds, Demand function for South Region
#' @param ss, Supply function for South Region
#' @return A double with difference between prices in both regions given arbitrage
#'
price_dif <- function(delta, dn, sn, ds, ss) {

  # Shifted functions
  s_sn <- function(x) sn(x - delta)
  s_ss <- function(x) ss(x + delta)

  # x-intercept
  xn <- fun_intersect(dn, s_sn)
  xs <- fun_intersect(ds, s_ss)

  # Difference of prices (evaluated shifted functions)
  s_sn(xn) - s_ss(xs)
}


#' Find the value of delta (transmission or arbitrage) that minimizes
#' the difference between prices of two regions (North and South)
#'
#' @param dn, Demand function for North Region
#' @param sn, Supply function for North Region
#' @param ds, Demand function for South Region
#' @param ss, Supply function for South Region
#' @param limits, The interval of values into which look for delta
#'
optimize_prices <- function(dn, sn, ds, ss, limits = c(-10000, 10000)) {
  uniroot(price_dif, limits, dn = dn, sn = sn, ds = ds, ss = ss)
}


# Supply and demand curves ------------------------------------------------

#' Generates the demand functions given the parameters in the demand file
#'
#' @param demand, The DataFrame with the demand
#' @return A tibble with the demand functions
#'
get_dem_curves <- function(demand) {
  dem_curves <- demand %>%
    mutate(
      dem_fun = map2(intercept, slope, function(i, s) {
        function(q) (q - i) / s
      })
    )

  dem_curves
}

#' Generates curve of the supply given the bids in the supply file
#' Uses step functiones to define the curves
#'
#' @param demand, The DataFrame with the supply
#' @return A tibble with the supply curves
#'
get_sup_curves <- function(supply) {
  sup_curves <- supply %>%
    pivot_longer(c(lower_bound, upper_bound),
      names_to = "names",
      values_to = "x"
    ) %>%
    rename(y = bid) %>%
    select(location, day, hour, x, y, group) %>%
    nest()

  sup_funs <- supply %>%
    nest() %>%
    mutate(sup_fun = map(data, function(d) {
      stepfun(d$upper_bound, c(d$bid, 500))
    })) %>%
    select(-data)

  sup_curves <- sup_curves %>% left_join(sup_funs)

  sup_curves
}


# Clearing Prices ---------------------------------------------------------

# This are the original clearing prices (first calculated without arbitrage)
get_clearing_prices <- function(dem_curves, sup_curves) {
  clearing_prices <- left_join(dem_curves, sup_curves) %>%
    mutate(
      demand = map2_dbl(dem_fun, sup_fun, fun_intersect),
      price = invoke_map_dbl(dem_fun, demand)
    )

  clearing_prices
}


# Transport (arbitrage) ---------------------------------------------------

# Calculate prices in equilibrium with arbitrage or considering
# transport restrictions

# First we determine the amount of arbitrage between both regions
# (the amount of transmission without constraints)
get_deltas <- function(clearing_prices) {
  deltas <- clearing_prices %>%
    select(location, day, hour, dem_fun, sup_fun, trans_cap) %>%
    group_by(location, day, hour) %>%
    pivot_wider(
      names_from = location,
      values_from = c(sup_fun, dem_fun)
    ) %>%
    mutate(delta = pmap(
      list(
        dem_fun_North,
        sup_fun_North,
        dem_fun_South,
        sup_fun_South
      ),
      optimize_prices
    )) %>%
    unnest_wider(delta) %>%
    mutate(root = root / abs(root) * pmin(trans_cap, abs(root)))

  deltas
}

# Next, we calculate prices in equilibrium
get_eq_prices <- function(clearing_prices, deltas) {
  eq_prices <- clearing_prices %>%
    left_join(deltas) %>%
    mutate(
      root = root * if_else(location == "North", 1, -1),
      shifted_dem_fun = map2(dem_fun, root, function(f, d) function(x) f(x + d)),
      new_dem = map2_dbl(shifted_dem_fun, sup_fun, fun_intersect),
      new_price = invoke_map_dbl(shifted_dem_fun, new_dem)
    ) %>%
    select(
      day, hour, location, trans_cap, tax_co2,
      demand, price, root, new_dem, new_price
    ) %>%
    arrange(day, hour)

  eq_prices
}


# P&L ---------------------------------------------------------------------

# @Note: Here I could implement different types of auctions (pay as bid,
# first price, etc.)

get_full_profits <- function(supply, eq_prices) {
  profits <- supply %>%
    left_join(eq_prices) %>%
    mutate(
      assigned_dem = pmax(pmin(new_dem, upper_bound) - lower_bound, 0),
      revenues = assigned_dem * new_price,
      fixed_costs = om_day / 4,
      var_costs = assigned_dem * marginal_cost,
      costs = fixed_costs + var_costs,
      emissions = (co2e * assigned_dem),
      carbon_tax = emissions * tax_co2,
      profit = revenues - costs - carbon_tax
    )

  profits
}

get_profits <- function(supply, eq_prices) {
  profits <- get_full_profits(supply, eq_prices)

  profit_yr <- profits %>%
    group_by(group, day) %>%
    summarise_at(vars(
      revenues, fixed_costs, var_costs,
      costs, emissions, carbon_tax, profit
    ), sum)

  print(profit_yr %>%
    arrange(profit) %>%
    mutate_at(
      vars(revenues, costs, carbon_tax, profit),
      scales::dollar
    ))

  profit_yr
}
