library(tidyverse)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(timetk)
my_wallet <- readRDS(here::here("data","raw.RDS")) %>% 
  select(ref_date,ticker,ret_adjusted_prices) %>% 
  pivot_wider(names_from = "ticker",
              values_from = "ret_adjusted_prices")
my_xts <- 
  my_wallet %>% 
  slice(-1) %>% 
  tk_xts()
index(my_xts) <- as.Date(index(my_xts))
port_spec <- portfolio.spec(colnames(my_wallet %>% select(-ref_date)))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")
# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
# Solve the optimization problem
opt <- optimize.portfolio(my_xts, portfolio = port_spec, optimize_method = "ROI")
opt
extractWeights(opt)
chart.Weights(opt)
## quadract function--------------------
# Create the portfolio specification
port_spec <- portfolio.spec(colnames(my_wallet %>% select(-ref_date)))
# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to maximize portfolio mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add an objective to minimize portfolio variance
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Solve the optimization problem
opt <- optimize.portfolio(R = my_xts, 
                          portfolio = port_spec, 
                          optimize_method = "ROI")


## appliying
asset_returns <- my_xts
equal_weights <- rep(1 / (ncol(my_wallet)-1), (ncol(my_wallet)-1))
r_benchmark <- Return.portfolio(R = asset_returns, weights = equal_weights, 
                                rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"
plot(r_benchmark)
## define optimatin problem
# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(asset_returns))
# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
#port_spec <- add.constraint(portfolio = port_spec, type = "weight_sum_constraint", min_sum=0.99, max_sum=1.01)
# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Print the portfolio specification
print(port_spec)

## backtesting
# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                 portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 60,
                                                 rolling_window = 60)

# Print the results
print(opt_rebal_base)
chart.Weights(opt_rebal_base)

# refine objetctives
rp <- random_portfolios(portfolio=port_spec, permutations = 100, rp_method ='simplex')

port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = 0.05, 
                           max_prisk = 0.1)
opt_rebal_rb <- optimize.portfolio.rebalancing(R = asset_returns, 
                                               portfolio = port_spec, 
                                               optimize_method = "random", rp = rp,
                                               trace = TRUE,
                                               rebalance_on = "quarters", 
                                               training_period = 60,
                                               rolling_window = 60)
chart.Weights(opt_rebal_rb)



## robust optimizatoin
opt_rebal_rb_robust <- 
  optimize.portfolio.rebalancing(
  R = asset_returns, 
  momentFUN = "moments_robust",
  portfolio = port_spec, 
  optimize_method = "DEoptim",
  #rp = rp,
  trace = TRUE,
  rebalance_on = "quarters", 
  training_period = 60,
  rolling_window = 60)
opt_rebal_rb_robust
# Chart the weights
chart.Weights(opt_rebal_rb_robust)


returns_base <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_base))
returns_rb <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_rb))

ret <- cbind(r_benchmark, returns_base,returns_rb)
table.AnnualizedReturns(R = ret)
charts.PerformanceSummary(R = ret)
