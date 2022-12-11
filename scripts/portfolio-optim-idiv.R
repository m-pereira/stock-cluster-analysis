## tips: install ROI.plugin.glpk and ROI.plugin.quadprog in ubuntu you need sudo apt-get install libglpk-dev
#install.packages("ROI.plugin.glpk")
library(dplyr)
library(tidyquant)
library(PortfolioAnalytics)
my_df <- readRDS(here::here("data","cleaned.RDS"))
ticks <- my_df %>% select(-ref_date) %>% colnames()
my_xts <- tk_xts(my_df)
pspec <- portfolio.spec(assets = ticks)
pspec <- add.constraint(portfolio=pspec,type="full_investment")
#pspec <- add.constraint(portfolio=pspec,type="box",min=0,max=0.1)
pspec <- add.constraint(portfolio=pspec, 
                        type="diversification", div_target=0.7)
spec <- add.objective(portfolio=pspec,type='risk',name='var')
library(tidyquant)
require(doParallel)
library(timetk)
registerDoParallel()

opt <- optimize.portfolio(my_xts, portfolio = pspec, 
                          optimize_method = "ROI")
opt

meansd.ef <- create.EfficientFrontier(
  R = my_xts,
  portfolio = pspec,
  type = "mean-sd",
  n.portfolios = 25,
)

summary(meansd.ef, digits=2) # to print the whole efficient frontier
meansd.ef$frontier[1:2,] # shows the first two portfolios
chart.EfficientFrontier(meansd.ef,
                        match.col="StdDev", # which column to use for risk
                        type="l", 
                        RAR.text="Sharpe Ratio",
                        tangent.line = FALSE,
                        chart.assets=TRUE,
                        labels.assets=TRUE#,
#                        xlim=c(0.03,0.20),ylim=c(0,0.055)
)


init <- portfolio.spec(assets = ticks)

init <- portfolio.spec(colnames(my_wallet %>% select(-ref_date)))
init <- add.constraint(portfolio=init, type="leverage",
                       min_sum=0.99, max_sum=1.01)
init <- add.constraint(portfolio=init, type="box", min=0.01, max=0.5)

minvar <- add.objective(portfolio=init, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=my_xts, portfolio=minvar,
                                 optimize_method="ROI", trace=TRUE)
opt_minvar
opt_minvar$weights %>% sort(decreasing = TRUE)
