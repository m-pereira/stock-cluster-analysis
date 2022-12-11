vignette("portfolio_vignette")
library(dplyr)
library(tidyr)
my_wallet <- readRDS(here::here("data","raw.RDS"))
library(corrr)
my_wallet %>% 
  select(ref_date,ticker,ret_adjusted_prices) %>% 
  pivot_wider(names_from = "ticker",
              values_from = "ret_adjusted_prices") %>% 
  select(-ref_date) %>% 
  correlate() %>% 
  rplot()


bova <- yfR::yf_get(
  "BOVA11.SA",
  first_date = Sys.Date() - 360,
  last_date = Sys.Date()
)

bova
library(tidyquant)
Ra <- 
my_wallet %>% 
  #group_by(ticker) %>% 
  tq_transmute(select   = price_adjusted, 
             mutate_fun = periodReturn, 
             period     = "monthly", 
             col_rename = "Ra") 
Ra

Rb <- bova %>% 
  tq_transmute(select     = price_adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
Rb
RaRb <- left_join(Ra, Rb)


RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)
RaRb_capm

Ra %>% 
tq_performance(
  Ra = Ra, 
  Rb = NULL, 
  performance_fun = SharpeRatio
)

Ra %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio, 
    Rf = 0.03 / 12, 
    p  = 0.99
  )

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL, 
                 performance_fun = table.Stats)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.CAPM)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL, 
                 performance_fun = table.AnnualizedReturns)
Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.Correlation)
Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.DownsideRisk)
Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL, 
                 performance_fun = table.DownsideRiskRatio)
Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.HigherMoments)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.InformationRatio)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = table.Variability)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = VaR)

Ra %>% 
  tq_performance(Ra = Ra, Rb = NULL,
                 performance_fun = SharpeRatio)

## portfolio optim
#https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/s-4portfolios.html#ss_4Intro
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)

my_wallet <- readRDS(here::here("data","raw.RDS")) %>% 
  select(ref_date,ticker,ret_adjusted_prices) %>% 
  pivot_wider(names_from = "ticker",
              values_from = "ret_adjusted_prices")
my_xts <- 
  my_wallet %>% 
  slice(-1) %>% 
  tk_xts()
index(my_xts) <- as.Date(index(my_xts))

init <- portfolio.spec(colnames(my_wallet %>% select(-ref_date)))
init <- add.constraint(portfolio=init, type="leverage",
                       min_sum=0.99, max_sum=1.01)
init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.65)
maxret <- add.objective(portfolio=init, type="return", name="mean")

opt_maxret <- optimize.portfolio(
  R=my_xts, portfolio=maxret,
  optimize_method="ROI",
  trace=TRUE)
print(opt_maxret)
# Maximize mean return with ROI
plot(opt_maxret, risk.col="StdDev", return.col="mean",
     main="Maximum Return Optimization", chart.assets=TRUE)


# Minimize variance with ROI
minvar <- add.objective(portfolio=init, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=my_xts, portfolio=minvar,
                                   optimize_method="ROI", trace=TRUE)
opt_minvar

plot(opt_minvar, risk.col="StdDev", return.col="mean",
     main="Minimum Variance Optimization", chart.assets=TRUE)


# Maximize quadratic utility with ROI
qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25)
opt_qu <- optimize.portfolio(R=my_xts, portfolio=qu,
                             optimize_method="ROI",
                             trace=TRUE)
print(opt_qu)

# Minimize expected tail loss with ROI
etl <- add.objective(portfolio=init, type="risk", name="ETL")
opt_etl <- optimize.portfolio(R=my_xts, portfolio=etl,
                              optimize_method="ROI",
                              trace=TRUE)

# Maximize mean return per unit ETL with random portfolios

meanETL <- add.objective(portfolio=init, type="return", name="mean")
meanETL <- add.objective(portfolio=meanETL, type="risk", name="ETL",
                           arguments=list(p=0.95))

opt_meanETL <- optimize.portfolio(R=my_xts, portfolio=meanETL,
                                  optimize_method="random",
                                  trace=TRUE, search_size=2000)
opt_meanETL
stats_meanETL <- extractStats(opt_meanETL)
dim(stats_meanETL)
head(stats_meanETL)
plot(opt_meanETL, risk.col="ETL", return.col="mean",
       main="mean-ETL Optimization", neighbors=25)

pct_contrib <- ES(R=my_xts, p=0.95, portfolio_method="component",
                    weights=extractWeights(opt_meanETL))

barplot(pct_contrib$pct_contrib_MES, cex.names=0.8, las=3, col="lightblue")




