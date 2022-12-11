my_df <- readRDS(here::here("data","cleaned.RDS")) 
my_df
quantile(my_df$B3SA3.SA,0.95)
mean(my_df$B3SA3.SA[my_df$B3SA3.SA > quantile(my_df$B3SA3.SA, 0.95)])
ESnorm(0.95)
library(dplyr)
library(PerformanceAnalytics)
my_df_d <- as.data.frame(my_df)
row.names(my_df_d) <- my_df_d$ref_date
my_df_d <- 
  my_df_d %>% select(-ref_date)
var_historical <- VaR(R = my_df_d,
                      p = 0.99, method = "historical")

var_historical

chart.VaRSensitivity(R = my_df_d %>% select(B3SA3.SA),
                    methods = c("GaussianVaR",
                                "HistoricalVaR"))

var_gaussian <- VaR(R = my_df_d, p = 0.95, method = "gaussian")
var_historical <- VaR(R = my_df_d, p = 0.95, method = "historical")

returns_tbl <- timetk::tk_tbl(my_df_d,
                      preserve_index = TRUE,
                      rename_index = "date")
library(ggplot2)
ggplot(returns_tbl, aes(x = B3SA3.SA))+
  geom_density(color = "darkblue",
               linewidth = .8)+
  geom_vline(xintercept = var_historical[,"B3SA3.SA"],
             linetype = "dotted",
             color = "red",
             linewidth = 1)+
  annotate(geom = "text",
           x = var_historical[,"B3SA3.SA"],
           y = 6,
           label = paste("99% Historical VaR"),
           color = "red",
           angle = 90,
           alpha = .8,
           vjust = -1.75)+
  geom_vline(xintercept = var_gaussian[,"B3SA3.SA"],
             linetype = "dotted",
             color = "black",
             linewidth = 1)+
  annotate(geom = "text",
           x = var_gaussian[,"B3SA3.SA"],
           y = 6,
           label = paste("95% Gaussian VaR"),
           color = "black",
           angle = 90,
           alpha = .8,
           vjust = -1)+
  theme_minimal()+
  labs(title = "Densidade de retornos da B3SA3",
       subtitle = "99% Historical Var e 95% Gaussian VaR",
       x = "",
       y = "")
