library(dplyr)
library(tidyr)
my_df <- readRDS(here::here("data","raw.RDS"))
my_df %>% glimpse()
my_df <- 
my_df %>% 
  select(ticker,ref_date,ret_adjusted_prices) %>% 
  pivot_wider(names_from = ticker,
              values_from = ret_adjusted_prices) %>% 
  filter(!is.na(B3SA3.SA))  

my_df %>% saveRDS(here::here("data","cleaned.RDS"))
