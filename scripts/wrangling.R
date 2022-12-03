library(dplyr)
library(tidyr)
my_df <- readRDS(here::here("data","raw.RDS"))
my_df %>% glimpse()
my_df_pca <- 
my_df %>% 
  select(ticker,ref_date,ret_adjusted_prices) %>% 
  pivot_wider(names_from = ticker,
              values_from = ret_adjusted_prices) %>% 
  filter(!is.na(B3SA3.SA))  
my_df_clust <- 
  my_df %>% 
    select(ticker,ref_date,ret_adjusted_prices) %>% 
    pivot_wider(names_from = ref_date,
                values_from = ret_adjusted_prices) %>% 
    janitor::remove_empty("cols")


my_df_pca %>% saveRDS(here::here("data","cleaned.RDS"))

my_df_clust %>% saveRDS(here::here("data","cleaned_clust.RDS"))
