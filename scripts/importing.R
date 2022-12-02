library(dplyr)
library(yfR)

## idiv composition
idiv <- read.csv2(here::here("data","IDIVDia_01-12-22.csv"),
                  header=TRUE,skip = 1,sep = ";",dec = ",",
                  fileEncoding = "Latin1") %>% 
                  janitor::clean_names() %>% 
                  tibble()

## idiv composition
idiv <- 
idiv %>% 
  mutate(codigo = paste0(codigo,".SA"))
idiv_v <- idiv %>% pull(codigo)

idiv_data <- yf_get(
  idiv_v,
  first_date = Sys.Date() - 360,
  last_date = Sys.Date()
)
idiv_data %>% saveRDS(here::here("data","raw.RDS"))
