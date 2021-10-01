library(RSocrata)
library(janitor)
library(tidyverse)

url <- "https://data.cdc.gov/resource/9bhg-hcku.csv"

df <- read.socrata(
  url,
  app_token = 'chCxsk4zel6QXbaemotF65C9L',
  email     = "tim.wiemken@gmail.com",
  password  = "ThisIsNotAGoodP@ssw0rd!!!")


df %>%
  filter(state == "United States" & 
           sex == "All Sexes" & 
           age_group%in%c("Under 1 year", "1-4 years", "5-14 years")) -> df
  