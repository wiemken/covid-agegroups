################################################################################
################################################################################
################  COVID PEDIATRIC DEATHS FROM CDC ##############################
################## October 4, 2021 ######################################
################################################################################

library(tidyverse)
library(RSocrata)
library(janitor)

url <- "https://data.cdc.gov/resource/nr4s-juj3.csv"
df2 <- read.socrata(
  url,
  app_token = 'chCxsk4zel6QXbaemotF65C9L',
  email     = "tim.wiemken@gmail.com",
  password  = "ThisIsNotAGoodP@ssw0rd!!!")



