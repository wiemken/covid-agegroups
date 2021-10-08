#######################################################################
#######################################################################
############# Maps FROM FULL DATSET #####################
############ October 6, 2021 ##########################################
#######################################################################

library(tidyverse)
library(janitor)
library(vroom)
library(zoo)
library(leaflet)
#remotes::install_github("hrbrmstr/albersusa")
library(albersusa)
library(sf)
library(tidycensus)
#tidycensus::census_api_key("1bea7542b64a438650b457bd6609c1d7bd75cbaa", install=T)
#readRenviron("~/.Renviron")

###==============================================================
###==============================================================
### LOAD DATA
### data: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4
df <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/Public Datasets/COVID/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")
###==============================================================
###==============================================================

###==============================================================
###==============================================================
### GET POPULATION BY STATE FOR RATE CALCULATION
### HAD TO USE CIVILIAN POP AS RESIDENT POP BY SINGLE YEAR OF AGE ISNT AVAILBLE
pops <- vroom("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-agesex-civ.csv")
pops %>%
  janitor::clean_names() -> pops
### convert state name to abbreviation for merging
pops$name <- state.abb[match(pops$name,state.name)]
### select all sexes and drop full US (want state only), 
###  select only columns of interest
### keep only age of interest to thin out
pops %>%
  filter(sex==0,
         !is.na(name),
         age <18
         ) %>%
  select(name, age, popest2019_civ) -> pops

### sum cases by age group of interest; 0-4, 5-11, 12-15, 16-17
pops %>%
  mutate(
    age_ = ifelse(age <=4, 1, 
                  ifelse(age >=5 & age <=11,2,
                         ifelse(age >=12 & age <=15,3,4)))
  ) %>%
  group_by(name, age_) %>%
  summarise(
    pops = sum(popest2019_civ)
  ) -> pops2


###==============================================================
###==============================================================




###==============================================================
###==============================================================
### DATA MANAGEMENT
df$age_group <- factor(df$age_group)
###==============================================================
###==============================================================


###==============================================================
###==============================================================
### GET TOTAL CASES BY STATE
df %>%
  group_by(res_state) %>%
  summarise(total_cases = n()) -> cases

cases <- merge(cases, pops2)
###==============================================================
###==============================================================

###==============================================================
###==============================================================
### PULL MAP
map <- albersusa::usa_sf()
map$iso_3166_2 <- as.character(map$iso_3166_2)
###==============================================================
###==============================================================


###==============================================================
### MERGE MAP AND DATA
###==============================================================
map %>%
  left_join(cases, by=c("iso_3166_2" = "res_state")) -> mapme
###==============================================================
###==============================================================




