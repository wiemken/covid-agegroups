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


## covid hosp: covid.cdc.gov/covid-data-trackjer/#new-hospital-admissions - extract with text sniper into csv
df <- read_excel("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/COVID pediatric case count/count github/covid-agegroups/ped hosp.xlsx")
### flu hospitalizations:  https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html
df <- vroom::vroom("https://raw.githubusercontent.com/wiemken/covid-agegroups/main/FluSurveillance_Custom_Download_Data.csv")

df %>%
  janitor::clean_names() -> df

table(df$age_category)
df %>%
  filter(catchment == "Entire Network" & 
           network == "FluSurv-NET" & 
           age_category == "< 18" &
           sex_category == "Overall" &
           race_category == "Overall") -> df

pop <- 72822113

df <- data.frame(df)
df <- df[-c(1:5),]
df <- subset(df, df$mmwr_week!=53)
row.names(df) <- seq(1:nrow(df))

df$cases <- round(as.numeric(df$cumulative_rate)*pop/100000,0)
keep <- seq(0,360, by=30)[-1]
mean(df[keep,"cases"], na.rm=T)
  
        