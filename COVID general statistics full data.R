#######################################################################
#######################################################################
############# GENERAL STATISTICS FROM FULL DATSET #####################
############ October 4, 2021 ##########################################
#######################################################################

library(tidyverse)
library(janitor)
library(vroom)
library(zoo)

### data: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4
df <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/Public Datasets/COVID/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

df %>%
  filter(age_group != "Missing",
         hosp_yn %in%c("Yes", "No")) -> john

john %>%
  group_by(age_group) %>%
  summarise(
    total.cases = n(),
    hosp = tally(hosp_yn=="Yes") -> test
  )


#### figure out pediatric case increase before/after current 2 months
### analysis date Oct 4, 2021 (also downloaded data on this date)
### create before/after 2 months ago and current 2 months
### convert date to yearmon
df %>%
  mutate(
    moyr = zoo::as.yearmon(case_month),
    cuts = ifelse(moyr < "2021-08", "before", "after")
  ) -> df
  
### group and perform summary stats
df %>%
  group_by(
    cuts, age_group
    ) %>%
  summarise(
    cases = n()
    ) -> test

### total cases before/after
df %>%
  group_by(cuts) %>%
  summarise(n())

## after peds :
909299/4045815
# 0.2247505
## before peds:
3554571/28760859
# 0.1235906

### if just jun/jul (before) to aug/sep (after)
## after is same.
df %>%
  filter(moyr >= "2021-06") %>%
  group_by(cuts) %>%
  summarise(n()) # 1448652 - total cases after

df %>%
  filter(moyr >= "2021-06",
         age_group%in%c("0 - 17 years")) %>%
  group_by(cuts) %>%
  summarise(n())

234420/1073634

###########################################################################
###########################################################################
###########################################################################
### compute increase in pediatric cases over past 2 months
###########################################################################
###########################################################################
df <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/Public Datasets/COVID/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")
names(df)


#### figure out pediatric case increase before/after current 2 months
### analysis date Oct 4, 2021 (also downloaded data on this date)
### create before/after 2 months ago and current 2 months
### convert date to yearmon
df %>%
  filter(age_group == "0 - 17 years"
         ) %>%
  mutate(
    moyr = zoo::as.yearmon(case_month),
    ) %>%
  filter(moyr >= "2021-06"
         ) %>%
  mutate(
    cuts = ifelse(moyr <= "2021-07", "Prior 2 Months", "Most Recent 2 Months")
     ) -> test

### group and perform summary stats
test %>%
  group_by(
    cuts
  ) %>%
  summarise(
    cases = n()
  ) -> out

### percent increase
(pull(out[1,]) - pull(out[2,])) /pull(out[2,]) *100



###########################################################################
###########################################################################
### hospitalizations
### data extract from : https://covid.cdc.gov/covid-data-tracker/#new-hospital-admissions
### manual extraction of rate into excel file
###########################################################################
###########################################################################
df <- readxl::read_excel("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/covid-agegroups/ped hosp.xlsx")

### population estimates in 2020 from: https://datacenter.kidscount.org/data/tables/101-child-population-by-age-group#detailed/1/any/false/574,1729,37,871,870,573,869,36,868,867/62,63,64,6,4693/419,420
## 0-1, 5-11, 12-14, 15-17
pop.2020.0_17 <- 19301292 + 28384878 + 12607256 + 12528687

df$cases <- round(df$rate_per_100000*pop.2020.0_17/100000,0)

df$moyr <- zoo::as.yearmon(as.Date(df$report_date))

df %>%
  filter(moyr >="2021-08") %>%
  summarise(
    total.cases = sum(cases)
  )  # 17879

17879/62566 #(total hospitalizations from : https://covid.cdc.gov/covid-data-tracker/#new-hospital-admissions  on Oct 4, 2021
  



