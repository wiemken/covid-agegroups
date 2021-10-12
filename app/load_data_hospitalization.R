library(rio)
library(tidyverse)
library(vroom)
library(MMWRweek)

#Load in US Census data estimates from: https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
#census_hosp <<- vroom::vroom("https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv")
census <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/covid-agegroups/app/census.csv")

### Clean names with janitor::clean_names
### sex, origin, race == 0 represents all categories combined
### keep only 2020/2021
### compute population sums by age groups
census %>%
  janitor::clean_names() %>%
  filter(sex == 0, origin==0, race==0, year %in%c(2020, 2021)) %>%
  rowwise() %>%
  mutate(
    pop0_4 = sum(pop_0, pop_1, pop_2, pop_3, pop_4),
    pop5_11 = sum(pop_5, pop_6, pop_7, pop_8, pop_9, pop_10, pop_11),
    pop12_17 = sum(pop_12, pop_13, pop_14, pop_15, pop_16, pop_17),
    pop18_29 = sum(pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29),
    pop30_39 = sum(pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39),
    pop40_49 = sum(pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49),
    pop50_64 = sum(pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64),
    pop65_74 = sum(pop_65, pop_66, pop_67, pop_68, pop_69, pop_70, pop_71, pop_72, pop_73, pop_74),
    pop75_84 = sum(pop_75, pop_76, pop_77, pop_78, pop_79, pop_80, pop_81, pop_82, pop_83, pop_84), 
    pop85 = sum(pop_85, pop_86, pop_87, pop_88, pop_89, pop_90, pop_91, pop_92, pop_93, pop_94, pop_95, pop_96, pop_97, pop_98, pop_99, pop_100),
    pop_unvax = sum(pop_0, pop_1, pop_2, pop_3, pop_4, pop_5, pop_6, pop_7, pop_8, pop_9, pop_10, pop_11),
    pop_vax = sum(pop_12, pop_13, pop_14, pop_15, pop_16, pop_17,  pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29, pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39,pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49,pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64,pop_65, pop_66, pop_67, pop_68, pop_69, pop_70, pop_71, pop_72, pop_73, pop_74, pop_75, pop_76, pop_77, pop_78, pop_79, pop_80, pop_81, pop_82, pop_83, pop_84, pop_85, pop_86, pop_87, pop_88, pop_89, pop_90, pop_91, pop_92, pop_93, pop_94, pop_95, pop_96, pop_97, pop_98, pop_99, pop_100)
  )  -> census_hosp

### hosp data from: https://covid.cdc.gov/covid-data-tracker/#covidnet-hospitalization-network
### need to manually download and upload to github - cdc download link has a token that changes
### skip first two lines b/c download has junk header
df_hosp <- vroom::vroom("https://raw.githubusercontent.com/wiemken/covid-agegroups/main/COVID-19Surveillance_All_Data.csv", skip=2)
df_hosp %>%
  janitor::clean_names() %>%
  filter(catchment == "Entire Network",
         network == "COVID-NET",
         sex == "Overall",
         race == "Overall") %>%
  mutate(
    weekly_rate = as.numeric(as.character(weekly_rate))
  )-> cases_hosp

cases_hosp %>%
  select(-year) %>%
  rename(
    year = mmwr_year
  ) %>%
  filter(age_category %in% c("0-4 yr", "5-11  yr", "12-17 yr", 
                             "18-29 yr", "30-39 yr", "40-49 yr",
                             "50-64 yr", "65-74 yr", "75-84 yr", 
                             "85+")) -> df_shiny_rate_hosp

#Factor age group to ensure proper arrangement
df_shiny_rate_hosp$age_group <- factor(df_shiny_rate_hosp$age_category, 
                                  levels = c("0-4 yr", "5-11  yr", "12-17 yr", "18-29 yr", "30-39 yr", "40-49 yr", "50-64 yr", "65-74 yr", "75-84 yr", "85+"), 
                                  labels = c("a0_4", "a5_11", "a12_17", "a18_29", "a30_39", "a40_49", "a50_64", "a65_74", "a75_84", "a85"))

## create week ending
df_shiny_rate_hosp$week <- MMWRweek::MMWRweek2Date(MMWRyear=df_shiny_rate_hosp$year,MMWRweek=df_shiny_rate_hosp$mmwr_week)

df_shiny_rate_hosp<- df_shiny_rate_hosp[,c("week", "year", "age_group", "cases" = "weekly_rate")]


# df_shiny_rate_hosp$age_group <- factor(df_shiny_rate_hosp$age_group, 
#                                        levels = c("a0_4", "a5_11", "a12_17", "a18_29", "a30_39", "a40_49", "a50_64", "a65_74", "a75_84", "a85"),
#                                        labels = c("0-4 Years", "5-11 Years", "12-17 Years", "18-29 Years", "30-39 Years", "40-49 Years", "50-64 Years", "65-74 Years", "75-84 Years", "85+ Years"))

############################################################
#################### EDIT  #################################
############################################################
### split into 2020 and 2021 to account for different population estimates

cases_2020_hosp <- subset(df_shiny_rate_hosp, df_shiny_rate_hosp$year==2020)
cases_2021_hosp <- subset(df_shiny_rate_hosp, df_shiny_rate_hosp$year==2021)

### compute cases for 2020 and 2021 separately
### to compute, multiply age-specific rate per 100,000 from CDC PowerBI by census population for age group and divide by 100,000
### output is estimated number of cases per age group
cases_2020_hosp %>%
  pivot_wider(names_from = age_group, values_from = weekly_rate) -> cases_2020_hosp
cases_2021_hosp %>%
  pivot_wider(names_from = age_group, values_from = weekly_rate) -> cases_2021_hosp



cases_2020_hosp %>%
  mutate(age_0_4 = a0_4 * census_hosp$pop0_4[1]/100000,
         age_5_11 = a5_11 * census_hosp$pop5_11[1]/100000,
         age_12_17 = a12_17 * census_hosp$pop12_17[1]/100000,
         age_18_29 = a18_29 * census_hosp$pop18_29[1]/100000,
         age_30_39 = a30_39 * census_hosp$pop30_39[1]/100000,
         age_40_49 = a40_49 * census_hosp$pop40_49[1]/100000,
         age_50_64 = a50_64 * census_hosp$pop50_64[1]/100000,
         age_65_74 = a65_74 * census_hosp$pop65_74[1]/100000,
         age_75_84 = a75_84 * census_hosp$pop75_84[1]/100000,
         age_85 = a85 * census_hosp$pop85[1]/100000
  ) -> cases_2020_hosp
cases_2021_hosp %>%
  mutate(age_0_4 = a0_4 * census_hosp$pop0_4[2]/100000,
         age_5_11 = a5_11 * census_hosp$pop5_11[2]/100000,
         age_12_17 = a12_17 * census_hosp$pop12_17[2]/100000,
         age_18_29 = a18_29 * census_hosp$pop18_29[2]/100000,
         age_30_39 = a30_39 * census_hosp$pop30_39[2]/100000,
         age_40_49 = a40_49 * census_hosp$pop40_49[2]/100000,
         age_50_64 = a50_64 * census_hosp$pop50_64[2]/100000,
         age_65_74 = a65_74 * census_hosp$pop65_74[2]/100000,
         age_75_84 = a75_84 * census_hosp$pop75_84[2]/100000,
         age_85 = a85 * census_hosp$pop85[2]/100000
  ) -> cases_2021_hosp

#Bind together 2020 and 2021 
df_hosp <- rbind(cases_2020_hosp, cases_2021_hosp)

#Pivot to long format for plotting
df_hosp %>%
  pivot_longer(cols = starts_with("age"), names_to = "age_group", values_to = "cases") ->> df_shiny_hosp

#Factor age group to ensure proper arrangement
df_shiny_hosp$age_group <- factor(df_shiny_hosp$age_group, levels = c("age_0_4", "age_5_11", "age_12_17", "age_18_29", "age_30_39", "age_40_49", "age_50_64", "age_65_74", "age_75_84", "age_85"), 
                             labels = c("0-4 Years", "5-11 Years", "12-17 Years", "18-29 Years", "30-39 Years", "40-49 Years", "50-64 Years", "65-74 Years", "75-84 Years", "85+ Years"))
#Ensure 'week' column is date format
df_shiny_hosp$week <- as.Date(df_shiny_hosp$week)

df_shiny_hosp$corrected_cases<-df_shiny_hosp$cases

