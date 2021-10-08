################################
### VRBPAC MEETING EXTRA DATA
###############################

## 1) Percent increase in cases 5-11 jun/jul 2021 to Aug/Sep 2021

#Libraries
library(dplyr)
library(janitor)
library(vroom)
library(RSocrata)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(tidyr)
library(rio)
library(Hmisc)
library(scales)

#Load in US Census data estimates from: https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
census <- vroom("https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv")

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
    pop12_15 = sum(pop_12, pop_13, pop_14, pop_15),
    pop16_17 = sum(pop_16, pop_17),
    pop18_29 = sum(pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29),
    pop30_39 = sum(pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39),
    pop40_49 = sum(pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49),
    pop50_64 = sum(pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64),
    pop65_74 = sum(pop_65, pop_66, pop_67, pop_68, pop_69, pop_70, pop_71, pop_72, pop_73, pop_74),
    pop75 = sum(pop_75, pop_76, pop_77, pop_78, pop_79, pop_80, pop_81, pop_82, pop_83, pop_84, pop_85, pop_86, pop_87, pop_88, pop_89, pop_90, pop_91, pop_92, pop_93, pop_94, pop_95, pop_96, pop_97, pop_98, pop_99, pop_100),
    pop_unvax = sum(pop_0, pop_1, pop_2, pop_3, pop_4, pop_5, pop_6, pop_7, pop_8, pop_9, pop_10, pop_11),
    pop_vax = sum(pop_12, pop_13, pop_14, pop_15, pop_16, pop_17,  pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29, pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39,pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49,pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64,pop_65, pop_66, pop_67, pop_68, pop_69, pop_70, pop_71, pop_72, pop_73, pop_74, pop_75, pop_76, pop_77, pop_78, pop_79, pop_80, pop_81, pop_82, pop_83, pop_84, pop_85, pop_86, pop_87, pop_88, pop_89, pop_90, pop_91, pop_92, pop_93, pop_94, pop_95, pop_96, pop_97, pop_98, pop_99, pop_100)
  )  -> census


### Load COVID-19 case data scraping CDC PowerBI table from: https://covid.cdc.gov/covid-data-tracker/#demographicsovertime
### data was scraped manually into an excel file and uploaded to github
cases <-rio::import("https://github.com/wiemken/covid-agegroups/blob/main/case_data.xlsx?raw=true")
cases %>%
  mutate(year = lubridate::year(week)) ->cases

cases %>%
  pivot_longer(cols = starts_with("a"), names_to = "age_group", values_to = "cases") ->> df_shiny_rate

#Factor age group to ensure proper arrangement
df_shiny_rate$age_group <- factor(df_shiny_rate$age_group, levels = c("a0_4", "a5_11", "a12_15", "a16_17", "a18_29", "a30_39", "a40_49", "a50_64", "a65_74", "a75"), 
                                  labels = c("0-4 Years", "5-11 Years", "12-15 Years", "16-17 Years", "18-29 Years", "30-39 Years", "40-49 Years", "50-64 Years", "65-74 Years", "≥75 Years"))
#Ensure 'week' column is date format
df_shiny_rate$week <- as.Date(df_shiny_rate$week)

############################################################
#################### EDIT  #################################
############################################################
### split into 2020 and 2021 to account for different population estimates

cases_2020 <- subset(cases, cases$year==2020)
cases_2021 <- subset(cases, cases$year==2021)

### compute cases for 2020 and 2021 separately
### to compute, multiply age-specific rate per 100,000 from CDC PowerBI by census population for age group and divide by 100,000
### output is estimated number of cases per age group
cases_2020 %>%
  mutate(age_0_4 = a0_4 * census$pop0_4[1]/100000,
         age_5_11 = a5_11 * census$pop5_11[1]/100000,
         age_12_15 = a12_15 * census$pop12_15[1]/100000,
         age_16_17 = a16_17 * census$pop16_17[1]/100000,
         age_18_29 = a18_29 * census$pop18_29[1]/100000,
         age_30_39 = a30_39 * census$pop30_39[1]/100000,
         age_40_49 = a40_49 * census$pop40_49[1]/100000,
         age_50_64 = a50_64 * census$pop50_64[1]/100000,
         age_65_74 = a65_74 * census$pop65_74[1]/100000,
         age_75 = a75 * census$pop75[1]/100000
  ) -> cases_2020
cases_2021 %>%
  mutate(age_0_4 = a0_4 * census$pop0_4[2]/100000,
         age_5_11 = a5_11 * census$pop5_11[2]/100000,
         age_12_15 = a12_15 * census$pop12_15[2]/100000,
         age_16_17 = a16_17 * census$pop16_17[2]/100000,
         age_18_29 = a18_29 * census$pop18_29[2]/100000,
         age_30_39 = a30_39 * census$pop30_39[2]/100000,
         age_40_49 = a40_49 * census$pop40_49[2]/100000,
         age_50_64 = a50_64 * census$pop50_64[2]/100000,
         age_65_74 = a65_74 * census$pop65_74[2]/100000,
         age_75 = a75 * census$pop75[2]/100000
  ) -> cases_2021

#Bind together 2020 and 2021 
df <- rbind(cases_2020, cases_2021)

df$moyr <- zoo::as.yearmon(as.Date(df$week))

### total cases 5-11
sum(df$age_5_11[-nrow(df)])
# 1,711,164
# 1,653,603  ## without 

1700000/28368818*100000  ##incidence 
  
## total cases 5-11 Aug/sep
new <- df[75:82,]
  sum(new$age_5_11)
  ## 519291.2
  
## same for jun jul
old<- df[66:74,]
  sum(old$age_5_11) 
  # 91092.27
  
#pct increase 
  
  ((519291.2 - 91092.27) / 91092.27) *100
  
#470.0716 % increase


## 2) Percent covid cases 5-1 aug/sep

  ## same data as above for "new" versus total
  
  ## total
new <- data.frame(new)

new$rowsumz <- rowSums(new[,c(13:22)])

sum(new$rowsumz) ## this is total cases

519291.2/5332439    ## 519291.2 computed in #1 as total cases in 5-11 in aug/sep
# 0.09738343 = 10%



## 3) of all <18, what % is 5-11

## same data

total_lt_18 <- rowSums(new[,13:16])

sum(total_lt_18)  ### total <18 aug/sep 1261500

519291.2/1261500  #519291.2 from #1 as total 5-11 aug/sep
## 0.4116458 = 41%



### 4) prop peds hospitlaizations through end of sep 2021 in 5-11


#Load in US Census data estimates from: https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
census <- vroom("https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv")

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
  )  -> census


df <- vroom("https://raw.githubusercontent.com/wiemken/covid-agegroups/main/COVID-19Surveillance_All_Data%20copy.csv")
df %>%
  janitor::clean_names() %>%
  filter(catchment == "Entire Network",
         network == "COVID-NET",
         sex == "Overall",
         race == "Overall") %>%
  mutate(
    weekly_rate = as.numeric(as.character(weekly_rate))
                             )-> cases

cases %>%
  select(-year) %>%
  rename(
    year = mmwr_year
  ) %>%
  filter(age_category %in% c("0-4 yr", "5-11  yr", "12-17 yr", 
                             "18-29 yr", "30-39 yr", "40-49 yr",
                             "50-64 yr", "65-74 yr", "75-84 yr", 
                             "85+")) -> df_shiny_rate

#Factor age group to ensure proper arrangement
df_shiny_rate$age_group <- factor(df_shiny_rate$age_category, 
                                  levels = c("0-4 yr", "5-11  yr", "12-17 yr", "18-29 yr", "30-39 yr", "40-49 yr", "50-64 yr", "65-74 yr", "75-84 yr", "85+"), 
                                  labels = c("a0_4Years", "a5_11Years", "a12_17Years", "a18_29Years", "a30_39Years", "a40_49Years", "a50_64Years", "a65_74Years", "a75_84Years", "a85Years"))

############################################################
#################### EDIT  #################################
############################################################
### split into 2020 and 2021 to account for different population estimates

cases_2020 <- subset(df_shiny_rate, df_shiny_rate$year==2020)
cases_2021 <- subset(df_shiny_rate, df_shiny_rate$year==2021)

### compute cases for 2020 and 2021 separately
### to compute, multiply age-specific rate per 100,000 from CDC PowerBI by census population for age group and divide by 100,000
### output is estimated number of cases per age group
cases_2020 %>%
  pivot_wider(names_from = age_group, values_from = weekly_rate) -> cases_2020
cases_2021 %>%
  pivot_wider(names_from = age_group, values_from = weekly_rate) -> cases_2021

cases_2020 %>%
  mutate(age_0_4 = a0_4Years * census$pop0_4[1]/100000,
         age_5_11 = a5_11Years * census$pop5_11[1]/100000,
         age_12_17 = a12_17Years * census$pop12_17[1]/100000,
         age_18_29 = a18_29Years * census$pop18_29[1]/100000,
         age_30_39 = a30_39Years * census$pop30_39[1]/100000,
         age_40_49 = a40_49Years * census$pop40_49[1]/100000,
         age_50_64 = a50_64Years * census$pop50_64[1]/100000,
         age_65_74 = a65_74Years * census$pop65_74[1]/100000,
         age_75_84 = a75_84Years * census$pop75_84[1]/100000,
         age_85 = a85Years * census$pop85[1]/100000
  ) -> cases_2020
cases_2021 %>%
  mutate(age_0_4 = a0_4Years * census$pop0_4[2]/100000,
         age_5_11 = a5_11Years * census$pop5_11[2]/100000,
         age_12_17 = a12_17Years * census$pop12_17[2]/100000,
         age_18_29 = a18_29Years * census$pop18_29[2]/100000,
         age_30_39 = a30_39Years * census$pop30_39[2]/100000,
         age_40_49 = a40_49Years * census$pop40_49[2]/100000,
         age_50_64 = a50_64Years * census$pop50_64[2]/100000,
         age_65_74 = a65_74Years * census$pop65_74[2]/100000,
         age_75_84 = a75_84Years * census$pop75_84[2]/100000,
         age_85 = a85Years * census$pop85[2]/100000
  ) -> cases_2021

#Bind together 2020 and 2021 
df <- rbind(cases_2020, cases_2021)

df$cases <- as.numeric(as.character(apply(df[,c(19:28)], 1, function(x) paste(x[!is.na(x)], collapse = ", "))))

### total cases 5-11
sum(df$cases[df$age_category == "5-11  yr"], na.rm=T)
### total cases
sum(df$cases, na.rm=T)
8622.211/2398703 ## this number is smaller but reasonably close to website.
#0.00359453

8622.211/28368818 *100000 ### incidence rate (pop from 2021 census file)

## pct cases in past 2 months
2213 / 8622.211 ## 26%


#week numbers aug/sep 2021 = 31:39 from: https://www.epochconverter.com/weeks/2021
## jun/jul 2021 = 23:30
## total cases 5-11 Aug/sep
sum(df$cases[df$age_category == "5-11  yr" & df$mmwr_week %in%c(31:39) & df$year==2021])

## total 5-11 in aug/sep= 2212.768

sum(df$cases[df$mmwr_week %in%c(31:39) & df$year==2021])

## total in aug sep: 310773.9

## prop 5-11 in aug sep= 
2212.768 / 310773.9 #  0.007120186 = .7%

## prop hosp of <18 in aug sep
sum(df$cases[df$age_category%in%c("0-4 yr", "5-11  yr", "12-17 yr") & df$mmwr_week %in%c(31:39) & df$year==2021])

# 9428.536 total hosp  in au sep <18
2212.768 / 9428.536 # = 0.2346884, 23%




## same jun/jul
sum(df$cases[df$age_category == "5-11  yr" & df$mmwr_week %in%c(23:30) & df$year==2021])

## total 5-11 in jun/jul= 539.0075

sum(df$cases[df$mmwr_week %in%c(23:30) & df$year==2021])

## total in jun/jul: 80988.8

## prop 5-11 in jun/jul= 
539.0075 / 80988.8 #  0.006655334 = .7%

## prop hosp of <18 in jun/jul
sum(df$cases[df$age_category%in%c("0-4 yr", "5-11  yr", "12-17 yr") & df$mmwr_week %in%c(23:30) & df$year==2021])

# 2438.471 total hosp  in jun/jul<18
539.0075 / 2438.471 # = 0.2210432, 22%



### pct increase jun/jul to aug sep

(2212.768 - 539.0075) / 2212.768 ## 0.7564103 = 75%




