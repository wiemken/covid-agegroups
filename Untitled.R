###### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### cases
library(tidyverse)
library(vroom)
###### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### read data - from shinyapp
df <- vroom::vroom("/Users/timwiemken/Downloads/csv.csv")


### denominator from cenwus
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
    pop18_39 =  sum(pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29, pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39),
    pop40_64 = sum(pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49, pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64)
    )-> census

## overall

# df$rate <- ifelse(df$Age_Group%in%c("18-29 Years", "30-39 Years"), df$Corrected_Cases/denom.18_39*1000000,
#                   ifelse(df$Age_Group%in%c("40-49 Years", "50-64 Years"), df$Corrected_Cases/denom.40_64*1000000, NA
#                   ))

#num.all.18_39 <- sum(df$Corrected_Cases[df$Age_Group%in%c("18-29 Years", "30-39 Years")], na.rm=T)
denom.18_39 <- pull(census[2,"pop18_39"])

#num.all.40_64 <- sum(df$Corrected_Cases[df$Age_Group%in%c("40-49 Years", "50-64 Years")], na.rm=T)
denom.40_64 <- pull(census[2,"pop40_64"])

### to sept 11
#num.sept.18_39 <- sum(df$Corrected_Cases[df$Age_Group%in%c("18-29 Years", "30-39 Years") & df$Week=="2021-09-11"], na.rm=T)
#num.sept.40_64 <- sum(df$Corrected_Cases[df$Age_Group%in%c("40-49 Years", "50-64 Years")& df$Week=="2021-09-11"], na.rm=T)

#num.all.18_39/denom.18_39 *100000
#num.all.40_64/denom.40_64 *100000

#num.sept.18_39/denom.18_39 *100000
#num.sept.40_64/denom.40_64 *100000


#mean(df$rate[df$Age_Group%in%c("18-29 Years", "30-39 Years")])
#mean(df$rate[df$Age_Group%in%c("40-49 Years", "50-64 Years")])


#df$rate[df$Age_Group%in%c("18-29 Years", "30-39 Years") & df$Week=="2021-09-11"]


#### compute rates for 18-39 and 40-64
df1839 <- subset(df, df$Age_Group%in%c("18-29 Years", "30-39 Years"))
df4064 <- subset(df, df$Age_Group%in%c("40-49 Years", "50-64 Years"))

df1839 %>%
  group_by(Week) %>%
  summarise(
    cases = sum(Corrected_Cases)
  ) %>%
  mutate(
    rate = cases / denom.18_39 *1000000
      ) -> totals1

df4064 %>%
  group_by(Week) %>%
  summarise(
    cases = sum(Corrected_Cases)
  ) %>%
  mutate(
    rate = cases / denom.40_64 *1000000
    ) -> totals2



### average weekly rate
mean(totals1$rate)
mean(totals2$rate)

### weekly rate 18-39
1695.151*0.5 - 1695.151
847.57 * (6*4)


### rate for sept 11
totals1$rate[totals1$Week=="2021-09-11"]
totals2$rate[totals2$Week=="2021-09-11"]

### 4 numbers
### rate covid by age group for week ending sept 11.

#rate to date 




###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### SAME AS ABOVE BUT FOR HOSPITALIZATIONS
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
df.hosp <- vroom::vroom("~/Desktop/hosp.csv") ## this comes from web app.. broken right now so manual

### must run census from load_data_hospitaizations
max(df.hosp$week) ### 10-17-2021 end
#### compute rates for 18-39 and 40-64
df1839_hosp <- subset(df.hosp, df.hosp$age_group%in%c("18-29 Years", "30-39 Years"))
df4064_hosp <- subset(df.hosp, df.hosp$age_group%in%c("40-49 Years", "50-64 Years"))


### use same census pop for age groups as above - manual code here. 
df1839_hosp %>%
  group_by(week) %>%
  summarise(
    cases = sum(cases)
  ) %>%
  mutate(
    rate = cases / 99202074 *1000000
  ) -> totals1_hosp

df4064_hosp %>%
  group_by(week) %>%
  summarise(
    cases = sum(cases)
  ) %>%
  mutate(
    rate = cases / 103816069 *1000000
  ) -> totals2_hosp


### 18-39 denom = 99202074
### 40-=64 denom = 103816069

### average weekly rate
mean(totals1_hosp$rate)
mean(totals2_hosp$rate)

totals1_hosp$rate[totals1_hosp$week=="2021-09-12"]
totals2_hosp$rate[totals2_hosp$week=="2021-09-12"]

#write.table(df_shiny_hosp, "~/Desktop/hosp.csv", sep=",", row.names=F, na="")

#### 18-39
abs(((42.5*0.1) - (42.1*.33))*24)

### 40-64
abs(((104.3002*0.1) - (104.3002*.33))*24)

### worst case sept 12
#### 18-39
abs(((55.03169*0.1) - (55.03169*.33))*24)

### 40-64
abs(((132.0275*0.1) - (132.0275*.33))*24)


