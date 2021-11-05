### cases

df <- vroom::vroom("/Users/timwiemken/Downloads/csv.csv")


### denom
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
num.all.18_39 <- sum(df$Corrected_Cases[df$Age_Group%in%c("18-29 Years", "30-39 Years")], na.rm=T)
denom.18_39 <- pull(census[2,"pop18_39"])

num.all.40_64 <- sum(df$Corrected_Cases[df$Age_Group%in%c("40-49 Years", "50-64 Years")], na.rm=T)
denom.40_64 <- pull(census[2,"pop40_64"])

### to sept 11
num.sept.18_39 <- sum(df$Corrected_Cases[df$Age_Group%in%c("18-29 Years", "30-39 Years") & df$Week<="2021-09-11"], na.rm=T)
num.sept.40_64 <- sum(df$Corrected_Cases[df$Age_Group%in%c("40-49 Years", "50-64 Years")& df$Week<="2021-09-11"], na.rm=T)

num.all.18_39/denom.18_39 *100000
num.all.40_64/denom.40_64 *100000

num.sept.18_39/denom.18_39 *100000
num.sept.40_64/denom.40_64 *100000


max(df$Week)


### 4 numbers
### rate covid by age group for week ending sept 11.

#rate to date 
