#Script will create function to create color or BW output of age groups selected
#------------------------------#
#Add % difference plot
#------------------------------#
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

############################################################
#################### CASE DATA #############################
############################################################
### Load COVID-19 case data scraping CDC PowerBI table from: https://covid.cdc.gov/covid-data-tracker/#demographicsovertime
### data was scraped manually into an excel file and uploaded to github
cases <-rio::import("https://github.com/wiemken/covid-agegroups/blob/main/case_data.xlsx?raw=true")
cases %>%
      mutate(year = lubridate::year(week)) ->cases

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

#Pivot to long format for plotting
df %>%
      pivot_longer(cols = starts_with("age"), names_to = "age_group", values_to = "cases") -> df

#Factor age group to ensure proper arrangement
df$age_group <- factor(df$age_group, levels = c("age_0_4", "age_5_11", "age_12_15", "age_16_17", "age_18_29", "age_30_39", "age_40_49", "age_50_64", "age_65_74", "age_75"), 
                       labels = c("0-4 Years", "5-11 Years", "12-15 Years", "16-17 Years", "18-29 Years", "30-39 Years", "40-49 Years", "50-64 Years", "65-74 Years", "≥75 Years"))
#Ensure 'week' column is date format
df$week <- as.Date(df$week)

#Compute totals for plot annotation
total04<-sum(df$cases[df$age_group=="0-4 Years"])
total511<-sum(df$cases[df$age_group=="5-11 Years"])
total1215<-sum(df$cases[df$age_group=="12-15 Years"])
total1617<-sum(df$cases[df$age_group=="16-17 Years"])
total1829<-sum(df$cases[df$age_group=="18-29 Years"])
total3039<-sum(df$cases[df$age_group=="30-39 Years"])
total4049<-sum(df$cases[df$age_group=="40-49 Years"])
total5064<-sum(df$cases[df$age_group=="50-64 Years"])
total6574<-sum(df$cases[df$age_group=="65-74 Years"])
total75<-sum(df$cases[df$age_group=="≥75 Years"])
total.overall <- sum(df$cases)
total.kidsunvax <- sum(df$cases[df$age_group%in%c("0-4 Years", "5-11 Years")])
total.kidsvax <- sum(df$cases[df$age_group%in%c("0-4 Years", "5-11 Years", "12-15 Years")])
total.adultswokidsvax <- sum(df$cases[df$age_group%nin%c("0-4 Years", "5-11 Years")])
total.adultswithkidsvax <- sum(df$cases[df$age_group%nin%c("0-4 Years", "5-11 Years", "12-15 Years")])

#Correct counts  using percent over/under estimate from CDC data overall
## https://covid.cdc.gov/covid-data-tracker/#demographics
df$corrected_cases<-NA
df$corrected_cases[df$age_group=="0-4 Years"]<-df$cases[df$age_group=="0-4 Years"]*(1-0.0242)
df$corrected_cases[df$age_group=="5-11 Years"]<-df$cases[df$age_group=="5-11 Years"]*1.0329
df$corrected_cases[df$age_group=="12-15 Years"]<-df$cases[df$age_group=="12-15 Years"]*1.094
df$corrected_cases[df$age_group=="16-17 Years"]<-df$cases[df$age_group=="16-17 Years"]*1.005
df$corrected_cases[df$age_group=="18-29 Years"]<-df$cases[df$age_group=="18-29 Years"]*1.0132
df$corrected_cases[df$age_group=="30-39 Years"]<-df$cases[df$age_group=="30-39 Years"]*(1-0.0085)
df$corrected_cases[df$age_group=="40-49 Years"]<-df$cases[df$age_group=="40-49 Years"]*1.0079
df$corrected_cases[df$age_group=="50-64 Years"]<-df$cases[df$age_group=="50-64 Years"]*1.0070
df$corrected_cases[df$age_group=="65-74 Years"]<-df$cases[df$age_group=="65-74 Years"]*(1-0.0418)
df$corrected_cases[df$age_group=="≥75 Years"]<-df$cases[df$age_group=="≥75 Years"]*(1-0.0279)

#Compute totals for plot annotation on corrected counts
# total04_corrected<-sum(df$corrected_cases[df$age_group=="0-4 Years"])
# total511_corrected<-sum(df$corrected_cases[df$age_group=="5-11 Years"])
# total1215_corrected<-sum(df$corrected_cases[df$age_group=="12-15 Years"])
# total1617_corrected<-sum(df$corrected_cases[df$age_group=="16-17 Years"])
# total1829_corrected<-sum(df$corrected_cases[df$age_group=="18-29 Years"])
# total3039_corrected<-sum(df$corrected_cases[df$age_group=="30-39 Years"])
# total4049_corrected<-sum(df$corrected_cases[df$age_group=="40-49 Years"])
# total5064_corrected<-sum(df$corrected_cases[df$age_group=="50-64 Years"])
# total6574_corrected<-sum(df$corrected_cases[df$age_group=="65-74 Years"])
# total75_corrected<-sum(df$corrected_cases[df$age_group=="≥75 Years"])
# total.overall_corrected <- sum(df$corrected_cases)
# total.kidsunvax_corrected <- sum(df$corrected_cases[df$age_group%in%c("0-4 Years", "5-11 Years")])
# total.kidsvax_corrected <- sum(df$corrected_cases[df$age_group%in%c("0-4 Years", "5-11 Years", "12-15 Years")])
# total.adultswokidsvax <- sum(df$corrected_cases[df$age_group%nin%c("0-4 Years", "5-11 Years")])
# total.adultswithkidsvax <- sum(df$corrected_cases[df$age_group%nin%c("0-4 Years", "5-11 Years", "12-15 Years")])

#------------------------------ End Cleaning ------------------------------#
#Begin functions
plotter <- function(data, corrected = F, age_groups, color = F){
      if("All" %in% age_groups){
            age_groups<-levels(data$age_group)
      }
      data <- data[data$age_group%in%c(age_groups),]
      dateVec <- seq(from = min(data$week), to = max(data$week), by = "weeks")
      if(color == T){
         if(corrected == F){yvar <- "cases"}
         if(corrected == T){yvar <- "corrected_cases"}
         data[yvar]<-round(data[yvar])
         brks <- c(seq(0, max(data[yvar]), by = (max(data[yvar])+(10000-max(data[yvar])%%10000))/15))
      p<-ggplot(data,
                aes(linetype=age_group,
                    color = age_group,
                    y=get(yvar),
                    group = 1,
                    x=week,
                    text = paste(
                       "</br>Week:", week,
                       "</br>Age:", age_group,
                       "</br>Cases:",get(yvar))
                    )) + 
            geom_line(size=0.8) +
            ylab("Number of Cases \n") +
            xlab("\nWeek Ending") +
            scale_y_continuous(label=comma, limits=c(0,max(data[yvar])), breaks= brks) +
            scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
            scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#e9cb67", "5-11 Years" = "#e3bb42", "12-15 Years" = "#dbaa2e",
                                                               "16-17 Years" = "#937926", "18-29 Years" = "#00adad", "30-39 Years" = "#007574", "40-49 Years" = "#004848", "50-64 Years" = "#fcabd5",
                                                               "65-74 Years" = "#f25ead", "≥75 Years" = "#6d1f48")) +
            scale_x_date(breaks = "2 weeks", limits = c(min(dateVec), max = max(dateVec)), expand = c(0.01428571,0.01428571)) +
            theme(
                  panel.background = element_blank(),
                  axis.line = element_line(color = "darkgray"),
                  axis.text.x = element_text(angle = 90),
                  legend.key=element_blank(),
                  legend.background=element_blank(),
                  legend.position = "right",
                  legend.key.width = unit(3, "line")
            )
      return(ggplotly(p, tooltip = c("text")))
      }
      if(color == F){
         if(corrected == F){yvar <- "cases"}
         if(corrected == T){yvar <- "corrected_cases"}
         data[yvar]<-round(data[yvar])
         brks <- c(seq(0, max(data[yvar]), by = (max(data[yvar])+(10000-max(data[yvar])%%10000))/15))
         p<-ggplot(data,
                   aes(linetype=age_group,
                       color = age_group,
                       y=get(yvar),
                       group = 1,
                       x=week,
                       text = paste(
                          "</br>Week:", week,
                          "</br>Age:", age_group,
                          "</br>Cases:",get(yvar)))) + 
            geom_line(size=0.8) +
            ylab("Number of Cases \n") +
            xlab("\nWeek Ending") +
            scale_y_continuous(label=comma, limits=c(0,max(data[yvar])), breaks=brks) +
            scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
            scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#BEBEBE", "5-11 Years" = "#C0C0C0", "12-15 Years" = "#C8C8C8",
                                           "16-17 Years" = "#D0D0D0", "18-29 Years" = "#D3D3D3", "30-39 Years" = "#D8D8D8", "40-49 Years" = "#DCDCDC", "50-64 Years" = "#E0E0E0",
                                           "65-74 Years" = "#E8E8E8", "≥75 Years" = "#F0F0F0")) +
            scale_x_date(breaks = "2 weeks", limits = c(min(dateVec), max = max(dateVec)), expand = c(0.01428571,0.01428571)) +
            theme(
               panel.background = element_blank(),
               axis.line = element_line(color = "darkgray"),
               axis.text.x = element_text(angle = 90),
               legend.key=element_blank(),
               legend.background=element_blank(),
               legend.position = "right",
               legend.key.width = unit(3, "line")
            )
         return(ggplotly(p, tooltip = c("text")))
      }
}

prepdatatable <- function(data, corrected = F, age_groups){
   names(data)<-c("Week", "0-4 Case Rate", "5-11 Case Rate", "12-15 Case Rate", "16-17 Case Rate", "18-29 Case Rate",
                  "30-39 Case Rate", "40-49 Case Rate", "50-64 Case Rate", "65-74 Case Rate", "75+ Case Rate", "Year",
                  "Age_Group", "Cases", "Corrected_Cases")
   if("All" %in% age_groups){
      age_groups<-levels(data$Age_Group)
   }
   if(corrected == F){
      data<-subset(data, select = -c(Corrected_Cases))
      data <- data[data$Age_Group%in%c(age_groups),]
      colz <- substr(age_groups, 1,3) 
      data %>%
         select(Week, starts_with(colz), Year, Age_Group, Cases) %>%
         mutate(Cases = round(Cases)) -> data
      return(data)
   }
   if(corrected == T){
      data<-subset(data, select = -c(Cases))
      data <- data[data$Age_Group%in%c(age_groups),]
      colz <- substr(age_groups, 1,3) 
      data %>%
         select(Week, starts_with(colz), Year, Age_Group, Corrected_Cases) %>%
         mutate(Corrected_Cases = round(Corrected_Cases)) -> data
      return(data)
   }
}



