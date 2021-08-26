library(dplyr)
library(janitor)
library(vroom)
library(RSocrata)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(tidyr)

############################################################
#################### CENSUS DATA ###########################
############################################################

#### CENSUS DATA: https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
census <- vroom("https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv")

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

cases <- readxl::read_excel("/Users/timothywiemken/Dropbox/Work/pfizer/covid epi/case by age 2021/case_data.xlsx")
cases %>%
  mutate(year = lubridate::year(week),) ->cases

############################################################
#################### EDIT  #################################
############################################################

cases_2020 <- subset(cases, cases$year==2020)
cases_2021 <- subset(cases, cases$year==2021)

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

df <- rbind(cases_2020, cases_2021)

roundme<-names(df)[13:22]
for(i in 1:3){
  df[,roundme[i]] <- ceiling(df[,roundme[i]])
}

df <- df[,c(1,13,14,15)]

df %>%
  pivot_longer(cols = starts_with("age"), names_to = "age_group", values_to = "cases") -> df

df$age_group <- factor(df$age_group, levels = c("age_0_4", "age_5_11", "age_12_15"), labels = c("0-4 Years", "5-11 Years", "12-15 Years"))
df$week <- as.Date(df$week)

total04<-sum(df$cases[df$age_group=="0-4 Years"])
total511<-sum(df$cases[df$age_group=="5-11 Years"])
total1215<-sum(df$cases[df$age_group=="12-15 Years"])
total <- sum(df$cases)

p<-ggplot(df, aes(fill=age_group, y=cases, x=week)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Number of Cases \n") +
  xlab("\nWeek Ending") +
  scale_y_continuous(label=comma, limits=c(0,70000), breaks=c(seq(0,70000, by=10000))) +
  scale_fill_discrete(name = "Age Group") +
  scale_x_date(date_breaks = "5 weeks") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(color = "darkgray"),
    axis.text.x = element_text(angle = 90)
  ) +
    annotate(geom="text", x=as.Date("2020-06-22"), 
             y=60000, 
             label=
               paste("Total 0-4 years:", format(total04, big.mark = ",", scientific=F),
              "\nTotal 5-11 years:", format(total511, big.mark = ",", scientific=F),
              "\nTotal 12-15 years:", format(total1215, big.mark = ",", scientific=F),
              "\nGrand Total:", format(total, big.mark = ",", scientific=F)),
              size=3)
             
  p
ggsave("~/Desktop/file.pdf", width=9, height=5)

yo <- ggplotly(p)

htmlwidgets::saveWidget(yo, "~/Desktop/plot.html")


#write.table(df, "~/Desktop/rawdata.csv", sep=",", row.names=F)




