
############################################################
#################### LOAD PACKAGES #########################
############################################################
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


cases <-rio::import("https://github.com/wiemken/covid-agegroups/blob/main/case_data.xlsx?raw=true")
cases %>%
  mutate(year = lubridate::year(week)) ->cases


nmz<-substr(names(cases), start=2, stop=nchar(names(cases)))[2:11]
names(cases) <- c("week", paste0("age_", nmz), "year")

### pivot to long format for plotting
cases %>%
  pivot_longer(cols = starts_with("age"), names_to = "age_group", values_to = "cases") -> df

### factor age group to ensure proper arrangement
df$age_group <- factor(df$age_group, levels = c("age_0_4", "age_5_11", "age_12_15", "age_16_17", "age_18_29", "age_30_39", "age_40_49", "age_50_64", "age_65_74", "age_75"), 
                       labels = c("0-4 Years", "5-11 Years", "12-15 Years", "16-17 Years", "18-29 Years", "30-39 Years", "40-49 Years", "50-64 Years", "65-74 Years", "≥75 Years"))
### ensure 'week' column is date format
df$week <- as.Date(df$week)

### totals from: https://covid.cdc.gov/covid-data-tracker/#demographics
total04<-format(693100, big.mark=",")
total511<-format(1362512, big.mark=",")
total1215<-format(1182692, big.mark=",")
total.kidsvax <- format(sum(693100, 1362512, 1182692), big.mark=",")
  
### start plotting first three age groups
df.plot <- df[df$age_group%in%c("0-4 Years", "5-11 Years", "12-15 Years"),]
### plot clustered bar graph
p<-ggplot(df.plot,
          aes(colour=age_group, 
              y=cases, 
              x=week)) + 
  geom_line(size=0.8) +
  ylab("Rate Per 100,000 Population \n") +
  xlab("\nWeek Ending") +
  scale_y_continuous(label=comma, limits=c(0,350), breaks=c(seq(0,350, by=50))) +
  scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#a6cee3", "5-11 Years" = "#1f78b4", "12-15 Years" = "#b2df8a")) +
  scale_x_date(date_breaks = "3 weeks",  limits = c(min(df.plot$week), max = max(df.plot$week)), expand=c(0.02,0.02)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(color = "darkgray"),
    axis.text.x = element_text(angle = 90),
    legend.key=element_blank(),
    legend.background=element_blank(),
    legend.position = "right",
    legend.key.width = unit(3, "line"))
  # ) +
  # annotate(geom="text", x=as.Date("2020-06-22"), 
  #          y=300, 
  #          label=
  #            paste("Total 0-4 years:", format(total04, big.mark = ",", scientific=F, digits=0),
  #                  "\nTotal 5-11 years:", format(total511, big.mark = ",", scientific=F, digits=0),
  #                  "\nTotal 12-15 years:", format(total1215, big.mark = ",", scientific=F, digits=0),
  #                  "\nGrand Total:", format(total.kidsvax, big.mark = ",", scientific=F, digits=0)),
  #          size=3) 
p 
ggsave(plot = p, "C:/Users/Wiemkt/OneDrive - Pfizer/Desktop/file.jpg", width=9, height=5)


