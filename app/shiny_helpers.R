#Script will create functions to be used in Shiny app
#------------------------------#
#
#------------------------------#
#Libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(tidyr)
library(Hmisc)
library(data.table)

#Begin functions
plotter <- function(data, corrected = F, age_groups, color = T){
      if("All" %in% age_groups){
            age_groups<-levels(data$age_group)
      }
      data <- data[data$age_group%in%c(age_groups),]
      dateVec <- seq(from = min(data$week), to = max(data$week), by = "weeks")
      if(color == T){
         if(corrected == F){yvar <- "cases"}
         if(corrected == T){yvar <- "corrected_cases"}
         data[yvar]<-round(data[yvar])
         brks <- c(seq(0, max(data[yvar], na.rm=T), by = (max(data[yvar], na.rm=T)+(10000-max(data[yvar], na.rm=T)%%10000))/15))
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
                                                               "65-74 Years" = "#f25ead", "75+ Years" = "#6d1f48")) +
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
         brks <- c(seq(0, max(data[yvar], na.rm=T), by = (max(data[yvar], na.rm=T)+(10000-max(data[yvar], na.rm=T)%%10000))/15))
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
                                           "65-74 Years" = "#E8E8E8", "75+ Years" = "#F0F0F0")) +
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

plotterpct <- function(data, corrected = F, age_groups, color = F){
   if("All" %in% age_groups){
      age_groups<-levels(data$age_group)
   }
   data <- data[data$age_group%in%c(age_groups),]
   dateVec <- seq(from = min(data$week), to = max(data$week), by = "weeks")
   
   if(color == T){
      if(corrected == F){yvar <- "percent_difference"}
      if(corrected == T){yvar <- "percent_difference_corrected"}
      brks <- c(seq((min(data[yvar], na.rm=T) - (min(data[yvar], na.rm=T) %% 5)),
              (max(data[yvar], na.rm=T) + (5 - max(data[yvar], na.rm=T) %% 5)), 
              by=25))
      
      p <- ggplot(data,
             aes(linetype = age_group,
                colour=age_group, 
                 y=get(yvar),
                 group = 1,
                 x=week,
                 text = paste(
                    "</br>Week:", week,
                    "</br>Age:", age_group,
                    "</br>Percent Difference:",get(yvar)
                 )
            )) + 
         geom_line(size=0.8) +
         ylab("Percent Change from Prior Week \n") +
         xlab("\nWeek Ending") +
         scale_y_continuous(label=comma, limits=c( 
               (min(data[yvar], na.rm=T) - (min(data[yvar], na.rm=T) %% 5)), 
               (max(data[yvar], na.rm=T) + (5 - max(data[yvar], na.rm=T) %% 5))), 
               breaks=brks) +
         scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
         scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#e9cb67", "5-11 Years" = "#e3bb42", "12-15 Years" = "#dbaa2e",
                                                            "16-17 Years" = "#937926", "18-29 Years" = "#00adad", "30-39 Years" = "#007574", "40-49 Years" = "#004848", "50-64 Years" = "#fcabd5",
                                                            "65-74 Years" = "#f25ead", "75+ Years" = "#6d1f48")) +
         scale_x_date(date_breaks = "2 weeks",  limits = c(min(dateVec), max = max(dateVec)), expand = c(0.01428571,0.01428571)) +
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
      if(corrected == F){yvar <- "percent_difference"}
      if(corrected == T){yvar <- "percent_difference_corrected"}
      brks <- c(seq((min(data[yvar], na.rm=T) - (min(data[yvar], na.rm=T) %% 5)),
                    (max(data[yvar], na.rm=T) + (5 - max(data[yvar], na.rm=T) %% 5)), 
                    by=25))
      
      p <- ggplot(data,
                  aes(linetype = age_group,
                      colour=age_group, 
                      y=get(yvar),
                      group = 1,
                      x=week,
                      text = paste(
                         "</br>Week:", week,
                         "</br>Age:", age_group,
                         "</br>Percent Difference:",get(yvar)
                      )
                  )) + 
         geom_line(size=0.8) +
         ylab("Percent Change from Prior Week \n") +
         xlab("\nWeek Ending") +
         scale_y_continuous(label=comma, limits=c( 
            (min(data[yvar], na.rm=T) - (min(data[yvar], na.rm=T) %% 5)), 
            (max(data[yvar], na.rm=T) + (5 - max(data[yvar], na.rm=T) %% 5))), 
            breaks=brks) +
         scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
         scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#BEBEBE", "5-11 Years" = "#C0C0C0", "12-15 Years" = "#C8C8C8",
                                                            "16-17 Years" = "#D0D0D0", "18-29 Years" = "#D3D3D3", "30-39 Years" = "#D8D8D8", "40-49 Years" = "#DCDCDC", "50-64 Years" = "#E0E0E0",
                                                            "65-74 Years" = "#E8E8E8", "75+ Years" = "#F0F0F0")) +
         scale_x_date(date_breaks = "2 weeks",  limits = c(min(dateVec), max = max(dateVec)), expand = c(0.01428571,0.01428571)) +
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

plotterrate <- function(data, age_groups, color = F){
   if("All" %in% age_groups){
      age_groups<-levels(data$age_group)
   }
   data <- data[data$age_group%in%c(age_groups),]
   dateVec <- seq(from = min(data$week), to = max(data$week), by = "weeks")
   if(color == T){
      yvar <- "cases"
      brks <- c(seq(0, max(data[yvar], na.rm=T), by = (max(data[yvar], na.rm=T)+(25-max(data[yvar], na.rm=T)%%25))/5))
      p<-ggplot(data,
                aes(linetype=age_group,
                    color = age_group,
                    y=get(yvar),
                    group = 1,
                    x=week,
                    text = paste(
                       "</br>Week:", week,
                       "</br>Age:", age_group,
                       "</br>Rate Per 100,000:",get(yvar))
                )) + 
         geom_line(size=0.8) +
         ylab("Rate Per 100,000 \n") +
         xlab("\nWeek Ending") +
         scale_y_continuous(label=comma, limits=c(0,max(data[yvar])), breaks= brks) +
         scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
         scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#e9cb67", "5-11 Years" = "#e3bb42", "12-15 Years" = "#dbaa2e",
                                                            "16-17 Years" = "#937926", "18-29 Years" = "#00adad", "30-39 Years" = "#007574", "40-49 Years" = "#004848", "50-64 Years" = "#fcabd5",
                                                            "65-74 Years" = "#f25ead", "75+ Years" = "#6d1f48")) +
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
      yvar <- "cases"
      brks <- c(seq(0, max(data[yvar], na.rm=T), by = (max(data[yvar], na.rm=T)+(25-max(data[yvar], na.rm)%%25))/5))
      p<-ggplot(data,
                aes(linetype=age_group,
                    color = age_group,
                    y=get(yvar),
                    group = 1,
                    x=week,
                    text = paste(
                       "</br>Week:", week,
                       "</br>Age:", age_group,
                       "</br>Rate Per 100,000:",get(yvar)))) + 
         geom_line(size=0.8) +
         ylab("Rate Per 100,000 \n") +
         xlab("\nWeek Ending") +
         scale_y_continuous(label=comma, limits=c(0,max(data[yvar])), breaks=brks) +
         scale_linetype_manual(name = "", values = c("solid", "longdash", "dotted","solid", "longdash", "dotted","solid", "longdash", "dotted","solid")) + 
         scale_colour_manual(name = "Age Group", values = c("0-4 Years" = "#BEBEBE", "5-11 Years" = "#C0C0C0", "12-15 Years" = "#C8C8C8",
                                                            "16-17 Years" = "#D0D0D0", "18-29 Years" = "#D3D3D3", "30-39 Years" = "#D8D8D8", "40-49 Years" = "#DCDCDC", "50-64 Years" = "#E0E0E0",
                                                            "65-74 Years" = "#E8E8E8", "75+ Years" = "#F0F0F0")) +
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

prepdatatable <- function(data, corrected = F, age_groups, sub){
   names(data)<-c("Week", "0-4 Rate", "5-11 Rate", "12-15 Rate", "16-17 Rate", "18-29 Rate",
                  "30-39 Rate", "40-49 Rate", "50-64 Rate", "65-74 Rate", "75+ Rate", "Year",
                  "Age_Group", "Cases", "Corrected_Cases", "Percent_Difference_Corrected", "Percent_Difference")
   if("All" %in% age_groups){
      age_groups<-levels(data$Age_Group)
   }
   if(corrected == F){
      data<-subset(data, select = -c(Corrected_Cases, Percent_Difference_Corrected))
      data <- data[data$Age_Group %in% age_groups,]
      grab<-gsub( " .*$", "", age_groups)
      #grab <- gsub("≥", "", grab)
      data %>%
         select(Week, starts_with(grab), Age_Group, Cases, Percent_Difference) %>%
         mutate(Cases = round(Cases)) -> data
      data <- data[names(data)[names(data)%in%grep(paste(sub, collapse = "|"), names(data), value = T)]]
      return(data)
   }
   if(corrected == T){
      data<-subset(data, select = -c(Cases, Percent_Difference))
      data <- data[data$Age_Group %in% age_groups,]
      grab<-gsub( " .*$", "", age_groups)
      #grab <- gsub("≥", "", grab)
      data %>%
         select(Week, starts_with(grab), Age_Group, Corrected_Cases, Percent_Difference_Corrected) %>%
         mutate(Corrected_Cases = round(Corrected_Cases)) -> data
      data <- data[names(data)[names(data)%in%grep(paste(sub, collapse = "|"),names(data), value = T)]]
      return(data)
   }
}



