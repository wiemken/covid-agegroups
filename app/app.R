#App will display output from shiny_helpers, have options to toggle BW/color, select certain age groups
#------------------------------#
#Add details in sidebar
#------------------------------#
#Libraries + Scripts
library(shiny)
library(DT)
library(tidyverse)

source("shiny_helpers.R")
source("shiny_helpers_hosp.R")

source("load_data.R")
source("load_data_hospitalization.R")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            strong("Estimating COVID-19 Case Counts and Hospitalization Counts Over Time"),
            hr(),
            
            ##########################################################################################################################
            ### SELECTOR OPTIONS
            radioButtons(inputId = "plottype", label = "Plot type?", choices = c("Cases" = 1, "Percentage Change in Cases" = 2, "Rate Per 100,000 (Cases)" = 3,
                                                                                    "Hospitalizations" = 4, "Percentage Change in Hospitalizations" = 5, "Rate Per 100,000 (Hospitalizations)" = 6)
                         ),
            radioButtons(inputId = "bwcolor", label = "Plot color scheme", choices = c("Color" = T, "Black and White" = F)),
            
            conditionalPanel(condition = "input.plottype == 1 || input.plottype == 2 || input.plottype == 3 ",
                selectizeInput(inputId = "age_group_selector", label = "Select age group(s)",
                        choices = c("All", levels(df_shiny$age_group)), selected = levels(df_shiny$age_group)[1],
                        multiple = T)
            ),
            
            conditionalPanel(condition = "input.plottype == 4 || input.plottype == 5 || input.plottype == 6 ",
                selectizeInput(inputId = "age_group_selector_hosp", label = "Select age group(s)",
                           choices = c("All", levels(df_shiny_hosp$age_group)), selected = levels(df_shiny_hosp$age_group)[1],
                           multiple = T)
            ),
            
            conditionalPanel(condition = "input.plottype == 1 || input.plottype == 2 || input.plottype == 3 ",
                selectizeInput(inputId = "correctyn", label = "Type of data to analyze (applies to cases only)", choices = c("Corrected" = T, "Not Corrected" = F))
            ),
            
            
            hr(),
            ##########################################################################################################################
            ### SIDEBAR TEXT
            strong("Data Availability:"),
            p(),
            tags$a(href = "https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv", 
                   "1. US Census Bureau Population Estimates"),
            br(),
            tags$a(href = "https://covid.cdc.gov/covid-data-tracker/#demographicsovertime",
                   "2. COVID-19 Incidence Rates by Age Group"),
            br(),
            tags$a(href = "https://covid.cdc.gov/covid-data-tracker/#covidnet-hospitalization-network",
                   "3. COVID-19 Hospitalization Rates by Age Group"),
            br(),
            p(),
            strong("Manuscript"),
            br(),
            tags$a(href = "https://github.com/wiemken/covid-agegroups/tree/main/Manuscript",
                   "See publication in the American Journal of Infection Control (In Press)")
            
        ),
        
        ##########################################################################################################################
        ### main panel plots and tables conditioned on selecting cases or hospitalizations
        mainPanel(
            br(),
            br(),
            conditionalPanel(condition = "input.plottype == 1 || input.plottype == 2 || input.plottype == 3 ",
                uiOutput('plot_output', height = "750")
                ),
            
            conditionalPanel(condition = "input.plottype == 1 || input.plottype == 2 || input.plottype == 3 ",
                uiOutput("datatable")
            ),
            
            conditionalPanel(condition = "input.plottype == 4 || input.plottype == 5 || input.plottype == 6 ",
                uiOutput('plot_output_hosp', height = "750")
            ),
            conditionalPanel(condition = "input.plottype == 4 || input.plottype == 5 || input.plottype == 6 ",
                uiOutput("datatable_hosp")
            )
        ) 
    ) 
) 

#------------------------------ End UI ------------------------------#
server <- function(input, output) {
    
    ##########################################################################################################################
    ### Plots for cases and hospitalizations, conditioned on plot type selected

    output$plot_output <- renderUI({
        tagList(
        if(input$plottype == 1){
            renderPlotly(
                plotter(
                    df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$plottype == 2){
            renderPlotly(
                plotterpct(
                    df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$plottype == 3){
            renderPlotly(
                plotterrate(
                    df_shiny_rate, age_groups = input$age_group_selector, color = input$bwcolor))
        }
        )
    })
        
    output$plot_output_hosp <- renderUI({
            tagList(
        if(input$plottype == 4){
            renderPlotly(
                plotter_hosp(
                    df_shiny_hosp, age_groups = input$age_group_selector_hosp, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$plottype == 5){
            renderPlotly(
                plotterpct_hosp(
                    df_shiny_hosp, age_groups = input$age_group_selector_hosp, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$plottype == 6){
            renderPlotly(
                plotterrate_hosp(
                    df_shiny_rate_hosp, age_groups = input$age_group_selector_hosp, color = input$bwcolor))
        }
        )
    })
    
    ##########################################################################################################################
    ### data tables conditioned on plot type
    
    output$datatable <- renderUI({
        tagList(
        fluidRow(
            DT::renderDataTable(
                datatable(
                prepdatatable(
                    df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week", "Rate")), rownames = F, extensions = "Buttons", 
                options = list(paging = TRUE,
                               scrollX=TRUE, 
                               searching = TRUE,
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               pageLength=5, 
                               lengthMenu=c(3,5,10)))
                ), 
             ),
            hr(),
        fluidRow(
            DT::renderDataTable(
                datatable(
                prepdatatable(
                    df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week","Age_Group","Cases","Percent_Diff")), rownames = F, extensions = "Buttons", 
                options = list(paging = TRUE,
                               scrollX=TRUE, 
                               searching = TRUE,
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               pageLength=5, 
                               lengthMenu=c(3,5,10)))
                )
        )
        )
    })
    
    
    
    output$datatable_hosp <- renderUI({
        tagList(
            fluidRow(
                DT::renderDataTable(
                    datatable(
                        prepdatatable_hosp(
                            df_shiny_hosp, age_groups = input$age_group_selector, sub = c("Week", "Rate")), rownames = F, extensions = "Buttons", 
                        options = list(paging = TRUE,
                                       scrollX=TRUE, 
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                                       pageLength=5, 
                                       lengthMenu=c(3,5,10)))
                    ), 
            ),
            hr(),
            fluidRow(
                DT::renderDataTable(
                    datatable(
                        prepdatatable_hosp(
                            df_shiny_hosp, age_groups = input$age_group_selector, sub = c("Week","Age_Group","Cases","Percent_Diff")), rownames = F, extensions = "Buttons", 
                        options = list(paging = TRUE,
                                       scrollX=TRUE, 
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                                       pageLength=5, 
                                       lengthMenu=c(3,5,10)))
                    )
            )
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
