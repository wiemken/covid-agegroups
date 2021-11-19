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
            "As of Nov 11, 2021 data are updated nightly (Note, Case data 2 weeks lagged)",
            hr(),
            p(),
            strong("Manuscript"),
            br(),
            tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8550884/pdf/main.pdf",
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
                    df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week", "Rate")), rownames = F,
                                extensions = 'Buttons'
                                , options = list( 
                                    dom = "Blfrtip"
                                    , buttons = 
                                        list("copy", list(
                                            extend = "collection"
                                            , buttons = c("csv", "excel", "pdf")
                                            , text = "Download"
                                        ) ) # end of buttons customization
                                    
                                    # customize the length menu
                                    , lengthMenu = list( c(10, 20, -1) # declare values
                                                         , c(10, 20, "All") # declare titles
                                    ) # end of lengthMenu customization
                                    , pageLength = 10
                                    
                                    
                                ))
                ), 
             ),
            hr(),
        fluidRow(
            DT::renderDataTable(
                datatable(
                prepdatatable(
                    df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week","Age_Group","Cases","Percent_Diff")), rownames = F,
                extensions = 'Buttons'
                , options = list( 
                    dom = "Blfrtip"
                    , buttons = 
                        list("copy", list(
                            extend = "collection"
                            , buttons = c("csv", "excel", "pdf")
                            , text = "Download"
                        ) ) # end of buttons customization
                    
                    # customize the length menu
                    , lengthMenu = list( c(10, 20, -1) # declare values
                                         , c(10, 20, "All") # declare titles
                    ) # end of lengthMenu customization
                    , pageLength = 10
                    
                    
                ))
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
                            df_shiny_hosp, age_groups = input$age_group_selector_hosp, sub = c("Week", "Rate")), rownames = F,
                        extensions = 'Buttons'
                        , options = list( 
                            dom = "Blfrtip"
                            , buttons = 
                                list("copy", list(
                                    extend = "collection"
                                    , buttons = c("csv", "excel", "pdf")
                                    , text = "Download"
                                ) ) # end of buttons customization
                            
                            # customize the length menu
                            , lengthMenu = list( c(10, 20, -1) # declare values
                                                 , c(10, 20, "All") # declare titles
                            ) # end of lengthMenu customization
                            , pageLength = 10
                            
                            
                        ))
                    ), 
            ),
            hr(),
            fluidRow(
                DT::renderDataTable(
                    datatable(
                        prepdatatable_hosp(
                            df_shiny_hosp, age_groups = input$age_group_selector_hosp, sub = c("Week","Age_Group","Cases","Percent_Diff")), rownames = F,
                        extensions = 'Buttons'
                        , options = list( 
                            dom = "Blfrtip"
                            , buttons = 
                                list("copy", list(
                                    extend = "collection"
                                    , buttons = c("csv", "excel", "pdf")
                                    , text = "Download"
                                ) ) # end of buttons customization
                            
                            # customize the length menu
                            , lengthMenu = list( c(10, 20, -1) # declare values
                                                 , c(10, 20, "All") # declare titles
                            ) # end of lengthMenu customization
                            , pageLength = 10
                            
                            
                        ))
                    )
            )
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
