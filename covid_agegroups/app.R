#App will display output from shiny_helpers, have options to toggle BW/color, select certain age groups
#------------------------------#
#Add details in sidebar
#------------------------------#
#Libraries + Scripts
library(shiny)
library(DT)
source("shiny_helpers.R")
source("load_data.R")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            strong("Estimating COVID-19 Case Counts Over Time"),
            hr(),
            radioButtons(inputId = "pctchange", label = "Plot type?", choices = c("Cases" = 1, "Percentage Change" = 2, "Rate Per 100,000" = 3)),
            radioButtons(inputId = "bwcolor", label = "Plot color scheme?", choices = c("Color" = T, "Black and White" = F)),
            selectizeInput(inputId = "age_group_selector", label = "Age? (Select all that apply, selecting 'All' will override all other choices",
                        choices = c("All", levels(df_shiny$age_group)), selected = levels(df_shiny$age_group)[1],
                        multiple = T),
            
            selectizeInput(inputId = "correctyn", label = "Type of data to analyze", choices = c("Corrected" = T, "Not Corrected" = F))
        ),
        
        mainPanel(
            br(),
            br(),
            uiOutput('plot_output', height = "750"),
            uiOutput("datatable")
        ) 
    ) 
) 

#------------------------------ End UI ------------------------------#
server <- function(input, output) {

    output$plot_output <- renderUI({
        tagList(
        if(input$pctchange == 1){
            renderPlotly(plotter(df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$pctchange == 2){
            renderPlotly(plotterpct(df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$pctchange == 3){
            renderPlotly(plotterrate(df_shiny_rate, age_groups = input$age_group_selector, color = input$bwcolor))
        }
        )
    })
    
    output$datatable <- renderUI({
        tagList(
        fluidRow(
            DT::renderDataTable(datatable(prepdatatable(df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week","Rate")), rownames = F))
        ),
        hr(),
        fluidRow(
            DT::renderDataTable(datatable(prepdatatable(df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn, sub = c("Week","Age_Group","Cases","Percent_Diff")), rownames = F))
        )
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
