#App will display output from shiny_helpers, have options to toggle BW/color, select certain age groups
#------------------------------#
#Add %difference plot, Add details in sidebar
#------------------------------#
#Libraries + Scripts
library(shiny)
library(DT)
source("shiny_helpers.R")
source("load_data.R")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            strong("COVID-19 Age Groups"),
            hr(),
            radioButtons(inputId = "pctchange", label = "View cases or Pct change plot?", choices = c("Cases" = F, "Pct Change" = T)),
            radioButtons(inputId = "bwcolor", label = "Color or Black and White?", choices = c("Color" = T, "Black and White" = F)),
            selectizeInput(inputId = "age_group_selector", label = "Age",
                        choices = c("All", levels(df_shiny$age_group)), selected = levels(df_shiny$age_group)[1],
                        multiple = T),
            
            selectizeInput(inputId = "correctyn", label = "Corrected data?", choices = c("Corrected" = T, "Not Corrected" = F))
        ),
        
        mainPanel(
            br(),
            br(),
            uiOutput('plot_output', height = "750"),
            dataTableOutput("datatable")
        ) 
    ) 
) 

#------------------------------ End UI ------------------------------#
server <- function(input, output) {

    output$plot_output <- renderUI({
        tagList(
        if(input$pctchange == F){
            renderPlotly(plotter(df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        },
        if(input$pctchange == T){
            renderPlotly(plotterpct(df_shiny, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn))
        }
        )
    })
    
    output$datatable <- DT::renderDataTable({
         datatable(prepdatatable(df_shiny, age_groups = input$age_group_selector, corrected = input$correctyn), rownames = F)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
