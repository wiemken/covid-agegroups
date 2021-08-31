#App will display output from shiny_helpers, have options to toggle BW/color, select certain age groups
#------------------------------#
#Cases not rounding, Add datatable with cases by age group, Corrected/Not corrected data, Fix breaks
#------------------------------#
#Libraries + Scripts
library(shiny)
source("shiny_helpers.R")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            strong("COVID-19 Age Groups"),
            hr(),
            selectizeInput(inputId = "age_group_selector", label = "Age",
                        choices = c("All", levels(df$age_group)), selected = levels(df$age_group)[1],
                        multiple = T),
            selectizeInput(inputId = "bwcolor", label = "Color or Black and White?", choices = c("Color" = T, "Black and White" = F)),
            selectizeInput(inputId = "correctyn", label = "Corrected data?", choices = c("Corrected" = T, "Not Corrected" = F)),
            hr(),
            hr(),
        ),
        
        mainPanel(
            br(),
            br(),
            plotlyOutput('plotter_output')
        ) 
    ) 
) 

#------------------------------ End UI ------------------------------#
server <- function(input, output) {

    output$plotter_output <- renderPlotly({
        plotter(df, age_groups = input$age_group_selector, color = input$bwcolor, corrected = input$correctyn)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
