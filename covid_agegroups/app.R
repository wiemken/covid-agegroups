#App will display output from shiny_helpers, have options to toggle BW/color, select certain age groups
#------------------------------#
#Cases not rounding, Age Groups displaying twice in legend, Add datatable with cases by age group
#------------------------------#
#Libraries
library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            strong("COVID-19 Age Groups"),
            hr(),
            selectizeInput(inputId = "age_group_selector", label = "Age",
                        choices = c("All", levels(df$age_group)), selected = levels(df$age_group)[1],
                        multiple = T),
            hr(),
            hr(),
        ),## Close SB Panel,
        
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
        plotter(df, age_groups = input$age_group_selector)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
