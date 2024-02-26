library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Statistik over fodboldspillere",
                           tabPanel("Skudforsøg",
                                    sidebarPanel(
                                      selectInput("hold",
                                                  label = " Vælg hold",
                                                  choices = c("Danmark", "Kina"),
                                                  selected = "Danmark"),
                                      selectInput("plname",
                                                  label = "Vælg spiller",
                                                  choices = ""),
                                      checkboxInput("triangle",
                                                    value = FALSE,
                                                    label = "Vis trekant til mål")
                                    ),
                                    mainPanel(
                                      plotOutput("bane"),
                                      fluidRow(
                                        column(width = 8,
                                               dataTableOutput("playertab")
                                        )
                                      )
                                    )
                           )
                )
)


server <- function(input,output,session) {
  #dette er fanen Skud
  observe({
    
  })
  output$bane <- renderPlot({
    
  })
  
}

shinyApp(ui=ui, server=server)