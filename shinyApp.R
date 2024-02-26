library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Statistik over fodboldspillere",
                           tabPanel("Skudforsøg",
                                    sidebarPanel(
                                      selectInput("hold",
                                                  label = " Vælg hold",
                                                  choices = unique(dkmshotsel$team.name),
                                                  selected = ""),
                                      selectInput("plname",
                                                  label = "Vælg spiller",
                                                  choices = unique(dkpshotsel$player.name)),
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
  # Fanen Skudforsøg
  observe({
    hold <- input$hold
    selectedPlayers <- dkmshotsel %>% 
      filter(team.name == hold) %>% 
      select(player.name) %>% 
      distinct()
    
    # Opdaterer input til at vælge spiller
    updateSelectInput(session, "plname",
                      choices = selectedPlayers,
                      selected = "")
  })
  output$bane <- renderPlot({
    
  })
  
}

shinyApp(ui=ui, server=server)
