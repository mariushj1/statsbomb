library(shiny)
library(shinythemes)
library(StatsBombR)
library(tidyverse)
library(ggsoccer)
source("util.R")

# Indhenter data
FComp <- FreeCompetitions() %>%
  filter(competition_id %in% c(72))

# Indhenter matches
Matches <- FreeMatches(FComp)

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

StatsBombData = allclean(StatsBombData)


# find en kamp
dkmatch <- StatsBombData %>% filter(match_id==3893795)
dkmshot <- dkmatch %>% filter(type.name=="Shot")

# angle and dist to goal
dkmshotsel <- dkmshot[,c(74,77,25,26,21,22,10,150,151,156,157)]
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(angle=mangle(unlist(location)))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(dist=disttogoal(unlist(location)))
dkpshotsel <- dkmshotsel %>% filter(team.id==853)
chpshotsel <- dkmshotsel %>% filter(team.id==1207)

# ui til shiny
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
                                                  choices = unique(dkpshotsel$player.name),
                                                  selected = NULL),
                                      selectInput("skud",
                                                  label = "Vælg skudforsøg",
                                                  choices = NULL,
                                                  selected = NULL),
                                      checkboxInput("triangle",
                                                    value = TRUE,
                                                    label = "Vis trekant til mål")
                                    ),
                                    mainPanel(
                                      plotOutput("bane"),
                                        )
                           )
                )
)


# Server til shiny
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
                      selected = )
  })
  observe({
    
    # Opdaterer valget af skud baseret på valg af spiller
      
    player <- input$plname
    
    seqSkud <- 1:nrow(dkmshotsel %>% 
      filter(player.name == player))
    
    
    updateSelectInput(session, "skud",
                      choices = seqSkud,
                      selected = )
  })
  
  output$bane <- renderPlot({
    
    # Angiver valg af skud og spiller
    skud <- input$skud
    player <- input$plname
    
    
    # Laver en ny dataframe kun med valgte spiller
    spillerSkud <- dkmshotsel %>% 
      filter(player.name == player)
    
    # Laver ny dataframe til freeze frame
    testshot <- spillerSkud[skud,]
    testff <- testshot$shot.freeze_frame[[1]]
    testff <- testff %>% rowwise() %>% mutate(x=location[1],
                                              y=location[2])
    
    # Laver trekanten
    dftriforshot <- dfForSingleShotTri(unlist(testshot$location))
    triangleAlpha <- ifelse(input$triangle == FALSE, 0, 0.5)
    
    
    hold <- input$hold
    modstander <- as.character(unique(dkmshotsel[dkmshotsel$team.name != hold,"team.name"]))
    
    # Laver ggplot 
    ggplot(testff) +
      annotate_pitch (
        dimensions = pitch_statsbomb,
        colour = "white",
        fill = "steelblue4") +
      geom_text(data=testff,aes(x=x,y=y,label=player.name, color = "#ffffff"), size=3.5,vjust=1)+
      geom_text(data=testshot,aes(x=location.x,y=location.y,label=player.name, color = "#ffffff"), size=4.5,vjust=1) +
      geom_point(data=testff,aes(x = x, y = y, color=teammate), size = 2) +
      scale_color_manual(values = c("TRUE" = "#549e3e", "FALSE" = "#d13038"),
                         labels = c("TRUE" = hold, "FALSE" = modstander),
                         name = "") +
      geom_point(data=testshot,aes(x = location.x, y = location.y), size = 4) + 
      geom_segment(data=testshot,aes(x = location.x, 
                                     y = location.y, 
                                     xend = shot.end_location.x,
                                     yend=shot.end_location.y),
                   colour = "yellow",
                   size = 1) +
      labs(title = paste0("Skudforsøg nr ", skud, ". for ", player),
           subtitle = paste0(hold, " vs ", modstander)) +
      geom_polygon(data = dftriforshot,aes(x=sx,y=sy),alpha=triangleAlpha) +
      theme_pitch() +
      direction_label() +
      coord_flip(xlim = c(75, 121)) +
      scale_y_reverse() +
      theme(plot.title = element_text(face = "bold"))
    
  })
}

# Kør shiny
shinyApp(ui=ui, server=server)
