library(tidyverse)
library(rvest)
library(plotly)
#Read in url
url<-("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html")
nbadata <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

attach(nbadata)

#The variables names appeared multiple times in the dataset so we must remove them
nbadata2<- nbadata %>% 
  filter(!grepl('Player', Player))

#Convert necessary columns from character to numeric
cols = c(1,4, 6:30)    
nbadata2[,cols] = apply(nbadata2[,cols],2,  function(x) as.numeric(as.character(x)))

#Replace "NAs" with 0s
nbadata2<- nbadata2%>%
  mutate_all(~replace(., is.na(.), 0))

#Make variable names more descriptive for shiny app 
nbadata2<-nbadata2%>%
  rename(
    "Rank"="Rk",
    "Position"="Pos",
    "Team"="Tm",
    "Games"="G",
    "Games Started"= "GS",
    "Minutes Played"="MP",
    "Field Goals"="FG",
    "Field Goal Attempts"="FGA",
    "Field Goal Percentage"="FG%",
    "3-Point Field Goals"="3P",
    "3-Point Field Goal Attempts"="3PA",
    "FG% on 3-Pt FGAs"="3P%",
    "2-Point Field Goals"="2P",
    "2-Point Field Goal Attempts"="2PA",
    "FG% on 2-Pt FGAs"="2P%",
    "Effective Field Goal Percentage"="eFG%",
    "Free Throws"="FT",
    "Free Throw Attempts"="FTA",
    "Free Throw Percentage"="FT%",
    "Offensive Rebounds"="ORB",
    "Defensive Rebounds"="DRB",
    "Total Rebounds"="TRB",
    "Assists"="AST",
    "Steals"="STL",
    "Blocks"="BLK",
    "Turnovers"="TOV",
    "Personal Fouls"="PF",
    "Points"="PTS"
    
  )
#Alphabetize names for app
statnames=sort(names(nbadata2[cols2]))
teamnames=sort(unique(nbadata2$Team))
#Remove team name "TOT" which represents total stats for players who have played for multiple teams
teamnames=teamnames[-29]

#Shiny App
library(shiny)

cols2 = c(4,6:30)
ui <- fluidPage(
  titlePanel('Comparing Per Game Individual NBA Stats'),
  headerPanel('Input Selection'),
  sidebarPanel(
    selectInput('xcol','X Variable', statnames, selected="Minutes Played"),
    selectInput('ycol','Y Variable', statnames, selected="Points"),
    selected = names(nbadata[cols2])[[2]],
    selectInput('team1',"Team 1", teamnames, selected="GSW"),
    selectInput('team2', "Team 2", teamnames, selected="LAL"),
    selectInput('player1', "Player 1", unique(nbadata2$Player), selected="Giannis Antetokounmpo"),
    selectInput('player2', "Player 2", unique(nbadata2$Player), selected="James Harden")),
  mainPanel(
    tabsetPanel(
      tabPanel("Scatterplot Team Comparison", plotlyOutput("compare_plot")),
      tabPanel("Histogram Team Comparison", plotlyOutput('compare_plot_hist')),
      tabPanel("Scatterplot Position Comparison", plotlyOutput("plot")),
      tabPanel("Player Comparison", plotlyOutput("scatterpolar"))
    )
  )
)


server <- function(input, output) {
  
  #reactive for team comparison data
  compare_data <- reactive(nbadata2 %>% filter(Team %in% c(input$team1,input$team2)))
  compare_data_x <- reactive(compare_data()[,input$xcol])
  
  compare_data_y <- reactive(compare_data()[,input$ycol])
  
  teamcolor<-reactive(nbadata2[,5] %>% filter(Team %in% c(input$team1,input$team2)))
  
  #Team comparison scatterplot
  output$compare_plot <- renderPlotly( plot1 <- plot_ly(
    data=compare_data(),
    x = compare_data_x(),
    y = compare_data_y(),
    color=~Team,
    showlegend=TRUE,
    type = 'scatter',
    mode = 'markers', 
    hoverinfo=('x, y'),
    text=~Player
  ) %>% layout(title="Individual Stats by Team",
               xaxis = list(title = input$xcol), yaxis = list(title = input$ycol)))
  
  #Team comparison histogram 
  output$compare_plot_hist <- renderPlotly(plot2<-plot_ly(
    data=compare_data(),
    x=compare_data_x(),
    type="histogram",
    color=~Team,
    text=~Team
  ) %>% layout(title="Individual Stats Histogram by Team",xaxis = list(title = input$xcol), yaxis=list(title="Count")))
  
  #X and Y variable choices
  x <- reactive({
    nbadata2[,input$xcol]
  })
  
  y <- reactive({
    nbadata2[,input$ycol]
  })
  
  #Scatterplot by position
  output$plot <- renderPlotly(
    plot1 <- plot_ly(
      data=nbadata2,
      x = x(),
      y = y(), 
      color=nbadata2$Position,
      showlegend=TRUE,
      type = 'scatter',
      mode = 'markers',
      text=~Player) %>%
      layout(title="Individual Stats Colored By Position", 
             xaxis = list(title = input$xcol), yaxis = list(title = input$ycol)))
  
 
  #Data for polar diagram comparing players
  scatterpolar_data1 <- reactive(nbadata2 %>% filter(Player == input$player1) %>% 
                                   select(c("Field Goals", "3-Point Field Goals",
                                            "Free Throws", "Assists", "Total Rebounds")))
  scatterpolar_data2 <- reactive(nbadata2 %>% filter(Player == input$player2) %>% 
                                   select(c("Field Goals", "3-Point Field Goals",
                                            "Free Throws", "Assists", "Total Rebounds" )))
  #Plot of polar diagram
  output$scatterpolar <- renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      mode='markers',
      fill = 'toself') %>% 
      add_trace(
        name = input$player1,
        r = as.numeric(scatterpolar_data1()[1,]),
        theta = names(scatterpolar_data1())) %>%
      add_trace(
        name = input$player2,
        r = as.numeric(scatterpolar_data2()[1,]),
        theta = names(scatterpolar_data2())) %>% 
      layout(title="Polar Diagram Player Comparison",
        polar = list(
          radialaxis = list(
            visible = T,
            range = "auto")), showlegend = T)) 
}

shinyApp(ui,server)

