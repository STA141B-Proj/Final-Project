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
    "Minutes Played Per Game"="MP",
    "Field Goals Per Game"="FG",
    "Field Goal Attempts Per game"="FGA",
    "Field Goal Percentage"="FG%",
    "3-Point Field Goals Per Game"="3P",
    "3-Point Field Goal Attempts Per Game"="3PA",
    "FG% on 3-Pt FGAs"="3P%",
    "2-Point Field Goals Per Game"="2P",
    "2-Point Field Goal Attempts Per Game"="2PA",
    "FG% on 2-Pt FGAs"="2P%",
    "Effective Field Goal Percentage"="eFG%",
    "Free Throws Per Game"="FT",
    "Free Throw Attempts Per Game"="FTA",
    "Free Throw Percentage"="FT%",
    "Offensive Rebounds Per Game"="ORB",
    "Defensive Rebounds Per Game"="DRB",
    "Total Rebounds Per Game"="TRB",
    "Assists Per Game"="AST",
    "Steals Per Game"="STL",
    "Blocks Per Game"="BLK",
    "Turnovers Per Game"="TOV",
    "Personal Fouls Per Game"="PF",
    "Points Per Game"="PTS"
    
  )




#Shiny App
library(shiny)
cols2 = c(4,5 ,6:30)
ui <- fluidPage(
  headerPanel('NBA Stats'),
  sidebarPanel(
    selectInput('xcol','X Variable', names(nbadata2[cols2]), selected="Minutes Played Per Game"),
    selectInput('ycol','Y Variable', names(nbadata2[cols2]), selected="Points Per Game"),
    selected = names(nbadata[cols2])[[2]],
    selectInput('team1',"Team 1", unique(nbadata2$Team), selected="GSW"),
    selectInput('team2', "Team 2", unique(nbadata2$Team), selected="LAL"),
    selectInput('player1', "Player 1", unique(nbadata2$Player)),
    selectInput('player2', "Player 2", unique(nbadata2$Player))),
  mainPanel(
    tabsetPanel(
      tabPanel("Scatterplot", plotlyOutput("compare_plot")),
      tabPanel("Histogram for X Variable", plotlyOutput('compare_plot_hist')),
      tabPanel("Which Positions Perform Best at Each Stat?", plotlyOutput("plot")),
      tabPanel("Player's Stat", plotlyOutput("scatterpolar"))
    )
  )
)


server <- function(input, output) {
  
  x <- reactive({
    nbadata2[,input$xcol]
  })
  
  y <- reactive({
    nbadata2[,input$ycol]
  })
  
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
      layout(xaxis = list(title = input$xcol), yaxis = list(title = input$ycol)))
  
  compare_data <- reactive(nbadata2 %>% filter(Team %in% c(input$team1,input$team2)))
  
  
   compare_data_x <- reactive(
    compare_data()[,input$xcol])
  
  compare_data_y <- reactive(
    compare_data()[,input$ycol])
  
  teamcolor<-reactive(nbadata2[,5] %>% filter(Team %in% c(input$team1,input$team2)))
  
  
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
  ) %>% layout(xaxis = list(title = input$xcol), yaxis = list(title = input$ycol)))
  
  
  output$compare_plot_hist <- renderPlotly(plot2<-plot_ly(
    data=compare_data(),
    x=compare_data_x(),
    type="histogram",
    color=~Team,
    text=~Team
    ) %>% layout(xaxis = list(title = input$xcol), yaxis=list(title="Count")))
  
  scatterpolar_data1 <- reactive(nbadata2 %>% filter(Player == input$player1) %>% 
                                  select(c("Games", "Minutes Played Per Game",
                                           "Points Per Game")))
  scatterpolar_data2 <- reactive(nbadata2 %>% filter(Player == input$player2) %>% 
                                   select(c("Games", "Minutes Played Per Game",
                                            "Points Per Game")))
  
  output$scatterpolar <- renderPlotly(
    plot_ly(
    type = 'scatterpolar',
    fill = 'toself') %>% 
      add_trace(
        name = input$player1,
        r = as.numeric(scatterpolar_data1()[1,]),
        theta = names(scatterpolar_data1())) %>%
      add_trace(
        name = input$player2,
        r = as.numeric(scatterpolar_data2()[1,]),
        theta = names(scatterpolar_data2())) %>% 
        layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = "auto")), showlegend = F)) 
}

shinyApp(ui,server)






