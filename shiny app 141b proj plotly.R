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


#Scale Data for polar diagram
#Scale from 0 to 100, essentially a percentile ranking

library(scales)
scaled<-nbadata2
scaled$`Age`<-round(rescale(scaled$`Age`, to = c(0, 100), 
                                 from = range(scaled$`Age`, finite = TRUE)), digits=2)
scaled$`Games`<-round(rescale(scaled$`Games`, to = c(0, 100), 
                                 from = range(scaled$`Games`, finite = TRUE)), digits=2)
scaled$`Games Started`<-round(rescale(scaled$`Games Started`, to = c(0, 100), 
                                      from = range(scaled$`Games Started`, finite = TRUE)), digits=2)
scaled$`Minutes Played`<-round(rescale(scaled$`Minutes Played`, to = c(0, 100), 
                                 from = range(scaled$`Minutes Played`, finite = TRUE)), digits=2)
scaled$`Field Goals`<-round(rescale(scaled$`Field Goals`, to = c(0, 100), 
                              from = range(scaled$`Field Goals`, finite = TRUE)), digits=2)
scaled$`Field Goal Attempts`<-round(rescale(scaled$`Field Goal Attempts`, to = c(0, 100), 
                                      from = range(scaled$`Field Goal Attempts`, finite = TRUE)), digits=2)
scaled$`Field Goal Percentage`<-round(rescale(scaled$`Field Goal Percentage`, to = c(0, 100), 
                                      from = range(scaled$`Field Goal Percentage`, finite = TRUE)), digits=2)
scaled$`3-Point Field Goals`<-round(rescale(scaled$`3-Point Field Goals`, to = c(0, 100), 
                                      from = range(scaled$`3-Point Field Goals`, finite = TRUE)), digits=2)
scaled$`3-Point Field Goal Attempts`<-round(rescale(scaled$`3-Point Field Goal Attempts`, to = c(0, 100), 
                                      from = range(scaled$`3-Point Field Goal Attempts`, finite = TRUE)), digits=2)
scaled$`FG% on 3-Pt FGAs`<-round(rescale(scaled$`FG% on 3-Pt FGAs`, to = c(0, 100), 
                                      from = range(scaled$`FG% on 3-Pt FGAs`, finite = TRUE)), digits=2)

scaled$`2-Point Field Goals`<-round(rescale(scaled$`2-Point Field Goals`, to = c(0, 100), 
                                            from = range(scaled$`2-Point Field Goals`, finite = TRUE)), digits=2)
scaled$`2-Point Field Goal Attempts`<-round(rescale(scaled$`2-Point Field Goal Attempts`, to = c(0, 100), 
                                                    from = range(scaled$`2-Point Field Goal Attempts`, finite = TRUE)), digits=2)
scaled$`FG% on 2-Pt FGAs`<-round(rescale(scaled$`FG% on 2-Pt FGAs`, to = c(0, 100), 
                                         from = range(scaled$`FG% on 2-Pt FGAs`, finite = TRUE)), digits=2)

scaled$`Effective Field Goal Percentage`<-round(rescale(scaled$`Effective Field Goal Percentage`, to = c(0, 100), 
                              from = range(scaled$`Effective Field Goal Percentage`, finite = TRUE)), digits=2)
scaled$`Free Throws`<-round(rescale(scaled$`Free Throws`, to = c(0, 100), 
                              from = range(scaled$`Free Throws`, finite = TRUE)), digits=2)

scaled$`Free Throw Attempts`<-round(rescale(scaled$`Free Throw Attempts`, to = c(0, 100), 
                              from = range(scaled$`Free Throw Attempts`, finite = TRUE)), digits=2)
scaled$`Free Throw Percentage`<-round(rescale(scaled$`Free Throw Percentage`, to = c(0, 100), 
                              from = range(scaled$`Free Throw Percentage`, finite = TRUE)), digits=2)
scaled$`Offensive Rebounds`<-round(rescale(scaled$`Offensive Rebounds`, to = c(0, 100), 
                              from = range(scaled$`Offensive Rebounds`, finite = TRUE)), digits=2)
scaled$`Defensive Rebounds`<-round(rescale(scaled$`Defensive Rebounds`, to = c(0, 100), 
                              from = range(scaled$`Defensive Rebounds`, finite = TRUE)), digits=2)
scaled$`Total Rebounds`<-round(rescale(scaled$`Total Rebounds`, to = c(0, 100), 
                              from = range(scaled$`Total Rebounds`, finite = TRUE)), digits=2)
scaled$`Assists`<-round(rescale(scaled$`Assists`, to = c(0, 100), 
                              from = range(scaled$`Assists`, finite = TRUE)), digits=2)
scaled$`Steals`<-round(rescale(scaled$`Steals`, to = c(0, 100), 
                              from = range(scaled$`Steals`, finite = TRUE)), digits=2)
scaled$`Blocks`<-round(rescale(scaled$`Blocks`, to = c(0, 100), 
                              from = range(scaled$`Blocks`, finite = TRUE)), digits=2)
scaled$`Turnovers`<-round(rescale(scaled$`Turnovers`, to = c(0, 100), 
                              from = range(scaled$`Turnovers`, finite = TRUE)), digits=2)
scaled$`Personal Fouls`<-round(rescale(scaled$`Personal Fouls`, to = c(0, 100), 
                              from = range(scaled$`Personal Fouls`, finite = TRUE)), digits=2)
scaled$`Points`<-round(rescale(scaled$`Points`, to = c(0, 100), 
                              from = range(scaled$`Points`, finite = TRUE)), digits=2)
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
    selectInput('player1', "Player 1 (Orange)", unique(nbadata2$Player), selected="Giannis Antetokounmpo"),
    selectInput('player2', "Player 2 (Green)", unique(nbadata2$Player), selected="James Harden")),
  mainPanel(
    tabsetPanel(
      tabPanel("Scatterplot Team Comparison", plotlyOutput("compare_plot"), h6("Scatter plot showing the relationship between your choosen X and Y Variables, color coordinated by your selected Teams.")),
      tabPanel("Histogram Team Comparison", plotlyOutput('compare_plot_hist'), h6("Histogram showing the number of players each of your selected Team has by your choosen X Variable.")),
      tabPanel("Scatterplot Position Comparison", plotlyOutput("plot"), h6("Scatter plot showing the relationship between your choosen X and Y Variables, color coordinated by position. This graph plots every player in the NBA.")),
      tabPanel("Player Comparison", plotlyOutput("scatterpolar"), h6("Scatter polar plot showing a comparison between your two selected players. Standarized to a scale of 0 to 100."))
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
  scatterpolar_data1 <- reactive(scaled %>% filter(Player == input$player1) %>% 
                                   select(names(scaled[4,6:30])))
  scatterpolar_data2 <- reactive(scaled %>% filter(Player == input$player2) %>% 
                                   select(names(scaled[4,6:30])))
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
      layout(polar = list(
               radialaxis = list(
                 visible = T,
                 range = "auto")), showlegend = F)) 
}

shinyApp(ui,server)
