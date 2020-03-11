library(tidyverse)
library(rvest)
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


