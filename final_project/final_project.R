library(ggplot2)
library(readxl)
#import draft data
draft <- read.csv("draft.txt") #change these to github links

#replace NA values in stats with 0
draft$GP[is.na(draft$GP)] <-0
draft$G[is.na(draft$G)] <-0 
draft$A[is.na(draft$A)] <-0 
draft$PTS[is.na(draft$PTS)] <-0 
draft$plus_minus[is.na(draft$plus_minus)] <-0 
draft$PIM[is.na(draft$PIM)] <-0 

#replace POS that is blank with NI (no information) 
draft$Pos <- as.character(draft$Pos)
draft$Pos[which(is.na(draft$Pos))] <- "unknown"
draft$Pos <- as.factor(draft$Pos)

#import standings data (Kurtis)
standings <- read_excel("standings.xlsx")

#need data on standings
#year is the start year of the season

#age of teams data

#

#predict points using draft position, exempt goalies
#may split up by position
players <- draft[which(draft$Pos!='G'),]
ggplot(data=players,aes(x=Overall, y=PTS))+
  geom_point()

pts_overall <- lm(PTS ~ poly(Overall,2),data=draft)
summary(pts_overall)

#predict points as a function of age
ggplot(data=players,aes(x=Age, y=PTS))+
  geom_point()

#minutes versus points plot

#regression to predict points in any given season
#inputs are age, draft position, position, amateur lg., time trend the year
#predict +/- as well
#predict save percentage with draft position
#use glm to predict if play in NHL or not
#predict games with draft postion and amateur league
#test salary cap vs. no salary cap 

#predict teams points with performance in previous years (use lags 1-5)
#see how long rebuilding takes
#glm make/miss playoffs


