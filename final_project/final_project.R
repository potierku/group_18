library(ggplot2)
library(readxl)
#import draft data
draft <- read.csv("draft.txt")

#replace NA values in stats with 0
draft$GP[is.na(draft$GP)] <-0
draft$G[is.na(draft$G)] <-0 
draft$A[is.na(draft$A)] <-0 
draft$PTS[is.na(draft$PTS)] <-0 
draft$plus_minus[is.na(draft$plus_minus)] <-0 
draft$PIM[is.na(draft$PIM)] <-0 

#replace POS that is blank with NI (no information) 
draft$Pos[which(draft$Pos=='')] <-NA 

#import standings data
standings <- read_excel("standings.xlsx")



#predict points using draft position, exempt goalies
ggplot(data=draft,aes(x=Overall, y=PTS))+
  geom_point()

players <- draft[POS!=G]
pts_overall <- lm(PTS ~ poly(Overall,2),data=draft)
summary(pts_overall)

#need data on standings
#year is the start year of the season

