library(ggplot2)
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

#predict points using draft position, exempt goalies
players <- draft[POS!=G]
lm(PTS ~ Overall,data=draft)
