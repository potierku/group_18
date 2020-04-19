library(ggplot2)
library(dplyr)
library(stringr)
#import draft data
draft <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/draft.txt",stringsAsFactors = FALSE)

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
standings <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/standings.csv")
standings$Team[which(standings$Team=="Anaheim Mighty Ducks")]<- "Anaheim Ducks"
standings$Team[which(standings$Team=="Phoenix Coyotes")]<- "Arizona Coyotes"
standings$Team[which(standings$Team=="Atlanta Thrashers")]<- "Winnipeg Jets"
standings$Team[which(standings$Team=="Quebec Nordiques")]<- "Colorado Avalanche"
standings$Team[which(standings$Team=="Hartford Whalers")]<- "Carolina Hurricanes"
standings$Team[which(standings$Team=="Minnesota North Stars")]<- "Dallas Stars"
standings <- standings[which(standings$Team != "Vegas Golden Knights"),]
standings$Team <- as.character(standings$Team)
standings$Team <- as.factor(standings$Team)
table(standings$Team)

#import goalies data
goalies <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/goalies_data.csv")

#import skaters data
skaters <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/skaters_data.csv")
skaters$ATOI <- skaters$TOI/skaters$GP
skaters$year_player <- paste(skaters$ï..Year,skaters$Player)
#this for loop is used to remove instances of a player being traded (whole season stats only)
for (i in c(1:length(skaters$year_player))){
  while (ifelse(i<length(skaters$year_player),skaters$year_player[i]==skaters$year_player[i+1],FALSE)){
    skaters<-skaters[-c(i+1),]
  }
}
skaters$Player <- as.character(skaters$Player)
#remove asterisks from skaters data
for (i in c(1:length(skaters$Player))){
  skaters$Player[i] <-(gsub("\\*","",skaters$Player[i]))
}


#import city temps and years old
temps <- read.csv("https://raw.githubusercontent.com/potierku/talk_data_to_me/master/final_project/NHL_city_temp_data.csv")

#year is the start year of the season

#predict points using draft position, exempt goalies
#may split up by position
draft$ppgp <- draft$PTS/draft$GP #created points per games player metric
players <- draft[which(draft$Pos!='G' & draft$GP>10),] #excludes goalies and players who have played less than 10 games

ggplot(data=players,aes(x=Overall, y=ppgp))+
  geom_point()

pts_overall <- lm(ppgp ~ poly(Overall,2),data=draft)
summary(pts_overall)

#predict points as a function of age
#cut off at 20 Pts to exclude marginal players that may not have a full career
peak_age_data <- skaters[which(skaters$PTS>20 & skaters$Age <40),]
ggplot(data=peak_age_data,aes(x=Age, y=PTS))+
  geom_point()
pts_age <- lm(PTS ~ poly(Age,2,raw=TRUE),data=peak_age_data)
summary(pts_age)
#took derivative of results to find age
(peak_age <- ((-1)*pts_age$coefficients[2])/(2*pts_age$coefficients[3]))

#average time on ice versus points plot
ggplot(data=skaters,aes(x=ATOI, y=PTS))+
  geom_point()+
  geom_smooth(method="auto")+
  facet_wrap(~Pos)
pts_atoi <- lm(PTS~poly(ATOI,2,raw=TRUE),data=skaters)
summary(pts_atoi)

################# Does Tanking Work? #####################
#regression to predict points in any given season, uses 5 lags of total team Points
tanking <- function(team){
  team_data <- standings[which(standings$Team == team),]
  team_reg <- lm(Pts ~ lag(Pts,n=1L) + lag(Pts,n=2L) + 
                 lag(Pts,n=3L)+ lag(Pts,n=4L)+ lag(Pts,n=5L),data=team_data)
  team_reg$coefficients
}

teams<- unique(standings$Team) #list of teams
tanking_results <- as.data.frame(lapply(teams,tanking)) #list apply
tanking_results <- t(tanking_results) #transpose results
colnames(tanking_results)<-c("intercept","lag1","lag2","lag3","lag4","lag5")
tanking_results <- as.data.frame(tanking_results) #had issue with atomic vectors
tanking_results<- rbind(tanking_results,sapply(tanking_results,mean)) #create new bottom row with averages
row.names(tanking_results) <- c(as.character(unique(standings$Team)),"Average") #labeling nicely

#inputs are age, draft position, position, amateur lg., time trend the year
draft_results <- as.data.frame(colnames(c("Year","Round","Overall","Team","Player","Nat.","POS","Age","To","Amateur.Team","Amateur.Lg","GP","G","A","PTS","+_-","PIM")))
for (i in c(1: (length(draft$Player)))){
  df <- skaters[which(as.character(skaters$Player) == as.character(draft$Player[i])),]
  df <- df[which(df$GP>10),]
  df <- df[c(1:3),]
  numerics <- df[,c(6:11)]
  draft_results <- rbind(draft_results,as.data.frame(c(draft[i,c(1:11)],sapply(numerics,sum))))
}

#predict +/- as well

#predict save percentage with draft position

#use glm to predict if play in NHL or not

#predict games with draft postion and amateur league

#test salary cap vs. no salary cap 

#predict teams points with performance in previous years (use lags 1-5)

#glm make/miss playoffs


