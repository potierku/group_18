library(ggplot2)
library(rowr)

ameslist <- as.data.frame(read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = TRUE,sep = ","))

GarageTemp <- model.matrix( ~ GarageType - 1, data=ameslist )
ameslist <- cbind.fill(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

Ames<-ameslist[which(sapply(ameslist,is.numeric)==TRUE)]

