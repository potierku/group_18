library(ggplot2)
library(rowr)

#import data
ameslist <- as.data.frame(read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = TRUE,sep = ","))

#exercise 1
#delete overallcond and overallqual
ameslist <- within(ameslist,rm("OverallCond","OverallQual"))

#forward selection
null_model <- lm(SalePrice~NULL,data=ameslist)

rmse_lm <- sort(sapply(ameslist,lm_sales))
rmse_lm[2]

#garage combined
#GarageTemp <- model.matrix( ~ GarageType - 1, data=ameslist )
#ameslist <- cbind.fill(ameslist, GarageTemp)
#ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
#unique(ameslist$GarageOutside)

#numeric only
#Ames<-ameslist[which(sapply(ameslist,is.numeric)==TRUE)]

#rmse = function(actual, predicted) {
#  sqrt(mean((actual - predicted) ^ 2))
#}

rmse = function(model){
  sqrt(mean(resid(model)^2))
} 

get_complexity = function(model) {
  length(coef(model)) - 1
}

lm_sales = function(column){
    test_lm<-lm(SalePrice~column, data = ameslist)
    rmse(test_lm)
}


