library(ggplot2)
library(rowr)

ameslist <- as.data.frame(read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = TRUE,sep = ","))

#Run the above, but instead specifying header = FALSE. 
#ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = FALSE, sep = ",")

#What data type are the various columns? 
#typeof(ameslist$v1)
#this reurns "NULL" which makes sense as the column contains mixed types of data

#Now try ommitting the line altogether. 
#ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv", sep = ",")
#What is the default behavior of the read.table function?1
#default behavior is same as header=FALSE

names(ameslist)

#Go through the variables in the dataset and make a note about your 
#interpretation for each. 
#Many will be obvious, but some require additional thought.
#Interpretations for confusing variables are:
#LotShape describes what the lot looks like, reg likely means rectangular and IR1, IR2, etc. are for irregular shapes
#MSSubClass is the type of dwelling in the sale according to googling it.
#The conditions do not provide enough information to say. It may be useful to check if they cause differnces in the sale price to see if they can be ignored. 

typeof(ameslist)

GarageTemp <- model.matrix( ~ GarageType - 1, data=ameslist )
ameslist <- cbind.fill(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

#Utilizing a similar approach to what you did above, fix this so that the only outputs are zero and one.
#our code does not output NA values, merging instead of cbinding may be the reason

Ames<-ameslist[which(sapply(ameslist,is.numeric)==TRUE)]

names(Ames)
pairs(~LotArea+YearBuilt+OverallQual+OverallCond+BsmtFinSF1+TotRmsAbvGrd+MoSold+FullBath+
        HalfBath+GarageCars+BedroomAbvGr+YrSold,data=Ames)

Ames_small <- Ames[,c(4,5,6,7,10,20,21,22,24,27,36,37,38)]

cor(Ames_small)
#most matched prior beliefs, overall condition and sale price were negatively correlated, which was surprising
reg<-lm(SalePrice~GrLivArea,data=ameslist)
ggplot(ameslist,aes(x=GrLivArea,y=SalePrice))+
  geom_point()+
  geom_abline(intercept=coef(reg)["(Intercept)"],slope=coef(reg)["GrLivArea"])

#the following line gives information abou the largest outlier above the line
ameslist[which.max(ameslist$SalePrice-(ameslist$GrLivArea*coef(reg)["GrLivArea"]+coef(reg)["(Intercept)"])),]

#/////////////////exercise 2///////////////////////////


