library(ggplot2)
library(rowr)

#functions
rmse = function(model){
  sqrt(mean(resid(model)^2))
} 

get_complexity = function(model) {
  length(coef(model)) - 1
}

#import data
ameslist <- as.data.frame(read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = TRUE,sep = ","))

#identifies columns with NA values
na_cols <- names(which(colSums(is.na(ameslist))>0))

#fixes for these columns
ameslist$Alley <- addNA(ameslist$Alley) #lot is not on an alley
ameslist$MasVnrType[is.na(ameslist$MasVnrType)] <- "None" #replace NA with None
ameslist$MasVnrArea[is.na(ameslist$MasVnrArea)] <- 0 #replace NA with 0
ameslist$Electrical <- addNA(ameslist$Electrical)
ameslist$GarageType <- addNA(ameslist$GarageType)
ameslist$GarageFinish <- addNA(ameslist$GarageFinish)
ameslist$PoolQC <- addNA(ameslist$PoolQC)
ameslist$Fence <- addNA(ameslist$Fence)
ameslist$MiscFeature <- addNA(ameslist$MiscFeature)

#delete overallcond and overallqual
#additionally, delete variables with unuseable NA (continuous variables that describe factor variables with y/n)
ameslist <- within(ameslist,rm("OverallCond","OverallQual","LotFrontage","BsmtQual",
                               "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
                               "GarageYrBlt","GarageQual","GarageCond"))
#exercise 1
#forward selection
#null model
null_model <- lm(SalePrice~NULL,data=ameslist)

#one variable
rmse_lm1 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~column,data=ameslist))))
rmse_lm1
lm1 <-lm(SalePrice~Alley,data=ameslist)

#two variable
rmse_lm2 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+column,data=ameslist))))
rmse_lm2
lm2 <- lm(SalePrice~Alley+Neighborhood,data=ameslist)

#three variable
rmse_lm3 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+column,data=ameslist))))
rmse_lm3
lm3 <-lm(SalePrice~Alley+Neighborhood+GrLivArea,data=ameslist) 
  
#four variable
rmse_lm4 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            column,data=ameslist))))
rmse_lm4
lm4 <-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual,data=ameslist)

#five variable
rmse_lm5 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+column,data=ameslist))))
rmse_lm5
lm5 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl,data=ameslist)

#six variable
rmse_lm6 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+column,data=ameslist))))
rmse_lm6
lm6 <-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF,data=ameslist)

#seven variable
rmse_lm7 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+
                                                            column,data=ameslist))))
rmse_lm7
lm7<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtSF1,data=ameslist)

#eight variable
rmse_lm8 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            column,data=ameslist))))
rmse_lm8
lm8<- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+BldgType,data=ameslist)

#nine variable
rmse_lm9 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            BldgType+
                                                            column,data=ameslist))))
rmse_lm9
lm9<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+BldgType+
          ExterQual,data=ameslist)

#ten variable
rmse_lm10 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            BldgType+ExterQual+
                                                            column,data=ameslist))))
rmse_lm10
lm10<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
           BldgType+ExterQual+Condition2,data=ameslist)

#elven variable
rmse_lm11 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+
                                                             column,data=ameslist))))
rmse_lm11
lm11<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
           BldgType+ExterQual+Condition2+YearBuilt,data=ameslist)

#twelve variable
rmse_lm12 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+
                                                             column,data=ameslist))))
rmse_lm12
rm12<- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
            BldgType+ExterQual+Condition2+YearBuilt+Functional,data=ameslist)

#thirteen variable
rmse_lm13 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             column,data=ameslist))))
rmse_lm13
lm13 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition,data=ameslist)

#fourteen variable
rmse_lm14 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             SaleCondition+
                                                             column,data=ameslist))))
rmse_lm14
lm14 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition+LotArea,data=ameslist)

#fifteen variable
rmse_lm15 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             SaleCondition+LotArea+
                                                             column,data=ameslist))))
rmse_lm15
lm15 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition+LotArea+GarageCars,data=ameslist)






