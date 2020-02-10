library(ggplot2)
library(rowr)
library(reshape2)
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
rmse1<-rmse_lm1[2]
lm1 <-lm(SalePrice~Alley,data=ameslist)

#two variable
rmse_lm2 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+column,data=ameslist))))
rmse2<-rmse_lm2[2]
lm2 <- lm(SalePrice~Alley+Neighborhood,data=ameslist)

#three variable
rmse_lm3 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+column,data=ameslist))))
rmse3<-rmse_lm3[2]
lm3 <-lm(SalePrice~Alley+Neighborhood+GrLivArea,data=ameslist) 
  
#four variable
rmse_lm4 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            column,data=ameslist))))
rmse4<-rmse_lm4[2]
lm4 <-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual,data=ameslist)

#five variable
rmse_lm5 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+column,data=ameslist))))
rmse5<-rmse_lm5[2]
lm5 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl,data=ameslist)

#six variable
rmse_lm6 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+column,data=ameslist))))
rmse6<-rmse_lm6[2]
lm6 <-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF,data=ameslist)

#seven variable
rmse_lm7 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+
                                                            column,data=ameslist))))
rmse7<-rmse_lm7[2]
lm7<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1,data=ameslist)

#eight variable
rmse_lm8 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            column,data=ameslist))))
rmse8<-rmse_lm8[2]
lm8<- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+BldgType,data=ameslist)

#nine variable
rmse_lm9 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            BldgType+
                                                            column,data=ameslist))))
rmse9<-rmse_lm9[2]
lm9<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+BldgType+
          ExterQual,data=ameslist)

#ten variable
rmse_lm10 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                            KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                            BldgType+ExterQual+
                                                            column,data=ameslist))))
rmse10<-rmse_lm10[2]
lm10<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
           BldgType+ExterQual+Condition2,data=ameslist)

#elven variable
rmse_lm11 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+
                                                             column,data=ameslist))))
rmse11<-rmse_lm11[2]
lm11<-lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
           BldgType+ExterQual+Condition2+YearBuilt,data=ameslist)

#twelve variable
rmse_lm12 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+
                                                             column,data=ameslist))))
rmse12<-rmse_lm12[2]
lm12<- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
            BldgType+ExterQual+Condition2+YearBuilt+Functional,data=ameslist)

#thirteen variable
rmse_lm13 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             column,data=ameslist))))
rmse13<-rmse_lm13[2]
lm13 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition,data=ameslist)

#fourteen variable
rmse_lm14 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             SaleCondition+
                                                             column,data=ameslist))))
rmse14<-rmse_lm14[2]
lm14 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition+LotArea,data=ameslist)

#fifteen variable
rmse_lm15 <- sort(sapply(ameslist,function(column) rmse(lm(SalePrice~Alley+Neighborhood+GrLivArea+
                                                             KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
                                                             BldgType+ExterQual+Condition2+YearBuilt+Functional+
                                                             SaleCondition+LotArea+
                                                             column,data=ameslist))))
rmse15<-rmse_lm15[2]
lm15 <- lm(SalePrice~Alley+Neighborhood+GrLivArea+KitchenQual+RoofMatl+TotalBsmtSF+BsmtFinSF1+
             BldgType+ExterQual+Condition2+YearBuilt+Functional+SaleCondition+LotArea+GarageCars,data=ameslist)

rmse_all <- c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8,rmse9,rmse10,rmse11,rmse12,rmse13,rmse14,rmse15)
lm_all <- c(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12,lm13,lm14,lm15)
rmse_complex <- c(1:15)
rmse_df <-data.frame(rmse_all,rmse_complex)

ggplot(rmse_df,aes(x=rmse_complex,y=rmse_all))+ylab("RMSE")+ggtitle("RMSE vs. Model Complexity")+
  geom_point()+scale_x_continuous(("Model Complexity"),labels=rmse_complex,breaks=rmse_complex)+
  theme(plot.title=element_text(hjust=0.5))

####part 2################################################
rmse_pred <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

set.seed(9)
num_obs <- nrow(ameslist)

train_index <- sample(num_obs, size = trunc(0.50 * num_obs))
train_data <- ameslist[train_index, ]
test_data <- ameslist[-train_index, ]

fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

#simplified function as SalePrice is the only response variable for this exercise
get_rmse = function(model, data) {
  rmse_pred(actual = subset(data, select = "SalePrice", drop = TRUE),
       predicted = predict(model, data))
}

get_rmse(model = fit_0, data = train_data) # train RMSE
get_rmse(model = fit_0, data = test_data) # test RMSE

rmse_train <-c(get_rmse(lm1,train_data),get_rmse(lm2,train_data),get_rmse(lm3,train_data),
               get_rmse(lm4,train_data),get_rmse(lm5,train_data),get_rmse(lm6,train_data),
               get_rmse(lm7,train_data),get_rmse(lm8,train_data),get_rmse(lm9,train_data),
               get_rmse(lm10,train_data),get_rmse(lm11,train_data),get_rmse(lm12,train_data),
               get_rmse(lm13,train_data),get_rmse(lm14,train_data),get_rmse(lm15,train_data))
rmse_test <-c(get_rmse(lm1,test_data),get_rmse(lm2,test_data),get_rmse(lm3,test_data),
              get_rmse(lm4,test_data),get_rmse(lm5,test_data),get_rmse(lm6,test_data),
              get_rmse(lm7,test_data),get_rmse(lm8,test_data),get_rmse(lm9,test_data),
              get_rmse(lm10,test_data),get_rmse(lm11,test_data),get_rmse(lm12,test_data),
              get_rmse(lm13,test_data),get_rmse(lm14,test_data),get_rmse(lm15,test_data))
rmse_df_2 <- data.frame(rmse_train,rmse_test,rmse_complex)
rmse_df_2_melted <- melt(rmse_df_2,id="rmse_complex")

#question 1
ggplot(rmse_df_2_melted,aes(x=rmse_complex,y=value,color=variable))+ylab("RMSE")+ggtitle("RMSE vs. Model Complexity")+
  geom_point()+scale_x_continuous(("Model Complexity"),labels=rmse_complex,breaks=rmse_complex)+
  theme(plot.title=element_text(hjust=0.5))

#question 2

optimizer <- function(var){
  if (class(var) == "integer"){
    linear_rmse<-get_rmse(lm(SalePrice~var,data=train_data),train_data)
    x2_rmse<-get_rmse(lm(SalePrice~(var^2),data=train_data),train_data)
    ifelse((min(var)>0),ln_rmse<-get_rmse(lm(SalePrice~log(var),data=train_data),train_data),ln_rmse<-99999999)
    #ifelse(length(unique(var))>2,poly2_rmse<-1.02*(get_rmse(lm(SalePrice~poly(var,2),data=train_data),train_data)),poly2_rmse<-999999999)
    #ifelse(length(unique(var))>3,poly3_rmse<-get_rmse(lm(SalePrice~poly(var,3),data=train_data),train_data),poly3_rmse<-999999999)
    #ifelse(length(unique(var))>4,poly4_rmse<-get_rmse(lm(SalePrice~poly(var,4),data=train_data),train_data),poly4_rmse<-999999999)
    #ifelse(length(unique(var))>5,poly5_rmse<-get_rmse(lm(SalePrice~poly(var,5),data=train_data),train_data),poly5_rmse<-999999999)
    recip_rmse<-get_rmse(lm(SalePrice~(1/var),data=train_data),train_data)
    power_.2_rmse<-get_rmse(lm(SalePrice~I(var^.2),data=train_data),train_data)
    power_.4_rmse<-get_rmse(lm(SalePrice~I(var^.4),data=train_data),train_data)
    power_.5_rmse<-get_rmse(lm(SalePrice~I(var^.5),data=train_data),train_data)
    power_.6_rmse<-get_rmse(lm(SalePrice~I(var^.6),data=train_data),train_data)
    power_.8_rmse<-get_rmse(lm(SalePrice~I(var^.8),data=train_data),train_data)
    #vector of results
    rmse_comp<-c(linear_rmse,x2_rmse,ln_rmse,recip_rmse,power_.2_rmse,
                 power_.4_rmse,power_.5_rmse,power_.6_rmse,power_.8_rmse)
    names(rmse_comp) <- c("linear","x^2","ln","reciprocal","power_.2_rmse","power_.4_rmse",
                          "power_.5_rmse","power_.6_rmse","power_.8_rmse")
    rmse_comp[which.min(rmse_comp)]
  }
}
#applies the optimization function to determine the best transformation
sapply(train_data,optimizer)

#all the variables in a linear regression
benchmark_lm <- lm(SalePrice~.,data=train_data)
get_rmse(benchmark_lm,train_data)

#knocked out variables, linear
benchmark_simple <- lm(SalePrice~(MSSubClass)+(MSZoning)+log(LotArea)+ (Street)+(Alley)+(LotShape)+(LandContour)+  
    (Utilities)+(LotConfig)+(LandSlope)+(Condition1)+(BldgType)+(HouseStyle)+   
    (YearBuilt)+(YearRemodAdd)+(RoofStyle)+(MasVnrType)+(MasVnrArea)+   
    (ExterQual)+(Foundation)+(BsmtFinSF1)+I(BsmtFinSF2)+(BsmtUnfSF)+(TotalBsmtSF)+      
    (HeatingQC)+(CentralAir)+(X1stFlrSF)+(X2ndFlrSF)+(LowQualFinSF)+(GrLivArea)+(BsmtFullBath)+ 
    (BsmtHalfBath)+(FullBath)+(HalfBath)+(BedroomAbvGr)+(KitchenAbvGr)+(KitchenQual)+(TotRmsAbvGrd)+(Functional)+   
    (Fireplaces)+(GarageType)+(GarageCars)+(GarageArea)+(PavedDrive)+(WoodDeckSF)+(OpenPorchSF)+  
    (EnclosedPorch)+(X3SsnPorch)+(ScreenPorch)+(PoolArea)+(PoolQC)+(Fence)+(MiscVal)+      
    (MoSold)+(YrSold)+(SaleType),data=train_data)
get_rmse(benchmark_simple,train_data)
get_rmse(benchmark_simple,test_data)


#knocked out droplevels variables
master2_lm <- lm(SalePrice~(MSSubClass)+(MSZoning)+log(LotArea)+ (Street)+(Alley)+(LotShape)+(LandContour)+  
    (LotConfig)+(LandSlope)+(Condition1)+(BldgType)+(HouseStyle)+   
    poly(YearBuilt,2)+(YearRemodAdd)+(RoofStyle)+(MasVnrType)+(MasVnrArea)+   
    (ExterQual)+(Foundation)+poly(BsmtFinSF1,2)+I(BsmtFinSF2^.2)+(BsmtUnfSF)+(TotalBsmtSF)+      
    (HeatingQC)+(CentralAir)+(X1stFlrSF)+poly(X2ndFlrSF,2)+I(LowQualFinSF^.2)+I(GrLivArea^.8)+I(BsmtFullBath^.2)+ 
    I(BsmtHalfBath^.2)+(FullBath)+I(HalfBath^.2)+(BedroomAbvGr)+poly(KitchenAbvGr,2)+(KitchenQual)+(TotRmsAbvGrd)+(Functional)+   
    I(Fireplaces^.4)+(GarageType)+(GarageCars)+(GarageArea)+(PavedDrive)+I(WoodDeckSF^.4)+I(OpenPorchSF^.2)+  
    I(EnclosedPorch^.2)+I(X3SsnPorch^.2)+(ScreenPorch)+(PoolArea)+(PoolQC)+(Fence)+I(MiscVal^.2)+      
    (MoSold)+log(YrSold)+(SaleType),data=train_data)

get_rmse(master2_lm,train_data)
get_rmse(master2_lm,test_data)



#full with transformations
master_lm <- lm(SalePrice~(MSSubClass)+(MSZoning)+log(LotArea)+ (Street)+(Alley)+(LotShape)+(LandContour)+  
                  (Utilities)+(LotConfig)+(LandSlope)+(Neighborhood)+(Condition1)+(Condition2)+(BldgType)+(HouseStyle)+   
                  poly(YearBuilt,2)+(YearRemodAdd)+(RoofStyle)+(RoofMatl)+(Exterior1st)+(Exterior2nd)+(MasVnrType)+(MasVnrArea)+   
                  (ExterQual)+(ExterCond)+(Foundation)+poly(BsmtFinSF1,2)+I(BsmtFinSF2^.2)+(BsmtUnfSF)+(TotalBsmtSF)+(Heating)+      
                  (HeatingQC)+(CentralAir)+(Electrical)+(X1stFlrSF)+poly(X2ndFlrSF,2)+I(LowQualFinSF^.2)+I(GrLivArea^.8)+I(BsmtFullBath^.2)+ 
                  I(BsmtHalfBath^.2)+(FullBath)+I(HalfBath^.2)+(BedroomAbvGr)+log(KitchenAbvGr)+(KitchenQual)+(TotRmsAbvGrd)+(Functional)+   
                  I(Fireplaces^.4)+(GarageType)+(GarageFinish)+(GarageCars)+(GarageArea)+(PavedDrive)+I(WoodDeckSF^.4)+I(OpenPorchSF^.2)+  
                  I(EnclosedPorch^.2)+I(X3SsnPorch^.2)+(ScreenPorch)+(PoolArea)+(PoolQC)+(Fence)+(MiscFeature)+I(MiscVal^.2)+      
                  (MoSold)+log(YrSold)+(SaleType)+(SaleCondition),data=train_data)

summary(master_lm)
test_data$Neighborhood <- droplevels(test_data$Neighborhood,"Blueste")
test_data$Condition2 <- droplevels(test_data$Condition2,c("Feedr","PosA","PosN"))
test_data$RoofMatl <- droplevels(test_data$RoofMatl,"Roll")
test_data$Exterior1st <- droplevels(test_data$Exterior1st,c("AsphShn","BrkComm","ImStucc","Stone"))
test_data$Exterior2nd <- droplevels(test_data$Exterior2nd,c("AsphShn","Stone"))
test_data$ExterCond <- droplevels(test_data$ExterCond,"Ex")
test_data$Heating <- droplevels(test_data$Heating,c("Floor","Wall"))
test_data$Electrical <- droplevels(test_data$Electrical,c("FuseP",NA))
test_data$MiscFeature <- droplevels(test_data$MiscFeature,"TenC")
test_data$SaleCondition <- droplevels(test_data$SaleCondition,"AdjLand")

get_rmse(benchmark_lm,test_data)
summary(master2_lm)
