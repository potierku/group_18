library(plyr)

#reads the csv
data <-read.csv("bank.csv",header=TRUE)
data$marital <-revalue(data$marital,c("married"=3,"single"=2,"divorced"=1))
reg <- lm(balance ~ age + marital ,data=data)
#anova(null_mpg_model,full_mpg_model)
summary(reg)
