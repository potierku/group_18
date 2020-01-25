#reads the csv
data <-read.csv("bank.csv",header=TRUE)
data$y <- NULL
full_model <- lm(balance ~ .,data=data)
summary(full_model)
#the variables to include in the null model were based upon those 
#which were significant to at least 10% in the full model.
null_model <- lm(balance ~ age + marital+job+
                   education+default+loan+month,data=data)
anova(null_model,full_model)
#since the P value is 0.7739, this suggests that the full model
#really isn't any better than the null model. 
