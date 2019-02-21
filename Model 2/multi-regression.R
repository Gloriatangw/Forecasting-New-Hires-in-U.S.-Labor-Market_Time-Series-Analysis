library(readxl)
library(MASS)
library(forecast)
data  =  read_excel("multitrain.xlsx")

y <- data$`New Hire.Train`
lmodel <- lm(`New Hire.Train`  ~ ., data = data)

round(accuracy(lmodel$fitted.values,y)[,c("RMSE","MAPE")],2)
summary(lmodel)




# Find the multicollinearity
car::vif(lmodel)
