#Upload libraries
library(forecast)
library(ggplot2)
library(scales)


data=read.csv("data.csv")
head(data,3)
tail(data,3)

data.ts=ts(data$New.Hire,start=c(2000,12),frequency =12 )

# Create training and testing setsusing function window

# senario 1
train.start.1=c(2000,12)
train.end.1=c(2016,12)

test.start.1=c(2017,1)
test.end.1=c(2017,12)

train.ts.1=window(data.ts,end=train.end.1)
test.ts.1=window(data.ts,start=test.start.1, end = test.end.1)

nTrain.1=length(train.ts.1)
nTest.1=length(test.ts.1)






##################################################################################################
# M1 = SEASONAL NAIVE MODEL
##################################################################################################

# Build a seasonal naive model using training set and label it as M1
# Since this model is trivial, it generates forecast unlike other models such as smoothing or ARIMA, 
# thus we need to indicate forecast horizon h=nTest (nTest represents how large testing/valdiation set is)
# We will also indicate confidence level for prediction interval

M1=snaive(train.ts.1,h=nTest.1,level=95,lambda="auto")

# Explanation:


# Accuracy metrics
round(accuracy(M1,test.ts.1)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit
# Note that these accuarcy metrics coincide with teh ones I got in excel. 

#RMSE MAPE
#Training set 336.16 5.54
#Test set     229.91 3.86

# Residual diagnistics
checkresiduals(M1)
# Residuals do not resemble white noise. There is depends/patterns that seasonal naive model 
# was unable to capture. Also there seems to be outliers and it would be interesting to see what happened 
# during quarters of extreme outlying points. 




##################################################################################################
# M2 = SMOOTHING MODEL
##################################################################################################

# Build teh best smoothing model
M2=ets(train.ts.1,lambda="auto")

# What is it?
M2

#ETS(A,Ad,A) -> additive noise(first A), additive trend (second A) and additive seasonality (3rd A)


M2F=forecast(M2,h=nTest.1,level=95)


# Accuracy metrics
round(accuracy(M2F,test.ts.1)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit

#               RMSE MAPE
#Training set 180.18 3.08
#Test set     212.37 3.54
# residual diagnostics
checkresiduals(M2)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate


##################################################################################################
# M3 = Regression, IN excel
##################################################################################################


##################################################################################################
# M4 = ARIMA MODEL
##################################################################################################

# Build the best arima model
M4=auto.arima(train.ts.1,lambda="auto")

# What is it?
M4

#ARIMA(2,1,2)(1,1,2)[12]  with drift -> this model includes y(t-1), y(t-4) and first order seasonal differencing and drift(trend)

# Generate forecast
M4F=forecast(M4,h=nTest.1,level=95)

# We may print the forecasts and corresponding 95 prediction intervals [lower bound,Upper bound]
M4F


# Accuracy metrics
round(accuracy(M4F,test.ts.1)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit

#               RMSE MAPE
#Training set 174.41 2.92
#Test set     183.48 3.10

# residual diagnostics
checkresiduals(M4)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate


# senario 2
train.start.2=c(2000,12)
train.end.2=c(2016,12)

test.start.2=c(2017,1)
test.end.2=c(2018,9)

train.ts.2=window(data.ts,end=train.end.2)
test.ts.2=window(data.ts,start=test.start.2, end = test.end.2)

nTrain.2=length(train.ts.2)
nTest.2=length(test.ts.2)






##################################################################################################
# M1 = SEASONAL NAIVE MODEL
##################################################################################################

# Build a seasonal naive model using training set and label it as M1
# Since this model is trivial, it generates forecast unlike other models such as smoothing or ARIMA, 
# thus we need to indicate forecast horizon h=nTest (nTest represents how large testing/valdiation set is)
# We will also indicate confidence level for prediction interval

M1.2=snaive(train.ts.2,h=nTest.2,level=95,lambda="auto")

# Explanation:


# Accuracy metrics
round(accuracy(M1.2,test.ts.2)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit
# Note that these accuarcy metrics coincide with teh ones I got in excel. 


M1.2F=forecast(M1.2,h=nTest.2,level=95)

#RMSE MAPE
#Training set 336.16 5.54
#Test set     353.45 5.32

# Residual diagnistics
checkresiduals(M1)
# Residuals do not resemble white noise. There is depends/patterns that seasonal naive model 
# was unable to capture. Also there seems to be outliers and it would be interesting to see what happened 
# during quarters of extreme outlying points. 


# draw graph
plot.snaive = data.frame(time = data$DATE,
           train = c(train.ts.2, rep(NA,length(test.ts.2))), 
           fitted = c(M1.2$fitted, rep(NA,length(test.ts.2))),
           test = c(rep(NA, length(train.ts.2)), test.ts.2),
           predict = c(rep(NA, length(train.ts.2)), M1.2F$mean),
           upper = c(rep(NA, length(train.ts.2)), M1.2F$upper),
           lower = c(rep(NA, length(train.ts.2)), M1.2F$lower))

plot.snaive %>%
  ggplot(aes(x = as.Date(time, format = '%m/%d/%y')))+
  geom_line(aes(y = train, group = 1), color = "blue") +
  geom_line(aes(y = test, group = 1), color = "red") +
  geom_line(aes(y = fitted, group = 1), color = "gray") +
  geom_line(aes(y = predict, group = 1), color= 'orange')+
 # scale_color_manual("", 
 #                     breaks = c("New Hire.train", "New Hire.test",
 #                                "Prediction.train","Prediction.test"),
 #                     values = c("blue", "red", "gray",'orange')) +
  labs(title = "SNAIVE", x = "Date", y = "New Hire")+
  scale_x_date(labels = date_format("%m/%d/%y"))+
  theme(plot.title = element_text(size=16,hjust = 0.5))
  


##################################################################################################
# M2 = SMOOTHING MODEL
##################################################################################################

# Build teh best smoothing model
M2.2=ets(train.ts.2,lambda="auto")

# What is it?
M2.2

#ETS(A,Ad,A) -> additive noise(first A), additive trend (second A) and additive seasonality (3rd A)


M2.2F=forecast(M2.2,h=nTest.2,level=95)


# Accuracy metrics
round(accuracy(M2.2F,test.ts.2)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit


#               RMSE MAPE
#Training set 180.18 3.08
#Test set     335.53 5.02

# residual diagnostics
checkresiduals(M2)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate


plot.ets = data.frame(time = data$DATE,
                         train = c(train.ts.2, rep(NA,length(test.ts.2))), 
                         fitted = c(M2.2$fitted, rep(NA,length(test.ts.2))),
                         test = c(rep(NA, length(train.ts.2)), test.ts.2),
                         predict = c(rep(NA, length(train.ts.2)), M2.2F$mean),
                         upper = c(rep(NA, length(train.ts.2)), M2.2F$upper),
                         lower = c(rep(NA, length(train.ts.2)), M2.2F$lower))

plot.ets %>%
  ggplot(aes(x = as.Date(time, format = '%m/%d/%y')))+
  geom_line(aes(y = train, group = 1), color = "blue") +
  geom_line(aes(y = test, group = 1), color = "red") +
  geom_line(aes(y = fitted, group = 1), color = "gray") +
  geom_line(aes(y = predict, group = 1), color= 'orange')+
  # scale_color_manual("", 
  #                     breaks = c("New Hire.train", "New Hire.test",
  #                                "Prediction.train","Prediction.test"),
  #                     values = c("blue", "red", "gray",'orange')) +
  labs(title = "SMOOTING", x = "Date", y = "New Hire")+
  scale_x_date(labels = date_format("%m/%d/%y"))+
  theme( plot.title = element_text(size=16,hjust = 0.5))


##################################################################################################
# M4 = ARIMA MODEL
##################################################################################################

# Build the best arima model
M4.2=auto.arima(train.ts.2,lambda="auto")

# What is it?
M4.2

#ARIMA(2,1,2)(1,1,2)[12] with drift -> this model includes y(t-1), y(t-4) and first order seasonal differencing and drift(trend)

# Generate forecast
M4.2F=forecast(M4.2,h=nTest.2,level=95)

# We may print the forecasts and corresponding 95 prediction intervals [lower bound,Upper bound]
M4.2F


# Accuracy metrics
round(accuracy(M4.2F,test.ts.2)[,c("RMSE","MAPE")],2) # rounded to 2nd decimal digit



#               RMSE MAPE
#Training set 174.41 2.92
#Test set     269.06 4.07

# residual diagnostics
checkresiduals(M4.2)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate



plot.arima = data.frame(time = data$DATE,
                      train = c(train.ts.2, rep(NA,length(test.ts.2))), 
                      fitted = c(M4.2$fitted, rep(NA,length(test.ts.2))),
                      test = c(rep(NA, length(train.ts.2)), test.ts.2),
                      predict = c(rep(NA, length(train.ts.2)), M4.2F$mean),
                      upper = c(rep(NA, length(train.ts.2)), M4.2F$upper),
                      lower = c(rep(NA, length(train.ts.2)), M4.2F$lower))

plot.arima %>%
  ggplot(aes(x = as.Date(time, format = '%m/%d/%y')))+
  geom_line(aes(y = train, group = 1), color = "blue") +
  geom_line(aes(y = test, group = 1), color = "red") +
  geom_line(aes(y = fitted, group = 1), color = "gray") +
  geom_line(aes(y = predict, group = 1), color= 'orange')+
  # scale_color_manual("", 
  #                     breaks = c("New Hire.train", "New Hire.test",
  #                                "Prediction.train","Prediction.test"),
  #                     values = c("blue", "red", "gray",'orange')) +
  labs(title = "ARIMA", x = "Date", y = "New Hire")+
  scale_x_date(labels = date_format("%m/%d/%y"))+
  theme( plot.title = element_text(size=16,hjust = 0.5))

##################################################################################################
# M = Champion model to forecast future values for the next 2 years (8 quarters)
##################################################################################################

# If your champion model is an ARIMA that you built in R 
# Refit it using all data ( training and testing) and predict next 2 years of data

M.ARIMA=auto.arima(data.ts,lambda="auto")

# Forecast future
MF.ARIMA=forecast(M.ARIMA,h=12,level=95)

# Print Forecasts and PI's
MF.ARIMA
plot(MF.ARIMA)



