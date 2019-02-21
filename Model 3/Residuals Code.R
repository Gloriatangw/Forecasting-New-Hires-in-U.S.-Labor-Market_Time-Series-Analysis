library(forecast)
setwd("C:/Users/Gyouran/Desktop")

# Use scan function if you are importing a column of numbers
# use read.csv if a table

MRegression.residuals=scan("model5.txt",skip = 1)
tsdisplay(MRegression.residuals)

# 3 lags

