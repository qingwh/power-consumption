
# Title: data visualization and time series regression modeling in R

# Last update: 2018.07.31

# File:  data visualization and time series analysis.R

###############
# Project Notes
###############

# Summarize project: We will use visualization techniques to gain a deeper understanding of the power consumption data.
# We will forecast the trends of time series data-submeter1,submeter2,submeter3 by using time series linear regression,
#HoltWinters Simple Exponential Smoothing and ARIMA model.

################
# Load packages
################
install.packages("RMySQL")
install.packages("dplyr")
install.packages("lubridate")
install.packages("plotly")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
library(RMySQL)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(plotly)

#################################
# obtain the data using SQL query
################################

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'yr_2006')
dbListFields(con,'yr_2007')
dbListFields(con,'yr_2008')
dbListFields(con,'yr_2009')
dbListFields(con,'yr_2010')

## Use the dbGetQuery function to download tables 2006 through 2010 with the specified attributes.
yr_2006<- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")
yr_2007<- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr_2008<- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr_2009<- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")
yr_2010<- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")

##Investigate each new data frame
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006) 
anyNA(yr_2006)
str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007) 
is.na(yr_2007)
anyNA(yr_2007)

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008) 
anyNA(yr_2008)

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009) 
anyNA(yr_2009)
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010) 
anyNA(yr_2010)

## Combine tables into one dataframe using dplyr
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)

str(newDF)
summary(newDF)
head(newDF)
tail(newDF) 

#############
# Preprocess
#############

## Combine Date and Time attribute values in a new attribute column
yrc <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yrc)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
yrcm<- yrc[,c(ncol(yrc), 1:ncol(yrc)-1)]
head(yrcm)
## Convert DateTime from POSIXlt to POSIXct 
yrcm$DateTime <- as.POSIXct(yrcm$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yrcm$DateTime, "tzone") <- "Europe/Paris"
## Inspect the data types
str(yrcm)

## Create "year" attribute with lubridate

yrcm$year <- year(yrcm$DateTime)
str(yrcm)
## Create "quarter" attribute with lubridate
yrcm$quarter <- quarter(yrcm$DateTime)
str(yrcm)

## Create "month" attribute with lubridate
yrcm$month <- month(yrcm$DateTime)
str(yrcm)
## Create "week" attribute with lubridate
yrcm$week <- week(yrcm$DateTime)
str(yrcm)
## Create "weekday" attribute with lubridate
yrcm$weekday <- wday(yrcm$DateTime)
str(yrcm)
## Create "day" attribute with lubridate
yrcm$day <- day(yrcm$DateTime)
str(yrcm)
## Create "hour" attribute with lubridate
yrcm$hour <- hour(yrcm$DateTime)
str(yrcm)
## Create "minute" attribute with lubridate
yrcm$minute <- minute(yrcm$DateTime)
str(yrcm)

summary(yrcm) 

# plot
hist(yrcm$Sub_metering_1)
hist(yrcm$Sub_metering_2) 
hist(yrcm$Sub_metering_3)

#######################################
#Subsetting and Meaningful Time Periods
#######################################

## Subset the second week of 2008 - All Observations
houseWeek <- filter(yrcm, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(yrcm, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Reducing Granularity## 

###### Power Consumption in the 9th day of January 2008 - 10 Minute frequency###### 

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(yrcm, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###### Power Consumption in the second week of 2008 - 2 hour frequency###### 

## Subset the second week of 2008 - 2 hours frequency
houseWeek <- filter(yrcm, year == 2008 & week == 2 & (hour == 0| hour == 2|hour == 6|hour == 8|hour == 10|hour == 12|hour == 14|hour == 16|hour == 18|hour == 20|hour == 22)& minute==0)
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 2 hours frequency
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###### Power Consumption in the second week of 2008 - 1 hour frequency###### 

## Subset the second week of 2008 -  1 hour frequency
houseWeek <- filter(yrcm, year == 2008 & week == 2 & minute==0)
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 1 hour frequency
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###### Power Consumption in January, 2008 - 4 hours frequency###### 

## Subset January 2008 - 4 hours frequency
housemonth <- filter(yrcm, year == 2008 & month == 1 & (hour == 0| hour == 4|hour == 8|hour == 12|hour == 16|hour == 20) & minute == 0)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 4 hours frequency
plot_ly(housemonth, x = ~housemonth$DateTime, y = ~housemonth$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~housemonth$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~housemonth$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


###### Power Consumption in January, 2008 - 6 hours frequency######   

## Subset January 2008 - 6 hours frequency
housemonth <- filter(yrcm, year == 2008 & month == 1 & (hour == 0| hour == 6|hour == 12|hour == 18) & minute == 0)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 6 hours frequency
plot_ly(housemonth, x = ~housemonth$DateTime, y = ~housemonth$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~housemonth$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~housemonth$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

############################################
## prepare data for Time Series Analysis
############################################

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(yrcm, weekday == 2 & hour == 20 & minute == 1)
str(house070809weekly)

########## SubMeter3 ##########
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
## Plot sub-meter 3 with autoplot
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, colour ="red", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

########## SubMeter1 ##########
## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

########## SubMeter2 ##########

## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))

## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")

#################################################
## time series linear regression and forecasting
#################################################

########## SubMeter3 ##########
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#Residual standard error: 6.871 on 104 degrees of freedom
#Multiple R-squared:  0.3831,	Adjusted R-squared:  0.07461 
#F-statistic: 1.242 on 52 and 104 DF,  p-value: 0.1747


########## SubMeter1 ##########

## Apply time series linear regression to the sub-meter 1 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


#Residual standard error: 4.13 on 104 degrees of freedom
#Multiple R-squared:  0.3437,	Adjusted R-squared:  0.01556 
#F-statistic: 1.047 on 52 and 104 DF,  p-value: 0.4131

########## SubMeter2 ##########
## Apply time series linear regression to the sub-meter 2 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#Residual standard error: 6.323 on 104 degrees of freedom
#Multiple R-squared:  0.3343,	Adjusted R-squared:  0.001384 
#F-statistic: 1.004 on 52 and 104 DF,  p-value: 0.4825


####################################
## Decompose a Seasonal Time Series
####################################

########## SubMeter3 ##########

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

########## SubMeter1 ##########

## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly)

########## SubMeter2 ##########

## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly$seasonal)

############################################
## HoltWinters Simple Exponential Smoothing
############################################

########## SubMeter3 ##########
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


########## SubMeter1 ##########

## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

########## SubMeter2 ##########

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM2_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))


########## residual analysis ##########

##plot Forecast Errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed
  # data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=TRUE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=TRUE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast
  # errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

########## SubMeter3 ##########

# plot acf
acf(tsSM3_HW070809forC$residuals, lag.max=20)
### Box-Ljung test
Box.test(tsSM3_HW070809forC$residuals, lag=20, type="Ljung-Box")

##Box-Ljung test

##data:  tsSM3_HW070809forC$residuals
##X-squared = 13.104, df = 20, p-value = 0.8729
# plot residual
plot.ts(tsSM3_HW070809forC$residuals)
# plot Forecast Errors
tsSM3_HW070809forC$residuals<-na.omit(tsSM3_HW070809forC$residuals)
plotForecastErrors(tsSM3_HW070809forC$residuals)

########## SubMeter1 ##########
# plot acf
acf(tsSM1_HW070809forC$residuals, lag.max=20)
### Box-Ljung test
Box.test(tsSM1_HW070809forC$residuals, lag=20, type="Ljung-Box")
## Box-Ljung test

## data:  tsSM1_HW070809forC$residuals
## X-squared = 0.10308, df = 20, p-value = 1

# plot residual
plot.ts(tsSM1_HW070809forC$residuals)
# plot Forecast Errors
tsSM1_HW070809forC$residuals<-na.omit(tsSM1_HW070809forC$residuals)
plotForecastErrors(tsSM1_HW070809forC$residuals)
########## SubMeter2 ##########
# plot acf
acf(tsSM2_HW070809forC$residuals, lag.max=20)
### Box-Ljung test
Box.test(tsSM2_HW070809forC$residuals, lag=20, type="Ljung-Box")

##Box-Ljung test

##data:  tsSM2_HW070809forC$residuals
##X-squared = 6.332, df = 20, p-value = 0.9984
# plot residual
plot.ts(tsSM2_HW070809forC$residuals)
# plot Forecast Errors
tsSM2_HW070809forC$residuals<-na.omit(tsSM2_HW070809forC$residuals)
plotForecastErrors(tsSM2_HW070809forC$residuals)


###############
####ARIMA MODEL
###############

########## SubMeter3 ##########
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

#time series of first differences#
SM3timeseriesdiff1 <- diff(tsSM3_070809weekly, differences=1)
## Plot first differences
plot.ts(SM3timeseriesdiff1)

# plot a correlogram
acf(tsSM3_070809weekly, lag.max=20) 
# plot a partial autocorrelogram
pacf(tsSM3_070809weekly, lag.max=20)

# find the appropriate ARIMA model
auto.arima(tsSM3_070809weekly)

# Forecasting Using an ARIMA Model
SM3timeseriesarima <- arima(tsSM3_070809weekly, order=c(1,0,0))
SM3timeseriesarima

## Coefficients:
##  ar1  intercept
##0.0844     4.3154
##s.e.  0.0812     0.6184
##sigma^2 estimated as 50.35:  log likelihood = -530.41,  aic = 1066.82
  
## Forecast  with 99.5% confidence levels
SM3timeseriesforecasts<-forecast(SM3timeseriesarima, h=25,
               level=c(99.5))
SM3timeseriesforecasts
## plot forecasting of SM3
plot(SM3timeseriesforecasts, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

########## SubMeter1 ##########
## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

# find the appropriate ARIMA model
auto.arima(tsSM1_070809weekly)

# Forecasting Using an ARIMA Model
SM1timeseriesarima <- arima(tsSM1_070809weekly, order=c(1,0,0))
SM1timeseriesarima

## Coefficients:
##  ar1  intercept
## -0.0136     0.4842
## s.e.   0.0796     0.3267
## sigma^2 estimated as 17.21:  log likelihood = -446.17,  aic = 898.33

## Forecast  with 99.5% confidence levels
SM1timeseriesforecasts<-forecast(SM1timeseriesarima, h=25,
                                 level=c(99.5))
SM1timeseriesforecasts
## plot forecasting of SM1
plot(SM1timeseriesforecasts, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

########## SubMeter2 ##########

## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809weekly,col ="blue", xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")

# find the appropriate ARIMA model
auto.arima(tsSM2_070809weekly)

# Forecasting Using an ARIMA Model
SM2timeseriesarima <- arima(tsSM2_070809weekly, order=c(0,0,0))
SM2timeseriesarima

# Coefficients:
#   intercept
# 1.0255
# s.e.     0.5034
# sigma^2 estimated as 39.78:  log likelihood = -511.92,  aic = 1027.85

## Forecast  with 99.5% confidence levels
SM2timeseriesforecasts<-forecast(SM2timeseriesarima, h=25,
                                 level=c(99.5))
SM2timeseriesforecasts
## plot forecasting of SM2
plot(SM2timeseriesforecasts, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


########## residual analysis ##########

########## SubMeter3 ##########
acf(SM3timeseriesforecasts$residuals, lag.max=20)
Box.test(SM3timeseriesforecasts$residuals, lag=20, type="Ljung-Box")

#####Box-Ljung test

#####data:  SM3timeseriesforecasts$residuals
#####X-squared = 16.472, df = 20, p-value = 0.6869

# make time plot of forecast errors
plot.ts(SM3timeseriesforecasts$residuals) 
# make a histogram
plotForecastErrors(SM3timeseriesforecasts$residuals) 

########## SubMeter1 ##########
acf(SM1timeseriesforecasts$residuals, lag.max=20)
Box.test(SM1timeseriesforecasts$residuals, lag=20, type="Ljung-Box")

######Box-Ljung test

######data:  SM1timeseriesforecasts$residuals
######X-squared = 0.65163, df = 20, p-value = 1

# make time plot of forecast errors
plot.ts(SM1timeseriesforecasts$residuals) 
# make a histogram
plotForecastErrors(SM1timeseriesforecasts$residuals) 
 
########## SubMeter2 ##########
acf(SM2timeseriesforecasts$residuals, lag.max=20)
Box.test(SM2timeseriesforecasts$residuals, lag=20, type="Ljung-Box")

######Box-Ljung test

######data:  SM2timeseriesforecasts$residuals
######X-squared = 0.69477, df = 20, p-value = 1

# make time plot of forecast errors
plot.ts(SM2timeseriesforecasts$residuals) 
# make a histogram
plotForecastErrors(SM2timeseriesforecasts$residuals) 






