
# Title: forecasting tasks in R

# Last update: 2018.07.18

# File:  forecasting tasks.R

###############
# Project Notes
###############

# Summarize project: TSLM Forecast ---#
#1. Create a subset that shows the total kWh per year for submeter 3 for the years 2007-09. 
#Forecast for 2010 and 2011. 
#2. Create a subset that shows the total kWh per month for submeter 3 for the months Jan-07 through Oct-10. 
#Forecast for Nov-10 through Dec-11. 

#--- Decompose ---#
# 1. Using the ts for SM3 that shows kWh by month over the Jan-07 thru Oct-10 time period, 
#decompose this ts into seasonal, trend, and random components. Also provide a plot for these components.
# 2. Create a subset that shows kWh by hour over each day during Feb-10. 
#Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.
#--- HW ---# 
#Need non-seasonal ts for HW. 
#Therefore, create a ts object for SM3 for each of the following 4 seasons: 
#Win-09/10, Spr-10, Smr-10, Fall-10 (thru 11/25). 
#To do this, create a subset that shows kWh by day over each season, then forecast the next 30 days. 

################
# Load packages
################
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("plotly")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
library(tidyr)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

###############
# Load dataset 
###############

hhpwr <- read.csv("C:/Users/amdin/Desktop/task2/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(hhpwr)
str(hhpwr)

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)


# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")


class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "America/New_York"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%d/%m/%Y")

str(hhpwrDT)

##------- Change data types---------##

hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))
str(hhpwrDT)


# create a new dataset with year,month,hour col
hhpwrDTnew<- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime)) %>% # Add col Month  using dplyr and lubridate functions
  mutate(Day= day(DateTime)) %>%
  mutate(Hour= hour(DateTime)) 

str(hhpwrDTnew)

#--- TSLM Forecast ---#

#1 Create a subset that shows the total annual consumption (kWh) for each submeter over the Jan-07 thru Dec-09 period. 
hhpwrDTyr<- hhpwrDTnew %>% 
  filter(Year==2007 | Year==2008 | Year==2009)%>% 
  group_by(Year) %>%  # Group data by Year
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) 

str(hhpwrDTyr)

names(hhpwrDTyr)
class(hhpwrDTyr)  
head(hhpwrDTyr)  

# A tibble: 3 x 3
# Year  SM3 DateTime           
#<dbl> <dbl> <dttm>             
#2007 3023. 2007-01-01 00:00:00
#2008 3179. 2008-01-01 00:00:00
#2009 3557. 2009-01-01 00:00:00

hhpwrDTyrSM3_070809<- ts(hhpwrDTyr$SM3, frequency=1, start=c(2007))
plot.ts(hhpwrDTyrSM3_070809)
hhpwrDTyrSM3_070809

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM3 <- tslm(hhpwrDTyrSM3_070809 ~ trend) 
summary(fitSM3)

#Residual standard error: 90.44 on 1 degrees of freedom
#Multiple R-squared:  0.9457,	Adjusted R-squared:  0.8915 
#F-statistic: 17.43 on 1 and 1 DF,  p-value: 0.1497
#Forecast for 2010 and 2011.

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=2, level=c(80,90))
forecastfitSM3c

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylab= "kWh", xlab="Time")

##Point      Forecast   Lo 80    Hi 80    Lo 90    Hi 90
##2010       3786.881 3278.680 4295.082 2744.325 4829.437
##2011       4053.869 3381.582 4726.156 2674.697 5433.041


##2. Create a subset that shows the total kWh per month for submeter 3 for the months Jan-07 through Oct-10. 
  
  hhpwrDTMon<-subset(hhpwrDTnew,DateTime >= as.POSIXct('2007-01-01 00:00',
                                tz = "America/New_York") &
           DateTime<= as.POSIXct('2010-10-31 23:59',
                                 tz = "America/New_York"))%>% 
           group_by(Year,Month) %>%  # Group data by Month
           summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) 

head(hhpwrDTMon)

# A tibble: 6 x 4
# Groups:   Year [1]
#Year   Month   SM3   DateTime           
#<dbl>   <dbl> <dbl> <dttm>             
# 2007     1   330. 2007-01-01 00:00:00
# 2007     2   270. 2007-02-01 00:00:00
# 2007     3   290. 2007-03-01 00:00:00
# 2007     4   190. 2007-04-01 00:00:00
# 2007     5   229. 2007-05-01 00:00:00
# 2007     6   189. 2007-06-01 00:00:00


#Forecast for Nov-10 through Dec-11. 

hhpwrDTMonSM3<- ts(hhpwrDTMon$SM3, frequency=12, start=c(2007,1))
plot.ts(hhpwrDTMonSM3)
hhpwrDTMonSM3

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM3 <- tslm(hhpwrDTMonSM3 ~ trend+season) 
summary(fitSM3)

##Multiple R-squared:  0.7666,	Adjusted R-squared:  0.6817 
##F-statistic:  9.03 on 12 and 33 DF,  p-value: 2.522e-07

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=14, level=c(80,90))
forecastfitSM3c

## Point Forecast    Lo 80    Hi 80    Lo 90    Hi 90
## Nov 2010       351.1559 292.2140 410.0978 274.8784 427.4335
## Dec 2010       397.1039 338.1620 456.0458 320.8264 473.3815
## Jan 2011       399.1654 341.1026 457.2283 324.0255 474.3054
## Feb 2011       365.8654 307.8026 423.9283 290.7255 441.0054
## Mar 2011       363.0754 305.0126 421.1383 287.9355 438.2154
## Apr 2011       339.6254 281.5626 397.6883 264.4855 414.7654
## May 2011       356.2827 298.2198 414.3455 281.1428 431.4226
## Jun 2011       318.8117 260.7488 376.8745 243.6718 393.9516
## Jul 2011       248.0702 190.0073 306.1330 172.9303 323.2101
## Aug 2011       221.6874 163.6246 279.7503 146.5475 296.8274
## Sep 2011       323.6137 265.5508 381.6765 248.4738 398.7536
## Oct 2011       351.1327 293.0698 409.1955 275.9928 426.2726
## Nov 2011       374.0949 313.2450 434.9448 295.3482 452.8416
## Dec 2011       420.0429 359.1930 480.8928 341.2962 498.7896


## Plot sub-meter 3 forecast, and add labels
plot(forecastfitSM3c, ylab= "kWh", xlab="Time")

###############
# Decompose
###############

#1. Using the ts for SM3 that shows kWh by month over the Jan-07 thru Oct-10 time period, decompose this ts into seasonal, trend, and random components. Also provide a plot for these components.

## Decompose Sub-meter 3 into trend, seasonal and remainder
componentshhpwrDTMonSM3 <- decompose(hhpwrDTMonSM3)
## Plot decomposed sub-meter 3 
plot(componentshhpwrDTMonSM3)
## Check summary statistics for decomposed sub-meter 3 
summary(componentshhpwrDTMonSM3)

#2. Create a subset that shows kWh by hour over each day during Feb-10. 

hhpwrDThr<- hhpwrDTnew %>% 
  filter(Year==2010 & Month==2)%>% 
  group_by(Day,Hour) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) 

str(hhpwrDThr)
head(hhpwrDThr)  
# A tibble: 6 x 4
# Groups:   Day [1]
#Day   Hour   SM3    DateTime           
#<int> <int> <dbl> <dttm>             
# 1     0   0.041 2010-02-01 00:00:00
# 1     1   0.041 2010-02-01 01:00:00
# 1     2   0.306 2010-02-01 02:00:00
# 1     3   0.431 2010-02-01 03:00:00
# 1     4   0.041 2010-02-01 04:00:00
# 1     5   0.041 2010-02-01 05:00:00

#Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.

hhpwrDThrSM3<- ts(hhpwrDThr$SM3,frequency = 24,start=c(1,0))
hhpwrDThrSM3
plot.ts(hhpwrDThrSM3)


## Decompose Sub-meter 3 into trend, seasonal and remainder
componentshhpwrDThrSM3 <- decompose(hhpwrDThrSM3)
## Plot decomposed sub-meter 3 
plot(componentshhpwrDThrSM3)
## Check summary statistics for decomposed sub-meter 3 
summary(componentshhpwrDThrSM3$seasonal)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-0.46787 -0.19153  0.03918  0.00000  0.18684  0.41216 
summary(componentshhpwrDThrSM3$trend)
##   Min.  1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
## 0.1230  0.4985  0.6190   0.6240   0.7081  1.0827      24 
summary(componentshhpwrDThrSM3$random)
## Min.     1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
##-0.972925 -0.250369  0.015107 -0.001649  0.286488  0.926115        2

###############
# Holt Winters
###############

#create a ts object for SM3 for each of the following 4 seasons: Win-09/10, Spr-10, Smr-10, Fall-10 

#create a ts object for SM3 for Win-09/10
hhpwrDTDaywin<- hhpwrDTnew %>% 
  filter((Year==2009&Month==12)|(Year==2010&(Month==1| Month==2)))%>% 
  group_by(Year,Month,Day) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))

head(hhpwrDTDaywin)

# A tibble: 6 x 5
# Groups:   Year, Month [1]
#Year   Month   Day   SM3 DateTime           
#<dbl> <dbl> <int> <dbl> <dttm>             
# 2009    12     1  10.8  2009-12-01 00:00:00
# 2009    12     2  12.9  2009-12-02 00:00:00
# 2009    12     3  9.91  2009-12-03 00:00:00
# 2009    12     4  9.13  2009-12-04 00:00:00
# 2009    12     5  13.3  2009-12-05 00:00:00
# 2009    12     6  12.4  2009-12-06 00:00:00

#Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.

hhpwrDTDaywinSM3<- ts(hhpwrDTDaywin$SM3,frequency =30,start=c(12,1))
hhpwrDTDaywinSM3
plot.ts(hhpwrDTDaywinSM3)

## Decompose Sub-meter 3 into trend, seasonal and remainder
componenthhpwrDTDaywinSM3<- decompose(hhpwrDTDaywinSM3)
## Plot decomposed sub-meter 3 
plot(componenthhpwrDTDaywinSM3)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
hhpwrDTDaywinSM3Adjusted <- hhpwrDTDaywinSM3-componenthhpwrDTDaywinSM3$seasonal
autoplot(hhpwrDTDaywinSM3Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(hhpwrDTDaywinSM3Adjusted))

## Holt Winters Exponential Smoothing & Plot
hhpwrDTDaywin <- HoltWinters(hhpwrDTDaywinSM3Adjusted, beta=FALSE, gamma=FALSE)
plot(hhpwrDTDaywin, ylim = c(0, 25))

## Forecast HoltWinters with diminished confidence levels
hhpwrDTDaywinforC <- forecast(hhpwrDTDaywin, h=30, level=c(10,25))
## Plot only the forecasted area
plot(hhpwrDTDaywinforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(16))

Box.test(hhpwrDTDaywinforC$residuals, lag=20, type="Ljung-Box")

#Box-Ljung test
#data:  hhpwrDTDaywinforC$residuals
#X-squared = 20.808, df = 20, p-value = 0.4085
plot.ts(hhpwrDTDaywinforC$residuals)

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
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast
  # errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(hhpwrDTDaywinforC$residuals)

######create a ts object for SM3 for Spr-10 ######

hhpwrDTDaySpr<- hhpwrDTnew %>% 
  filter((Year==2010)&(Month==3| Month==4| Month==5))%>% 
  group_by(Month,Day) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))

head(hhpwrDTDaySpr)

# A tibble: 6 x 4
# Groups:   Month [1]
##Month   Day   SM3 DateTime           
##<dbl> <int> <dbl> <dttm>             
## 3      1   7.12 2010-03-01 00:00:00
## 3      2   3.60 2010-03-02 00:00:00
## 3      3   7.22 2010-03-03 00:00:00
## 3      4   2.93 2010-03-04 00:00:00
## 3      5   5.92 2010-03-05 00:00:00
## 3      6   3.97 2010-03-06 00:00:00

#Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.

hhpwrDTDaySprSM3<- ts(hhpwrDTDaySpr$SM3,frequency =30,start=c(3,1))
hhpwrDTDaySprSM3
plot.ts(hhpwrDTDaySprSM3)

## Decompose Sub-meter 3 into trend, seasonal and remainder
componenthhpwrDTDaySprSM3<- decompose(hhpwrDTDaySprSM3)
## Plot decomposed sub-meter 3 
plot(componenthhpwrDTDaySprSM3)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
hhpwrDTDaySprSM3Adjusted <- hhpwrDTDaySprSM3-componenthhpwrDTDaySprSM3$seasonal
autoplot(hhpwrDTDaySprSM3Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(hhpwrDTDaySprSM3Adjusted))

## Holt Winters Exponential Smoothing & Plot
hhpwrDTDaySpr <- HoltWinters(hhpwrDTDaySprSM3Adjusted, beta=FALSE, gamma=FALSE)
plot(hhpwrDTDaySpr, ylim = c(0, 25))

## Forecast HoltWinters with diminished confidence levels
hhpwrDTDaySprforC <- forecast(hhpwrDTDaySpr, h=30, level=c(10,25))
## Plot only the forecasted area
plot(hhpwrDTDaySprforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(6))

Box.test(hhpwrDTDaySprforC$residuals, lag=20, type="Ljung-Box")

#Box-Ljung test
#data:  hhpwrDTDaywinforC$residuals
#X-squared = 45.642, df = 20, p-value = 0.0009023
plot.ts(hhpwrDTDaySprforC$residuals)
plotForecastErrors(hhpwrDTDaySprforC$residuals)

######create a ts object for SM3 for Smr-10######

hhpwrDTDaySmr<- hhpwrDTnew %>% 
  filter((Year==2010)&(Month==6| Month==7| Month==8))%>% 
  group_by(Month,Day) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))

head(hhpwrDTDaySmr)


# A tibble: 6 x 4
# Groups:   Month [1]
# Month   Day   SM3 DateTime           
# <dbl> <int> <dbl> <dttm>             
# 6        1 11.9  2010-06-01 00:00:00
# 6        2  8.45 2010-06-02 00:00:00
# 6        3 12.3  2010-06-03 00:00:00
# 6        4 12.5  2010-06-04 00:00:00
# 6        5 12.2  2010-06-05 00:00:00
# 6        6 11.4  2010-06-06 00:00:00

#Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.

hhpwrDTDaySmrSM3<- ts(hhpwrDTDaySmr$SM3,frequency =30,start=c(6,1))
hhpwrDTDaySmrSM3
plot.ts(hhpwrDTDaySmrSM3)

## Decompose Sub-meter 3 into trend, seasonal and remainder
componenthhpwrDTDaySmrSM3<- decompose(hhpwrDTDaySmrSM3)
## Plot decomposed sub-meter 3 
plot(componenthhpwrDTDaySmrSM3)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
hhpwrDTDaySmrSM3Adjusted <- hhpwrDTDaySmrSM3-componenthhpwrDTDaySmrSM3$seasonal
autoplot(hhpwrDTDaySmrSM3Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(hhpwrDTDaySmrSM3Adjusted))

## Holt Winters Exponential Smoothing & Plot
hhpwrDTDaySmr <- HoltWinters(hhpwrDTDaySmrSM3Adjusted, beta=FALSE, gamma=FALSE)
plot(hhpwrDTDaySmr, ylim = c(0, 25))

## Forecast HoltWinters with diminished confidence levels
hhpwrDTDaySmrforC <- forecast(hhpwrDTDaySmr, h=30, level=c(10,25))
## Plot only the forecasted area
plot(hhpwrDTDaySmrforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(9))

Box.test(hhpwrDTDaySmrforC$residuals, lag=20, type="Ljung-Box")
#Box-Ljung test
#data:  hhpwrDTDaywinforC$residuals
#X-squared = 19.828, df = 20, p-value = 0.4687
plot.ts(hhpwrDTDaySmrforC$residuals)
plotForecastErrors(hhpwrDTDaySmrforC$residuals)

######create a ts object for SM3 for  Fall-10 ######

hhpwrDTDayfall<- hhpwrDTnew %>% 
  filter((Year==2010)&(Month==9| Month==10| Month==11))%>% 
  group_by(Month,Day) %>%  # Group data by Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))

head(hhpwrDTDayfall)
# A tibble: 6 x 4
# Groups:   Month [1]
#Month   Day   SM3 DateTime           
#<dbl> <int> <dbl> <dttm>             
# 9     1     14.0  2010-09-01 00:00:00
# 9     2     11.8  2010-09-02 00:00:00
# 9     3     9.75 2010-09-03 00:00:00
# 9     4    13.2  2010-09-04 00:00:00
# 9     5     7.47 2010-09-05 00:00:00
# 9     6    10.2  2010-09-06 00:00:00


# Create a ts object for SM3 and decompose this ts into seasonal, trend, and random components.

hhpwrDTDayfallSM3<- ts(hhpwrDTDayfall$SM3,frequency =30,start=c(9,1))
hhpwrDTDayfallSM3
plot.ts(hhpwrDTDayfallSM3)

## Decompose Sub-meter 3 into trend, seasonal and remainder
componenthhpwrDTDayfallSM3<- decompose(hhpwrDTDayfallSM3)
## Plot decomposed sub-meter 3 
plot(componenthhpwrDTDayfallSM3)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
hhpwrDTDayfallSM3Adjusted <- hhpwrDTDayfallSM3-componenthhpwrDTDayfallSM3$seasonal
autoplot(hhpwrDTDayfallSM3Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(hhpwrDTDayfallSM3Adjusted))

## Holt Winters Exponential Smoothing & Plot
hhpwrDTDayfall <- HoltWinters(hhpwrDTDayfallSM3Adjusted, beta=FALSE, gamma=FALSE)
plot(hhpwrDTDayfall, ylim = c(0, 25))

## Forecast HoltWinters with diminished confidence levels
hhpwrDTDayfallforC <- forecast(hhpwrDTDayfall, h=30, level=c(10,25))
## Plot only the forecasted area
plot(hhpwrDTDayfallforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(12))

Box.test(hhpwrDTDayfallforC$residuals, lag=20, type="Ljung-Box")

#Box-Ljung test
#data:  hhpwrDTDaywinforC$residuals
#X-squared = 13.943, df = 20, p-value = 0.8334
plot.ts(hhpwrDTDayfallforC$residuals)
plotForecastErrors(hhpwrDTDayfallforC$residuals) 



