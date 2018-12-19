# Title: Filtering and ploting-TimeSeries in R

# Last update: 2018.07.10

# File:  Filtering-ploting-TimeSeries.R
###############
# Project Notes
###############
#--- Filtering ---#
# 1. Create a subset that shows the total annual consumption (kWh) for each submeter over the Jan-07 thru Dec-09 period. 
# 2. Create a subset that shows the average daily consumption (kWh) for each submeter by weekday for the winter seasons (Dec-Feb) over Jan-07 thru Oct-10 period. This subset should only have 7 values (one for each weekday - it reflects the typical usage per weekday during the winter season). 
# 3. Create a subset that shows the average hourly kWh used for each hour of the day during January 2010. This subset should only have 24 values (it reflects the typical usage per hour of day during Jan-10). 
# 4. Create a subset that shows consumption at 12:00pm (noon) for the 1st day of each month for 2009. 

#--- Plot ---#
# Note: Use Plotly (or ggplot2). To create the plots, you may need to convert the subsets to long format.
# 1. Create a multi-variate side-by-side column plot for the annual ds above (#1) showing all submeters. 
# 2. Ditto for (#2,3,4) except create a line chart.

################################
## Install and load packages
################################

install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
require(lubridate) # work with dates
require(dplyr)     # data manipulation (filter, summarize, mutate)
require(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)
###############
# Load dataset 
###############

hhpwr <- read.csv("C:/Users/admin/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(hhpwr)
str(hhpwr)

##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()

hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = TRUE)
str(hhpwrDT)

hhpwr<- hhpwrDT%>%separate(DateTime, c("Date", "Time"), sep = " ")
str(hhpwr)
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

# Note: Understand the difference between as.numeric(as.character()) and as.numeric()

hhpwrDT$Global_active_power <- as.numeric(as.character(hhpwr$Global_active_power))
hhpwrDT$Global_reactive_power <- as.numeric(as.character(hhpwr$Global_reactive_power))
hhpwrDT$Voltage <- as.numeric(as.character(hhpwr$Voltage))
hhpwrDT$Global_intensity <- as.numeric(as.character(hhpwr$Global_intensity))
hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))
str(hhpwrDT)


## ------ Evaluate NA values ----------##

# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Count the number of values = NA
sum(is.na(hhpwrDT$Sub_metering_1)) # Review any metadata with dataset


#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): filter for time interval (Yr/Mo/Day) using lubridate w/in dplyr mutate() to create col.
# 2. dplyr::filter(): select cols to filter by; full ds + col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select desired time interval to subset
# 4. dplyr::summarize(): select which vars and any calculations for the vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 
#1 Create a subset that shows the total annual consumption (kWh) for each submeter over the Jan-07 thru Dec-09 period. 
Yr.sum.wide<- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009)%>% 
  group_by(Year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) 
  str(Yr.sum.wide)
  names(Yr.sum.wide)
  class(Yr.sum.wide)  
  head(Yr.sum.wide)  

  # A tibble: 3 x 5
  #Year   SM1   SM2   SM3 DateTime           
  #   <dbl> <dbl> <dbl> <dbl> <dttm>             
  #1  2007  643.  854. 3023. 2007-01-01 00:00:00
  #2  2008  584.  662. 3179. 2008-01-01 00:00:00
  #3  2009  593.  592. 3557. 2009-01-01 00:00:00
 
#convert to long format
Yr.sum.long<- gather(Yr.sum.wide, "SM", "Power_consumption", 2:4) 
str(Yr.sum.long)  
  
 ## Plot sub-meter 1, 2 and 3 with title, legend and labels - Year frequency
  
YrsumBar<- ggplot(Yr.sum.long, aes(Year, Power_consumption,fill=SM)) +
  geom_bar(position = "dodge",stat="identity", na.rm = TRUE) + 
  ggtitle("Annual consumption (kWh) 2007~2009") +
  xlab("Year") + ylab("Power (kWh)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))
  
YrsumBar
  
  
#2 Create a subset that shows the average daily consumption (kWh) for each submeter by weekday for the winter seasons (Dec-Feb) over Jan-07 thru Oct-10 period.  

mondata<- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  mutate(Month = month(DateTime)) # Add col Month  using dplyr and lubridate functions

wday.mean<- mondata %>% 
  mutate(weekday= wday(DateTime)) %>% # Add col Weekday using dplyr and lubridate functions
  subset(DateTime >= as.POSIXct('2009-01-01 00:00',
                                tz = "America/New_York") &
           DateTime<= as.POSIXct('2010-10-31 23:59',
                                  tz = "America/New_York"))%>% 
  filter(Month %in% c(12,1,2)) %>% 
  group_by(weekday) %>%
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
          SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
          SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3),
          DateTime = first(DateTime)) 
head(wday.mean)  

#weekday SM1   SM2   SM3 DateTime           
#<dbl>  <dbl> <dbl> <dbl> <dttm>             
#1      0.002 0.002 0.007 2009-01-04 00:00:00
#2      0.001 0.001 0.008 2009-01-05 00:00:00
#3      0.001 0     0.01  2009-01-06 00:00:00
#4      0.001 0.003 0.008 2009-01-07 00:00:00
#5      0.001 0.001 0.009 2009-01-01 00:00:00
#6      0.001 0.001 0.009 2009-01-02 00:00:00


#convert to long format

wday.mean.long<- gather(wday.mean, "SM", "Power_consumption", 2:4) 

str(wday.mean.long)  

# create line chart.

wdaymeanline<- ggplot(wday.mean.long, aes(weekday, Power_consumption,group=SM,color=SM)) +
  geom_line(position = 'jitter',stat="identity", na.rm = TRUE) + 
  geom_point()+
  ggtitle("each weekday consumption (kWh) during the winter season") +
  xlab("weekday") + ylab("Power (kWh)") +
  scale_x_date(breaks=date_breaks("1 day"))+ 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))

wdaymeanline


#3 Create a subset that shows the average hourly kWh used for each hour of the day during January 2010. 
Hourdata<-mondata %>% 
  mutate(Hour= hour(DateTime)) # Add col hour using dplyr and lubridate functions

hr.mean<- Hourdata %>% 
  filter(Year == 2010 & Month == 1) %>% 
  group_by(Hour) %>%
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) 
head(hr.mean)  

# A tibble: 6 x 5
# Hour   SM1   SM2   SM3 DateTime           
# <int> <dbl> <dbl> <dbl> <dttm>             
#  0     0.001  0 0.007 2010-01-01 00:00:00
#  1     0      0 0.005 2010-01-01 01:00:00
#  2     0      0 0.003 2010-01-01 02:00:00
#  3     0      0 0.002 2010-01-01 03:00:00
#  4     0      0 0.001 2010-01-01 04:00:00
#  5     0      0 0.001 2010-01-01 05:00:00

#convert to long format

hr.mean.long<- gather(hr.mean, "SM", "Power_consumption", 2:4) 

str(hr.mean.long)  

# create line chart.
hrmeanline<- ggplot(hr.mean.long, aes(Hour, Power_consumption,group=SM,color=SM)) +
  geom_line(position = 'jitter',stat="identity", na.rm = TRUE) + 
  geom_point()+
  ggtitle("average hourly kWh used for each hour of the day during January 2010") +
  xlab("Hour") + ylab("Power (kWh)") +
  scale_x_date(breaks=date_breaks("1 hour"))+ 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))

hrmeanline


#4 Create a subset that shows consumption at 12:00pm (noon) for the 1st day of each month for 2009.

mon.wide<- Hourdata %>%
  mutate(Day= day(DateTime)) %>% # Add col Day using dplyr and lubridate functions
  mutate(Minute= minute(DateTime)) %>% # Add col Minute using dplyr and lubridate functions
  filter(Year == 2009 & Day == 1 & Hour== 12 & Minute==0) %>% 
  group_by(Month)%>%
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3)) 
head(mon.wide)  

#Month   SM1   SM2   SM3
#<dbl> <dbl> <dbl> <dbl>
#   1    0      0     0
#   2    0.038  0     0
#   3    0.001  0.002 0.018
#   4    0      0     0
#   5    0      0.039 0.001
#   6    0.039  0     0.018

#convert to long format

mon.long<- gather(mon.wide, "SM", "Power_consumption", 2:4) 

str(mon.long)  

# create line chart.
monline<- ggplot(mon.long, aes(Month, Power_consumption,group=SM,color=SM)) +
  geom_line(position = 'jitter',stat="identity", na.rm = TRUE) + 
  geom_point()+
  ggtitle("average hourly kWh used for each hour of the day during January 2010") +
  xlab("Hour") + ylab("Power (kWh)") +
  scale_x_date(breaks=date_breaks("1 month"))+ 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))

monline


