# apply_seasons

# getSeason function was used to apply dates for seasons as appropriate. Season 
season2 is the correct column
```r
trend <- "~/Desktop/Dilanka_ayya/seasons"
setwd(trend)

require(gdata)
require(dplyr)
require(zoo)

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

rm(list = ls())

# functions

# Do not change 2012. 2012 is a good year to which to convert all of the dates; 
#since it is a leap year, any February 29ths in your data set will be handled smoothly.
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-10",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#************

trend <- read_excel("trend.xlsx", sheet = "1") 
trend[['Date']] <- as.Date.POSIXct(trend[['Date']], format='%m/%d/%y')

trend$year <- year(trend$Date)
library(xlsx)
write.xlsx(trend, file = "trend1.xlsx")

library(zoo)
yq <- as.yearqtr(as.yearmon(trend$Date, "%m/%d/%Y") + 1/12)
trend$Season <- factor(format(yq, "%q"), levels = 1:4, 
                       labels = c("winter", "spring", "summer", "fall"))


trend$Season2<-getSeason(trend$Date)
```
