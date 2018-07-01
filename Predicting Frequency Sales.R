
#######
# Predict Frequency of House Sales
######



# Import libraries

library(tidyverse)
library(lubridate)
library(ggthemes)
library(forecast)
library(zoo)
library(cowplot)


# Set working directory
setwd("C:/Users/simse/Data Science/Predict Frequency of House Purchases")

# Import data
url <- url("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv")
data <- read.csv("ppd_data.csv", col.names = c("uprn", 
                                               "purchase_price", 
                                               "date",
                                               "postcode",
                                               "prop_type", 
                                               "new_build", 
                                               "free_lease",
                                               "add1", "add2", "add3",
                                               "add4", "add5", 
                                               "add6", "county",
                                               "record_type", 
                                               "link"))


head(data)  # Check top of df
tail(data)  # Check bottom of df
names(data) # Check variable names

data <- unique(data) # Remove duplicates

# Remove unwanted variables
data <- data[ , c("purchase_price", 
                  "date",
                  "postcode",
                  "prop_type",
                  "new_build", 
                  "free_lease")]

data$date <- ymd(data$date)         # Coerce the date variable to a date object
summary(data)                       # Summary stats
head(arrange(data, purchase_price)) # Minimum purchase price of £140?


# Looks like there are a few of these. Probably garage sales? Let's keep them in.




# Create a frequencies dataframe by date to analyse
df_freq <- as.data.frame(data %>% group_by(date) %>%  tally())
head(df_freq)
colnames(df_freq)[2] <- "freq" # Change the frequency var name


# This is not a complete time series as the days where there 
# were no purchases (like weekends perhaps) do not appear 
# in the df. We want to show every date in the ts, and if 
# there weren't any sales, have a "0" in our freq variable.


# Create df with all dates in order to fill in the missing ones
dates <- data.frame(
  date = seq(as.Date("1995-06-01"), as.Date("2018-06-01"), by = "day")
  ) 

# Merge the dates with our df to include all dates (includingones with no sales)
df_freq <- merge(df_freq, dates, by = "date", all = TRUE) 

# Any dates with no sales has a "0" in the frequency variable
df_freq[is.na(df_freq$freq), "freq"] <- 0 
rm(dates)


df_freq$year <- format(df_freq$date, "%Y")
df_freq$day <-  format(df_freq$date, "%d")

# Now we have a complete time series, we can start plotting 
# the data to get some insights.



# Line graph all dates
ggplot(data = df_freq, aes(date, freq)) +
  geom_line() +
  labs(x = NULL,
       y = "Frequency of Residential Sales") +
  theme_light() 


# Can't really see any seasonal trends in this format.
# Althoguh we can see a general decrease in sales around
# the recession... Let's separate the 
# graphs by year for two years and see if this helps us spot anything.


# Line graph for 2016 and 2017 separate
cowplot::plot_grid(
  ggplot(data = df_freq[df_freq$date >= "2016-01-01" & df_freq$date < "2017-01-01", ], aes(date, freq)) +
    geom_line() +
    labs(x = NULL,
         y = "Frequency of Residential Sales") +
    scale_x_date(expand = c(0,0)) +
    theme_light(),
  ggplot(data = df_freq[df_freq$date >= "2017-01-01" & df_freq$date <= "2018-01-01", ], aes(date, freq)) +
    geom_line() +
    labs(x = NULL,
         y = "Frequency of Residential Sales") +
    scale_x_date(expand = c(0,0)) +
    theme_light(), 
  nrow = 2, ncol = 1
)

# Interesting oscillations going on regularly... We can assume this is 
# weekday-weekend changes. Things seem to ramp up towards the end
# of each week if so.


# Let's make a heatmap for the whole series. This might help us  visualise 
# what's going on better.


library(plyr)

datetxt <- as.Date(df_freq$date)
dates <- data.frame(date = datetxt,
                    year = as.numeric(format(datetxt, format = "%Y")),
                    month = as.numeric(format(datetxt, format = "%m")),
                    monthf = format(datetxt, format = "%b"),
                    weekday = as.numeric(format(datetxt, format = "%d")),
                    weekdayf = format(datetxt, format = "%a"),
                    week = as.numeric(format(datetxt, format = "%V")))
df_freq <- as.data.frame(c(df_freq, dates))
df_freq$date <- as.Date(df_freq$date)
# Order dates for calendar
df_freq$monthf<-factor(df_freq$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
df_freq$weekday = as.POSIXlt(df_freq$date)$wday
df_freq$weekdayf<-factor(df_freq$weekday,levels=as.character(rev(1:7)),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
df_freq[is.na(df_freq)] <- "Sun"

# Transform df - create yearmonth column and remove uneccesary columns
df_freq$yearmonth <- as.yearmon(df_freq$date)
df_freq$yearmonthf <- factor(df_freq$yearmonth)
df_freq <- ddply(df_freq,.(yearmonthf), transform, monthweek=1+week-min(week))  
df_freq$date <- as.Date(df_freq$date)
df_freq$yearmonth <- as.factor(df_freq$yearmonth)
df_freq <- df_freq[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "freq")]
df_freq <- df_freq[-92, ] 
rownames(df_freq) <- 1:nrow(df_freq)


# Calendar heatmap for 2017 and 2016
png("TEST.png",
    units = "in", width = 12, height = 7, res = 600)
ggplot(data = tail(df_freq, 500), aes(monthweek, weekdayf, fill = freq)) + 
  geom_tile(colour = "white") +
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x = "Week of Month",
       y = "",
       fill = "Frequency")
dev.off()
  
