# sofya-155.github.io
SOFYA ASSEFA

GOOGLE DATA ANALYSYS CAPSTONE PROJECT
CASE STUDY: CYCLISTIC-BIKE-SHARE

INTRODUCTION


In this case study my team and I will be working as a junior data analyst for a fictional company, cyclistic. 
In order to answer the business questions, I will be following the 6 steps of data analysys which is
ASK, PREPARE, PROCESS, ANALYZE, SHARE AND ACT.


SCENARIO
I am a junior data analyst working on the marketing analyst team at Cyclistic, a bike-share
company in Chicago. The director of marketing believes the company’s future success
depends on maximizing the number of annual memberships. Therefore, my team wants to
understand how casual riders and annual members use Cyclistic bikes differently. From these
insights, my team will design a new marketing strategy to convert casual riders into annual
members. But first, Cyclistic executives must approve your recommendations, so they must be
backed up with compelling data insights and professional data visualizations.


ABOUT THE COMPANY

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown
to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations
across Chicago. The bikes can be unlocked from one station and returned to any other station
in the system anytime.
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to
broad consumer segments. One approach that helped make these things possible was the
flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships.
Customers who purchase single-ride or full-day passes are referred to as casual riders.
Customers who purchase annual memberships are Cyclistic members.
Cyclistic’s finance analysts have concluded that annual members are much more profitable
than casual riders. Although the pricing exibility helps Cyclistic attract more customers,
Moreno believes that maximizing the number of annual members will be key to future growth.
Rather than creating a marketing campaign that targets all-new customers, Moreno believes
there is a solid opportunity to convert casual riders into members. She notes that casual riders
are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.
Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into
annual members. In order to do that, however, the team needs to better understand of how
annual members and casual riders differ, why casual riders would buy a membership, and how
digital media could affect their marketing tactics. Moreno and her team are interested in
analyzing the Cyclistic historical bike trip data to identify trends.

ASK 

Three questions will guide the future marketing program:
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?
Moreno has assigned me the first question to answer: How do annual members and casual
riders use Cyclistic bikes differently?


PREPARE

I will use Cyclistic’s historical trip data which can be downloaded 
https://divvy-tripdata.s3.amazonaws.com/index.html The data is original which is collected by 
the company, so it is reliable, current and cited.
The data has been made available by Motivate International Inc. under this license
https://www.divvybikes.com/data-license-agreement 
This is public data that I can use to explore how different customer types are using Cyclistic bikes.
But note that data-privacy issues prohibit me from using rider's personally identifiable information. 
This means that I won’t be able to connect pass purchases to credit card numbers to determine if 
casual riders live in the cyclistic service area or if they have purchased multiple single passes.
I decided to analyse the 2022 trip data, so I downloaded the 12 month csv files for the year 2022.


PROCESS

Because it is a large data, I decided to use R.


CODE

install.packages("tidyverse")
library(tidyverse)  #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)


# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here
trip1 <- read_csv("202201-divvy-tripdata.csv")
trip2 <- read_csv("202202-divvy-tripdata.csv")
trip3 <- read_csv("202203-divvy-tripdata.csv")
trip4 <- read_csv("202204-divvy-tripdata.csv")
trip5 <- read_csv("202205-divvy-tripdata.csv")
trip6 <- read_csv("202206-divvy-tripdata.csv")
trip7 <- read_csv("202207-divvy-tripdata.csv")
trip8 <- read_csv("202208-divvy-tripdata.csv")
trip9 <- read_csv("202209-divvy-tripdata.csv")
trip10 <- read_csv("202210-divvy-tripdata.csv")
trip11 <- read_csv("202211-divvy-tripdata.csv")
trip12 <- read_csv("202212-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(trip1)
colnames(trip2)
colnames(trip3)
colnames(trip4)
colnames(trip5)
colnames(trip6)
colnames(trip7)
colnames(trip8)
colnames(trip9)
colnames(trip10)
colnames(trip11)
colnames(trip12)

# Inspect the dataframes and look for incongruencies
str(trip1)
str(trip2)
str(trip3)
str(trip4)
str(trip5)
str(trip6)
str(trip7)
str(trip8)
str(trip9)
str(trip10)
str(trip11)
str(trip12)

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(trip1,trip2,trip3,trip4,trip5,trip6,trip7,trip8,trip9,trip10,trip11,trip12)


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame? there are 795916 rows in the new table
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# Add columns that list the date, month, day, and year of each ride
# This allows to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units= "mins")
# Inspect the structure of the columns
str(all_trips)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# I will create a new version of the dataframe (clean) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_clean <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<=0),] 
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_clean$ride_length) #straight average (total ride length / rides)
median(all_trips_clean$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_clean$ride_length) #longest ride
min(all_trips_clean$ride_length) #shortest ride
# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_clean$ride_length)

# Compare members and casual users
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = mean)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = median)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = max)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = min)
# See the average ride time by each day for members vs casual users
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Tableau, or my presentation software
counts <- aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')





