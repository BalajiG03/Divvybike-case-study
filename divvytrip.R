#install and load libraries
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)#helps wrangle data
library(lubridate)#helps wrangle data attributes
#------------------------
#collect data
#------------------------
#upload divvy datasets (csv files)
q2_2019<-read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019<-read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019<-read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020<-read.csv("Divvy_Trips_2020_Q1.csv")
#----------------------------------------------
#oraganize data and combine into a single file
#----------------------------------------------
#compare the column names of each dataset
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)
#column name differes.so, we have to change column name in every dataset as column name of q1_2020 dataset to make consistent and to use command to join them into one dataset
#rename the column
(q2_2019<-rename(q2_2019,ride_id="X01...Rental.Details.Rental.ID",rideable_type="X01...Rental.Details.Bike.ID",started_at="X01...Rental.Details.Local.Start.Time",ended_at="X01...Rental.Details.Local.End.Time",
                 start_station_name="X03...Rental.Start.Station.Name",start_station_id="X03...Rental.Start.Station.ID",end_station_name="X02...Rental.End.Station.Name",end_station_id= "X02...Rental.End.Station.ID",
                 member_casual="User.Type"))
(q3_2019<-rename(q3_2019,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,
                 end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype))
(q4_2019<-rename(q4_2019,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,
                 end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype))
#check the dataframes and look for different datatypes exist
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)
#In dataframe we have to change ride_id and rideable_type to character.So, we can stack correctly with q1_2020 dataframe
q2_2019<-mutate(q2_2019,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))
q3_2019<-mutate(q3_2019,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))
q4_2019<-mutate(q4_2019,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))
#verify once more the dataframes
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)
#stack individual quarter's dataframes into one big dataframe as "trips"
trips<-bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)
View(trips)#view the new dataframe "trips"
#remove data not necessary for analyze
trips<-trips %>% select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,tripduration,"X01...Rental.Details.Duration.In.Seconds.Uncapped","X05...Member.Details.Member.Birthday.Year",
                           "Member.Gender"))
view(trips)
#-----------------------------------------------
#clean up and add data to prepare for analysis
#-----------------------------------------------
#inspect new table created
colnames(trips)#check the list of all column names
nrow(trips)#number of rows in dataframe
dim(trips)#columns and rows in dataframe
n_distinct(trips$ride_id)
is.null(trips)
head(trips)#check dataframe by viewing first 6 row
tail(trips)#check dataframe by last 6 row
str(trips)#check datatype of dataframe
summary(trips)#statistical summary of data
#there are few problems needed to be fix
##In the "member_casual" column, there are four labels customer,casual,subscriber and member. member_casual column has to be consolidate from four to two labels
trips<-trips %>% mutate(member_casual=recode(member_casual,"Subscriber"="member","Customer"="casual"))
#check the member_casual column consolidated to two label
table(trips$member_casual)
#data can only be aggregaed at ride-level. we want to add some column such as day,month,year. This provide additional oppourtunity to aggregate data.
trips$date<-format(trips$started_at)
trips$month<-format(as.Date(trips$date),"%m")
trips$day<-format(as.Date(trips$date),"%d")
trips$year<-format(as.Date(trips$date),"%Y")
trips$day_of_week<-format(as.Date(trips$date),"%A")
#tripduration has to be calculated because q1_2020 dataset has no value of duration. Hence add new column ride_length to calculate tripduration.
trips$ride_length<-difftime(trips$ended_at,trips$started_at)#units in seconds(difftime is used to calculate from datetime value)
#inspect dataframe 
str(trips)
#ride_length column in difftime format. It has to be changed to numeric format
is.difftime(trips$ride_length)#confirming the ride_length column
trips$ride_length<-as.numeric(as.character(trips$ride_length))
#verify the dataframe
str(trips)
summary(trips)#statistical result of dataset
#In ride length column duration values are in negative and few values were taken out of docks. Remove the bad data and create new dataframe trips_v2
trips_v2<-trips[!(trips$start_station_name=="HQ QR" | trips$ride_length<0),]
#verify if the dataset contain negative value
summary(trips_v2)
#-----------------------------
#Analysis
#-----------------------------
#analysis for ride_length. calculate mean,meadian,max,min(units: seconds)
summary(trips_v2$ride_length)
#compare the member and casual users to find diffrence between annual and casual riders
aggregate(trips_v2$ride_length~trips_v2$member_casual,FUN = mean)
aggregate(trips_v2$ride_length~trips_v2$member_casual,FUN = median)
aggregate(trips_v2$ride_length~trips_v2$member_casual,FUN = max)
aggregate(trips_v2$ride_length~trips_v2$member_casual,FUN = min)
#average ride time by each day for members vs casual users
aggregate(trips_v2$ride_length~trips_v2$member_casual + trips_v2$day_of_week,FUN = mean)
#sort the day of week
trips_v2$day_of_week<-ordered(trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
aggregate(trips_v2$ride_length~trips_v2$member_casual + trips_v2$day_of_week,FUN = mean)
#analyze ridership data by usertype and weekday and store it new dataframe trips v3
trips_v3<-trips_v2 %>% mutate(weekday=wday(started_at,label=TRUE)) %>% group_by(member_casual,weekday) %>% summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% arrange(member_casual,weekday)
#----------------------------
#Visualize dataframe
#----------------------------
#visualize number of rides by usertype using bar graph
#Plot a bar graph that shows weekly frequency distribution of memeber and casual riders with number of rides by member and casual
ggplot(trips_v3,mapping = aes(x=weekday,y=number_of_rides,fill=member_casual))+geom_col(position = "dodge")
#plot a bar graph that shows weekly average duration of ride by member and casual riders
ggplot(trips_v3,mapping = aes(x=weekday,y=average_duration,fill=member_casual))+geom_col(position = "dodge")
