# Calling various libraries used

library(lubridate)
library(ggplot2)
library(dplyr)



# Sourcing ----------------------------------------------------------------

# Loading the data set

uber<-read.csv("Uber Request Data.csv")
View(uber)






# Cleaning and Preparation ------------------------------------------------

# Cleaning the data set

uber$Request.timestamp<-parse_date_time(x= uber$Request.timestamp,orders = c("d/m/Y H:M" , "d-m-Y H:M:S"))

uber$Drop.timestamp<-parse_date_time(x=uber$Drop.timestamp,orders = c("d/m/y H:M","d-m-y H:M:S"))


# Deriving Type Driven Metrics

uber$Request.hour<-hour(uber$Request.timestamp)

uber$Drop.hour<-hour(uber$Drop.timestamp)

uber$Request.Day<-wday(uber$Request.timestamp,label = TRUE)


# Deriving Business Driven Metrics

uber$Time.Slot<-ifelse(uber$Request.hour>=5 & uber$Request.hour<=9,"Morning",ifelse(uber$Request.hour>=10 & uber$Request.hour<=13,"Near Noon",ifelse(uber$Request.hour>=14 & uber$Request.hour<=17,"Early Evening",ifelse(uber$Request.hour>=18 & uber$Request.hour<=22,"Late Evening","Midnight"))))




# Analysis PART 1----------------------------------------------------------------

# Identifying the most pressing problems visually




# 1. Frequency of different Requests

uber_denied_obj<-ggplot(uber,aes(x=factor(uber$Status),col = "red"))

uber_denied<-uber_denied_obj+geom_bar()+geom_text(stat = "count",aes(label=..count..),vjust=-1)+xlab("Status")+ylab("Count Frequency")+ggtitle("PLOT 1: FREQUECY OF DIFFERENT REQUESTS")
uber_denied




# 2. Most problematic type of requests according to Pickup Point

uber_rejected<-filter(uber,uber$Status!="Trip Completed")

problem_pickup_point_obj<-ggplot(uber_rejected,aes(x=factor(Status),fill=factor(Pickup.point)))

problem_pickup_point<-problem_pickup_point_obj+geom_bar()+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Status")+ylab("Count Frequency")+ggtitle("PLOT 2: PICKUP PROBLEM")+labs(fill="Pickup Point")
problem_pickup_point





# 3. Most problematic type of requests hourly


# (a) Considering all the factor levels of "Status"

problem_hourly_obj<-ggplot(uber,aes(x=Request.hour,fill=factor(Status)))

problem_hourly<-problem_hourly_obj+geom_bar()+xlab("Hours")+ylab("Count Frequency")+ggtitle("PLOT 3: HOURLY REQUESTS DISTRIBUTION")+labs(fill="Status")
problem_hourly


# (b) Considering only Denied Status

problem_hourly_rejected_obj<-ggplot(uber_rejected,aes(x=Request.hour,fill=factor(Status)))

problem_hourly_rejected<-problem_hourly_rejected_obj+geom_bar()+xlab("Hours")+ylab("Count Frequency")+ggtitle("PLOT 4: HOURLY PROBLEM")+labs(fill="Denied Status")
problem_hourly_rejected







# 4. Most problematic type of requests according to Day


problem_weekday_obj<-ggplot(uber_rejected,aes(x=Request.Day,fill=factor(Status)))

problem_weekday<-problem_weekday_obj+geom_bar(position = "dodge")+xlab("Weekdays")+ylab("Count Frequency")+ggtitle("PLOT 5: WEEKDAY PROBLEM")+theme(axis.text.x = element_text(angle = 90,hjust = 1))+labs(fill="Denied Status")
problem_weekday








# Analysis PART 2 ---------------------------------------------------------

# Finding out the gap between supply and demand and showing the same using plots



# 1. Finding the Hours when the highest gap exists


# (a) Finding The Hourly Supply Demand Gap Data Frame

demand_hourly<-group_by(uber,"Request_hour"=uber$Request.hour)
demand_hourly<-summarise(demand_hourly,"Demand" = n())
demand_hourly

uber_supply<-filter(uber,uber$Status=="Trip Completed")
supply_hourly<-group_by(uber_supply,"Request_hour"=uber_supply$Request.hour)
supply_hourly<-summarise(supply_hourly,"Supply" = n())
supply_hourly

hour_analysis<-cbind(demand_hourly,supply_hourly[,-1])
hour_analysis$Gap<-hour_analysis$Demand-hour_analysis$Supply
hour_analysis


# (b) Plotting Hourly Supply Demand Gap 

hourly_gap_obj<-ggplot(hour_analysis,aes(x=hour_analysis$Request_hour,y=hour_analysis$Gap))

hourly_gap<-hourly_gap_obj+geom_point()+geom_line(col="blue")+xlab("Request Hour")+ylab("Demand Supply Gap")+ggtitle("PLOT 6: HOURLY SUPPLY DEMAND GAP")
hourly_gap



# 2. Finding the Time Slot when the highest gap exists


# (a) Finding the Time Slot Supply Demand gap data frame

time_slot_demand<-group_by(uber,"Time_Slot"=uber$Time.Slot)
time_slot_demand<-summarise(time_slot_demand,"Demand"=n())
time_slot_demand

uber_supply_time_slot<-filter(uber,uber$Status=="Trip Completed")
time_slot_supply<-group_by(uber_supply_time_slot,"Time_Slot"=uber_supply_time_slot$Time.Slot)
time_slot_supply<-summarise(time_slot_supply,"Supply"=n())
time_slot_supply

time_slot_analysis<-cbind(time_slot_demand,time_slot_supply[,-1])
time_slot_analysis$Gap<-time_slot_analysis$Demand-time_slot_analysis$Supply
time_slot_analysis


# (b) Plotting Time Slot wise Supply Demand Gap 

time_slot_gap_obj<-ggplot(time_slot_analysis,aes(x=Time_Slot,y=time_slot_analysis$Gap))

time_slot_gap<-time_slot_gap_obj+geom_bar(stat="identity",col="blue")+xlab("Time Slot")+ylab("Demand Supply Gap")+ggtitle("PLOT 7: TIME-SLOT SUPPLY DEMAND GAP")+theme(axis.text.x = element_text(angle = 90,hjust = 1))
time_slot_gap





# 2. Finding the types of requests (city-airport or airport-city)
# for which the gap is the most severe in the identified time slots

# Therefore from PLOT 7 the Time Slots "Morning" and "Late Evening" are identified as severe



# (a) Plotting Demand Supply Gap for "Morning" based on location

morning_time_slot_obj<-ggplot(data = filter(uber,uber$Time.Slot=="Morning"),aes(x=Status,fill=factor(Pickup.point)))

morning_time_slot<-morning_time_slot_obj+geom_bar()+xlab("Status")+ylab("Count")+labs(fill="Location")+ggtitle("PLOT 8: MORNING, PICKUP ANALYSIS")+theme(axis.text.x = element_text(angle = 90,hjust = 1))
morning_time_slot


# (b) Plotting Demand Supply Gap for "Late Evening" based on location

late.evening_time_slot_obj<-ggplot(data = filter(uber,uber$Time.Slot=="Late Evening"),aes(x=Status,fill=factor(Pickup.point)))

late.evening_time_slot<-late.evening_time_slot_obj+geom_bar()+xlab("Status")+ylab("Count")+labs(fill="Location")+ggtitle("PLOT 9: LATE EVENING, PICKUP ANALYSIS")+theme(axis.text.x = element_text(angle = 90,hjust = 1))
late.evening_time_slot





# END ---------------------------------------------------------------------


