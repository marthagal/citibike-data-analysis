# Read data
stations <- read.csv("Unique_Station_List.csv")
bike <- read.csv("Master_Citibike_Data.csv")

# Load libraries
library(plyr)
library(lubridate)

# Review data structures
str(stations)
str(bike)

# Create station lookup table
lookup <- stations[c(4,7)]

# Modify bike$start.station.id to bike$Station.ID
colnames(bike)[colnames(bike)=="start.station.id"] <- "Station.ID"

# Join datasets and add Community_ID to bike dataset
dataset <- join(bike,lookup,by="Station.ID")
colnames(dataset)[colnames(dataset)=="Community_ID"] <- "Start_Community_ID"
colnames(dataset)[colnames(dataset)=="Station.ID"] <- "Start_Station_ID"
str(dataset)

# Modify dataset$end.station.id to dataset$Station.ID
colnames(dataset)[colnames(dataset)=="end.station.id"] <- "Station.ID"

# Join datasets and add Community_ID to bike dataset
dataset <- join(dataset,lookup,by="Station.ID")
colnames(dataset)[colnames(dataset)=="Community_ID"] <- "End_Community_ID"
colnames(dataset)[colnames(dataset)=="Station.ID"] <- "End_Station_ID"
str(dataset)

# Calculate average age per station
dataset$birth.year <- as.numeric(as.character(dataset$birth.year))
average_age_by_station <- aggregate(birth.year~Start_Station_ID, data=dataset, FUN=function(x) c(mean=mean(x)))
colnames(average_age_by_station)[colnames(average_age_by_station)=="birth.year"] <- "Avg_Birth_Year_User"
colnames(average_age_by_station)[colnames(average_age_by_station)=="Start_Station_ID"] <- "Station.ID"
stations <- join(stations,average_age_by_station,by="Station.ID")

# Calculate average trip time per station where the rider is subscriber
subscriber_data <- subset(dataset, usertype=="Subscriber")
average_trip_duration_station_sub <- aggregate(tripduration~Start_Station_ID, data=subscriber_data, FUN=function(x) c(mean=mean(x)))
colnames(average_trip_duration_station_sub)[colnames(average_trip_duration_station_sub)=="tripduration"] <- "Avg_Trip_Duration_Sub"
colnames(average_trip_duration_station_sub)[colnames(average_trip_duration_station_sub)=="Start_Station_ID"] <- "Station.ID"
stations <- join(stations,average_trip_duration_station_sub,by="Station.ID")

# Calculate average trip time per station where the rider is customer
customer_data <- subset(dataset, usertype=="Customer")
average_trip_duration_station_cust <- aggregate(tripduration~Start_Station_ID, data=customer_data, FUN=function(x) c(mean=mean(x)))
colnames(average_trip_duration_station_cust)[colnames(average_trip_duration_station_cust)=="tripduration"] <- "Avg_Trip_Duration_Cust"
colnames(average_trip_duration_station_cust)[colnames(average_trip_duration_station_cust)=="Start_Station_ID"] <- "Station.ID"
stations <- join(stations,average_trip_duration_station_cust,by="Station.ID")

# Calculate percentage of customers and subscribers for each station
percentage <- dataset[c(4,13)]
percentage_cust_sub <- prop.table(table(percentage),1)
p <- data.frame(percentage_cust_sub)
p$Freq <- p$Freq*100
# Customers
customer_p <- subset(p, usertype=="Customer")
customer_per <- customer_p[c(1,3)]
colnames(customer_per)[colnames(customer_per)=="Freq"] <- "Percentage_Customers"
colnames(customer_per)[colnames(customer_per)=="Start_Station_ID"] <- "Station.ID"
customer_per$Station.ID <- as.numeric(as.character(customer_per$Station.ID))
stations <- join(stations,customer_per,by="Station.ID")
# Subscribers
subscriber_p <- subset(p, usertype=="Subscriber")
subscriber_per <- subscriber_p[c(1,3)]
colnames(subscriber_per)[colnames(subscriber_per)=="Freq"] <- "Percentage_Subscribers"
colnames(subscriber_per)[colnames(subscriber_per)=="Start_Station_ID"] <- "Station.ID"
subscriber_per$Station.ID <- as.numeric(as.character(subscriber_per$Station.ID))
stations <- join(stations,subscriber_per,by="Station.ID")

# Calculate percentage of men and women for each station
percentage_g <- dataset[c(4,15)]
percentage_g <- subset(percentage_g, gender != 0)
percentage_gender <- prop.table(table(percentage_g),1)
p_g <- data.frame(percentage_gender)
p_g$Freq <- p_g$Freq*100
# Males
p_male <- subset(p_g, gender==1)
p_males <- p_male[c(1,3)]
colnames(p_males)[colnames(p_males)=="Freq"] <- "Percentage_Males"
colnames(p_males)[colnames(p_males)=="Start_Station_ID"] <- "Station.ID"
p_males$Station.ID <- as.numeric(as.character(p_males$Station.ID))
stations <- join(stations,p_males,by="Station.ID")
# Females
p_female <- subset(p_g, gender==2)
p_females <- p_female[c(1,3)]
colnames(p_females)[colnames(p_females)=="Freq"] <- "Percentage_Females"
colnames(p_females)[colnames(p_females)=="Start_Station_ID"] <- "Station.ID"
p_females$Station.ID <- as.numeric(as.character(p_females$Station.ID))
stations <- join(stations,p_females,by="Station.ID")

# Create overage variable to indicate if a ride resulted in an overage
dataset$overage <- "FALSE"
dataset$overage[dataset$usertype == "Subscriber" & dataset$tripduration > 2700] <- "TRUE"
dataset$overage[dataset$usertype == "Customer" & dataset$tripduration > 1800] <- "TRUE"

# Calcuate percentage of overages
percentage_overage <- dataset[c(4,18)]
percentage_overages <- prop.table(table(percentage_overage),1)
p_o <- data.frame(percentage_overages)
p_o$Freq <- p_o$Freq*100
# Overages
p_over <- subset(p_o, overage=="TRUE")
p_overage <- p_over[c(1,3)]
colnames(p_overage)[colnames(p_overage)=="Freq"] <- "Percentage_Overages"
colnames(p_overage)[colnames(p_overage)=="Start_Station_ID"] <- "Station.ID"
p_overage$Station.ID <- as.numeric(as.character(p_overage$Station.ID))
stations <- join(stations,p_overage,by="Station.ID")

# Create round-trip variable to indicate if a ride was a round-trip to the same station
dataset$roundtrip <- "FALSE"
dataset$roundtrip[dataset$Start_Station_ID == dataset$End_Station_ID] <- "TRUE"

# Calcuate percentage of roundtrips
percentage_roundtrip <- dataset[c(4,19)]
percentage_roundtrips <- prop.table(table(percentage_roundtrip),1)
p_r <- data.frame(percentage_roundtrips)
p_r$Freq <- p_r$Freq*100
# Overages
p_round <- subset(p_r, roundtrip=="TRUE")
p_roundtrip <- p_round[c(1,3)]
colnames(p_roundtrip)[colnames(p_roundtrip)=="Freq"] <- "Percentage_Roundtrips"
colnames(p_roundtrip)[colnames(p_roundtrip)=="Start_Station_ID"] <- "Station.ID"
p_roundtrip$Station.ID <- as.numeric(as.character(p_roundtrip$Station.ID))
stations <- join(stations,p_roundtrip,by="Station.ID")

# Create hour variable to show the time that the trip occurred
dataset$starttime_c <- (as.character(dataset$starttime, format = "%m/%d/%Y %H:%M:%S"))
dataset$starttime_p <- strptime(dataset$starttime, format = "%m/%d/%Y %H:%M" )
dataset$hour <- hour(dataset$starttime_p)

# Calculate peak hour for a given station
peak_hour <- dataset[c(4,22)]
myFun <- function(x){
  tbl <- table(x$hour)
  x$freq <- rep(names(tbl)[which.max(tbl)],nrow(x))
  x
}
peak_hours <- ddply(peak_hour,.(Start_Station_ID),.fun=myFun)
p_hours <- peak_hours[c(1,3)]
p_hours$freq <- as.numeric(p_hours$freq)
peak_hour_by_station <- aggregate(freq~Start_Station_ID, data=p_hours, FUN=function(x) c(mean=mean(x)))
colnames(peak_hour_by_station)[colnames(peak_hour_by_station)=="freq"] <- "Peak_Hour"
colnames(peak_hour_by_station)[colnames(peak_hour_by_station)=="Start_Station_ID"] <- "Station.ID"
stations <- join(stations,peak_hour_by_station,by="Station.ID")

# Write csv
write.csv(stations, file="stationdata.csv")

