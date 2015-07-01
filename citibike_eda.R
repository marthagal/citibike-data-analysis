# Read data
stations <- read.csv("stationdata.csv")

# Install libraries
library(ggplot2)
library(gcookbook)

# Review data structure
str(stations)

# Station Usage by Average Birth Year
ggplot(stations, aes(x=reorder(X, Avg_Birth_Year_User), y=Avg_Birth_Year_User)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + coord_cartesian(ylim=c(1950,1990)) + 
   xlab("Station") + ylab("Average Year of Birth") + 
  ggtitle("Average Station Usage By Year of Birth")

ggplot(stations, aes(x=reorder(X, Avg_Birth_Year_User), y=Avg_Birth_Year_User)) + 
  geom_bar(colour="blue", stat="identity") + coord_cartesian(ylim=c(1950,1990)) + 
  xlab("Station") + ylab("Average Year of Birth") + 
  ggtitle("Average Station Usage By Year of Birth")
# Takeaways 
## Some Brooklyn stations appear to skew younger
## Stations range from an average birth year of 1972 to 1982

# Station Usage by Gender - Percentage of Females Using Stations
ggplot(stations, aes(x=reorder(X, Percentage_Females), y=Percentage_Females)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Percentage of Station Usage by Females") + 
  ggtitle("Percentage of Station Usage by Females")

ggplot(stations, aes(x=reorder(X, Percentage_Females), y=Percentage_Females)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Percentage of Station Usage by Females") + 
  ggtitle("Percentage of Station Usage by Females")

# Takeaways 
## Two Community Areas (Brooklyn_2 and Manhattan_3) have a much higher percentage of female users; Manhattan_5 
## has a much lower percentage
## Variance in station usage by women ranges from less than 10% of uses to over 45%

# Trip Duration by Station - Subscribers
ggplot(stations, aes(x=reorder(X, Avg_Trip_Duration_Sub), y=Avg_Trip_Duration_Sub)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Trip Duration by Station - Subscribers")


ggplot(stations, aes(x=reorder(X, Avg_Trip_Duration_Sub), y=Avg_Trip_Duration_Sub)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Trip Duration by Station - Subscribers")
# Takeaways 
## Variance in average trip duration by subscribers ranges from less than 500 seconds to over 2000 seconds
## Brooklyn-initiated trips tend to have longer durations for subscribers

# Trip Duration by Station - Customers
ggplot(stations, aes(x=reorder(X, Avg_Trip_Duration_Cust), y=Avg_Trip_Duration_Cust)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Trip Duration by Station - Customers")

ggplot(stations, aes(x=reorder(X, Avg_Trip_Duration_Cust), y=Avg_Trip_Duration_Cust)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Trip Duration by Station - Customers")



# Takeaways 
## Customer trips tend to be longer
## There are a few outliers at the top that should be investigated from Brooklyn and Manhattan_5

# Station Usage by Customers vs. Subscribers - Percentage of Customers Using Stations
ggplot(stations, aes(x=reorder(X, Percentage_Customers), y=Percentage_Customers)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Percentage of Station Usage by Casual Customers")

ggplot(stations, aes(x=reorder(X, Percentage_Customers), y=Percentage_Customers)) + 
  geom_bar(fill="light green", stat="identity") + 
  xlab("Station") + ylab("Trip Duration by Station") + 
  ggtitle("Percentage of Station Usage by Casual Customers")
# Takeaways 
## Variance in customer station usage ranges from zero to almost 50%
## There are four stations that have a very high percentage of customer usage in Brooklyn_2 and Manhattan_5

# Percentage of Rides Resulting in Overages
ggplot(stations, aes(x=reorder(X, Percentage_Overages), y=Percentage_Overages)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Percentages Overages") + 
  ggtitle("Percentage of Rides Resulting in Overages")

ggplot(stations, aes(x=reorder(X, Percentage_Overages), y=Percentage_Overages)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Percentage Overages") + 
  ggtitle("Percentage of Rides Resulting in Overages")

# Takeaways 
## Variance in overage percentages ranges from zero to 35%
## There are four stations that have a very high percentage of overages; again, in Brooklyn_2 and Manhattan_5

# Percentage of Rides That Are Roundtrips to the same station
ggplot(stations, aes(x=reorder(X, Percentage_Roundtrips), y=Percentage_Roundtrips)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Percentage Roundtrips") + 
  ggtitle("Percentage of Roundtrip Rides")

ggplot(stations, aes(x=reorder(X, Percentage_Roundtrips), y=Percentage_Roundtrips)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Percentage Roundtrips") + 
  ggtitle("Percentage of Roundtrip Rides")

# Station Peak Hours
ggplot(stations, aes(x=reorder(X, Peak_Hour), y=Peak_Hour)) + 
  geom_bar(aes(colour=Community_ID), stat="identity") + 
  xlab("Station") + ylab("Peak Hour") + 
  ggtitle("Peak Hour of Use By Station")

ggplot(stations, aes(x=reorder(X, Peak_Hour), y=Peak_Hour)) + 
  geom_bar(colour="blue", stat="identity") + 
  xlab("Station") + ylab("Peak Hour") + 
  ggtitle("Peak Hour of Use By Station")


# Percentage of Rides Resulting in Overages by Community Area
percentage_overage_by_com <- aggregate(Percentage_Overages~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(percentage_overage_by_com, aes(x=reorder(Community_ID, Percentage_Overages), y=Percentage_Overages)) + 
  geom_bar(stat="identity") + 
  xlab("Community Area") + ylab("Percentages Overages") + 
  ggtitle("Percentage of Rides Resulting in Overages by Community Area")
# Takeaways 
## Manhattan_3 has the lowest occurence of overages; Manhattan_7 has the highest; all of Brooklyn's CDs are near the top.

# Percentage of Rides Resulting in Overages by Community Area
percentage_roundtrip_by_com <- aggregate(Percentage_Roundtrips~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(percentage_roundtrip_by_com, aes(x=reorder(Community_ID, Percentage_Roundtrips), y=Percentage_Roundtrips)) + 
  geom_bar(stat="identity") + 
  xlab("Community Area") + ylab("Percentage Roundrips") + 
  ggtitle("Percentage of Roundtrip Rides by Community Area")

# Percentage of Rides By Women by Community Area
percentage_females_by_com <- aggregate(Percentage_Females~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(percentage_females_by_com, aes(x=reorder(Community_ID, Percentage_Females), y=Percentage_Females)) + 
  geom_bar(fill="dark green",stat="identity") + 
  xlab("Community Area") + ylab("Percentages Rides by Women") + 
  ggtitle("Percentages of Rides by Women by Community Area")
# Takeaways 
## Manhattan_3 has the highest percentage of female riders; Manhattan_5 has the lowest; all of Brooklyn's CDs are near the top.

# Percentage of Rides By Subscribers by Community Area
percentage_sub_by_com <- aggregate(Percentage_Subscribers~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(percentage_sub_by_com, aes(x=reorder(Community_ID, Percentage_Subscribers), y=Percentage_Subscribers)) + 
  geom_bar(stat="identity") + 
  xlab("Community Area") + ylab("Percentages Rides by Subscribers") + 
  ggtitle("Percentages of Rides by Subscribers by Community Area")
# Takeaways 
## The percentage of subscribers is fairly consistent at around 90% with the exception of Brooklyn_1 (75%).

# Average Birth Year by Community Area
avg_birth_year_by_com <- aggregate(Avg_Birth_Year_User~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(avg_birth_year_by_com, aes(x=reorder(Community_ID, Avg_Birth_Year_User), y=Avg_Birth_Year_User)) + 
  geom_bar(fill="dark red",stat="identity") + coord_cartesian(ylim=c(1965,1985)) + 
  xlab("Community Area") + ylab("Average Birth Year") + 
  ggtitle("Average Birth Year by Community Area")
# Takeaways 
## Brooklyn subscribers tend to be younger than Manhattan subscribers.

# Trip Duration by Customers by Community Area
trip_duration_cust_by_com <- aggregate(Avg_Trip_Duration_Cust~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(trip_duration_cust_by_com, aes(x=reorder(Community_ID, Avg_Trip_Duration_Cust), y=Avg_Trip_Duration_Cust)) + 
  geom_bar(stat="identity") + 
  xlab("Community Area") + ylab("Average Trip Duration") + 
  ggtitle("Average Trip Duration for Customers by Community Area")
# Takeaways 
## Longer trips in Brooklyn_3 and Manhattan_7, shorter trips in Manhattan_3 and Manhattan_2

# Trip Duration by Subscribers by Community Area
trip_duration_sub_by_com <- aggregate(Avg_Trip_Duration_Sub~Community_ID, data=stations, FUN=function(x) c(mean=mean(x)))
ggplot(trip_duration_sub_by_com, aes(x=reorder(Community_ID, Avg_Trip_Duration_Sub), y=Avg_Trip_Duration_Sub)) + 
  geom_bar(fill="dark green",stat="identity") + 
  xlab("Community Area") + ylab("Average Trip Duration") + 
  ggtitle("Average Trip Duration for Subscribers by Community Area")
# Takeaways 
## Longer trips in Brooklyn_6 and Manhattan_7, shorter trips in Manhattan_5 and Manhattan_2
