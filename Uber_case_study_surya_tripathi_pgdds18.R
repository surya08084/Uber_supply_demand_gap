#######  Set working directory #########################
setwd("~/Documents/Data Science/Course 2/Uber_Assignment")
#Libraries used in the code
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(stringr)
library(gridExtra)

#Load the uber data set
uber_cs<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)


#Data Cleansing:date and time format cleaning
uber_cs$Request.timestamp<-parse_date_time(x=uber_cs$Request.timestamp,order=c("d/m/Y H:M:S" , "d-m-Y H:M"))
uber_cs$Drop.timestamp<-parse_date_time(x=uber_cs$Drop.timestamp,order=c("d/m/Y H:M:S" , "d-m-Y H:M"))

#Adding weekdays column from the request and drop timestamp column to analyze load based on days of the week

uber_cs<-mutate(uber_cs,Req_weekdays=weekdays(uber_cs$Request.timestamp))
uber_cs<-mutate(uber_cs,Drop_weekdays=weekdays(uber_cs$Drop.timestamp))
uber_cs$Req_weekdays<-substr(uber_cs$Req_weekdays,1,3)
uber_cs$Drop_weekdays<-substr(uber_cs$Drop_weekdays,1,3)

#Converting categorical data into factors for better visual plotting

uber_cs$Pickup.point <- as.factor(uber_cs$Pickup.point)
uber_cs$Status <- as.factor(uber_cs$Status)
uber_cs$Driver.id <- as.factor(uber_cs$Driver.id)
uber_cs$Req_weekdays <- as.factor(uber_cs$Req_weekdays)
uber_cs$Drop_weekdays <- as.factor(uber_cs$Drop_weekdays)


# Separating time, date of request and drop time and creating new column
uber_req <- separate(uber_cs, col = "Request.timestamp", into = c("Request.date","Request.time"), sep = " ")
uber_final <- separate(uber_req, col = "Drop.timestamp", into = c("Drop.date","Drop.time"), sep = " ")

#Calculating request hour and drop hour of the day from above data
uber_final$Req.hrs <- as.factor(substring(uber_final$Request.time,0,2))
uber_final$drop.hrs <- as.factor(substring(uber_final$Drop.time,0,2))

#Time slot bucketting based on request hour
### request hour>=1 and <=5 ----Early Morning  
### request hour >5 and <12 ---Morning
### request hour >=12 and <15 ---After noon
### request hour >=15 and <=17 ---Evening
### other request hour will be treated as--- late evening

#first verify is there any NA values in request hour column

sum(is.na(uber_final$Req.hrs))     

#There is no NA values in Req.hrs column hence handelling of NA values are not required in this column

#Converting req hour in numeric format
uber_final$Req.hrs<-as.numeric(uber_final$Req.hrs)

#bucketting of timeslot as above logic

uber_final$timeSlot<-ifelse(uber_final$Req.hrs >= 1 & uber_final$Req.hrs<=5,"Early Morning",ifelse(uber_final$Req.hrs>5 & uber_final$Req.hrs<12,"Morning",
                            ifelse(uber_final$Req.hrs>=12 & uber_final$Req.hrs<15,"Afternoon",
                                   ifelse(uber_final$Req.hrs>=15 & uber_final$Req.hrs<=17,"Evening","Late-evening"))))


## calculating the Triptime to verify if there is any unusual trip like started on x day but still
## not ended after 1 or 2 days
uber_final$triptime<-as.numeric(uber_cs$Drop.timestamp-uber_cs$Request.timestamp)

#Output of this query does not have any row which means that almost everytrip is completed in less than 84 min 

filter(uber_final,uber_final$triptime>=84)

#This output indicates there is NA value of triptime only for those 
#trips which have status cancelled or No cars available
#This means that it does not need any NA value handelling.
uber_final[which(is.na(uber_final$triptime)),]%>%
  filter(!Status %in% c("Cancelled","No Cars Available"))


#Plots
############Question 1################
#Create plots to visualise the frequency of requests that get cancelled
#or show 'no cars available'; identify the most problematic types of requests 
#(city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

#####Answer 1###########################################################################################################

#converting values of time slot variable into factors and it's levels for better plotting and visualizations

uber_final$timeSlot<-factor(uber_final$timeSlot,levels=c("Early Morning","Morning","Afternoon","Evening","Late-evening"))


###Analysis based on timeslot of the day
ggplot(uber_final,aes(x=timeSlot,fill=Pickup.point))+geom_bar(position = "dodge")+ facet_wrap(~Status)+
  xlab("Time slot of the day") + ylab("Number of requests")


###Analysis based on request hour

ggplot(uber_final,aes(x=factor(Req.hrs),fill=Pickup.point))+geom_bar(position = "dodge") + facet_wrap(~Status)+
  xlab("Req Hours of the day") + ylab("Number of requests")


#Answer
# From above graph it is clear that "no cars available" is more at airport pickup points and it is in the late evening.
# Cancellation requests are more in the city and it is more in the morning slot(office hours).
# Overall no cars available scenario at airport are more problematic as these requests are not getting served by cab provider.
# Cancellation is more in the city during morning slot.
# While cancel cases are getting served to customer but it is getting cancelled may be for other reasons like :estimated time of
# arrival(ETA) is more hence it might be cancelled by customer or there is less duty on the route due to traffic/less number of 
# trips hence driver might be cancelling the trip etc.


############################### Weekdays analysis ##################################################################

#converting values of Req_weekdays variable into factors and it's levels for better plotting and visualizations

uber_final$Req_weekdays<-factor(uber_final$Req_weekdays,levels=c("Mon","Tue","Wed","Thu","Fri"))

no_cars_city<-ggplot(filter(uber_final,Status == "No Cars Available" & Pickup.point=="City"),aes(x=Req_weekdays))+geom_bar(fill="darkorange")+
              ggtitle("No Cars Available from City to Airport") +
              xlab("Cab Req day-wise") + ylab("Number of requests") +facet_wrap(~timeSlot)
cancel_cars_city<-ggplot(filter(uber_final,Status == "Cancelled" & Pickup.point=="City"),aes(x=Req_weekdays))+geom_bar(fill="darkblue")+
                  ggtitle("Cancelled cars from City to Airport") +
                  xlab("Cab Req day-wise") + ylab("Number of requests") +facet_wrap(~timeSlot)

cancel_cars_airport<- ggplot(filter(uber_final,Status == "Cancelled" & Pickup.point=="Airport"),aes(x=Req_weekdays))+geom_bar(fill="darkred")+
                      ggtitle("Cancelled cars from Airport to City") +
                      xlab("Cab Req day-wise") + ylab("Number of requests") + facet_wrap(~timeSlot)

no_cars_airport<-ggplot(filter(uber_final,Status == "No Cars Available" & Pickup.point=="Airport"),aes(x=Req_weekdays))+geom_bar(fill="darkgreen")+
                 ggtitle("No cars available from Airport to City") +
                 xlab("Cab Req day-wise") + ylab("Number of requests") +facet_wrap(~timeSlot)

grid.arrange(no_cars_city, cancel_cars_city, cancel_cars_airport, no_cars_airport, nrow = 2, ncol = 2)

##### Explanation   ###############
# No cars available scenario from City to Airport - Though frequency of request is low(around 100) but this scenario is in 
# the morning is more compare to other time slot and is uniform across five days in morning slot.
# No cars available scenario from Airport to City – The occurrence of this scenario is more in 
# the late evening time slot and almost equal across five days of the week and almost negligible in other time slot especially in the morning.
# Cancelled cars from city to airport - The occurrence of this scenario is more in the morning slot and is almost equal across the five days 
# of the week (Mon,Tue,Wed,Thrs,Fri).While in other time slot it is almost negligible.
# Cancelled cars from Airport to city - frequency of cancellation is very less at airport and it is around 20-25 requests per time slot.
# And among these less cancellation ,late evening has more cancellation (20-30 requests per day of the week) across all 5 days of the week.


#################################################################################################################
###Q-2 #####
###Find out the gap between supply and demand and show the same using plots.
###Find the time slots when the highest gap exists
###Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

### Answer 
#Assumption:Following are the decision taken to derive supply demand variable of city and airport.
           #Supply city--Pickup point airport and trip status is completed,pickup point is city and trip status is cancelled.
           #Demand city--Pickup point is city and status is "no cars available"
           #Supply airport--Pickup point city and trip status completed,pickup point airport and trip status cancelled.
           #Demand airport--pickup point Airport and trip status "no cars available"
           

uber_final$supply_demand<-ifelse((uber_final$Pickup.point=="Airport" & uber_final$Status=="Trip Completed") |( uber_final$Pickup.point=="City" & uber_final$Status=="Cancelled")
                                 ,"Supply City",
                                 ifelse((uber_final$Pickup.point=="City" & uber_final$Status=="No Cars Available") ,"Demand City",
                                 ifelse((uber_final$Pickup.point=="City" 
                                 & uber_final$Status=="Trip Completed") | (uber_final$Pickup.point=="Airport" &  uber_final$Status=="Cancelled"),
                                 "Supply Airport",
                                 ifelse((uber_final$Pickup.point=="Airport" & uber_final$Status=="No Cars Available"),
                                 "Demand Airport",""))))


#converting values of supply_demand variable into factors and it's levels for better plotting and visualizations

uber_final$supply_demand<-factor(uber_final$supply_demand,levels=c("Demand Airport","Supply Airport","Demand City","Supply City"))


#Plot based on time slot

ggplot(uber_final,aes(x=timeSlot,fill=supply_demand))+geom_bar(position = "dodge")+
      ggtitle("Supply demand analysis") +
      xlab("Cab Req timeslot-wise") + ylab("Number of requests")+
      geom_text(stat='count',aes(label=..count..), position=position_dodge(width=1),vjust=-0.5)


#Plot Based on request hour

ggplot(uber_final,aes(x=Req.hrs,fill=supply_demand))+geom_bar(position = "dodge")+
  ggtitle("Supply demand analysis") +
  xlab("Cab Req reqhour-wise") + ylab("Number of requests")

##### Answer
# Demand and supply gap in the city, is more in the morning time slot while in the airport ,demand and supply
# gap is more in the late evening.
# Demand at airport is less in afternoon, Evening and morning but supply is more in these time slots.
# Supply in the city is more compare to demand in the city and this gap is more in the morning and late evening.
# In the late evening, demand at Airport (airport-city), is more and supply is less which in turn hitting the 
# revenue of the company because company is not able to serve cab to these customer.
# In the morning tme slot,supply is more in the city(city-airport) compare to demand.
# So most severe gap (Demand-supply ) is in late evening at Airport to city route.


##### Q3 #################################################################################
### What do you think is the reason for this issue for the supply-demand gap?
### Write the answer in less than 100 words. You may accompany the write-up with plot(s).

#Answer#########
# Routing trips to high demand zone and once these locations got enough supply then also no alert messages sent to drivers.
# as supply increased compare to demand and this eventually leads to supply demand gap.
# In the evening time at airport supply is more and demand is less while in the late evening supply is less and demand is more.
# Which means drivers waited till evening for the trip but maximum drivers were not getting the trip which leads them to change the pickup location.
# and when in late evening demand gets increased then supply were not up to that extent which created supply demand gap at airport pick up point.
# Till early morning in the city supply was almost balanced with demand but in the morning slot supply got increased and demand was less compare to supply.
# This seems every driver thought that due to office hour rush will be there in the city and they moved to city but no alert was sent to driveres from the
# cab provider that the demand is already filled in by the drivers and moving to city will not be profitable.
# Also in the city,morning time due to office hour people take trip to office and it's almost short distance trip.So for driver point of view they
# can achieve their daily count by attending these trips hence they try to attend these kind of trips.
# This sudden increase in the supply of cab increases gap in supply and demand.

###overall trip status analysis pickup pointwise

ggplot(uber_final,aes(x=Status,fill=Pickup.point))+geom_bar(position = "dodge")+
  ggtitle("Pick up point-wise Over all trip Status Analysis") +
  xlab("Trip Status") + ylab("Number of requests")

#Final cleansed data for visulaization on tableau
write.csv(uber_final,"final_uber_case_study.csv")
