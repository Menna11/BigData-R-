install.packages('ggplot2') #To Install
library(ggplot2)           #To Import
hotel_bookings<-read.csv("D:\\Downloads\\database\\hotel_bookings.csv")
data<-na.omit(hotel_bookings)
data$sum_nights<- data$stays_in_weekend_nights+ data$stays_in_week_nights



#Distribution of Arrival week number using histogram
hist(data$arrival_date_week_number,main="Arrival Date Week Number Histogram",col="lightblue",xlab="Week Number",breaks = 10)


#Distribution of the types of Hotels using pie chart
counts<-table(data$hotel)

resort<-counts["Resort Hotel"]
city<-counts["City Hotel"]

slices<-c(resort,city)
lbls<-c("Resort Hotel","City Hotel")

pct<-slices/sum(slices)*100

lbls<-paste(lbls,pct)
lbls<-paste(lbls,"%",sep="")

pie(slices,labels=lbls,col=rainbow(length(lbls)),main="Distribution of the types of Hotels")



#Distribution of lead time using density plot
hist(data$lead_time, prob=TRUE,col="lightblue",main="Lead Time Distribution",xlab="Lead Time")
lines(density(data$lead_time),col="red",lwd=2)
lines(density(data$lead_time,adjust=2),lty="dotted",col="darkgreen",lwd=3)


#BarPlot of Arrival Month and count along with a DotPlot
ggplot(data) + geom_bar(aes(x = arrival_date_month), position = "dodge") + xlab("Arrival Month") + ylab("Count")
ggplot(data) + geom_point(aes(x = days_in_waiting_list, y = arrival_date_month)) + xlim(0,100) +xlab("Number of days in waiting list")+ ylab("Arrival Month")


#BarPlot of total number of nights against hotel type
ggplot(data) + geom_bar(aes(x =sum_nights, fill=hotel), position = "dodge")+ xlim(0,30) + xlab("Sum Of Nights") + ylab("Count")


#Bar plot for City Reserved Rooms vs Assigned Rooms
citydata<-subset(data,data$hotel=="City Hotel")
cityRooms<-table(citydata$reserved_room_type , citydata$assigned_room_type)
barplot(cityRooms,legend.text = TRUE, main="City: Reserved Rooms vs Assigned Rooms",xlab="Room Type",col=rainbow(8))

#Bar plot for Resort Reserved Rooms vs Assigned Rooms
resortdata<-subset(data,data$hotel=="Resort Hotel")
resortRooms<-table(resortdata$reserved_room_type,resortdata$assigned_room_type)
barplot(resortRooms,legend.text = TRUE, main="Resort: Reserved Rooms vs Assigned Rooms",xlab="Room Type",col=rainbow(8))
