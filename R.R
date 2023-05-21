hotel_bookings<-read.csv("D:\\Downloads\\database\\hotel_bookings.csv")
data<-na.omit(hotel_bookings)
hist(data$arrival_date_week_number,main="Arrival Date Week Number Histogram",col="lightblue",xlab="Week Number",breaks = 10)
#Distribution of Arrival week number using histogram
hist(data$arrival_date_week_number,main="Arrival Date Week Number Histogram",col="lightblue",xlab="Week Number",breaks = 10)


#Distribution of the types of Hotels using pie chart
counts<-table(data$hotel)
resort<-counts["Resort Hotel"]
city<-counts["City Hotel"]
slices<-c(resort,city)
pct<-slices/sum(slices)*100
lbls<-paste(lbls,pct)
lbls<-paste(lbls,"%",sep="")
pie(slices,labels=lbls,col=rainbow(length(lbls)),main="Distribution of the types of Hotels")


#Distribution of lead time using density plot
hist(data$lead_time, prob=TRUE,col="lightblue",main="Lead Time Distribution",xlab="Lead Time")
lines(density(data$lead_time),col="red",lwd=2)
lines(density(data$lead_time,adjust=2),lty="dotted",col="darkgreen",lwd=3)