hotel_bookings<-read.csv("D:\\Downloads\\database\\hotel_bookings.csv")
data<-na.omit(hotel_bookings)
hist(data$arrival_date_week_number,main="Arrival Date Week Number Histogram",col="lightblue",xlab="Week Number",breaks = 10)