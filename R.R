install.packages('ggplot2') #To Install
library(ggplot2)           #To Import
hotel_bookings<-read.csv("D:\\Downloads\\database\\hotel_bookings.csv")
data<-na.omit(hotel_bookings)
data$sum_nights<- data$stays_in_weekend_nights+ data$stays_in_week_nights


#Data Cleaning
 dataset <- replace(dataset, dataset == "NULL", NA)
 missing_values <- colSums(is.na(dataset))
 missing_values_count <- sum(is.na(dataset))
 nan_replacements <- list(children = 0.0, country = "Unknown", agent = 0, company = 0)
 full_data_cln <- dataset
 full_data_cln$children <- ifelse(is.na(full_data_cln$children), nan_replacements$children, full_data_cln$children)
 full_data_cln$country <- ifelse(is.na(full_data_cln$country), nan_replacements$country, full_data_cln$country)
 full_data_cln$agent <- ifelse(is.na(full_data_cln$agent), nan_replacements$agent, full_data_cln$agent)
 full_data_cln$company <- ifelse(is.na(full_data_cln$company), nan_replacements$company, full_data_cln$company)
 full_data_cln$meal <- gsub("Undefined", "SC", full_data_cln$meal)
 zero_guests <- full_data_cln$adults + full_data_cln$children + full_data_cln$babies == 0
 full_data_cln <- full_data_cln[!zero_guests, ]




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



#PieChartforCountries

country_data <- as.data.frame(table(full_data_cln$country[full_data_cln$is_canceled == 0]))
country_data <- country_data[order(-country_data$Freq), ]
top_countries <- 5
other_freq <- sum(country_data$Freq[(top_countries + 1):nrow(country_data)])
country_data$Var1 <- as.character(country_data$Var1)
country_data_new <- rbind(country_data[1:top_countries, ], c("Other", other_freq))
country_data_new$Freq <- as.numeric(country_data_new$Freq)
percentages <- round(country_data_new$Freq / sum(country_data_new$Freq) * 100, 1)
labels <- paste(country_data_new$Var1, percentages, "%")
pie(country_data_new$Freq, labels = labels, main = "Guests by Country")


#boxblotforroomtypes

 full_data_cln$adr_pp <- full_data_cln$adr / (full_data_cln$adults + full_data_cln$children)
 full_data_guests <- full_data_cln[full_data_cln$is_canceled == 0, ]
 room_prices <- full_data_guests[, c("hotel", "reserved_room_type", "adr_pp")]
 room_prices <- room_prices[order(room_prices$reserved_room_type), ]

 library(ggplot2)

 ggplot(room_prices, aes(x = reserved_room_type, y = adr_pp)) +
  geom_boxplot(aes(fill = hotel), outlier.shape = NA) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "green")) +
  labs(title = "Price of room types per night and person",
       x = "Room type",
       y = "Price [EUR]") +
  ylim(0, 160) +
  theme_minimal()



#correlationpart
numeric_vars <- sapply(full_data_cln, is.numeric)
numeric_data <- full_data_cln[, numeric_vars]
cancel_corr <- cor(numeric_data$is_canceled, numeric_data)
cancel_corr_sorted <- sort(abs(cancel_corr), decreasing = TRUE)
cancel_corr_sorted[-1]

#Decisiontree

 library(rpart)
 data_subset <- full_data_cln[c("is_canceled", "lead_time", "total_of_special_requests", "booking_changes", "previous_cancellations")]
 set.seed(123)
 train_indices <- sample(nrow(data_subset), 0.7 * nrow(data_subset))
 train_data <- data_subset[train_indices, ]
 test_data <- data_subset[-train_indices, ]
 train_data$is_canceled <- factor(train_data$is_canceled)
 model <- rpart(is_canceled ~ lead_time + total_of_special_requests + booking_changes + previous_cancellations, data = train_data, method = "class")
 rpart.plot(model, type = 3, extra = 102, fallen.leaves = TRUE, under = TRUE, faclen = 0, cex = 0.8)
 predictions <- predict(model, newdata = test_data, type = "class")
 accuracy <- sum(predictions == test_data$is_canceled) / nrow(test_data) * 100
 cat("Accuracy:", accuracy)

