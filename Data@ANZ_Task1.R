#Data@ANZ Virtual Internship Program

#Task 1 - Data Analysis & Visualization

library(dplyr)
library(ggplot2)
library(forecast)
library(ggmap)
library(lubridate)
library(maps)
library(stringr)
library(scales)
library(data.table)
library(ggmosaic)
library(RColorBrewer)


# Importing the dataset
ANZ_df <- read.csv('ANZ_data.csv', header = TRUE, na.strings=c("","NA"))

#Saving the date as a date variable
ANZ_df$date <- as.Date(ANZ_df$date,format = "%d/%m/%Y")

#Encoding categorical data
ANZ_df$movement <- factor(ANZ_df$movement,
                         levels = c('debit', 'credit'),
                         labels = c(1, 2))

#Finding the average transaction amount
average_transactions <- mean(ANZ_df$amount)#Average_transaction_amount=187.9336

#Finding the average debit transaction amount and average credit transaction amount

debit_data <- subset(ANZ_df, ANZ_df$movement==1)
average_debittrans <- mean(debit_data$amount)
  
credit_data <- subset(ANZ_df, ANZ_df$movement==2)  
average_credittrans <- mean(credit_data$amount)

ggplot(ANZ_df, aes(x=factor(movement)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill = c("#3399FF", "#CC0033"), colour="black")+  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + labs(title = "Distribution of transaction type", y = "Percentage", x = "Transaction type") + coord_flip()



###############################################################################################################################


#Analyzing Merchant State and Transaction Description


ANZ_df_merchant <- ANZ_df %>% group_by(merchant_state) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))


ggplot(ANZ_df_merchant, aes(x=merchant_state , y=count)) + 
  geom_bar(width = 0.3, stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#3399FF", "#CC0033")) + coord_flip()+ geom_text(aes(label= count), vjust=2) + labs(title = "Distribution of Transactions by Merchant State", y="Tranactions", x="Merchant State")

ggplot(ANZ_df_merchant, aes(x=merchant_state , y=mean_amount)) + 
  geom_bar(width = 0.3, stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#3399FF", "#CC0033")) + coord_flip()+ geom_text(aes(label= round(mean_amount,2)), vjust=2) + labs(title = "Distribution of Mean Transaction Amount by Merchant State", y="Mean Transaction Amount", x="Merchant State")


ANZ_df_dis <- ANZ_df %>% group_by(txn_description) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))


ggplot(ANZ_df_dis, aes(x=txn_description , y=count)) + 
  geom_bar(width = 0.3, stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#3399FF", "#CC0033")) + coord_flip()+ geom_text(aes(label= count), vjust=2) + labs(title = "Distribution of Transactions by Purpose", y="Tranactions", x="Purpose")

ggplot(ANZ_df_dis, aes(x=txn_description , y=mean_amount)) + 
  geom_bar(width = 0.3, stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#3399FF", "#CC0033")) + coord_flip()+ geom_text(aes(label= round(mean_amount,2)), vjust=2) + labs(title = "Distribution of Mean Transaction Amount by Purpose", y="Mean Transaction Amount", x="Purpose")


#################################################################################################################################


#Finding the unique set of 100 hypothetical customers
ANZ_df_id <- unique(ANZ_df$customer_id)

ANZ_df_cus <- ANZ_df %>% group_by(customer_id, age, gender, long_lat) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))

sum(ANZ_df_cus$count)

agebreaks <- c(18,30,45,60,80)
agelabels <- c("18-29","30-44","45-59","60+")

setDT(ANZ_df_cus)[ , agegroups := cut(ANZ_df_cus$age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

#Visualizing transaction count
boxplot(ANZ_df_cus$count, horizontal = TRUE, col = "#3399FF", xlab= "Transaction Count", main="Distribution of Transaction Count")

summary(ANZ_df_cus$count)
#Average number of transactions made by a customer = 120.43 

#Visualizing mean transaction amount 
summary(ANZ_df_cus$mean_amount)
ggplot(ANZ_df_cus, aes(x=mean_amount)) + geom_histogram(color="darkblue", fill="lightblue")+labs(title="Distribution of Mean Transaction Amount", x="Mean Amount", y="Frequency")

boxplot(ANZ_df_cus$mean_amount, horizontal = TRUE, col = "#3399FF", xlab= "Mean Transaction Amount", main="Distribution of Mean Transaction Amount")


ggplot(ANZ_df_cus, aes(x=factor(gender)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill = c("#FF66CC", "#3399FF"), colour="black")+  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + labs(title = "Gender distribution of ANZ customers", y = "Percentage", x = "Gender") + coord_flip()

ggplot(ANZ_df_cus, aes(x=factor(ANZ_df_cus$agegroups)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill = c("#FFCC00", "#CC0033", "#660099", "#3399FF"), colour="black")+  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + labs(title = "Age group distribution of ANZ customers", y = "Percentage", x = "Age group") + coord_flip()


ggplot(ANZ_df_cus, aes(x=gender, y=age, fill= gender))+ geom_boxplot(width=0.3)+ labs(title = "Distribution of Customer Age by Gender", x= "Customer Gender", y = "Customer Age") + coord_flip()


ggplot(data = ANZ_df_cus) +
  geom_mosaic(aes(x = product(agegroups, gender), fill=gender), conds=product(agegroups), na.rm=TRUE) + labs(x = "Gender", y = "Age group", title='Distribution of Age groups by Gender')+ theme(axis.text.x=element_text(angle=-25, hjust= .1)) + facet_grid(agegroups~.) + 
  guides(fill=guide_legend(title = "Gender", reverse = TRUE))


ggplot(ANZ_df_cus, aes(x=gender, y=total_amount, fill= gender))+ geom_boxplot(width=0.3)+ labs(title = "Distribution of Total Transaction Amount by Customer Gender", x= "Gender", y = "Total transaction amount") + coord_flip()

ggplot(ANZ_df_cus, aes(x=gender, y=mean_amount, fill= gender))+ geom_boxplot(width=0.3)+ labs(title = "Distribution of Mean Transaction Amount by Customer Gender", x= "Gender", y = "Mean transaction amount") + coord_flip()

ggplot(ANZ_df_cus, aes(x=agegroups, y=mean_amount, fill= agegroups))+ geom_boxplot(width=0.3)+ labs(title = "Distribution of Mean Transaction Amount by Age Group", x= "Age Group", y = "Mean transaction amount") + coord_flip()


#################################################################################################################################


#Finding the average transaction amount per month. Therefore creating a new variable in the ANZ_df as "month"
ANZ_df$month <- month(as.POSIXlt(ANZ_df$date, format="%d/%m/%Y"))
attach(ANZ_df)

#August
trans_Aug <- subset(ANZ_df, ANZ_df$month == 8)
average_trans_Aug <- mean(trans_Aug$amount)#average_trans_Aug=185.1219

ANZ_df_cus_Aug <- trans_Aug %>% group_by(customer_id, age, gender, long_lat) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))

#Average transaction count in August= 39.43  
summary(ANZ_df_cus_Aug$count)

#September
trans_Sep <- subset(ANZ_df, ANZ_df$month == 9)
average_trans_Sep <- mean(trans_Sep$amount)#average_trans_Sep=182.0459

ANZ_df_cus_Sept <- trans_Sep %>% group_by(customer_id, age, gender, long_lat) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))

#Average transaction count in September= 40.13
summary(ANZ_df_cus_Sept$count)

#October
trans_Oct <- subset(ANZ_df, ANZ_df$month == 10)
average_trans_Oct <- mean(trans_Oct$amount)#average_trans_Oct=196.4273

ANZ_df_cus_Oct <- trans_Oct %>% group_by(customer_id, age, gender, long_lat) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))

#Average transaction count in October= 40.87
summary(ANZ_df_cus_Oct$count)


ANZ_df$month <- factor(ANZ_df$month,
                       levels = c('8', '9', '10'),
                       labels = c('August','September', 'October' ))

ggplot(ANZ_df, aes(x=factor(month)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill = c("#FFCC00", "#3399FF", "#CC0033"), colour="black")+  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + labs(title = "Distribution of transactions by month", y = "Percentage", x = "Month") + coord_flip()


#################################################################################################################################


#Plotting the daily transactions' time series pattern

Trans_daily <- group_by(ANZ_df,date)%>% summarize(count= n())

ggplot(data = Trans_daily, aes(x=date, y = count))+
  geom_line(color = "#00AFBB", size = 1) + labs(title = "Time Series of Daily Transaction Count of ANZ Customers", y = "Daily Transaction Count", x = "Date")

summary(Trans_daily)

#Decomposing the time series pattern into trend + seasonality + noise

Trans_weekly = ts(Trans_daily$count, frequency = 7)
decompose_dailytrans = decompose(Trans_weekly, "additive")

plot(as.ts(decompose_dailytrans$seasonal))
plot(as.ts(decompose_dailytrans$trend))
plot(as.ts(decompose_dailytrans$random))
plot(decompose_dailytrans, type ='l', col = "blue")

#Plotting the weekly transactions' time series pattern

Weekly_trans1 <- colSums(matrix(Trans_daily$count,7))
Week<-seq(1,13,by=1)
Weeklydata1<-data.frame(Weekly_trans1, Week)

ggplot(data = Weeklydata1, aes(x=Week, y = Weekly_trans1))+
  geom_line(color = "#00AFBB", size = 1) + labs(title = "Time Series of Weekly Transaction Count of ANZ Customers", y = "Weekly Transaction Count", x = "Week")


#Plotting the time series of daily mean transaction amount

Trans_dailyamount <- group_by(ANZ_df,date)%>% summarize(dailymean= mean(amount))

ggplot(data = Trans_dailyamount, aes(x=date, y = dailymean))+
  geom_line(color = "#00AFBB", size = 1) + labs(title = "Time Series of Daily Mean Transaction amount of ANZ Customers", y = "Daily Mean Transaction amount", x = "Date")

summary(Trans_dailyamount)

#Decomposing the time series pattern into trend + seasonality + noise

Trans_weeklyamount = ts(Trans_dailyamount$dailymean, frequency = 7)
decompose_dailytransamount = decompose(Trans_weeklyamount, "additive")

plot(as.ts(decompose_dailytransamount$seasonal))
plot(as.ts(decompose_dailytransamount$trend))
plot(as.ts(decompose_dailytransamount$random))
plot(decompose_dailytransamount, type ='l', col = "blue")

#Plotting the weekly mean transactions' amount time series pattern

Weekly_trans2 <- colSums(matrix(Trans_dailyamount$dailymean,7))
Week<-seq(1,13,by=1)
Weeklydata2<-data.frame(Weekly_trans2, Week)

ggplot(data = Weeklydata2, aes(x=Week, y = Weekly_trans2))+
  geom_line(color = "#00AFBB", size = 1) + labs(title = "Time Series of Weekly Mean Transaction Amount of ANZ Customers", y = "Weekly Mean Transaction Amount", x = "Week")


###################################################################################################################################


#Finding the unique locations of the 100 hypothetical customers
ANZ_df_cus <- ANZ_df %>% group_by(customer_id, age, gender, long_lat) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount))

ANZ_df_cus[,c("longitude", "latitude")] <- str_split_fixed(ANZ_df_cus$long_lat, " ", 2)

ANZ_df_cus$longitude <- as.numeric(ANZ_df_cus$longitude)
ANZ_df_cus$latitude <- as.numeric(ANZ_df_cus$latitude)

#Produce the map of Australia
register_google(key = "Enter your Google API key here")

Aus_center = as.numeric(geocode("Australia"))


#Geographic map of Australia showing the number of transactions made by each customer
ggmap(get_googlemap(center=Aus_center, scale=2, zoom=4)) + geom_point(data = ANZ_df_cus,
                                                                      aes(x = longitude, y = latitude, size = count), color = "purple", alpha = 0.5)+labs(x = "Longitude", y="Latitude")


#Geographic map of Australia showing the mean transaction amount per customer
ggmap(get_googlemap(center=Aus_center, scale=2, zoom=4)) + geom_point(data = ANZ_df_cus,
                                                                      aes(x = longitude, y = latitude, size = mean_amount), color = "red", alpha = 0.5)+labs(x = "Longitude", y="Latitude")

#Plotting ANZ Customer locations of Melbourne and Sydney
Melb <- as.numeric(geocode("Melbourne"))
ggmap(get_googlemap(center=Melb, scale=2, zoom=10)) + geom_point(data = ANZ_df_cus, aes(x = longitude, y = latitude), color = "red", size =4)+labs(x = "Longitude", y="Latitude")

Syd <- as.numeric(geocode("Sydney"))
ggmap(get_googlemap(center=Syd, scale=2, zoom=10)) + geom_point(data = ANZ_df_cus, aes(x = longitude, y = latitude), color = "red", size =4)+labs(x = "Longitude", y="Latitude")

                                                                 



