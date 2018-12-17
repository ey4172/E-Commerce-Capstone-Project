# To be run 1st
# memory.limit(3500)
# Loading the required libraries
library("dplyr")
library("tidyr")
library("sqldf")
library("ggplot2")
library("readxl")
library("lubridate")
library("stringr")
library("zoo")
library("data.table")
library("minpack.lm")
library('gridExtra')
library("MASS")
library("car")

# Reading the data files
consumer_data <- read.csv('ConsumerElectronics.csv', header = T, na.strings = c("","NA"))

str(consumer_data)
summary(consumer_data)

# Column "product_analytic_super_category" does not carry any information and dropped
consumer_data <- consumer_data[ ,-15]
# Renaming column
colnames(consumer_data)[1] = c("fsn_id")
colnames(consumer_data)[11] = c("order_payment_type")

# NA values in the dataset
sum(is.na(consumer_data))
# total NA values
sapply(consumer_data,function(x) sum(length(which(is.na(x)))))
# columns gmv, cust_id and pincode contains missing values

# Removing the missing values 
consumer_data = na.omit(consumer_data)

# Finding duplicate entries
sum(duplicated(consumer_data))

# Removing duplicated values
consumer_data = unique(consumer_data)

# column wise analysis
###################################################################################
# fsn id contains no missing values and it signifies the unique identifier of each SKU (stock keeping unit)

# Order_date
class(consumer_data$order_date)
consumer_data$order_date = as.Date(consumer_data$order_date)

# No cleaning required in the year and month columns, they also do not contain any missing values.
# Subsetting out data only between July 2015 and June 2016
consumer_data = consumer_data[consumer_data$order_date >="2015-07-01" & consumer_data$order_date <="2016-06-30",]

consumer_data = consumer_data[which(year(consumer_data$order_date) == consumer_data$Year),]
consumer_data = consumer_data[which(month(consumer_data$order_date) == consumer_data$Month),]

# Creating weeks
consumer_data$week <- isoweek(consumer_data$order_date)

# order_id is the unique identification id for each order generated. 
range(consumer_data$order_id)
# Changing the format of the column.
consumer_data$order_id = format(consumer_data$order_id,scientific = FALSE)

# order_item_id generates two different ids  for the same order id if you order 2 or more products under
# the same id. orders are tracked using this variable. 
range(consumer_data$order_item_id)
consumer_data$order_item_id = format(consumer_data$order_item_id,scientific = FALSE)

# Product MRP; gmv and units cannot be 0
range(consumer_data$product_mrp)
consumer_data = consumer_data[consumer_data$product_mrp > 0 & consumer_data$gmv > 0 & consumer_data$units > 0 ,]

# GMV 
# Subsetting out rows where equation MRP*units >= GMV doesn't hold. Since we can
# offer discounts but cannot charge a higher price 
consumer_data = subset(consumer_data,((product_mrp*units) >= gmv))

# Units
range(consumer_data$units)

consumer_data$deliverybdays <- as.character(consumer_data$deliverybdays)
consumer_data$deliverycdays <- as.character(consumer_data$deliverycdays)

# deliverybdays - business days and deliverycdays - calender days
# removing negative values

consumer_data = consumer_data[(as.numeric(consumer_data$deliverybdays) >= 0) | as.character(consumer_data$deliverybdays) == "\\N" ,]
consumer_data = consumer_data[(as.numeric(consumer_data$deliverycdays) >= 0) | as.character(consumer_data$deliverycdays) == "\\N" ,]

# order payment type
table(consumer_data$order_payment_type)
# no edits required : simply shows how the payment was made by the customer

# SLA
range(consumer_data$sla)
# Rarely SLA for any delivery will be more than 2 months; hence filtering out other value 
consumer_data = consumer_data[consumer_data$sla <= 60,]

# Product procurement sla
range(consumer_data$product_procurement_sla)

# filtering out negative sla values
# also procurement beyound 2 months is not logical; hence filtering out
consumer_data = consumer_data[consumer_data$product_procurement_sla >= 0 & consumer_data$product_procurement_sla <= 60,]

# Customer IDS and Pincodes 
# Converting from scientific notation
consumer_data$cust_id = format(consumer_data$cust_id,scientific = FALSE)
consumer_data$pincode = format(consumer_data$pincode,scientific = FALSE)

# Product analytic categories and super categories for the entire dataset
table(consumer_data$product_analytic_category)
table(consumer_data$product_analytic_sub_category)
table(consumer_data$product_analytic_vertical)

# computing discount
consumer_data$Discount_Percentage <- (((consumer_data$product_mrp*consumer_data$units)-consumer_data$gmv)/(consumer_data$product_mrp*consumer_data$units))*100

# gmv per unit
consumer_data$gmvperunit <- as.numeric(consumer_data$gmv)/consumer_data$units
summary(consumer_data$gmvperunit)

# we are categorising products as mass market and luxury - based on gmv per unit
typetemp <- quantile(consumer_data$gmvperunit, .8)

# removing columns that we will not use
consumer_data = consumer_data [,-c(3:6)]
# removing fsn_id, customer id and pincode
consumer_data = consumer_data [,-c(1,9,10)]

consumer_data$Date <- as.Date(consumer_data$order_date, format = "%Y-%m-%d")

#########################################################################################
TotalGMV <- sum(consumer_data$gmv)
TotalGMV

# Filtering out rows were product sub category is camera accessory, home audio and gaming accessory.
consumer_data = consumer_data[(consumer_data$product_analytic_sub_category == "CameraAccessory") |
                                (consumer_data$product_analytic_sub_category == "GamingAccessory")|
                                (consumer_data$product_analytic_sub_category == "HomeAudio"),]

GMV <- consumer_data %>% group_by(product_analytic_sub_category) %>% summarise(GMV = sum(gmv))

# GMV % of the total; we will use the same to derive the marketing spend
GMV$GMV <- GMV$GMV/TotalGMV
GMV

#########################################################################################

ProductList = read_excel("Media data and other information.xlsx", sheet = 1)
#########################################################################################
#########################################################################################
# Making holiday  information usable 
SSC = read_excel("Media data and other information.xlsx", sheet = 3)
colnames(SSC) <- c("Year", "EventName")

SSC <- SSC %>% fill(Year)

#SSC$EventNameNew <- sapply(strsplit(SSC$EventName, split='(', fixed=TRUE), function(x) (x[1]))
SSC <- separate(SSC, EventName, into = c("EventName", "Day"), sep = "\\(")
SSC$Day <- gsub(")","", SSC$Day)
SSC$Day <- gsub("th","",SSC$Day)
SSC$Day <- gsub("nd","",SSC$Day)
SSC$Day <- gsub("rd","",SSC$Day)

SSC$EventName <- str_trim(SSC$EventName)
SSC$Day <- str_trim(SSC$Day)

#Create Start date and End date
SSC <- separate(SSC, Day, into = c("StartDay", "EndDay"), sep = "-")
SSC$StartDay <- str_trim(SSC$StartDay)
SSC <- separate(SSC, StartDay, into = c("StartDay", "StartMonth"), sep = " ")
SSC$EndDay <- str_trim(SSC$EndDay)
SSC <- separate(SSC, EndDay, into = c("EndDay", "EndMonth"), sep = " ")
SSC$StartMonth <- sapply(strsplit(SSC$StartMonth, split="'", fixed=TRUE), function(x) (x[1]))
SSC$EndMonth <- sapply(strsplit(SSC$EndMonth, split="'", fixed=TRUE), function(x) (x[1]))

SSC$StartMonth <- ifelse(!is.na(SSC$StartMonth), SSC$StartMonth, SSC$EndMonth)

SSC$StartDate <- paste(SSC$StartDay, SSC$StartMonth, SSC$Year, sep="-")
SSC$EndDate <- paste(SSC$EndDay, SSC$EndMonth, SSC$Year, sep="-")

#NewYR and X-Mas
SSC$EndDate <- gsub("3-Jan-2015","3-Jan-2016",SSC$EndDate)
SSC <- SSC[,c("EventName","StartDate","EndDate")]

#Daussera Issue
x <- SSC[SSC$EventName == "Daussera sale", 2]
SSC[SSC$EventName == "Daussera sale", 2] <- SSC[SSC$EventName == "Daussera sale", 3]
SSC[SSC$EventName == "Daussera sale", 3] <- x

#July Issue
SSC$StartDate <- gsub("July","Jul",SSC$StartDate)
SSC$EndDate <- gsub("July","Jul",SSC$EndDate)

SSC$EventName <- str_trim(SSC$EventName)
SSC$StartDate <- str_trim(SSC$StartDate)
SSC$EndDate <- str_trim(SSC$EndDate)

SSC$StartDate <- as.Date(SSC$StartDate, format = "%d-%b-%Y")
SSC$EndDate <- as.Date(SSC$EndDate, format = "%d-%b-%Y")
SSC$NumDays <- as.numeric(as.Date(SSC$EndDate)-as.Date(SSC$StartDate)+1)


SSCNew <- data.frame(EventName=character(),
                     EventDate=as.Date(character()),
                     stringsAsFactors=FALSE)
class(SSCNew$EventDate) <- "Date"

for (i in 1:nrow(SSC))
{
  for (j in 1:SSC$NumDays[i])
  {
    EventName <- SSC$EventName[i]
    EventDate <- SSC$StartDate[i]  + j - 1
    SSCNew <- rbind(SSCNew,cbind(EventName,EventDate))
  }
}

SSCNew$EventDate <- as.numeric(levels(SSCNew$EventDate))[SSCNew$EventDate]
class(SSCNew$EventDate) <- "Date"

rm(SSC)

consumer_data <- merge(x = consumer_data, y = SSCNew, by.x = "order_date", by.y = "EventDate", all.x = TRUE)

#########################################################################################
#########################################################################################
# Making NPS Scores usable 

MonthlyNPSScore = read_excel("Media data and other information.xlsx", sheet = 4,col_names = FALSE, skip = 1)
MonthlyNPSScore <- t(MonthlyNPSScore)

MonthlyNPSScore = as.data.frame(MonthlyNPSScore[-1,])
colnames(MonthlyNPSScore) <- c("Date", "NPS")
MonthlyNPSScore$Date <- paste("01", MonthlyNPSScore$Date, sep = "'")
MonthlyNPSScore$Date <- gsub("'", "-", MonthlyNPSScore$Date)

#Month Name Issue
MonthlyNPSScore$Date <- gsub("June","Jun",MonthlyNPSScore$Date)
MonthlyNPSScore$Date <- gsub("July","Jul",MonthlyNPSScore$Date)
MonthlyNPSScore$Date <- gsub("Sept","Sep",MonthlyNPSScore$Date)
MonthlyNPSScore$Date <- as.Date(MonthlyNPSScore$Date, format = "%d-%b-%y")

str(MonthlyNPSScore)
MonthlyNPSScore$NPS <- as.numeric(MonthlyNPSScore$NPS)

MonthlyNPSScore$x <- days_in_month(MonthlyNPSScore$Date)
MonthlyNPSScore$NPSScore <- MonthlyNPSScore$NPS/MonthlyNPSScore$x
MonthlyNPSScore <- MonthlyNPSScore[ ,-c(2,3)]
consumer_data <- merge(x = consumer_data, y = MonthlyNPSScore, by = "Date", all.x = TRUE)
consumer_data <- consumer_data %>% arrange(by=consumer_data$order_date)%>%fill(NPSScore)


#########################################################################################
#########################################################################################
# Making media spent information usable
MediaInvestment = read_excel("Media data and other information.xlsx", sheet = 2)

colnames(MediaInvestment) = MediaInvestment[1, ] # the first row will be the header
MediaInvestment = MediaInvestment[-1, ]        
MediaInvestment$Date <- paste(MediaInvestment$Year, MediaInvestment$Month, "01", sep ="-")
MediaInvestment$Date <- as.Date(MediaInvestment$Date, "%Y-%m-%d")
MediaInvestment$x <- days_in_month(MediaInvestment$Date)
MediaInvestment <- MediaInvestment[,c(-1,-2)]
colname_media <- colnames(MediaInvestment)
colname_media <- colname_media[c(1:(length(colname_media)-1))]

for (j in 1:nrow(MediaInvestment))
{
  for(i in 1:(ncol(MediaInvestment)-2))
  {
    MediaInvestment[j,i] <- as.double((MediaInvestment[j,i]))/(as.double(MediaInvestment$x[j]))
  }
}

MediaInvestment <- MediaInvestment[,-ncol(MediaInvestment)]

# Merging the data
consumer_data <- merge(x = consumer_data, y = MediaInvestment, by = "Date", all.x = TRUE)
consumer_data <- consumer_data %>% arrange(by=consumer_data$order_date)%>%fill(colname_media)

#########################################################################################
#########################################################################################

max_week <- max(consumer_data$week)
consumer_data[consumer_data$Date > "2016-01-03",c("week")] <- consumer_data[consumer_data$Date > "2016-01-03",c("week")] + max_week

range(consumer_data$week)

consumer_data$ItemType <- ifelse(consumer_data$gmvperunit > typetemp, "Luxury", "MassMarket")

Data_CameraAccessory = consumer_data[(consumer_data$product_analytic_sub_category == "CameraAccessory"),]
Data_GamingAccessory = consumer_data[(consumer_data$product_analytic_sub_category == "GamingAccessory"),]
Data_HomeAudio = consumer_data[(consumer_data$product_analytic_sub_category == "HomeAudio"),]

#rm(consumer_data)
# getting the media spent proportional to category specific gmv
Data_CameraAccessory[,19:28] <- lapply(Data_CameraAccessory[,19:28],as.numeric)
Data_CameraAccessory[,19:28] <- lapply(Data_CameraAccessory[,19:28]*as.numeric(GMV[(GMV$product_analytic_sub_category == "CameraAccessory"),2]), as.numeric)

write.csv(Data_CameraAccessory,'Data_CameraAccessory.csv')

Data_GamingAccessory[,19:28] <- lapply(Data_GamingAccessory[,19:28],as.numeric)
Data_GamingAccessory[,19:28] <- lapply(Data_GamingAccessory[,19:28]*as.numeric(GMV[(GMV$product_analytic_sub_category == "GamingAccessory"),2]), as.numeric)

write.csv(Data_GamingAccessory,'Data_GamingAccessory.csv')

Data_HomeAudio[,19:28] <- lapply(Data_HomeAudio[,19:28],as.numeric)
Data_HomeAudio[,19:28] <- lapply(Data_HomeAudio[,19:28]*as.numeric(GMV[(GMV$product_analytic_sub_category == "HomeAudio"),2]), as.numeric)

write.csv(Data_HomeAudio,'Data_HomeAudio.csv')
rm(list = ls())
#########################################################################################
#########################################################################################
#EDA
#########################################################################################
#########################################################################################
# Reading the required files
Data_CameraAccessory = read.csv('Data_CameraAccessory.csv')
Data_GamingAccessory = read.csv('Data_GamingAccessory.csv')
Data_HomeAudio = read.csv('Data_HomeAudio.csv')

consumer_data_filtered <- rbind(Data_CameraAccessory,Data_GamingAccessory)
consumer_data_filtered <- rbind(consumer_data_filtered,Data_HomeAudio)

colnames(consumer_data_filtered)
consumer_data_filtered$Year = year(consumer_data_filtered$Date)

# GMV
range(consumer_data_filtered$gmv)

boxplot(consumer_data_filtered$gmv)
quantile(consumer_data_filtered$gmv,seq = c(0,1,0.010))

#Units
range(consumer_data_filtered$units)
boxplot(consumer_data_filtered$units)
quantile(consumer_data_filtered$units,seq = c(0,1,0.10))

#deliverybdays, sla and deliverycdays
range(as.numeric(consumer_data_filtered$deliverybdays))
boxplot(as.numeric(consumer_data_filtered$deliverybdays))
quantile(as.numeric(consumer_data_filtered$deliverybdays,seq = c(0,1,0.10)))

range(as.numeric(consumer_data_filtered$deliverycdays))
boxplot(as.numeric(consumer_data_filtered$deliverycdays))
quantile(as.numeric(consumer_data_filtered$deliverycdays,seq = c(0,1,0.10)))

range(consumer_data_filtered$sla)
boxplot(consumer_data_filtered$sla)
quantile(consumer_data_filtered$sla)

# Orderpayment type
table(consumer_data_filtered$order_payment_type)

# Product Analytic Category
table(consumer_data_filtered$product_analytic_category)

# Product Analytic Sub Category
table(consumer_data_filtered$product_analytic_sub_category)

# Product MRP:
range(consumer_data_filtered$product_mrp)
boxplot(consumer_data_filtered$product_mrp)
quantile(consumer_data_filtered$product_mrp)

# Discount Percentage
range(consumer_data_filtered$Discount_Percentage)
boxplot(consumer_data_filtered$Discount_Percentage)
quantile(consumer_data_filtered$Discount_Percentage)

# NPS Scores:
range(consumer_data_filtered$NPSScore)
boxplot(consumer_data_filtered$NPSScore)

# For media investment file, analysis done in Excel by using formatting to 
# identify months where total investment is the highest and in which months was the spending
# maximum in which mode of channel.

# Item Type:
table(consumer_data_filtered$ItemType)
###########################################################################################
###########################################################################################

# Plots done in Tableau

# Variation of GMV and Total Investment Spent across months dual axis plot created in Tableau.
# Variation of GMV by week 
# Number of Units broken down by the type of Product Categories for different months/ weeks of the years considered
# Bubble chart showing the popular products sold for each category sub type
# Tree plot showing the product verticals with the highest GMV 


# How is the distribution of SLA for different product sub categories
b1 = ggplot(consumer_data_filtered,aes(x = product_analytic_sub_category, y = sla , col = product_analytic_sub_category))+geom_boxplot()
b1
mean(consumer_data_filtered$sla)


# Distribution of GMV for each product category
b2 = ggplot(consumer_data_filtered,aes(x =  product_analytic_sub_category, y = gmv , col = product_analytic_sub_category))+geom_boxplot()
b2

# Mean GMV values for all three sub categories remained the same. We observe a lot of outliers in the 
# Camera Accessory category. 

# Mean discount percentage every week for the 2 years
d1 = consumer_data_filtered%>%
  filter(product_analytic_sub_category == "CameraAccessory")%>%
  group_by(week)%>%
  summarise(mean_discount_ca = mean(Discount_Percentage))
d1$product_analytic_subtype = " CameraAccessory"

d2 = consumer_data_filtered%>%
  filter(product_analytic_sub_category == "GamingAccessory")%>%
  group_by(week)%>%
  summarise(mean_discount_ga = mean(Discount_Percentage))
d2$product_analytic_subtype = " GamingAccessory"

d3 = consumer_data_filtered%>%
  filter(product_analytic_sub_category == "HomeAudio")%>%
  group_by(week)%>%
  summarise(mean_discount_ha = mean(Discount_Percentage))
d3$product_analytic_subtype = "HomeAusio"

b3 = ggplot(d1,aes(x = week , y = mean_discount_ca))+geom_line()
b3 + ggtitle("Mean Discount offered by week for Camera Accessories")
# Maximum discount offered in week 33

b4 = ggplot(d2,aes(x = week , y = mean_discount_ga))+geom_line()
b4 + ggtitle("Mean Discount offered by week for Gaming Accessories")
# Maximum discount offered in the week 32, 42, 52, 60 and so on

b5 = ggplot(d3,aes(x = week , y = mean_discount_ha))+geom_line()
b5 + ggtitle("Mean Discount offered by week for Home Audio")
# Reverse trend observed for Home Audio Sub category, lowest discount in the week 32 , highest in 50, 54

# Distribution of discounts offered on different events
e = consumer_data_filtered[!is.na(consumer_data_filtered$EventName),]
b6 = ggplot(e, aes(x = EventName , y = Discount_Percentage , fill = EventName)) +geom_boxplot()
b6 + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle('Distribution of Discount Percentage by Event Name')

# Comparision of  mean gmv for normal days vs days on which there are holiday promotions
consumer_data_filtered$holiday = ifelse(is.na(consumer_data_filtered$EventName) == TRUE,0,1)
aggregate(consumer_data_filtered$gmv,by = list(consumer_data_filtered$holiday), FUN = "mean")
# The average gmv value is higher on the day when holiday promotions are there.

# Distribution of discount percentage offered on different types of goods
b7 = ggplot(consumer_data_filtered,aes(x = ItemType, y = Discount_Percentage, fill = ItemType))+geom_boxplot()
b7
# Mass market goods are sold at a higher discounts

aggregate(consumer_data_filtered$gmv,by = list(consumer_data_filtered$ItemType), FUN = "median")
# The median gmv value for Mass Market goods > the one for Luxury Items.

# Consumer sentiment score 
b8 = ggplot(consumer_data_filtered, aes( x = week, y= NPSScore))+geom_line()
b8 + ggtitle('Consumer Sentiment Score by week')

rm(list=ls())


#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

