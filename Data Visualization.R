# To be run after the 1st file - given separately
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
# Reading the required files
Data_CameraAccessory = read.csv('Data_CameraAccessory.csv')
Data_GamingAccessory = read.csv('Data_GamingAccessory.csv')
Data_HomeAudio = read.csv('Data_HomeAudio.csv')

consumer_data_filtered <- rbind(Data_CameraAccessory,Data_GamingAccessory)
consumer_data_filtered <- rbind(consumer_data_filtered,Data_HomeAudio)

colnames(consumer_data_filtered)


myData <- consumer_data_filtered
myData$EventName <- as.character(myData$EventName)
myData[is.na(myData)] <- 0

#Creating Dummies for Categorical Variables 
mydummy <- data.frame(model.matrix(~order_payment_type, data=myData))
myData <- cbind(myData[,], mydummy)
myData <- myData [,-31] 

mydummy <- data.frame(model.matrix(~ItemType, data=myData))
myData <- cbind(myData[,], mydummy)
myData <- myData [,-32] 

myData$EventName <- ifelse(myData$EventName == "0", "0", "1")

mydummy <- data.frame(model.matrix(~EventName, data=myData))
myData <- cbind(myData[,], mydummy)
myData <- myData [,-33] 

########################################################################################
# Renaming column
colnames(myData)[20] = c("TotalInvestment")
colnames(myData)[24] = c("ContentMarketing")
colnames(myData)[25] = c("OnlineMarketing")

CameraAccessory_Grouped <- sqldf("SELECT Week, sum(gmv) as gmv, sum(units), avg(product_mrp), avg(gmvperunit) as Sold_price, avg(NPSScore) as NPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as TV, sum(Digital) as Digital, sum(Sponsorship) as Sponsorship, sum(ContentMarketing) as ContentMarketing, sum(OnlineMarketing) as OnlineMarketing, sum(Affiliates) as Affiliates, sum(SEM) as SEM, sum(Radio) as Radio, sum(Other) as Other, sum(order_payment_typePrepaid) as order_payment_typePrepaid, sum(ItemTypeMassMarket) as ItemTypeMassMarket, sum(EventName1) as EventName FROM myData WHERE product_analytic_sub_category = 'CameraAccessory' GROUP BY Week ORDER BY Week ASC")
GamingAccessory_Grouped <- sqldf("SELECT Week, sum(gmv) as gmv, sum(units), avg(product_mrp), avg(gmvperunit) as Sold_price, avg(NPSScore) as NPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as TV, sum(Digital) as Digital, sum(Sponsorship) as Sponsorship, sum(ContentMarketing) as ContentMarketing, sum(OnlineMarketing) as OnlineMarketing, sum(Affiliates) as Affiliates, sum(SEM) as SEM, sum(Radio) as Radio, sum(Other) as Other, sum(order_payment_typePrepaid) as order_payment_typePrepaid, sum(ItemTypeMassMarket) as ItemTypeMassMarket, sum(EventName1) as EventName FROM myData WHERE product_analytic_sub_category = 'GamingAccessory' GROUP BY Week ORDER BY Week ASC")
HomeAudio_Grouped <- sqldf("SELECT Week, sum(gmv) as gmv, sum(units), avg(product_mrp), avg(gmvperunit) as Sold_price, avg(NPSScore) as NPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as TV, sum(Digital) as Digital, sum(Sponsorship) as Sponsorship, sum(ContentMarketing) as ContentMarketing, sum(OnlineMarketing) as OnlineMarketing, sum(Affiliates) as Affiliates, sum(SEM) as SEM, sum(Radio) as Radio, sum(Other) as Other, sum(order_payment_typePrepaid) as order_payment_typePrepaid, sum(ItemTypeMassMarket) as ItemTypeMassMarket, sum(EventName1) as EventName FROM myData WHERE product_analytic_sub_category = 'HomeAudio' GROUP BY Week ORDER BY Week ASC")

########################################################################################
# Creating AdStock for individual Advertising Channel
########################################################################################

#adstock<-function(x,rate=0){
#  return(as.numeric(stats::filter(x,filter=rate,method="recursive")))
#}

#AdstockRate<-function(Data,Impact,Ads){
#  modFit<-nls(data=Data,Impact ~a+b*adstock(Ads,rate),
#              start=c(a=1,b=1,rate=0))
#  if(summary(modFit)$coefficients[3,1]>0){
#    AdstockRate=summary(modfit)$coefficients[3,1]
#  }
#  else{
#    library(minpack.lm)
#    nls.out<-nlsLM(Impact~ a+b*adstock(Ads,rate),data=Data,start=list(a=1,b=1,rate=0),
#                   lower=c(a=-Inf,b=-Inf,rate=0),upper=c(a=Inf,b=Inf,rate=1)) 
#    AdstockRate=summary(nls.out)$coefficients[3,1]
#  }
#  return(AdstockRate)
#}

#AdstockRate(CameraAccessory_Grouped,CameraAccessory_Grouped$gmv,CameraAccessory_Grouped$TotalInvestment)
#AdstockRate(GamingAccessory_Grouped,GamingAccessory_Grouped$gmv,GamingAccessory_Grouped$TotalInvestment)
#AdstockRate(HomeAudio_Grouped,HomeAudio_Grouped$gmv,HomeAudio_Grouped$TotalInvestment)
########################################################################################

# Assigning Adstock Rates for each Adv channel 
adstock_rate <- c(0.4, 0.4, 0.3, 0.5, 0.5, 0.1, 0.2, 0.2, 0.1) # we have considered these rates based on secondary research 
# These rates are use for EDA only

# Adstock function
adstock<-function(x,rate){
  return(as.numeric(stats::filter(x,filter=rate,method="recursive")))
}

## Camera Accessory

Adv_channels <- CameraAccessory_Grouped[,8:16]
AdvChannel_AdStock <- Adv_channels

CameraAccessory_Grouped <- CameraAccessory_Grouped[,-c(8:16)]
# Calucalte Adstock for all Adv channels
for (i in 1:ncol(Adv_channels)) {
  AdvChannel_AdStock[,i] <- as.numeric(stats::filter(as.list(Adv_channels[,i]), filter= adstock_rate[i], method="recursive"))
}

# Add adstock columns in the final dataset for Linear Regression
CameraAccessory_Grouped <- cbind(CameraAccessory_Grouped, AdvChannel_AdStock)

## Gaming Accessory

Adv_channels <- GamingAccessory_Grouped[,8:16]
AdvChannel_AdStock <- Adv_channels

GamingAccessory_Grouped <- GamingAccessory_Grouped[,-c(8:16)]
# Calucalte Adstock for all Adv channels
for (i in 1:ncol(Adv_channels)) {
  AdvChannel_AdStock[,i] <- as.numeric(stats::filter(as.list(Adv_channels[,i]), filter= adstock_rate[i], method="recursive"))
}

# Add adstock columns in the final dataset for Linear Regression
GamingAccessory_Grouped <- cbind(GamingAccessory_Grouped, AdvChannel_AdStock)

## Home Audio Accessory

Adv_channels <- HomeAudio_Grouped[,8:16]
AdvChannel_AdStock <- Adv_channels

HomeAudio_Grouped <- HomeAudio_Grouped[,-c(8:16)]
# Calucalte Adstock for all Adv channels
for (i in 1:ncol(Adv_channels)) {
  AdvChannel_AdStock[,i] <- as.numeric(stats::filter(as.list(Adv_channels[,i]), filter= adstock_rate[i], method="recursive"))
}

# Add adstock columns in the final dataset for Linear Regression
HomeAudio_Grouped <- cbind(HomeAudio_Grouped, AdvChannel_AdStock)

########################################################################################
########################################################################################
# EDA
# Loading the datasets.
ca =  CameraAccessory_Grouped
ga =  GamingAccessory_Grouped 
ha =  HomeAudio_Grouped 

adstock_data = rbind(ca,ga)
adstock_data = rbind(adstock_data,ha)
rm(ca)
rm(ga)
rm(ha)
write.csv(adstock_data,'adstock_data.csv')

# Analyzing GMV against each channel adstock
g1 = ggplot(adstock_data,aes(x = TV , y = gmv ))+ geom_point()
g1 + geom_smooth(method = "lm", formula = y ~ x) + xlab('TV Adstock')+ggtitle('GMV against TV Adstock')

g2 = ggplot(adstock_data,aes(x = Digital , y = gmv ))+ geom_point()
g2 + xlab('Digital Adstock')+ggtitle('GMV against Digital Adstock')

g3 = ggplot(adstock_data,aes(x = Sponsorship , y = gmv ))+ geom_point()
g3 + xlab('Sponsorship Adstock')+ggtitle('GMV against Sponsorship Adstock')

g4 = ggplot(adstock_data,aes(x = ContentMarketing , y = gmv ))+ geom_point()
g4 + xlab('Content Marketing Adstock')+ggtitle('GMV against Content Marketing Adstock')

g5 = ggplot(adstock_data,aes(x = OnlineMarketing , y = gmv ))+ geom_point()
g5 + xlab('Online Marketing Adstock')+ggtitle('GMV against Online Marketing Adstock')+geom_smooth(method = "lm", formula = y ~ x)

g6 = ggplot(adstock_data,aes(x = Affiliates , y = gmv ))+ geom_point()
g6 + xlab('Affiliates Adstock')+ggtitle('GMV against Affiliates Adstock')+geom_smooth(method = "lm", formula = y ~ x)

g7 = ggplot(adstock_data,aes(x = SEM , y = gmv ))+ geom_point()
g7 + xlab('SEM Adstock')+ggtitle('GMV against SEM Adstock')

g8 = ggplot(adstock_data,aes(x = Radio , y = gmv ))+ geom_point()
g8 + xlab('Radio Adstock')+ggtitle('GMV against Radio Adstock')+geom_smooth(method = "lm", formula = y ~ x)

g9 = ggplot(adstock_data,aes(x = Other , y = gmv ))+ geom_point()
g9 + xlab('Other Adstock')+ggtitle('GMV against Other Adstock')+geom_smooth(method = "lm", formula = y ~ x)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol= 3)

rm(list=ls())
########################################################################################
########################################################################################
