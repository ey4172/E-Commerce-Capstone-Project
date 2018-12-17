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
library("Boruta")
library("bayesm")
library("GGally")
library("AID")
library("caret")

#########################################################################################
# Reading the data files
#########################################################################################

Data_CameraAccessory = read.csv('Data_CameraAccessory.csv')
Data_GamingAccessory = read.csv('Data_GamingAccessory.csv')
Data_HomeAudio = read.csv('Data_HomeAudio.csv')

myData <- rbind(Data_CameraAccessory,Data_GamingAccessory)
myData <- rbind(myData,Data_HomeAudio)

colnames(myData)

# Renaming column
colnames(myData)[20] = c("TotalInvestment")
colnames(myData)[24] = c("ContentMarketing")
colnames(myData)[25] = c("OnlineMarketing")

# converting spent into crores
myData[20:29] <- myData[20:29]*10000000

#########################################################################################
# Creating dummy variables
#########################################################################################


myData$EventName <- as.character(myData$EventName)
myData[is.na(myData)] <- 0

#Creating Dummies for Categorical Variables 
myData <- cbind(myData[,], data.frame(model.matrix(~order_payment_type, data=myData))[,-1])
myData <- myData[ , -which(names(myData) %in% 'order_payment_type')]
colnames(myData)[ncol(myData)] <- 'order_payment_typePrepaid'

myData <- cbind(myData[,], data.frame(model.matrix(~ItemType, data=myData))[,-1])
myData <- myData[ , -which(names(myData) %in% 'ItemType')]
colnames(myData)[ncol(myData)] <- 'ItemTypeMassMarket'

myData$EventName <- ifelse(myData$EventName == "0", "0", "1")
myData <- cbind(myData[,], data.frame(model.matrix(~EventName, data=myData))[,-1])
myData <- myData[ , -which(names(myData) %in% 'EventName')]
colnames(myData)[ncol(myData)] <- 'EventName1'

#########################################################################################
# Creating 3 category-wise data frames; changing col order
#########################################################################################
CameraAccessory_Grouped <- sqldf("SELECT Week, product_analytic_vertical, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'CameraAccessory' GROUP BY Week, product_analytic_vertical ORDER BY Week ASC")
gmv <- CameraAccessory_Grouped[,3]
Ads <- CameraAccessory_Grouped[,c(10:19)]
Price <- CameraAccessory_Grouped[,c(6,8)]
Promotion <- CameraAccessory_Grouped[,c(7)]
Product <- CameraAccessory_Grouped[,c(2,4,5)]

Product <- cbind(Product[,], data.frame(model.matrix(~product_analytic_vertical, data=Product))[,-1])
Product <- Product[ , -which(names(Product) %in% 'product_analytic_vertical')]

Others <- CameraAccessory_Grouped[,c(1,9,20,21,22)]
CameraAccessory_Grouped <- cbind(gmv, Ads, Price, Promotion, Product, Others)

GamingAccessory_Grouped <- sqldf("SELECT Week, product_analytic_vertical, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'GamingAccessory' GROUP BY Week, product_analytic_vertical ORDER BY Week ASC")
gmv <- GamingAccessory_Grouped[,3]
Ads <- GamingAccessory_Grouped[,c(10:19)]
Price <- GamingAccessory_Grouped[,c(6,8)]
Promotion <- GamingAccessory_Grouped[,c(7)]

Product <- GamingAccessory_Grouped[,c(2,4,5)]
Product <- cbind(Product[,], data.frame(model.matrix(~product_analytic_vertical, data=Product))[,-1])
Product <- Product[ , -which(names(Product) %in% 'product_analytic_vertical')]

Others <- GamingAccessory_Grouped[,c(1,9,20,21,22)]
GamingAccessory_Grouped <- cbind(gmv, Ads, Price, Promotion, Product, Others)

HomeAudio_Grouped <- sqldf("SELECT Week, product_analytic_vertical, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'HomeAudio' GROUP BY Week, product_analytic_vertical ORDER BY Week ASC")
gmv <- HomeAudio_Grouped[,3]
Ads <- HomeAudio_Grouped[,c(10:19)]
Price <- HomeAudio_Grouped[,c(6,8)]
Promotion <- HomeAudio_Grouped[,7]

Product <- HomeAudio_Grouped[,c(2,4,5)]
Product <- cbind(Product[,], data.frame(model.matrix(~product_analytic_vertical, data=Product))[,-1])
Product <- Product[ , -which(names(Product) %in% 'product_analytic_vertical')]

Others <- HomeAudio_Grouped[,c(1,9,20,21,22)]
HomeAudio_Grouped <- cbind(gmv, Ads, Price, Promotion, Product, Others)

rm(gmv, Ads, Price, Promotion, Product, Others)


# Removing All-zero columns, if any
CameraAccessory_Grouped <- CameraAccessory_Grouped[, (which(colSums(CameraAccessory_Grouped) != 0))]
GamingAccessory_Grouped <- GamingAccessory_Grouped[, (which(colSums(GamingAccessory_Grouped) != 0))]
HomeAudio_Grouped <- HomeAudio_Grouped[, (which(colSums(HomeAudio_Grouped) != 0))]

########################################################################################
########################################################################################
# We are going to build  the following 5 models for each of the 3 categoris
# 1. Basic linear 
# 2. Multiplicative
# 3. Koyck
# 4. Distributed lag additive
# 5. Distributed lag multiplicative
# 6. Hierarchical (for reference only)
########################################################################################
########################################################################################

########################################################################################
########################################################################################
# 1. Basic Linear Model
########################################################################################
########################################################################################

########################################################################################
# 1a. Basic Linear Model - Camera Accessory 
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped), 0.7*nrow(CameraAccessory_Grouped))

train_camAcces <- CameraAccessory_Grouped[indices,]
test_camAcces <- CameraAccessory_Grouped[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

#----------------------------------------------------------------------------------------
# Run the step object
step
#----------------------------------------------------------------------------------------
model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                totalSEM + totalRadio + totalOther + Promotion + totalunits + 
                avgsla + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + totalorder_payment_typePrepaid + 
                totalItemTypeMassMarket, data = train_camAcces)
summary(model_2)
vif(model_2)

# Removing totalAffiliates for high vif
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalSEM + totalRadio + totalOther + Promotion + totalunits + 
                avgsla + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + totalorder_payment_typePrepaid + 
                totalItemTypeMassMarket, data = train_camAcces)
summary(model_3)
vif(model_3)

# Removing totalSEM for high vif
model_4 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalRadio + totalOther + Promotion + totalunits + 
                avgsla + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + totalorder_payment_typePrepaid + 
                totalItemTypeMassMarket, data = train_camAcces)
summary(model_4)
vif(model_4)

# Removing totalunits for high vif
model_5 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + totalRadio + 
                totalOther + Promotion + avgsla + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFlash + product_analytic_verticalLens + 
                product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_camAcces)
summary(model_5)
vif(model_5)

# Removing TotalInvestment for high vif
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + totalRadio + 
                totalOther + Promotion + avgsla + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFlash + product_analytic_verticalLens + 
                product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_camAcces)
summary(model_6)
vif(model_6)

# Removing totalRadio for high vif
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing +
                totalOther + Promotion + avgsla + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFlash + product_analytic_verticalLens + 
                product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_camAcces)
summary(model_7)
vif(model_7)

# Removing totalContentMarketing for high vif
model_8 <- lm(formula = gmv ~ totalTV + totalDigital + totalOnlineMarketing + totalOther + Promotion + avgsla + 
                product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + totalorder_payment_typePrepaid + 
                totalItemTypeMassMarket, data = train_camAcces)
summary(model_8)
vif(model_8)

# Removing totalItemTypeMassMarket  for high vif
model_9 <- lm(formula = gmv ~ totalTV + totalDigital + totalOnlineMarketing + totalOther + Promotion + avgsla + 
                product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_9)
vif(model_9)

# All feature vif in reasonable range eliminating the Multi-Collinearity
# Handling the p-val feature
# Removing Promotion
model_10 <- lm(formula = gmv ~ totalTV + totalDigital + totalOnlineMarketing + totalOther + avgsla + 
                 product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                 product_analytic_verticalLens + product_analytic_verticalStrap + 
                 product_analytic_verticalTelescope + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_10)
vif(model_10)

# Removing avgsla
model_11 <- lm(formula = gmv ~ totalTV + totalDigital + totalOnlineMarketing + totalOther + 
                 product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                 product_analytic_verticalLens + product_analytic_verticalStrap + 
                 product_analytic_verticalTelescope + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_11)
vif(model_11)


# Removing product_analytic_verticalCameraBattery
model_12 <- lm(formula = gmv ~ totalTV + totalDigital + totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraHousing + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFlash + 
                 product_analytic_verticalLens + product_analytic_verticalStrap + 
                 product_analytic_verticalTelescope + totalorder_payment_typePrepaid, 
               data = train_camAcces)
summary(model_12)
vif(model_12)

# Removing totalTV
model_13 <- lm(formula = gmv ~ totalDigital + totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                 product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_13)
vif(model_13)

# Removing totalDigital
model_14 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                 product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_14)
vif(model_14)

# Removing product_analytic_verticalCameraHousing
model_15 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + 
                 product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_15)
vif(model_15)

# Removing product_analytic_verticalCameraMicrophone
model_16 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_16)
vif(model_16)

# Removing product_analytic_verticalTelescope
model_17 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_17)
vif(model_17)

# Removing product_analytic_verticalCameraMount
model_18 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_18)
vif(model_18)

# Removing product_analytic_verticalCameraTripod
model_19 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraRemoteControl + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_19)
vif(model_19)


# Removing product_analytic_verticalCameraBatteryCharger
model_20 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraRemoteControl + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_20)
vif(model_20)

# Removing product_analytic_verticalExtensionTube
model_21 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraRemoteControl +  
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_21)
vif(model_21)

# Removing product_analytic_verticalCameraEyeCup
model_22 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraRemoteControl +  
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_22)
vif(model_22)

# Removing product_analytic_verticalStrap
model_23 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraRemoteControl +  
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_23)
vif(model_23)

# Removing product_analytic_verticalCameraAccessory
model_24 <- lm(formula = gmv ~ totalOnlineMarketing + 
                 totalOther + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraRemoteControl +  
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_24)
vif(model_24)

# Removing product_analytic_verticalFlash
model_25 <- lm(formula = gmv ~ totalOnlineMarketing + totalOther + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraRemoteControl +  
                 product_analytic_verticalLens + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_25)
vif(model_25)

# Removing product_analytic_verticalCameraRemoteControl
model_26 <- lm(formula = gmv ~ totalOnlineMarketing + totalOther + product_analytic_verticalCameraBag + 
                 product_analytic_verticalLens + totalorder_payment_typePrepaid, data = train_camAcces)
summary(model_26)
vif(model_26)

# All P-val & vif are in range
## Linear Regression Model for Camera Accessory gmv
Final_CameraAcc_model <- model_26

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv, test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.8266455

########################################################################################
# 1b. Basic Linear Model - Game Accessory 
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped), 0.7*nrow(GamingAccessory_Grouped))

train_gameAcces <- GamingAccessory_Grouped[indices,]
test_gameAcces <- GamingAccessory_Grouped[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

#----------------------------------------------------------------------------------------
# Run the step object
step
#----------------------------------------------------------------------------------------

model_2 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                totalSEM + totalRadio + avgmrp + Sold_price + Promotion + 
                totalunits + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_2)
vif(model_2)

# Removing totalOnlineMarketing for high vif
model_3 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalSponsorship + 
                totalContentMarketing + totalAffiliates + 
                totalSEM + totalRadio + avgmrp + Sold_price + Promotion + 
                totalunits + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_3)
vif(model_3)

# Removing totalSEM for high vif
model_4 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalSponsorship + 
                totalContentMarketing + totalAffiliates + 
                totalRadio + avgmrp + Sold_price + Promotion + 
                totalunits + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_4)
vif(model_4)

# Removing totalunits for high vif values
model_5 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalSponsorship + 
                totalContentMarketing + totalAffiliates + 
                totalRadio + avgmrp + Sold_price + Promotion + 
                avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_5)
vif(model_5)

# Removing TotalInvestment
model_6 <- lm(formula = gmv ~ totalDigital + totalSponsorship + 
                totalContentMarketing + totalAffiliates + totalRadio + avgmrp + 
                Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_6)
vif(model_6)

# Removing totalContentMarketing
model_7 <- lm(formula = gmv ~ totalDigital + totalSponsorship + 
                totalAffiliates + totalRadio + avgmrp + 
                Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalItemTypeMassMarket + totalEvent + totalOther, 
              data = train_gameAcces)
summary(model_7)
vif(model_7)

# Removing totalItemTypeMassMarket
model_8 <- lm(formula = gmv ~ totalDigital + totalSponsorship + 
                totalAffiliates + totalRadio + avgmrp + 
                Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalEvent + totalOther, data = train_gameAcces)
summary(model_8)
vif(model_8)


# Removing totalSponsorship
model_9 <- lm(formula = gmv ~ totalDigital + totalAffiliates + totalRadio + avgmrp + 
                Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                week + totalEvent + totalOther, data = train_gameAcces)
summary(model_9)
vif(model_9)

# Removing totalOther
model_10 <- lm(formula = gmv ~ totalDigital + totalAffiliates + totalRadio + avgmrp + 
                 Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 week + totalEvent, data = train_gameAcces)
summary(model_10)
vif(model_10)

# All feature vif are in reasonable range (<10)
# Handling the high p-value feature
# Removing product_analytic_verticalGamingMousePad
model_11 <- lm(formula = gmv ~ totalDigital + totalAffiliates + totalRadio + avgmrp + 
                 Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week + totalEvent, data = train_gameAcces)
summary(model_11)
vif(model_11)

# Removing totalDigital
model_12 <- lm(formula = gmv ~ totalAffiliates + totalRadio + avgmrp + 
                 Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week + totalEvent, data = train_gameAcces)
summary(model_12)
vif(model_12)

# Removing totalEvent
model_13 <- lm(formula = gmv ~ totalAffiliates + totalRadio + avgmrp + 
                 Sold_price + Promotion + avgsla + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_13)
vif(model_13)

# Removing avgsla
model_14 <- lm(formula = gmv ~ totalAffiliates + totalRadio + avgmrp + 
                 Sold_price + Promotion + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_14)
vif(model_14)

# Removing totalRadio
model_15 <- lm(formula = gmv ~ totalAffiliates + avgmrp + 
                 Sold_price + Promotion + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_15)
vif(model_15)

# Removing product_analytic_verticalGamingAdapter
model_16 <- lm(formula = gmv ~ totalAffiliates + avgmrp + 
                 Sold_price + Promotion + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit +
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_16)
vif(model_16)

# Removing product_analytic_verticalGamingMemoryCard
model_17 <- lm(formula = gmv ~ totalAffiliates + avgmrp + 
                 Sold_price + Promotion + product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingAccessoryKit +
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + 
                 product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_17)
vif(model_17)

# Removing product_analytic_verticalGamingAccessoryKit
model_18 <- lm(formula = gmv ~ totalAffiliates + avgmrp + Sold_price + Promotion + 
                 product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + product_analytic_verticalJoystickGamingWheel + 
                 week, data = train_gameAcces)
summary(model_18)
vif(model_18)

# Removing product_analytic_verticalJoystickGamingWheel
model_19 <- lm(formula = gmv ~ totalAffiliates + avgmrp + Sold_price + Promotion + 
                 product_analytic_verticalGamePad + 
                 product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + week, data = train_gameAcces)
summary(model_19)
vif(model_19)

# Removing product_analytic_verticalGamingHeadset
model_20 <- lm(formula = gmv ~ totalAffiliates + avgmrp + Sold_price + Promotion + 
                 product_analytic_verticalGamePad + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + week, data = train_gameAcces)
summary(model_20)
vif(model_20)

# Removing Promotion
model_21 <- lm(formula = gmv ~ totalAffiliates + avgmrp + Sold_price +
                 product_analytic_verticalGamePad + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + week, data = train_gameAcces)
summary(model_21)
vif(model_21)

# Removing Sold_price
model_22 <- lm(formula = gmv ~ totalAffiliates + avgmrp +
                 product_analytic_verticalGamePad + product_analytic_verticalGamingKeyboard + 
                 product_analytic_verticalGamingMouse + week, data = train_gameAcces)
summary(model_22)
vif(model_22)

## Linear Regression Model for Game Accessory gmv
final_GameAcc_model <- model_22

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.9036745

########################################################################################
# 1c. Basic Linear Model - Home Audio 
########################################################################################

set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped), 0.7*nrow(HomeAudio_Grouped))

train_homeAudio <- HomeAudio_Grouped[indices,]
test_homeAudio <- HomeAudio_Grouped[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

#----------------------------------------------------------------------------------------
# Run the step object
step
#----------------------------------------------------------------------------------------

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + Sold_price + totalunits + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                totalEvent, data = train_homeAudio)
summary(model_2)
vif(model_2)

# Removing TotalInvestment for high vif
model_3 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + Sold_price + totalunits + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                totalEvent, data = train_homeAudio)
summary(model_3)
vif(model_3)

# Removing totalOnlineMarketing for high vif value 
model_4 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + totalContentMarketing +  
                totalAffiliates + Sold_price + totalunits + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                totalEvent, data = train_homeAudio)
summary(model_4)
vif(model_4)

# Removing totalContentMarketing  
model_5 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship +  
                totalAffiliates + Sold_price + totalunits + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                totalEvent, data = train_homeAudio)
summary(model_5)
vif(model_5)

# Removing  totalunits 
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship +  
                totalAffiliates + Sold_price + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                totalEvent, data = train_homeAudio)
summary(model_6)
vif(model_6)

# Removing totalItemTypeMassMarket
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship +  
                totalAffiliates + Sold_price + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent, data = train_homeAudio)
summary(model_7)
vif(model_7)

# Removing totalSponsorship
model_8 <- lm(formula = gmv ~ totalTV + totalDigital +  
                totalAffiliates + Sold_price + product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent, data = train_homeAudio)
summary(model_8)
vif(model_8)

# Removing totalAffiliates
model_9 <-  lm(formula = gmv ~ totalTV + totalDigital +  
                 Sold_price + product_analytic_verticalDock + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                 avgNPSScore + totalorder_payment_typePrepaid + 
                 totalEvent, data = train_homeAudio)
summary(model_9)
vif(model_9)

# All vif in range of <10
# Handling the p-value features 
# Removing Sold_price
model_10 <- lm(formula = gmv ~ totalTV + totalDigital +  
                 product_analytic_verticalDock + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                 avgNPSScore + totalorder_payment_typePrepaid + 
                 totalEvent, data = train_homeAudio)
summary(model_10)
vif(model_10)

# Removing avgNPSScore
model_11 <-  lm(formula = gmv ~ totalTV + totalDigital +  product_analytic_verticalDock + 
                  product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                  totalorder_payment_typePrepaid + totalEvent, data = train_homeAudio)
summary(model_11)
vif(model_11)

# Removing product_analytic_verticalDock
model_12 <-  lm(formula = gmv ~ totalTV + totalDigital + 
                  product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                  totalorder_payment_typePrepaid + totalEvent, data = train_homeAudio)
summary(model_12)
vif(model_12)

# Removing totalorder_payment_typePrepaid
model_13 <-  lm(formula = gmv ~ totalTV + totalDigital + 
                  product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                  totalEvent, data = train_homeAudio)
summary(model_13)
vif(model_13)

# Removing product_analytic_verticalFMRadio
model_14 <- lm(formula = gmv ~ totalTV + totalDigital + product_analytic_verticalHomeAudioSpeaker + 
                 totalEvent, data = train_homeAudio)
summary(model_14)
vif(model_14)

# All features have < 5% P Value.
final_HomeAudioAcc_model <- model_14

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.8938633

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12)
rm(model_13,model_14,model_15,model_16,model_17,model_18,model_19,model_20, model_21, model_22, model_23, model_24, model_25, model_26)

########################################################################################
########################################################################################
# 2. Multiplicative Model 
########################################################################################
########################################################################################

# Interaction between different KPIs and the growth of dependent variable with respect to growth of independent variable is considered
# Our main focus here is to identify the relation between gmv and marketing spent from different channels
# We can only include numerical variables here as - Ln(0) is not defined; 
# To handle the Log(0) case, Log(x+1) transformation is used with help of log1p() function

########################################################################################

CameraAccessory_Grouped_ln <- as.data.frame(log1p(CameraAccessory_Grouped))
GamingAccessory_Grouped_ln <- as.data.frame(log1p(GamingAccessory_Grouped))
HomeAudio_Grouped_ln <- as.data.frame(log1p(HomeAudio_Grouped))
########################################################################################

########################################################################################
# 2a. Multiplicative Model - Camera Accessory 
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_ln), 0.7*nrow(CameraAccessory_Grouped_ln))

train_camAcces <- CameraAccessory_Grouped_ln[indices,]
test_camAcces <- CameraAccessory_Grouped_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalRadio + totalOther + Sold_price + 
                totalunits + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_2)
vif(model_2)

# Removing totalOnlineMarketing 
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + 
                totalAffiliates + totalRadio + totalOther + Sold_price + 
                totalunits + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_3)
vif(model_3)

# Removing TotalInvestment 
model_4 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + 
                totalAffiliates + totalRadio + totalOther + Sold_price + 
                totalunits + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_4)
vif(model_4)

# Removing totalAffiliates 
model_5 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalRadio + totalOther + Sold_price + 
                totalunits + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_5)
vif(model_5)

# Removing totalOther 
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalRadio + Sold_price + 
                totalunits + product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_6)
vif(model_6)

# Removing totalunits 
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalRadio + Sold_price + 
                product_analytic_verticalCameraAccessory + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraFilmRolls + 
                product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalSoftbox + product_analytic_verticalStrap + 
                product_analytic_verticalTelescope + week + avgNPSScore, 
              data = train_camAcces)
summary(model_7)
vif(model_7)

# vif values are within reasonable range for all features
# Handling the high p-val features
# Removing product_analytic_verticalCameraTripod 
model_8 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalRadio + Sold_price + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                week + avgNPSScore, data = train_camAcces)
summary(model_8)
vif(model_8)

# Removing totalDigital 
model_9 <- lm(formula = gmv ~ totalTV + totalSponsorship + 
                totalContentMarketing + totalRadio + Sold_price + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + 
                product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                week + avgNPSScore, data = train_camAcces)
summary(model_9)
vif(model_9)

# Removing product_analytic_verticalCameraFilmRolls 
model_10 <- lm(formula = gmv ~ totalTV + totalSponsorship + 
                 totalContentMarketing + totalRadio + Sold_price + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_10)
vif(model_10)

# Removing product_analytic_verticalTelescope 
model_11 <- lm(formula = gmv ~ totalTV + totalSponsorship + 
                 totalContentMarketing + totalRadio + Sold_price + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 product_analytic_verticalStrap + week + avgNPSScore, data = train_camAcces)
summary(model_11)
vif(model_11)

# Removing product_analytic_verticalCameraAccessory 
model_12 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 product_analytic_verticalStrap + week + avgNPSScore, data = train_camAcces)
summary(model_12)
vif(model_12)

# Removing product_analytic_verticalStrap 
model_13 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_13)
vif(model_13)

# Removing product_analytic_verticalCameraBattery 
model_14 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_14)
vif(model_14)

# Removing product_analytic_verticalCameraBatteryCharger 
model_15 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_15)
vif(model_15)

# Removing avgNPSScore 
model_16 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + product_analytic_verticalFilter + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week, data = train_camAcces)
summary(model_16)
vif(model_16)

# Removing product_analytic_verticalFilter 
model_17 <- lm(formula = gmv ~ totalTV + totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week, data = train_camAcces)
summary(model_17)
vif(model_17)

# Removing totalTV 
model_18 <- lm(formula = gmv ~ totalSponsorship + totalContentMarketing + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week, data = train_camAcces)
summary(model_18)
vif(model_18)

# Removing totalContentMarketing 
model_19 <- lm(formula = gmv ~ totalSponsorship + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + 
                 product_analytic_verticalFlashShoeAdapter + product_analytic_verticalSoftbox + 
                 week, data = train_camAcces)
summary(model_19)
vif(model_19)

# Removing product_analytic_verticalFlashShoeAdapter 
model_20 <- lm(formula = gmv ~ totalSponsorship + totalRadio + Sold_price +  
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                 product_analytic_verticalExtensionTube + 
                 product_analytic_verticalSoftbox + 
                 week, data = train_camAcces)
summary(model_20)
vif(model_20)

## Multiplicative Model for Camera Accessory gmv
Final_CameraAcc_model <- model_20

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9284756

########################################################################################
# 2b. Multiplicative Model - Game Accessory 
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_ln), 0.7*nrow(GamingAccessory_Grouped_ln))

train_gameAcces <- GamingAccessory_Grouped_ln[indices,]
test_gameAcces <- GamingAccessory_Grouped_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_2)
vif(model_2)

# Removing totalOnlineMarketing for high vif
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + 
                totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_3)
vif(model_3)

# Removing TotalInvestment
model_4 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + 
                totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_4)
vif(model_4)

# Removing totalAffiliates
model_5 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOther + avgmrp + 
                Sold_price + Promotion + totalunits + product_analytic_verticalCoolingPad + 
                product_analytic_verticalGameControlMount + product_analytic_verticalGamingAccessoryKit + 
                product_analytic_verticalGamingChargingStation + product_analytic_verticalGamingMousePad + 
                product_analytic_verticalJoystickGamingWheel + product_analytic_verticalMotionController + 
                week + avgNPSScore, data = train_gameAcces)
summary(model_5)
vif(model_5)

# Removing Sold_price
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOther + avgmrp + 
                Promotion + totalunits + product_analytic_verticalCoolingPad + 
                product_analytic_verticalGameControlMount + product_analytic_verticalGamingAccessoryKit + 
                product_analytic_verticalGamingChargingStation + product_analytic_verticalGamingMousePad + 
                product_analytic_verticalJoystickGamingWheel + product_analytic_verticalMotionController + 
                week + avgNPSScore, data = train_gameAcces)
summary(model_6)
vif(model_6)

# All VIF in acceptable range.
# Handling the high p-val features
# Removing product_analytic_verticalGameControlMount
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOther + avgmrp + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + 
                product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_7)
vif(model_7)

# Removing product_analytic_verticalGamingChargingStation
model_8 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOther + avgmrp + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + 
                product_analytic_verticalGamingAccessoryKit + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_8)
vif(model_8)

# Removing product_analytic_verticalGamingAccessoryKit
model_9 <- lm(formula = gmv ~ totalTV + totalDigital + totalSponsorship + 
                totalContentMarketing + totalOther + avgmrp + Promotion + 
                totalunits + product_analytic_verticalCoolingPad + 
                product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                product_analytic_verticalMotionController + week + avgNPSScore, 
              data = train_gameAcces)
summary(model_9)
vif(model_9)

# Removing totalDigital
model_10 <- lm(formula = gmv ~ totalTV + totalSponsorship + 
                 totalContentMarketing + totalOther + avgmrp + Promotion + 
                 totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week + avgNPSScore, 
               data = train_gameAcces)
summary(model_10)
vif(model_10)

# All feature are in reasonable VIF range now
# Handling the high p-val features
# Removing avgNPSScore
model_11 <- lm(formula = gmv ~ totalTV + totalSponsorship + 
                 totalContentMarketing + totalOther + avgmrp + Promotion + 
                 totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, 
               data = train_gameAcces)
summary(model_11)
vif(model_11)

# Removing totalSponsorship
model_12 <- lm(formula = gmv ~ totalTV + 
                 totalContentMarketing + totalOther + avgmrp + Promotion + 
                 totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, 
               data = train_gameAcces)
summary(model_12)
vif(model_12)

# Removing totalOther
model_13 <- lm(formula = gmv ~ totalTV + 
                 totalContentMarketing + avgmrp + Promotion + 
                 totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, 
               data = train_gameAcces)
summary(model_13)
vif(model_13)

# Removing totalContentMarketing
model_14 <- lm(formula = gmv ~ totalTV + avgmrp + 
                 Promotion + totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, data = train_gameAcces)
summary(model_14)
vif(model_14)

# Removing totalTV
model_15 <- lm(formula = gmv ~ avgmrp + 
                 Promotion + totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, data = train_gameAcces)
summary(model_15)
vif(model_15)

# Removing product_analytic_verticalGamingMousePad
model_16 <- lm(formula = gmv ~ avgmrp + 
                 Promotion + totalunits + product_analytic_verticalCoolingPad + 
                 product_analytic_verticalJoystickGamingWheel + 
                 product_analytic_verticalMotionController + week, data = train_gameAcces)
summary(model_16)
vif(model_16)

## Multiplicative Model for Game Accessory gmv
final_GameAcc_model <- model_16

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.986069

########################################################################################
# 2c. Multiplicative Model - Home Audio 
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_ln), 0.7*nrow(HomeAudio_Grouped_ln))

train_homeAudio <- HomeAudio_Grouped_ln[indices,]
test_homeAudio <- HomeAudio_Grouped_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                totalRadio + avgmrp + Sold_price + Promotion + totalunits + 
                product_analytic_verticalDJController + product_analytic_verticalDock + 
                product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + week + avgNPSScore + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_2)
vif(model_2)

# Removing totalAffiliates for high vif 
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalRadio + avgmrp + Sold_price + Promotion + totalunits + 
                product_analytic_verticalDJController + product_analytic_verticalDock + 
                product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + week + avgNPSScore + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_3)
vif(model_3)

# Removing Sold_price 
model_4 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalRadio + avgmrp + Promotion + totalunits + 
                product_analytic_verticalDJController + product_analytic_verticalDock + 
                product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + week + avgNPSScore + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_4)
vif(model_4)

# Removing TotalInvestment
model_5 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + totalRadio + 
                avgmrp + Promotion + totalunits + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_5)
vif(model_5)

# Removing totalOnlineMarketing
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalRadio + 
                avgmrp + Promotion + totalunits + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_6)
vif(model_6)

# Removing totalunits
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + totalContentMarketing + 
                totalRadio + avgmrp + Promotion + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_7)
vif(model_7)

# Removing totalItemTypeMassMarket
model_8 <- lm(formula = gmv ~ totalTV + totalDigital + totalContentMarketing + 
                totalRadio + avgmrp + Promotion + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                week + avgNPSScore + totalorder_payment_typePrepaid, 
              data = train_homeAudio)
summary(model_8)
vif(model_8)

# Feature multicollinearity is handled with high vif values
# Feature handling with high p-val
# Removing totalTV
model_9 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + 
                totalRadio + avgmrp + Promotion + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                week + avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_9)
vif(model_9)

# Removing product_analytic_verticalDJController
model_10 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + 
                 totalRadio + avgmrp + Promotion + 
                 product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSlingBox + 
                 week + avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_10)
vif(model_10)

# Removing product_analytic_verticalSlingBox
model_11 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + 
                 totalRadio + avgmrp + Promotion + 
                 product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 product_analytic_verticalHomeAudioSpeaker + 
                 week + avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_11)
vif(model_11)

# Removing product_analytic_verticalHomeAudioSpeaker
model_12 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + totalRadio + 
                 avgmrp + Promotion + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 week + avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_12)
vif(model_12)

# Removing Promotion
model_13 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + totalRadio + 
                 avgmrp + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 week + avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_13)
vif(model_13)

# Removing week
model_14 <- lm(formula = gmv ~ totalDigital + totalContentMarketing + totalRadio + 
                 avgmrp + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_14)
vif(model_14)

# Removing totalContentMarketing
model_15 <- lm(formula = gmv ~ totalDigital + totalRadio + 
                 avgmrp + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 avgNPSScore + totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_15)
vif(model_15)

# Removing avgNPSScore
model_16 <- lm(formula = gmv ~ totalDigital + totalRadio + 
                 avgmrp + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_16)
vif(model_16)

# Removing product_analytic_verticalHiFiSystem
model_17 <- lm(formula = gmv ~ totalDigital + totalRadio + 
                 avgmrp + product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + 
                 totalorder_payment_typePrepaid, data = train_homeAudio)
summary(model_17)
vif(model_17)

final_HomeAudioAcc_model <- model_17

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9432842

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9, model_10)
rm(model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18,model_19,model_20)

########################################################################################
########################################################################################
# 3. Koyck Model 
########################################################################################
########################################################################################

# Data preparation for 3 categories and introduction of lag variable

CameraAccessory_Grouped_Koyck <- sqldf("SELECT Week, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'CameraAccessory' GROUP BY Week ORDER BY Week ASC")
gmv <- CameraAccessory_Grouped_Koyck[,2]
Ads <- CameraAccessory_Grouped_Koyck[,c(9:18)]
Price <- CameraAccessory_Grouped_Koyck[,c(5,7)]
Promotion <- CameraAccessory_Grouped_Koyck[,c(6)]
Product <- CameraAccessory_Grouped_Koyck[,c(3,4)]
Others <- CameraAccessory_Grouped_Koyck[,c(1,8,19,20,21)]
CameraAccessory_Grouped_Koyck <- arrange(cbind(gmv, Ads, Price, Promotion, Product, Others), week)

# Introducting the gmv lag variable
CameraAccessory_Grouped_Koyck$gmv_lag <- lag(CameraAccessory_Grouped_Koyck$gmv,1)
CameraAccessory_Grouped_Koyck$gmv_lag[1] = 0

GamingAccessory_Grouped_Koyck <- sqldf("SELECT Week, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'GamingAccessory' GROUP BY Week ORDER BY Week ASC")
gmv <- GamingAccessory_Grouped_Koyck[,2]
Ads <- GamingAccessory_Grouped_Koyck[,c(9:18)]
Price <- GamingAccessory_Grouped_Koyck[,c(5,7)]
Promotion <- GamingAccessory_Grouped_Koyck[,c(6)]
Product <- GamingAccessory_Grouped_Koyck[,c(3,4)]
Others <- GamingAccessory_Grouped_Koyck[,c(1,8,19,20,21)]
GamingAccessory_Grouped_Koyck <- arrange(cbind(gmv, Ads, Price, Promotion, Product, Others), week)

# Introducting the gmv lag variable
GamingAccessory_Grouped_Koyck$gmv_lag <- lag(GamingAccessory_Grouped_Koyck$gmv,1)
GamingAccessory_Grouped_Koyck$gmv_lag[1] = 0

HomeAudio_Grouped_Koyck <- sqldf("SELECT Week, sum(gmv) as totalgmv, sum(units) as totalunits, avg(sla) as avgsla, avg(product_mrp) as avgmrp, avg(Discount_Percentage) as avgdiscount, avg(gmvperunit) as Sold_price, avg(NPSScore) as avgNPSScore, sum(TotalInvestment) as TotalInvestment, sum(TV) as totalTV, sum(Digital) as totalDigital, sum(Sponsorship) as totalSponsorship, sum(ContentMarketing) as totalContentMarketing, sum(OnlineMarketing) as totalOnlineMarketing, sum(Affiliates) as totalAffiliates, sum(SEM) as totalSEM, sum(Radio) as totalRadio, sum(Other) as totalOther, sum(order_payment_typePrepaid) as totalorder_payment_typePrepaid, sum(ItemTypeMassMarket) as totalItemTypeMassMarket, sum(EventName1) as totalEvent FROM myData WHERE product_analytic_sub_category = 'HomeAudio' GROUP BY Week ORDER BY Week ASC")
gmv <- HomeAudio_Grouped_Koyck[,2]
Ads <- HomeAudio_Grouped_Koyck[,c(9:18)]
Price <- HomeAudio_Grouped_Koyck[,c(5,7)]
Promotion <- HomeAudio_Grouped_Koyck[,c(6)]
Product <- HomeAudio_Grouped_Koyck[,c(3,4)]
Others <- HomeAudio_Grouped_Koyck[,c(1,8,19,20,21)]
HomeAudio_Grouped_Koyck <- arrange(cbind(gmv, Ads, Price, Promotion, Product, Others), week)

# Introducting the gmv lag variable
HomeAudio_Grouped_Koyck$gmv_lag <- lag(HomeAudio_Grouped_Koyck$gmv,1)
HomeAudio_Grouped_Koyck$gmv_lag[1] = 0

rm(gmv, Ads, Price, Promotion, Product, Others)

########################################################################################
# 3a. Koyck Model - Camera Accessory
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_Koyck), 0.7*nrow(CameraAccessory_Grouped_Koyck))

train_camAcces <- CameraAccessory_Grouped_Koyck[indices,]
test_camAcces <- CameraAccessory_Grouped_Koyck[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalSEM + totalRadio + totalOther + Sold_price + 
                Promotion + totalunits + avgsla + week + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_2)
vif(model_2)

# Removing TotalInvestment, totalSponsorship and totalOther - VIF
model_3 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalSEM + totalRadio + Sold_price + 
                Promotion + totalunits + avgsla + week + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_3)
vif(model_3)

# Removing week
model_4 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalSEM + totalRadio + Sold_price + 
                Promotion + totalunits + avgsla + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_4)
vif(model_4)

# Removing totalRadio
model_5 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_5)
vif(model_5)

# Removing totalContentMarketing
model_6 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalOnlineMarketing + 
                totalAffiliates + totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_6)
vif(model_6)

# Removing totalOnlineMarketing
model_7 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalAffiliates + totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_7)
vif(model_7)

# Removing totalAffiliates
model_8 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + avgNPSScore + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_8)
vif(model_8)

# Removing avgNPSScore
model_9 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + totalorder_payment_typePrepaid + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_9)
vif(model_9)

# Removing totalorder_payment_typePrepaid
model_10 <- lm(formula = gmv ~ totalTV + totalDigital + 
                totalSEM + Sold_price + 
                Promotion + totalunits + avgsla + 
                totalEvent + gmv_lag, data = train_camAcces)
summary(model_10)
vif(model_10)

## Koyck Model for Camera Accessory gmv
Final_CameraAcc_model <- model_10

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9693125
# gmv lag is not significant - hence Koyck model is not relevent in this scenario

########################################################################################
# 3b. Koyck Model - Game Accessory
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_Koyck), 0.7*nrow(GamingAccessory_Grouped_Koyck))

train_gameAcces <- GamingAccessory_Grouped_Koyck[indices,]
test_gameAcces <- GamingAccessory_Grouped_Koyck[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                totalAffiliates + totalSEM + totalRadio + totalOther + avgmrp + 
                Sold_price + totalunits + avgsla + week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket, data = train_gameAcces)
summary(model_2)
vif(model_2)

# gmv lag is not significant - hence Koyck model is not relevent in this scenario
########################################################################################

########################################################################################
# 3c. Koyck Model - Home Audio
########################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_Koyck), 0.7*nrow(HomeAudio_Grouped_Koyck))

train_homeAudio <- HomeAudio_Grouped_Koyck[indices,]
test_homeAudio <- HomeAudio_Grouped_Koyck[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")
#----------------------------------------------------------------------------------------
# Run the step object
step

model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
              totalContentMarketing + totalOnlineMarketing + totalSEM + 
              totalOther + Sold_price + Promotion + totalunits + week + 
              avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket, 
              data = train_homeAudio)
summary(model_2)
vif(model_2)

# gmv lag is not significant - hence Koyck model is not relevent in this scenario

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10)
########################################################################################
########################################################################################


########################################################################################
########################################################################################
# 4. Distributed lag Model (Additive)
########################################################################################
########################################################################################

# Data preparation
CameraAccessory_Grouped_distributedlag  <- arrange(CameraAccessory_Grouped_Koyck, week)
CameraAccessory_Grouped_distributedlag <- CameraAccessory_Grouped_distributedlag[,-which(colnames(CameraAccessory_Grouped_distributedlag) %in% "gmv_lag")]

GamingAccessory_Grouped_distributedlag  <- arrange(GamingAccessory_Grouped_Koyck, week)
GamingAccessory_Grouped_distributedlag <- GamingAccessory_Grouped_distributedlag[,-which(colnames(GamingAccessory_Grouped_distributedlag) %in% "gmv_lag")]

HomeAudio_Grouped_distributedlag <- arrange(HomeAudio_Grouped_Koyck, week)
HomeAudio_Grouped_distributedlag <- HomeAudio_Grouped_distributedlag[,-which(colnames(HomeAudio_Grouped_distributedlag) %in% "gmv_lag")]

# Define Adstock Rate
# We are considering 0.5 as decay rate and t-3 as decay period i.e there will be no  effect of the marketing inniatives after 3 weeks 
# Incase some values are not significant - they will be automatically discarded

adstock_rate <- 0.50 
max_memory_1   <- 1
max_memory_2   <- 2
max_memory_3   <- 3

############################################################################################
# a.Preparating lag data for camera accessories
############################################################################################
# Lag 1 data preparation
learn_rates <- rep(adstock_rate, max_memory_1+1) ^ c(0:max_memory_1)

cam_lag_1 <- sapply(CameraAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(cam_lag_1) <- paste(colnames(cam_lag_1), "Lag1", sep = "_")

game_lag_1 <- sapply(GamingAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(game_lag_1) <- paste(colnames(game_lag_1), "Lag1", sep = "_")

audio_lag_1 <- sapply(HomeAudio_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(audio_lag_1) <- paste(colnames(audio_lag_1), "Lag1", sep = "_")

# Lag 2 data preparation
learn_rates <- rep(adstock_rate, max_memory_2+1) ^ c(0:max_memory_2)

cam_lag_2 <- sapply(CameraAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(cam_lag_2) <- paste(colnames(cam_lag_2), "Lag2", sep = "_")

game_lag_2 <- sapply(GamingAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(game_lag_2) <- paste(colnames(game_lag_2), "Lag2", sep = "_")

audio_lag_2 <- sapply(HomeAudio_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(audio_lag_2) <- paste(colnames(audio_lag_2), "Lag2", sep = "_")

# Lag 3 data preparation
learn_rates <- rep(adstock_rate, max_memory_3+1) ^ c(0:max_memory_3)

cam_lag_3 <- sapply(CameraAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(cam_lag_3) <- paste(colnames(cam_lag_3), "Lag3", sep = "_")

game_lag_3 <- sapply(GamingAccessory_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(game_lag_3) <- paste(colnames(game_lag_3), "Lag3", sep = "_")

audio_lag_3 <- sapply(HomeAudio_Grouped_distributedlag, function(x) stats::filter(x, learn_rates, method="convolution"))
colnames(audio_lag_3) <- paste(colnames(audio_lag_3), "Lag3", sep = "_")

CameraAccessory_Grouped_distributedlag <- cbind(CameraAccessory_Grouped_distributedlag, cam_lag_1, cam_lag_2, cam_lag_3)
GamingAccessory_Grouped_distributedlag <- cbind(GamingAccessory_Grouped_distributedlag, game_lag_1, game_lag_2, game_lag_3)
HomeAudio_Grouped_distributedlag <- cbind(HomeAudio_Grouped_distributedlag, audio_lag_1, audio_lag_2, audio_lag_3)


CameraAccessory_Grouped_distributedlag <- CameraAccessory_Grouped_distributedlag[c(-1,-(nrow(CameraAccessory_Grouped_distributedlag)-1),-nrow(CameraAccessory_Grouped_distributedlag)),]
GamingAccessory_Grouped_distributedlag <- GamingAccessory_Grouped_distributedlag[c(-1,-(nrow(GamingAccessory_Grouped_distributedlag)-1),-nrow(GamingAccessory_Grouped_distributedlag)),]
HomeAudio_Grouped_distributedlag <- HomeAudio_Grouped_distributedlag[c(-1,-(nrow(HomeAudio_Grouped_distributedlag)-1),-nrow(HomeAudio_Grouped_distributedlag)),]

##############################################################################################
##############################################################################################
# 4a. Distributed lag Model (Additive) - Camera Accessory
##############################################################################################
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag), 0.7*nrow(CameraAccessory_Grouped_distributedlag))

train_camAcces <- CameraAccessory_Grouped_distributedlag[indices,]
test_camAcces <- CameraAccessory_Grouped_distributedlag[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

##############################################################################################
# perform Boruta search for significant independent variables

boruta_output <- Boruta(gmv ~ ., data=na.omit(train_camAcces), doTrace=2)  
# collect Confirmed variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% "Confirmed"])  
boruta_signif

# Removing unimportant attributed
gmv <- CameraAccessory_Grouped_distributedlag[,"gmv"]
CameraAccessory_Grouped_distributedlag_new <- CameraAccessory_Grouped_distributedlag[,boruta_signif]
CameraAccessory_Grouped_distributedlag_new <- cbind(gmv, CameraAccessory_Grouped_distributedlag_new)

set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag_new), 0.7*nrow(CameraAccessory_Grouped_distributedlag_new))

train_camAcces <- CameraAccessory_Grouped_distributedlag_new[indices,]
test_camAcces <- CameraAccessory_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Removing totalOnlineMarketing
model_2 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalContentMarketing + 
                totalAffiliates + totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket  , data = train_camAcces)
summary(model_2)

# Removing totalDigital
model_3 <- lm(formula = gmv ~ TotalInvestment + totalContentMarketing + 
                totalAffiliates + totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket  , data = train_camAcces)
summary(model_3)

# Removing TotalInvestment
model_4 <- lm(formula = gmv ~ totalContentMarketing + 
                totalAffiliates + totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket  , data = train_camAcces)
summary(model_4)

# Removing totalSEM
model_5 <- lm(formula = gmv ~ totalContentMarketing + 
                totalAffiliates + totalunits + avgNPSScore + totalItemTypeMassMarket  , data = train_camAcces)
summary(model_5)

# Removing totalAffiliates
model_6 <- lm(formula = gmv ~ totalContentMarketing + 
                totalunits + avgNPSScore + totalItemTypeMassMarket  , data = train_camAcces)
summary(model_6)

## Distributed Lag Model (Additive) for Camera Accessory gmv
Final_CameraAcc_model <- model_6

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9819624
# as this does not have any significant lag variable - this model is not relevent

##############################################################################################
##############################################################################################
# 4b. Distributed lag Model (Additive) - Game Accessory
##############################################################################################
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag), 0.7*nrow(GamingAccessory_Grouped_distributedlag))

train_gameAcces <- GamingAccessory_Grouped_distributedlag[indices,]
test_gameAcces <- GamingAccessory_Grouped_distributedlag[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

##############################################################################################
# perform Boruta search for significant independent variables
boruta_output <- Boruta(gmv ~ ., data=na.omit(train_gameAcces), doTrace=2)  
# collect Confirmed variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% "Confirmed"])  
boruta_signif

# Removing unimportant attributed
gmv <- GamingAccessory_Grouped_distributedlag[,"gmv"]
GamingAccessory_Grouped_distributedlag_new <- GamingAccessory_Grouped_distributedlag[,boruta_signif]
GamingAccessory_Grouped_distributedlag_new <- cbind(gmv, GamingAccessory_Grouped_distributedlag_new)

set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag_new), 0.7*nrow(GamingAccessory_Grouped_distributedlag_new))

train_gameAcces <- GamingAccessory_Grouped_distributedlag_new[indices,]
test_gameAcces <- GamingAccessory_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Removing totalAffiliates
model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalOnlineMarketing + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_2)
vif(model_2)

# Removing totalunits_Lag1, totalunits_Lag2 and totalItemTypeMassMarket_Lag1 - vif
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalOnlineMarketing + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_3)
vif(model_3)

# Removing totalOnlineMarketing and totalAffiliates - vif
model_4 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_4)
vif(model_4)

# Removing totalItemTypeMassMarket_Lag2
model_5 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_5)
vif(model_5)

# Removing totalTV
model_6 <- lm(formula = gmv ~ TotalInvestment + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_6)
vif(model_6)

# Removing TotalInvestment
model_7 <- lm(formula = gmv ~ totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_7)
vif(model_7)

# Removing week
model_8 <- lm(formula = gmv ~ totalunits +  
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalTV_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_8)
vif(model_8)

# Removing totalTV_Lag2
model_9 <- lm(formula = gmv ~ totalunits +  
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_9)
vif(model_9)

# Removing totalOnlineMarketing_Lag2
model_10 <- lm(formula = gmv ~ totalunits +  
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1  +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_10)
vif(model_10)

# Removing TotalInvestment_Lag1
model_11 <- lm(formula = gmv ~ totalunits +  
                 avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalAffiliates_Lag1  +
                 avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                 avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_11)
vif(model_11)

# Removing avgNPSScore
model_12 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalAffiliates_Lag1  +
                 avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                 avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_12)
vif(model_12)

# Removing avgNPSScore_Lag2
model_13 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalAffiliates_Lag1  +
                 avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                 totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_13)
vif(model_13)

# Removing avgNPSScore_Lag1
model_14 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalAffiliates_Lag1  +
                 totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                 totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_14)
vif(model_14)

# Removing gmv_Lag1
model_15 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + totalAffiliates_Lag1  +
                 totalorder_payment_typePrepaid_Lag1 + gmv_Lag2 +
                 totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_15)
vif(model_15)

# Removing totalorder_payment_typePrepaid_Lag1
model_16 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + totalAffiliates_Lag1  +
                 gmv_Lag2 +
                 totalorder_payment_typePrepaid_Lag2, data = train_gameAcces)
summary(model_16)
vif(model_16)

# Removing totalorder_payment_typePrepaid_Lag2
model_17 <- lm(formula = gmv ~ totalunits +  
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + totalAffiliates_Lag1  +
                 gmv_Lag2 
                 , data = train_gameAcces)
summary(model_17)
vif(model_17)

## Distributed Lag Model for Game Accessory gmv
final_GameAcc_model <- model_17

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.9061027

##############################################################################################
##############################################################################################
# 4c. Distributed lag Model (Additive) - Home Audio
##############################################################################################
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag), 0.7*nrow(HomeAudio_Grouped_distributedlag))

train_homeAudio <- HomeAudio_Grouped_distributedlag[indices,]
test_homeAudio <- HomeAudio_Grouped_distributedlag[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

##############################################################################################
# perform Boruta search for significant independent variables
boruta_output <- Boruta(gmv ~ ., data=na.omit(train_homeAudio), doTrace=2)  
# collect Confirmed variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% "Confirmed"])  
boruta_signif

# Removing unimportant attributed
gmv <- HomeAudio_Grouped_distributedlag[,"gmv"]
HomeAudio_Grouped_distributedlag_new <- HomeAudio_Grouped_distributedlag[,boruta_signif]
HomeAudio_Grouped_distributedlag_new <- cbind(gmv, HomeAudio_Grouped_distributedlag_new)

set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag_new), 0.7*nrow(HomeAudio_Grouped_distributedlag_new))

train_homeAudio <- HomeAudio_Grouped_distributedlag_new[indices,]
test_homeAudio <- HomeAudio_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Removing totalTV
model_2 <- lm(formula = gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp +
                Promotion + totalunits + totalItemTypeMassMarket + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_2)
vif(model_2)

# Removing totalItemTypeMassMarket
model_3 <- lm(formula = gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp +
                Promotion + totalunits + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_3)
vif(model_3)

# Removing totalOnlineMarketing
model_4 <- lm(formula = gmv ~ totalAffiliates + totalSEM + avgmrp +
                Promotion + totalunits + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_4)
vif(model_4)

# Removing totalAffiliates
model_5 <- lm(formula = gmv ~ totalSEM + avgmrp +
                Promotion + totalunits + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_5)
vif(model_5)

# Removing Promotion
model_6 <- lm(formula = gmv ~ totalSEM + avgmrp +
                totalunits + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_6)
vif(model_6)

final_HomeAudioAcc_model <- model_6

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9916531

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13,model_14,model_15,model_16,model_17)

########################################################################################
########################################################################################
# 5. Distributed lag Model (Multiplicative)
########################################################################################
########################################################################################

CameraAccessory_Grouped_distributedlag_ln <- as.data.frame(log1p(CameraAccessory_Grouped_distributedlag_new))
GamingAccessory_Grouped_distributedlag_ln <- as.data.frame(log1p(GamingAccessory_Grouped_distributedlag_new))
HomeAudio_Grouped_distributedlag_ln <- as.data.frame(log1p(HomeAudio_Grouped_distributedlag_new))

##############################################################################################
# 5a. Distributed lag Model (Multiplicative) - Camera Accessory
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag_ln), 0.7*nrow(CameraAccessory_Grouped_distributedlag_ln))

train_camAcces <- CameraAccessory_Grouped_distributedlag_ln[indices,]
test_camAcces <- CameraAccessory_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Removing totalSponsorship
model_2 <- lm(formula = gmv ~ TotalInvestment + totalDigital + totalContentMarketing +
                totalOnlineMarketing + totalAffiliates + totalSEM + totalunits +
                avgNPSScore + totalItemTypeMassMarket, data = train_camAcces)
summary(model_2)

# as this does not have any significant lag variable - this model is not revelent

##############################################################################################
# 5b. Distributed lag Model (Multiplicative) - Game Accessory
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag_ln), 0.7*nrow(GamingAccessory_Grouped_distributedlag_ln))

train_gameAcces <- GamingAccessory_Grouped_distributedlag_ln[indices,]
test_gameAcces <- GamingAccessory_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Removing totalTV_Lag2
model_2 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalOnlineMarketing + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                avgNPSScore_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_2)

# Removing avgNPSScore_Lag1
model_3 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalOnlineMarketing + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_3)

# Removing totalOnlineMarketing
model_4 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_4)


# Removing TotalInvestment
model_5 <- lm(formula = gmv ~ totalTV + totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_5)

# Removing totalTV
model_6 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + TotalInvestment_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_6)

# Removing TotalInvestment_Lag1
model_7 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalAffiliates_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_7)

# Removing totalAffiliates_Lag1
model_8 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalunits_Lag1 +
                totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_8)

# Removing totalorder_payment_typePrepaid_Lag1
model_9 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalunits_Lag1 +
                totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_9)

# Removing totalorder_payment_typePrepaid_Lag2
model_10 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalunits_Lag1 +
                totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                totalunits_Lag2 + avgNPSScore_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_10)

# Removing avgNPSScore_Lag2
model_11 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                 avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_11)

# Removing avgNPSScore
model_12 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                 totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_12)

# Removing totalItemTypeMassMarket
model_13 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                 totalorder_payment_typePrepaid + gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_13)

# Removing totalorder_payment_typePrepaid
model_14 <- lm(formula = gmv ~ totalAffiliates + totalunits + week + 
                 gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_14)

# Removing week 
model_15 <- lm(formula = gmv ~ totalAffiliates + totalunits + 
                 gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2 + totalItemTypeMassMarket_Lag2, data = train_gameAcces)
summary(model_15)

# Removing totalItemTypeMassMarket_Lag2 
model_16 <- lm(formula = gmv ~ totalAffiliates + totalunits + 
                 gmv_Lag1 + totalunits_Lag1 +
                 totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2, data = train_gameAcces)
summary(model_16)

# Removing totalItemTypeMassMarket_Lag1 
model_17 <- lm(formula = gmv ~ totalAffiliates + totalunits + 
                 gmv_Lag1 + totalunits_Lag1 +
                 gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2, data = train_gameAcces)
summary(model_17)

# Removing totalAffiliates 
model_18 <- lm(formula = gmv ~ totalunits + 
                 gmv_Lag1 + totalunits_Lag1 +
                 gmv_Lag2 + totalOnlineMarketing_Lag2 +
                 totalunits_Lag2, data = train_gameAcces)
summary(model_18)

# Removing totalOnlineMarketing_Lag2 
model_19 <- lm(formula = gmv ~ totalunits + 
                 gmv_Lag1 + totalunits_Lag1 +
                 gmv_Lag2 +
                 totalunits_Lag2, data = train_gameAcces)
summary(model_19)

## Distributed Lag Model (multiplicative) for Game Accessory gmv
final_GameAcc_model <- model_19

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.9922839

##############################################################################################
# 5c. Distributed lag Model (Multiplicative) - Home Audio
##############################################################################################

# Divide the data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag_ln), 0.7*nrow(HomeAudio_Grouped_distributedlag_ln))

train_homeAudio <- HomeAudio_Grouped_distributedlag_ln[indices,]
test_homeAudio <- HomeAudio_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Removing totalTV
model_2 <- lm(formula = gmv ~   
                totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp + Promotion + totalunits +
                totalItemTypeMassMarket + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_2)

# Removing totalAffiliates
model_3 <- lm(formula = gmv ~   
                totalOnlineMarketing + totalSEM + avgmrp + Promotion + totalunits +
                totalItemTypeMassMarket + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_3)

# Removing totalSEM
model_4 <- lm(formula = gmv ~   
                totalOnlineMarketing + avgmrp + Promotion + totalunits +
                totalItemTypeMassMarket + totalAffiliates_Lag1, data = train_homeAudio)
summary(model_4)

# Removing totalItemTypeMassMarket
model_5 <- lm(formula = gmv ~   
                totalOnlineMarketing + avgmrp + Promotion + totalunits +
                totalAffiliates_Lag1, data = train_homeAudio)
summary(model_5)

final_HomeAudioAcc_model <- model_5

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9776283

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9)
rm(model_10,model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18,model_19)

#######################################################################################################

#######################################################################################################
#######################################################################################################
# 6. Hierarchical Model (for reference only)
#######################################################################################################
#######################################################################################################

#######################################################################################################
### 6a. Camera Accessory Hierarchical Model ###
#######################################################################################################

# The base model (model_1) was created without the Advertisement Channel feature variables. Advertisment Channel Feature variable were added to uncover the hierarchical value addition in overall model
# Based on this significant Advertsiement Channel were identified to include in the final Model.
str(CameraAccessory_Grouped)
model_1 <- lm(gmv~avgmrp+Sold_price+Promotion+totalunits+avgsla+avgNPSScore+totalorder_payment_typePrepaid+totalItemTypeMassMarket+totalEvent,CameraAccessory_Grouped) 
summary(model_1)

model_2 <- update(model_1, . ~ . + totalTV)
model_3 <- update(model_2, . ~ . + totalDigital)
model_4 <- update(model_3, . ~ . + totalSponsorship)
model_5 <- update(model_4, . ~ . + totalContentMarketing)
model_6 <- update(model_5, . ~ . + totalOnlineMarketing)
model_7 <- update(model_6, . ~ . + totalSEM)
model_8 <- update(model_7, . ~ . + totalOther)
model_9 <- update(model_8, . ~ . + totalAffiliates)
model_10 <- update(model_9, . ~ . + totalRadio)

anova(base_model, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, test = "F")

# Final Model based on the Anova result 
Cam_finalModel <- update(model_1, . ~ . +totalTV + totalDigital + totalOnlineMarketing + totalOther + totalAffiliates) 
summary(Cam_finalModel)


#######################################################################################################
### 6b. Gaming Accessory Hierarchical Model ###
#######################################################################################################

# The base model (model_1) was created without the Advertisement Channel feature variables. Advertisment Channel Feature variable were added to uncover the hierarchical value addition in overall model
# Based on this significant Advertsiement Channel were identified to include in the final Model.
str(GamingAccessory_Grouped)
model_1 <- lm(gmv~avgmrp+Sold_price+Promotion+totalunits+avgsla+avgNPSScore+totalorder_payment_typePrepaid+totalItemTypeMassMarket+totalEvent,GamingAccessory_Grouped) 
summary(model_1)

model_2 <- update(model_1, . ~ . + totalTV)
model_3 <- update(model_2, . ~ . + totalDigital)
model_4 <- update(model_3, . ~ . + totalSponsorship)
model_5 <- update(model_4, . ~ . + totalContentMarketing)
model_6 <- update(model_5, . ~ . + totalOnlineMarketing)
model_7 <- update(model_6, . ~ . + totalSEM)
model_8 <- update(model_7, . ~ . + totalOther)
model_9 <- update(model_8, . ~ . + totalAffiliates)
model_10 <- update(model_9, . ~ . + totalRadio)

anova(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, test = "F")

# Final Model based on the Anova result 
Game_finalModel <- update(model_1, . ~ . + totalContentMarketing + totalOnlineMarketing + totalOther + totalRadio) 
summary(Game_finalModel)

#######################################################################################################
### 6c. Home Audio Accessory Hierarchical Model ###
#######################################################################################################

# The base model (model_1) was created without the Advertisement Channel feature variables. Advertisment Channel Feature variable were added to uncover the hierarchical value addition in overall model
# Based on this significant Advertsiement Channel were identified to include in the final Model.
str(HomeAudio_Grouped)
model_1 <- lm(gmv~avgmrp+Sold_price+Promotion+totalunits+avgsla+avgNPSScore+totalorder_payment_typePrepaid+totalItemTypeMassMarket+totalEvent,HomeAudio_Grouped) 
summary(model_1)

model_2 <- update(model_1, . ~ . + totalTV)
model_3 <- update(model_2, . ~ . + totalDigital)
model_4 <- update(model_3, . ~ . + totalSponsorship)
model_5 <- update(model_4, . ~ . + totalContentMarketing)
model_6 <- update(model_5, . ~ . + totalOnlineMarketing)
model_7 <- update(model_6, . ~ . + totalSEM)
model_8 <- update(model_7, . ~ . + totalOther)
model_9 <- update(model_8, . ~ . + totalAffiliates)
model_10 <- update(model_9, . ~ . + totalRadio)

anova(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, test = "F")

# Final Model based on the Anova result 
HomeAudio_finalModel <- update(model_1, . ~ . + totalTV + totalDigital + totalSponsorship + totalSEM + totalOther) 
summary(HomeAudio_finalModel)
