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

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped), 0.9*nrow(CameraAccessory_Grouped))

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

model_1 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + 
          totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
          totalRadio + Promotion + totalunits + product_analytic_verticalCameraAccessory + 
          product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
          product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
          product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
          product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
          product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
          product_analytic_verticalFlash + product_analytic_verticalLens + 
          product_analytic_verticalStrap + product_analytic_verticalTelescope + 
          week + totalItemTypeMassMarket + product_analytic_verticalCameraFilmRolls, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_1)

# Removing product_analytic_verticalCameraFilmRolls
model_2 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                   product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalSponsorship
model_3 <- train(gmv ~ TotalInvestment + totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                   product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing week
model_4 <- train(gmv ~ TotalInvestment + totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                   product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing product_analytic_verticalCameraAccessory
model_5 <- train(gmv ~ TotalInvestment + totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                   product_analytic_verticalCameraHousing + product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalCameraHousing
model_6 <- train(gmv ~ TotalInvestment + totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraEyeCup + 
                   product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing product_analytic_verticalCameraEyeCup
model_7 <- train(gmv ~ TotalInvestment + totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing TotalInvestment
model_8 <- train(gmv ~ totalTV + 
                   totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalOnlineMarketing
model_9 <- train(gmv ~ totalTV + 
                   totalContentMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing totalTV
model_10 <- train(gmv ~  
                   totalContentMarketing + totalAffiliates + 
                   totalRadio + Promotion + totalunits + 
                   product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + product_analytic_verticalLens + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing product_analytic_verticalCameraMount
model_11 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraMicrophone + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                    product_analytic_verticalFlash + product_analytic_verticalLens + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing product_analytic_verticalTelescope
model_12 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraMicrophone + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                    product_analytic_verticalFlash + product_analytic_verticalLens + 
                    product_analytic_verticalStrap + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

# Removing product_analytic_verticalExtensionTube
model_13 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraMicrophone + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalFlash + product_analytic_verticalLens + 
                    product_analytic_verticalStrap + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_13)

# Removing product_analytic_verticalCameraMicrophone
model_14 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalFlash + product_analytic_verticalLens + 
                    product_analytic_verticalStrap + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_14)

# Removing product_analytic_verticalStrap
model_15 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalFlash + product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_15)

# Removing product_analytic_verticalFlash
model_16 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBag + product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_16)

# Removing product_analytic_verticalCameraBag
model_17 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + Promotion + totalunits + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_17)

# Removing Promotion
model_18 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + totalunits + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraRemoteControl + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_18)

# Removing product_analytic_verticalCameraRemoteControl
model_19 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalRadio + totalunits + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_19)

# Removing totalRadio
model_20 <- train(gmv ~  
                    totalContentMarketing + totalAffiliates + 
                    totalunits + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_20)

# Removing totalContentMarketing
model_21 <- train(gmv ~  
                    totalAffiliates + 
                    totalunits + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryCharger + 
                    product_analytic_verticalCameraTripod + 
                    product_analytic_verticalLens + 
                    totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_21)

## Linear Regression Model for Camera Accessory gmv
Final_CameraAcc_model <- model_21

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv, test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9927484

########################################################################################
# 1b. Basic Linear Model - Game Accessory 
########################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped), 0.9*nrow(GamingAccessory_Grouped))

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

model_1 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + avgsla + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    week + avgNPSScore + totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_1)

# Removing product_analytic_verticalGamingHeadset
model_2 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + avgsla + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    week + avgNPSScore + totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing avgNPSScore
model_2 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + avgsla + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    week + totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing week
model_3 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + avgsla + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing avgsla
model_4 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing product_analytic_verticalGamingAccessoryKit
model_5 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalGamingMousePad + product_analytic_verticalJoystickGamingWheel + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalGamingMousePad
model_6 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingAdapter + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalJoystickGamingWheel + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing product_analytic_verticalGamingAdapter
model_7 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    product_analytic_verticalJoystickGamingWheel + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing product_analytic_verticalJoystickGamingWheel
model_8 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    totalItemTypeMassMarket + totalEvent, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalEvent
model_9 <- train( gmv ~ TotalInvestment + totalTV + totalSponsorship + 
                    totalContentMarketing + totalSEM + totalRadio + avgmrp + 
                    Sold_price + Promotion + totalunits + product_analytic_verticalGamePad + 
                    product_analytic_verticalGamingKeyboard + 
                    product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMouse + 
                    totalItemTypeMassMarket, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

## Linear Regression Model for Game Accessory gmv
final_GameAcc_model <- model_9

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.9474771

########################################################################################
# 1c. Basic Linear Model - Home Audio 
########################################################################################

# Divide the data in 90:10 

set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped), 0.9*nrow(HomeAudio_Grouped))

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

model_1 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalSEM + totalOther + Sold_price + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_1)

# Removing totalOther
model_2 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalSEM + Sold_price + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing Sold_price
model_3 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalSEM + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalSEM
model_4 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Check for multicolinearity 
model_4 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                 totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                 totalAffiliates + totalunits + 
                 product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                 totalItemTypeMassMarket + totalEvent, data = train_homeAudio)
summary(model_4)
vif(model_4)

# Removing TotalInvestment
model_5 <- train(gmv ~ totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   product_analytic_verticalHomeAudioSpeaker + avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalHomeAudioSpeaker
model_6 <- train(gmv ~ totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalDigital
model_7 <- train(gmv ~ totalTV +  
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalSponsorship
model_8 <- train(gmv ~ totalTV +  
                   totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalTV
model_9 <- train(gmv ~   
                   totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing totalAffiliates
model_10 <- train(gmv ~   
                   totalContentMarketing + totalOnlineMarketing + 
                   totalunits + 
                   product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                   avgNPSScore + 
                   totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing avgNPSScore
model_11 <- train(gmv ~   
                    totalContentMarketing + totalOnlineMarketing + 
                    totalunits + 
                    product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                    totalItemTypeMassMarket + totalEvent, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# All features have < 5% P Value.
final_HomeAudioAcc_model <- model_11

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9960572

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12)
rm(model_13,model_14,model_15,model_16,model_17,model_18,model_19,model_20, model_21)

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

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_ln), 0.9*nrow(CameraAccessory_Grouped_ln))

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

model_1 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalRadio + totalOther + Sold_price + 
                   totalunits + avgsla + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalLens + product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore + totalItemTypeMassMarket, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_1)

# Removing totalItemTypeMassMarket
model_2 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalRadio + totalOther + Sold_price + 
                   totalunits + avgsla + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalLens + product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalRadio
model_3 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + avgsla + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalLens + product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing product_analytic_verticalLens
model_4 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + avgsla + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing avgsla
model_5 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalCameraBatteryCharger
model_6 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing product_analytic_verticalCameraTripod
model_7 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMicrophone + product_analytic_verticalCameraMount + 
                   product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing product_analytic_verticalCameraMicrophone
model_8 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMount + 
                   product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing product_analytic_verticalExtensionTube
model_9 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMount + 
                   product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing product_analytic_verticalExtensionTube
model_10 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + Sold_price + 
                   totalunits + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                   product_analytic_verticalCameraMount + 
                   product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                   product_analytic_verticalSoftbox + 
                   product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                   week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

model_10 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                 totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                 totalAffiliates + totalOther + Sold_price + 
                 totalunits + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                 product_analytic_verticalCameraMount + 
                 product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                 product_analytic_verticalSoftbox + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_10)
vif(model_10)

# Removing totalOnlineMarketing - vif
model_11 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalSponsorship + totalContentMarketing + 
                    totalAffiliates + totalOther + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFilter + product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing product_analytic_verticalFilter
model_12 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalSponsorship + totalContentMarketing + 
                    totalAffiliates + totalOther + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

# Removing totalOther
model_13 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalSponsorship + totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_13)

# Removing totalSponsorship
model_14 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_14)

model_14 <- lm(formula = gmv ~ TotalInvestment + totalTV + totalDigital + 
                 totalContentMarketing + 
                 totalAffiliates + Sold_price + 
                 totalunits + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                 product_analytic_verticalCameraMount + 
                 product_analytic_verticalFlashShoeAdapter + 
                 product_analytic_verticalSoftbox + 
                 product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                 week + avgNPSScore, data = train_camAcces)
summary(model_14)
vif(model_14)

# Removing TotalInvestment
model_15 <- train(gmv ~  totalTV + totalDigital + 
                    totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    week + avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_15)

# Removing week
model_16 <- train(gmv ~  totalTV + totalDigital + 
                    totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_16)

# Removing totalDigital
model_17 <- train(gmv ~  totalTV + 
                    totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_17)

# Removing totalTV
model_18 <- train(gmv ~    
                    totalContentMarketing + 
                    totalAffiliates + Sold_price + 
                    totalunits + product_analytic_verticalCameraAccessory + 
                    product_analytic_verticalCameraBattery + 
                    product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraFilmRolls + 
                    product_analytic_verticalCameraMount + 
                    product_analytic_verticalFlashShoeAdapter + 
                    product_analytic_verticalSoftbox + 
                    product_analytic_verticalStrap + product_analytic_verticalTelescope + 
                    avgNPSScore, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_18)

## Multiplicative Model for Camera Accessory gmv
Final_CameraAcc_model <- model_18

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.998684

########################################################################################
# 2b. Multiplicative Model - Game Accessory 
########################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_ln), 0.9*nrow(GamingAccessory_Grouped_ln))

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

model_2 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
                   product_analytic_verticalGamingChargingStation + product_analytic_verticalGamingHeadset + 
                   product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalGamingSpeaker + product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing product_analytic_verticalGamingAccessoryKit
model_3 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingAdapter + 
                   product_analytic_verticalGamingChargingStation + product_analytic_verticalGamingHeadset + 
                   product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalGamingSpeaker + product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing product_analytic_verticalGamingHeadset
model_4 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingAdapter + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalGamingSpeaker + product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing product_analytic_verticalGamingSpeaker
model_5 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingAdapter + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalGamingMemoryCard + product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalGamingMemoryCard
model_6 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingAdapter + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing product_analytic_verticalGamingAdapter
model_7 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalJoystickGamingWheel + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing product_analytic_verticalJoystickGamingWheel
model_8 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalGamingMousePad + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing product_analytic_verticalGamingMousePad
model_9 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingChargingStation + 
                   product_analytic_verticalMotionController + week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing product_analytic_verticalMotionController
model_10 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                   totalSponsorship + totalContentMarketing + totalOnlineMarketing + 
                   totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                   product_analytic_verticalGamingChargingStation + 
                   week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing totalOnlineMarketing
model_10 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalSponsorship + totalContentMarketing + 
                    totalAffiliates + totalOther + avgmrp + Sold_price + Promotion + 
                    totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                    product_analytic_verticalGamingChargingStation + 
                    week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing totalOther
model_11 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalSponsorship + totalContentMarketing + 
                    totalAffiliates + avgmrp + Sold_price + Promotion + 
                    totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                    product_analytic_verticalGamingChargingStation + 
                    week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing totalSponsorship
model_12 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + 
                    totalAffiliates + avgmrp + Sold_price + Promotion + 
                    totalunits + product_analytic_verticalCoolingPad + product_analytic_verticalGameControlMount + 
                    product_analytic_verticalGamingChargingStation + 
                    week + avgNPSScore, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

## Multiplicative Model for Game Accessory gmv
final_GameAcc_model <- model_12

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.9983094

########################################################################################
# 2c. Multiplicative Model - Home Audio 
########################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_ln), 0.9*nrow(HomeAudio_Grouped_ln))

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

model_2 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                    totalRadio + avgmrp + Sold_price + Promotion + totalunits + 
                    avgsla + product_analytic_verticalDJController + product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalorder_payment_typePrepaid + totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalRadio
model_3 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    avgsla + product_analytic_verticalDJController + product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalorder_payment_typePrepaid + totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalorder_payment_typePrepaid
model_4 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    avgsla + product_analytic_verticalDJController + product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing avgsla
model_5 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDJController + product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing product_analytic_verticalDJController
model_6 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalOnlineMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalOnlineMarketing
model_7 <- train( gmv ~ TotalInvestment + totalTV + totalDigital + 
                    totalContentMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing TotalInvestment
model_8 <- train( gmv ~ totalTV + totalDigital + 
                    totalContentMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalTV
model_9 <- train( gmv ~ totalDigital + 
                    totalContentMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + avgNPSScore + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing avgNPSScore
model_10 <- train( gmv ~ totalDigital + 
                    totalContentMarketing + totalAffiliates + 
                    avgmrp + Sold_price + Promotion + totalunits + 
                    product_analytic_verticalDock + 
                    product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                    product_analytic_verticalHomeAudioSpeaker + week + 
                    totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing week
model_11 <- train( gmv ~ totalDigital + 
                     totalContentMarketing + totalAffiliates + 
                     avgmrp + Sold_price + Promotion + totalunits + 
                     product_analytic_verticalDock + 
                     product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                     product_analytic_verticalHomeAudioSpeaker + 
                     totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing totalDigital
model_12 <- train( gmv ~  
                     totalContentMarketing + totalAffiliates + 
                     avgmrp + Sold_price + Promotion + totalunits + 
                     product_analytic_verticalDock + 
                     product_analytic_verticalDockingStation + product_analytic_verticalHiFiSystem + 
                     product_analytic_verticalHomeAudioSpeaker +  
                     totalItemTypeMassMarket, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

final_HomeAudioAcc_model <- model_12

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9971113

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9, model_10)
rm(model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18)

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

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_Koyck), 0.9*nrow(CameraAccessory_Grouped_Koyck))

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

model_2 <- train(gmv ~ totalTV + totalDigital + totalSponsorship + 
                   totalContentMarketing + totalOnlineMarketing + totalSEM + 
                   totalRadio + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + avgsla + week + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalOnlineMarketing
model_3 <- train(gmv ~ totalTV + totalDigital + totalSponsorship + 
                   totalContentMarketing + totalSEM + 
                   totalRadio + totalOther + avgmrp + Sold_price + Promotion + 
                   totalunits + avgsla + week + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing avgmrp
model_4 <- train(gmv ~ totalTV + totalDigital + totalSponsorship + 
                   totalContentMarketing + totalSEM + 
                   totalRadio + totalOther + Sold_price + Promotion + 
                   totalunits + avgsla + week + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalSponsorship
model_5 <- train(gmv ~ totalTV + totalDigital +  
                   totalContentMarketing + totalSEM + 
                   totalRadio + totalOther + Sold_price + Promotion + 
                   totalunits + avgsla + week + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing week
model_6 <- train(gmv ~ totalTV + totalDigital +  
                   totalContentMarketing + totalSEM + 
                   totalRadio + totalOther + Sold_price + Promotion + 
                   totalunits + avgsla + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalOther
model_7 <- train(gmv ~ totalTV + totalDigital +  
                   totalContentMarketing + totalSEM + 
                   totalRadio + Sold_price + Promotion + 
                   totalunits + avgsla + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalContentMarketing
model_8 <- train(gmv ~ totalTV + totalDigital +  
                   totalSEM + 
                   totalRadio + Sold_price + Promotion + 
                   totalunits + avgsla + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalRadio
model_9 <- train(gmv ~ totalTV + totalDigital +  
                   totalSEM + 
                   Sold_price + Promotion + 
                   totalunits + avgsla + totalItemTypeMassMarket + totalEvent + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing totalEvent
model_10 <- train(gmv ~ totalTV + totalDigital +  
                   totalSEM + 
                   Sold_price + Promotion + 
                   totalunits + avgsla + totalItemTypeMassMarket + 
                   gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing Promotion
model_11 <- train(gmv ~ totalTV + totalDigital +  
                    totalSEM + 
                    Sold_price + 
                    totalunits + avgsla + totalItemTypeMassMarket + 
                    gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing avgsla
model_12 <- train(gmv ~ totalTV + totalDigital +  
                    totalSEM + 
                    Sold_price + 
                    totalunits + totalItemTypeMassMarket + 
                    gmv_lag, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

## Koyck Model for Camera Accessory gmv
Final_CameraAcc_model <- model_12

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9886561
# gmv lag is not significant - hence Koyck model is not relevent in this scenario

########################################################################################
# 3b. Koyck Model - Game Accessory
########################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_Koyck), 0.9*nrow(GamingAccessory_Grouped_Koyck))

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

model_2 <- lm(formula = gmv ~ TotalInvestment + totalSponsorship + totalContentMarketing + 
                totalOnlineMarketing + totalAffiliates + totalSEM + totalRadio + 
                totalOther + avgmrp + Promotion + totalunits + week + avgNPSScore + 
                totalorder_payment_typePrepaid + totalItemTypeMassMarket, data = train_gameAcces)
summary(model_2)

# gmv lag is not significant - hence Koyck model is not relevent in this scenario
########################################################################################

########################################################################################
# 3c. Koyck Model - Home Audio
########################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_Koyck), 0.9*nrow(HomeAudio_Grouped_Koyck))

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

# gmv lag is not significant - hence Koyck model is not relevent in this scenario

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7)
rm(model_8,model_9,model_10,model_11,model_12)
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

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag), 0.9*nrow(CameraAccessory_Grouped_distributedlag))

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
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag_new), 0.9*nrow(CameraAccessory_Grouped_distributedlag_new))

train_camAcces <- CameraAccessory_Grouped_distributedlag_new[indices,]
test_camAcces <- CameraAccessory_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Removing totalEvent
model_2 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing + totalAffiliates +
                   totalSEM + totalunits + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalorder_payment_typePrepaid
model_3 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing + totalAffiliates +
                   totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalAffiliates
model_4 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalOnlineMarketing
model_5 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing +
                   totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing totalContentMarketing
model_6 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship +
                   totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalSponsorship
model_7 <- train(gmv ~  TotalInvestment + totalTV + totalDigital +
                   totalSEM + totalunits + avgNPSScore + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing avgNPSScore
model_8 <- train(gmv ~  TotalInvestment + totalTV + totalDigital +
                   totalSEM + totalunits + totalItemTypeMassMarket + 
                   gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

## Distributed Lag Model (Additive) for Camera Accessory gmv
Final_CameraAcc_model <- model_8

## Checking the acuracy of the Prediction model
Predict_1 <- predict(Final_CameraAcc_model,test_camAcces[,-1])
test_camAcces$predict_gmv <- Predict_1

r <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)
rsquared <- cor(test_camAcces$gmv,test_camAcces$predict_gmv)^2
rsquared
# rsquared = 0.9445533
# as this does not have any significant lag variable - this model is not relevent

##############################################################################################
##############################################################################################
# 4b. Distributed lag Model (Additive) - Game Accessory
##############################################################################################
##############################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag), 0.9*nrow(GamingAccessory_Grouped_distributedlag))

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
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag_new), 0.9*nrow(GamingAccessory_Grouped_distributedlag_new))

train_gameAcces <- GamingAccessory_Grouped_distributedlag_new[indices,]
test_gameAcces <- GamingAccessory_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Removing totalDigital
model_2 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + totalEvent + gmv_Lag1 +                      
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalunits_Lag2
model_3 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + totalEvent + gmv_Lag1 +                      
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalorder_payment_typePrepaid
model_4 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + totalEvent + gmv_Lag1 +                      
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalOnlineMarketing
model_5 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + totalEvent + gmv_Lag1 +                      
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing totalorder_payment_typePrepaid_Lag1
model_6 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + totalEvent + gmv_Lag1 +                      
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing gmv_Lag1
model_7 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalAffiliates +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + totalEvent +                      
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalAffiliates
model_8 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing +
                   totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + totalEvent +                      
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing week
model_9 <- train(gmv ~  TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing +
                   totalSEM + totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + totalEvent +                      
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing totalTV
model_10 <- train(gmv ~  TotalInvestment + totalDigital + totalSponsorship + totalContentMarketing +
                   totalSEM + totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + totalEvent +                      
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing totalSponsorship
model_11 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + totalEvent +                      
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing avgNPSScore
model_12 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket + totalEvent +                      
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

# Removing totalEvent
model_13 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_13)

# Removing totalItemTypeMassMarket_Lag2
model_14 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_14)

# Removing totalItemTypeMassMarket_Lag1
model_15 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_15)

# Removing totalorder_payment_typePrepaid_Lag2
model_16 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2 + avgsla_Lag3, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_16)

# Removing avgsla_Lag3
model_17 <- train(gmv ~  TotalInvestment + totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_17)

# Removing TotalInvestment
model_18 <- train(gmv ~  totalDigital + totalContentMarketing +
                    totalSEM + totalRadio + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_18)

# Removing totalRadio
model_19 <- train(gmv ~  totalDigital + totalContentMarketing +
                    totalSEM + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_19)

# Removing totalContentMarketing
model_20 <- train(gmv ~  totalDigital + 
                    totalSEM + totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_20)

# Removing totalSEM
model_21 <- train(gmv ~  totalDigital + 
                    totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_21)

# Removing totalDigital
model_22 <- train(gmv ~   
                    totalunits + totalItemTypeMassMarket +                       
                    totalunits_Lag1 + gmv_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_22)

## Distributed Lag Model for Game Accessory gmv
final_GameAcc_model <- model_22

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.6662461

##############################################################################################
##############################################################################################
# 4c. Distributed lag Model (Additive) - Home Audio
##############################################################################################
##############################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag), 0.9*nrow(HomeAudio_Grouped_distributedlag))

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
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag_new), 0.9*nrow(HomeAudio_Grouped_distributedlag_new))

train_homeAudio <- HomeAudio_Grouped_distributedlag_new[indices,]
test_homeAudio <- HomeAudio_Grouped_distributedlag_new[-indices,]

model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Removing gmv_Lag1 
model_2 <- train(gmv ~  totalTV + totalOnlineMarketing + totalAffiliates +
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +  totalItemTypeMassMarket_Lag1 +                      
                   gmv_Lag2 + totalunits_Lag2 + totalItemTypeMassMarket_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalTV 
model_3 <- train(gmv ~   totalOnlineMarketing + totalAffiliates +
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +  totalItemTypeMassMarket_Lag1 +                      
                   gmv_Lag2 + totalunits_Lag2 + totalItemTypeMassMarket_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalItemTypeMassMarket_Lag2 
model_4 <- train(gmv ~   totalOnlineMarketing + totalAffiliates +
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +  totalItemTypeMassMarket_Lag1 +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalOnlineMarketing 
model_5 <- train(gmv ~   totalAffiliates +
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +  totalItemTypeMassMarket_Lag1 +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing totalAffiliates 
model_6 <- train(gmv ~   
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +  totalItemTypeMassMarket_Lag1 +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalItemTypeMassMarket_Lag1 
model_7 <- train(gmv ~   
                   totalSEM + avgmrp + Promotion + totalunits + totalorder_payment_typePrepaid + totalItemTypeMassMarket +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalorder_payment_typePrepaid 
model_8 <- train(gmv ~   
                   totalSEM + avgmrp + Promotion + totalunits + totalItemTypeMassMarket +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalItemTypeMassMarket 
model_9 <- train(gmv ~   
                   totalSEM + avgmrp + Promotion + totalunits +                      
                   gmv_Lag2 + totalunits_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

final_HomeAudioAcc_model <- model_9

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9988668

rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13,model_14,model_15,model_16,model_17)
rm(model_18,model_19,model_20,model_21,model_22)
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

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Grouped_distributedlag_ln), 0.9*nrow(CameraAccessory_Grouped_distributedlag_ln))

train_camAcces <- CameraAccessory_Grouped_distributedlag_ln[indices,]
test_camAcces <- CameraAccessory_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_camAcces)
summary(model_1)

# Removing totalorder_payment_typePrepaid 
model_2 <- train(gmv ~ TotalInvestment + totalTV + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalAffiliates + totalSEM + totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalTV 
model_3 <- train(gmv ~ TotalInvestment + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalAffiliates + totalSEM + totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalAffiliates 
model_4 <- train(gmv ~ TotalInvestment + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalSEM + totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalSEM 
model_5 <- train(gmv ~ TotalInvestment + totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing TotalInvestment 
model_6 <- train(gmv ~ totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalDigital 
model_7 <- train(gmv ~  totalSponsorship + totalContentMarketing + totalOnlineMarketing +
                   totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalSponsorship 
model_8 <- train(gmv ~  totalContentMarketing + totalOnlineMarketing +
                   totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent + gmv_Lag1, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing gmv_Lag1 
model_9 <- train(gmv ~  totalContentMarketing + totalOnlineMarketing +
                   totalunits + avgNPSScore +  totalItemTypeMassMarket +                   
                   totalEvent, train_camAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# as this does not have any significant lag variable - this model is not relevent

##############################################################################################
# 5b. Distributed lag Model (Multiplicative) - Game Accessory
##############################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Grouped_distributedlag_ln), 0.9*nrow(GamingAccessory_Grouped_distributedlag_ln))

train_gameAcces <- GamingAccessory_Grouped_distributedlag_ln[indices,]
test_gameAcces <- GamingAccessory_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_gameAcces)
summary(model_1)

# Removing totalEvent 
model_2 <- train(gmv ~ TotalInvestment + totalTV +totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +  
                   totalAffiliates + totalSEM + totalRadio + totalunits + week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + week_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing week_Lag1 
model_3 <- train(gmv ~ TotalInvestment + totalTV +totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +  
                   totalAffiliates + totalSEM + totalRadio + totalunits + week + avgNPSScore + totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalorder_payment_typePrepaid 
model_4 <- train(gmv ~ TotalInvestment + totalTV +totalDigital + totalSponsorship + totalContentMarketing + totalOnlineMarketing +  
                   totalAffiliates + totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing totalDigital 
model_5 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing + totalOnlineMarketing +  
                   totalAffiliates + totalSEM + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing totalSEM 
model_6 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing + totalOnlineMarketing +  
                   totalAffiliates + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalOnlineMarketing 
model_7 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing +  
                   totalAffiliates + totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalAffiliates 
model_8 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing +  
                   totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + totalItemTypeMassMarket_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalItemTypeMassMarket_Lag2 
model_9 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing +  
                   totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalorder_payment_typePrepaid_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing totalorder_payment_typePrepaid_Lag1 
model_10 <- train(gmv ~ TotalInvestment + totalTV + totalSponsorship + totalContentMarketing +  
                   totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                   totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing totalTV 
model_11 <- train(gmv ~ TotalInvestment  + totalSponsorship + totalContentMarketing +  
                    totalRadio + totalunits + week + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

# Removing week 
model_12 <- train(gmv ~ TotalInvestment  + totalSponsorship + totalContentMarketing +  
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_12)

# Removing totalContentMarketing 
model_13 <- train(gmv ~ TotalInvestment  + totalSponsorship +   
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2 + avgsla_Lag3 , train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_13)

# Removing avgsla_Lag3 
model_14 <- train(gmv ~ TotalInvestment  + totalSponsorship +   
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_14)

# Removing totalSponsorship 
model_15 <- train(gmv ~ TotalInvestment +   
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_15)

# Removing TotalInvestment 
model_16 <- train(gmv ~    
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 + totalorder_payment_typePrepaid_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_16)

# Removing totalorder_payment_typePrepaid_Lag2 
model_17 <- train(gmv ~    
                    totalRadio + totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_17)

# Removing totalRadio 
model_18 <- train(gmv ~    
                    totalunits + avgNPSScore + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_18)

# Removing avgNPSScore 
model_19 <- train(gmv ~    
                    totalunits + totalItemTypeMassMarket + gmv_Lag1 +                
                    totalunits_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2, train_gameAcces, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_19)

## Distributed Lag Model (multiplicative) for Game Accessory gmv
final_GameAcc_model <- model_19

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_GameAcc_model,test_gameAcces[,-1])
test_gameAcces$predict_gmv <- Predict_1

r <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)
rsquared <- cor(test_gameAcces$gmv,test_gameAcces$predict_gmv)^2
rsquared
# rsquared = 0.6703744

##############################################################################################
# 5c. Distributed lag Model (Multiplicative) - Home Audio
##############################################################################################

# Divide the data in 90:10 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Grouped_distributedlag_ln), 0.9*nrow(HomeAudio_Grouped_distributedlag_ln))

train_homeAudio <- HomeAudio_Grouped_distributedlag_ln[indices,]
test_homeAudio <- HomeAudio_Grouped_distributedlag_ln[-indices,]

# Develop the first model 
model_1 <-lm(gmv~.,data=train_homeAudio)
summary(model_1)

# Removing totalTV 
model_2 <- train(gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2 +                
                   totalItemTypeMassMarket_Lag2, train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_2)

# Removing totalItemTypeMassMarket_Lag2 
model_3 <- train(gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + totalItemTypeMassMarket_Lag1 + gmv_Lag2 + totalunits_Lag2                 
                   , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_3)

# Removing totalItemTypeMassMarket_Lag1 
model_4 <- train(gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag1 + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_4)

# Removing gmv_Lag1 
model_5 <- train(gmv ~ totalOnlineMarketing + totalAffiliates + totalSEM + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_5)

# Removing totalAffiliates 
model_6 <- train(gmv ~ totalOnlineMarketing + totalSEM + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_6)

# Removing totalSEM 
model_7 <- train(gmv ~ totalOnlineMarketing + avgmrp + Promotion + totalunits +  
                   totalorder_payment_typePrepaid + totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_7)

# Removing totalorder_payment_typePrepaid 
model_8 <- train(gmv ~ totalOnlineMarketing + avgmrp + Promotion + totalunits +  
                   totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_8)

# Removing totalOnlineMarketing 
model_9 <- train(gmv ~ avgmrp + Promotion + totalunits +  
                   totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_9)

# Removing Promotion 
model_10 <- train(gmv ~ avgmrp + totalunits +  
                   totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                 , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_10)

# Removing avgmrp 
model_11 <- train(gmv ~ totalunits +  
                    totalItemTypeMassMarket + gmv_Lag2 + totalunits_Lag2                 
                  , train_homeAudio, method = "lm", trControl = trainControl(method="cv", number = 5, verboseIter = TRUE) )
summary(model_11)

final_HomeAudioAcc_model <- model_11

## Checking the acuracy of the Prediction model
Predict_1 <- predict(final_HomeAudioAcc_model,test_homeAudio[,-1])
test_homeAudio$predict_gmv <- Predict_1

r <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)
rsquared <- cor(test_homeAudio$gmv,test_homeAudio$predict_gmv)^2
rsquared
# rsquared = 0.9822208

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


