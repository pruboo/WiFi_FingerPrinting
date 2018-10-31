#### 0. Load Libraries and Import Dataset ####

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load(caret, readr, plyr, dplyr, tidyr, tidyverse, ggplot2, lubridate,
               ggmap, scatterplot3d, parallel, doParallel, doSNOW, randomForest)

# load training data #
trainingData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/trainingData.csv")
dim(trainingData)

# load validation data #
validationData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/validationData.csv")
dim(validationData)

# wifi_trainingData #
#wifi_trainData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/trainingData.csv", na = '100')
#dim(wifi_trainData)

#### 1. Data Exploration and Visualization ####

## 1.1 Google Map of Building

# latitude and longitude coordinates
location = c(Lat = -0.067232, Lon = 39.992631)
# satellite image
uni_exterior <- get_map(location = location, 
                        zoom = 20, 
                        source = "google", 
                        maptype = "satellite", 
                        scale = 'auto',
                        crop = TRUE,
                        color = 'color')
# plot satellite image of buildings in data set
ggmap(myMap, extent = 'panel')

# 3D image of reference points in data set
scatterplot3d(trainingData$LONGITUDE, trainingData$LATITUDE, trainingData$FLOOR,
              type = 'p',
              highlight.3d = FALSE,
              color = 'blue',
              angle = 155,
              pch = 16,
              box = FALSE,
              main = "Location Reference Points Across Three Buildings",
              cex.lab = 1,
              cex.main = 1,
              cex.sub = 1,
              col.sub = 'blue',
              xlab = 'Longitude', ylab = 'Latitude', zlab = 'Building Floor')

# 'SPACEID' is used in all buildings (not unique to one particular bldg)
ggplot(trainingData, aes(as.character(BUILDINGID), as.character(SPACEID))) +
  geom_point()+
  xlab("BUILDING #")+
  ylab("SPACE ID")+
  ggtitle("Space ID vs Building")

# remove columns that only have NA's
wifi_trainData <- wifi_trainData[,colSums(is.na(wifi_trainData))<nrow(wifi_trainData)]

# remove rows that only have NA's
wifi_trainData <- wifi_trainData[rowSums(is.na(wifi_trainData[,1:460])) != ncol(wifi_trainData[,1:460]),]

# now convert NA's to -105 dBM
wifi_trainData[is.na(wifi_trainData)] <- -105
summary(wifi_trainData[450:474])

### change all WAP's to numerical ###
wifi_trainData2 <- wifi_trainData
wifi_trainData2 <- sapply(wifi_trainData2, as.numeric)
wifi_trainData2 <- as.tibble(wifi_trainData2)
str(wifi_trainData2[460:474])

wifi_trainData2$FLOOR <- factor(wifi_trainData$FLOOR, 
                                levels = c(0, 1, 2, 3, 4), 
                                labels = c(1, 2, 3, 4, 5))

wifi_trainData2$BUILDINGID <- factor(wifi_trainData$BUILDINGID,
                                     levels = c(0, 1, 2),
                                     labels = c(1, 2, 3))

wifi_trainData2$SPACEID <- as.factor(wifi_trainData$SPACEID)

wifi_trainData2$RELATIVEPOSITION <- factor(wifi_trainData$RELATIVEPOSITION,
                                           levels = c(1, 2),
                                           labels = c("inside", "outside"))

wifi_trainData2$USERID <- as.factor(wifi_trainData$USERID)
wifi_trainData2$PHONEID <- as.factor(wifi_trainData$PHONEID)
wifi_trainData2$TIMESTAMP <- as_datetime(wifi_trainData$TIMESTAMP)

# count number of valid WAP's per row [1:520]
wifi_trainData2$WAP_num <- apply(wifi_trainData[,1:465], 1,
                                function(x) length(which(!is.na(x))))
sum(is.na(wifi_trainData2$WAP_num))
glimpse(wifi_trainData2[,1:465])
glimpse(wifi_trainData2[,466:475])
summary(wifi_trainData2$WAP_num)

# create a new location ID for tagging
wifi_trainData2$ID <- wifi_trainData2 %>%
  group_indices(BUILDINGID, FLOOR, SPACEID)
glimpse(wifi_trainData2[,466:476])

# convert 'ID' to categorical
wifi_trainData2$ID <- as.factor(wifi_trainData2$ID)

# create data frame for frequency per location
ID_freq <- as.data.frame(table(wifi_trainData2$ID))

# plot histogram of instance counts at locations
ggplot(ID_freq, aes(x = Freq)) +
  geom_histogram(fill='green', binwidth = 2, color='black')+
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency Count of Location ID Instances') +
  xlab('Number of Instances for a Loacation ID') +
  ylab('Frequency of Observed Instance Count') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))

# distribution of WAP count by building using boxplot
ggplot(wifi_trainData2, aes(x=BUILDINGID, y=WAP_num)) + 
  geom_boxplot(fill='lightblue') +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Detected Wireless Access Points by Building') +
  labs(x="Building Number", y= 'WAP Counts' ) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#### 2. Predicting 'Building' ####

## 2.1 Preprocess Data

# change all +100 dBm (no WAP detected) to -105 #
# WAP range: -104dBm (extremely poor signal) to 0dbM #
#trainingData[trainingData==100] <- -105
#validationData[validationData==100] <- -105 ### try using grep() ###

# create a new data set for predicting building #
#td_building <- trainingData
#summary(td_building)
#glimpse(td_building[,510:529])

# convert features to numeric #
#td_building <- sapply(td_building, as.numeric)

# convert matrix to tibble #
#td_building <- as.tibble(td_building)

## 2.2 Convert Data Types
#td_building %>%
  #mutate_at(FLOOR = as.factor(FLOOR),
         #BUILDINGID = as.factor(BUILDINGID),
         #SPACEID = as.factor(SPACEID),
         #RELATIVEPOSITION = as.factor(RELATIVEPOSITION),
         #USERID = as.factor(USERID),
         #PHONEID = as.factor(PHONEID),
         #TIMESTAMP = as_datetime(TIMESTAMP))

#td_building$FLOOR <- as.factor(td_building$FLOOR)
#td_building$BUILDINGID <- as.factor(td_building$BUILDINGID)
#td_building$SPACEID <- as.factor(td_building$SPACEID)
#td_building$RELATIVEPOSITION <- as.factor(td_building$RELATIVEPOSITION)
#td_building$USERID <- as.factor(td_building$USERID)
#td_building$PHONEID <- as.factor(td_building$PHONEID)
#td_building$TIMESTAMP <- as_datetime(td_building$TIMESTAMP)

#glimpse(td_building[,510:531])

## 2.3 Feature Engineering

# create a new 'max' column with highest RSSI and 'max_id' # [1:465] = WAP columns
dim(wifi_trainData2)
wifi_trainData2$max <- apply(wifi_trainData2[,1:465], 1, max)
wifi_trainData2$max_id <- apply(wifi_trainData2[,1:465], 1, function(x) names(which.max(x)))
glimpse(wifi_trainData2[,466:477])
summary(wifi_trainData2$max)

#library(dplyr)
#td_building %>%
  #summarise(Score = max(Score))

# use apply to find if any of the rows has zero values #
wifi_trainData3 <- wifi_trainData2 %>%
  filter(apply(wifi_trainData2[1:465], 1, function(x) any(x==0)))

summary(wifi_trainData3[460:478])
as_tibble(td_building_try2)

## user ID 6 made 118/520 mistakes
## explore different wifi strengths and set a 'range' ##


glimpse(validationData[510:529])


#### WIFI 2.0 ####

#### 0. LOAD DATA ####
td <- trainingData
vd <- validationData

### 0.1 Change Data Types for Training and Validation
td$TIMESTAMP <- as_datetime(td$TIMESTAMP)
td$FLOOR <- as.factor(td$FLOOR)
td$BUILDINGID <- as.factor(td$BUILDINGID)
td$SPACEID <- as.factor(td$USERID)
td$RELATIVEPOSITION <- as.factor(td$RELATIVEPOSITION)
td$USERID <- as.factor(td$USERID)
td$PHONEID <- as.factor(td$PHONEID)
str(td[520:529])

### 0.2 The Same for Validation
vd$TIMESTAMP <- as_datetime(vd$TIMESTAMP)
vd$FLOOR <- as.factor(vd$FLOOR)
vd$BUILDINGID <- as.factor(vd$BUILDINGID)
vd$SPACEID <- as.factor(vd$USERID)
vd$RELATIVEPOSITION <- as.factor(vd$RELATIVEPOSITION)
vd$USERID <- as.factor(vd$USERID)
vd$PHONEID <- as.factor(vd$PHONEID)
str(vd[520:529])

#### 1. PREPROCESS ####

### 1.1 Change WAP Values from +100 to -105
td <- distinct(td)
td[td==100] <- -105

vd <- distinct(vd)
vd[vd==100] <- 105

### 1.2 Count Values != +100/-105
td$count <- apply(td[,1:520], 1, function(x) length(which(x!=-105))) # count how many are not -105
td$max <- apply(td[,1:520], 1, max) # max WAP
td$max2 <- apply(td[,1:520],1,function(x) names(td[,1:520])[which(x==max(x))]) # returns the max WAP name(s)
td$max3 <- apply(td[1:520],1,function(x) names(which.max(x))) # returns only one highest WAP w. name

### 1.3 The Same for Validation
vd$count <- apply(vd[,1:520], 1, function(x) length(which(x!=-105)))
vd$max <- apply(vd[,1:520], 1, max)
vd$max2 <- apply(vd[,1:520],1,function(x) names(vd[,1:520])[which(x==max(x))])
vd$max3 <- apply(vd[1:520],1,function(x) names(which.max(x)))

### 1.4 Remove Redundant Rows
td <- subset(td, td$max!=0 & td$count!=0)

####
td<-subset(td,td$max<=-30 & td$max>=-80)

### 1.5 Remove 1-Value Columns
waps_td <- td[,c(1:520)]
useless_waps <- apply(waps_td, 2, function(x) length(unique(x))==1)
td_new <- td[,-c(which(useless_waps==TRUE))]

waps_vd <- vd[,c(1:520)]
useless_waps_vd <- apply(waps_vd, 2, function(x) length(unique(x))==1)
vd_new <- vd[,-c(which(useless_waps_vd==TRUE))]

### 1.6 Identify WAPS in Training
Waps_td_names <- grep("WAP", names(td_new), value = TRUE)

### 1.7 The Same for Validation
Waps_vd_names <- grep("WAP", names(vd_new), value = TRUE)

### only non-overlapping data from td and vd
Waps_tdvd <- intersect(Waps_td_names, Waps_vd_names) # joining td and vd

### 1.8 Remove Columns
x <- names(td_new[Waps_td_names]) %in% Waps_tdvd == FALSE
td_new_2 <- td_new[-which(x)]

y <- names(vd_new[Waps_vd_names]) %in% Waps_tdvd  == FALSE
vd_new_2 <- vd_new[-which(y)]

####

#### 2. BUILDING PREDICTION #### td_new_2
detectCores()
clusterF1 <- makeCluster(detectCores()-1)
registerDoSNOW(clusterF1)

td_build <- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max2,max,count,SPACEID,FLOOR,
                               RELATIVEPOSITION,LATITUDE,LONGITUDE,max3))

## Model
set.seed(147)
fitControl <- trainControl(method = "repeatedcv", number=3,repeats = 3, 
                           verboseIter = TRUE, allowParallel = TRUE)

## knn
knnFit <- train(BUILDINGID~., data = td_build, method = "knn",
                metric = "Accuracy", trControl = fitControl, preProcess = c("zv", "center", "scale"))

plot(knnFit)
knnFit

predict.knn <- predict(knnFit ,vd_new_2)
postResample(predict.knn , vd_new_2$BUILDINGID)
ConfusionMatrix<-confusionMatrix(vd_new_2$BUILDINGID , predict.knn)

ConfusionMatrix
save(knnFit, file = "knnFit.rda")
load("knnFit.rda")
rm(ConfusionMatrix)

#svm
SvmFit<-caret::train(BUILDINGID~., data= td_build, method="svmLinear", 
                     trControl=fitControl)

SvmFit
predict.svm <- predict(SvmFit ,vd_new_2)
predict.svm2 <- predict(SvmFit ,td_new_2)
postResample(predict.svm , vd_new_2$BUILDINGID)
confusionMatrix(vd_new_2$BUILDINGID , predict.svm)
rm(confusionMatrix)
confusionMatrix
save(SvmFit, file = "SvmFit.rda")
load("SvmFit.rda")
#add prediction column in training dataset 
td_new_2$build_prediction<-predict.svm2
vd_new_2$build_prediction<-predict.svm

#make building+floor ID in td and vd
td_new_2$B_fID<-as.factor(paste(td_new_2$BUILDINGID,td_new_2$FLOOR))
vd_new_2$B_fID<-as.factor(paste(vd_new_2$BUILDINGID,vd_new_2$FLOOR))

####FLOOR ####
td_floor<- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max_2,count,SPACEID,BUILDINGID,
                              RELATIVEPOSITION,LATITUDE,LONGITUDE,max,max3))
#build.0 <- filter(td_floor, build_prediction == 0)
#build.1 <- filter(td_floor, build_prediction == 1)
#build.2 <- filter(td_floor, build_prediction== 2)
#knn
fitControl <- trainControl(method = "repeatedcv", number=3,repeats = 3, 
                           verboseIter = TRUE, allowParallel = TRUE)
## knn
knnFit_floor <- train(FLOOR~.,data = td_floor,method = "knn",
                      metric = "Accuracy",trControl = fitControl,preProcess= c("center", "scale"))

#build.0$FLOOR<- factor(build.0$FLOOR)

plot(knnFit_floor)

knnFit_floor



predict.knn_floor <- predict(knnFit_floor ,vd_new_2) #(model name, data set)
postResample(predict.knn_floor , vd_new_2$FLOOR)
ConfusionMatrix<-confusionMatrix(vd_new_2$FLOOR , predict.knn_floor)
ConfusionMatrix
save(knnFit_floor, file = "knnFit_floor.rda")
load("knnFit_floor.rda")
#svm
SvmFit_floor<-caret::train(FLOOR~., data= td_floor, method="svmLinear", 
                           trControl=fitControl,preProcess= c("center", "scale"))
SvmFit_floor
predict.svm_floor <- predict(SvmFit_floor ,vd_new_2)
predict.svm_floor2 <- predict(SvmFit_floor ,td_new_2)
postResample(predict.svm_floor , vd_new_2$FLOOR)
confusionMatrix(vd_new_2$FLOOR , predict.svm_floor)
rm(confusionMatrix)
save(SvmFit_floor, file = "SvmFit_floor.rda")

td_new_2$floor_prediction<-predict.svm_floor2
vd_new_2$floor_prediction<-predict.svm_floor


#Random Forest 
WAP_floor <- grep("WAP", names(td_floor), value=T)
bestmtry<-tuneRF(td_floor[WAP], td_floor$FLOOR, ntreeTry=100, stepFactor=2, 
                 improve=0.05,trace=TRUE, plot=T)

system.time(RF_floor<-randomForest(FLOOR~.,
                                   data= td_floor2, 
                                   importance=T,maximize=T,
                                   method="rf", trControl=fitControl,
                                   ntree=100, mtry=52,allowParalel=TRUE))
save(RF_floor, file = "RF_floor.rda")
load("RF_floor.rda")
RF_floor
predict.rf_floor <- predict(RF_floor ,vd_new_2)
predict.rf_floor2 <- predict(RF_floor ,td_new_2)
postResample( predict.rf_floor , vd_new_2$FLOOR)

#add prediction column in training dataset 
td_new_2$floor_prediction<-predict.rf_floor2
vd_new_2$floor_prediction<-predict.rf_floor
# Add dummy variable for BuildingID&floor id 
DummyVar <- dummyVars("~BUILDINGID", data = td_new_2, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = td_new_2))
td_new_2<-cbind(td_new_2, DummyVarDF)


DummyVar2 <- dummyVars("~BUILDINGID", data = vd_new_2, fullRank=T)
DummyVarDF2 <- data.frame(predict(DummyVar2, newdata = vd_new_2))
vd_new_2<-cbind(vd_new_2, DummyVarDF2)

td_floor2<- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max_2,count,SPACEID,BUILDINGID,
                               RELATIVEPOSITION,LATITUDE,LONGITUDE,max,max3,build_prediction))
####DUMMY FOR FLOOR ####
DummyVar3 <- dummyVars("~FLOOR", data = vd_new_2, fullRank=T)
DummyVarDF3 <- data.frame(predict(DummyVar3, newdata = vd_new_2))
vd_new_2<-cbind(vd_new_2, DummyVarDF3)

DummyVar4 <- dummyVars("~FLOOR", data = td_new_2, fullRank=T)
DummyVarDF4 <- data.frame(predict(DummyVar4, newdata = td_new_2))
td_new_2<-cbind(td_new_2, DummyVarDF4)

#dummy for predicted floors
DummyVar5 <- dummyVars("~floor_prediction", data = td_new_2, fullRank=T)
DummyVarDF5 <- data.frame(predict(DummyVar5, newdata = td_new_2))
td_new_2<-cbind(td_new_2, DummyVarDF5)

DummyVar6 <- dummyVars("~floor_prediction", data = vd_new_2, fullRank=T)
DummyVarDF6 <- data.frame(predict(DummyVar6, newdata = vd_new_2))
vd_new_2<-cbind(vd_new_2, DummyVarDF6)

#LONGITUTDE 
td_lon<- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max_2,count,SPACEID,floor_prediction,
                            RELATIVEPOSITION,build_prediction,BUILDINGID,LATITUDE,max,max3,FLOOR,
                            FLOOR.1,FLOOR.2,FLOOR.3,FLOOR.4))
#Random Forest 
WAP_lon <- grep("WAP", names(td_lon), value=T)
bestmtry<-tuneRF(td_lon[WAP_lon], td_lon$LONGITUDE, ntreeTry=100, stepFactor=2, 
                 improve=0.05,trace=TRUE, plot=T)

system.time(RF_lon<-randomForest(LONGITUDE~.,
                                 data= td_lon, 
                                 importance=T,maximize=T,
                                 method="rf", trControl=fitControl,
                                 ntree=100, mtry=52,allowParalel=TRUE))

save(RF_lon, file = "RF_lon.rda")
RF_lon

predict.rf_lon <- predict(RF_lon ,vd_new_2)
postResample( predict.rf_lon , vd_new_2$LONGITUDE)

##SVM
td_lon2<- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max_2,count,SPACEID,
                             RELATIVEPOSITION,BUILDINGID,LATITUDE,max,max3,FLOOR,
                             FLOOR.1,FLOOR.2,FLOOR.3,FLOOR.4,floor_prediction.1,floor_prediction.2,
                             floor_prediction.3,floor_prediction.4,BUILDINGID.1,BUILDINGID.2))
#SVM 
SvmFit_lon<-caret::train(LONGITUDE~., data= td_lon, method="svmLinear", 
                         trControl=fitControl,preProcess= c("center", "scale"))
SvmFit_lon
predict.svm_lon <- predict(SvmFit_lon ,vd_new_2)
predict.svm_lon2 <- predict(SvmFit_lon ,td_new_2)
postResample(predict.svm_lon , vd_new_2$LONGITUDE)
confusionMatrix(vd_new_2$LONGITUDE , predict.svm_lon)
rm(confusionMatrix)
save(SvmFit_lon, file = "SvmFit_lon.rda")

td_new_2$floor_prediction<-predict.svm_floor2
vd_new_2$floor_prediction<-predict.svm_floor

####LATITUDE ####
td_lat<- select(td_new_2,-c(TIMESTAMP,USERID,PHONEID,max_2,count,SPACEID,floor_prediction,
                            RELATIVEPOSITION,build_prediction,BUILDINGID,LONGITUDE,max,max3,FLOOR))
#Random Forest 
WAP <- grep("WAP", names(td_lat), value=T)
bestmtry<-tuneRF(td_lat[WAP], td_lat$LONGITUDE, ntreeTry=100, stepFactor=2, 
                 improve=0.05,trace=TRUE, plot=T)

system.time(RF_lat<-randomForest(LATITUDE~.,
                                 data= td_lat, 
                                 importance=T,maximize=T,
                                 method="rf", trControl=fitControl,
                                 ntree=100, mtry=52,allowParalel=TRUE))
save(RF_lat, file = "RF_lat.rda")
RF_lat
predict.rf_lat <- predict(RF_lat ,vd_new_2)
postResample( predict.rf_lat , vd_new_2$LATITUDE)

####checking the errors of building prediction ###

td_err<- td_new_2 %>% select(WAP027,WAP028,SPACEID,USERID,PHONEID,
                             RELATIVEPOSITION,LATITUDE,LONGITUDE,FLOOR,BUILDINGID) %>%
  filter(apply(td_new_2[,1:2],1,function(x) any(x!=-105)))


ggplot() +
  geom_point(data = td_new_2, aes(x = LONGITUDE, y = LATITUDE, colour = "Training dataset")) +
  geom_point(data = vd_new_2, aes(x = LONGITUDE, y = LATITUDE, colour = "Test dataset")) +
  ggtitle("Locations (Training and Test sets)") 

