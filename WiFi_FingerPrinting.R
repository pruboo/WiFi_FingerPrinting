#### 0. Load Libraries and Import Dataset ####

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load(readr, plyr, dplyr, tidyr, tidyverse, ggplot2, lubridate, ggmap, scatterplot3d)

# load training data #
trainingData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/trainingData.csv")
dim(trainingData)

# load validation data #
validationData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/validationData.csv")
dim(validationData)

# wifi_trainingData #
wifi_trainData <- read_csv("Dropbox/ubiqum/Task_3.3_Wifi_Locationing/UJIndoorLoc/trainingData.csv", na = '100')

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
scatterplot3d(wifi_trainData$LONGITUDE, wifi_trainData$LATITUDE, wifi_trainData$FLOOR,
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

# change data types
wifi_trainData$FLOOR <- as.factor(wifi_trainData$FLOOR)
wifi_trainData$BUILDINGID <- as.factor(wifi_trainData$BUILDINGID)
wifi_trainData$SPACEID <- as.factor(wifi_trainData$SPACEID)
wifi_trainData$RELATIVEPOSITION <- as.factor(wifi_trainData$RELATIVEPOSITION)
wifi_trainData$USERID <- as.factor(wifi_trainData$USERID)
wifi_trainData$PHONEID <- as.factor(wifi_trainData$PHONEID)
wifi_trainData$TIMESTAMP <- as_datetime(wifi_trainData$TIMESTAMP)

glimpse(wifi_trainData[,510:529])
as_tibble(wifi_trainData)

# count WAP's
wifi_trainData$WAP_num <- apply(wifi_trainData[,1:520], 1,
                                function(x) length(which(!is.na(x))))
glimpse(wifi_trainData[,510:530])

# create a new location ID for tagging
wifi_trainData$ID <- wifi_trainData %>%
  group_indices(BUILDINGID, FLOOR, SPACEID)
glimpse(wifi_trainData[,510:531])
summary(wifi_trainData$ID)

# convert 'ID' to categorical
wifi_trainData$ID <- factor(wifi_trainData$ID)

# create data frame for frequency per location
ID_freq <- as.data.frame(table(wifi_trainData$ID))

# remove columns with all NA's
wifi_trainData <- wifi_trainData[,colSums(is.na(wifi_trainData[1:16608,]))<nrow(wifi_trainData[1:16608,])]

# remove rows with all NA's



#### 2. Predicting 'Building' ####

## 2.1 Preprocess Data

# change all +100 dBm (no WAP detected) to -105 #
# WAP range: -104dBm (extremely poor signal) to 0dbM #
trainingData[trainingData==100] <- -105
validationData[validationData==100] <- -105 ### try using grep() ###

# create a new data set for predicting building #
td_building <- trainingData
summary(td_building)
glimpse(td_building[,510:529])

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

td_building$FLOOR <- as.factor(td_building$FLOOR)
td_building$BUILDINGID <- as.factor(td_building$BUILDINGID)
td_building$SPACEID <- as.factor(td_building$SPACEID)
td_building$RELATIVEPOSITION <- as.factor(td_building$RELATIVEPOSITION)
td_building$USERID <- as.factor(td_building$USERID)
td_building$PHONEID <- as.factor(td_building$PHONEID)
td_building$TIMESTAMP <- as_datetime(td_building$TIMESTAMP)

glimpse(td_building[,510:531])
as_tibble(td_building)

## 2.3 Feature Engineering

# create a new 'max' column with highest RSSI and 'max_id' #
td_building$max <- apply(td_building[,1:520], 1, max)

td_building_try <- td_building
td_building_try$max_id <- apply(td_building_try[,1:520], 1, function(x) names(which.max(x)))
glimpse(td_building_try[,510:531])

#library(dplyr)
#td_building %>%
  #summarise(Score = max(Score))

# use apply to find if any of the rows has zero values #
td_building_try2 <- td_building_try %>%
  filter(apply(td_building_try[1:520], 1, function(x) any(x==0)))

summary(td_building_try2[520:531])
as_tibble(td_building_try2)

## user ID 6 made 118/520 mistakes
## explore different wifi strengths and set a 'range' ##


glimpse(validationData[510:529])











## Nilu ##
####counting the numbers for each RRSI ####
levels=unique(do.call(c,td[,1:521])) #all unique values in df
out <- sapply(levels,function(x)rowSums(td[,1:52]==x)) #count occurrences of x in each row
colnames(out) <- levels
out

#### counting the values which are not equal to 100####
td$count.2 <- apply(td[,1:520], 1, function(x) length(which(x!=100)))
td <- td[!td$count.2==0,]
#removin the repeated rows #
td<-distinct(td)


####Histogram
x <- td_building[,1:520]
x <- stack(x)

x <- x[-grep(-105, x$values),]
hist(x$values, xlab = "WAP strength",
     main = "Distribution of WAPs signal stength (Training set)", col = "red")
