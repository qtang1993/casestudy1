rm(list=ls())
setwd("~/Documents/MSDS/Data Mining/Case Study 1")
library(jsonlite)
library(tibble)
library(dplyr)
library(rgdal)
library(tools)
library(dplyr)
library(ggplot2)
library(readr)
library(raster)
library(MASS)
library(RColorBrewer)
library(kedd)
library(ks)
library(ggmap)
library(ROCR)

yelp_dataset <- read.csv("yelp_dataset.csv")


### .JSON to .CSV FOR YELP DATA ###

# # Convert .JSON to dataset with Urbana Business Data
# yelp <- stream_in(file("yelp_academic_dataset_business.json"))
# yelp_data <- flatten(yelp)
# str(yelp_data)
# yelp_data <- as_data_frame(yelp_data)
# yelp_dataset <- yelp_data %>% filter(city=="Urbana")
# yelp_dataset[,"categories"] <- NULL
# yelp_dataset[,"full_address"] <- NULL
# yelp_businesses <- yelp_dataset
# 
# # Create Backup
# yelp_dataset_backup <- yelp_dataset
# yelp_dataset <- yelp_dataset_backup
# 
# # Create CSV without categories included:
# yelp_dataset <- as.matrix(yelp_dataset)
# write.csv(yelp_dataset, file="yelp_dataset.csv")


# # Convert .JSON to dataset with Urbana Business Review Data
# yelp_review <- stream_in(file("yelp_academic_dataset_review.json"))
# yelp_review_data <- flatten(yelp_review)
# str(yelp_review_data)
# yelp_review_data <- as_data_frame(yelp_review_data)
# 
# # Create Backup
# yelp_review_dataset_backup <- yelp_review_data
# # use if backup needed
# yelp_review_data <- yelp_review_dataset_backup
# 
# # Filter Yelp Review Data
# business <- yelp_dataset[,"business_id"]
# business <- as.data.frame(unlist(business))
# yelp_review_dataset <- filter(yelp_review_data, yelp_review_data$business_id %in% business[,1])
# 
# # Create CSV for yelp reviews included:
# yelp_review_dataset <- as.matrix(yelp_review_dataset)
# write.csv(yelp_review_dataset, file="yelp_dataset_review.csv")

### CLEAN UP YELP DATA ###

# # Merge Yelp Review Data with Yelp Data and Clean up Yelp Master Data
# yelp_master <- merge(yelp_review_dataset, yelp_dataset, by="business_id")
# names(yelp_master)[c(4,7,20,22)] <- c("review_stars", "review_type", "business_stars", "business_type")
# yelp_master <- select(yelp_master, -starts_with("attributes.Dietary"), -starts_with("attributes.Hair"), -starts_with("attributes.Ambience"), -starts_with("neighborhoods"))
# # get rid of any NAs in the longitude/latitude 
# yelp_master <- yelp_master[complete.cases(yelp_master$longitude),]
# yelp_master <- yelp_master[complete.cases(yelp_master$latitude),]

# change long/lat to meters and bind to crime_data
yelp_master_lonlat = cbind(yelp_master$longitude, yelp_master$latitude)
yelp_master_meters = project(yelp_master_lonlat, proj="+init=epsg:26971")
head(yelp_master_meters)
yelp_master <- cbind(yelp_master, yelp_master_meters[,1], yelp_master_meters[,2])
names(yelp_master)[c(82,83)] <- c("longmeters", "latmeters")

# # Create CSV for master dataset of yelp reviews and businesses:
# yelp_master[,c("categories", "full_address")] <- NULL
# write.csv(yelp_master, file="yelp_master.csv")

### CLEAN UP CRIME DATA ###

# Load the crime data and clean up the crime data
# Filter for years 2005 - 2015
# Add 1 to all crimes
# Remove rows that have NAs in Longitude or Latitude
crime_data <- read.csv("UrbanaData.csv")
crime_data <- filter(crime_data, crime_data$YEAR.OCCURRED > 2004)
crime_data <- cbind(crime_data, 1)
names(crime_data)[c(38, 37, 36)] <- c("response", "long", "lat")
crime_data <- crime_data[complete.cases(crime_data$long),]
crime_data <- crime_data[complete.cases(crime_data$lat),]

# change long/lat to meters and bind to crime_data
crimes.locations.lonlat = cbind(crime_data$long, crime_data$lat)
crimes.locations.meters = project(crimes.locations.lonlat, proj="+init=epsg:26971")
head(crimes.locations.meters)
crime_data <- cbind(crime_data, crimes.locations.meters[,1], crimes.locations.meters[,2])
names(crime_data)[c(39,40)] <- c("longmeters", "latmeters")

# Clean Crime Data so error coordinates are not included 
crime_data <- crime_data[crime_data$lat < 41 & crime_data$lat > 39 & crime_data$long < -87 & crime_data$long > -89, ]

## URBANA CRIME HEAT MAP ##

# Download the base map
urbana <- get_map(location = "Urbana, Illinois", zoom = 14)
# Draw the heat map
map_total <- ggmap(urbana, extent = "device", darken=0.7) + geom_density2d(data = crime_data, aes(x = long, y = lat), size = 0.3) + 
      stat_density2d(data = crime_data, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      geom_point(aes(x=longitude, y=latitude), data=yelp_businesses, col="orange", size = 0.2, alpha=0.4)
map_total

### FILTER CRIME AND YELP DATA BY YEAR ###

# read in yelp data
yelp_master <- read.csv("yelp_master.csv")

crime_data05 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2005)
yelp_data05 <- filter(yelp_master, grepl('2005', date))

crime_data06 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2006)
yelp_data06 <- filter(yelp_master, grepl('2006', date))

crime_data07 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2007)
yelp_data07 <- filter(yelp_master, grepl('2007', date))

crime_data08 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2008)
yelp_data08 <- filter(yelp_master, grepl('2008', date))

crime_data09 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2009)
yelp_data09 <- filter(yelp_master, grepl('2009', date))

crime_data10 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2010)
yelp_data10 <- filter(yelp_master, grepl('2010', date))

crime_data11 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2011)
yelp_data11 <- filter(yelp_master, grepl('2011', date))

crime_data12 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2012)
yelp_data12 <- filter(yelp_master, grepl('2012', date))

crime_data13 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2013)
yelp_data13 <- filter(yelp_master, grepl('2013', date))
# pare down data so each row is a unique business
yelp_data13 <- yelp_data13[!duplicated(yelp_data13[,2]),]

crime_data14 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2014)
yelp_data14 <- filter(yelp_master, grepl('2014', date))
# pare down data so each row is a unique business
yelp_data14 <- yelp_data14[!duplicated(yelp_data14[,2]),]

crime_data15 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2015)
yelp_data15 <- filter(yelp_master, grepl('2015', date))
# pare down data so each row is a unique business
yelp_data15 <- yelp_data15[!duplicated(yelp_data15[,2]),]

### CREATE TRAINING DATA SETS ### 

# Get non-crime data and combine with crime data
# Urbana City Bounds
# "northeast" : {
#   "lat" : 40.1574203,
#   "lng" : -88.1530727
# },
# "southwest" : {
#   "lat" : 40.0732478,
#   "lng" : -88.23302029999999
# }

# Create list of coordinates that span Urbana
long <- seq(-88.23302, -88.15307, length.out = 20000)
lat <- seq(40.07324, 40.15742, length.out = 20000)
urbana_full <- cbind(0, long, lat)
urbana_full <- as.data.frame(urbana_full)
names(urbana_full)[1] <- "response"

# change long/lat to meters and bind to urbana_full
urbana_longlat = cbind(urbana_full$long, urbana_full$lat)
urbana_longlat_meters = project(urbana_longlat, proj="+init=epsg:26971")
head(urbana_longlat_meters)
urbana_full <- cbind(urbana_full, urbana_longlat_meters[,1], urbana_longlat_meters[,2])
names(urbana_full)[c(4,5)] <- c("longmeters", "latmeters")

### TRAIN MODEL ON RESPONSES FROM 2014 ###

# Training Set for 2014 (Non-Crime coordinates and Yelp Data)
# remove crime coordinates from non-crime coordinates
urbana_full14 <- urbana_full[!(urbana_full$long %in% crime_data14$Longitude),]

# create a set of 20,000 points (including crime)
fill <- 20000 - nrow(crime_data14)
urbana_full14 <- urbana_full14[sample(nrow(urbana_full14), fill), ]

# bind the non-crime coordinates with the crime coordinates
head(crime_data14)
train14 <- rbind(urbana_full14, crime_data14[,c('response', 'long', 'lat', 'longmeters', 'latmeters')])

## ADDING PREDICTOR: CRIME DENSITY FOR PREVIOUS YEAR ## 

# get crime densities for 2014 training point based on 2013s data points

h = Hpi(crime_data13[,c("longmeters","latmeters")], pilot="dscalar")
crime_density = kde(crime_data13[,c("longmeters","latmeters")], H=h, eval.points=train14[,c("longmeters","latmeters")])$estimate

# bind crime density as predictor to training data
train14 = cbind(train14, crime_density)
head(train14)

## ADDING PREDICTOR: 2014 % OF BUSINESSES ONLY TAKING CASH WITHIN A 1 MILE RADIUS ## 

# calculate business cash predictors
names(yelp_data14)
# reduce data down to business cash attributes
business_cash14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "attributes.Accepts.Credit.Cards", "longmeters", "latmeters"))]
# remove NAs from cash attribute column
business_cash14 <- business_cash14[complete.cases(business_cash14$attributes.Accepts.Credit.Cards),]
# calculate businesses in 1 mile radius to coordinate
str(business_cash14)
str(train14)

credit <- 0 
train14$cash <- 0
for (i in 1:nrow(train14)){
    distance <- sqrt((train14[i,"longmeters"]-business_cash14[,"longmeters"])^2 + (train14[i,"latmeters"]-business_cash14[,"latmeters"])^2)
    business_radius <- which(distance < 1609.34)
    
    for (j in 1:length(business_radius)) {
      credit <- c(credit, business_cash14[business_radius[j],"attributes.Accepts.Credit.Cards"])
    }
    
    credit_total <- sum(credit)/length(business_radius)
    
    if (is.na(credit_total)) {
      train14$cash[i] <- 0
      credit <- 0
    }
    else {
      train14$cash[i] <- ((1 - credit_total) * 100)
      credit <- 0
    }
}

## ADDING PREDICTOR: 2014 AVERAGE RATINGS OF BUSINESSES IN 1 MILE RADIUS ## 

# calculate business cash predictors

names(yelp_data14)
# reduce data down to business ratings attributes
business_rating14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "business_stars", "longmeters", "latmeters"))]
# remove NAs from ratings attribute column
business_rating14 <- business_rating14[complete.cases(business_rating14$business_stars),]

# calculate average rating of businesses in 250 m 
ratings <- 0 
train14$avg_rating <- 0

for (i in 1:nrow(train14)){
  distance <- sqrt((train14[i,"longmeters"]-business_rating14[,"longmeters"])^2 + (train14[i,"latmeters"]-business_rating14[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  
  for (j in 1:length(business_radius)) {
    ratings <- c(ratings, business_rating14[business_radius[j], "business_stars"])
  }
  
  if (is.na((sum(ratings)/length(business_radius)))) {
    train14$avg_rating[i] <- 0
    ratings <- 0
  }
  else {
    train14$avg_rating[i] <- (sum(ratings)/length(business_radius))
    ratings <- 0
  }
}


## FIT LOGISTIC REGRESSION MODEL ##

## PREDICTORS: % OF CASH ONLY BUSINESSES WITHIN A 1 MILE RADIUS ##
## RESPONSE: LOCATION OF CRIME OR NOT (1 or 0 respectively) ##

str(train14)
logm_14cash2 = glm(response ~ crime_density+cash, data = train14, family=binomial)
logm_14cash = glm(response ~ cash, data = train14, family=binomial)
summary(logm_14cash2)
summary(logm_14cash)

# CRIME DENSITY + CASH AS PREDICTORS
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -2.503e+00  4.128e-02  -60.63   <2e-16 ***
#   crime_density  3.994e+07  7.751e+05   51.53   <2e-16 ***
#   cash           1.300e-01  7.094e-03   18.33   <2e-16 ***

# CASH AS PREDICTOR
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.020321   0.024827  -41.10   <2e-16 ***
#   cash         0.344693   0.005966   57.78   <2e-16 ***

## PREDICTORS: AVERAGE BUSINESS RATING IN A 2 MILE RADIUS ##
## RESPONSE: LOCATION OF CRIME OR NOT (1 or 0 respectively) ##

str(train14)
logm_14ratings = glm(response ~ crime_density+avg_rating, data = train14, family=binomial)
logm_14ratings2 = glm(response ~ avg_rating, data = train14, family=binomial)
summary(logm_14ratings)
summary(logm_14ratings2)

# CRIME DENSITY + AVG_RATING AS PREDICTORS
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -2.872e+00  8.976e-02  -31.99   <2e-16 ***
#   crime_density  4.330e+07  7.873e+05   55.00   <2e-16 ***
#   avg_rating     2.377e-01  2.539e-02    9.36   <2e-16 ***

# AVG_RATING AS PREDICTOR
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.17096    0.04533  -25.83   <2e-16 ***
#   avg_rating   0.41353    0.01307   31.63   <2e-16 ***


### PREDICT RESPONSES ON 2015 DATA, USING FITTED MODEL AND PREDICTORS FROM 2014 ###

# build prediction data for 2015 data
# crime density from 2014
# % of businesses cash only within 2 miles for 2015
# avg ratings for businesses within 2 miles for 2015

# get coordinates from urbana city
urbana_full15 <- urbana_full[,2:5]

# create a set of 20,000 points (including crime)
fill2 <- 20000 - nrow(crime_data15)
set.seed(20)
urbana_full15 <- urbana_full15[sample(nrow(urbana_full15), fill2), ]

# include 2015 crime coordinates (without responses)
predict15 <- rbind(urbana_full15, crime_data15[,c('long', 'lat', 'longmeters', 'latmeters')])

## ADDING PREDICTOR: CRIME DENSITY FOR PREVIOUS YEAR ## 

# get crime densities for 2015 prediction points based on 2014s data points

h = Hpi(crime_data14[,c("longmeters","latmeters")], pilot="dscalar")
crime_density14 = kde(crime_data14[,c("longmeters","latmeters")], H=h, eval.points=predict15[,c("longmeters","latmeters")])$estimate

# bind crime density as predictor to training data
predict15 = cbind(predict15, crime_density14)
head(predict15)

## ADDING PREDICTOR: % OF BUSINESSES ONLY TAKING CASH WITHIN A 1 MILE RADIUS ## 

# calculate business cash predictors
names(yelp_data15)
# reduce data down to business cash attributes
business_cash15 <- yelp_data15[,(c("business_id", "name", "longitude", "latitude", "attributes.Accepts.Credit.Cards", "longmeters", "latmeters"))]
# remove NAs from cash attribute column
business_cash15 <- business_cash15[complete.cases(business_cash15$attributes.Accepts.Credit.Cards),]
# calculate businesses in 250 m radius to coordinate
str(business_cash15)


credit <- 0 
predict15$cash <- 0
for (i in 1:nrow(predict15)){
  distance <- sqrt((predict15[i,"longmeters"]-business_cash15[,"longmeters"])^2 + (predict15[i,"latmeters"]-business_cash15[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  for (j in 1:length(business_radius)) {
    credit <- c(credit, business_cash15[business_radius[j],"attributes.Accepts.Credit.Cards"])
  }
  credit_total <- sum(credit)/length(business_radius)
  
  if (is.na(credit_total)) {
    predict15$cash[i] <- 0
    credit <- 0
  }
  else {
    predict15$cash[i] <- ((1 - credit_total) * 100)
    credit <- 0
  }
}


## ADDING PREDICTOR: AVERAGE RATINGS OF BUSINESSES IN 1 MILE RADIUS ## 

# calculate business cash predictors

names(yelp_data15)
# reduce data down to business ratings attributes
business_rating15 <- yelp_data15[,(c("business_id", "name", "longitude", "latitude", "business_stars", "longmeters", "latmeters"))]
# remove NAs from ratings attribute column
business_rating15 <- business_rating15[complete.cases(business_rating15$business_stars),]

# calculate average rating of businesses in 2 mile radius
ratings <- 0 
predict15$avg_rating <- 0
for (i in 1:nrow(predict15)){
  distance <- sqrt((predict15[i,"longmeters"]-business_rating15[,"longmeters"])^2 + (predict15[i,"latmeters"]-business_rating15[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  for (j in 1:length(business_radius)) {
    ratings <- c(ratings, business_rating15[business_radius[j], "business_stars"])
  }
  if (is.na((sum(ratings)/length(business_radius)))) {
    predict15$avg_rating[i] <- 0
    ratings <- 0
  }
  else {
    predict15$avg_rating[i] <- (sum(ratings)/length(business_radius))
    ratings <- 0
  }
}

### PREDICTION ###

# run prediction for cash + crime density
crime_predict_cash2 <- predict(logm_14cash2, predict15, type="response")
# run prediction for cash
crime_predict_cash <- predict(logm_14cash2, predict15, type="response")

# run prediction for avg_ratings + crime density
crime_predict_ratings2 <- predict(logm_14ratings2, predict15, type="response")
# run prediction for avg_ratings
crime_predict_ratings <- predict(logm_14ratings, predict15, type="response")


## EVALUATE PREDICTIONS ##
urbana_full15 <- urbana_full

# create a set of 20,000 points (including crime)
fill2 <- 20000 - nrow(crime_data15)
urbana_full15 <- urbana_full15[sample(nrow(urbana_full15), fill2), ]

# create true points for 2015 crime
true15 <- rbind(urbana_full15, crime_data15[,c('response','long', 'lat', 'longmeters', 'latmeters')])

# cash + crime density predictions vs. true crime responses
pr_cashcrime <- prediction(crime_predict_cash2, true15$response)
prf_cashcrime <- performance(pr_cashcrime, measure = "tpr", x.measure = "fpr")

# cash predictions vs. true crime responses
pr_cash <- prediction(crime_predict_cash, true15$response)
prf_cash <- performance(pr_cash, measure = "tpr", x.measure = "fpr")

# avg business rating + crime density predictions vs. true crime responses
pr_ratingscrime <- prediction(crime_predict_ratings2, true15$response)
prf_ratingscrime <- performance(pr_ratingscrime, measure = "tpr", x.measure = "fpr")

# avg business rating predictions vs. true crime responses
pr_ratings <- prediction(crime_predict_ratings, true15$response)
prf_ratings <- performance(pr_ratings, measure = "tpr", x.measure = "fpr")

# plot ROC curve for 4 predictions
plot(prf_cashcrime, col='mediumvioletred', main="ROC Curves")
plot(prf_cash, col='red', main="ROC Curves")
plot(prf_ratingscrime, col='red', main="ROC Curves")
plot(prf_ratings, col='red', main="ROC Curves")



auc <- performance(pr_cashcrime, measure = "auc")
auc <- auc@y.values[[1]]
auc
