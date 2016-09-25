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

crime_data14 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2014)
yelp_data14 <- filter(yelp_master, grepl('2014', date))

crime_data15 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2015)
yelp_data15 <- filter(yelp_master, grepl('2015', date))

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
long <- seq(-88.23302, -88.15307, length.out = 10000)
lat <- seq(40.07324, 40.15742, length.out = 10000)
urbana_full <- cbind(0, long, lat)
urbana_full <- as.data.frame(urbana_full)
names(urbana_full)[1] <- "response"

# change long/lat to meters and bind to urbana_full
urbana_longlat = cbind(urbana_full$long, urbana_full$lat)
urbana_longlat_meters = project(urbana_longlat, proj="+init=epsg:26971")
head(urbana_longlat_meters)
urbana_full <- cbind(urbana_full, urbana_longlat_meters[,1], urbana_longlat_meters[,2])
names(urbana_full)[c(4,5)] <- c("longmeters", "latmeters")

## TRAIN MODEL ON RESPONSES FROM 2014, USING PREDICTORS FROM 2013 ##

# Training Set for 2014 (Non-Crime coordinates and Yelp Data)
# remove crime coordinates from non-crime coordinates
urbana_full14 <- urbana_full[!(urbana_full$long %in% crime_data14$Longitude),]
# bind the non-crime coordinates with the crime coordinates
head(crime_data14)
train14 <- rbind(urbana_full14, crime_data14[,c('response', 'long', 'lat', 'longmeters', 'latmeters')])

# get crime densities for 2014 training point based on 2013s data points
kde2d(crime)
h = Hpi(crime_data13[,c("longmeters","latmeters")], pilot="dscalar")
crime_density = kde(crime_data13[,c("longmeters","latmeters")], H=h, eval.points=train14[,c("longmeters","latmeters")])$estimate

# bind crime density as predictor to training data
train14 = cbind(train14, crime_density)

# calculate business cash predictors
names(yelp_data14)
# reduce data down to business cash attributes
business_cash14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "attributes.Accepts.Credit.Cards", "longmeters", "latmeters"))]
# remove NAs from cash attribute column
business_cash14 <- business_cash14[complete.cases(business_cash14$attributes.Accepts.Credit.Cards),]

# add predictors to training data

# fit glm

## PREDICT RESPONSES ON 2015 DATA, USING FITTED MODEL AND PREDICTORS FROM 2014 ##

## EVALUATE PREDICTIONS ##


