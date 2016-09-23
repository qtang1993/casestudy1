rm(list=ls())

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
library(ggmap)

yelp_dataset <- read.csv("yelp_dataset.csv")


### Data Cleaning ###

# Convert .JSON to dataset with Urbana Business Data
yelp <- stream_in(file("yelp_academic_dataset_business.json"))
yelp_data <- flatten(yelp)
str(yelp_data)
yelp_data <- as_data_frame(yelp_data)
yelp_dataset <- yelp_data %>% filter(city=="Urbana")
yelp_dataset[,"categories"] <- NULL
yelp_dataset[,"full_address"] <- NULL
yelp_businesses <- yelp_dataset

# Create Backup
yelp_dataset_backup <- yelp_dataset
yelp_dataset <- yelp_dataset_backup

# Create CSV without categories included:
yelp_dataset <- as.matrix(yelp_dataset)
write.csv(yelp_dataset, file="yelp_dataset.csv")


# Convert .JSON to dataset with Urbana Business Review Data
yelp_review <- stream_in(file("yelp_academic_dataset_review.json"))
yelp_review_data <- flatten(yelp_review)
str(yelp_review_data)
yelp_review_data <- as_data_frame(yelp_review_data)

# Create Backup
yelp_review_dataset_backup <- yelp_review_data
# use if backup needed
yelp_review_data <- yelp_review_dataset_backup

# Filter Yelp Review Data
business <- yelp_dataset[,"business_id"]
business <- as.data.frame(unlist(business))
yelp_review_dataset <- filter(yelp_review_data, yelp_review_data$business_id %in% business[,1])

# Create CSV for yelp reviews included:
yelp_review_dataset <- as.matrix(yelp_review_dataset)
write.csv(yelp_review_dataset, file="yelp_dataset_review.csv")

# Merge Yelp Review Data with Yelp Data
yelp_master <- merge(yelp_review_dataset, yelp_dataset, by="business_id")
names(yelp_master)[c(4,7,20,22)] <- c("review_stars", "review_type", "business_stars", "business_type")
yelp_master <- select(yelp_master, -starts_with("attributes.Dietary"), -starts_with("attributes.Hair"), -starts_with("attributes.Ambience"), -starts_with("neighborhoods"))

# Create CSV for master dataset of yelp reviews and businesses:
yelp_master[,c("categories", "full_address")] <- NULL
write.csv(yelp_master, file="yelp_master.csv")
        

# URBANA CRIME HEAT MAP

# load the data
crime_data <- read.csv("UrbanaData.csv")
crime_data <- filter(crime_data, crime_data$YEAR.OCCURRED > 2004)

# Download the base map
urbana <- get_map(location = "Urbana, Illinois", zoom = 14)
# Draw the heat map
map_total <- ggmap(urbana, extent = "device", darken=0.7) + geom_density2d(data = crime_data, aes(x = Longitude, y = Latitude), size = 0.3) + 
      stat_density2d(data = crime_data, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      geom_point(aes(x=longitude, y=latitude), data=yelp_businesses, col="orange", size = 0.2, alpha=0.4)
map_total

# TODO:
# separate crime by year
# Hypothesis Testing
