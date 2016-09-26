# install.packages("ROCR")
# install.packages("caret")
library(ROCR)
library(zoo)
library(rgdal)
library(maptools)
library(lubridate)
library(ks)
library(geosphere)
library(MASS)
require(caret)

# read in data file 
setwd("/Users/tomoei/Downloads/")
crime <- read.csv("UrbanaData.csv")
# yelp.master <- read.csv("yelp_master.csv")
yelp <- read.csv("yelp_dataset.csv")

# clean up crime data
# only keep crimes that reported after 2005 and remove crime data without a location.
# only keep crimes happened in Urbana IL
crime <- crime[crime$YEAR.OCCURRED >= 2005 & !is.na(crime$Latitude) ,]
crime <- crime[crime$Latitude < 40.1574203 & crime$Latitude > 40.0732478 & crime$Longitude < -88.1530727  & crime$Longitude > -88.23302029999999, ]

# keep yelp data columns without NAs.
yelp <- yelp[!is.na(yelp$longitude) & yelp$city == "Urbana", c(3,5,8,10,11)]

# project lat and lon into x and y
crimes.locations.lonlat = cbind(crime$Longitude, crime$Latitude)
crimes.locations.meters = project(crimes.locations.lonlat, proj="+init=epsg:26971")
crime$x <- crimes.locations.meters[,1]
crime$y <- crimes.locations.meters[,2]

yelp.locations.lonlat = cbind(yelp$longitude, yelp$latitude)
yelp.locations.meters = project(yelp.locations.lonlat, proj="+init=epsg:26971")
yelp$x <- yelp.locations.meters[,1]
yelp$y <- yelp.locations.meters[,2]

# compute total number of crimes that reported within 50 meters around a business
# cumulative number of crimes since 2005
yelp$total <- 0
for (i in 1:nrow(yelp)){
  distance <- sqrt((crime[,"x"]-yelp[i,"x"])^2 + (crime[,"y"]-yelp[i,"y"])^2)
  yelp[i,"total"] <- length(distance[distance < 50])
}

# Find the median of numbers of crimes around businesses
quantile(yelp$total,0.5)
# if greater than median, then crime rate is high (label as 1)

yelp$level <- 0
yelp$level <- ifelse(yelp$total >= 2, 1, 0)

yelp$level <- as.factor(yelp$level)
yelp$stars <- as.factor(yelp$stars)

#####################################################################
##################### linear discriminant analysis ##################
#####################################################################
accuracys <- c()
auc <- c()
colors <- c(1:10)
fids <- createFolds(yelp$total,k=10)
for (i in 1:10){
  test <- yelp[fids[[i]],]
  train <- yelp[-fids[[i]],]
  lda.fit = lda(level ~ review_count + stars , data = train)
  predictions = predict(lda.fit, newdata = test)
  num.correct = sum(predictions$class == test$level)
  accuracy = num.correct / nrow(test)
  accuracys <- c(accuracys, accuracy)

  pred <- prediction(predictions$posterior[,2],test$level)
  perf <- performance(pred, "tpr","fpr")
  auc = performance(pred, "auc")@y.values[[1]]
  aucs <- c(aucs, auc)
  par(new=TRUE)
  plot(perf, col = i)
  abline(0, 1, lty="dashed")
}

mean(accuracys)
mean(aucs)

#####################################################################
###################### Try with different levels ####################
#####################################################################
quantile(yelp$total,c(0.25, 0.5, 0.75))
yelp$rate <- " "
for (i in 1:nrow(yelp)){
  if (yelp$total[i] <= 0){
    yelp$rate[i] <- "Low"
  }else if (yelp$total[i] <= 2){
    yelp$rate[i] <- "Below Avg"
  }else if (yelp$total[i] <= 104){
    yelp$rate[i] <- "Above Avg"
  }else{
    yelp$rate[i] <- "High"
  }
}

yelp$rate <- as.factor(yelp$rate)
yelp$stars <- as.factor(yelp$stars)

accuracys <- c()
fids <- createFolds(yelp$total,k=10)
for (i in 1:10){
  test <- yelp[fids[[i]],]
  train <- yelp[-fids[[i]],]
  lda.fit = lda(rate ~ review_count + stars + x + y , data = train)
  predictions = predict(lda.fit, newdata = test)
  num.correct = sum(predictions$class == test$rate)
  accuracy = num.correct / nrow(test)
  accuracys <- c(accuracys, accuracy)
}

mean(accuracys)

#####################################################################
########################## logistic regression ######################
#####################################################################

fit = glm(level ~ review_count + stars, data = train, family="binomial")
summary(fit)
predict.lr <- predict(fit, newdata = test, type="response")
# none of variables are significant.


