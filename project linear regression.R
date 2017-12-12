#install.packages(c("dplyr", "ggplot2", "GGally"))
library(dplyr)
library(ggplot2)
library(GGally)
#install.packages("car")
library(car) # for VIF
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(ROCR)
library(MASS)
#install.packages("glmnet")
library(glmnet)
library(lubridate)
library(stringr)

# setting up
finalDelta <- read.csv("finaldelta.csv", stringsAsFactors = FALSE)
finalDelta$weekday = as.factor(finalDelta$weekday)
noBikes <- read.csv("Updated BikeStationNumber.csv", stringsAsFactors = FALSE)
noBuses <- read.csv("Updated - BusStopNumber.csv", stringsAsFactors = FALSE)
noParks <- read.csv("Updated - ParkNumber.csv", stringsAsFactors = FALSE)
dev <- read.csv("devQ2.csv")
allTAZ <- left_join(finalDelta, noBikes, by = c("Dest.taz" = "TAZ"))
colnames(allTAZ)[16] <- "numberBikesDest"
allTAZ <- left_join(allTAZ, noBikes, by = c("Orig.taz" = "TAZ"))
colnames(allTAZ)[17] <- "numberBikesOrig"
allTAZ <- left_join(allTAZ, noBuses, by = c("Dest.taz" = "TAZ"))
colnames(allTAZ)[18] <- "numberBusDest"
allTAZ <- left_join(allTAZ, noBuses, by = c("Orig.taz" = "TAZ"))
colnames(allTAZ)[19] <- "numberBusOrig"
allTAZ <- left_join(allTAZ, noParks, by = c("Dest.taz" = "TAZ"))
colnames(allTAZ)[20] <- "numberParksDest"
allTAZ <- left_join(allTAZ, noParks, by = c("Orig.taz" = "TAZ"))
colnames(allTAZ)[21] <- "numberParksOrig"
allTAZ <- left_join(allTAZ, dev, by = c("Dest.taz" = "TAZ"))
allTAZ <- left_join(allTAZ, dev, by = c("Orig.taz" = "TAZ"))
allTAZ <- allTAZ[,-22]
colnames(allTAZ)[22] <- "CIE.dest"
colnames(allTAZ)[23] <- "MED.dest"
colnames(allTAZ)[25] <- "MIPS.dest"
colnames(allTAZ)[26] <- "Mixed.dest"
colnames(allTAZ)[27] <- "Mixres.dest"
colnames(allTAZ)[28] <- "PDR.dest"
colnames(allTAZ)[29] <- "Resident.dest"
colnames(allTAZ)[30] <- "Retail.dest"
colnames(allTAZ)[31] <- "Vacant.dest"
colnames(allTAZ)[32] <- "Visitor.dest"
colnames(allTAZ)[33] <- "AvgCost.dest"
allTAZ <- allTAZ[,-34]
colnames(allTAZ)[34] <- "CIE.orig"
colnames(allTAZ)[35] <- "MED.orig"
colnames(allTAZ)[37] <- "MIPS.orig"
colnames(allTAZ)[38] <- "Mixed.orig"
colnames(allTAZ)[39] <- "Mixres.orig"
colnames(allTAZ)[40] <- "PDR.orig"
colnames(allTAZ)[41] <- "Resident.orig"
colnames(allTAZ)[42] <- "Retail.orig"
colnames(allTAZ)[43] <- "Vacant.orig"
colnames(allTAZ)[44] <- "Visitor.orig"
colnames(allTAZ)[45] <- "AvgCost.orig"



# hierarchical clustering
tazArea <- read.csv("TAZarea.csv")
d = dist(tazArea$area_sqft)
clusters = hclust(d, method = 'ward.D')
#plot(clusters)
clusterCut <- cutree(clusters, 2)
tazClusters <- as.data.frame(clusterCut)
clusterClassification <- cbind(tazArea, tazClusters$clusterCut)
allTAZ <- left_join(allTAZ, clusterClassification, by = c("Dest.taz" = "TAZ"))
allTAZ <- allTAZ[,-(46:50)]
allTAZ <- left_join(allTAZ, clusterClassification, by = c("Orig.taz" = "TAZ"))
allTAZ <- allTAZ[,-(47:51)]
colnames(allTAZ)[46] <- "dest.cluster"
colnames(allTAZ)[47] <- "orig.cluster" # 2 = high area/low density , 1 = low area/high density

allTAZ$weekday = as.factor(wday(allTAZ$Dep_Time.date))
allTAZ$hour = as.factor(hour(allTAZ$Dep_Time.date))
allTAZ$numberBikesDest = as.numeric(allTAZ$numberBikesDest)
allTAZ$numberBikesOrig = as.numeric(allTAZ$numberBikesOrig)
allTAZ$numberBusDest = as.numeric(allTAZ$numberBusDest)
allTAZ$numberBusOrig = as.numeric(allTAZ$numberBusOrig)
allTAZ$numberParksDest = as.numeric(allTAZ$numberParksDest)
allTAZ$numberParksOrig = as.numeric(allTAZ$numberParksOrig)
allTAZ[is.na(allTAZ)] <- 0

# split into 4 types of routes
allTAZ$classify = ifelse(allTAZ$dest.cluster == 2, ifelse(allTAZ$orig.cluster == 1, 'low-high', 'low-low'), ifelse(allTAZ$orig.cluster == 2, 'high-low', 'high-high'))

# all data
set.seed(245)
split = sample.split(allTAZ$classify, SplitRatio = 0.8)
train.ALL = filter(allTAZ, split == TRUE)
test.ALL = filter(allTAZ, split == FALSE)
# TYPE 1: low-low
set.seed(34)
LL = filter(allTAZ, allTAZ$classify == "low-low")#, SplitRatio = 0.7)
split = sample.split(LL$classify, SplitRatio = 0.8)
train.LL = LL %>% filter(split == TRUE)
test.LL = filter(LL, split == FALSE)
# TYPE 2: low-high
set.seed(98)
LH = as.data.frame(filter(allTAZ, allTAZ$classify == "low-high"))
split = sample.split(LH$classify, SplitRatio = 0.8)
train.LH = LH %>% filter(split == TRUE)
test.LH = filter(LH, split == FALSE)
# TYPE 3: high-low
set.seed(379)
HL = as.data.frame(filter(allTAZ, allTAZ$classify == "high-low"))
split = sample.split(HL$classify, SplitRatio = 0.8)
train.HL = HL %>% filter(split == TRUE)
test.HL = filter(HL, split == FALSE)
# TYPE 4: high-high
set.seed(325)
HH = as.data.frame(filter(allTAZ, allTAZ$classify == "high-high"))
split = sample.split(HH$classify, SplitRatio = 0.8)
train.HH = HH %>% filter(split == TRUE)
test.HH = filter(HH, split == FALSE)


#------begin linear regression model-------------------------------------

# ALL
linear.ALL <- lm(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                 data = train.ALL)
summary(linear.ALL)
vif(linear.ALL)
deltaPred.ALL <- predict(linear.ALL, newdata=test.ALL) # this builds a vector of predicted values on the test set
SSE.ALL = sum((test.ALL$delta - deltaPred.ALL)^2)
RMSE.ALL = sqrt(SSE.ALL/length(train.ALL$delta))
SST.ALL = sum((test.ALL$delta - mean(train.ALL$delta))^2)
OSR2.ALL = 1 - SSE.ALL/SST.ALL
print("Linear Regression OSR^2 for all types is:")
print(OSR2.ALL)

"""
# LH
linear.LH <- lm(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                   data = train.LH)
summary(linear.LH)
vif(linear.LH)
deltaPred.LH <- predict(linear.LH, newdata=test.LH) # this builds a vector of predicted values on the test set
SSE.LH = sum((test.LH$delta - deltaPred.LH)^2)
SST.LH = sum((test.LH$delta - mean(train.LH$delta))^2)
OSR2.LH = 1 - SSE.LH/SST.LH
print(OSR2.LH)

# HL
linear.HL <- lm(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                data = train.HL)
summary(linear.HL)
vif(linear.HL)
deltaPred.HL <- predict(linear.HL, newdata=test.HL) # this builds a vector of predicted values on the test set
SSE.HL = sum((test.HL$delta - deltaPred.HL)^2)
SST.HL = sum((test.HL$delta - mean(train.HL$delta))^2)
OSR2.HL = 1 - SSE.HL/SST.HL
print(OSR2.HL)
"""

# HH
linear.HH <- lm(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                data = train.HH)
summary(linear.HH)
vif(linear.HH)
deltaPred.HH <- predict(linear.HH, newdata=test.HH) # this builds a vector of predicted values on the test set
SSE.HH = sum((test.HH$delta - deltaPred.HH)^2)
SST.HH = sum((test.HH$delta - mean(train.HH$delta))^2)
OSR2.HH = 1 - SSE.HH/SST.HH
print("Linear Regression OSR^2 for HH types is:")
print(OSR2.HH)
RMSE.HH = sqrt(SSE.HH/length(train.HH$delta))
print(RMSE.HH)
#------end linear regression model-------------------------------------

#------begin RF model-------------------------------------
set.seed(144)
rf.ALL <- randomForest(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                       data = train.ALL)

pred.rf.ALL <- predict(rf.ALL, newdata = test.ALL) # just to illustrate

(importance(rf.ALL))/1000000
SSE.rf.ALL = sum((test.ALL$delta - pred.rf.ALL)^2)
SST.rf.ALL = sum((test.ALL$delta - mean(train.ALL$delta))^2)
OSR2.rf.ALL = 1 - SSE.rf.ALL/SST.rf.ALL
print("Random Forest OSR^2 for ALL types is:")
print(OSR2.rf.ALL)
RMSE.rf.ALL = sqrt(SSE.rf.ALL/length(train.ALL$delta))
pred.rf.ALL <- predict(rf.ALL, newdata = train.ALL) # just to illustrate
SSE.rf.ALL = sum((train.ALL$delta - pred.rf.ALL)^2)
SST.rf.ALL = sum((train.ALL$delta - mean(train.ALL$delta))^2)
R2.rf.ALL = 1 - SSE.rf.ALL/SST.rf.ALL

set.seed(234)
rf.HH <- randomForest(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, 
                       data = train.HH)

pred.rf.HH <- predict(rf.HH, newdata = test.HH) # just to illustrate

(importance(rf.HH))/1000000
SSE.rf.HH = sum((test.HH$delta - pred.rf.HH)^2)
SST.rf.HH = sum((test.HH$delta - mean(train.HH$delta))^2)
OSR2.rf.HH = 1 - SSE.rf.HH/SST.rf.HH
RMSE.rf.HH = sqrt(SSE.rf.HH/length(train.HH$delta))
print("Random Forest OSR^2 for HH types is:")
print(OSR2.rf.HH)
pred.rf.HH <- predict(rf.HH, newdata = train.HH) # just to illustrate
SSE.rf.HH = sum((train.HH$delta - pred.rf.HH)^2)
SST.rf.HH = sum((train.HH$delta - mean(train.HH$delta))^2)
R2.rf.HH = 1 - SSE.rf.HH/SST.rf.HH


#------end RF model-------------------------------------

#------begin boosting model-------------------------------------
#tGrid = expand.grid(n.trees = seq(1,75,10)*500, interaction.depth = c(4,6,8),
#                    shrinkage = 0.001, n.minobsinnode = 10)
# cross validation on n.trees and interaction depth
# WARNING: this took me ~1 hour to run
#boost.ALL <- train(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig,
#                 data = as.data.frame(train.ALL),
#                 method = "gbm",
#                 tuneGrid = tGrid,
#                 trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
#                 metric = "RMSE",
#                 distribution = "gaussian")

#train.boost
#best.boost <- train.boost$finalModel
#pred.best.boost <- predict(best.boost, newdata = test.ctr.mm, n.trees = 11500) # can use same model matrix

# with cross-validated values
boost.ALL <- gbm(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig,
                 data = train.ALL,
                 distribution = "gaussian",
                 n.trees = 35500,
                 shrinkage = 0.001,
                 interaction.depth = 8)
pred.boost.ALL <- predict(boost.ALL, newdata = test.ALL, n.trees=35500)
summary(pred.boost.ALL) # tells us what is the most influential variables.
SSE.boost.ALL = sum((test.ALL$delta - pred.boost.ALL)^2)
SST.boost.ALL = sum((test.ALL$delta - mean(train.ALL$delta))^2)
OSR2.boost.ALL = 1 - SSE.boost.ALL/SST.boost.ALL
print("Boosting OSR^2 for ALL types is:")
print(OSR2.boost.ALL)

# NOTE: we need to specify number of trees to get a prediction for boosting
pred.boost.ALL <- predict(boost.ALL, newdata = test.ALL, n.trees=35500)
# after you have trained a model, you can get the predictions for all earlier iterations as well, for example:

summary(pred.boost.ALL) # tells us what is the most influential variables.

SSE.boost.ALL = sum((test.ALL$delta - pred.boost.ALL)^2)
SST.boost.ALL = sum((test.ALL$delta - mean(train.ALL$delta))^2)
OSR2.boost.ALL = 1 - SSE.boost.ALL/SST.boost.ALL
print("Boosting OSR^2 for ALL types is:")
print(OSR2.boost.ALL)
pred.boost.ALL <- predict(boost.ALL, newdata = train.ALL, n.trees=35500)
SSE.boost.ALL.R2 = sum((train.ALL$delta - pred.boost.ALL)^2)
SST.boost.ALL.R2 = sum((train.ALL$delta - mean(train.ALL$delta))^2)
R2.boost.ALL = 1 - SSE.boost.ALL.R2/SST.boost.ALL.R2
print("Boosting R^2 for ALL types is:")
print(R2.boost.ALL)
print("Boosting OSR^2 for ALL types is:")
print(OSR2.boost.ALL)
RMSE.baseline.ALL = sqrt(SSE.boost.ALL/length(train.ALL$delta))
print("Boosting RMSE for ALL types is:")
print(RMSE.baseline.ALL)
#------end boosting model-------------------------------------

trainX = model.matrix(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, data = train.ALL)
testX = model.matrix(delta ~ Dest.area_sqft + Orig.area_sqft + weekday + numberBikesOrig + numberBikesDest + numberBusDest + numberBusOrig + numberParksDest + numberParksOrig + hour + CIE.dest + MIPS.dest + Mixres.dest + Resident.dest + Retail.dest + Visitor.dest + CIE.orig + MIPS.orig + Mixres.orig + Resident.orig + Retail.orig + Visitor.orig, data = test.ALL)
trainY = train.ALL[, 'delta']
testY = test.ALL[, 'delta']
set.seed(500)

mod.lasso = glmnet(trainX, trainY, alpha = 1, standardize = TRUE)
plot(mod.lasso, xvar = "lambda")
cv.lasso <- cv.glmnet(x = trainX, y = trainY, alpha = 1)

cv.lasso$lambda.min
cv.lasso$lambda.1se

plot(cv.lasso)
pred.lasso.train <- predict(cv.lasso, newx = trainX)
pred.lasso.test <- predict(cv.lasso, newx = testX)

# tells us the non-zero coefficient indicies
nzero.lasso <- predict(cv.lasso, type = "nonzero")


OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

printMetricsHelp <- function(train, test, pred.train, pred.test, doExp) {
  if(doExp) {
    train <- exp(train)
    test <- exp(test)
    pred.train <- exp(pred.train)
    pred.test <- exp(pred.test)
  }
  
  trainRsq <- OSR2(pred.train, train, train)
  testRsq <- OSR2(pred.test, train, test)
  trainMAE <- mean(abs(train - pred.train))
  testMAE <- mean(abs(test - pred.test))
  trainRMSE <- sqrt(mean((train - pred.train)^2))
  testRMSE <- sqrt(mean((test - pred.test)^2))
  
  print(str_c("Training set R^2: ", trainRsq))
  print(str_c("Training set MAE: ", trainMAE))
  print(str_c("Training set RMSE: ", trainRMSE))
  print(str_c("Test set R^2: ", testRsq))
  print(str_c("Test set MAE: ", testMAE))
  print(str_c("Test set RMSE: ", testRMSE))
}

printMetrics <- function(train, test, pred.train, pred.test) {
  print("Metrics for Delta:")
  printMetricsHelp(train, test, pred.train, pred.test, FALSE)
  print("")
}

printMetrics(trainY, testY, pred.lasso.train, pred.lasso.test)

tmp_coeffs <- coef(cv.lasso, s = "lambda.min")
data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

#-----------begin baseline model ---------------------------
baseline.ALL = mean(train.ALL$delta)
SSE.baseline.ALL = sum((train.ALL$delta - baseline.ALL)^2)
SST.baseline.ALL = sum((train.ALL$delta - baseline.ALL)^2)
R2.baseline.ALL = 1 - SSE.baseline.ALL/SST.baseline.ALL
RMSE.baseline.ALL = sqrt(SSE.baseline.ALL/length(train.ALL$delta))


SSE.baseline.ALL = sum((test.ALL$delta - baseline.ALL)^2)
SST.baseline.ALL = sum((train.ALL$delta - baseline.ALL)^2)
OSR2.baseline.ALL = 1 - SSE.baseline.ALL/SST.baseline.ALL

baseline.HH = mean(train.HH$delta)
SSE.baseline.HH = sum((train.HH$delta - baseline.HH)^2)
SST.baseline.HH = sum((train.HH$delta - baseline.HH)^2)
R2.baseline.HH = 1 - SSE.baseline.HH/SST.baseline.HH
RMSE.baseline.HH = sqrt(SSE.baseline.HH/length(train.HH$delta))


SSE.baseline.HH = sum((test.HH$delta - baseline.HH)^2)
SST.baseline.HH = sum((train.HH$delta - baseline.HH)^2)
OSR2.baseline.HH = 1 - SSE.baseline.HH/SST.baseline.HH

#------begin cost analysis-------------------------------------
# select the HH route with the highest delta to perform analysis on
maxDelta <- max(train.HH$delta)
maxDelta
route = train.HH[1,]
pred.rf.route <- predict(rf.HH, newdata = route) # just to illustrate
pred.rf.route

# change the number of bus stops in origin TAZ
route2 = train.HH[1,]
route2[,19] = route2[,19] + 10
pred.rf.route2 <- predict(rf.HH, newdata = route2) # just to illustrate
pred.rf.route2

# change the number of bus stops in destination TAZ
route3 = train.HH[1,]
route3[,18] = route3[,18] + 10
pred.rf.route3 <- predict(rf.HH, newdata = route3) # just to illustrate
pred.rf.route3

# change the number of bike stations in origin TAZ
route4 = train.HH[1,]
route4[,17] = route4[,17] + 1
pred.rf.route4 <- predict(rf.HH, newdata = route4) # just to illustrate
pred.rf.route4

# change the number of bike stations in destination TAZ
route5 = train.HH[1,]
route5[,16] = route5[,16] + 1
pred.rf.route5 <- predict(rf.HH, newdata = route5) # just to illustrate
pred.rf.route5

# number of bike stations in origin TAZ
routeChange = train.HH[1,]
predictedTimeDelta = 0
routeChange$numberBikesOrig = 0
numberOfBikeStations = 0:10
for (i in numberOfBikeStations) {
routeChange$numberBikesOrig = i
predictedTimeDelta[i+1] <- predict(rf.HH, newdata = routeChange) # just to illustrate
}
plot(numberOfBikeStations, predictedTimeDelta)
title('Predicted Delta vs Number of Origin Bike Stations (TAZ 334 to 176)')
timeSaved1 <- routeChange$delta - min(predictedTimeDelta)
print("Time saved by increasing number of bike stations in origin TAZ by 2 is:")
print(timeSaved1)

# number of bus stops in destination TAZ
routeChange = train.HH[1,]
predictedTimeDelta = 0
routeChange$numberBusDest = 0
numberOfBusStations = 2:20
for (i in numberOfBusStations) {
  routeChange$numberBusDest = i
  predictedTimeDelta[i-1] <- predict(rf.HH, newdata = routeChange) # just to illustrate
}
plot(numberOfBusStations, predictedTimeDelta)
title('Predicted Delta vs Number of Destination Bus Stations (TAZ 334 to 176)')
timeSaved2 <- routeChange$delta- min(predictedTimeDelta)
print("Time saved by increasing number of bus stops in destination TAZ by 12 is:")
print(timeSaved2)

# number of mixres in destination TAZ
routeChange = train.HH[1,]
predictedTimeDelta = 0
routeChange$Mixres.dest = 0
numberOfMixres = 0:20
for (i in numberOfMixres) {
  routeChange$Mixres.dest = i
  predictedTimeDelta[i+1] <- predict(rf.HH, newdata = routeChange) # just to illustrate
}
plot(numberOfMixres, predictedTimeDelta)
title('Predicted Delta vs Number of Destination Mixed Use Buildings in Development (TAZ 334 to 176)')
print("Time saved by increasing Mixres by 2 is:")
timeSaved3 <- routeChange$delta- min(predictedTimeDelta)
print("Time saved by increasing number of mixed use buildings in development in destination TAZ by 2 is:")
print(timeSaved3)

routeChange = train.HH[1,]
routeChange$Mixres.dest = 2
routeChange$numberBusDest = 12
routeChange$numberBikesOrig = 2
predictedTimeDelta <- predict(rf.HH, newdata = routeChange)
routeChange$delta - predictedTimeDelta
