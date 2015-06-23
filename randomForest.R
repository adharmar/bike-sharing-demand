# load dependencies
library(party)
library(RWeka)
library(randomForest)

# load train and test data
train <- read.csv(file.choose(), stringsAsFactors=FALSE)
test <- read.csv(file.choose(), stringsAsFactors=FALSE)

# create useable data from datetime
train$hour <- factor(format(as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M"), format="%H"))
train$month <- factor(format(as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M"), format="%B"))
train$weekday <- factor(format(as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M"), format="%u"))
train$year <- factor(format(as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M"), format="%y"))

train$weather<-factor(train$weather)
train$holiday<-factor(train$holiday)
train$workingday<-factor(train$workingday)
train$season<-factor(train$season)

aggregate(train[,"count"],list(train$weekday),mean)

aggregate(train[,"count"], list(train$hour), mean)


test$hour <- factor(format(as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M"), format="%H"))
test$month <- factor(format(as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M"), format="%B"))
test$weekday <- factor(format(as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M"), format="%u"))
test$year <- factor(format(as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M"), format="%y"))

test$weather<-factor(test$weather)
test$holiday<-factor(test$holiday)
test$workingday<-factor(test$workingday)
test$season<-factor(test$season)


#create Sunday variable
train$isSunday[train$weekday == "7"] <- "1"
train$isSunday[train$weekday != "7"] <- "0"

test$isSunday[test$weekday == "7"] <- "1"
test$isSunday[test$weekday != "7"] <- "0"

#convert to factor
train$isSunday <- as.factor(train$isSunday)
test$isSunday <- as.factor(test$isSunday)



#convert time and create $hour as integer to evaluate for daypart
train$numerichour<- as.numeric(train$hour)
test$numerichour<- as.numeric(test$hour)

#create daypart column, default to 4 to make things easier for ourselves
train$daypart <- "4"
test$daypart <- "4"


#4AM - 10AM = 1
train$daypart[(train$numerichour < 10) & (train$numerichour > 3)] <- 1
test$daypart[(test$numerichour < 10) & (test$numerichour > 3)] <- 1


#11AM - 3PM = 2
train$daypart[(train$numerichour < 16) & (train$numerichour > 9)] <- 2
test$daypart[(test$numerichour < 16) & (test$numerichour > 9)] <- 2


#4PM - 9PM = 3
train$daypart[(train$numerichour < 22) & (train$numerichour > 15)] <- 3
test$daypart[(test$numerichour < 22) & (test$numerichour > 15)] <- 3

#convert daypart to factor
train$daypart <- as.factor(train$daypart)
test$daypart <- as.factor(test$daypart)

#convert hour back to factor
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)



#formula<-count ~ season+holiday+humidity+atemp+workingday+weather+hour+weekday+windspeed+isSunday+daypart

#Ctree with factorized attributes
#bikectree<-ctree(formula, data=train)

#Bagged Version of REPTree in Weka
#optns <- Weka_control(W = "weka.classifiers.trees.REPTree")
#Bagging <- make_Weka_classifier("weka/classifiers/meta/Bagging")
#bikerep<-Bagging(formula, data=train, control = optns)


#####RANDOM FOREST STARTS HERE#########
#variables
myNtree = 500
myMtry = 5
myImportance = TRUE
#set the random seed
set.seed(415)
#fit and predict casual
#ranforest <- randomForest(formula, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
#test$casual <- predict(ranforest, test)

###RandomForest2
#fit and predict casual
#no windspeed and holiday
#remove temp, add daypart,isSunday,season

casualFit <- randomForest(casual ~ hour + year+season + daypart+ isSunday+ humidity + atemp + workingday + weekday, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$casual <- predict(casualFit, test)

#fit and predict registered

registeredFit <- randomForest(registered ~ hour + year + daypart+ isSunday+ season + weather + workingday + humidity + weekday + atemp, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$registered <- predict(registeredFit, test)

#add both columns into final count, round to whole number

test$count <- round(test$casual + test$registered, 0)

#fit <- rpart(count ~ hour + month + year + weekday + weather + atemp +
#               workingday + holiday + windspeed + humidity + season, data=train, control=rpart.control(minsplit=2, cp=0))
#fit.ctree

# predict using the combined fits
predictx <- predict(ranforest, test)



# Create submission dataframe and output to file
submit.ctree <- data.frame(datetime = test$datetime, count = test$count) #predictx)
write.csv(submit.ctree, file = "RF3.csv", row.names = FALSE)


## feature importance
imp <- importance(casualFit, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
