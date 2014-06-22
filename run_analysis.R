## Description: R code to (re-)create independent tidy data set of 'Human Activity Recognition Using Smartphones Data' (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). Merges test and training data; retains only measurements on mean and standard deviation; assigns descriptive level names and labels; averages measurements by subject and activity; stores results in second, independent tidy data set.

## Note: all files (UCI HAR Dataset) must be located in current working directory

## 0. Loading packages
library(plyr)

## 1. Loading the 'test' data
subjTest <- read.table("subject_test.txt", header=F)
yTest <- read.table("y_test.txt", header=F)
XTest <- read.table("X_test.txt", header=F)
testData <- cbind(subjTest, yTest, XTest)

## 2. Loading the 'training' data
subjTrain <- read.table("subject_train.txt", header=F)
yTrain <- read.table("y_train.txt", header=F)
XTrain <- read.table("X_train.txt", header=F)
trainData <- cbind(subjTrain, yTrain, XTrain)

## 3. Merging the two data sets
mergedData <- rbind(testData, trainData)

## 4. Subsetting the data (extracting the measurements on the mean and standard deviation for each measurement)
variables <- read.table("features.txt", header=F)
subIndex <- grep("[Mm]ean|std", variables$V2)
subIndex <- c(1,2,subIndex+2)
mergedData <- mergedData[,subIndex]

## 5. Assigning descriptive variable names
varNames <- grep("[Mm]ean|std", variables$V2, value=T)
varNames <- gsub("-","",varNames)
varNames <- gsub("[()]","",varNames)
varNames <- gsub(",","",varNames)
names(mergedData) <- c("subject","activity",varNames)

## 6. Assigning descriptive activity names / turning 'activities' variable into a factor
factorNames <- read.table("activity_labels.txt", header=F)
factorNames$V2 <- tolower(factorNames$V2)
mergedData$activity <- factor(mergedData$activity, levels=factorNames$V1, labels=factorNames$V2)

## 7. Calculating the average of each variable for each activity and each subject
mergedData2 <- cbind(subject_activity=interaction(mergedData$subject, mergedData$activity), mergedData)
mergedData2=mergedData2[,-c(2:3)]
myList <- split(mergedData2, mergedData2$subject_activity)
myMeans <- sapply(myList, function(x) colMeans(x[,varNames]))

## 8. Storing and saving results of step 7 in an independent tidy data set
tidyData <- data.frame(t(myMeans))
tidyData <- cbind(subject_activity=colnames(myMeans), tidyData)
rownames(tidyData) <- 1:180
splitnames = strsplit(as.character(tidyData$subject_activity),"\\.")
firstElement <- function(x) {x[1]}
subject <- sapply(splitnames,firstElement)
subject <- factor(subject)
secondElement <- function(x) {x[2]}
activity <- sapply(splitnames, secondElement)
activity <- factor(activity)
tidyData <- cbind(subject, activity, tidyData)
write.table(tidyData, file="tidyUCI_HAR.txt")