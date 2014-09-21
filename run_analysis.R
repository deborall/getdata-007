#Author: David Eborall
workingDir <- "~/dev/getdata-007"
setwd(workingDir) #Sett this for GIT reposit clone
#Course Project: Getting and cleaning Data
#Dependancies
library(plyr) #for dplyr
library(stringr) # String functions
library(reshape2)

workingDataPath <- "data"
if (!file.exists(workingDataPath)) 
  {  
    dir.create(workingDataPath)
    message(paste(workingDataPath, c("created..."), sep=" "))
  }

dataSetZip <- 'Dataset.zip'
dataSetPath <- paste(workingDataPath, dataSetZip, sep="/")
if (!file.exists(dataSetPath))
{
  remoteFileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url=remoteFileURL,destfile=dataSetPath, method="curl")
}
#unzip dataset file
unzip(zipfile=dataSetPath, exdir=workingDataPath)
newWorkingDir <- paste(workingDir, workingDataPath, "UCI HAR Dataset", sep="/")
setwd(newWorkingDir)
#Read the Human Activity Recognition Using Smartphones Data Set 
#Obtained from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#

test_x <- read.table("./test/X_test.txt", sep = '')
train_x <-read.table("./train/X_train.txt", sep = '')
test_y <- read.table("./test/Y_test.txt", sep = '')
train_y <-read.table("./train/Y_train.txt", sep = '')
train_subject <-read.table("./train/subject_train.txt", sep = '')
test_subject <-read.table("./test/subject_test.txt", sep = '')
activity_labels <- read.table("activity_labels.txt", sep = '')
features <- read.table("features.txt", sep = '')
#Step 1 - Merges the training and the test sets to create one data set.

featureHead <- as.character(features[,2])
combinedTestOnly <- cbind(test_subject, test_y, test_x)
combinedTrainOnly <- cbind(train_subject, train_y, train_x)
Combined <- rbind(combinedTestOnly, combinedTrainOnly)
names(Combined) <- c("Subject", "Index", featureHead)

#Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
s1 <- Combined[, grep("std\\(\\)", names(Combined))]
s2 <- Combined[, grep("mean\\(\\)", names(Combined))]
s3 <- Combined[,c("Subject", "Index")]
Combined <- cbind(s3, s1, s2)


#Step 3 - Uses descriptive activity names to name the activities in the data set
names(activity_labels) <- c('Index', 'ActivityType')


mergedCombined <- join(Combined,activity_labels, type='left')
#Removes undesireable characters from the header

#Capitalises Aggregate to make it more readible
names(mergedCombined) <- str_replace_all(names(mergedCombined), "mean", "Mean")
names(mergedCombined) <- str_replace_all(names(mergedCombined), "std", "StdDev")
names(mergedCombined) <- str_replace_all(names(mergedCombined), "[\\(\\)]", "")
head(mergedCombined,1)
#Tidy data

measuredNames <- names(mergedCombined[,3:68])
mcMelt <- melt(mergedCombined, id=c("Subject", "ActivityType"), measure.vars= measuredNames)
names(mcMelt) <- c("SubjectID", "ActivityType", "MeasureMentType","Measurement")
#head(mcMelt,200)

testMeanBySubjectByActivity <- dcast(mcMelt, SubjectID + MeasureMentType ~ ActivityType, mean, value.var = "Measurement")

head(testMeanBySubjectByActivity)

write.table(testMeanBySubjectByActivity, "project_tidy.txt", row.name=FALSE)
