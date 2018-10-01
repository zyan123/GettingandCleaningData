#Merges the training and the test sets to create one data set.
#download the data
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, destfile = "./cleandata.zip")
listZip <- unzip("./cleandata.zip")
#load trainging data into R
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
#load test data into R
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
#merge
traindata <- cbind(subjecttrain, ytrain, xtrain)
testdata <- cbind(subjecttest, ytest, xtest)
dataset <- rbind(traindata, testdata)

#Extracts only the measurements on the mean and standard deviation for each measurement
featuredata <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]
measurements <- grep("mean\\(\\)|std\\(\\)", featuredata)
extractdata <- dataset[,c(1,2,measurements+2)]
colnames(extractdata) <- c("subject", "activity", featuredata[measurements])

#Uses descriptive activity names to name the activities in the data set
activitydata <- read.table("./UCI HAR Dataset/activity_labels.txt")
extractdata$activity <- factor(extractdata$activity, levels = activitydata[,1], labels = activitydata[,2])

#Appropriately labels the data set with descriptive variable names
names(extractdata) <- gsub("-std", "Std", names(extractdata))
names(extractdata) <- gsub("-mean", "Mean", names(extractdata))
names(extractdata) <- gsub("^f", "Frequency", names(extractdata))
names(extractdata) <- gsub("^t", "Time", names(extractdata))
names(extractdata) <- gsub("\\()", "", names(extractdata))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
groupData <- extractdata %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(groupData, "./MeanData.txt", row.names = FALSE)


