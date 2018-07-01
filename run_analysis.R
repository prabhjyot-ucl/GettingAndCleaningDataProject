# Coursera Getting and Cleaning Data Course Project
# Name: Prabhjyot Singh
# Date: 01.07.2018

# Dataset source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# This script will do the following:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# 1. Merge the training and the test sets to create one data set.
setwd('G:\\Data Science\\Project assignment4\\UCI HAR Dataset');  #Set my working director

# Import required training files  
features <- read.table('./features.txt',header=FALSE);
activityLabels <- read.table('./activity_labels.txt',header=FALSE); 
colnames(activityLabels) <- c("activityId","activityType");
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
colnames(subjectTrain) <- "subjectId";
xTrain <- read.table('./train/x_train.txt',header=FALSE); colnames(xTrain) <- 
  features[,2];
yTrain <- read.table('./train/y_train.txt',header=FALSE); colnames(yTrain) <- 
  "activityId";

# Merge Data for training 
trainingSet = cbind(yTrain,subjectTrain,xTrain);

# Importing test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
colnames(subjectTest) <- "subjectId";
xTest <- read.table('./test/x_test.txt',header=FALSE); colnames(xTest) <- 
  features[,2];
yTest <- read.table('./test/y_test.txt',header=FALSE); colnames(yTest) <- 
  "activityId";

# Merge Data into complete test set
testSet = cbind(yTest,subjectTest,xTest);

# Combine Training and test dataset
MergedData = rbind(trainingSet,testSet);

# Create columns vector to prepare data for subsetting
columns <- colnames(MergedData);


# 2. Extracting only mean and standard deviation for each measurement

# Create a vector that indentifies the ID, mean & stddev columns as TRUE
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
             !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
             grepl("-std..",columns) & !grepl("-std()..-",columns));

# Update MergedData as in #1
MergedData <- MergedData[vector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Add in descriptive activity names to MergedData & update columns vector
MergedData <- merge(MergedData,activityLabels,by='activityId',all.x=TRUE);
MergedData$activityId <-activityLabels[,2][match(MergedData$activityId, activityLabels[,1])] 

columns <- colnames(MergedData);

# 4. Labeling appropriate activity names

# Tidy column names
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

# Update MergedData 
colnames(MergedData) <- columns;

# Remove activityType column
MergedData <- MergedData[,names(MergedData) != 'activityType'];

# 5. Creating second, independent tidy data set with the average of 
#    each variable for each activity and each subject.

# Averaging 
tidyDatares <- aggregate(MergedData[,names(MergedData) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=MergedData$activityId,
                        subjectId=MergedData$subjectId),mean);

# Export result set 
write.table(tidyDatares, './FinalTidyDataResult.txt',row.names=FALSE,sep='\t')