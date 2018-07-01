#GOALS
Script was designed to:
Input UCI HAR Dataset downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement. 
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# 1. MERGING TRAINING & TEST SETS
Before merging, all applicable columns were imported  he follwing files were imported:

- 'features.txt'
- 'activity_labels.txt'
- 'subject_train.txt'
- 'x_train.txt'
- 'y_train.txt'
- 'subject_test.txt'
- 'x_test.txt'
- 'y_test.txt'

#2. EXTRACTING MEASUREMENTS ON MEAN & STANDARD DEVIATION
A logical vector was created identifying TRUE for the ID, mean & standard deviation columns and FALSE for others.

#3. RENAME ACTIVITIES IN DATA SET
'activity_labels.txt' was merged with the subsetted data to add descriptive activity names to merged and subsetted data set. Values in 'activityId' column were then replaced with the matching values from the 'activityType' column in order to make the data easier to read. 

##4. LABELING DATA SET WITH DESCRIPTIVE ACTIVITY NAMES
Used the 'gsub' function to clean up the column names in merged & subsetted data set. 'activityType' column removed in order to tidy data further.

##5. INDEPENDNENT TIDY DATA SET CREATED WITH AVERAGE FOR EACH VARIABLE & EACH SUBJECT
New table was created which contains average for each variable for each activity and subject.