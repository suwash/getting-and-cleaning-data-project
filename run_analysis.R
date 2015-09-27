
# Clean-up workspace
rm(list=ls())

# 1.----------------------------------------------------------------------------------------------- 
#Merge the training and the test sets to create one data set.
#Download file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
#unzip the data file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Read in the data from files
features     = read.table('./data/UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('./data/UCI HAR Dataset/activity_labels.txt',header=FALSE)
#Training data
subjectTrain = read.table('./data/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       = read.table('./data/UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       = read.table('./data/UCI HAR Dataset/train/y_train.txt',header=FALSE)
# Test data
subjectTest = read.table('./data/UCI HAR Dataset/test/subject_test.txt',header=FALSE)   #imports subject_test.txt
xTest       = read.table('./data/UCI HAR Dataset/test/x_test.txt',header=FALSE)         #imports x_test.txt
yTest       = read.table('./data/UCI HAR Dataset/test/y_test.txt',header=FALSE)         #imports y_test.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"
#Assign column names to the test data
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"
# Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)
testData = cbind(yTest,subjectTest,xTest)

# Combining training and test data
finalData = rbind(trainingData,testData)

# Column names of finaldata for selection of desired mean() & stddev() columns.
colNames  = colnames(finalData)

# 2.-------------------------------------------------------------------------------------------
##Extract only the measurements on the mean and standard deviation for each measurement. 

# logicalVector which is TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
mean_sd_Data = finalData[logicalVector==TRUE]   #subset of data

# 3.---------------------------------------------------------------------------------------
# Merging finalData set with the acitivityType table to include descriptive activity names
mean_sd_Data = merge(mean_sd_Data,activityType,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(mean_sd_Data)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)){
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(mean_sd_Data) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = mean_sd_Data[,names(mean_sd_finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')