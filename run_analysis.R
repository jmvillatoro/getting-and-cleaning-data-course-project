rm(list=ls())
# Set the working directory. Remember use the directory where you unzip the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
setwd('C:/Users/JoséMiguel/Documents/Getting and Cleaning Data/UCI HAR Dataset');

# Loading the training data
features <- read.table('./features.txt',header=FALSE); 
activityType <- read.table('./activity_labels.txt',header=FALSE); 
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
xTrain <- read.table('./train/x_train.txt',header=FALSE); 
yTrain <- read.table('./train/y_train.txt',header=FALSE); 

# Loading the test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
xTest <- read.table('./test/x_test.txt',header=FALSE); 
yTest <- read.table('./test/y_test.txt',header=FALSE); 

# Assign column names
colnames(activityType) <- c('activityId','activityType');
colnames(subjectTrain) <- "subjectId";
colnames(xTrain) <- features[,2];
colnames(yTrain) <- "activityId";
colnames(subjectTest) <- "subjectId";
colnames(xTest) <- features[,2];
colnames(yTest) <- "activityId";

# Merging yTrain, subjectTrain, xTrain
training <- cbind(yTrain,subjectTrain,xTrain);

# Merging xTest, yTest, subjectTest
test <- cbind(yTest,subjectTest,xTest);

# Combine training and test data
trainingTest <- rbind(training,test);

# Create a vector of the column names from our new dataframe
columnNames <- colnames(trainingTest);

# Create a vector with the T values for mean and sd, ignoring the others.
meanSd <- (grepl("activity..",columnNames) | grepl("subject..",columnNames) | grepl("-mean..",columnNames) & !grepl("-meanFreq..",columnNames) & !grepl("mean..-",columnNames) | grepl("-std..",columnNames) & !grepl("-std()..-",columnNames));

# Subset trainingTest based on the meanSd
trainingTest <- trainingTest[meanSd==TRUE];

# Merge the trainingTest set with the activityType table for descriptive activity names
trainingTest <- merge(trainingTest,activityType,by='activityId',all.x=TRUE);

# Update columnNames to include the new column names after merge
columnNames <- colnames(trainingTest);

# Cleaning
for (i in 1:length(columnNames))
{
  columnNames[i] <- gsub("\\()","",columnNames[i])
  columnNames[i] <- gsub("-std$","StdDev",columnNames[i])
  columnNames[i] <- gsub("-mean","Mean",columnNames[i])
  columnNames[i] <- gsub("^(t)","Time",columnNames[i])
  columnNames[i] <- gsub("^(f)","Freq",columnNames[i])
  columnNames[i] <- gsub("([Gg]ravity)","Gravity",columnNames[i])
  columnNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames[i])
  columnNames[i] <- gsub("[Gg]yro","Gyro",columnNames[i])
  columnNames[i] <- gsub("AccMag","AccelMagnitude",columnNames[i])
  columnNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccelJerkMagnitude",columnNames[i])
  columnNames[i] <- gsub("JerkMag","JerkMagnitude",columnNames[i])
  columnNames[i] <- gsub("GyroMag","GyroMagnitude",columnNames[i])
  columnNames[i] <- gsub("-Timeime","Time",columnNames[i])
  columnNames[i] <- gsub("-Freqreq","Freq",columnNames[i])
};

# Descriptive column names for trainingTest.
colnames(trainingTest) <- columnNames;

# Create trainingTestNAT without the activityType column
trainingTestNAT <- trainingTest[,names(trainingTest) != 'activityType'];

# Summarize trainingTestNAT table including just the mean of each variable for each activity and each subject
tidyData <- aggregate(trainingTestNAT[,names(trainingTestNAT) != c('activityId','subjectId')],by=list(activityId=trainingTestNAT$activityId,subjectId = trainingTestNAT$subjectId),mean);

# Merge the tidyData with activityType to include descriptive actvity names
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
