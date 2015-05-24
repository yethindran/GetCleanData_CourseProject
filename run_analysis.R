run_analysis <- function() {

	x_test <- read.table("./test/X_test.txt")
	y_test <- read.table("./test/y_test.txt")
	subject_test <- read.table("./test/subject_test.txt")

	x_train <- read.table("./train/X_train.txt")
	y_train <- read.table("./train/y_train.txt")
	subject_train <- read.table("./train/subject_train.txt")

	x_all <- rbind(x_train, x_test)
	y_all <- rbind(y_train, y_test)
	subject_all <- rbind(subject_train, subject_test)

	features <- read.table("./features.txt")

	selColNames1 <- as.character(features$V2[grep("mean()",features$V2)])
	selColNames2 <- as.character(features$V2[grep("std()",features$V2)])
	selColNames <- c(selColNames1, selColNames2)

	colnames(x_all) <- c(as.character(features$V2))
	mergeFile <- cbind(subject_all, y_all, x_all)
	selColNames <- c("subject", "activity", c(selColNames))
	colnames(mergeFile) <- c(selColNames)

	meanStdFile <- subset(mergeFile, select = c(selColNames))

	descriptiveNames <- selColNames
  	for(j in 3:81)
	{
		descriptiveNames[j] <- descriptiveName(selColNames[j]) 
	}
	colnames(meanStdFile) <- c(descriptiveNames)

	library(plyr)
	finalDF <- ddply(meanStdFile, .(subject, activity), colwise(mean))

	activity_labels <- read.table("./activity_labels.txt")
	actLabels <- lapply(activity_labels, as.character)
	activityNames <- c(1:180)
	activityNames <- as.character(activityNames)
	for (i in 1:180)
	{
   		activityNames[i] <- actLabels$V2[finalDF$activity[i]] 
	}
	finalDF$activity <- activityNames
	write.table(finalDF, "tidy_dataset.txt",row.name=FALSE)

}




# this function creates a descriptive name for the column
descriptiveName <- function(str) {
 
	returnStr <- NULL
   
	if(length(grep("BodyAccJerkMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gyro Jerk Magnitude of Body Acceleration ")
	}
	else if(length(grep("BodyGyroJerkMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gyro Jerk Magnitude of Body Angular velocity ")
	}
	else if(length(grep("BodyAccJerk",str)) > 0)
	{
		returnStr <- paste(returnStr,"Jerk of Body Acceleration ")
	}
	else if(length(grep("GravityAccMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration Magnitude ")
	}
	else if(length(grep("GravityAcc",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration ")
	}
	else if(length(grep("BodyAccMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Body Acceleration Magnitude ")
	}
	else if(length(grep("BodyGyroJerk",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gyro Jerk of Body Angular velocity ")
	}
	else if(length(grep("BodyGyro",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration Magnitude ")
	}
	else if(length(grep("BodyAcc",str)) > 0)
	{
		returnStr <- paste(returnStr,"Body Acceleration ")
	}


	if(length(grep("meanFreq",str)) > 0)
	{
		returnStr <- paste(returnStr,"Average frequency ")
	}	
	else if(length(grep("mean",str)) > 0)
	{
		returnStr <- paste(returnStr,"Average ")
	}


	if(length(grep("std",str)) > 0)
	{
		returnStr <- paste(returnStr,"Standard Deviation ")
	}


	if(length(grep("-X",str)) > 0)
	{
		returnStr <- paste(returnStr,"in X axis ")
	}
	if(length(grep("-Y",str)) > 0)
	{
		returnStr <- paste(returnStr,"in Y axis ")
	}
	if(length(grep("-Z",str)) > 0)
	{
		returnStr <- paste(returnStr,"in Z axis ")
	}

	returnStr

}
