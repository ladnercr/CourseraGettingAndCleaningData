## Course Project
library(dplyr)

if (!file.exists("UCI HAR Dataset")) {
    dir.create("UCI HAR Dataset")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./UCI HAR Dataset/Dataset.zip")

unzip(zipfile="./UCI HAR Dataset/Dataset.zip")

# ,exdir="./UCI HAR Dataset")
## Part One:  Merge the training and the test sets to create one data set. ###########################

## Read in the necessary files
train.data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
test.data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Merge the columns & rows to create the merged dataset
dat <- rbind(cbind(train.data, train.activity, train.subject), cbind(test.data, test.activity, test.subject))


## Part Two:  Extract only the measurements on the mean and sd for each measurement. ################
## Read in the features dataframe & use it to name the variables of merged dataset
features <- read.table("./UCI HAR Dataset/features.txt")
features_vector <- as.character(features$V2)

## Create the names vector and apply it to the data dataframe to name its' variables
names_vector <- c(features_vector, "activityID", "subject")
names(dat) <- names_vector

vec <- grep("mean\\(\\)|std\\(\\)", features_vector, value = TRUE)
dat <- dat[, c(vec,"activityID", "subject" )]


## Part 3: Use descriptive activity names to name the activities in the data set
activity.labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity.labels) <- c("V1" = 'activityID', "V2" = 'activity')
dat <- left_join(dat, activity.labels, by = "activityID")


## Part 4: Appropriately labels the data set with descriptive variable names.
## gsub replaces all matches
names(dat) <- gsub("Acc", "Acceleration", names(dat))
names(dat) <- gsub("Gyro", "Gyroscope", names(dat))
names(dat) <- gsub("Mag", "Magnitude", names(dat))
## gsub^ replaces just the first character in a string if applicable
names(dat) <- gsub("^t", "time", names(dat))
names(dat) <- gsub("^f", "frequency", names(dat))


## Step 5: From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.
activity.subject <- dat %>%
    group_by(activity, subject) %>%
    summarise_each(funs(mean)) %>%
    arrange(activityID)