library("plyr")
library("dplyr")

## use the features.txt file to create a character vector of feature names
features <- read.table("~/UCI HAR Dataset/features.txt", sep=" ", header=FALSE);
features <- as.character(features$V2);

##load in the test data
test_subjects_test <- read.csv("~/UCI HAR Dataset/test/subject_test.txt", head=FALSE);
test_Y_test <- read.csv("~/UCI HAR Dataset/test/Y_test.txt", head=FALSE);
test_X_test <- read.csv("~/UCI HAR Dataset/test/X_test.txt", head=FALSE);

##split the test_x_test data into a data frame using lapply to split the one field into 561 fields by splitting the text at every 16 characters for every row of data
test_X_test_df <- laply(seq(1,nrow(test_X_test), 1), function(i) laply(seq(1,nchar(as.character(test_X_test[i,])),16), function(j) substring(as.character(test_X_test[1,]), j, j+15)));

##update the column names for the test data
colnames(test_subjects_test) <- c("subject");
colnames(test_Y_test) <- c("activity");
colnames(test_X_test_df) <- features;

##merge the test data into a tidy set of test data
tidy_test <- cbind(test_subjects_test, test_Y_test, test_X_test_df)

train_subjects_train <- read.csv("~/UCI HAR Dataset/train/subject_train.txt", head=FALSE);
train_Y_train <- read.csv("~/UCI HAR Dataset/train/Y_train.txt", head=FALSE);
train_X_train <- read.csv("~/UCI HAR Dataset/train/X_train.txt", head=FALSE);

##split the train_X_train data into a data frame using lapply to split the one field into 561 fields by splitting the text at every 16 characters for every row of data
train_X_train_df <- laply(seq(1,nrow(train_X_train), 1), function(i) laply(seq(1,nchar(as.character(train_X_train[i,])),16), function(j) substring(as.character(train_X_train[1,]), j, j+15)));

colnames(train_subjects_train) <- c("subject");
colnames(train_Y_train) <- c("activity");
colnames(train_X_train_df) <- features

##merge the train data into a tidy set of train data
tidy_train <- cbind(train_subjects_train, train_Y_train, train_X_train_df)

##merge tidy_train and tidy_test for tidy_data
tidy_data <- rbind(tidy_test, tidy_train)

trim.leading <- function (x) sub("^\\s+", "", x)

##get activity labels
activityLabels <- read.csv("~/UCI HAR Dataset/activity_labels.txt", head=FALSE);
activityData <- cbind(substring(activityLabels$V1, 1, 1), (activityLabels$V1, 3, 20))
colnames(activityData) <- c("activity", "activityName")
activityData[,1] <- as.numeric(activityData[,1])
View(activityData)

##search column names for either "mean" or "std" (case insenstive) in order to pull a subset of columns
subjectAndActivityColumns <- c("subject", "activity")
meanAndStdColumns <- grep("mean|std", colnames(tidy_data), ignore.case = TRUE)
myvars<- colnames(tidy_data)[meanAndStdColumns]

tidy_subset_data <- tidy_data[append(subjectAndActivityColumns,myvars)]
tidy_subset_data[, 3:88] <- trim.leading(tidy_subset_data[, 3:88])

##update the measurements to numeric values
tidy_subset_data[, 3:88] <- sapply(tidy_subset_data[, 3:88], trim())

trimSpace(tidy_subset_data)

meantable <- aggregate(tidy_subset_data, by=list(tidy_subset_data$activity, tidy_subset_data$subject), FUN=mean)

View(meantable)
write.table(meantable, file = "gettingAndCleaning.txt", sep = ",", col.names = NA, qmethod = "double")
