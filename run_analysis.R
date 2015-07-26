# 1. Merges the training and the test sets to create one data set.

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_combined <- rbind(x_train, x_test)
y_combined <- rbind(y_train, y_test)
subj_combined <- rbind(subject_train, subject_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

x_names <- read.table("UCI HAR Dataset/features.txt")
required_columns <- grep("-mean[(][)]|-std[(][)]", x_names[, 2])
x_combined <- x_combined[, required_columns]
names(x_combined) <- x_names[required_columns, 2]
names(x_combined) <- tolower(names(x_combined))
names(x_combined) <- gsub("[(]|[)]", "", names(x_combined))


# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y_combined[,1] = activities[y_combined[,1], 2]
names(y_combined) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names.

names(subj_combined) <- "subject"
dt <- cbind(subj_combined, y_combined, x_combined)


# 5. From the data set in Step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

unique_subject = unique(subj_combined)[,1]
subject_count = length(unique_subject)
activity_count = length(activities[,1])
numCols = dim(dt)[2]
result = dt[1:(subject_count*activity_count), ]

row = 1
for (s in 1:subject_count) {
        for (a in 1:activity_count) {
                result[row, 1] = unique_subject[s]
                result[row, 2] = activities[a, 2]
                tmp <- dt[dt$subject==s & dt$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}

write.table(result, "result_with_averages.txt", row.names = FALSE)