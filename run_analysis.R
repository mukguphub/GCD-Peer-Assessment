
## 1) Merges the training and the test sets to create one data set
read_merge_data <- function(directory) {
  ## read the dataset in test and train sets
  path <- paste("./", directory, "/test/X_test.txt", sep="")
  test_data <- read.table(path)
  path <- paste("./", directory, "/train/X_train.txt", sep="")
  train_data <- read.table(path)
  
  ## read the activity labels
  path <- paste("./", directory, "/activity_labels.txt", sep="")
  activity_labels <- read.table(path)
  
  ## read the test and training subject labels
  path <- paste("./", directory, "/train/subject_train.txt", sep="")
  subject_train <- read.table(path)
  path <- paste("./", directory, "/test/subject_test.txt", sep="")
  subject_test <- read.table(path)
  
  ## read the test and training y labels
  path <- paste("./", directory, "/train/y_train.txt", sep="")
  y_train <- read.table(path)
  path <- paste("./", directory, "/test/y_test.txt", sep="")
  y_test <- read.table(path)
  
  ## merge y test and training activity labels
  y_train_labels <- merge(y_train,activity_labels,by="V1")
  y_test_labels <- merge(y_test,activity_labels,by="V1")
  
  ## merge the test and training data and the respective labels together
  train_data <- cbind(subject_train,y_train_labels,train_data)
  test_data <- cbind(subject_test,y_test_labels,test_data)
  
  ## now then we'll merge the test and training data together
  all_data <- rbind(train_data,test_data)
  
  return (all_data)
}

## 2) Extracts only the measurements on the mean and standard deviation for each measurement

extract_mean <- function(data_set, directory) {
  path <- paste("./", directory, "/features.txt", sep="")
  features_data <- read.table(path)
  mean_std_rows <- subset(features_data,  grepl("(mean\\(\\)|std\\(\\))", features_data$V2) )
  
  colnames(data_set) <- c("Subject","Activity_Id","Activity",as.vector(features_data[,2]))
  
  mean_columns <- grep("mean()", colnames(data_set), fixed=TRUE)
  std_columns <- grep("std()", colnames(data_set), fixed=TRUE)
  
  mean_std_column_vector <- c(mean_columns, std_columns)
  
  mean_std_column_vector <- sort(mean_std_column_vector)
  
  extracted_data_set <- data_set[,c(1,2,3,mean_std_column_vector)]
  return (extracted_data_set)
}

## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive activity names
## 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject

write_tidy_set <- function(data_set, path_to_tidyset_file) {
  require(reshape2)
  melt_data <- melt(data_set, id=c("Subject","Activity_Id","Activity"))
  
  tidy_data <- dcast(melt_data, formula = Subject + Activity_Id + Activity ~ variable, mean)
  
  ## format the column names
  col_names_vector <- colnames(tidy_data)
  col_names_vector <- gsub("-mean()","Mean",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("-std()","Std",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("BodyBody","Body",col_names_vector,fixed=TRUE)
  
  ## put back in the tidy column names
  colnames(tidy_data) <- col_names_vector
  
  ## write the output into a file
  write.table(tidy_data, file=path_to_tidyset_file, sep="\t", row.names=FALSE)
}

merged_data <- read_merge_data("UCI HAR Dataset")
extracted_mean_std_data_set <- extract_mean(merged_data, "UCI HAR Dataset")
write_tidy_set(extracted_mean_std_data_set, "./tidydata.txt")

read_tidy_set <- function(path_to_tidyset_file) {
  tidy_set <- read.table(path_to_tidyset_file)
  
  return (tidy_set)
}

