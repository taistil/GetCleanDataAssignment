run_analysis <- function (x) {
  
  #Load in training data sets
  
  subjectrain <- read.table("subject_train.txt")
  trainset <- read.table("X_train.txt")
  trainlabel <- read.table("y_train.txt")
  
  #Load in test data sets
  
  subjectest <- read.table("subject_test.txt")
  testset <- read.table("X_test.txt")
  testlabel <- read.table("y_test.txt")
  
  #Load in activity labels and put in descriptive columns
  
  activitylabels <- read.table("activity_labels.txt")
  actname <- c("Key","ActivityLabel")
  colnames(activitylabels) <- actname
  
  #Adhere to tidy data by putting a descriptive name for column headers of subject ID data set and activity data set
  colnames(subjectrain)[1] <- "SubjectID"
  colnames(subjectest)[1] <- "SubjectID"
  colnames(trainlabel)[1] <- "Activity"
  colnames(testlabel)[1] <- "Activity"
  
  #Adhere to tidy data by putting a descriptive name for column header of combined data set
  
  features <- read.table("features.txt")
  featuresvec <- as.character(features[,2])
  colnames(testset) <- featuresvec
  colnames(trainset) <- featuresvec
  
  #Bind Activities and subject IDs to the test and training sets
  
  trainset <- data.frame(subjectrain, trainlabel, trainset)
  testset <- data.frame(subjectest, testlabel, testset)
  
  #Bind test and training data
  combinedset <- rbind(testset,trainset)
  
  #Select only columns with "mean" and "std"
  patterns <- c("mean","std")
  parsedcombined <- combinedset[,c(1,2,grep(paste(patterns, collapse="|"),names(combinedset)))]
  
  #Replace activity label key with activity labels in combined dataset
  
  parsedcombined$Activity <- activitylabels$ActivityLabel[parsedcombined$Activity]
  
  #Make column names more readable; remove abbreviations
  
  colnames(parsedcombined) <- sub("tBody", "timeBody", colnames(parsedcombined))
  colnames(parsedcombined) <- sub("fBody", "frequencyBody", colnames(parsedcombined))
  colnames(parsedcombined) <- sub("Mag", "Magnitude", colnames(parsedcombined))
  colnames(parsedcombined) <- sub("Acc", "Accelerometer", colnames(parsedcombined))
  
  #Arrange by activity
  
  parsedcombined <- arrange(parsedcombined, Activity)
  
  #Group by Subject ID and Activity, then get the mean of each activity for each subject
  
  tidyset <- parsedcombined %>% group_by(SubjectID, Activity)
  tidyset %>% summarise_at(3:81,mean)
  
}