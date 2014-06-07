#step1
trainData <- read.table('./train/X_train.txt')
dim(trainData)
head(trainData)
trainLabel <- read.table('./train/Y_train.txt')
table(trainLabel)
trainSubject <- read.table('./train/subject_train.txt')
testData <- read.table('./test/X_test.txt')
dim(testData)
testLabel <- read.table('./test/y_test.txt')
table(testLabel)
testSubject <- read.table('./test/subject_test.txt')
joinData <- rbind(trainData,testData)
joinLabel <- rbind(trainLabel,testLabel)
joinSubject <- rbind(trainSubject,testSubject)
#Step2
features <- read.table('./features.txt')
dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)",features[,2])
length(meanStdIndices) 
joinData <- joinData[,meanStdIndices]
dim(joinData) ## 10299*66
names(joinData) <- gsub("\\(\\)","",features[meanStdIndices,2])
names(joinData) <- gsub("mean","Mean",names(joinData))
names(joinData) <- gsub("std","Std",names(joinData))
names(joinData) <- gsub("-","",names(joinData))
#Step3
activity <- read.table('./activity_labels.txt')
activity[,2] <- tolower(activity[,2])
activityLabel <- activity[joinLabel[,1],2]
joinLabel[,1] <- activityLabel
names(joinLabel) <- "activity"
#step4
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject,joinLabel,joinData)
dim(cleanedData)
write.table(cleanedData,"merged_data.txt") # write out the 1st dataset
#step5
subjectLen <- length(table(joinSubject))
activityLen <- nrow(activity)
columnLen <- ncol(cleanedData)
result <- matrix(NA,nrow=subjectLen*activityLen,ncol=columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "tidy_dataset.txt") # write out the 2nd dataset