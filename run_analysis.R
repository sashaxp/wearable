#1.  Merges the training and the test sets to create one data set.
#2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.	Uses descriptive activity names to name the activities in the data set
#4.	Appropriately labels the data set with descriptive variable names. 
#5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


runanalysis <- function() {

  trainDataUri<- ".\\UCI HAR Dataset\\train\\X_train.txt"
  trainLabelsUri<- ".\\UCI HAR Dataset\\train\\y_train.txt"
  trainSubjectsUri<- ".\\UCI HAR Dataset\\train\\subject_train.txt"

  testDataUri<- ".\\UCI HAR Dataset\\test\\X_test.txt"
  testLabelsUri<- ".\\UCI HAR Dataset\\test\\y_test.txt"
  testSubjectsUri<- ".\\UCI HAR Dataset\\test\\subject_test.txt"

  activityLabelsUri<-".\\Documents\\UCI HAR Dataset\\activity_labels.txt"
  featuresUri<-".\\Documents\\UCI HAR Dataset\\features.txt"
  
  # reading trains
  trainData <- read.table(trainDataUri, header=FALSE)
  trainLabels <- read.table(trainLabelsUri, header=FALSE)
  trainSubjects <- read.table(trainSubjectsUri, header=FALSE)
  
  # reading tests
  testData <- read.table(testDataUri, header=FALSE)
  testLabels <- read.table(testLabelsUri, header=FALSE)
  testSubjects <- read.table(testSubjectsUri, header=FALSE)
  
  # Naming activities in the data set
  
  activities <- read.table(activityLabelsUri,header=FALSE,colClasses="character")
  testLabels$V1 <- factor(testLabels$V1,levels=activities$V1,labels=activities$V2)
  trainLabels$V1 <- factor(trainLabels$V1,levels=activities$V1,labels=activities$V2)
  
  # Labeling the data set with descriptive activity names
  features <- read.table(featuresUri,header=FALSE,colClasses="character")
  colnames(testData)<-features$V2
  colnames(trainData)<-features$V2
  colnames(testLabels)<-c("Activity")
  colnames(trainLabels)<-c("Activity")
  colnames(testSubjects)<-c("Subject")
  colnames(trainSubjects)<-c("Subject")
  
  # Merging  test and training sets into one data set
  testData<-cbind(testData,testLabels)
  testData<-cbind(testData,testSubjects)
  trainData<-cbind(trainData,trainLabels)
  trainData<-cbind(trainData,trainSubjects)
  data<-rbind(testData,trainData)
  
  # Extracting mean and standard deviation measurements
  data_mean<-sapply(data,mean,na.rm=TRUE)
  data_sd<-sapply(data,sd,na.rm=TRUE)
  
  # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  DT <- data.table(data)
  tidy<-DT[,lapply(.SD,mean),by="Activity,Subject"]
  write.table(tidy,file="tidy.txt",sep=",",row.names = FALSE)
  
}