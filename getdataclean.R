if(!("qdap" %in% installed.packages())){install.packages("qdap")}
library(qdap)# if qdap package is not installed, install and load qdap package(for mgsub function)
if(!("dplyr" %in% installed.packages())){install.packages("dplyr")}
library(dplyr) # if dplyr package is not installed, install and load dplyr package

############################# load data and variable names ############################################
X_train<-read.table("UCI HAR Dataset/train/X_train.txt") #read X_train data
y_train<-read.table("UCI HAR Dataset/train/y_train.txt") #read y_train data
sub_train<-read.table("UCI HAR Dataset/train/subject_train.txt") #read subject_train data

X_test<-read.table("UCI HAR Dataset/test/X_test.txt") #read X_test data
y_test<-read.table("UCI HAR Dataset/test/y_test.txt") #read y_test data
sub_test<-read.table("UCI HAR Dataset/test/subject_test.txt") #read subject_test data

act_label<-read.table("UCI HAR Dataset/activity_labels.txt")
features<-read.table("UCI HAR Dataset/features.txt")

####################### merge training and test sets and label the data ###############################
Train<-cbind(sub_train,y_train,X_train) #put training set subject,activity,and data together

Test<-cbind(sub_test,y_test,X_test) #put test set subject,activity,and data together

ALL<-rbind(Train,Test) #Merges the training and the test sets to create one data set

colname<-as.character(features[[2]]) 

colnames(ALL)<-c("Subject","Activity",colname) #label the data set with descriptive variable names.

ALL$Activity<-mgsub(act_label[[1]],act_label[[2]],ALL$Activity) #Use descriptive activity names to name the activities in the data set

####################### make labels cleaner ###############################
ALL_data<-ALL[,c("Subject","Activity",grep("mean\\(\\)|std\\(\\)",colnames(ALL),value=TRUE))] #Extracts only the measurements on the mean and standard deviation for each measurement.

colnames(ALL_data)<-mgsub(c("Acc","Gyro","Mag","BodyBody"),c("Accelerometer","Gyroscope","Magnitude","Body"),colnames(ALL_data)) #make the descriptive variable names easier to read

colnames(ALL_data)<-gsub("\\(|\\)","",colnames(ALL_data))

############### get tidy data set with the average for each activity and each subject ################
alldata<-group_by(ALL_data,Subject,Activity)

tidy<-summarize_each(alldata,funs(mean)) #creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(tidy,"tidydata.txt",row.name = FALSE)


