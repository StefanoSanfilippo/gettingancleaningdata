# 0. PREPARING DE DATA

setwd("C:/DataPortatil/Coursera/DataScience_JHopskins/3-GettingandCleaningData/Project")
data_test<-read.table("X_test.txt")
data_train<-read.table("X_train.txt")
activity_test<-read.table("y_test.txt")
activity_train<-read.table("y_train.txt")
subject_test<- read.table("subject_test.txt")
subject_train<-read.table("subject_train.txt")
var_names<- read.table("features.txt")


# 4.	APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES
library(dplyr)
#Renaming the column of subject_test and subject_train
subject_test<- rename(subject_test, subject = V1)
subject_train<- rename(subject_train, subject = V1)

#Renaming the column of activity_test and activity_train
activity_test<-  rename(activity_test, activity = V1)
activity_train<-  rename(activity_train, activity = V1)

# Renaming columns of data_test and data_table, using var_names
colnames(data_test)[1:561] = as.character(var_names[,2])
colnames(data_train)[1:561] = as.character(var_names[,2])

names(data_test)
names(data_train)

# Binding subject and activity
subj_act_test<- cbind(subject_test, activity_test)
subj_act_train<- cbind(subject_train, activity_train)

# Adding a column indicating the dataframe
subj_act_test$dataset<- "test"
subj_act_train$dataset<- "train"


# 1.a MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET

# Row-binding data sets (data_test and data_train) in one: data_test_train
data_test_train<- rbind(data_test, data_train)
dim(data_test_train)

# Row-binding data sets (subj_act_test and subj_act_train) in one: subj_act_all
subj_act_all<- rbind(subj_act_test, subj_act_train)
dim(subj_act_all)
head(subj_act_all)
tail(subj_act_all)


# 2.	EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT

# Using the grep() function select the column names with "mean anf "std" (note the single quote
# to select two patterns with grep)
data_test_train_narrow<- data_test_train[,grep('mean|std', names(data_test_train))]
names(data_test_train_narrow)
head(data_test_train_narrow)
dim(data_test_train_narrow)


# 1.b MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET

# Binding subject_activity and data in the final data frame: ALLDATA, which include 79 measurement
# variables, plus activity, subject and the original set (test or train)
alldata<- cbind(subj_act_all, data_test_train_narrow)
dim(alldata)
head(alldata)
tail(alldata)


# 3.	USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET

# Substitute de activity values with the proper names
alldata$activity<-ifelse(alldata$activity==1, "WALKING",
                        ifelse(alldata$activity==2, "WALKING_UPSTAIRS",
                        ifelse(alldata$activity==3, "WALKING_DOWNSTAIRS",
                        ifelse(alldata$activity==4, "SITTING",
                        ifelse(alldata$activity==5, "STANDING",
                        ifelse(alldata$activity==6, "LAYING", "OTHER"))))))
head(alldata)
tail(alldata)


# 5.	FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH 
# THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT

# We can dot by grouping alldata with group_by() and summarise_at() functions of dplyr package.
# summarise_at() allows to compute the mean of multiple variables.
allmeans<- alldata %>%
  group_by(subject, activity) %>%
  summarise_at(.vars = names(.)[4:82], .funs = c(mean="mean"))

dim(allmeans)
names(allmeans)
head(allmeans)
tail(allmeans)
