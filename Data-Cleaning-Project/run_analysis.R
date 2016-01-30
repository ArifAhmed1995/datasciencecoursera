run_analysis <- function()
{
    train_feature_values <- read.table(file = "UCI HAR Dataset/train/X_train.txt")
    #This contains the data for the 561 variables for each state,each id in the training set.(For example 561 values for ID : 3, State : WALKING)
    
    train_state <- read.table(file = "UCI HAR Dataset/train/y_train.txt")
    #Contains data about state of the current ID.(Walking,Lying,etc) in the train set.
    
    train_ids <- read.table(file = "UCI HAR Dataset/train/subject_train.txt")
    #Contains data about the current ID in the train set.This should also be the first column of the clean dataset.
    
    test_feature_values <- read.table(file = "UCI HAR Dataset/test/X_test.txt")
    #This contains the data for the 561 variables for each state,each id in the testing set.(For example 561 values for ID : 3, State : WALKING)
    
    test_state <- read.table(file = "UCI HAR Dataset/test/y_test.txt")
    #Contains data about state of the current ID.(Walking,Lying,etc) in the test set.
    
    test_ids <- read.table(file = "UCI HAR Dataset/test/subject_test.txt")
    #Contains data about the current ID in the test set.This should also be the first column of the clean dataset.
    
    features <- read.table(file = "UCI HAR Dataset/features.txt")
    #Get data about thetest_data_full <- cbind(train_ids,train_state,train_feature_values) measurements.
    
    #Merging data into one data frame.
    train_data_full <- cbind(train_ids,train_state,train_feature_values)
    test_data_full <- cbind(test_ids,test_state,test_feature_values)
    complete_data <- rbind(test_data_full,train_data_full)
    
    required_columns <- vector(mode = "character", length = 0L)
    #This is the vector of required columns which have the search words 'mean' and 'std'.
    
    library(stringr)
    library(plyr)
    library(reshape2)
    library(dplyr)
    
    temp <- as.character(features$V2)
    colnames(complete_data) <- c(c("Id","State"),temp)
    #Setting reasonable column names. 
    
    column_choice <- c(c(TRUE,TRUE),grepl("mean|std",temp))
    #Only Id,State and the columns containing keywords 'mean' or 'std' are selected for the complete dataset.
    
    complete_data$State <- mapvalues(as.character(complete_data$State),from = c("1","2","3","4","5","6")
                                     ,to = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
    #Replacing state by the key values (i.e. Walking,sitting,etc)
    
    complete_data <- complete_data[,column_choice]
    #Extract the final complete dataset

    tidy_data <- complete_data
    tidy_data <- tidy_data %>% group_by(Id,State) %>% summarise_each(funs(mean))
    write.csv(tidy_data,file = "Tidy-Data.csv")
    print(colnames(tidy_data))
    #Summarize the variables according to Id and State.
}