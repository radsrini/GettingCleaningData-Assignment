run_analysis<-function(){
        library(dplyr)
        ## loads data from the url https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
        if (!file.exists("Dataset.zip")){
                fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(fileUrl,destfile="./Dataset.zip",mode="wb")              
                unzip("./Dataset.zip", files = NULL, list = FALSE, overwrite = TRUE, exdir = "Dataset")
                t<- file.rename("Dataset/UCI HAR Dataset","Dataset/Data")
        }
        
        ##read all the test data
        df_test_x <- read.table("Dataset/Data/test/X_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_test_y <- read.table("Dataset/Data/test/y_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_test_sub <- read.table("Dataset/Data/test/subject_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        
        ##read all the training data
        df_train_x <- read.table("Dataset/Data/train/X_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_train_y <- read.table("Dataset/Data/train/y_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_train_sub <- read.table("Dataset/Data/train/subject_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
               
        ## read the feature.txt that is actually the column names for X_test.txt and X_train.txt values
        df_feature_labels <- read.table("Dataset/Data/features.txt",header = FALSE, sep = "",blank.lines.skip = TRUE)
        
        ## read the column names and filter out the ones with mean() and std() 
        ## and store the column index in neededCol
        colNams <- as.character(df_feature_labels[, "V2"])
        neededCol1 <- grep("mean()",colNams,fixed=T)
        neededCol2 <- grep("std()",colNams,fixed=T)
        neededCol <- sort(append(neededCol1,neededCol2))

        ## consolidate test and training date
        df_withColName <- rbind(df_test_x,df_train_x)
        ## select only the needed columns
        df_selectedCol <- select(df_withColName, neededCol)
        names(df_selectedCol) <- colNams[neededCol]
 
        ## add the activity column and values from y_test.txt and y_train.txt
        df_activity <- rbind(df_test_y,df_train_y)
        names(df_activity) <- "activity"
        df_activity$activity <- as.character(df_activity$activity)
        df_selectedCol <- cbind(df_selectedCol,df_activity)

        ## add the subject column and values from subject_test.txt and subject_train.txt
        df_subject <- rbind(df_test_sub,df_train_sub)
        names(df_subject) <- "subject"
        df_subject$subject <- as.character(df_subject$subject)
        df_selectedCol <- cbind(df_selectedCol,df_subject)

        ## calculate the mean for every column grouped by subject and activity
        dx<- df_selectedCol %>% group_by(subject,activity) %>% summarise_each(funs(mean))
        
        ## add column names
        resultLabels <-names(dx)
        resultLabels <- gsub("^tBody","timeBody",resultLabels);
        resultLabels <- gsub("^tGravity","timeGravity",resultLabels);
        resultLabels <- gsub("^fBody","frequencyBody",resultLabels);
        resultLabels <- gsub("^fGravity","frequencyGravity",resultLabels);
        resultLabels <- gsub("(Acc)","Accelerometer",resultLabels);
        resultLabels <- gsub("(Gyro)","Gyroscope",resultLabels);
        resultLabels <- gsub("(BodyBody)","Body",resultLabels);
        resultLabels <- gsub("(-mean)","Mean",resultLabels);
        resultLabels <- gsub("(-std)","Standard",resultLabels);
        resultLabels <- gsub("()","",resultLabels,fixed=T);
        names(dx) <- resultLabels
        
        
        #dx<-merge(dx,df2,by = "CustomerId", all.x=TRUE)

        df_activity_names <- read.table("Dataset/Data/activity_labels.txt",header = FALSE, sep = "",blank.lines.skip = TRUE)
        names(df_activity_names)<- c("activity","name")
        dx<-merge(dx,df_activity_names,by = "activity", all.x=TRUE)
        tx<-dx[,c(2,69,3:68)]
        colnames(tx)[2] <- "activity"
        tx <- tx[order(tx$subject),]
        write.table(names(tx), file = "columnNames.txt", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
        
        write.table(tx, file = "results.txt", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
}

test<-function(){
#         df1 = data.frame(CustomerId=c(1:6),Product=c(rep("Toaster",3),rep("Radio",3)))
#         df2 = data.frame(CustomerId=c(2,4,6),State=c("Alabama","Ohio","CA"))
#         tx<-merge(df1,df2,by = "CustomerId", all.x=TRUE)
#         tx
        df_activity_names <- read.table("Dataset/Data/activity_labels.txt",header = FALSE, sep = "",blank.lines.skip = TRUE)
        names(df_activity_names)<- c("activity","name")
        
        print(df_activity_names)
        dx = data.frame(activity=c(1:6),c1=c(rep("Toaster",3),rep("Radio",3)))
        #dx$activity[df_activity_names$activity == dx$activity] <- df_activity_names$name
        #dx$activity <- replace(dx$activity, dx$activity == df_activity_names$activity, df_activity_names$name)
        #dx
        #dx <- lapply(dx, function(x){replace(dx$activity, dx$activity == df_activity_names$activity, df_activity_names$name)})
        #df[,c(1,3,2,4,5:50)]
        dx$activity[dx$activity == df_activity_names$activity] <- df_activity_names$name
        tx<-merge(dx,df_activity_names,by = "activity", all.x=TRUE)
        tx<-tx[,c(3,2)]
        tx
}
