---
title: "Tidy Data Assignment"
date: "Sunday, April 26, 2015"
---

## Section 1 : load Data from the given url 
Given Url: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
The Dataset.zip is generated in the local current directory and test and train directory containing all the needed data files are extracted in Dataset/Data directory.
```{r loadData,echo=TRUE}
        if (!file.exists("Dataset.zip")){
                fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(fileUrl,destfile="./Dataset.zip",mode="wb")              
                unzip("./Dataset.zip", files = NULL, list = FALSE, overwrite = TRUE, exdir = "Dataset")
                t<- file.rename("Dataset/UCI HAR Dataset","Dataset/Data")
        }
```

## Section 2 : Read test data
```{r readTestData,echo=TRUE}
        ##read all the test data
        df_test_x <- read.table("Dataset/Data/test/X_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_test_y <- read.table("Dataset/Data/test/y_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_test_sub <- read.table("Dataset/Data/test/subject_test.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
```

## Section 3 : Read training data
```{r readTrainData,echo=TRUE}
        ##read all the training data
        df_train_x <- read.table("Dataset/Data/train/X_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_train_y <- read.table("Dataset/Data/train/y_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
        df_train_sub <- read.table("Dataset/Data/train/subject_train.txt",header = FALSE, sep = "",stringsAsFactors=FALSE)
```

## Section 4 : Read given column names data from features.txt 
The required columns names are extracted from the given column names.
```{r readcolumnNames,echo=TRUE}               
        ## read the feature.txt that is actually the column names for X_test.txt and X_train.txt values
        df_feature_labels <- read.table("Dataset/Data/features.txt",header = FALSE, sep = "",blank.lines.skip = TRUE)
        
        ## read the column names and filter out the ones with mean() and std() 
        ## and store the column index in neededCol
        colNams <- as.character(df_feature_labels[, "V2"])
        neededCol1 <- grep("mean()",colNams,fixed=T)
        neededCol2 <- grep("std()",colNams,fixed=T)
        neededCol <- sort(append(neededCol1,neededCol2))
```

## Section 5 : Extracting selective data 
Using the required columns names the needed data are extracted into merged
```{r extractAndMergetData,echo=TRUE, message=F, warning=F} 
         library(dplyr)
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
```

## Section 6 : Group and summarize selective data 
Using group and summarize find the average per activity per subject
```{r groupAndSummarise,echo=TRUE, message=F, warning=F} 
        library(dplyr)
        ## calculate the mean for every column grouped by subject and activity
        dx<- df_selectedCol %>% group_by(subject,activity) %>% summarise_each(funs(mean))

```

## Section 7 : Add column names to the resultant data 
Add meaningful column names to the result set.
```{r addColumnNames,echo=TRUE} 
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

```

## Section 8 : Add activity name in place of activity id 
```{r addactivityNames,echo=TRUE} 
        df_activity_names <- read.table("Dataset/Data/activity_labels.txt",header = FALSE, sep = "",blank.lines.skip = TRUE)
        names(df_activity_names)<- c("activity","name")
        dx<-merge(dx,df_activity_names,by = "activity", all.x=TRUE)
        tx<-dx[,c(2,69,3:68)]
        colnames(tx)[2] <- "activity"
        tx <- tx[order(tx$subject),]

```

## Section 9 : Write result data and column name information into a text 
The result.txt generated in the current directory has the average for all the subjects. This is a comma separated txt file and each subjects's activity data is separated from one another by "\\n". It contains 180 rows and 70 columns (68 avarage data, subject and activity)

The columnNames.txt contains all the column names for the result.txt.
```{r writeFiles,echo=TRUE}         
        write.table(names(tx), file = "columnNames.txt", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
        write.table(tx, file = "results.txt", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
```

```{r, include=FALSE}
   # add this chunk to end of README.rmd
   file.rename(from="README.rmd", 
               to="README.md")
```