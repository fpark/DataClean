## run_analysis.R

library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)

## load activity label data used to create a factor variable in other dataset
activity_labels <- read.table("./Data/UCI HAR Dataset/activity_labels.txt",
                              header = FALSE,
                              stringsAsFactors=FALSE,
                              col.names=c("activity_id","label"))


## merge test and train dataset files to subject data frames with 10299 obs, 1 var
s1 <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt",
                 header = FALSE,
                 stringsAsFactors=FALSE,
                 col.names=c("subject_id"))
s2 <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt",
                 header = FALSE,
                 stringsAsFactors=FALSE,
                 col.names=c("subject_id"))
subject <- rbind(s1, s2)  ## combine rows
subject$id <- seq(along=subject$subject_id)  ## add unique row index
## subject has 10299 obs, 2 varisables - subject_id, index_id

## read y test and train data files and combine datasets
y1 <- read.table("./Data/UCI HAR Dataset/test/y_test.txt",
                 header = FALSE,
                 stringsAsFactors=FALSE,
                 col.names=c("activity_id"))
y2 <- read.table("./Data/UCI HAR Dataset/train/y_train.txt",
                 header = FALSE,
                 stringsAsFactors=FALSE,
                 col.names=c("activity_id"))
activity <- rbind(y1, y2)
activity$id <- seq(along=activity$activity_id)
## change activity_id to a factor variable with level from activity_lables dataset
f <-  as.factor(activity$activity_id)
levels(f) <- activity_labels$label
activity$activity_id <- f
########## activity now has 10299 obs, 2 vars - activity_id, index_id


## build  feature dataset used to filter mean and std computes ##############################
features <- read.table("./Data/UCI HAR Dataset/features.txt",
                       header = FALSE,
                       stringsAsFactors=FALSE,
                       col.names=c("feature_id", "descr"))
## split a column with three different variable into 3 different columns for each variable
f_temp <- colsplit(string=features[,2],
                     pattern="-",
                     names=c("feature", "computemethod", "xyz"))
features$computemethod <- sub("()", "", f_temp$computemethod, fixed=TRUE)
# ---> fileter features with std and mean
features <- features %>% filter(computemethod  %in% c("mean", "std"))
####### features now has 66 obs and 2 vars - feature_id, computemethod - mean, std



########### Build meausre dataset
x1 <- read.table("./Data/UCI HAR Dataset/test/x_test.txt", header = FALSE , stringsAsFactors=FALSE)
x2 <- read.table("./Data/UCI HAR Dataset/train/x_train.txt", header = FALSE, stringsAsFactors=FALSE)
measure <- rbind(x1, x2)  # 10299 obs, 561 vars
## measure has 10299 obs and 56 variables

## extract out V in V1, V2, ...V561 to 1,2.. to be able to map to feature_id
colnames(measure) <- gsub("^V", "", colnames(measure))
## only include measures -std, mean
measure <- measure[,features$feature_id]
measure$id <- seq(1,nrow(measure), by=1)  #don't use along parm with decimals cols no error msg and did not work
## 10299 obs, 66 variables + 1 = 67 vars including id field for join with other datasets
##########################################################################################


############# tidydata
## join subject, activity, measure datasets by id
## tidy data set with 10299 obs, 69 vars including id, subject_id, activity_id,
## and measure var(1-66)
dflist <- list(as.data.frame(subject), as.data.frame(activity), as.data.frame(measure))
tidydata <- join_all(dflist)

# change subject_id to factor variable
tidydata$subject_id <- as.factor(tidydata$subject_id)
## compute avg for each activity
tidydata$avg <- rowMeans(m[,3:68])

## only keep the relevant info to create cross tabs and convert it back to data frame for further shaping
tidydata <- tidydata[,c("subject_id", "activity_id", "avg")]
xt <- xtabs(avg ~ subject_id + activity_id, data=tidydata)
xt <- as.data.frame(xt)

## group and sort the output data by subject and activity
output <- xt %>% group_by(subject_id, activity_id) %>% arrange(subject_id, activity_id)

## make sure to include col.names and row.names paraeters to FALSE. Column names incorrect when set to TRUE
write.table(output, file = "./data/tidydata_output.txt", col.names=FALSE, row.names=FALSE)








