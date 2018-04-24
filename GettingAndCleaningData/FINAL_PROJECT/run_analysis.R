
################################################################
# 1 MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET
################################################################

# create data directory to store data
if (!file.exists('data')){
  dir.create('data')
}
 
# download data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = 'data/data_project.zip')
unzip('data/data_project.zip',exdir = 'data/')
date_of_download <- date()

# upload the files into R

features <- read.table('data/UCI HAR Dataset/features.txt',
                       col.names = c('Index_Feature','Feature'))
activity_labels <- read.table('data/UCI HAR Dataset/activity_labels.txt',
                              col.names = c('Index_Activity','Activity'))

X_train <- read.table('data/UCI HAR Dataset/train/X_train.txt',
                      col.names = features$Feature,check.names = FALSE)
y_train <- read.table('data/UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('data/UCI HAR Dataset/train/subject_train.txt')

X_test <- read.table('data/UCI HAR Dataset/test/X_test.txt',
                     col.names = features$Feature,check.names = FALSE)
y_test <- read.table('data/UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('data/UCI HAR Dataset/test/subject_test.txt')

X_train_p = cbind(y_train, subject_train,X_train)
X_test_p = cbind(y_test, subject_test,X_test)

names(X_train_p)[c(1,2)] <- c('Activity','Subject')
names(X_test_p)[c(1,2)] <- c('Activity','Subject')

write.csv(x=X_train_p,file = 'X_train.csv',sep = ',',row.names = F)
write.csv(x=X_test_p,file = 'X_test.csv',sep = ',',row.names = F)


# merging train and test sets

X_data <- rbind(X_train,X_test)
y_data <- rbind(y_train,y_test)
subject_data <- rbind(subject_train,subject_test)


#####################################################################
# 2 EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION 
#####################################################################

col_names <- colnames(X_data)
col_mean  <- grep('mean\\(\\)',col_names)
col_std   <- grep('std\\(\\)',col_names)

X_mean_std <- X_data[,c(col_mean,col_std)]

##########################################################################
# 3 USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
##########################################################################

X_clean <- cbind(y_data,X_mean_std)
colnames(X_clean)[1] <- 'Index_Activity'
X_clean <- cbind(subject_data,X_clean)
colnames(X_clean)[1] <- 'Subject'

X_clean <- merge(activity_labels,X_clean)
X_clean$Index_Activity <- NULL

#####################################################################
# 4 APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES 
#####################################################################


col_names <- colnames(X_clean)

col_names <- sub(x = col_names,pattern = '^t',replacement = 'Time domain signal: ')
col_names <- sub(x = col_names,pattern = '^f',replacement = 'Frequency domain signal: ')
col_names <- sub(x = col_names,pattern = '-',replacement = ', ')
col_names <- sub(x = col_names,pattern = 'mean\\(\\)',replacement = ' mean value ')
col_names <- sub(x = col_names,pattern = 'std\\(\\)',replacement = ' standart deviation ')
col_names <- sub(x = col_names,pattern = '-X',replacement = 'in X direction')
col_names <- sub(x = col_names,pattern = '-Y',replacement = 'in Y direction')
col_names <- sub(x = col_names,pattern = '-Z',replacement = 'in Z direction')
col_names <- sub(x = col_names,pattern = 'AccJerk',replacement = ' acceleration jerk')
col_names <- sub(x = col_names,pattern = 'Acc',replacement = ' acceleration')
col_names <- sub(x = col_names,pattern = 'GyroJerk',replacement = ' angular velocity jerk')
col_names <- sub(x = col_names,pattern = 'Gyro',replacement = ' angular velocity')
col_names <- sub(x = col_names,pattern = 'Mag',replacement = ' magnitude')

colnames(X_clean) <- col_names

######################################################################################
# 5 TIDY DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT
######################################################################################

X_tidy <- aggregate(X_clean[,3:68],by=list(X_clean$Activity,X_clean$Subject),FUN=mean)
colnames(X_tidy)[1] <- 'Activity'
colnames(X_tidy)[2] <- 'Subject'


write.table(X_tidy,file = 'tidy_data_set.txt',row.names = F)


