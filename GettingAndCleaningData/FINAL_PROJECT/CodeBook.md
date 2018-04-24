# Code Book

## Variables

- "subject_data": 30 subjects, which were participated in the experiment 
- "features": names of measurements made on subjects
- "X_data": merged train and test data from measurements
- "y_data": merged train and test labels of activities used for measurements
- "X_mean_std": measurements on the mean and standard deviation
- "X_clean": cleaned version of "X_mean_std", in which activities and variables are descriptive
- "X_tidy": aggregated measurements from "X_clean" for every activity and subject with "mean" function

## Project data

- all data are downloaded using [this link](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

## Output

- the final tidy data set "X_tidy" is stored in "tidy_data_set.txt"
