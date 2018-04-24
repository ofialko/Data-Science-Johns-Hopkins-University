# Getting and Cleaning Data in R

One of the most exciting areas in all of data science right now is wearable computing. 
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data collected from the accelerometers from the Samsung Galaxy S smartphone. 
The goal here is to prepare tidy data that can be used for later analysis.

A full description of the project is available [here](http://rpubs.com/fialkool/Gettingandcleaning).



The script run_analysis.R does the following

- creates a folder "data" and downloads projects data into it
- unzips the data, since project data are all enclosed in a single .zip file
- reads variables names into "features". These are used to name the columns in the final data set
- reads activity label names into "activity_labels". These are used to name activity of every measurement   
- reads train and test measurements data and corresponding labels  into X and y data sets
- merges train and test data sets into "X_data" and "y_data"
- uses "grep" function to extract only measurements on the mean and standard deviation
- uses a combination of "cbind" and "merge" functions to name the activities in the data set 
- uses "sub" function and regular expressions to label the data set with descriptive variable names
- aggregates the data set for each activity and subject
- writes the final tidy data set into "tidy_data_set.txt" file.	
