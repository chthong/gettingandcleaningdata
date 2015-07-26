## Cook Book For run_analysis.R Project

The run_analysis.R script clean up the data collected from the accelerometers of Samsung Galaxy S smartphones and make it ready for further analysis. Full description of the data is available at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Source of the data is: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The script performs the following steps:

1. Merging the training and test data sets to create combined data sets, namely for X, y and subj (the test subjects).

2. Read features.txt, find the columns for mean and standard deviation, and extract the corresponding measurements. All measurements appear to be floating point numbers in the range (-1, 1).

3. Name and clean the column labels for X obtained from features.txt by removing '()' and change the names to lowercase. The first column contains the subject IDs. The second column contains the activity names and the remaining 66 columns contain the measurements. The cleaned features names for the measurements are similar to the following:
    * tbodyacc-mean-x 
    * tbodyacc-mean-y 
    * tbodyacc-mean-z 
    * tbodyacc-std-x 
    * tbodyacc-std-y 
    * tbodyacc-std-z
<br/></br>
4. Reads activity_labels.txt and applies descriptive activity names to name the activities in the data set, namely:
    * walking
    * walkingupstairs
    * walkingdownstairs
    * sitting
    * standing
    * laying
<br/></br>
5. Label y column as 'activity'.

6. Name subject column as 'subject'.

7. Creates a second, independent tidy data set with the average of each measurement for each activity and each subject. The result is saved as result_with_averages.txt.
