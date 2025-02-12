CodeBook
================================================
This file describes the variables, the data, and other transformations or work I did to clean the data.

* The site where the data was obtained:  
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones      
The data for the project:  
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip   
* The file run_analysis.R performs the following steps th clean the data for the project:  
 1.Read the X\_train.txt, Y\_train.txt, subject_train.txt from the "./train" folder and store them in *trainData*, *trainLabel* and *trainSubject* variables respectively.    
 2.Read X_test.txt, Y_test.txt and subject_test.txt from the "./test" folder and store in *testData*, *testLabel* and *testSubject* variables respectively.   
 3.Concatenate *testData* and *trainData* to generate a data frame called *joinData*; concatenate *testLabel* to *trainLabel* to generate a data frame called *joinLabel*, concetenate *testSubject* and *trainSubject* to generate a data frame called *joinSubject*   
 4.Read the feature.txt file from the "./" folder and store the data in a variable called *features*. We only extract the measurements on the mean and standard deviation. This results in a 66 indices list. We get a subset of *joinData* with the 66 corresponding columns.                                                                                                                                                                                                                                                                    
 5.Clean the column names of the subset. We remove the "()" and "-" symbols in the names, as well as make the first letter of "mean" and "std" a capital letter "M" and "S" respectively.
 6.Read the activity_labels.txt file from the "./data" folder and store the data in a variable called *activity*.
 7.Clean the activity names in the second column of *activity*. We first make all names to lower cases. If the name has an underscore between letters, we remove the underscore and capitalize the letter immediately after the underscore.
 8.Transform the values of *joinLabel* according to the *activity* data frame. 
 9.Combine the *joinSubject*, *joinLabel* and *joinData* by column to get a new cleaned data frame.
 10.Write the *jCleanedData* out to "merged_data.txt" file in current working directory.  
 11.Finally, generate a second independent tidy data set with the average of each measurement for each activity and each subject.we have 30 unique subjects and 6 unique activities, which result in a 180 combinations of the the tw. Then,for each combination, we calculate the mean of each measurement with the corresponding combination. So, after initializing the *result* data frame and performing the two for-loops, we get a 180��68 data frame.  
 12.Write the *result* out to "tidy_dataset.txt" file in current working directory. 
