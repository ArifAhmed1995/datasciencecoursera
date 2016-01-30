##README

* Extract all required data into data frames.

* Merge the training set and test data by using the 'rbind'
  and 'cbind' operations in R. 

* Set the column names of the dataset to be reasonable, using 
  colnames function in R.

* Extract required columns containing the keywords 'mean'
  and 'std'.

* Set the numbers in the column 'State' to the key 
  values(i.e.Sitting , Lying, etc)

* Group the dataset by 'Id' and 'State' , and summarize the rest
  the columns by taking their average.This is done by using functions
  such as group_by and summarise_each of the dplyr package in R.

* Write data to CSV file.
