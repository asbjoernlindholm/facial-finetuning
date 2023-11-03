###############################################################################
## Replication script for evaluation metrics 
## Author: Asbjørn Lindholm
## Date: 17/08/2023
##############################################################################

# clear memory
rm(list = ls())

libs <- c("tidyverse", "stargazer")

# load liberaries
installed_libs <- libs %in% rownames(installed.packages())
if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

# set paths to directories
data_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Data/"
graph_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Graphs/"
result_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Results/"
table_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Tables/"

### Part 1: Create table for attractiveness ---------------------------------

trait <- "attractiveness"

note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the", trait, "score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean", trait, "score of the train dataset.")

rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

train_set <- c("513", "5.235", "1.266", "", "")
test_set <- c("172", "5.395", "1.409", "", "")

model_prediction <- c("172", "5.520", "1.237", "0.732", "0.787")
random_guessing <- c("", "", "", "1.516 ± 0.078", "0.002 ± 0.076")
mean_guessing <- c("", "", "", "1.145", "0.000")

data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# create table
stargazer(data,
          style = "apsr",
          type = "latex",
          out = paste0(table_path, "evaluation_metrics_", trait,".tex"),
          #type = "html",
          #out = paste0(table_path, "evaluation_metrics_", trait,".html"),
          summary = FALSE,
          rownames = TRUE,
          
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)



### Part 2: Create table for trustworthiness ---------------------------------

trait <- "trustworthiness"

note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the", trait, "score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean", trait, "score of the train dataset.")

rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

train_set <- c("513", "5.653", "1.087", "", "")
test_set <- c("172", "5.654", "1.079", "", "")

model_prediction <- c("172", "5.570", "0.867", "0.576", "0.754")
random_guessing <- c("", "", "", "1.230 ± 0.063", "0.000 ± 0.075") 
mean_guessing <- c("", "", "", "0.889", "0.000")

data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# create table
stargazer(data,
          style = "apsr",
          type = "latex",
          out = paste0(table_path, "evaluation_metrics_", trait,".tex"),
          #type = "html",
          #out = paste0(table_path, "evaluation_metrics_", trait,".html"),
          summary = FALSE,
          rownames = TRUE,
          
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)





### Part 3: Create table for dominance ---------------------------------

trait <- "dominance"

note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the", trait, "score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean", trait, "score of the train dataset.")

rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

train_set <- c("513", "5.071", "1.121", "", "")
test_set <- c("172", "4.972", "1.154", "", "")

model_prediction <- c("172", "5.119", "0.933", "0.602", "0.756")
random_guessing <- c("", "", "", "1.298 ± 0.065", "-0.001 ± 0.075")
mean_guessing <- c("", "", "", "0.947", "0.000")

data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# create table
stargazer(data,
          style = "apsr",
          type = "latex",
          out = paste0(table_path, "evaluation_metrics_", trait,".tex"),
          #type = "html",
          #out = paste0(table_path, "evaluation_metrics_", trait,".html"),
          summary = FALSE,
          rownames = TRUE,
          
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)
         
         

###############################################################################
## End of script
##############################################################################