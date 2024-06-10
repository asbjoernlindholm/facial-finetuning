################################################################################
## Script for calculating model evaluation metrics 
################################################################################

# load packages
library(tidyverse)
library(keras)

# path to OMI training data 
data_path <- "/your_OMI_data_path/"

# path to OMI test images
image_path <- "/your_OMI_data_path/"

# path to models
model_path <- "/your_model_path/"

# path for saving tables with evaluation metrics
table_path <- "/your_table_path/"

# load data
load(paste0(data_path, "OMI_training_data.RData"))

# filter for test data
train_df <- omi_df %>% filter(image_folder == "train")
test_df <- omi_df %>% filter(image_folder == "test")


### 1. Evaluation metrics for attractiveness_____________________________________

# ready data for prediction
test_generator <- 
  flow_images_from_dataframe(
    data = test_df,
    directory = image_path,
    x_col = "Filename",
    y_col = "attractiveness",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = FALSE
  )

# load model
model <- load_model_tf(paste0(model_path, "Attractiveness_ResNet50.keras"))

# make predictions
predictions <-  model %>% predict(test_generator)

# add predictions to data and rename columns
df <- cbind(test_df, predictions[,1])
colnames(df)[ncol(df)] <- "predicted_attractiveness"
df <- df %>% rename(annotated_attractiveness = attractiveness)

# get training data stats
train_set <- c(
  nrow(train_df), 
  round(mean(train_df$attractiveness), 3), 
  round(sd(train_df$attractiveness), 3), 
  "", 
  ""
) %>% as.character()

# get test data stats
test_set <- c(
  nrow(test_df), 
  round(mean(test_df$attractiveness),3), 
  round(sd(test_df$attractiveness),3), 
  "", 
  ""
) %>% as.character()

# get model prediction stats
model_prediction <- c(
  nrow(df), 
  round(mean(df$predicted_attractiveness), 3), 
  round(sd(df$predicted_attractiveness), 3), 
  round(mean(abs(df$annotated_attractiveness - df$predicted_attractiveness)), 3),
  round(cor(df$annotated_attractiveness, df$predicted_attractiveness), 3)
) %>% as.character()

# get mean guessing stats
mean_guessing <- c(
  "", 
  "", 
  "", 
  round(mean(abs(df$attractiveness - mean(train_df$attractiveness))), 3), 
  "0.000"
) %>% as.character()

# get random guessing stats
set.seed(123)
random_mae <- numeric(1000)
random_pcc <- numeric(1000)
n <- nrow(df)

for (i in 1:1000) {
  random_guesses <- rnorm(n, mean = mean(train_df$attractiveness), sd = sd(train_df$attractiveness))  
  random_mae[i] <- mean(abs(random_guesses - df$annotated_attractiveness))  
  random_pcc[i] <- cor(df$annotated_attractiveness, random_guesses, method = "pearson")  
}

random_guessing <- c(
  "", 
  "", 
  "", 
  paste(round(mean(random_mae),3), "±", round(sd(random_mae),3)), 
  paste(round(mean(random_pcc),3), "±",  round(sd(random_pcc),3))
)

# define vector with row names 
rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

# gather stats in dataframe
data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# make note
note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the attractiveness score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean attractiveness score of the train dataset.")

# create and save table
stargazer(data,
          out = paste0(table_path, "Attractiveness_evaluation_metrics.tex"),
          header = FALSE,
          style = "apsr",
          type = "latex",
          summary = FALSE,
          rownames = TRUE,
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)


### 2. Evaluation metrics for trustworthiness___________________________________

# ready data for prediction
test_generator <- 
  flow_images_from_dataframe(
    data = test_df,
    directory = image_path,
    x_col = "Filename",
    y_col = "trustworthiness",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = FALSE
  )

# load model
model <- load_model_tf(paste0(model_path, "Trustworthiness_ResNet50.keras"))

# make predictions
predictions <-  model %>% predict(test_generator)

# add predictions to data and rename columns
df <- cbind(test_df, predictions[,1])
colnames(df)[ncol(df)] <- "predicted_trustworthiness"
df <- df %>% rename(annotated_trustworthiness = trustworthiness)

# get training data stats
train_set <- c(
  nrow(train_df), 
  round(mean(train_df$trustworthiness), 3), 
  round(sd(train_df$trustworthiness), 3), 
  "", 
  ""
) %>% as.character()

# get test data stats
test_set <- c(
  nrow(test_df), 
  round(mean(test_df$trustworthiness),3), 
  round(sd(test_df$trustworthiness),3), 
  "", 
  ""
) %>% as.character()

# get model prediction stats
model_prediction <- c(
  nrow(df), 
  round(mean(df$predicted_trustworthiness), 3), 
  round(sd(df$predicted_trustworthiness), 3), 
  round(mean(abs(df$annotated_trustworthiness - df$predicted_trustworthiness)), 3),
  round(cor(df$annotated_trustworthiness, df$predicted_trustworthiness), 3)
) %>% as.character()

# get mean guessing stats
mean_guessing <- c(
  "", 
  "", 
  "", 
  round(mean(abs(df$trustworthiness - mean(train_df$trustworthiness))), 3), 
  "0.000"
) %>% as.character()


# get random guessing stats
set.seed(123)
random_mae <- numeric(1000)
random_pcc <- numeric(1000)
n <- nrow(df)

for (i in 1:1000) {
  random_guesses <- rnorm(n, mean = mean(train_df$trustworthiness), sd = sd(train_df$trustworthiness))  
  random_mae[i] <- mean(abs(random_guesses - df$annotated_trustworthiness))  
  random_pcc[i] <- cor(df$annotated_trustworthiness, random_guesses, method = "pearson")  
}

random_guessing <- c(
  "", 
  "", 
  "", 
  paste(round(mean(random_mae),3), "±", round(sd(random_mae),3)), 
  paste(round(mean(random_pcc),3), "±",  round(sd(random_pcc),3))
)

# define vector with row names 
rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

# gather stats in dataframe
data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# make note
note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the trustworthiness score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean trustworthiness score of the train dataset.")

# create and save table
stargazer(data,
          out = paste0(table_path, "Trustworthiness_evaluation_metrics.tex"),
          header = FALSE,
          style = "apsr",
          type = "latex",
          summary = FALSE,
          rownames = TRUE,
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)



### 3. Evaluation metrics for dominance_________________________________________

# ready data for prediction
test_generator <- 
  flow_images_from_dataframe(
    data = test_df,
    directory = image_path,
    x_col = "Filename",
    y_col = "dominance",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = FALSE
  )

# load model
model <- load_model_tf(paste0(model_path, "Dominance_ResNet50.keras"))

# make predictions
predictions <-  model %>% predict(test_generator)

# add predictions to data and rename columns
df <- cbind(test_df, predictions[,1])
colnames(df)[ncol(df)] <- "predicted_dominance"
df <- df %>% rename(annotated_dominance = dominance)

# get training data stats
train_set <- c(
  nrow(train_df), 
  round(mean(train_df$dominance), 3), 
  round(sd(train_df$dominance), 3), 
  "", 
  ""
) %>% as.character()

# get test data stats
test_set <- c(
  nrow(test_df), 
  round(mean(test_df$dominance),3), 
  round(sd(test_df$dominance),3), 
  "", 
  ""
) %>% as.character()

# get model prediction stats
model_prediction <- c(
  nrow(df), 
  round(mean(df$predicted_dominance), 3), 
  round(sd(df$predicted_dominance), 3), 
  round(mean(abs(df$annotated_dominance - df$predicted_dominance)), 3),
  round(cor(df$annotated_dominance, df$predicted_dominance), 3)
) %>% as.character()

# get mean guessing stats
mean_guessing <- c(
  "", 
  "", 
  "", 
  round(mean(abs(df$dominance - mean(train_df$dominance))), 3), 
  "0.000"
) %>% as.character()


# get random guessing stats
set.seed(123)
random_mae <- numeric(1000)
random_pcc <- numeric(1000)
n <- nrow(df)

for (i in 1:1000) {
  random_guesses <- rnorm(n, mean = mean(train_df$dominance), sd = sd(train_df$dominance))  
  random_mae[i] <- mean(abs(random_guesses - df$annotated_dominance))  
  random_pcc[i] <- cor(df$annotated_dominance, random_guesses, method = "pearson")  
}

random_guessing <- c(
  "", 
  "", 
  "", 
  paste(round(mean(random_mae),3), "±", round(sd(random_mae),3)), 
  paste(round(mean(random_pcc),3), "±",  round(sd(random_pcc),3))
)

# define vector with row names 
rows <- c("Number of Images", "Mean", "Standard deviation", "Mean Absolute Error",
          "Pearson's correlation coefficient")

# gather stats in dataframe
data <- data.frame(rows, train_set, test_set, model_prediction, random_guessing, mean_guessing)

# Set the row names using the 'rows' vector
rownames(data) <- data$rows

# Remove the 'rows' column
data$rows <- NULL

# change columns names
colnames(data) <- c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing")

# make note
note <- paste("Random guessing is based on predictions drawn from a normal distribution with the mean and standard deviation of the dominance score in the training dataset, 1000 runs. Mean guessing is based on always guessing the mean dominance score of the train dataset.")

# create and save table
stargazer(data,
          out = paste0(table_path, "Dominance_evaluation_metrics.tex"),
          header = FALSE,
          style = "apsr",
          type = "latex",
          summary = FALSE,
          rownames = TRUE,
          column.labels = c("Train set", "Test set", "Model predictions", "Random guessing", "Mean guessing"),
          notes = paste("Note:", note),  
          notes.append = TRUE)


################################################################################
## End of script
################################################################################