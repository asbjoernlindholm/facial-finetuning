################################################################################
### Script for fine-tuning dominance model
################################################################################

# load packages
library(tidyverse)
library(keras)

# path to OMI data
data_path <- "/your_data_path/"

# path to OMI images
image_path <- "/your_image_path/"

# path to directory for saving models
model_path <- "/your_model_path/"

# set seed
set.seed(123)

### 1. Load and ready data for training_________________________________________

# load data
load(paste0(data_path, "OMI_training_data.RData"))

# split data in training, validation and test data
train_df <- omi_df %>% filter(image_folder == "train") 
val_df <- omi_df %>% filter(image_folder == "validation")
test_df <- omi_df %>% filter(image_folder == "test")

# ready data
train_generator <- 
  flow_images_from_dataframe(
    data = train_df,
    directory = paste0(image_path, "Train/"),
    x_col = "Filename",
    y_col = "dominance",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = TRUE,
    subset = "training"
  )

validation_generator <- 
  flow_images_from_dataframe(
    data = val_df,
    directory = paste0(image_path, "Validation"),
    x_col = "Filename",
    y_col = "dominance",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = TRUE
  )

test_generator <- 
  flow_images_from_dataframe(
    data = test_df,
    directory = paste0(image_path, "Test"),
    x_col = "Filename",
    y_col = "dominance",
    target_size = c(224, 224),
    batch_size = 16,
    class_mode = "other",
    shuffle = FALSE
  )

### 2. setup model______________________________________________________________

# get resnet50 model
resnet_base <- 
  application_resnet50(
    weights = "imagenet",
    include_top = FALSE,
    input_shape = c(224, 224, 3)
  )

# freeze weights
freeze_weights(resnet_base)

# define model
inputs <- layer_input(shape  = c(224, 224, 3))

data_augmentation <- 
  keras_model_sequential() %>%
  layer_random_flip("horizontal") %>%
  layer_random_rotation(0.1) %>%
  layer_random_zoom(0.2) %>%
  layer_random_brightness(0.1) %>%
  layer_random_contrast(0.2) 


outputs <- 
  inputs %>% 
  data_augmentation() %>%
  resnet_preprocess_input() %>%
  resnet_base() %>%
  layer_flatten() %>%
  layer_dense(4096, activation = "relu",
              kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dropout(0.5) %>%
  layer_dense(1, activation = "linear")  

model <- keras_model(inputs, outputs)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("mean_absolute_error", "mean_squared_error")
)

# define callbacks
callbacks <- list(
  callback_model_checkpoint(
    filepath = paste0(model_path, "Dominance_ResNet50.keras"),
    save_best_only = TRUE,
    monitor = "val_loss"
  ),
  callback_early_stopping(
    monitor = "val_loss",
    patience = 5
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.1,
    patience = 3,
    verbose = 0,
    mode = "auto",
    min_delta = 1e-04,
    cooldown = 0,
    min_lr = 0
  )
)


### 3. fit model and evaluate on test data______________________________________

# fit model
start_time <- Sys.time()

history <-  
  model %>% 
  fit(
    train_generator,
    epochs = 150,
    validation_data = validation_generator,
    callbacks = callbacks
  )

end_time <- Sys.time()

train_time <- difftime(end_time, start_time, units = c("hours"))

# load best saved model
model <- load_model_tf(paste0(model_path, "Dominance_ResNet50.keras"))

# test
result <- evaluate(model, test_generator)
cat(sprintf("Test mae: %.3f\n", result["mean_absolute_error"]))


################################################################################
## End of script
################################################################################