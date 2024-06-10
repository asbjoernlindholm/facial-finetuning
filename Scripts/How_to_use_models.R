################################################################################
### This script shows how to use the models
################################################################################

# packages
libs <- c("tidyverse", "keras")

installed_libs <- libs %in% rownames(installed.packages())
if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

# load liberaries
invisible(lapply(libs, library, character.only = T))

# path to images
image_path <- "/your_image_path"

# path to models 
model_path <- "/your_model_path/"

# make data frame with path to images
df <- list.files(image_path, full.names = TRUE) %>% 
  data.frame() %>% rename("path_to_image" = ".")

# ready data for prediction
data_generator <- 
  flow_images_from_dataframe(
    data = df,
    directory = image_path,
    x_col = "path_to_image",
    y_col = NULL,
    target_size = c(224, 224),
    batch_size = 1,
    class_mode = NULL,
    shuffle = FALSE
  )

# load model
model <- load_model_tf(paste0(model_path, "Attractiveness_ResNet50.keras"))

# make predictions
predictions <-  model %>% predict(data_generator)

# add predictions to data
df <- cbind(df, predictions[,1])






