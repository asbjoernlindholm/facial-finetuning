################################################################################
## Script for downloading and wrangling OMI data and images
################################################################################

rm(list = ls())

library(tidyverse)

### 1. Create directories for data and images___________________________________

# path to your directory
directory_path <- "your_directory_path/"


# subdirectories to create
subdirectories <- c("OMI_Data", "OMI_Images", "Data", "Images")

# create directories if they don't exist
invisible(lapply(subdirectories, function(subdir) {
  dir_path <- file.path(directory_path, subdir)
  if (!dir.exists(dir_path)) {
    print(paste("Creating directory for", subdir))
    dir.create(dir_path, recursive = TRUE)
  } else {
    print(paste("Directory already exists for", subdir))
  }
}))

# make paths 
omi_data_path <- paste0(directory_path, subdirectories[1], "/")
omi_image_path <- paste0(directory_path, subdirectories[2], "/")
data_path <- paste0(directory_path, subdirectories[3], "/")  
image_path <- paste0(directory_path, subdirectories[4], "/")



### 1. Download and wrangle OMI data____________________________________________

# url to OMI data
file_url <- "https://raw.githubusercontent.com/jcpeterson/omi/main/attribute_means.csv"

# download file
if(!file.exists(paste0(omi_data_path, "attribute_means.csv"))) {
  download.file(file_url, paste0(omi_data_path, "attribute_means.csv"))
} else {
  print("OMI data already downloaded")
}

# load OMI data
raw_data <- read.csv(paste0(omi_data_path, "attribute_means.csv"))

# images of children
images_to_remove <- 
  c("8", "12", "33", "37", "47", "53", "55", "57", "59", "60", "61", "62", "73",
    "81", "89", "98", "110", "114", "120", "121",  "127", "129", "136", "152", 
    "157", "164", "172", "174", "194", "214", "218", "226", "231", "232", "235", 
    "243", "252", "254", "257", "261", "262", "263", "264", "284", "285", "286", 
    "298", "317", "319", "320", "327", "329", "336", "343", "344", "353", "357", 
    "359", "397", "398", "406", "418", "419", "425", "429", "430", "431", "440", 
    "442", "449", "453", "469", "470", "473", "489", "493", "506", "532", "542", 
    "547", "552", "567", "576", "578", "612", "613", "619", "620", "628", "629", 
    "632", "634", "639", "650", "660", "668", "669", "673", "675", "687", "690", 
    "693", "700", "703", "705", "718", "721", "722", "724", "725", "732", "735", 
    "744", "747", "760", "761", "769", "786", "789", "794", "795", "799", "817", 
    "822", "844", "845", "858", "871", "873", "876", "882", "890", "894", "896", 
    "919", "923", "929", "941", "948", "951", "968", "971", "990", "993", "994", 
    "997", "1001", "1004") 


# data wrangling
omi_df <- raw_data %>% 
  filter(!stimulus %in% images_to_remove) %>%
  mutate(
    Filename = paste0(stimulus, ".jpg"),
    attractiveness = attractive/10,
    dominance = dominant/10,
    trustworthiness = trustworthy/10
  ) %>% 
  select(Filename, attractiveness, trustworthiness, dominance, gender, age)
  


### 2. Download and divide images into subdirectories___________________________

# create image urls
base.url <- "https://raw.githubusercontent.com/jcpeterson/omi/main/images/"
image_urls <- paste0(base.url, 1:1004, ".jpg")

# download images
for (url in image_urls) {
  file_name <- basename(url)
  destfile <- paste0(omi_image_path, file_name)
  if(!file.exists(destfile)){
    download.file(url, destfile)
  } else {
    print(paste(file_name, "already downloaded"))
  }
}


# create subdirectories within image_path
dir.create(paste0(image_path,"Train"))
dir.create(paste0(image_path,"Validation"))
dir.create(paste0(image_path,"Test"))


# set seed and draw random sample
set.seed(123)
omi_df$sample <- sample(nrow(omi_df), nrow(omi_df), replace = FALSE)

# loop through df and copy images to directory
for (i in 1:nrow(omi_df)) {
  print(paste("working on row", i, "out of", nrow(omi_df)))
  
  # specify folder
  if(omi_df$sample[i] <= floor(0.6*nrow(omi_df))) {folder <- "Train"}
  if(omi_df$sample[i] > floor(0.6*nrow(omi_df)) & omi_df$sample[i] <= floor(0.80*nrow(omi_df))) {folder <- "Validation"}
  if(omi_df$sample[i] > floor(0.80*nrow(omi_df))) {folder <- "Test"}
  
  omi_df$image_folder[i] <- tolower(folder)
  
  # specify source and destination image
  src <- paste0(omi_image_path, omi_df$Filename[i])
  dest <- paste0(image_path, folder, "/", omi_df$Filename[i])
  
  # copy image to folder
  file.copy(src, dest, overwrite = TRUE)
}

# save data
save(omi_df, file = paste0(data_path, "OMI_training_data.RData"))
