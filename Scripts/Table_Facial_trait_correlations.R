###############################################################################
## Replication script for correlation between facial traits
## Author: Asbjørn Lindholm
## Date: 17/08/2023
##############################################################################

# clear memory
rm(list = ls())

# load liberaries
libs <- c("tidyverse", "stargazer", "corrplot")

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

# load data 
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

# clean data
traits <- df %>% 
  filter(image_missing == 0, image_bad == 0, image_text == 0) %>% 
  dplyr::select(attractiveness_pd, trustworthiness_pd, dominance_pd) 

# make data frame with correlations
data <- data.frame(round(cor(traits), 3))

# change columns and row names
colnames(data) <- c("Attractiveness", "Trustworthiness", "Dominance")
rownames(data) <- c("Attractiveness", "Trustworthiness", "Dominance")

# create table
stargazer(data,
          style = "apsr",
          type = "latex",
          out = paste0(table_path, "facial_trait_correlations", ".tex"),
          #type = "html",
          #out = paste0(table_path, "evaluation_metrics_", trait,".html"),
          summary = FALSE,
          rownames = TRUE)
          
          