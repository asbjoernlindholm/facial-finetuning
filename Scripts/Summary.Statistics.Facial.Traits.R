###############################################################################
## Replication script for table with summary statistics for facial traits
## Author: Asbjørn Lindholm
## Date: 17/08/2023
###############################################################################

# clear memory
rm(list = ls())

# load libraries
libs <- c("tidyverse", "stargazer", "sensemakr", "KRLS", "AER","sandwich")

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


# load data
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

# Keep only observations with images, not bad images and without text
df <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0) %>%
  select(attractiveness_pd, trustworthiness_pd, dominance_pd)


stargazer::stargazer(df, 
                     summary=TRUE,
                     type = "latex", 
                     digits=1,
                     style = "apsr",
                     out= paste0(table_path, "summary.stats.facial.traits.tex"),
                     summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
                     covariate.labels = c("Attractiveness", "Trustworthiness", "Dominance"))
                     #summary.labels = c("Facial Trait", "Obs.", "Mean", "St. Dev.", "Min", "Q1", "Median", "Q3", "Max"))
