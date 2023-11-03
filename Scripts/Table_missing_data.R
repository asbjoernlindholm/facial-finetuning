###############################################################################
## Replication script for table with summary of collected data
## Author: Asbjørn Lindholm
## Date: 29/09/2023
###############################################################################


rm(list = ls())

libs <- c("tidyverse", "stargazer", "kableExtra")

installed_libs <- libs %in% rownames(installed.packages())
if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

# load liberaries
invisible(lapply(libs, library, character.only = T))

# set paths to directories
data_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Data/"
graph_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Graphs/"
result_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Results/"
table_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Tables/"

###############################################################################
## Part 1: Data summary
###############################################################################

# load data
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

# create data frame 
Election = c("General", "Local", "Pooled", "Pooled (%)", "Personal votes accounted for by observations (%)")

data <- data.frame(
  Election,
  Major_party_candidates = rep(NA, length(Election)),
  Images_collected = rep(NA, length(Election)),
  Images_missing = rep(NA, length(Election)),
  Images_discarded = rep(NA, length(Election)),
  Age = rep(NA, length(Election)),
  Gender = rep(NA, length(Election)),
  Education = rep(NA, length(Election)),
  Complete_data = rep(NA, length(Election))
)


# general election
data[1,2] <- df %>% filter(election == "FV22") %>% nrow() # major party candidates
data[1,3] <- df %>% filter(election == "FV22", image_missing == 0) %>% nrow() # non-missing images 
data[1,4] <- df %>% filter(election == "FV22", image_missing == 1) %>% nrow() # missing images
data[1,5] <- df %>% filter(election == "FV22", image_text == 1) %>% nrow() + df %>% filter(election == "FV22", image_bad == 1) %>% nrow() # images discarded
data[1,6] <- df %>% filter(election == "FV22", !is.na(age)) %>% nrow() # candidates with age information
data[1,7] <- df %>% filter(election == "FV22", !is.na(gender)) %>% nrow() # candidates with gender information
data[1,8] <- df %>% filter(election == "FV22", !is.na(education)) %>% nrow() # candidates with education information
data[1,9] <- df %>% filter(election == "FV22", image_missing == 0, image_bad == 0, image_text == 0) %>%  # complete data
  dplyr::select(personal_votes, ballot_placement, age, gender, education) %>% filter(complete.cases(.)) %>% nrow() 


# local election
data[2,2] <- df %>% filter(election == "KV21") %>% nrow()
data[2,3] <- df %>% filter(election == "KV21", image_missing == 0) %>% nrow()
data[2,4] <- df %>% filter(election == "KV21", image_missing == 1) %>% nrow()
data[2,5] <- df %>% filter(election == "KV21", image_missing == 0, image_text == 1) %>% nrow() + df %>% filter(election == "KV21", image_missing == 0, image_bad == 1) %>% nrow()
data[2,6] <- df %>% filter(election == "KV21", !is.na(age)) %>% nrow()
data[2,7] <- df %>% filter(election == "KV21", !is.na(gender)) %>% nrow()
data[2,8] <- df %>% filter(election == "KV21", !is.na(education)) %>% nrow()
data[2,9] <- df %>% filter(election == "KV21", image_missing == 0, image_bad == 0, image_text == 0) %>% 
  dplyr::select(personal_votes, ballot_placement, age, gender, education) %>% filter(complete.cases(.)) %>% nrow() 


# Pooled election data
data[3,2] <- df %>% nrow()
data[3,3] <- df %>% filter(image_missing == 0) %>% nrow()
data[3,4] <- df %>% filter(image_missing == 1) %>% nrow()
data[3,5] <- df %>% filter(image_missing == 0, image_text == 1) %>% nrow() + df %>% filter(image_missing == 0, image_bad == 1) %>% nrow()
data[3,6] <- df %>% filter(!is.na(age)) %>% nrow()
data[3,7] <- df %>% filter(!is.na(gender)) %>% nrow()
data[3,8] <- df %>% filter(!is.na(education)) %>% nrow()
data[3,9] <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0) %>% 
  dplyr::select(personal_votes, ballot_placement, age, gender, education) %>% filter(complete.cases(.)) %>% nrow() 

# pooled data (%)
data[,2] <- as.numeric(data[,2])
data[,3] <- as.numeric(data[,3])
data[,4] <- as.numeric(data[,4])
data[,5] <- as.numeric(data[,5])
data[,6] <- as.numeric(data[,6])
data[,7] <- as.numeric(data[,7])
data[,8] <- as.numeric(data[,8])
data[,9] <- as.numeric(data[,9])


data[4,2] <- round(data[3,2]/data[3,2]*100,2)
data[4,3] <- round(data[3,3]/data[3,2]*100,2)
data[4,4] <- round(data[3,4]/data[3,2]*100,2)
data[4,5] <- round(data[3,5]/data[3,2]*100,2)
data[4,6] <- round(data[3,6]/data[3,2]*100,2)
data[4,7] <- round(data[3,7]/data[3,2]*100,2)
data[4,8] <- round(data[3,8]/data[3,2]*100,2)
data[4,9] <- round(data[3,9]/data[3,2]*100,2)


# calculate percentage of personal votes accounted for by observations

df$images_disc <- ifelse(df$image_text==1|df$image_bad==1,1,0)

# Pooled
data[5,2] <- 100

dat <- df %>% filter(image_missing == 0) 
data[5,3] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)

# image missing 
dat <- df %>% filter(image_missing == 1)
data[5,4] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)

# image text
dat1 <- df %>% filter(image_text == 1)
dat2 <- df %>% filter(image_bad == 1)
data[5,5] <- round((sum(dat1$personal_votes) + sum(dat2$personal_votes))/sum(df$personal_votes) * 100, 2)

# age
dat <- df %>% filter(!is.na(age))
data[5,6] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)

dat <- df %>% filter(!is.na(gender))
data[5,7] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)

dat <- df %>% filter(!is.na(education))
data[5,8] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)

dat <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0) %>% 
  dplyr::select(personal_votes, ballot_placement, age, gender, education) %>% filter(complete.cases(.))
data[5,9] <- round(sum(dat$personal_votes)/sum(df$personal_votes) * 100, 2)


###############################################################################
## Part 2: Create table
###############################################################################

table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "lcccccccc",
      col.names = c("Election", "Major party candidates", "Images collected", "Images missing", "Images discarded", "Age", "Gender", "Education", "Complete data")) %>%
  kable_classic() %>%
  footnote(general = "The first three rows indicate the number of observations in absolute numbers. 
           Fourth row indicate the number of observations in percent in relation to the pooled number of major party candidates. 
           The fifth row indicates the percentage of personal votes accounted for by the observations in each column in relation to the personal votes accounted for by the major party candidates")

writeLines(table, paste0(table_path, "summary.missing.data.tex"))


