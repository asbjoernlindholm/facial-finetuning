###############################################################################
## Replication script for Sensemakr plots
## Author: Asbjørn Lindholm
## Date: 16/10/2023
##############################################################################

# clear memory
rm(list = ls())

libs <- c("tidyverse", "sensemakr")

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


### Part 0: Load data and transform variables ---------------------------------

load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

# Keep only observations with images, not bad images and without text
df <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0)

# Recode general election dependent variables
fv22 <- df %>% filter(election == "FV22") %>% 
  mutate(percentile_rank_std = scale(percentile_rank),
         personal_votes_std = scale(log(personal_votes)))

# Recode local election dependent variables
kv21 <- df %>% filter(election == "KV21") %>%
  mutate(percentile_rank_std = scale(percentile_rank),
         personal_votes_std = scale(log(personal_votes))) 

# Recombine data
df <- rbind(fv22, kv21)

df$election_type <- factor(df$election_type)

# Keep only observations with more than one candidate on the ballot
df2 <- df %>% filter(no_candidates_on_ballot != 1)

###############################################################################
## Part 1: Sensemakr - Attractiveness
###############################################################################

trait <- "Attractiveness"
trait2 <- "attractiveness_pd"
election <- "Pooled Data"

# Ballot Paper Placement
model1b <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "+ age + gender + education"), data = df2)

ballot.sensitivity <- sensemakr(model = model1b, 
                                treatment = trait2,
                                benchmark_covariates = "gender1",
                                kd = 1:2) 

pdf(paste0(graph_path, "Sensemakr.", trait, ".bpp.pdf"), width = 10, height = 8) 

par(mfrow = c(1, 2))
par(mar = c(0, 3, 0, 3))

plot(ballot.sensitivity, cex.main = 1, main = "a) Point estimate")

plot(ballot.sensitivity, sensitivity.of = "t-value", main = "b) t-value", cex.main = 1)

dev.off() 

# Personal votes
model2b <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ age + gender + education"), data = df)

votes.sensitivity <- sensemakr(model = model2b, 
                               treatment = trait2,
                               benchmark_covariates = "gender1",
                               kd = 1:2) 


pdf(paste0(graph_path, "Sensemakr.", trait, ".personal.votes.pdf"), width = 10, height = 8) 

par(mfrow = c(1, 2))
par(mar = c(0, 3, 0, 3))

plot(votes.sensitivity, cex.main = 1, main = "a) Point estimate")

plot(votes.sensitivity, sensitivity.of = "t-value", main = "b) t-value", cex.main = 1)

dev.off() 


###############################################################################
## Part 2: Sensemakr - Trustworthiness
###############################################################################

trait <- "Trustworthiness"
trait2 <- "trustworthiness_pd"
election <- "Pooled Data"

# personal votes 
model2b <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ age + gender + education"), data = df)

pdf(paste0(graph_path, "Sensemakr.", trait, ".personal.votes.pdf"), width = 10, height = 8) 

par(mfrow = c(1, 2))
par(mar = c(0, 3, 0, 3))

plot(votes.sensitivity, cex.main = 1, main = "a) Point estimate")

plot(votes.sensitivity, sensitivity.of = "t-value", main = "b) t-value", cex.main = 1)

dev.off() 



###############################################################################
## Part 2: Sensemakr - Dominance
###############################################################################

trait <- "Dominance"
trait2 <- "dominance_pd"
election <- "Pooled Data"


# Personal votes
model2b <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ age + gender + education"), data = df)

votes.sensitivity <- sensemakr(model = model2b, 
                               treatment = trait2,
                               benchmark_covariates = "gender1",
                               kd = 1:1) 


pdf(paste0(graph_path, "Sensemakr.", trait, ".personal.votes.pdf"), width = 10, height = 8) 

par(mfrow = c(1, 2))
par(mar = c(0, 3, 0, 3))

plot(votes.sensitivity, cex.main = 1, main = "a) Point estimate")

plot(votes.sensitivity, sensitivity.of = "t-value", main = "b) t-value", cex.main = 1)

dev.off() 

