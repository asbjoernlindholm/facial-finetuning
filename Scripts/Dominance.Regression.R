###############################################################################
## Replication script for tables using Dominance as predictor
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

# laod data
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

# In order to make the data from the two elections comparable, 
# we Z-score transform the dependent variables. 

# Keep only observations with images, not bad images and without text
df <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0)

fv22 <- df %>% filter(election == "FV22") %>% 
  mutate(percentile_rank_std = scale(percentile_rank),
         personal_votes_std = scale(log(personal_votes)),
         relative_success_std = scale(log(relative_success)))


kv21 <- df %>% filter(election == "KV21") %>%
  mutate(percentile_rank_std = scale(percentile_rank),
         personal_votes_std = scale(log(personal_votes)),
         relative_success_std = scale(log(relative_success)))


df <- rbind(fv22, kv21)

df$election_type <- factor(df$election_type)

# Keep only observations with more than one candidate on the ballot
df2 <- df %>% filter(no_candidates_on_ballot != 1)

# define names
trait <- "Dominance"
trait2 <- "dominance_pd"
election <- "Pooled data"

# Define clustered standard error function
se_robust <- function(x) {
  
  coeftest(x, vcovCL = df2$name2)[, "Std. Error"]
}


###############################################################################
## Main Results
###############################################################################

###### Ballot percentile Rank ######

# Ballot percentile rank - bivariate
model1a <- lm(formula = paste0("percentile_rank_std ~ ", trait2), data = df2)

# Ballot percentile rank + controls 
model1b <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "+ age + gender + education"), data = df2)

# Ballot percentile rank + gender interaction
model1c <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*party_ideology"), data = df2)

# Ballot percentile rank + gender interaction + controls
model1d <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*party_ideology + age + gender + education"), data = df2)

# Ballot percentile rank + election interaction
model1e <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*election_type"), data = df2)

# Ballot percentile rank + election interaction + controls
model1f <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*election_type + age + gender + education"), data = df2)


###### Log(personal votes) std ######

# Log(personal votes) std - bivariate
model2a <- lm(formula = paste0("personal_votes_std ~ ", trait2), data = df)

# Log(personal votes) std + controls 
model2b <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ age + gender + education"), data = df)

# Log(personal votes) std + gender interaction
model2c <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*party_ideology"), data = df)

# Log(personal votes) std + gender interaction + controls
model2d <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*party_ideology + age + gender + education"), data = df)

# Log(personal votes) std + election interaction
model2e <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*election_type"), data = df)

# Log(personal votes) std + election interaction + controls
model2f <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*election_type + age + gender + education"), data = df)

# List models
models <- list(model1a, model1b, model1c, model1d, model1e, model1f, 
               model2a, model2b, model2c, model2d, model2e, model2f)


stargazer(models,
          type = "latex",
          style = "apsr",
          out = paste0(table_path, "Main.Results.", trait,".", election, ".tex"),
          dep.var.labels = c("Ballot Paper Placement", "Personal Votes"),
          order = c("dominance_pd", 
                    "dominance_pd:party_ideology",
                    "dominance_pd:election_typeLocal election",
                    "election_typeLocal election",
                    "party_ideology",
                    "gender", "age"),
          covariate.labels = c(trait, 
                               paste(trait, "x Right Wing Party"),
                               paste(trait, "x Local Election"),
                               "Local Election", "Right Wing Party", "Female", "Age"), 
          se = lapply(models, se_robust),
          no.space = FALSE,
          single.row = FALSE,
          df = FALSE,
          initial.zero = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align=FALSE,
          model.numbers = TRUE,
          omit.stat = c("ser"),
          omit = c("education"),
          add.lines = list(c("Education Control", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")),
          notes = c("Note: Entries are unstandardized coefficients from linear models with clustered standard errors. *p \\textless{} .05; **p \\textless{} .01; ***p \\textless{} .001."),
          notes.append = FALSE)


###############################################################################
## Results with incumbency as control 
###############################################################################

###### Ballot percentile Rank ######

# Ballot percentile rank - bivariate
model1a <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "+ incumbent"), data = df2)

# Ballot percentile rank + controls 
model1b <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "+ incumbent + age + gender + education"), data = df2)

# Ballot percentile rank + gender interaction
model1c <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*party_ideology + incumbent"), data = df2)



# Ballot percentile rank + gender interaction + controls
model1d <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*party_ideology + incumbent + age + gender + education"), data = df2)


# Ballot percentile rank + election interaction
model1e <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*election_type + incumbent"), data = df2)


# Ballot percentile rank + election interaction + controls
model1f <- lm(formula = paste0("percentile_rank_std ~ ", trait2, "*election_type + incumbent + age + gender + education"), data = df2)


###### Log(personal votes) std ######

# Log(personal votes) std - bivariate
model2a <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ incumbent"), data = df)

# Log(personal votes) std + controls 
model2b <- lm(formula = paste0("personal_votes_std ~ ", trait2, "+ incumbent + age + gender + education"), data = df)

# Log(personal votes) std + gender interaction
model2c <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*party_ideology + incumbent"), data = df)

# Log(personal votes) std + gender interaction + controls
model2d <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*party_ideology + incumbent + age + gender + education"), data = df)

# Log(personal votes) std + election interaction
model2e <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*election_type + incumbent"), data = df)

# Log(personal votes) std + election interaction + controls
model2f <- lm(formula = paste0("personal_votes_std ~ ", trait2, "*election_type + incumbent + age + gender + education"), data = df)


# List models
models <- list(model1a, model1b, model1c, model1d, model1e, model1f, 
               model2a, model2b, model2c, model2d, model2e, model2f)



stargazer(models,
          type = "latex",
          style = "apsr",
          out = paste0(table_path, "Incumbency.", trait,".", election, ".model.results.tex"),
          dep.var.labels = c("Ballot Paper Placement", "Personal Votes"),
          order = c("dominance_pd", 
                    "dominance_pd:party_ideology",
                    "dominance_pd:election_typeLocal election",
                    "election_typeLocal election",
                    "party_ideology", "incumbent1",
                    "gender", "age"),
          covariate.labels = c(trait, 
                               paste(trait, "x Right Wing Party"),
                               paste(trait, "x Local Election"),
                               "Local Election", "Right Wing Party", "Incumbent", "Female", "Age"), 
          se = lapply(models, se_robust),
          no.space = FALSE,
          single.row = FALSE,
          df = FALSE,
          initial.zero = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align=FALSE,
          model.numbers = TRUE,
          omit.stat = c("ser"),
          omit = c("education"),
          add.lines = list(c("Education Control", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")),
          notes = c("Note: Entries are unstandardized coefficients from linear models with clustered standard errors. *p \\textless{} .05; **p \\textless{} .01; ***p \\textless{} .001."),
          notes.append = FALSE)


