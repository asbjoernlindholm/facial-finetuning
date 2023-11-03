###############################################################################
## Replication script for table X.
## Correlation between age, gender, education and facial traits as outcomes
## Author: Asbjørn Lindholm
## Date: 11/08/2023
##############################################################################

# clear memory
rm(list = ls())

libs <- c("tidyverse", "stargazer", "lmtest", "sandwich")

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

### Part 1: Load data and transform variables ---------------------------------

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


### Part 2: Specify models ----------------------------------------------------

attr_model_1 <- lm(attractiveness_pd ~ age, data = df)
attr_model_2 <- lm(attractiveness_pd ~ gender, data = df)
attr_model_3 <- lm(attractiveness_pd ~ education, data = df)

trust_model_1 <- lm(trustworthiness_pd ~ age, data = df)
trust_model_2 <- lm(trustworthiness_pd ~ gender, data = df)
trust_model_3 <- lm(trustworthiness_pd ~ education, data = df)

dom_model_1 <- lm(dominance_pd ~ age, data = df)
dom_model_2 <- lm(dominance_pd ~ gender, data = df)
dom_model_3 <- lm(dominance_pd ~ education, data = df)

### Part 3: Create stargazer table ---------------------------------------------

# Define clustered standard error function
se_robust <- function(x) {
  coeftest(x, vcovCL = df$name2)[, "Std. Error"]
}

# list models
models <- list(attr_model_1, attr_model_2, attr_model_3,
               trust_model_1, trust_model_2, trust_model_3,
               dom_model_1, dom_model_2, dom_model_3)

# stargazer table
stargazer(models,
          type = "latex",
          style = "apsr",
          out = paste0(table_path, "Table_Facial_traits_as_outcome.tex"),
          
          dep.var.labels = c("Attractiveness", "Trustworthiness", "Dominance"),
          covariate.labels = c("Age", "Female", "Education: High School",
                               "Education: Vocational", "Education: Seminary", "Education: Undergraduate",
                               "Education: Graduate", "Education: Ph.d"),
          se = lapply(models, se_robust),
          no.space = FALSE,
          single.row = FALSE,
          df = FALSE,
          initial.zero = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align=FALSE,
          model.numbers = TRUE,
          omit.stat = c("ser"),
          notes = c("Note: Entries are unstandardized coefficients from linear models with clustered standard errors. *p \\textless{} .05; **p \\textless{} .01; ***p \\textless{} .001."),
          notes.append = FALSE)
          
