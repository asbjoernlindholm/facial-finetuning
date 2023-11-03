###############################################################################
## Replication script for table X.
## Regression models with all facial  traits in the same model
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

# specify models with ballot paper placement as DV
model_bpp1 <- lm(percentile_rank_std ~ attractiveness_pd + trustworthiness_pd + dominance_pd, data = df2)
model_bpp2 <- lm(percentile_rank_std ~ attractiveness_pd + trustworthiness_pd + dominance_pd + age + gender + education, data = df2)

# specify models with ballot paper placement as DV
model_pv1 <- lm(personal_votes_std ~ attractiveness_pd + trustworthiness_pd + dominance_pd, data = df)
model_pv2 <- lm(personal_votes_std ~ attractiveness_pd + trustworthiness_pd + dominance_pd + age + gender + education, data = df)

### Part 3: Create stargazer table ---------------------------------------------

# Define clustered standard error function
se_robust <- function(x) {
  coeftest(x, vcovCL = df2$name2)[, "Std. Error"]
}

# list models
models <- list(model_bpp1, model_bpp2, model_pv1, model_pv2)

# stargazer table

stargazer(models,
          type = "latex",
          style = "apsr",
          out = paste0(table_path, "Table_All_Facial_traits_in_model.tex"),
          dep.var.labels = c("Ballot Paper Placement", "Personal Votes"),
          covariate.labels = c("Attractiveness","Trustworthiness", "Dominance",
                               "Age", "Female"),
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
          add.lines = list(c("Education Control", "No", "Yes", "No", "Yes")))#,
          #notes = c("Note: Entries are unstandardized coefficients from linear models with clustered standard errors.", 
          #          "*p \\textless{} .05; **p \\textless{} .01; ***p \\textless{} .001."),
          #notes.append = FALSE)
          
