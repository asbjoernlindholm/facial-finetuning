###############################################################################
## Replication script for tables for KRLS results
## Author: Asbjørn Lindholm
## Date: 29/09/2023
###############################################################################

rm(list = ls())

libs <- c("tidyverse","kableExtra", "KRLS", "fastDummies")

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

# load and preprocess data
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))

#In order to make the data from the two elections comparable, 
#we Z-score transform the dependent variables. 

#Keep only observations with images, not bad images and without text
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

# Create education dummies
df <- fastDummies::dummy_cols(df, select_columns = "education")

df <- df %>% rename("education_Primary_School" = "education_Primary School",
                    "education_High_School" = "education_High School",
                    "education_Phd" = "education_Ph.d")


df$election_type <- factor(df$election_type)

#Keep only observations with more than one candidate on the ballot
df2 <- df %>% filter(no_candidates_on_ballot != 1)

###############################################################################
## Part 1: Attractiveness - Ballot Paper Placement
###############################################################################

trait <- "Attractiveness"
trait2 <- "attractiveness_pd"
election <- "Pooled Data"
dv <- "bpp"

d <- df2  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("percentile_rank_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

# run krls - this takes a long time to run
out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Attractiveness.Ballot.Position.RData"))

# load krls results
load(paste0(result_path, "KRLS.Attractiveness.Ballot.Position.RData"))
res <- summary(out)

# data frame for results
ame <- c("attractiveness_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
rownames(data) <- c(trait, "Female", "Age", 
                      "Education: Primary School", "Education: High School", "Education: Vocational", 
                      "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))


###############################################################################
## Part 2: Attractiveness - Personal votes
###############################################################################

dv <- "personal_votes"

d <- df  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("personal_votes_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Attractiveness.Personal.Votes.RData"))


# load krls results
load(paste0(result_path, "KRLS.Attractiveness.Personal.Votes.RData"))
res <- summary(out)

# data frame for results
ame <- c("attractiveness_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
data <- data[, -1]

rownames(data) <- c(trait, "Female", "Age", 
                    "Education: Primary School", "Education: High School", "Education: Vocational", 
                    "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))



###############################################################################
## Part 3: Trustworthiness - Ballot Paper Placement
###############################################################################

trait <- "Trustworthiness"
trait2 <- "trustworthiness_pd"
election <- "Pooled Data"
dv <- "bpp"

d <- df2  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("percentile_rank_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

# run krls - this takes a long time to run
out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Trustworthiness.Ballot.Position.RData"))

# load krls results
load(paste0(result_path, "KRLS.Trustworthiness.Ballot.Position.RData"))
res <- summary(out)

# data frame for results
ame <- c("trustworthiness_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
data <- data[, -1]

# create table
rownames(data) <- c(trait, "Female", "Age", 
                    "Education: Primary School", "Education: High School", "Education: Vocational", 
                    "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))


###############################################################################
## Part 4: Trustworthiness - Personal votes
###############################################################################

dv <- "personal_votes"

d <- df  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("personal_votes_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Trustworthiness.Personal.Votes.RData"))


# load krls results
load(paste0(result_path, "KRLS.Trustworthiness.Personal.Votes.RData"))
res <- summary(out)

# data frame for results
ame <- c("trustworthiness_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
data <- data[, -1]

rownames(data) <- c(trait, "Female", "Age", 
                    "Education: Primary School", "Education: High School", "Education: Vocational", 
                    "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))



###############################################################################
## Part 4: Trustworthiness - Ballot Paper Placement
###############################################################################

trait <- "Dominance"
trait2 <- "dominance_pd"
election <- "Pooled Data"
dv <- "bpp"

d <- df2  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("percentile_rank_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

# run krls - this takes a long time to run
out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Dominance.Ballot.Position.RData"))

# load krls results
load(paste0(result_path, "KRLS.Dominance.Ballot.Position.RData"))
res <- summary(out)

# data frame for results
ame <- c("dominance_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
data <- data[, -1]

# create table
rownames(data) <- c(trait, "Female", "Age", 
                    "Education: Primary School", "Education: High School", "Education: Vocational", 
                    "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))


###############################################################################
## Part 6: Dominance - Personal votes
###############################################################################

dv <- "personal_votes"

d <- df  %>%
  mutate(gender = as.numeric(gender),
         election_type = ifelse(election_type == "Local election", 1, 0)) %>%
  mutate(election_type = as.numeric(as.character(election_type))) 

d <- d[, c("personal_votes_std", trait2, "gender", "age",
           "education_Primary_School", "education_High_School", "education_Vocational",
           "education_Seminary", "education_Undergraduate", "education_Graduate", "education_Phd")]
d <- d[complete.cases(d), ]

out <- krls(y = d[,1], X = d[,-1], derivative = TRUE, binary = TRUE, print.level = 0)
summary(out)

save(out, file=paste0(result_path, "KRLS.Dominance.Personal.Votes.RData"))


# load krls results
load(paste0(result_path, "KRLS.Dominance.Personal.Votes.RData"))
res <- summary(out)

# data frame for results
ame <- c("dominance_pd", "gender", "age", "education_Primary_School*", 
         "education_High_School*", "education_Vocational*", "education_Seminary*",
         "education_Undergraduate*", "education_Graduate*", "education_Phd*")

data <- data.frame(
  ame,
  Est = rep(NA, length(ame)),
  Std_error = rep(NA, length(ame)),
  t_value = rep(NA, length(ame)),
  Pr = rep(NA, length(ame))
)

# add summary results to data table
for(i in 1:10) {
  data[i,2] <- round(res$coefficients[i], 3)
  data[i,3] <- round(res$coefficients[i, "Std. Error"], 3)
  data[i,4] <- round(res$coefficients[i, "t value"], 3)
  data[i,5] <- round(res$coefficients[i, "Pr(>|t|)"], 3)
}

# create table
data <- data[, -1]

rownames(data) <- c(trait, "Female", "Age", 
                    "Education: Primary School", "Education: High School", "Education: Vocational", 
                    "Education: Seminary", "Education: Undergraduate", "Education: Graduate", "Education: Phd")


table <- data %>%
  kbl(format = "latex", booktabs = TRUE, align = "cccc",
      col.names = c("Estimate", "Std. error", "t-value", "P(>|t|)")) %>%
  kable_styling()

# fix problem with special latex characters
table <- gsub("P\\([^)]*\\|t\\|\\)", "P(\\\\textgreater{}\\\\textbar{}t\\\\textbar{})", table, perl = TRUE)

# write to .tex
writeLines(table, paste0(table_path, "table_krls_", trait, "_", dv, ".tex"))



#### End of script #### 
