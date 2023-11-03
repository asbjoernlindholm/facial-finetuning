################################################################################
## Replication script for main effects plot
## Author: Asbjørn Lindholm
## Date: 10/08/2023
################################################################################

# Clear memory
rm(list = ls())

# Load libraries
libs <- c("tidyverse", "ggplot2", "sandwich", "lmtest")

installed_libs <- libs %in% rownames(installed.packages())
if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

# set paths to directories
data_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Data/"
graph_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Graphs/"
result_path <- "/Users/AsbjoernLindholm/Documents/AU/Artikel/Replication/Results/"


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

# define independent variable names
trait_1 <- "attractiveness_pd"
trait_2 <- "trustworthiness_pd"
trait_3 <- "dominance_pd"



# specify models with ballot paper placement as DV
attr_model_bpp_1 <- lm(formula = paste0("percentile_rank_std ~ ", trait_1), data = df2)
attr_model_bpp_2 <- lm(formula = paste0("percentile_rank_std ~ ", trait_1, "+ age + gender + education"), data = df2)

trust_model_bpp_1 <- lm(formula = paste0("percentile_rank_std ~ ", trait_2), data = df2)
trust_model_bpp_2 <- lm(formula = paste0("percentile_rank_std ~ ", trait_2, "+ age + gender + education"), data = df2)

dom_model_bpp_1 <- lm(formula = paste0("percentile_rank_std ~ ", trait_3), data = df2)
dom_model_bpp_2 <- lm(formula = paste0("percentile_rank_std ~ ", trait_3, "+ age + gender + education"), data = df2)

# specify models with personal votes as DV
attr_model_pv_1 <- lm(formula = paste0("personal_votes_std ~ ", trait_1), data = df)
attr_model_pv_2 <- lm(formula = paste0("personal_votes_std ~ ", trait_1, "+ age + gender + education"), data = df)

trust_model_pv_1 <- lm(formula = paste0("personal_votes_std ~ ", trait_2), data = df)
trust_model_pv_2 <- lm(formula = paste0("personal_votes_std ~ ", trait_2, "+ age + gender + education"), data = df)

dom_model_pv_1 <- lm(formula = paste0("personal_votes_std ~ ", trait_3), data = df)
dom_model_pv_2 <- lm(formula = paste0("personal_votes_std ~ ", trait_3, "+ age + gender + education"), data = df)

### Part 3: Plot effects ------------------------------------------------------

# extract coefficients
coefs <- c(
           coef(attr_model_bpp_1)[trait_1], # coefs for attractiveness
           coef(attr_model_bpp_2)[trait_1],
           coef(attr_model_pv_1)[trait_1],
           coef(attr_model_pv_2)[trait_1],
           coef(trust_model_bpp_1)[trait_2], # coefs for trustworthiness
           coef(trust_model_bpp_2)[trait_2],
           coef(trust_model_pv_1)[trait_2],
           coef(trust_model_pv_2)[trait_2],
           coef(dom_model_bpp_1)[trait_3], # coefs for dominance
           coef(dom_model_bpp_2)[trait_3],
           coef(dom_model_pv_1)[trait_3],
           coef(dom_model_pv_2)[trait_3])

# define clustered standard error function
get_standard_error <- function(model, df_name) {
  result <- data.frame(coeftest(model, vcovCL = df2$name2)[, "Std. Error"])
  se <- result[2,1]
  return(se)
}

# get clustered standard errors
se_coefs <- c(
  get_standard_error(attr_model_bpp_1, df2$name2),
  get_standard_error(attr_model_bpp_2, df2$name2),
  get_standard_error(attr_model_pv_1, df$name2),
  get_standard_error(attr_model_pv_2, df$name2),
  get_standard_error(trust_model_bpp_1, df2$name2),
  get_standard_error(trust_model_bpp_2, df2$name2),
  get_standard_error(trust_model_pv_1, df$name2),
  get_standard_error(trust_model_pv_2, df$name2),
  get_standard_error(dom_model_bpp_1, df2$name2),
  get_standard_error(dom_model_bpp_2, df2$name2),
  get_standard_error(dom_model_pv_1, df$name2),
  get_standard_error(dom_model_pv_2, df$name2)
)

# get names for Independent and dependent variables
IVs <- c(rep("Attractiveness", 4), rep("Trustworthiness", 4), rep("Dominance", 4))
DVs <- rep(c(rep("Ballot Paper Placement", 2), rep("Personal Votes", 2)), 3)
control <- rep(c("Without control", "With control"), 6)

# combine to dataframe
plotdf <- data.frame(coefs, se_coefs, IVs, DVs, control)

# calculate lower and upper intervals
plotdf$li <- plotdf$coefs - 1.96*plotdf$se_coefs
plotdf$ui <- plotdf$coefs + 1.96*plotdf$se_coefs

# Adjust the order of the levels for plotting
plotdf$DVs <- factor(plotdf$DVs, levels = rev(c("Ballot Paper Placement","Personal Votes"))) 
plotdf$IVs <- factor(plotdf$IVs, levels = rev(c("Attractiveness", "Trustworthiness", "Dominance")))
#plotdf$control <- factor(plotdf$control, levels = rev(c("With control", "Without control")))

# plot
main_effects_plot <- 
  ggplot(plotdf, aes(x = coefs, y = IVs, color = control)) +
  geom_point(aes(shape = DVs), position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = li, xmax = ui, shape = DVs), 
                 height = 0.01,
                 position = position_dodge(width = 0.3)) +
  xlim(-0.3, 0.3) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.05)) +  
  coord_cartesian() +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  xlab("Effect") + ylab("") +
  scale_color_manual(values = c("Without control" = "gray50", "With control" = "gray80")) +
  scale_shape_manual(values = c("Personal Votes" = 21, "Ballot Paper Placement" = 16)) +
  scale_shape(solid = TRUE) +
  labs(color = "Control", shape = "Dependent variable") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("") +
  guides(color = guide_legend(reverse = TRUE, order = 2), shape = guide_legend(reverse = TRUE, order = 1))

# Save the plot as a PDF file
ggsave(paste0(graph_path, "main_effects_plot.pdf"), plot = main_effects_plot, width = 8, height = 6, units = "in")