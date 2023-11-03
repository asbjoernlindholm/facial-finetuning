###############################################################################
## Replication of Annotated vs. predicted scores
## Author: Asbjørn Lindholm
## Date: 16/10/2023
##############################################################################

# clear memory
rm(list = ls())

libs <- c("tidyverse")

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

# load test data
load(paste0(data_path, "annotated.and.predicted.test.data.RData"))
test_df <- df

# load full training data
load(paste0(data_path, "Attractiveness.Full_data.RData"))

# load candidate data
load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))


###############################################################################
## Part 1: Attractiveness
###############################################################################

trait <- "Attractiveness"

# Predicted vs. Annotated scatterplot
p1 <- test_df %>% 
  ggplot(aes(x = annotated_attractiveness, y = predicted_attractiveness)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", trait)) +
  ylab(paste("Predicted", trait)) + 
  ggtitle("a) Scatter plot of annotated and predicted score")

# Predicted vs. Annotated density plot
p2 <- test_df %>% ggplot(aes(x = annotated_attractiveness, color = "Annotated score")) +
  geom_density(alpha = 0.5, fill = "#009E73") +
  geom_density(aes(x = predicted_attractiveness, color = "Predicted score"), alpha = 0.5, fill = "#0072B2") +
  scale_color_manual(values = c("Annotated score" = "#009E73", "Predicted score" = "#0072B2")) +
  xlim(0, 10) +
  labs(title = "b) Density plot of annotated and predicted score",
       x = trait,
       y = "Density") + theme_bw() +
  theme(legend.title = element_blank(),
        legend.position=c(0.84,.91))

# Gender/age plot with full training data set
p3 <- full_df %>% mutate(gender = ifelse(gender > 50, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=score)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(aes(color = factor(gender)),alpha = 0.2, size = 0.5, width = 0.2) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  #geom_density(aes(fill=gender), alpha = 0.5) +
  xlab("Gender") + ylab(paste("Annotated", trait)) +
  #  xlim(1, 10) +
  ggtitle("c) Boxplot with annotated scores for training data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position = "none")

# Gender/age plot with full candidate data
p4 <- 
  df %>% mutate(gender = ifelse(gender == 0, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=attractiveness_pd)) +
  #geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  xlab("Gender") + ylab(paste("Predicted", trait)) +
  #  xlim(1, 10) +
  ggtitle("d) Boxplot with predicted scores for candidate data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position=c(.95,.8),
        legend.title = element_text( size=8), legend.text=element_text(size=5),
        legend.key.width=unit(0.3,"cm")) 

evaluation_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave(paste0(graph_path, "Age_gender.plot.", trait, ".pdf"), plot = evaluation_plot, width = 12, height = 12, units = "in")


###############################################################################
## Part 2: Trustworthiness
###############################################################################

# load full training data
load(paste0(data_path, "Trustworthiness.Full_data.RData"))

trait <- "Trustworthiness"

# Predicted vs. Annotated scatterplot
p1 <- test_df %>% 
  ggplot(aes(x = annotated_trustworthiness, y = predicted_trustworthiness)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", trait)) +
  ylab(paste("Predicted", trait)) + 
  ggtitle("a) Scatter plot of annotated and predicted score")

# Predicted vs. Annotated density plot
p2 <- test_df %>% ggplot(aes(x = annotated_trustworthiness, color = "Annotated score")) +
  geom_density(alpha = 0.5, fill = "#009E73") +
  geom_density(aes(x = predicted_trustworthiness, color = "Predicted score"), alpha = 0.5, fill = "#0072B2") +
  scale_color_manual(values = c("Annotated score" = "#009E73", "Predicted score" = "#0072B2")) +
  xlim(0, 10) +
  labs(title = "b) Density plot of annotated and predicted score",
       x = trait,
       y = "Density") + theme_bw() +
  theme(legend.title = element_blank(),
        legend.position=c(0.84,.91))

# Gender/age plot with full training data set
p3 <- full_df %>% mutate(gender = ifelse(gender > 50, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=score)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(aes(color = factor(gender)),alpha = 0.2, size = 0.5, width = 0.2) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  #geom_density(aes(fill=gender), alpha = 0.5) +
  xlab("Gender") + ylab(paste("Annotated", trait)) +
  #  xlim(1, 10) +
  ggtitle("c) Boxplot with annotated scores for training data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position = "none")

# Gender/age plot with full candidate data
p4 <- 
  df %>% mutate(gender = ifelse(gender == 0, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=trustworthiness_pd)) +
  #geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  xlab("Gender") + ylab(paste("Predicted", trait)) +
  #  xlim(1, 10) +
  ggtitle("d) Boxplot with predicted scores for candidate data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position=c(.94,.8),
        legend.title = element_text( size=8), legend.text=element_text(size=5),
        legend.key.width=unit(0.3,"cm")) 

evaluation_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave(paste0(graph_path, "Age_gender.plot.", trait, ".pdf"), plot = evaluation_plot, width = 12, height = 12, units = "in")



###############################################################################
## Part 3: Dominance
###############################################################################

# load full training data
load(paste0(data_path, "Dominance.Full_data.RData"))

trait <- "Dominance"

# Predicted vs. Annotated scatterplot
p1 <- test_df %>% 
  ggplot(aes(x = annotated_dominance, y = predicted_dominance)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", trait)) +
  ylab(paste("Predicted", trait)) + 
  ggtitle("a) Scatter plot of annotated and predicted score")

# Predicted vs. Annotated density plot
p2 <- test_df %>% ggplot(aes(x = annotated_dominance, color = "Annotated score")) +
  geom_density(alpha = 0.5, fill = "#009E73") +
  geom_density(aes(x = predicted_dominance, color = "Predicted score"), alpha = 0.5, fill = "#0072B2") +
  scale_color_manual(values = c("Annotated score" = "#009E73", "Predicted score" = "#0072B2")) +
  xlim(0, 10) +
  labs(title = "b) Density plot of annotated and predicted score",
       x = trait,
       y = "Density") + theme_bw() +
  theme(legend.title = element_blank(),
        legend.position=c(0.84,.91))

# Gender/age plot with full training data set
p3 <- full_df %>% mutate(gender = ifelse(gender > 50, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=score)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(aes(color = factor(gender)),alpha = 0.2, size = 0.5, width = 0.2) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  #geom_density(aes(fill=gender), alpha = 0.5) +
  xlab("Gender") + ylab(paste("Annotated", trait)) +
  #  xlim(1, 10) +
  ggtitle("c) Boxplot with annotated scores for training data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position = "none")

# Gender/age plot with full candidate data
p4 <- 
  df %>% mutate(gender = ifelse(gender == 0, "Male", "Female")) %>% 
  ggplot(aes(x=factor(gender), y=dominance_pd)) +
  #geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = age), alpha = 0.5, size = 0.5, width = 0.2) +
  scale_color_gradientn(colors = rev(viridis::viridis(10))) +
  xlab("Gender") + ylab(paste("Predicted", trait)) +
  #  xlim(1, 10) +
  ggtitle("d) Boxplot with predicted scores for candidate data") +
  theme_bw() + scale_y_continuous(breaks=c(0, 2,4, 6, 8), limits=c(1, 9)) +
  theme(legend.position=c(.94,.8),
        legend.title = element_text( size=8), legend.text=element_text(size=5),
        legend.key.width=unit(0.3,"cm")) 

evaluation_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave(paste0(graph_path, "Age_gender.plot.", trait, ".pdf"), plot = evaluation_plot, width = 12, height = 12, units = "in")
