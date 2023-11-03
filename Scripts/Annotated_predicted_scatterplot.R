###############################################################################
## Replication of annotated vs. predicted scatter plot
## Author: Asbjørn Lindholm
## Date: 10/08/2023
##############################################################################

# clear memory
rm(list = ls())

libs <- c("tidyverse", "ggplot2")

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

### Load data and make scatter plots -------------------------------------------

# load data
load(paste0(data_path, "annotated.and.predicted.test.data.RData"))

### Grey version

# attractiveness
p1 <- df %>% 
  ggplot(aes(x = annotated_attractiveness, y = predicted_attractiveness)) +
  geom_point(size = 1.5, color = "black", alpha = 0.3) + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey40", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "attractivenss")) +
  ylab(paste("Predicted", "attractivenss")) + 
  ggtitle("")

# trustworthiness
p2 <- df %>% 
  ggplot(aes(x = annotated_trustworthiness, y = predicted_trustworthiness)) +
  geom_point(size = 1.5, color = "black", alpha = 0.3) + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey40", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "trustworthiness")) +
  ylab(paste("Predicted", "trustworthiness")) + 
  ggtitle("")


# dominance
p3 <- df %>% 
  ggplot(aes(x = annotated_dominance, y = predicted_dominance)) +
  geom_point(size = 1.5, color = "black", alpha = 0.3) + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey40", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "dominance")) +
  ylab(paste("Predicted", "dominance")) + 
  ggtitle("")

scatter_plots <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)

ggsave(paste0(graph_path, "annotated_predicted_scatterplot.grey.pdf"), plot = scatter_plots, width = 18, height = 6, units = "in")

## Blue version

# attractiveness
p1 <- df %>% 
  ggplot(aes(x = annotated_attractiveness, y = predicted_attractiveness)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "attractivenss")) +
  ylab(paste("Predicted", "attractivenss")) + 
  ggtitle("")

# trustworthiness
p2 <- df %>% 
  ggplot(aes(x = annotated_trustworthiness, y = predicted_trustworthiness)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "trustworthiness")) +
  ylab(paste("Predicted", "trustworthiness")) + 
  ggtitle("")


# dominance
p3 <- df %>% 
  ggplot(aes(x = annotated_dominance, y = predicted_dominance)) +
  geom_point(size = 1.5, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +
  xlim(2, 10) +
  ylim(2, 10) +
  theme_bw() +
  xlab(paste("Annotated", "dominance")) +
  ylab(paste("Predicted", "dominance")) + 
  ggtitle("")

scatter_plots <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)

ggsave(paste0(graph_path, "annotated_predicted_scatterplot_blue.pdf"), plot = scatter_plots, width = 18, height = 6, units = "in")