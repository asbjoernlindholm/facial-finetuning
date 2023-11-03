###############################################################################
## Replication script for lowess plots
## Author: Asbjørn Lindholm
## Date: 09/10/2023
###############################################################################

rm(list = ls())

libs <- c("tidyverse", "ggplot2")

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
## Part 0: Data wrangling
## 
###############################################################################


load(paste0(data_path, "FT22.KV21.Annotated.Merged.RData"))


#In order to make the data from the two elections comparable, 
#we Z-score transform the dependent variables. 

#Keep only observations with images, not bad images and without text
df <- df %>% filter(image_missing == 0, image_bad == 0, image_text == 0)

df <- df %>% mutate(relative_success = ifelse(relative_success == 0, relative_success + 0.001,
                                              relative_success))

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

#Keep only observations with more than one candidate on the ballot
df2 <- df %>% filter(no_candidates_on_ballot != 1)


###############################################################################
## Part 1: Attractiveness
###############################################################################

trait <- "Attractiveness"
trait2 <- "attractiveness_pd"
election <- "Pooled Data"

###### Ballot Percentile Rank ######
y_name <- "Ballot Paper Placement"

p1 <- df2 %>% filter(election == "FV22") %>% ggplot(aes(x = attractiveness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df2 %>% filter(election == "KV21") %>% ggplot(aes(x = attractiveness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df2 %>% ggplot(aes(x = attractiveness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)

ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)


###### Personal Votes ######
y_name <- "Personal Votes"

p1 <- df %>% filter(election == "FV22") %>% ggplot(aes(x = attractiveness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df %>% filter(election == "KV21") %>% ggplot(aes(x = attractiveness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df %>% ggplot(aes(x = attractiveness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)

ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)



###############################################################################
## Part 2: Trustworthiness
###############################################################################

trait <- "Trustworthiness"
trait2 <- "trustworthiness_pd"
election <- "Pooled data"


###### Ballot Percentile Rank ######
y_name <- "Ballot Paper Placement"

p1 <- df2 %>% filter(election == "FV22") %>% ggplot(aes(x = trustworthiness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df2 %>% filter(election == "KV21") %>% ggplot(aes(x = trustworthiness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df2 %>% ggplot(aes(x = trustworthiness_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)

ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)


###### Personal Votes ######
y_name <- "Personal Votes"

p1 <- df %>% filter(election == "FV22") %>% ggplot(aes(x = trustworthiness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df %>% filter(election == "KV21") %>% ggplot(aes(x = trustworthiness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df %>% ggplot(aes(x = trustworthiness_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)

ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)


###############################################################################
## Part 3: Dominance
###############################################################################

trait <- "Dominance"
trait2 <- "dominance_pd"
election <- "Pooled Data"


###### Ballot Percentile Rank ######
y_name <- "Ballot Paper Placement"

p1 <- df2 %>% filter(election == "FV22") %>% ggplot(aes(x = dominance_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df2 %>% filter(election == "KV21") %>% ggplot(aes(x = dominance_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df2 %>% ggplot(aes(x = dominance_pd, y = percentile_rank_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5), limits = c(-2,2)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)

ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)

###### Personal Votes ######
y_name <- "Personal Votes"

p1 <- df %>% filter(election == "FV22") %>% ggplot(aes(x = dominance_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.4, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("General Election") + 
  scale_y_continuous(name = y_name, breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p2 <- df %>% filter(election == "KV21") %>% ggplot(aes(x = dominance_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Local Election") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))


p3 <- df %>% ggplot(aes(x = dominance_pd, y = personal_votes_std)) +
  geom_point(size = 1.5, alpha = 0.2, color = "#003d73") + 
  stat_smooth(method = "loess", se = FALSE, linetype = "longdash", color = "grey50", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  theme_bw() + ggtitle("Pooled Elections") + 
  scale_y_continuous(name = "", breaks = seq(-4,4,0.5), labels = seq(-4,4,0.5), limits = c(-4,4)) +
  scale_x_continuous(name = trait, breaks = seq(2,8,1), labels = seq(2,8,1), limits = c(2,8))

p_norm <- gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol = 3)


ggsave(plot = p_norm, filename = paste0(graph_path, "lowess.plot.", trait, ".", y_name, ".pdf"), path = graph_path,
       device = pdf, width = 15, height = 5)
