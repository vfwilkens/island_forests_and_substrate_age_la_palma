
# packages ----------------------------------------------------------------

library(devtools)
library(readxl)
library(dplyr)
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)
library(veganEx)
library(here)

#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)
# load data ---------------------------------------------------------------

data_raw <- read_xlsx(here("2_diversity_and_community_analysis/data/Data_lp.xlsx"))


# data to community matrix ------------------------------------------------

data_pa <- data_raw %>%
  mutate(site = paste(ecosystem, plot, sep = "_")) %>%   # merge ecosystem + plot
  distinct(species, site) %>%                            # avoid duplicates
  mutate(present = 1L) %>%
  pivot_wider(
    names_from  = site,
    values_from = present,
    values_fill = 0L
  ) 

data_pa_matrix <- as.matrix(data_pa[, c(2:103)])
rownames(data_pa_matrix) <- data_pa$species

data_pa_matrix_t <- t(data_pa_matrix)

# calculate dissimilarity -------------------------------------------------

dissimilarity_matrix <- vegdist(data_pa_matrix_t,
                                method = "jaccard")  

# visualize differences first for overview
nmds_north_south <- metaMDS(dissimilarity_matrix,
                            try = 1000,
                            trymax = 5000)


stressplot(nmds_north_south)
nmds_north_south

# test for significant differences in community composition

sig_dis_comm <- adonis2(dissimilarity_matrix ~ ecosystem_age, data = sites, permutations = 1000)
sig_dis_comm

# anosim:

anosim(dissimilarity_matrix, sites$ecosystem_age, permutations = 1000)

anosim.pairwise(dissimilarity_matrix, grouping = sites$ecosystem_age,
                p.adjust.m = "bonferroni", perm = 10000)

# load meta data ----------------------------------------------------------

data_meta <- read_xlsx(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx")) %>%
  mutate(group = paste(ecosystem, age, sep = '_'))

data_meta

cca_north_south <- cca(data_pa_matrix_t ~ age + elevation + MAT + MAP,
                       data = data_meta)

summary(cca_north_south)
anova.cca(cca_north_south, by = "axis")
plot(cca_north_south)

plot(cca_north_south, display = c("sites"), type = 'n')
points(cca_north_south, display = "sites", pch = 21, bg = "grey85")

plot(cca_north_south, display = "bp", add = TRUE, col = "firebrick", lwd = 1.5)


plot(cca_north_south, type = "n")

# 2) add sites + species (optional)
points(cca_north_south, display = "sites", pch = 21, bg = "grey85")
text(cca_north_south,  display = "species", cex = 0.8, col = "grey30")

# 3) add constraining-variable arrows (biplot scores)
ordiarrows(cca_north_south, display = "bp", col = "firebrick", lwd = 1.5)
text(cca_north_south,      display = "bp", col = "firebrick", cex = 0.9)
# data_pa_matrix_t
# data_meta

summary(cca_north_south)

# RDA analysis

# compare with RDA but not necessary as CCA fits better
# rda_north_south <- rda(data_pa_matrix_t ~ age + elevation + MAT + MAP,
#                          data = data_meta)
# summary(rda_north_south)
# 
# anova(rda_north_south, by = "axis")
# anova(rda_north_south, by = "margin")
# 
# 
# plot(rda_north_south, display = c("sites", "bp"))



# preparation for plotting ------------------------------------------------

site_scores_cca <- data.frame(scores(cca_north_south, display = "sites", choice = 1:3))
site_scores_cca$plotcode <- rownames(site_scores_cca)
site_scores_cca

sites <- site_scores_cca %>%
  mutate(plotcode = str_replace_all(plotcode, "_", "")) %>%
  left_join(., data_meta, by = "plotcode") %>%
  mutate(ecosystem_age = paste(ecosystemtype, age, sep = "_"))

bp <- data.frame(scores(cca_north_south, display = "bp", choices = 1:3))
bp

# plot it -----------------------------------------------------------------

# set colours and labels 

# 1) Define colours in the order you want
cols <- c(
  "Fa_old"   = "#33658A",
  "Pi_old"   = "#c97704",
  "Fa_young" = "#86BBD8",
  "Pi_young" = "#FFC15E"
)

# 2) Define pretty labels (same names as your data levels)
labs_ecosys <- c(
  "Pi_old"   = "Canary pine forest (mature)",
  "Fa_old"   = "Fayal-Brezal (mature)",
  "Pi_young" = "Canary pine forest (immature)",
  "Fa_young" = "Fayal-Brezal (immature)"
)

# 3) Ensure factor order + labels in the data
sites <- sites %>%
  mutate(ecosystem_age = factor(ecosystem_age,
                                levels = c("Pi_old", "Fa_old", "Pi_young", "Fa_young")))

bp$term <- c("Age", "Elevation", "Temperature", "Precipitation")
bp

#spacing for the arrows
label_mult <- 1
fs = 12


cca_ggplot <- ggplot(data = sites, aes(
  x = CCA1, y = CCA2, color = ecosystem_age
)) +
  geom_point(size = 2) + 
  stat_ellipse(
    aes(group = ecosystem_age),
    level = 0.95, type = "t", linewidth = 0.7
  ) +
  scale_color_manual(
    values = cols,
    labels = labs_ecosys,
    breaks = c("Pi_old", "Fa_old", "Pi_young", "Fa_young")
  ) +
  theme_minimal()  +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text.x  = element_text(size = fs, colour = "black"),
    axis.text.y  = element_text(size = fs, colour = "black"),
    axis.title.x = element_text(size = fs + 5, colour = "black"),
    axis.title.y = element_text(size = fs + 5, colour = "black"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black")
  ) +
  labs(
    x = "CCA1 (5.52 % of variance)",
    y = "CCA2 (2.57 % of variance)",
    color = NULL
  ) +
  theme(
    legend.position = "none") +
  geom_segment(
    data = bp,
    aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = bp,
    aes(x = label_mult * CCA1, y = label_mult * CCA2, label = term),
    inherit.aes = FALSE,
    color = "black",
    size = fs/1.5,
    box.padding = 0.7,     # spacing between labels
    point.padding = 0.4,   # spacing from points/ellipses
    force = 8,             # stronger repulsion
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  annotate("text", x = -2.6, y = 3.2, label = "a)", hjust = 0, size = fs)

cca_ggplot

# For CCA1 vs CCA3

cca_ggplot_1_3 <- ggplot(data = sites, aes(
  x = CCA1, y = CCA3, color = ecosystem_age
)) +
  geom_point(size = 2) + 
  stat_ellipse(
    aes(group = ecosystem_age),
    level = 0.95, type = "t", linewidth = 0.7
  ) +
  scale_color_manual(
    values = cols,
    labels = labs_ecosys,
    breaks = c("Pi_old", "Fa_old", "Pi_young", "Fa_young")
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text.x  = element_text(size = fs, colour = "black"),
    axis.text.y  = element_text(size = fs, colour = "black"),
    axis.title.x = element_text(size = fs + 5, colour = "black"),
    axis.title.y = element_text(size = fs + 5, colour = "black"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black")
  ) +
  labs(
    x = "CCA1 (5.52 % of variance)",
    y = "CCA3 (1.84 % of variance)",
    color = NULL
  ) +
  theme(
    legend.position = c(0.5, 0.9),
    legend.text = element_text(size = fs + 7, colour = "black"
  )) +
  geom_segment(
    data = bp,
    aes(x = 0, y = 0, xend = CCA1, yend = CCA3),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = bp,
    aes(x = label_mult * CCA1, y = label_mult * CCA3, label = term),
    inherit.aes = FALSE,
    color = "black",
    size = fs / 1.5,
    box.padding = 0.7,     # spacing between labels
    point.padding = 0.4,   # spacing from points/ellipses
    force = 8,             # stronger repulsion
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  annotate("text", x = -2.6, y = 6.2, label = "b)", hjust = 0, size = fs)

cca_ggplot_1_3

# combine to one compound figure

library(gridExtra)

grid.arrange(cca_ggplot,
             cca_ggplot_1_3,
             ncol = 2)

tiff(here("figures/cca_v1.tiff"), width = 35, height = 25, unit = "cm", res = 300)
grid.arrange(cca_ggplot,
             cca_ggplot_1_3,
             ncol = 2)
dev.off()

pdf(here("figures/cca_v1.pdf"), width = 15, height = 13)
grid.arrange(cca_ggplot,
             cca_ggplot_1_3,
             ncol = 2)
dev.off()
