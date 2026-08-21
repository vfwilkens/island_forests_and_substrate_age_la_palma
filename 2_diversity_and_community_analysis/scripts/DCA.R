# packages ----------------------------------------------------------------

library(dplyr)
library(vegan)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel)
library(rdacca.hp)
library(tidyverse)


# read data ---------------------------------------------------------------


data_raw <- read_xlsx(here("2_diversity_and_community_analysis/data/Data_lp.xlsx"))
data_meta <- read_xlsx(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))


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


# performe DCA ------------------------------------------------------------

dca_la_palma <- vegan::decorana(
  veg = data_pa_matrix_t
)

dca_la_palma
#summary(dca_la_palma)

plot(dca_la_palma, display = "sites", choices = c(1,2))

# extract eigenvalues
dca_la_palma
eigenvalues <- dca_la_palma$evals

# estimation for explained variance

prop_explained <- eigenvalues / sum(eigenvalues)

prop_explained

# axis 1 and 2 are relatively more important than axis 3 and 4
# so I extract the scores of the first two DCA axis and use these for further analysis

site_scores <- scores(dca_la_palma, display = "sites")
site_scores

site_scores_axis_1_2 <- data.frame(rownames(data_pa_matrix_t), site_scores[,c(1,2)])

colnames(site_scores_axis_1_2) <- c("plotcode", "DCA1", "DCA2")

# write scores to excel ---------------------------------------------------

write_xlsx(site_scores_axis_1_2, path = here("2_diversity_and_community_analysis/data/dca_scores_plot.xlsx"))
