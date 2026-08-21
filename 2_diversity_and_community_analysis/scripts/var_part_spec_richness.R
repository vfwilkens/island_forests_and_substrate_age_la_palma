# packages ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(vegan)
library(rdacca.hp)
library(ggplot2)
library(ggvenn)
library(ggVennDiagram)
library(tidyverse)
library(ggpattern)


# load data ----------------------------------------------------------------

data_predictors <- read_xlsx(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))

# filter for Fa and Pi

data_predictors_Fa <- data_predictors %>%
  filter(ecosystemtype == "Fa")

data_predictors_Pi <- data_predictors %>%
  filter(ecosystemtype == "Pi")

# hierpar -----------------------------------------------------------------

resp_Fa_spec_rich <- data.frame(data_predictors_Fa$species_richness)


pred_Fa_spec_rich <- data.frame(data_predictors_Fa$age, data_predictors_Fa$elevation,
                      data_predictors_Fa$MAT, data_predictors_Fa$MAP)

resp_Pi_spec_rich <- data.frame(data_predictors_Pi$species_richness)


pred_Pi_spec_rich <- data.frame(data_predictors_Pi$age, data_predictors_Pi$elevation,
                      data_predictors_Pi$MAT, data_predictors_Pi$MAP)

hierpar_spec_rich_Fa <- rdacca.hp(
  resp_Fa_spec_rich, pred_Fa_spec_rich,
  var.part = T
)

hierpar_spec_rich_Fa

hierpar_spec_rich_Pi <- rdacca.hp(
  resp_Pi_spec_rich, pred_Pi_spec_rich,
  var.part = F
)
hierpar_structure_Pi


# plot it -----------------------------------------------------------------

# load results hier par species richness

res_hier_par_spec_rich <- read_xlsx(here("2_diversity_and_community_analysis/data/Res_hier_par_spec_richn.xlsx"))

predictors_endemicity <- c(
  "Age" = "#ca0020",
  "Elevation" = "#f4a582",
  "Precipitation" = "#92c5de",
  "Temperature" = "#0571b0"
)

eco_labels <- c(
  Fa_Spec_rich = "Fayal Brezal",
  Pi_Spec_rich = "Pine forest"
)

hier_par_plot_spec_rich <- ggplot(
  data = res_hier_par_spec_rich, aes(x = Ecosystem, y = Individual*100, fill = Predictor)
) +
  geom_bar(
    stat = "identity",
    color = "black"
  ) +
  scale_x_discrete(labels = eco_labels,
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(name = NULL, values = predictors_endemicity
  ) +
  scale_y_continuous(limits = c(0,40)) +
  theme_classic()+
  theme(legend.position = c(0.8,0.8)) +
  xlab("") +
  ylab("% of explained variance") +
  annotate("text", x = 1, y = 40, 
           label = "c) Relative importance of predictors",
           hjust = 0) +
  labs(fill = NULL, pattern = NULL)
hier_par_plot_spec_rich


pdf(here("figures/hier_par_spec_rich.pdf"), height = 7, width = 7)
hier_par_plot_spec_rich
dev.off()
