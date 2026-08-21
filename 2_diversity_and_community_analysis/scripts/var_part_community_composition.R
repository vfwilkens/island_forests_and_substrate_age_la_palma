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

data_community_comp <- read_xlsx(here("2_diversity_and_community_analysis/data/dca_scores_plot.xlsx"))%>%
  mutate(plotcode = str_remove_all(plotcode, "_"))

data_predictors <- read_xlsx(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))

# join metadata to CC data

data_community_comp_meta <- data_community_comp %>%
  left_join(.,  data_predictors, by = "plotcode")

# filter for Fa and Pi ecosystem

data_community_comp_meta_Fa <- data_community_comp_meta %>%
  filter(ecosystemtype == "Fa")

data_community_comp_meta_Pi <- data_community_comp_meta %>%
  filter(ecosystemtype == "Pi")


# var part ----------------------------------------------------------------

resp_Fa_cc <- data.frame(data_community_comp_meta_Fa$DCA1,
                         data_community_comp_meta_Fa$DCA2)


pred_Fa_cc <- data.frame(data_community_comp_meta_Fa$age, data_community_comp_meta_Fa$elevation,
                                data_community_comp_meta_Fa$MAT, data_community_comp_meta_Fa$MAP)

resp_Pi_cc <- data.frame(data_community_comp_meta_Pi$DCA1,
                         data_community_comp_meta_Pi$DCA2)


pred_Pi_cc <- data.frame(data_community_comp_meta_Pi$age, data_community_comp_meta_Pi$elevation,
                                data_community_comp_meta_Pi$MAT, data_community_comp_meta_Pi$MAP)

# hier par

hierpar_cc_Fa <- rdacca.hp(
  resp_Fa_cc, pred_Fa_cc,
  var.part = T
)

hierpar_cc_Fa

hierpar_cc_Pi <- rdacca.hp(
  resp_Pi_cc, pred_Pi_cc,
  var.part = F
)

hierpar_cc_Pi


# plot results ------------------------------------------------------------

data_hier_par_cc <- read_xlsx(here("2_diversity_and_community_analysis/data/Res_hier_par_CC.xlsx"))

predictors_endemicity <- c(
  "Age" = "#ca0020",
  "Elevation" = "#f4a582",
  "Precipitation" = "#92c5de",
  "Temperature" = "#0571b0"
)

eco_labels <- c(
  Fa_cc = "Fayal Brezal",
  Pi_cc = "Pine forest"
)

hier_par_plot_cc <- ggplot(
  data = data_hier_par_cc, aes(x = Ecosystem, y = Individual*100, fill = Predictor)
) +
  geom_bar(
    stat = "identity",
    color = "black"
  ) +
  scale_x_discrete(labels = eco_labels,
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(name = NULL, values = predictors_endemicity
  ) +
  scale_y_continuous(limits = c(0,50)) +
  theme_classic()+
  theme(legend.position = c(0.8,0.8)) +
  xlab("") +
  ylab("% of explained variance") +
  annotate("text", x = 1, y = 50, 
           label = "c) Relative importance of predictors",
           hjust = 0) +
  labs(fill = NULL, pattern = NULL)
hier_par_plot_cc


pdf(here("figures/hier_par_cc.pdf"), height = 7, width = 7)
hier_par_plot_cc
dev.off()
