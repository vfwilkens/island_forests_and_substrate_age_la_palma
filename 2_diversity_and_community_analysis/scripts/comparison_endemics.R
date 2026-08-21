
# packages ----------------------------------------------------------------

library(dplyr)
library(vegan)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel)
library(rdacca.hp)
library(FSA)
library(rcompanion)


# load data ---------------------------------------------------------------

data_meta <- read_xlsx(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))


# prepare data for analysis -----------------------------------------------

data_meta_endemics <- data_meta %>%
  mutate(ecosystem_age = paste(ecosystemtype, age, sep = "_")) %>%
  mutate(rel_freq_endemics = native_and_endemic/species_richness) %>%
  mutate(rel_freq_natives = native_not_endemic/species_richness) %>%
  mutate(rel_freq_invasives = invasive/species_richness)

data_meta_endemics_Fa_young <- data_meta_endemics %>%
  filter(ecosystem_age == "Fa_young")

data_meta_endemics_Fa_old <- data_meta_endemics %>%
  filter(ecosystem_age == "Fa_old")

data_meta_endemics_Pi_young <- data_meta_endemics %>%
  filter(ecosystem_age == "Pi_young")

data_meta_endemics_Pi_old <- data_meta_endemics %>%
  filter(ecosystem_age == "Pi_old")

# check if it is working
mean(data_meta_endemics_Fa_old$rel_freq_invasives)
sd(data_meta_endemics_Fa_old$rel_freq_invasives)

# statistics --------------------------------------------------------------

kruskal_species_richness <- kruskal.test(species_richness ~ ecosystem_age,
                                         data = data_meta_endemics)
kruskal_species_richness
# p = 2.413e-12

kruskal_endemics <- kruskal.test(rel_freq_endemics ~ ecosystem_age,
                                 data = data_meta_endemics)
kruskal_endemics
# p = 0.000184

kruskal_natives <- kruskal.test(rel_freq_natives ~ ecosystem_age,
                                data = data_meta_endemics)
kruskal_natives
# p = 0.00021

kruskal_invsives <- kruskal.test(rel_freq_invasives ~ ecosystem_age,
                                 data = data_meta_endemics)
kruskal_invsives
# p = 0.0012

# all are significant follow up with Dunn multiple comparison test

dunn_species_richness <- FSA::dunnTest(species_richness ~ ecosystem_age,
                                       data = data_meta_endemics,
                                       method = "bonferroni")
dunn_species_richness


dunn_endemics <- FSA::dunnTest(rel_freq_endemics ~ ecosystem_age,
                               data = data_meta_endemics,
                               method = "bonferroni")
dunn_endemics

dunn_natives <- FSA::dunnTest(rel_freq_natives ~ ecosystem_age,
                              data = data_meta_endemics,
                              method = "bonferroni")
dunn_natives

dunn_invasives <- FSA::dunnTest(rel_freq_invasives ~ ecosystem_age,
                                data = data_meta_endemics,
                                method = "bonferroni")
dunn_invasives

# extract significance letters

richness_sig_letters <- cldList(P.adj ~ Comparison, data = dunn_species_richness$res)
richness_sig_letters
richness_sig_letter_for_plotting <- data.frame(
  group = c("Fa_young", "Fa_old", "Pi_young", "Pi_old"),
  label = c("a", "a", "b", "b")
)

endemics_sig_letters <- cldList(P.adj ~ Comparison, data = dunn_endemics$res)
endemics_sig_letters
endemics_sig_letter_for_plotting <- data.frame(
  group = c("Fa_young", "Fa_old", "Pi_young", "Pi_old"),
  label = c("a", "b", "b", "b")
)

natives_sig_letters <- cldList(P.adj ~ Comparison, data = dunn_natives$res)
natives_sig_letters
natives_sig_letter_for_plotting <- data.frame(
  group = c("Fa_young", "Fa_old", "Pi_young", "Pi_old"),
  label = c("a", "b", "b", "b")
)

invasives_sig_letters <- cldList(P.adj ~ Comparison, data = dunn_invasives$res)
invasives_sig_letters
invasives_sig_letter_for_plotting <- data.frame(
  group = c("Fa_young", "Fa_old", "Pi_young", "Pi_old"),
  label = c("ab", "a", "b", "b")
)



# use hierarchical partitioning to assess the relative importance of predictors

# filter data for the different ecosystems

data_meta_endemics_Fa <- data_meta_endemics %>%
  filter(ecosystemtype == "Fa")

data_meta_endemics_Pi <- data_meta_endemics %>%
  filter(ecosystemtype == "Pi")

# hier par

## Faya Brezal
iv_Fa <- data.frame(data_meta_endemics_Fa$age,
                    data_meta_endemics_Fa$elevation,
                    data_meta_endemics_Fa$`temperature(°C)`,
                    data_meta_endemics_Fa$precipitation)

hier_par_Fa_endemics <- rdacca.hp(data_meta_endemics_Fa$rel_freq_endemics,
                                  iv_Fa)
hier_par_Fa_endemics

hier_par_Fa_natives <- rdacca.hp(data_meta_endemics_Fa$rel_freq_natives,
                                 iv_Fa)
hier_par_Fa_natives

hier_par_Fa_invasives <- rdacca.hp(data_meta_endemics_Fa$rel_freq_invasives,
                                iv_Fa)
hier_par_Fa_invasives

## Pine forest
iv_Pi <- data.frame(data_meta_endemics_Pi$age,
                    data_meta_endemics_Pi$elevation,
                    data_meta_endemics_Pi$`temperature(°C)`,
                    data_meta_endemics_Pi$precipitation)

hier_par_Pi_endemics <- rdacca.hp(data_meta_endemics_Pi$rel_freq_endemics,
                                  iv_Pi)
hier_par_Pi_endemics

hier_par_Pi_natives <- rdacca.hp(data_meta_endemics_Pi$rel_freq_natives,
                                 iv_Pi)
hier_par_Pi_natives

hier_par_Pi_invasives <- rdacca.hp(data_meta_endemics_Pi$rel_freq_invasives,
                                iv_Pi)
hier_par_Pi_invasives


# plot it - boxplots of pairwise comparisons ------------------------------

ecosystems_ages <- c(
  "Fa_young" = "Fayal-Brezal (immature)",
  "Fa_old" = "Fayal-Brezal (mature)",
  "Pi_young" = "Canary pine forest (immature)",
  "Pi_old" = "Canary pine forest (mature)"
)
fs <- 12  # your desired font size

theme_richness_style <- theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text.x  = element_text(size = fs, colour = "black"),
    axis.text.y  = element_text(size = fs, colour = "black"),
    axis.title.x = element_text(size = fs, colour = "black"),
    axis.title.y = element_text(size = fs + 5, colour = "black"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black")
  )

# common fill scale
fill_scale <- scale_fill_manual(
  breaks = names(ecosystems_ages),
  labels = ecosystems_ages,
  values = c("#86BBD8", "#33658A", "#FFC15E", "#c97704")
)

# helper for the common x
x_scale <- scale_x_discrete(limits = c("Fa_young", "Fa_old", "Pi_young", "Pi_old"))




boxplot_richness <- data_meta_endemics %>%
  ggplot(aes(x = ecosystem_age, y = species_richness)) +
  geom_boxplot(aes(fill = ecosystem_age)) +
  geom_jitter(width = 0.2, size = 2, shape = 4) +
  scale_x_discrete(limits = c("Fa_young", "Fa_old", "Pi_young", "Pi_old")) +
  scale_y_continuous(limits = c(0, 21), 
                     breaks = seq(0, 20, by = 5)) +
  scale_fill_manual(breaks = names(ecosystems_ages),
                    labels = ecosystems_ages,
                    values = c("#86BBD8", "#33658A", "#FFC15E", "#c97704")) +
  theme_minimal() +
  theme(
    legend.position = c(0.8,0.75),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text.x  = element_text(size = fs, colour = "black"),
    axis.text.y  = element_text(size = fs, colour = "black"),
    axis.title.x = element_text(size = fs, colour = "black"),
    axis.title.y = element_text(size = fs + 5, colour = "black"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black")
  ) +
  labs(x = NULL, y = "Species richness") +
  theme(axis.text.x = element_blank()) +
  annotate(
    "text",
    x = 1, y = 21,
    label = "a) Species richness:\nKruskal-Wallis p = 2.41e-12",
    hjust = 0,
    size = fs /2 # <-- ggplot text size is in mm-ish units, not pt
  ) +
  geom_text(
    data = richness_sig_letter_for_plotting,
    aes(x = group, y = 19, label = label),
    inherit.aes = FALSE,
    size = fs / 2    # <-- match the annotate size
  )
boxplot_richness

boxplot_endemics <- data_meta_endemics %>%
  ggplot(aes(x = ecosystem_age, y = rel_freq_endemics)) +
  geom_boxplot(aes(fill = ecosystem_age)) +
  geom_jitter(width = 0.2, size = 2, shape = 4) +
  x_scale +
  scale_y_continuous(limits = c(0, 1.25),
                     breaks = seq(0, 1, by = 0.25)) +
  fill_scale +
  theme_richness_style +
  labs(x = NULL, y = "Proportion") +
  theme(axis.text.x = element_blank()) +
  annotate(
    "text",
    x = 1, y = 1.25,
    label = "b) Proportion of endemic species:\nKruskal-Wallis p = 0.00018",
    hjust = 0,
    size = fs / 2
  ) +
  geom_text(
    data = endemics_sig_letter_for_plotting,
    aes(x = group, y = 1.10, label = label),  # adjust if needed
    inherit.aes = FALSE,
    size = fs / 2
  )

boxplot_endemics

boxplot_natives <- data_meta_endemics %>%
  ggplot(aes(x = ecosystem_age, y = rel_freq_natives)) +
  geom_boxplot(aes(fill = ecosystem_age)) +
  geom_jitter(width = 0.2, size = 2, shape = 4) +
  x_scale +
  scale_y_continuous(limits = c(0, 1.25),
                     breaks = seq(0, 1, by = 0.25)) +
  fill_scale +
  theme_richness_style +
  labs(x = NULL, y = "Proportion") +
  theme(axis.text.x = element_blank()) +
  annotate(
    "text",
    x = 1, y = 1.25,
    label = "c) Proportion of native species:\nKruskal-Wallis p = 0.0002",
    hjust = 0,
    size = fs / 2
  ) +
  geom_text(
    data = natives_sig_letter_for_plotting,
    aes(x = group, y = 1.10, label = label),  # adjust if needed
    inherit.aes = FALSE,
    size = fs / 2
  )

boxplot_natives

# load hier par results

res_hier_par_plot_endemicity <- read_xlsx(here("2_diversity_and_community_analysis/data/Results_hierpar_endemicity.xlsx")) %>%
  mutate(across(where(is.numeric), function(x) if_else(x<0,0,x))) %>%
  filter(!Ecosystem %in% c("Fa_invasives", "Pi_invasives"))

res_hier_par_plot_endemicity


predictors_endemicity <- c(
  "Age" = "#ca0020",
  "Elevation" = "#f4a582",
  "Precipitation" = "#92c5de",
  "Temperature" = "#0571b0"
)

eco_labels <- c(
  Fa_endemics = "Fayal-Brezal - Endemics",
  Fa_natives   = "Fayal-Brezal - Natives",
  Pi_endemics = "Canary pine forest - Endemics",
  Pi_invasives = "Canary pine forest - Invasives",
  Fa_Spec_rich = "Fayal-Brezal - Species richness",
  Pi_Spec_rich = "Canary pine forest - Species richness"
)

desired_order <- c("Pi_natives", "Pi_endemics", "Pi_Spec_rich", "Fa_natives", "Fa_endemics", "Fa_Spec_rich")

res_hier_par_plot_endemicity <- res_hier_par_plot_endemicity %>%
  mutate(Ecosystem = factor(Ecosystem, levels = desired_order))

# plot it
hier_par_plot_endemicity <- ggplot(
  data = res_hier_par_plot_endemicity, aes(x = Ecosystem, y = Individual*100, fill = Predictor)
) +
  geom_bar(
    stat = "identity",
    color = "black"
  ) +
  scale_x_discrete(labels = eco_labels) +
  scale_fill_manual(name = NULL, values = predictors_endemicity
                    ) +
  scale_y_continuous(limits = c(0,50)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text.x  = element_text(size = fs, colour = "black"),
    axis.text.y  = element_text(size = fs, colour = "black"),
    axis.title.x = element_text(size = fs, colour = "black"),
    axis.title.y = element_text(size = fs + 5, colour = "black"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black")
  ) +
  theme(
    legend.position = c(0.8, 0.85),
    legend.text = element_text(size = fs + 5, colour = "black")
  ) +
  xlab("") +
  ylab("% of explained variance") +
  annotate("text", y = 50, x = 1, 
           label = "e) Relative importance of predictors",
           hjust = 0,
           size = fs / 2) +
  labs(fill = NULL, pattern = NULL)
hier_par_plot_endemicity


# plots in compound figure ------------------------------------------------

lay <- rbind(
  c(1, 2),
  c(3, 4)
)
library(gridExtra)
grid.arrange(boxplot_richness,
             boxplot_endemics,
             boxplot_natives,
             hier_par_plot_endemicity,
             layout_matrix = lay)

tiff(here("figures/endemicity_analysis_v1.tiff"), height = 45, width = 30, unit = "cm",
     res = 300)
grid.arrange(boxplot_richness,
             boxplot_endemics,
             boxplot_natives,
             hier_par_plot_endemicity,
             layout_matrix = lay)
dev.off()

pdf(here("figures/endemicity_analysis_v1.pdf"), height = 15, width = 12)
grid.arrange(boxplot_richness,
             boxplot_endemics,
             boxplot_natives,
             hier_par_plot_endemicity,
             layout_matrix = lay)
dev.off()

### END