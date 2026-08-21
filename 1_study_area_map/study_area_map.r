library(terra)
library(readxl)
library(ggplot2)
library(patchwork)
library(tidyterra)

#La Palma coastline obtained from USGS Island database (https://www.usgs.gov/publications/global-islands)
la_palma_query <- "SELECT * FROM islands WHERE Plate = 'Africa' AND Name_USGSO IN ('La Palma')"
la_palma_mask <- terra::vect("/Users/wilkens/Documents/Data/islands.gpkg", query = la_palma_query)

plot(la_palma_mask)

#Canary Islands vegetation classes obtained from the Spanish government (https://www.ign.es/web/ign/portal)
ecosystems <- vect("/Volumes/homes/CanaryData/vegClasses.gpkg")

v_pine <- subset(ecosystems, ecosystems$Veg_class_ == "Pine Forest")
v_fay <- subset(ecosystems, ecosystems$Veg_class_ == "Fayal-Brezal")

v_plots$ecosystem_age <- stringr::str_c(v_plots$ecosystemtype, " ", v_plots$age)

p_map <- ggplot() +
    geom_spatvector(data = la_palma_mask, fill = "white", color = "black") +
    geom_spatvector(data = v_fay, fill = "#778896", color = NA, alpha = 0.5) +
    geom_spatvector(data = v_pine, fill = "#ae987a", color = NA, alpha = 0.5) +
    geom_spatvector(data = v_plots, aes(fill = ecosystem_age), shape = 21, color = "black", size = 2) +
    scale_fill_manual(values = c("#33658A", "#86BBD8", "#c97704", "#FFC15E")) +
    theme_void() +
    theme(legend.position = "none")

p_map

field_data <- read_excel(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))
v_plots <- vect(field_data, geom = c("longitude", "latitude"), crs = "EPSG:4326")

hypervolumes <- read.csv(here("1_study_area_map/data/PF_FB_hypervolumes.csv"))

field_data <- read_excel(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))
field_data <- as.data.frame(field_data)
field_data$ecosystem_age <- stringr::str_c(field_data$ecosystemtype, " ", field_data$age)

hyper <- ggplot(hypervolumes) +
    # geom_point(aes(x = t, y = p, col = Name, size = ValueAtRandomPoints), alpha = 0.05) +
    ggforce::geom_mark_hull(aes(x = t, y = p, group = Name, fill = Name), concavity = 2.5, alpha = 0.35) +
    geom_point(data = field_data, aes(x = MAT, y = MAP, fill = ecosystem_age), shape = 21, color = "black", size = 2) +
    # scale_color_manual(values = c("pine" = "#ae987a", "fay" = "#778896")) +
    scale_fill_manual(values = c("Fa old" = "#33658A", "Fa young" = "#86BBD8", "Pi old" = "#c97704", "Pi young" = "#FFC15E", "pine" = "#ae987a", "fay" = "#778896")) +
    xlab("Mean annual temperature [°C]") +
    ylab("Annual precipitation [mm]") +
    xlim(c(7, 26)) +
    ylim(c(250, 1500)) +
    theme_minimal() +
    theme(
        legend.position = "none",
        strip.text = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black")
    )

hyper

layout <- "
AAA##
AAABB
AAABB
AAA##
"

plot_images <- p_map + hyper +
    plot_layout(design = layout)

ggsave(
    filename = here("figures/hypervolume_study_area.pdf"),
    plot = plot_images,
    width = 10, # standard width in inches
    height = 8, # standard height in inches
    dpi = 500, # 300 DPI is standard for print publication
    bg = "white" # Ensure background is white (not transparent)
)