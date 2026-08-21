library(here)
library(tidyverse)
library(tidyterra)
library(terra)
library(hypervolume)
library(readxl)

v <- vect("/mnt/NAS/CanaryData/vegClasses.gpkg")
v_pine <- subset(v, v$Veg_class_ == "Pine Forest")
v_fay <- subset(v, v$Veg_class_ == "Fayal-Brezal")
v_forest <- subset(v, v$Veg_class_ == "Pine Forest" | v$Veg_class_ == "Fayal-Brezal")

###############################################################################
## Prepare Climate Data, Create Sample Points, Extract Climate Variables

## Precipitation
prec_path <- "/mnt/NAS/CanaryData/CHELSA_CANARIES/pr/1979-2013/"
r_prec <- rast(list.files(prec_path, pattern = "\\.tif$", full.names = TRUE))
r_prec <- crop(r_prec, v, mask = TRUE)
r_prec <- sum(r_prec)
names(r_prec) <- "p"

## Temperature
tas_path <- "/mnt/NAS/CanaryData/CHELSA_CANARIES/tas/1979-2013"
r_tas <- rast(list.files(tas_path, pattern = "\\.tif$", full.names = TRUE))
r_tas <- crop(r_tas, v, mask = TRUE)
r_tas <- mean(r_tas)
names(r_tas) <- "t"
r_tas_pine <- crop(r_tas, v_pine, mask = TRUE)

## Create Point Sample and then extract the climate values from the rasters
r_clim <- c(r_prec, r_tas)

pts_pine <- spatSample(v_pine, size = 5000, method = "regular")
pts_pine_clim <- terra::extract(r_clim, pts_pine, method = "simple", bind = TRUE)
df_pine_clim <- as.data.frame(pts_pine_clim)
df_pine_clim$t <- df_pine_clim$t - 273.15
df_pine_clim <- df_pine_clim[, c(5,6)]

pts_fay <- spatSample(v_fay, size = 5000, method = "regular")
pts_fay_clim <- terra::extract(r_clim, pts_fay, method = "simple", bind = TRUE)
df_fay_clim <- as.data.frame(pts_fay_clim)
df_fay_clim$t <- df_fay_clim$t - 273.15
df_fay_clim <- df_fay_clim[, c(5,6)]

df_forests <- rbind(df_pine_clim, df_fay_clim)

## Calculate hypervolumes
hv_pine <- hypervolume_gaussian(df_pine_clim, name = "pine", kde.bandwidth = estimate_bandwidth(df_pine_clim))
hv_fay <- hypervolume_gaussian(df_fay_clim, name = "fay", kde.bandwidth = estimate_bandwidth(df_fay_clim))

hv_forests <- hypervolume_join(hv_pine, hv_fay)

## Occupancy ---- Funktioniert irgendwie nicht!!
hv_occupancy <- hypervolume_n_occupancy(hv_forests, method = "box", FUN = "mean")

df_hyper <- hypervolume_to_data_frame(hv_forests)


## Load Mature Immature data
field_data <- read_excel(here("2_diversity_and_community_analysis/data/MvsI_meta_data_29_01_2026.xlsx"))
field_data <- as.data.frame(field_data)

write.csv(df_hyper, here("1_study_area_map/data/PF_FB_hypervolumes.csv"))

ggplot(df_hyper) +
  geom_point(aes(x = t, y = p, col = Name, size = ValueAtRandomPoints), alpha = 0.1) +
  geom_point(data = field_data, aes(x = MAT, y = MAP, col = age)) +
  theme_minimal()


