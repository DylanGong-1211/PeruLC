library(terra)
library(geodata)
library(here)
library(dplyr)

p2000 <- here("extdata","ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.area-subset.0.-68.-19.-82.nc")
p2010 <- here("extdata", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.area-subset.0.-68.-19.-82.nc")
p2020 <- here("extdata","C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.area-subset.0.-68.-19.-82.nc")

lc2000 <- rast(p2000)
lc2010 <- rast(p2010)
lc2020 <- rast(p2020)
lc2000 <- lc2000[["lccs_class"]]
lc2010 <- lc2010[["lccs_class"]]
lc2020 <- lc2020[["lccs_class"]]

cropland_vals <- c(10, 11, 12, 20, 30, 40)
forest_vals <- c(50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90,160,170)
builtup_vals <- c(190)
water_vals <- c(210)
ice_vals <- c(220)
bare_vals <- c(200, 201, 202)
grass_vals <- c(100, 110, 120, 121, 122, 130, 150, 151, 152, 153)
wetland_vals <- c(180)

lc2000_1 <- lc2000
values(lc2000_1) <- 0
lc2000_1[lc2000 %in% cropland_vals] <- 1
lc2000_1[lc2000 %in% forest_vals] <- 2
lc2000_1[lc2000 %in% builtup_vals] <- 3
lc2000_1[lc2000 %in% water_vals] <- 4
lc2000_1[lc2000 %in% ice_vals] <- 5
lc2000_1[lc2000 %in% bare_vals] <- 6
lc2000_1[lc2000 %in% grass_vals] <- 7
lc2000_1[lc2000 %in% wetland_vals] <- 8

lc2010_1 <- lc2010
values(lc2010_1) <- 0
lc2010_1[lc2010 %in% cropland_vals] <- 1
lc2010_1[lc2010 %in% forest_vals]   <- 2
lc2010_1[lc2010 %in% builtup_vals]  <- 3
lc2010_1[lc2010 %in% water_vals]    <- 4
lc2010_1[lc2010 %in% ice_vals]      <- 5
lc2010_1[lc2010 %in% bare_vals]     <- 6
lc2010_1[lc2010 %in% grass_vals]    <- 7
lc2010_1[lc2010 %in% wetland_vals]  <- 8

lc2020_1 <- lc2020
values(lc2020_1) <- 0
lc2020_1[lc2020 %in% cropland_vals] <- 1
lc2020_1[lc2020 %in% forest_vals] <- 2
lc2020_1[lc2020 %in% builtup_vals] <- 3
lc2020_1[lc2020 %in% water_vals] <- 4
lc2020_1[lc2020 %in% ice_vals] <- 5
lc2020_1[lc2020 %in% bare_vals] <- 6
lc2020_1[lc2020 %in% grass_vals]  <- 7
lc2020_1[lc2020 %in% wetland_vals] <- 8


peru <- gadm(country = "PER", level = 0, path = tempdir())
peru <- project(peru, crs(lc2000_1))
lc2000_peru <- mask(crop(lc2000_1, peru), peru)
lc2010_peru <- mask(crop(lc2010_1,peru), peru)
lc2020_peru <- mask(crop(lc2020_1, peru), peru)

par(mfrow = c(2, 2), mar = c(4, 4, 4, 3))

color_table <- data.frame(
  values = 1:8,
  colors = c("gold", "forestgreen", "red", "blue",
             "lightcyan", "tan", "yellowgreen", "purple")
)

label_table <- data.frame(
  values = 1:8,
  labels = c("Cropland", "Forest", "Built-up", "Water",
             "Snow/Ice", "Bare area", "grassland", "Wetland")
)

png(here("Project_Image", "peru_lc_001020.png"), width = 3200, height = 2000, res = 250)
par(mfrow = c(2, 2), mar = c(4, 4, 4, 3))

plot(lc2000_peru,col = color_table %>%
       filter(values %in% unique(values(lc2000_peru))) %>%
    pull(colors),
  main = "Peru Land Cover 2000",
  legend = FALSE,
  axes = TRUE)
lines(peru, col = "black", lwd = 0.5)
legend("right", legend = leg, fill = cols, cex = 0.6, bg = "white", xpd = TRUE)

plot(lc2010_peru,col = color_table %>%
    filter(values %in% unique(values(lc2010_peru))) %>%
    pull(colors),
  main = "Peru Land Cover 2010",
  legend = FALSE,
  axes = TRUE)
lines(peru, col = "black", lwd = 0.5)
legend("right", legend = leg, fill = cols, cex = 0.6, bg = "white", xpd = TRUE)

plot(lc2020_peru, col = color_table %>%
    filter(values %in% unique(values(lc2020_peru))) %>%
    pull(colors),
  main = "Peru Land Cover 2020",
  legend = FALSE,
  axes = TRUE)
lines(peru, col = "black", lwd = 0.5)

legend("bottomright", legend = leg, fill = cols, cex = 0.6, bg = "white", xpd = TRUE)

dev.off()

