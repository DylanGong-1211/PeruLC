library(terra)
library(geodata)
library(here)
library(dplyr)
library(tidyr)

# order and extract raw data
order_files <- c(sprintf("ESACCI-LC-L4-LCCS-Map-300m-P1Y-%s-v2.0.7cds.area-subset.0.-68.-19.-82.nc", 2000:2015),
sprintf("C3S-LC-L4-LCCS-Map-300m-P1Y-%s-v2.1.1.area-subset.0.-68.-19.-82.nc", 2016:2020))
all_files <- here("extdata", order_files)



lc_reclassify <- function(lyr){
  # Define new classes
  cropland_vals <- c(10, 11, 12, 20, 30, 40)
  forest_vals <- c(50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90,160,170)
  builtup_vals <- c(190)
  water_vals <- c(210)
  ice_vals <- c(220)
  bare_vals <- c(200, 201, 202)
  grass_vals <- c(100, 110, 120, 121, 122, 130, 150, 151, 152, 153)
  wetland_vals <- c(180)

  # Reclassify
  new_lyr <- lyr
  values(new_lyr) <- 0
  new_lyr[lyr %in% cropland_vals] <- 1
  new_lyr[lyr %in% forest_vals] <- 2
  new_lyr[lyr %in% builtup_vals] <- 3
  new_lyr[lyr %in% water_vals] <- 4
  new_lyr[lyr %in% ice_vals] <- 5
  new_lyr[lyr %in% bare_vals] <- 6
  new_lyr[lyr %in% grass_vals] <- 7
  new_lyr[lyr %in% wetland_vals] <- 8

  return(new_lyr)
}

# setting plot color
color_table <- data.frame(
  values = 1:8,
  colors = c("gold", "forestgreen", "red", "blue",
             "lightcyan", "tan", "yellowgreen", "purple")
)

#setting lable name
label_table <- data.frame(
  values = 1:8,
  labels = c("Cropland", "Forest", "Built-up", "Water",
             "Snow/Ice", "Bare area", "grassland", "Wetland")
)



peru <- gadm(country = "PER", level = 0, path = tempdir())


years <- 2000:2020

# Make sure the lccs layer has same projection as Peru
con_layer <- rast(all_files[1])[["lccs_class"]]
peru <- project(peru, crs(con_layer))


png(here("Project_Image", "peru_lc_2000_2020.png"), width = 6000, height = 4500, res = 300)
par(mfrow = c(4, 6), mar = c(4, 4, 6, 3), oma = c(0, 0, 0, 8))
for (i in 1:length(all_files)) {
  lc_rast <- rast(all_files[i])[["lccs_class"]]
  lc_reclass <- lc_reclassify(lc_rast)
  lc_peru <- mask(crop(lc_reclass, peru), peru)
  present_vals <- sort(unique(values(lc_peru)[values(lc_peru) > 0]))
  plot_colors <- color_table %>% filter(values %in% present_vals) %>% pull(colors)
  plot(lc_peru,
       col = plot_colors,
       main = paste("Peru Land Cover", years[i]),
       legend = FALSE,
       axes = TRUE,
       cex.main = 1.8)
  lines(peru, col = "black", lwd = 1)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottomright",
       legend = label_table$labels,
       fill = color_table$colors,
       cex = 2.5,
       bg = "white",
       xpd = TRUE,
       inset = c(0, 0))
dev.off()



