library(terra)
library(geodata)
library(here)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)


# order and extract raw data
order_files <- c(
  sprintf("ESACCI-LC-L4-LCCS-Map-300m-P1Y-%s-v2.0.7cds.area-subset.0.-68.-19.-82.nc", 2000:2015),
  sprintf("C3S-LC-L4-LCCS-Map-300m-P1Y-%s-v2.1.1.area-subset.0.-68.-19.-82.nc", 2016:2020)
)
all_files <- here("extdata", order_files)
years <- 2000:2020

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

# Peru boundary
peru <- gadm(country = "PER", level = 0, path = tempdir())
con_layer <- rast(all_files[1])[["lccs_class"]]
peru <- project(peru, crs(con_layer))
peru_sf <- st_as_sf(peru)

all_layers <- lapply(1:length(all_files), function(i) {lc_rast <- rast(all_files[i])[["lccs_class"]]
  lc_reclass <- lc_reclassify(lc_rast)
  lc_peru <- mask(crop(lc_reclass, peru), peru)
  levels(lc_peru) <- data.frame(value = 1:8,
                                label = c("Cropland", "Forest", "Built-up", "Water","Snow/Ice",
                                          "Bare area", "Grassland", "Wetland"))
  names(lc_peru) <- paste0("year of", years[i])
  lc_peru
})
lc_stack <- rast(all_layers)

writeRaster(
  lc_stack,
  here("processed_data", "lc_stack_2000_2020.tif"),
  overwrite = TRUE
)
