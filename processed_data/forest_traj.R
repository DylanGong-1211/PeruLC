library(terra)
library(here)
library(timeseriesTrajectories)
library(dplyr)
library(tidyterra)
lc_stack <- rast(here("processed_data", "lc_stack_2000_2020.tif"))
years <- 2000:2020
tar_years <- c(2000, 2005, 2010, 2015, 2020)
tar_index <- which(years %in% tar_years)

forest_layers <- lapply(tar_index, function(i) {
  x <- lc_stack[[i]]
  forest_2bi <- ifel(x == 2, 1, 0)
  names(forest_2bi) <- names(x)
  forest_2bi
})

forest_stack <- rast(forest_layers)

forest_cell <- terra::cellSize(forest_stack[[1]], unit = "km")
forest_area_layers <- lapply(1:nlyr(forest_stack), function(i) {
x <- forest_stack[[i]] * forest_cell
names(x) <- names(forest_stack[[i]])
x
})
forest_area_stack <- rast(forest_area_layers)

years5 <- c(2000, 2005, 2010, 2015, 2020)
dataPreview(
  forest_area_stack,
  timepoints = years5,
  vertunits = "km2",
  xAngle = 0
)

forest_presence <- presenceData(forest_stack, nodata = -999)
presencePlot(forest_presence,
             pltunit = "m",
             dataEpsg = 4326,
             scalePos = "bottomright",
             narrowPos = "topleft",
             narrowSize = 1,
             categoryName = "forest",
             xAxis = "Longitude (m)",
             yAxis = "Latitude (m)",
             axisText = 1,
             axisLabel = 1,
             plotTitle = 1.2)

plot(forest_presence[[1]], )

traj_data <- rastertrajData(forest_stack,
                            zeroabsence = 'yes')

