# From Lei Song

library(timeseriesTrajectories)
library(ggplot2)
library(terra)
library(here)
library(dplyr)
library(tidyterra)
library(pdftools)

# I just read the file that you shared with me.
lc_stack <- rast(here("processed_data", "lc_stack_2000_2020.tif"))


##### Use forest as an example land cover type #####
# Make sure you do for other types. Cropland could be an interesting one.

# 1. Data prepare
## Convert to binary
forest_binary <- lc_stack == 2
## Convert to actual area, not pixel size
forest_area <- forest_binary * cellSize(forest_binary[[1]], unit = "km")

## Define some common parameters
years <- 2000:2020
vert_units <- "Km2"

# 2. Check time series change in areas
# Use this following code to replace dataPreview
png(
  filename = here("Project_Image", "forest_timeseries_change_area.png"),
  width = 2400,
  height = 1800,
  res = 300
)

plot_timeseries(
  forest_area, timepoints = years, vertunits = vert_units, xAngle = 0) +
  scale_y_continuous(limits = c(800000, 820000))+
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_text(angle = 90),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"))

dev.off()


# 3. Check the overall change conditions in the time series
## Summarize the presence data
num_pres_change <- presenceData(forest_binary, nodata = -999)

## Plot it. Just use the following code, do not use presencePlot
png(filename = here("Project_Image", "forest_number_of_presences.png"),
  width = 2100,
  height = 1800,
  res = 300
)
ggplot() +
  geom_spatraster(
    data = num_pres_change$`Data for number of presence`) +
  scale_fill_viridis_c(
    name = "Number of\npresences",
    option = "C",
    na.value = "transparent") +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right")

max_val <- global(num_pres_change$`Data for number of changes`,
                  "max", na.rm = TRUE)[[1]]
dev.off()

ggplot() +
  geom_spatraster(data = num_pres_change$`Data for number of changes`) +
  scale_fill_distiller(
    name = "Number of\nchanges",
    palette = "YlOrRd",
    direction = 1,
    na.value = "transparent",
    breaks = 0:max_val,
    labels = 0:max_val) +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right")

# 4. Check change trajectories
## Get the tranjectories
traj_data <- rastertrajData(forest_area, zeroabsence = 'yes')

# Use plot_trajectory instead of trajPlot. And you could customize everything
# after the function.
png(
  filename = here("Project_Image", "forest_traj_plot.png"),
  width = 2400,
  height = 1800,
  res = 300
)
plot_trajectory(traj_data) +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right")
dev.off()

png(
  filename = here("Project_Image", "forest_plots", "forest_traj_plot_mask.png"),
  width = 2400,
  height = 1800,
  res = 300
)
plot_trajectory(traj_data) +
  coord_sf(xlim = c(-76, -72), ylim = c(-12, -8), expand = FALSE)+
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right")
dev.off()



# 5. Stack everything
## Here you could just subset a few years to check.
forest_yr_check <- subset(forest_binary, c(1, 6, 11, 16, 21))
tps <- c(2000, 2005, 2010, 2015, 2020)

## Make sure the layers and timePoints match
stackbar_data <- rasterstackData(
  x = forest_yr_check, timePoints = tps)

## Plot it. Use plot_stackbar instead of stackbarPlot

pdf(file = here("Project_Image","forest_traj_plots", "stackbar_plot.pdf"), width = 10, height = 6)

 plot_stackbar(stackbar_data,
             axisSize = 10,
             lbAxSize = 10,
             lgSize = 7.5,
             titleSize = 12,
             datbreaks = "no",
             upperlym = 35,
             lowerlym = - 50,
             lymby = 5,
             upperlym2 = 0.5,
             lymby2 = 0.1,
             xAngle = 0)

dev.off()

pdf_convert(
  pdf = here("Project_Image", "stackbar_plot.pdf"),
  format = "png",
  filenames = c(
    here("Project_Image", "forest_stackbar_plot_1.png"),
    here("Project_Image", "forest_stackbar_plot_2.png")
  )
)
