library(timeseriesTrajectories)
library(ggplot2)
library(terra)
library(here)
library(dplyr)
library(tidyterra)
library(pdftools)


lc_stack <- rast(here("processed_data", "lc_stack_2000_2020.tif"))

# 1. Data preparation
build_ups_binary <- lc_stack == 3
build_ups_area <- build_ups_binary * cellSize(build_ups_binary[[1]], unit = "km")
build_vals <- global(build_ups_area, "sum", na.rm = TRUE)[,1]
range(build_vals)


# 2. Check time series change in area
years <- 2000:2020
vert_units <- "Km2"


timeseries_build <- plot_timeseries(
  build_ups_area, timepoints = years, vertunits = vert_units, xAngle = 0) +
  scale_y_continuous(limits = c(0, max(build_vals) * 1.05))+
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

dir.create(here("Project_Image", "build_plots"), recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = here("Project_Image", "build_plots", "build_timeseries_area.png"),
  plot = timeseries_build,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 3. Check overall change conditions in the time series
num_pres_change_build <- presenceData(build_ups_area, nodata = -999)

p_presence_build <- ggplot() +
  geom_spatraster(
    data = num_pres_change_build$`Data for number of presence`
  ) +
  scale_fill_viridis_c(
    name = "Number of\npresences",
    option = "C",
    na.value = "transparent"
  ) +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )


ggsave(
  filename = here("Project_Image","build_plots", "build_number_of_presences.png"),
  plot = p_presence_build,
  width = 7,
  height = 6,
  dpi = 300,
  bg = "white"
)

## Plot number of changes
max_val_build <- global(
  num_pres_change_build$`Data for number of changes`,
  "max",
  na.rm = TRUE
)[[1]]

p_change_build <- ggplot() +
  geom_spatraster(
    data = num_pres_change_build$`Data for number of changes`
  ) +
  scale_fill_distiller(
    name = "Number of\nchanges",
    palette = "YlOrRd",
    direction = 1,
    na.value = "transparent",
    breaks = 0:max_val_crop,
    labels = 0:max_val_crop
  ) +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(
  filename = here("Project_Image","build_plots", "build_number_of_changes.png"),
  plot = p_change_build,
  width = 7,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 4. Check change trajectories
## Get the tranjectories
traj_data_build <- rastertrajData(build_ups_area, zeroabsence = "yes")

png(
  filename = here("Project_Image","build_plots", "build_traj_plot.png"),
  width = 2400,
  height = 1800,
  res = 300
)

plot_trajectory(traj_data_build) +
  theme_void(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right")

dev.off()

# 5. Stackbar plot using 5 selected years
build_yr_check <- subset(build_ups_binary, c(1, 6, 11, 16, 21))
tps <- c(2000, 2005, 2010, 2015, 2020)

stackbar_data_build <- rasterstackData(
  x = build_yr_check,
  timePoints = tps
)

pdf(file = here("Project_Image", "build_plots", "stackbar_plot.pdf"),
    width = 10, height = 6)

plot_stackbar(
  stackbar_data_build,
  axisSize = 10,
  lbAxSize = 10,
  lgSize = 7.5,
  titleSize = 12,
  datbreaks = "no",
  upperlym = 35,
  lowerlym = -50,
  lymby = 5,
  upperlym2 = 0.5,
  lymby2 = 0.1,
  xAngle = 0
)

dev.off()

pdf_convert(
  pdf = here("Project_Image", "build_plots", "stackbar_plot.pdf"),
  format = "png",
  filenames = c(
    here("Project_Image", "build_plots", "build_stackbar_plot_1.png"),
    here("Project_Image", "build_plots", "build_stackbar_plot_2.png")
  )
)

