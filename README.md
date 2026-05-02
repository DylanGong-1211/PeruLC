# Peru Land Cover Change Analysis (2000–2020)

This project analyzes land cover change in Peru from 2000 to 2020 using annual ESA CCI / C3S land cover data from the Copernicus Climate Data Store. The analysis focuses on three selected land cover types: **forest**, **cropland**, and **built-up land**. In addition to overall area change, the project applies trajectory-based methods to examine how these categories changed through time and across space.

The project combines raster preprocessing, reclassification, time-series area analysis, spatial trajectory mapping, and stacked bar trajectory plots. The goal is to move beyond simple net change and better understand the patterns, timing, and spatial structure of land cover change in Peru.

## Research Question

**How did forest, cropland, and built-up land in Peru change from 2000 to 2020, and did they follow different patterns of change?**

## Data

- **Source:** Copernicus Climate Data Store (CDS)
- **Website:** `https://cds.climate.copernicus.eu/`
- **Products:** ESA CCI / C3S annual land cover data
- **Time span:** 2000–2020
- **Spatial resolution:** 300 m
- **Layer used:** `lccs_class`
- **Study area:** Peru

The original land cover product contains many detailed classes. For this project, the original classes were reclassified into 8 broader categories to make the results easier to interpret and compare across years.

### Reclassified Categories

1. Cropland  
2. Forest  
3. Built-up  
4. Water  
5. Snow/Ice  
6. Bare area  
7. Grassland  
8. Wetland  

## Methods

The workflow consists of the following main steps:

### 1. Prepare annual land cover data
- Read annual NetCDF land cover files from 2000 to 2020
- Extract the `lccs_class` layer
- Crop and mask all rasters to Peru

### 2. Reclassify land cover classes
- Group the original detailed land cover codes into 8 broader categories

### 3. Build the land cover stack
- Combine all annual layers into a single raster stack (`lc_stack`)

### 4. Visualize annual land cover change
- Create an animated map of Peru’s land cover change from 2000 to 2020

### 5. Prepare category-specific inputs
- Convert forest, cropland, and built-up land into binary rasters
- Convert binary rasters to area rasters

### 6. Apply trajectory analysis
Using functions from the `timeseriesTrajectories` package:
- `plot_timeseries()` for time-series area plots
- `plot_trajectory()` for spatial trajectory maps
- `plot_stackbar()` for stacked bar trajectory plots

## Main Outputs

This repository includes the following project outputs:

- Animated land cover map of Peru
- Time-series plots of forest, cropland, and built-up area
- Spatial trajectory maps
- Stacked bar trajectory plots
- Presentation slides summarizing the analysis

## Key Findings

- **Forest and cropland followed different temporal trajectories.**
- **Forest lost more than it gained** over the full study period.
- **Cropland gained more than it lost** over the full study period.
- Forest showed **strong early loss** and **later partial recovery**.
- Cropland showed **strong early expansion**, especially in 2000–2005.
- In both categories, **one-way gain/loss** was much larger than alternation.
- Some forest-loss areas appear to align with cropland-gain areas, suggesting possible localized **forest-to-cropland conversion**.

## Repository Structure

```text
.
├── extdata/             # Raw annual land cover files
├── processed_data/      # Processed raster outputs (e.g., lc_stack)
├── Project_Image/       # Exported plots, maps, and GIFs
├── R/                   # Analysis and plotting scripts
├── slides/              # Presentation files
└── README.md            # Project overview
