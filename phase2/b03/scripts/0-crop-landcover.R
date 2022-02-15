# a254
# Bronwyn Rayfield and Jed Lloren, ApexRMS
# Run with R-4.1.2

# Workspace---------------------------------------------------------------------------------------------
# Load packages
library(raster)
library(sf)
library(tidyverse)

# Set up directories
# Assumes these directories already exist
rawMapsDir <- "../data/spatial"


# Read in data
# Spatial
b03Raw <- raster(file.path(rawMapsDir,"b03-studyarea.tif"))
landcover <- raster(file.path(rawMapsDir,"b03_landcover_30m.tif"))

# Refomatting data---------------------------------------------------------------------------------------------
# Resample the study area raster
# This changes the b03 raster from a 90x90 resolution to a 30x30 resolution
b03Resampled <- resample(b03Raw, landcover, method = "ngb")

# Reclassify the b03 raster to remove 0 values
matrix <- matrix(c(0, 1, NA, 1), nrow = 2, ncol = 2)
b03Reclass <- reclassify(b03Resampled, matrix)

# Crop the landcover raster to the modified study area raster
b03Landcover <- landcover %>%
  crop(b03Reclass) %>%
  mask(b03Reclass)

# Save outputs---------------------------------------------------------------------------------------------
# Modified b03 study area with new resolution
writeRaster(b03Reclass, 
            file.path(rawMapsDir, "b03-studyarea.tif"), 
            overwrite = TRUE)
# Cropped landcover raster
writeRaster(b03Landcover, 
            file.path(rawMapsDir, "b03-landcover.tif"), 
            overwrite = TRUE)
