#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create demo data ----
#
#
#
# Author(s): Lauren Stevens
# Date: 2025-01-29
#
# Last Modified by: Lauren Stevens
# Date: 2025-01-29
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note 'Restart R' before is important o/w inconsistent behaviour may appear
#
# Large RasterLayers are not loaded into memory, but contain a pointer to a
# file storing the data. save() then only stores a pointer to the temp file
# holding the data which may not be available later.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Library
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(ape)
library(utils)
library(terra)
library(raster)
library(methods)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_path <- '/datasets/work/nrca-phylocontrol/work/App_demo_data/Erigeron/'
# data_path <- 'path.to.data'

if (!dir.exists(data_path)) stop("Directory doesn't exist!")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species and target
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_names      <- "Erigeron"
species_vec        <- 1
names(species_vec) <- "Erigeron"
targetSpecies      <- "Erigeron bonariensis"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tree
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_tree          <- paste0(data_path,"Erigeron.tre")
demo_tree          <- ape::read.tree(file_tree)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Traits
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_traits        <- paste0(data_path,"Erigeron_traits.csv")
demo_traits        <- data.table::fread(file_traits)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Occurrence Map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geo_file           <- paste0(data_path,"Erigeron_occurrences.csv")
demo_geo_data      <- data.table::fread(geo_file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SD Models - target only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Maxent
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maxent_path <- paste0(data_path,
                      "sdm/maxent_results/chelsa2.1_bio/",
                      gsub(" ","_",targetSpecies),
                      "/")
maxent_model_file <- paste0(maxent_path, "maxent_predict.grd")
demo_maxent_grd   <- terra::rast(maxent_model_file)

demo_maxent_grd <- terra::project(demo_maxent_grd, "EPSG:4326")
demo_maxent_grd <- raster::raster(demo_maxent_grd)

# Crop raster
disp_win_wgs84 <- sf::st_sfc(sf::st_point(c(-180, -89)),
                             sf::st_point(c(180, 89)),
                             crs = 4326)
# Get the bounding box
bb <- sf::st_bbox(disp_win_wgs84)
# crop the rasters to the extent
cropped_maxent_result_grd <- terra::crop(demo_maxent_grd, bb)

# # Convert to rasterLayer before saving
# # https://github.com/rspatial/terra/issues/549
# # https://tmieno2.github.io/R-as-GIS-for-Economists/convert-to-rb.html
# demo_maxent     <- methods::as(demo_maxent_grd, "Raster")
# # https://cran.r-project.org/web/packages/terra/terra.pdf
# demo_maxent <- raster::raster(demo_maxent_grd)

# put in memory
# demo_maxent <- demo_maxent*1
demo_maxent <- cropped_maxent_result_grd*1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Thresholds
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
thresholds_file <- paste0(maxent_path, "maxent_thresholds.csv")
demo_thresholds <- utils::read.csv(thresholds_file, header = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Climatch
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_path   <- paste0(data_path,
                      "sdm/climatch_results/chelsa2.1_bio/",
                      gsub(" ","_",targetSpecies),
                      "/")
clim_model_file <- paste0(clim_path, "climatch_predict.grd")
demo_clim_grd   <- terra::rast(clim_model_file)

demo_clim_grd <- terra::project(demo_clim_grd, "EPSG:4326")
demo_clim_grd <- raster::raster(demo_clim_grd)

# Crop raster
disp_win_wgs84 <- sf::st_sfc(sf::st_point(c(-180, -89)),
                             sf::st_point(c(180, 89)),
                             crs = 4326)
#Get the bounding box
bb <- sf::st_bbox(disp_win_wgs84)
#crop the rasters to the extent
cropped_clim_result_grd <- terra::crop(demo_clim_grd, bb)

# # Convert to rasterLayer before saving
# # https://github.com/rspatial/terra/issues/549
# # https://tmieno2.github.io/R-as-GIS-for-Economists/convert-to-rb.html
# demo_clim       <- methods::as(demo_clim_grd  , "Raster")
# # https://cran.r-project.org/web/packages/terra/terra.pdf
# demo_clim   <- raster::raster(demo_clim_grd)

# put in memory
# demo_clim   <- demo_clim*1
demo_clim <- cropped_clim_result_grd*1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving in RData format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save(species_names,
#      species_vec,
#      targetSpecies,
#      demo_tree,
#      demo_traits,
#      demo_geo_data,
#      demo_thresholds,
#      demo_maxent,#_grd,
#      demo_clim,#_grd,
#      file = "data-raw/demo.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To load the data again ?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load("data-raw/demo.RData")
# # Convert back to SpatRaster (in app as well)
# maxent_result_grd <- methods::as(demo_maxent,'SpatRaster')
# clim_result_grd   <- methods::as(demo_clim  ,'SpatRaster')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
