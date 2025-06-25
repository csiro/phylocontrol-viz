## code to prepare `demo` dataset goes here

source('data-raw/create_demo_data.R')
# load('data-raw/demo.RData')

# # External
# usethis::use_data(species_names, overwrite = TRUE)
# usethis::use_data(species_vec  , overwrite = TRUE)
# usethis::use_data(targetSpecies, overwrite = TRUE)
# usethis::use_data(demo_tree    , overwrite = TRUE)
# usethis::use_data(demo_traits  , overwrite = TRUE)
# usethis::use_data(demo_geo_data, overwrite = TRUE)

# or Internal in single R/sysdata.rda file
usethis::use_data(species_names,
                  species_vec,
                  targetSpecies,
                  demo_tree,
                  demo_traits,
                  demo_geo_data,
                  demo_thresholds,
                  demo_maxent,#_grd,
                  demo_clim,#_grd,
                  overwrite = TRUE, internal = TRUE)
