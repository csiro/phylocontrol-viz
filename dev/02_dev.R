# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
# attachment::att_amend_desc()

# usethis::use_package("pkg.you.want.to.add")
usethis::use_package("bslib",min_version = T)
usethis::use_package("shinyWidgets")
usethis::use_package("shinycssloaders")
usethis::use_package("shinyhelper")
usethis::use_package("shinyjs")
usethis::use_package("ggplot2")
usethis::use_package("plotly")
usethis::use_package("data.table")
usethis::use_package("leaflet")
usethis::use_package("leaflet.extras")
usethis::use_package("leaflet.extras2")
usethis::use_package("dplyr")
usethis::use_package("tools")
usethis::use_package("tidyr")
usethis::use_package("tidytree",min_version = T)
# usethis::use_package("viridisLite")
usethis::use_package("shinybusy")
usethis::use_package("ggtree",min_version = T)
usethis::use_package("ape")
usethis::use_package("scico")
# usethis::use_package("RColorBrewer")
usethis::use_package("DT")
usethis::use_package("castor")
# usethis::use_package("countrycode")
# usethis::use_package("sp")
usethis::use_package("htmltools")
usethis::use_package("raster")
usethis::use_package("sf")
usethis::use_package("terra")
usethis::use_package("rworldmap")
usethis::use_package("rintrojs")

## Add modules ----
## Create a module infrastructure in R/
# golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_handler("handlers")
golem::add_css_file("framework-style")
golem::add_css_file("package-style")
golem::add_css_file("custom-style")
# golem::add_sass_file("custom")
# golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "demo", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation

## Vignette ----
# usethis::use_vignette("phylocontrol.viz")
# devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
# usethis::use_github()

# # GitHub Actions
# usethis::use_github_action()
# # Chose one of the three
# # See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# # Add action for PR
# usethis::use_github_action_pr_commands()

# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()
#
# # Jenkins
# usethis::use_jenkins()
#
# # GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
