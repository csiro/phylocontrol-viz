#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Deploy the ShinyApp (Do not remove this comment)
#
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set path to data to use own data or run demo=T
dir_path <- "path.to.folder"

# Do you want to run the demo?
run_demo <- TRUE
# run_demo <- FALSE

# To deploy ----
pkgload::load_all(path = ".",export_all = FALSE)
if (dir_path!='path.to.folder') {
  run_app(dir_path = dir_path, demo=run_demo)
} else {
  print('Warning: currently running the Demo.
        Please update `dir_path` in app.R
        if you want to use your own data.')
  run_app(demo=T)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
