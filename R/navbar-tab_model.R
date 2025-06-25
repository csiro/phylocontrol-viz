# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tab Model ----
# (sourced in global.R)
#
# Description: The user interface for the body of tab. Visualisation of SDMs.
#
# Author: Louise Ord, Lauren Stevens, Stephanie Chen
# Date: 2021-03-03
#
# Last Modified by: Lauren Stevens
# Date: 2025-01-28
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# 1. Tab ----
# ~~~~~~~~~~~~~
navbar_tab_model <- function(input,output,session,vals) {

  output$navbar_tab_model <- shiny::renderUI({
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        id = 'model_sidebar',
        width = '450px',
        bg = 'white',

        div(id='model_options',
            shinyWidgets::pickerInput(
              inputId  = 'model_method',
              label    = HTML(paste0('Method')),
              choices  = list('MaxEnt' = 'maxent',
                              'CLIMATCH' = 'climatch'),
              multiple = FALSE
            ),

            shinyWidgets::pickerInput(
              inputId  = 'model_timespan',
              label    = HTML(paste0('Timespan')),
              choices  = list('Current' = 'chelsa2.1_bio'),
              multiple = FALSE
            ),

            shinyWidgets::pickerInput(
              inputId  = 'model_species',
              label    = HTML(paste0('Species')),
              choices  = vals$treeChoices,
              # selected = input$species_map1,
              selected = vals$targetSpecies,
              options  = shinyWidgets::pickerOptions(liveSearch = TRUE),
              multiple = FALSE
            )
        )
      ),

      shiny::h4("Species distribution models"),
      p("Select study group and target species from 'Phylogeny' tab."),
      uiOutput('note_demo_model'),
      leaflet::leafletOutput('model', height = '550px')
    )
  })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
