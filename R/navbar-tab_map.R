# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tab Map ----
# (sourced in global.R)
#
# Description: The user interface for the body of tab. Occurrence map.
#
# Author: Louise Ord, Lauren Stevens
# Date: 2021-03-03
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# 1. Tab ----
# ~~~~~~~~~~~~~

navbar_tab_map <- function(input,output,session,species_vec,vals) {

output$navbar_tab_map <- shiny::renderUI({
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      id = 'map_sidebar',
      width = '450px',
      bg = 'white',
      div(id='map_options',
          shinyWidgets::pickerInput(inputId  = 'country',
                                    label    = 'Country',
                                    choices  = vals$country_list,
                                    selected = 'Australia',
                                    options  = shinyWidgets::pickerOptions(
                                      actionsBox = T,
                                      selectedTextFormat = 'count > 3'),
                                    multiple = T),
          shinyWidgets::pickerInput(inputId  = 'species_map1',
                                    label    = HTML(paste0('Species 1 (',
                                                           span("Teal",
                                                                style = "color: #007377;
                                                                         font-weight: bold;"),')')),
                                    choices  = vals$treeChoices,
                                    selected = vals$targetSpecies,
                                    options  = shinyWidgets::pickerOptions(
                                      liveSearch = T),
                                    multiple = F),
          shinyWidgets::pickerInput(inputId  = 'species_map2',
                                    label    = HTML(paste0('Species 2 (',
                                                           span("Purple",
                                                                style = "color: #6D2077;
                                                                         font-weight: bold;"),')')),
                                    choices  = vals$treeChoices,
                                    selected = vals$treeChoices[1],
                                    options  = shinyWidgets::pickerOptions(
                                      liveSearch = T,
                                      dropupAuto = F),
                                    multiple = F)
      )
    ),
    shiny::h4(paste(names(species_vec)[as.numeric(input$species)],
                    'occurrences map')),
    p("Select study group and target species from 'Phylogeny' tab. Visualise
      the locality of preserved specimens using data from GBIF for two species
      at a time."),
    ## Map ----
    leaflet::leafletOutput('map',height = '550px')
  )
})
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
