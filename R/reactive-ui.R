# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  A reactive user interface (sourced in server.R) ----
#
# Description:
#   This reactive user interface uses bslib.
#
# Author: Louise Ord, Lauren Stevens, Stephanie Chen
# Date: 2023-03-06
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

reactive_ui <- function(input, output, session) {

output$ui <- renderUI({

  bslib::page_navbar(id = 'nav',

                     window_title = 'PhyloControl',
                     title        = 'PhyloControl',

                     # title = tags$img(src = "img/graphic_white.png",width='10%'),

                     theme = bslib::bs_theme(version = 5),
                     # theme = bslib::bs_theme(version = 5,bootswatch = "flatly"),
                     # theme = bslib::bs_theme(version = 5,bootswatch = "sandstone"),

                     # navbar_options = bslib::navbar_options( # bslib (>= 0.9.0)
                     # header colour
                     bg      = "#142C3F",
                     underline = FALSE,
                     padding = '0px', #),

                     # initial landing page
                     selected = 'Home',

                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     # Tabs/Panels
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     bslib::nav_panel("Home",
                                      icon = icon("fas fa-home"),
                                      shiny::uiOutput('navbar_tab_home')
                     ),

                     bslib::nav_panel("Phylogeny",
                                      icon = icon("fas fa-seedling"),
                                      shiny::uiOutput('navbar_tab_phylogeny')
                     ),

                     bslib::nav_panel("Map",
                                      icon = icon("fas fa-map-location-dot"),
                                      shiny::uiOutput('navbar_tab_map')
                     ),

                     bslib::nav_panel("Model",
                                      icon = icon("fas fa-layer-group"),
                                      shiny::uiOutput('navbar_tab_model')
                     ),

                     bslib::nav_panel("Report",
                                      icon = icon('fas fa-newspaper'),
                                      shiny::uiOutput('navbar_tab_report')
                     ),

                     # bslib::nav_panel("About",
                     #                  icon = icon("fas fa-info-circle"),
                     #                  shiny::uiOutput('navbar_tab_about')
                     # ),

                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     # Help
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     bslib::nav_item(
                       shiny::actionButton(inputId = 'help',
                                           label   = "Help",
                                           style   = 'background-color: #ffffff;',
                                           icon    = shiny::icon('fas fa-circle-question'))
                     ),

                     bslib::nav_spacer(),

                     bslib::nav_menu(
                       title = "",
                       align = 'right',
                       icon = shiny::icon('link'),
                       bslib::nav_item(link_repo),
                       bslib::nav_item(link_demo)
                     )

  ) # bslib::page_navbar

})

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
