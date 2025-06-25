# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The user interface ----
#
# Description:
#  The UI controls the layout and appearance of the application
#  in the user's browser window.
#   1. Fluid page layout
#   2. Custom styling and JavaScript
#   3. Output dynamic UI
#
# Author: Louise Ord, Lauren Stevens
# Date: 2019-02-02
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    # ~~~~~~~~~~~~~~~~~~~~~~~~~
    # 1. Fluid page layout ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    bslib::page(

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2. Custom styling and JavaScript ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tagList(

        # 2.7. Add busy spinner ----
        shinybusy::add_busy_spinner(spin = "fading-circle",
                         color = cblues[1],
                         timeout = 200,
                         # position = 'bottom-left',
                         position = "full-page",
                         onstart=T),

        shinybusy::use_busy_spinner(spin = "fading-circle",
                         color = cblues[1],
                         # position = 'bottom-left',
                         position = "full-page",
                         margins = c(10, 10),
                         spin_id = NULL,
                         height = "50px",
                         width = "50px")

      ),

      # 2.8. Access custom shiny package options ----
      # Allows common JS operations to be accessed through Shiny with shinyjs
      shinyjs::useShinyjs(),

      # Initiate rintrojs
      rintrojs::introjsUI(),

      # ~~~~~~~~~~~~~~~~~~~~~~~~~
      # 3. Output dynamic UI ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~
      # The dynamic UI is rendered in reactive-ui.R which is sourced in server.R.
      uiOutput("ui")

    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "img",
    app_sys("app/img")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "phylocontrol.viz"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

    # Custom styling CSS for modal ----
    tags$link(rel = "stylesheet", type = "text/css",
              href = "framework-style.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "package-style.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "custom-style.css"),

    # Google fonts ----
    tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans",
              rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat",
              rel="stylesheet")
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
