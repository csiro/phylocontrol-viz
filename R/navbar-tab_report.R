# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tab Report ----
# (sourced in global.R)
#
# Description: The user interface for the body of tab. Export of table.
#
# Author: Louise Ord, Lauren Stevens
# Date: 2021-03-03
#
# Last Modified by: Stephanie Chen
# Date: 2024-09-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# 1. Tab ----
# ~~~~~~~~~~~~~

navbar_tab_report <- function(input,output,session) {

  output$navbar_tab_report <- shiny::renderUI({
    shiny::fluidPage(
      br(),
      shiny::h4("List of species with distance measures"),
      br(),
      DT::dataTableOutput('report_table')
    )
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
