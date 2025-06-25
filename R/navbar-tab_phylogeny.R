# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tab Phylogeny ----
# (sourced in global.R)
#
# Description: The user interface for the body of tab. Tree and traits.
#
# Author(s): Louise Ord, Lauren Stevens
# Date: 2021-03-03
#
# Last Modified by: Lauren Stevens
# Date: 2025-01-20
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# 1. Tab ----
# ~~~~~~~~~~~~~
navbar_tab_phylogeny <- function(input,output,session,species_vec) {

  output$navbar_tab_phylogeny <- renderUI({

    bslib::page_sidebar(
      ## First sidebar ----
      sidebar = bslib::sidebar(
        id='phylogeny_sidebar',
        bg = 'white',
        width = '450px',
        # class = "p-0",
        bslib::accordion(
          id= 'acc',
          open = c('Taxonomy','Phylogenetic Tree'),
          multiple = T,
          bslib::accordion_panel(
            title = 'Taxonomy',
            div(id='study_options',
                # h4('Taxonomy'),
                selectInput(inputId  = 'species',
                            label    = 'Study group',
                            choices  = species_vec,
                            selected = 1,
                            multiple = F),
                uiOutput('ui_reactive')) |>
              shinyhelper::helper(
                title = 'Taxonomy options',
                type = 'inline',
                buttonLabel = 'Close',
                content = c(
                  'Select a taxonomic group and
                  a species to highlight.
                  <br>
                  This target species will show up
                  as a bright pink in the tree.'),
                size = 'm')
          ),
          bslib::accordion_panel(
            title = 'Phylogenetic Tree',
            div(id='tree_options',
                # h4('Phylogenetic tree'),
                p('Click a node on the phylogeny to:'),
                shinyWidgets::actionGroupButtons(
                  inputIds = c('reroot',
                               'collapse',
                               'reset'),
                  labels   = c('Re-Root',
                               'Collapse/Expand',
                               'Reset'),
                  size = 'normal',
                  fullwidth = FALSE),
                br(),br()
            ) |>
              shinyhelper::helper(
                title   = 'Phylogeny Interaction',
                type    = 'inline',
                buttonLabel = 'Close',
                content = paste0('To interact with the phylogeny: <br>
                                 - Click the node on the tree and select `Re-Root` to
                                  change where the tree is rooted from.
                                  The current root will show up as a light green square,
                                  whereas the selected root to re-root on will appear as
                                  a light blue diamond. <br>
                                 - Or `Collapse/Expand` tree at that node <br>
                                 - Click the tree, hold and drag to go up/down <br>
                                 - Use scroll to zoom in/out <br>
                                 - Or use the icons in the top right: <br><br>',
                                 shiny::icon('fas fa-arrows-up-down-left-right'), ' pan <br>',
                                 shiny::icon('fas fa-magnifying-glass'), ' zoom on area selected <br>',
                                 shiny::icon('fas fa-square-plus'), ' zoom in <br>',
                                 shiny::icon('fas fa-square-minus'), ' zoom out <br>',
                                 shiny::icon('fas fa-house-chimney'), ' reset axes <br>',
                                 shiny::icon('fas fa-camera'), ' download plot as png <br><br>
                                           - `Reset` to go back to the orignal rooted tree.'),
                size = 'm'),
            div(id='layout_options',
                h6('Layout'),
                # h4(HTML(paste0('&nbsp;&nbsp;&nbsp;','Layout'))),
                shinyWidgets::prettyCheckbox(inputId = 'ladderize',
                                             label   = 'Ladderize',
                                             shape   = 'curve',
                                             bigger  = F,
                                             value   = T),
                shiny::conditionalPanel(
                  'input.ladderize',
                  shinyWidgets::prettyCheckbox(inputId = 'right',
                                               label   = 'Smallest clade on right',
                                               shape   = 'curve',
                                               bigger  = F,
                                               value   = T)),
                # default branch.length=F, T='none'
                shinyWidgets::prettyCheckbox(inputId = 'brLength',
                                             label   = 'Cladogram',
                                             shape   = 'curve',
                                             bigger  = F,
                                             value   = F),
                shinyWidgets::prettyCheckbox(inputId = 'branchValue',
                                             label   = 'Add support values',
                                             shape   = 'curve',
                                             bigger  = F,
                                             value   = F)
            ) |>
              shinyhelper::helper(
                title = 'Phylogeny Layout Options',
                type = 'inline',
                buttonLabel = 'Close',
                content = c('These are options for changing the tree layout: <br>
                             - Ladderise and Smallest clade: can flip and rearrange the tree <br>
                             - Cladogram: changes the the length of branches to align
                               on the right e.g. no branch length. <br>
                             - Add support values: add branch support values (if available in
                               .tre file) as a heatmap along branches in grey scale
                               (thickness also changes).
                             <br><br>'),
                size = 'm')
          ),
          bslib::accordion_panel(
            title = 'Heatmap Options',
            div(id='heatmap_options',
                uiOutput('heatmap_options'),
                div(style="display: inline-block;",
                    shinyWidgets::prettyCheckbox(inputId = 'highlight',
                                                 label   = 'Highlight same as target species',
                                                 shape   = 'curve',
                                                 bigger  = F,
                                                 value   = F)),
                bslib::layout_columns(
                  shinyWidgets::prettyCheckbox(inputId = 'show_legend',
                                               label   = 'Show legend',
                                               value   = F,
                                               shape   = 'curve',
                                               bigger  = F),
                  shiny::actionButton(inputId = 'update',
                                      label   = 'Update'))
            ) |>
              shinyhelper::helper(
                title = 'Heatmap options',
                type = 'inline',
                buttonLabel = 'Close',
                content = c('To add heatmaps, select max. 4 traits in the dropdown box. <br>
                             If you want to also show the legend select `Show Legend`. <br>
                             Click `Update` to show them on the tree diagram.
                             <br><br>
                             You can hover over the heatmap to get the value or use the legend to map colours.
                             <br><br>
                             To just highlight the target species trait as one colour and all
                             others as grey, select `Highlight same as target species`, then `Update` again.
                             <br><br>
                             To clear the heatmaps, delete all traits in the dropdown then `Update`.'),
                size = 'm')
          ),
          bslib::accordion_panel(
            title = 'Export',
            div(id='export_options',
                h6('Save'),
                bslib::layout_columns(
                  shiny::downloadButton(outputId = 'exportData',
                                        width='100%',
                                        label = 'Data (.csv)'),
                  shiny::downloadButton(outputId = 'exportTree',
                                        width='100%',
                                        label = 'Tree (.tre)'))
            ) |>
              shinyhelper::helper(
                title = 'Export options',
                type = 'inline',
                buttonLabel = 'Close',
                content = c(paste0(
                  'Data - export a csv with all the trait category and
                                         values for the highlighted target species.',
                  '<br>',
                  '<br>',
                  'Tree - export a .tre (parenthetic format)
                                         file with the current rooted tree.'
                )),
                size = 'm')
          )
        )
      ), # sidebar

      div(id='phylogeny_div',
          uiOutput('ggtree_plot')
      )
    ) # page_sidebar

  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
