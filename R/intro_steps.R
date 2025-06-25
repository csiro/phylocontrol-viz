# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intro steps ----
#
# Description:
#  Contains the instructions to build the walk-through for rintrojs.
#  It is sourced in global.R but used in server.R
#
#   1. Initial instructions   - intro_steps_init
#   2. Phylogeny instructions - intro_steps_phylogeny
#   3. Map instructions       - intro_steps_map
#   4. Model instructions     - intro_steps_model
#   5. Report instructions    - intro_steps_report
#
# Author: Lauren Stevens
# Date: 2025-01-13
#
# Last Modified by: Lauren Stevens
# Date: 2025-01-20
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Initial instructions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
intro_steps_init <- list(
  #~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOTE: No element ID puts intro box in center of screen
  list(
    title = "Welcome to the PhyloControl Demo",
    intro = paste0("A phylogeny and spatial data visualisation
    interface for risk analysis and decision support in weed biological control. <br><br>
    For a tour of the features on a specific page, go to '",icon('fas fa-circle-question'),"
    Help' in the navbar or click any of the ",icon('fas fa-circle-question')," icons. <hr>
    By continuing, you are agreeing to the T&Cs. <br> <br>
    <a href='https://www.csiro.au/en/about/Policies/Legal/Legal-notice'>Legal and Disclaimer</a>
    <br>
    <a href='https://www.csiro.au/en/about/Policies/Legal/Copyright'>Copyright</a>"),
    tooltipClass = "larger"
  ),
  list(
    title = "Welcome To PhyloControl",
    intro = paste0("A phylogeny and spatial data visualisation interface for
    risk analysis and decision support in weed biological control. <br><br>
    For a tour of the features on a specific page,  go to '",icon('fas fa-circle-question'),"
    Help' in the navbar or click any of the ",icon('fas fa-circle-question')," icons. <hr>
    By continuing, you are agreeing to the T&Cs. <br> <br>
    <a href='https://www.csiro.au/en/about/Policies/Legal/Legal-notice'>Legal and Disclaimer</a>
    <br>
    <a href='https://www.csiro.au/en/about/Policies/Legal/Copyright'>Copyright</a>"),
    tooltipClass = "larger"
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Phylogeny instructions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
intro_steps_phylogeny <- list(
  # list(
  #   element = 'li.nav-item:nth-child(2)',
  #   title = 'Phylogeny Tab',
  #   intro = 'Go to "Phylogeny" to explore occurrence data.'
  # ),
  list(element = '#study_options',
       title   = 'Study Group',
       intro   = 'Select the study group and a species of importance.
       This species will appear in pink in the phylogeny.',
       position = 'right'),
  list(element = '#phylogeny_div',
       title   = 'Phylogeny',
       scrollTo = 'tooltip',
       tooltipClass = "medium",
       intro   = paste0('To interact with the phylogeny: <br>
                        - Click on a node to select node then use buttons in sidebar to
                        `Re-Root` or `Collapse/Expand` node(s) <br>
                        - Click on tree, hold and drag to go up/down <br>
                        - Use scroll to zoom in/out <br>
                        - Or use the icons in the top right: <br><br>',
       shiny::icon('fas fa-arrows-up-down-left-right'), ' pan <br>',
       shiny::icon('fas fa-magnifying-glass'), ' zoom on area selected <br>',
       shiny::icon('fas fa-square-plus'), ' zoom in <br>',
       shiny::icon('fas fa-square-minus'), ' zoom out <br>',
       shiny::icon('fas fa-house-chimney'), ' reset axes <br>',
       shiny::icon('fas fa-camera'), ' download plot as png <br><br>',
       'Select a node to test the re-root or collapse in the next step.<br>
       Use `Reset` to go back to the original rooted tree.'),
       position = 'left'),
  list(element = '#tree_options',
       title   = 'Phylogeny Options',
       intro   = 'Once a node is selected in the phylogeny the tree can be re-rooted or
       the node can be collapsed or expanded. To get back to the original tree,
       use "Reset".',
       position = 'right'),
  list(element = '#layout_options',
       title   = 'Phylogeny Layout Options',
       intro   = 'These are options for changing the tree layout: <br>
          - Ladderise and Smallest clade: can flip and rearrange the tree <br>
          - Cladogram: changes the the length of branches to align
            on the right e.g. no branch length. <br>
          - Add support values: add branch support values (if available in
            .tre file) as a heatmap along branches in grey scale
            (thickness also changes).
            <br><br>',
       position = 'right'),
  list(element = '#heatmap_options',
       title   = 'Heatmaps',
       intro   = "Add heatmap by selecting a trait and press 'Update'.
       Other options to include the legend and alter the colours such that the
       target is highlighted are available.",
       position = 'right'),
  list(element = '#export_options',
       title   = 'Export Options',
       intro   = 'Save the phylogeny as a tree or the traits as a csv table.',
       position = 'right'),
  list(
    title = 'Phylogeny step complete!',
    intro = 'Go to the "Map" or "Model" tab and press "Help" again
    for instructions on those pages.',
    tooltipClass = "larger"
  )#,
  # list(
  #   element = 'li.nav-item:nth-child(3)',
  #   title = 'Map Tab',
  #   intro = 'Go to "Map" to explore occurrence data.'
  # )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Map instructions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
intro_steps_map <- list(
  list(element = '#map_options',
       title   = 'Map Options',
       intro   = 'Select the country or countries of interest for two species to compare.',
       position = 'right')
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Model instructions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
intro_steps_model <- list(
  list(element = '#model_options',
       title   = 'Model Options',
       intro   = 'Select the model and species to explore.',
       position = 'right')
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Report instructions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
intro_steps_report <- list(
  list(title   = 'Report',
       intro   = 'A table of distance measure from the target species to each species in the phylogeny.',
       tooltipClass = "larger")
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
