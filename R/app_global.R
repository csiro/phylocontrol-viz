# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global Environment ----
#
# Description:
#  Contains static elements that are read in once when the app opens
#  and are shared across all sessions.
#
#   1. Libraries
#   2. Paths
#   3. Global definitions
#
# Author(s): Louise Ord, Lauren Stevens
# Date: 2019-02-02
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print('Running global')

#======================================
#' @import data.table
#' @noRd
#======================================

.datatable.aware=TRUE # ! important

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Libraries ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyhelper)
library(shinyjs)
library(ggplot2)
library(plotly)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(dplyr)
library(tools)
library(tidyr)
library(tidytree)
library(viridisLite)
library(shinybusy)
library(ggtree)
library(ape)
library(scico)
library(RColorBrewer)
library(DT)
library(castor)
library(sp)
library(htmltools)
library(terra)
library(sf)
library(raster)
library(rworldmap)
library(rintrojs)
# library(methods)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Paths ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Global definitions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1. Colours ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Chosen colour palettes

# Primary, Light, Dark
cblues <- c("#004B87", "#00A9CE", "#142C3F")

# Dark Teal, Light Mint, Mint, Dark Forest
cgreens <- c("#007377", "#71CC98", "#00855B",
             "#44693D")

# Mid, Light, Dark
cgreys  <- c("#707070", "#D9D9D9", "#575757")

# Lavender, Light teal, Primary, Fuchsia, Plum, Dark teal, Light mint
cpalette <- c("#9faee5", "#2dccd3", "#004B87",
              "#DC1894", "#6D2077", "#007377",
              "#71CC98")
cpalette_light <- c("#9faee540","#2dccd340", "#004B8740",
                    "#DC189450","#6D207750", "#00737760",
                    "#71CC9850")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.2. Palettes for the heatmaps ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Continuous scales from scico - max 4
pal        <- 'tokyo' # package('scico')
begin_pal  <- 0.1     # take off the bottom 10%
end_pal    <- 0.9     # take off the top 10%
heatmap_pals <- c('tokyo','bilbao','davos','turku')

# colour for target highlight colour palete - this colour + grey
highlight_col  <- '#632c5e'

# colour for highlighting box around heatmaps
fuchsia      <- '#DC1894' #"violetred"  #"deeppink2"
dark_fuchsia <- '#8F005B' #"violetred4" #"deeppink4"
mid_fuchsia  <- '#CC0081' #"violetred3" #"deeppink3"

# direction of palettes for continuous scales
# EDIT HERE with new param
dir_pal <- c('Cell Count'     = 1,
             'Geo. Overlap'   =-1,
             'Patristic Dist.'= 1,
             'Degree of Sep.' = 1)

# Discrete scales - change here if use more than 5 colours per categorical heatmap
# 5 sections of palettes of diff colours for discrete heatmap with legend
pal_5sec <- c(
  c('#679436','#A5BE00','#90A955','#25A18E','#4A7C59'), # greens

  c('#0353A4', '#006DAA', '#2892D7', '#41BBD9', '#76A917'),  # blues

  c("#471ca8", "#884ab2", "#ff930a", "#f24b04", "#d1105a"), # purples

  c("#653209", "#83552D", "#9C6644", "#B88B4A", "#C8691C") # browns
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.3. Links
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
link_repo <- tags$a(shiny::icon("github"),
                    "Repo",
                    href = "https://github.com/csiro/phylocontrol-viz",
                    target = "_blank")
link_demo <- tags$a(shiny::icon("r-project"),
                    "Demo",
                    href = "https://shiny.csiro.au/phylocontrol-viz-demo/",
                    target = "_blank")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
