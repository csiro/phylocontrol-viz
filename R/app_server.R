# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Server Side Code ----
#
# Description:
#  Contains the instructions to build the application. It interfaces
#  between the global environment and the user interface.
#   1. Source reactive user interface
#   2. Define reactive values and expressions
#   3. Source event handlers
#   4. Render ui (outputs)
#   5. Reactive
#   6. ObserveEvents
#   7. Outputs
#
# Author(s): Louise Ord, Lauren Stevens, Stephanie Chen
# Date: 2019-02-02
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
app_server <- function(input, output, session) {

  # for shinyhelpers to work
  shinyhelper::observe_helpers(withMathJax = TRUE)

  # Countries & Continents
  rworldmap_lowres <- rworldmap::getMap(resolution = 'low')
  country_continent_df <- rworldmap_lowres@data[,c('ADMIN',
                                                   'continent',
                                                   'REGION')]
  names(country_continent_df) <- c("country.name.en", "continent", "region")

  # Update country names
  country_continent_df <- country_continent_df |>
    dplyr::mutate(country.name.en = dplyr::case_when(
      country.name.en == 'Swaziland' ~ 'Eswatini',
      country.name.en == 'Burma' ~ 'Myanmar',
      TRUE ~ country.name.en))

  demo <- golem::get_golem_options(which = 'demo')
  print(paste0('Running demo: ',demo))
  if (!demo) {
    dir_path <- golem::get_golem_options(which = 'dir_path')
    if (is.null(dir_path)) {
      data_path <- NULL
      stop('Error: `dir_path` not set. Please pass this info through
            run_app(dir_path=...).')
    } else {
      stopifnot(is.character(dir_path))
      stopifnot(nchar(dir_path)>0)
      stopifnot(dir.exists(dir_path))

      data_path <- dir_path
      print(paste0('`dir_path` is set to ',dir_path))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 3.4. Data ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(data_path)) {
      # find species by folder names in data_path
      species_names  <- list.dirs(data_path, full.names = F, recursive = F)
      # remove top dir ""
      species_names  <- species_names[species_names!=""]
      # build a vector for input
      vec_len        <- seq(1,length(species_names))
      names(vec_len) <- species_names
      species_vec    <- vec_len # alphabetical
      # species_filename <- unname(sapply(species_names,function(x) strsplit(x,'_')[[1]][1]))

      # find .tre files in paths
      file_tree   <- list.files(path = paste0(data_path, species_names),
                                pattern    = '.tre$',
                                full.names = T,
                                recursive  = F)

      if (length(file_tree) == 0) {
        stop("`dir_path` doesn't have any trees present. Make sure you are pointing
           to an appropriate folder.")
      }

      # find trait files
      file_traits <- list.files(path = paste0(data_path, species_names),
                                pattern    = '*_traits.csv$',
                                full.names = T,
                                recursive  = F)

      # find patristic distance files
      file_dist <- list.files(path = paste0(data_path, species_names),
                              pattern    = '*_patristic_distances*',
                              full.names = T,
                              recursive  = F)

      # find geo overlap files
      file_olap <- list.files(path = paste0(data_path, species_names),
                              pattern    = '*_cell_overlap*',
                              full.names = T,
                              recursive  = F)

      # find cell counts files
      file_count <- list.files(path = paste0(data_path, species_names),
                               pattern    = '*_cell_counts*',
                               full.names = T,
                               recursive  = F)

      # text file with target name
      file_target <- list.files(path = paste0(data_path, species_names),
                                pattern    = '*_default_target.txt',
                                full.names = T,
                                recursive  = F)

      # find occurrence data files
      file_occurrences <- list.files(path = paste0(data_path, species_names),
                                     pattern    = '*_occurrences.csv',
                                     full.names = T,
                                     recursive  = F)

      # find model data files
      file_sdm <- list.files(path = paste0(data_path,species_names),
                             pattern    = 'sdm',
                             recursive  = F,
                             full.names = T)
      # file_model_grd <- list.files(paste0(data_path, species_names),
      #                              pattern = '*.grd',
      #                              full.names = T,
      #                              recursive = T)
      # # print(file_model_grd)

      # use file to get default target
      targetSpecies <- c()
      # check if file exists and warn if doesn't
      for (spec in species_names) {
        if (length(grep(spec,file_target))==0) {
          print('Warning: No default target file found.
          Will use first species from tree as target.')
          targetSpecies <- c(targetSpecies,'')
        } else {
          file_spec <- file_target[grep(spec,file_target)]
          targetSpecies <- c(targetSpecies,
                             unname(unlist(fread(file_spec,header = F,sep=','))))
        }
      }

    }

    # start app with intro steps when not demo
    rintrojs::introjs(session,
                      options= list(exitOnOverlayClick = 'false',
                                    overlayOpacity = 0.25,
                                    showBullets = F,
                                    steps=intro_steps_init[2]),
                      events = list(onbeforechange = rintrojs::readCallback("switchTabs")))


  } else { # !demo

    # start app with intro steps when demo
    rintrojs::introjs(session,
                      options= list(exitOnOverlayClick = 'false',
                                    overlayOpacity = 0.25,
                                    showBullets = F,
                                    steps=intro_steps_init[1]),
                      events = list(onbeforechange = rintrojs::readCallback("switchTabs")))

  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1. Source reactive user interface ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. Define reactive values and expressions ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## 2.1. Current state reactive values ----
  # These reactive values are updated through event handlers, triggering any
  # reactive expressions, render functions or event handlers that depend on
  # them to be re-executed
  vals <- reactiveValues(

    # explore
    mytree        = NULL,   # keep/update mytree when rerooted etc
    myOG          = NULL,   # out group
    treeData      = NULL,   # data from tree
    targetSpecies = NULL,   # the target species to highlight
    treeChoices   = NULL,   # list of the species in the tree
    traitNames    = NULL,   # list of trait names for dropdown
    traits        = NULL,   # selected traits for heatmap
    plotHeight    = NULL,   # dynamic height based on # of species in tree
    first_click   = TRUE,   # don't delete previous trace (root node) when first click nodes
    numCurves     = NULL,   # number of curves in plotly plot - needed for support values
    Nodes         = 0,      # current node
    numNodes      = 0,      # node to start on (internal node)
    totNodes      = 0,      # total nodes internal & tip
    lenNodes      = 0,      # len of tip/leaf nodes
    heatmap       = FALSE,  # show heatmap or not - controlled by update
    highlight     = FALSE,  # highlight targets in heatmap
    show_legend   = FALSE,  # show legend with heatmaps
    list_nodes    = c(),    # list of nodes for reproduction of phylogeny
    collapsed_nodes = NULL, # list of nodes to collapse

    # map
    country_list = NULL,
    country      = 'Australia',
    study_group  = NULL,
    species_map1 = NULL,
    species_map2 = NULL,

    # sdm
    model_method   = NULL,
    model_timespan = NULL,
    model_species  = NULL,
    mapModel       = NULL
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Call functions for reactive ui (server-side)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reactive_ui(input,output,session)
  navbar_tab_phylogeny(input,output,session,species_vec)
  # https://stackoverflow.com/questions/78749269/how-to-hide-show-nav-panel-based-on-user-role
  if (exists('file_occurrences')) {
    if(length(file_occurrences)>0) {
      navbar_tab_map(input,output,session,species_vec,vals)
    }
  } else {
    if (demo) {
      navbar_tab_map(input,output,session,species_vec,vals)
    }
  }
  if (exists('file_sdm')) {
    if(length(file_sdm)>0) {
      navbar_tab_model(input,output,session,vals)
    }
  } else {
    if (demo) {
      navbar_tab_model(input,output,session,vals)
    }
  }
  navbar_tab_report(input,output,session)
  navbar_tab_home(input,output,session)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Show/hide Map/Model tab if data available
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$nav,{
    # https://github.com/rstudio/bslib/issues/1117
    # https://stackoverflow.com/questions/78749269/how-to-hide-show-nav-panel-based-on-user-role
    if (exists('file_occurrences')) {
      if(length(file_occurrences)>0) {
        bslib::nav_show('nav','Map')
      } else {
        bslib::nav_hide('nav','Map')
      }
    } else {
      if (!demo) {
        bslib::nav_hide('nav','Map')
      }
    }
    if (exists('file_sdm')) {
      if(length(file_sdm)>0) {
        bslib::nav_show('nav','Model')
      } else {
        bslib::nav_hide('nav','Model')
      }
    } else {
      if (!demo) {
        bslib::nav_hide('nav','Model')
      }
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. Source event handlers ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. Render ui ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$note_demo_model <- renderUI({
    if (demo) {
      HTML(paste0("<p style='color: #E40028;'>Note: this is the demo,
            so only the target - ", targetSpecies," - is available.</p>"))
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ui sidebar: input for heatmap traits
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$heatmap_options <- shiny::renderUI({
    print('render trait filter')
    req(traitNames <- vals$traitNames)
    shiny::selectizeInput(inputId  = 'traits',
                          label    = 'Add heatmap(s) (max. 4)',
                          choices  = sort(traitNames),
                          selected = NULL,
                          options = list(maxItems = 4),
                          multiple = T)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ui sidebar input for target species
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$ui_reactive <- shiny::renderUI({
    req(vals_targetSpecies <- vals$targetSpecies)
    req(treeChoices <- vals$treeChoices)
    print('render ui reactive')
    div(
      shiny::selectInput(inputId  = 'target',
                         label    = 'Target species',
                         choices  = treeChoices,
                         selected = vals_targetSpecies)
    )
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ui output for the phylogeny with reactive height
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$ggtree_plot <- shiny::renderUI({
    req(plotHeight <- vals$plotHeight)
    if (!is.null(plotHeight)) {
      print('render plot - height reactive')
      plotly::plotlyOutput('ggtree',
                           height = paste0(plotHeight,'px'),
                           width='100%')
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 5. Reactive functions ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read in the tree using ape ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  myTree <- shiny::reactive({
    print('mytree')

    # read in tree
    req(species <- as.numeric(input$species))
    if (!demo) {
      if (length(grep(species_names[species],file_tree))>0) {
        mytree  <- ape::read.tree(file_tree[grep(species_names[species],file_tree)])
      } else {
        stop('Error: No tree found.')
      }
    } else {
      mytree <- demo_tree
    }
    # remove _ from labels
    mytree$tip.label <- gsub("^_","",mytree$tip.label)
    mytree$tip.label <- gsub("_"," ",mytree$tip.label)
    mytree
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Map reactive data ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  geoData <- shiny::reactive({
    print('map reactive data')

    req(species     <- as.numeric(input$species))
    # print(species)

    if (!demo) {
      geo_file <- file_occurrences[grep(species_names[species],file_occurrences)]
    } else {
      geo_file <- ""
    }

    if (length(geo_file)>0) {

      if (!demo) {
        geo_data <- data.table::fread(geo_file)
      } else {
        geo_data <- demo_geo_data
      }

      geo_data <- geo_data |>
        dplyr::mutate(country = dplyr::case_when(
          country == 'Swaziland' ~ 'Eswatini',
          country == 'Burma' ~ 'Myanmar',
          TRUE ~ country))

      geo_data <- dplyr::left_join(geo_data, country_continent_df,
                                   by=c('country'='country.name.en'))

      geo_data$map_filter_col <- paste0(geo_data$country,
                                        '/',
                                        geo_data$species)

    } else {
      geo_data <- NULL
    }
    geo_data
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Start ggtree as reactive ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggTree <- shiny::reactive({

    print('ggtree')

    req(vals_mytree <- vals$mytree)
    req(target      <- vals$targetSpecies)
    (ladderize   <- input$ladderize)
    (brLength    <- input$brLength)
    (brValue     <- input$branchValue)
    (collapsed_nodes <- vals$collapsed_nodes)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # make a ggtree basde on ladderise, right, brLength inputs
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!ladderize) {
      right <- T # not used when ladderize=F
    } else {
      right <- input$right
    }
    if (brLength) {
      branchLen = 'none'
    } else {
      branchLen = 'branch.length'
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # check branch support values
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (brValue) {
      if ("node.label" %in% names(vals_mytree)) {

        check_value <- sum(grepl('[0-9]',vals_mytree$node.label))
        check_multi <- sum(grepl(  '[/]',vals_mytree$node.label))

        if (check_value > 0) {
          if (check_multi > 0) {
            # if multiple - save original labels
            vals_mytree$node.label.orig <- vals_mytree$node.label
            for (row in 1:length(vals_mytree$node.label)) {
              vals_mytree$node.label[row] <- strsplit(vals_mytree$node.label.orig[row],
                                                      '/')[[1]][1]
            } # for row
          } # if check_multi
          # convert to numeric
          vals_mytree$node.label <- as.numeric(vals_mytree$node.label)
        } # if check_value
      } # node.label
    } # brValue

    # create ggtree with appropriate params
    tree_plot <- ggtree::ggtree(vals_mytree,
                                branch.length = branchLen,
                                ladderize = ladderize,
                                right=right)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # remove '_' from labels
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tree_plot$data$label <- gsub("^_","",tree_plot$data$label)
    # tree_plot$data$label <- gsub("_"," ",tree_plot$data$label)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add offspring and collapsed node information using
    # add_labels_for_collapsed_nodes() function
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tree_plot$data <- add_labels_for_collapsed_nodes(tree_plot)
    ## pad text ?
    # tree_plot$data[tree_plot$data$isTip,]$os_count <- tree_plot$data[tree_plot$data$isTip,]$label
    # tree_plot$data$os_count <- label_pad(tree_plot$data$os_count)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # which node to expand or collapse
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_offspring <- data.table::data.table()
    if (!is.null(collapsed_nodes)) {
      nodes    <- collapsed_nodes
      expand   <- nodes[duplicated(nodes)]  # when a collapsed node is re-selected, expand
      collapse <- nodes[!nodes %in% expand] #

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if nodes to collapse exist:
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (length(collapse)>0) {

        # make list of offspring for each node
        if (length(collapse)==1) {
          offspring <- list(tidytree::offspring(tree_plot, collapse))
          names(offspring) <- collapse
        } else {
          offspring <- tidytree::offspring(tree_plot, collapse)
        }

        # loop over nodes to collapse, collapse and check if
        # offspring is target
        for (node in collapse) {
          # check if node is in offspring in other nodes
          idx <- grep(node,names(offspring),invert=T)
          if (length(idx)>0) {
            check_offspring <- offspring[idx]
            is_os <- length(
              unlist(lapply(check_offspring,
                            function(x) {
                              grep(node, x$node)
                            })))
          } else {
            is_os <- 0
          }

          # collapse
          tree_plot <- tree_plot |>
            ggtree::collapse(node=node)

          # check to see if target is in offspring
          is_target <- grep(target,
                            gsub("_", " ",
                                 offspring[[grep(node,
                                                 names(offspring))]]$label))
          if (length(is_target)==0) { is_target <- 0 }

          is_offspring <- rbind(is_offspring,
                                data.table::data.table(
                                  'node'   = node,
                                  'is_os'  = is_os,
                                  'target' = is_target
                                ))
        } # for node

      } # if len>0

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if nodes to expand exist:
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (length(expand)>0) {
        for (node in expand) {
          tree_plot <- tree_plot |>
            ggtree::expand(node=node)
        }}

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # keep track of nodes still collapsed
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (length(collapse)>0) {
        vals$collapsed_nodes <- collapse
      } else {
        vals$collapsed_nodes <- NULL
      }

    } # !null c.nodes

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add tip text and points to tree and target point in fuchsia
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tree_plot <- tree_plot +
      ggplot2::geom_text(data=tree_plot$data[tree_plot$data$isTip,],
                         mapping = ggplot2::aes(x,y,label=paste0("  ",label)),
                         size=3,
                         hjust='inward',
                         vjust='bottom') +
      ggplot2::geom_text(data=tree_plot$data[tree_plot$data$label==target,],
                         mapping = ggplot2::aes(x,y,label=paste0("  ",label),fontface = "bold"),
                         size=3,
                         color=mid_fuchsia,
                         hjust='inward',
                         vjust='bottom') +
      ggplot2::geom_point(data=tree_plot$data[tree_plot$data$isTip,],
                          mapping = ggplot2::aes(x = x, y = y, #label = label,
                                                 text=sprintf("Species: %s<br>Node: %s<br>Parent: %s",
                                                              label,node,parent)),
                          color='#007377') +
      ggplot2::geom_point(data=tree_plot$data[tree_plot$data$label==target,],
                          mapping = ggplot2::aes(x = x, y = y, #label = label,
                                                 text=sprintf("Species: %s<br>Node: %s<br>Parent: %s",
                                                              label,node,parent)),
                          color=mid_fuchsia,size=2.5)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add internal nodes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tree_plot <- tree_plot +
      ggplot2::geom_point(data=tree_plot$data[!tree_plot$data$isTip,],
                          mapping = ggplot2::aes(x = x, y = y,
                                                 text=sprintf("Node: %s<br>Parent: %s<br>Support Value: %s",
                                                              node,parent,label)),
                          color='#142C3F')

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add root node shape
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tree_plot <- tree_plot +
      ggplot2::geom_point(data=tree_plot$data[tree_plot$data$node==vals$numNodes,],
                          mapping = ggplot2::aes(x = x, y = y,name='node',
                                                 text=sprintf("Root Node: %s<br>Parent: %s",
                                                              node,parent)),
                          shape=15,color=cpalette[7], size=2.5)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # IMPORTANT: order
    # add collapsed node offspring counts - important that this happens after
    # the other labels and points - o/w mucks up click functionality
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (nrow(is_offspring)>0) {
      for (os in 1:nrow(is_offspring)) {
        # change text to pink if target collapsed
        if (!is.na(is_offspring$target[os]) & is_offspring$target[os]>0) {
          text_col <- mid_fuchsia
          shiny::showNotification(ui = paste0('Warning: Target Species in Collapsed Node.'),
                                  duration = 30,
                                  closeButton = T,
                                  type = 'warning')
        } else {
          text_col <- "#1E22AA"
        }
        if (is_offspring$is_os[os]==0) {
          tree_plot <- tree_plot +
            # geom_point(data=tree_plot$data[tree_plot$data$node==node,],
            #            ggplot2::aes(x=x, y=y),
            #            shape=19,
            #            size=3.2,
            #            color='#007377') +
            ggplot2::geom_text(#data=new_data,
              data=tree_plot$data[tree_plot$data$node==is_offspring$node[os],],
              # ggplot2::aes(x=(`x`+.03),y=`y`,
              ggplot2::aes(x=`x`,y=`y`,
                           label=HTML(paste0("<b>   ",
                                             `os_count`,
                                             "</b>"))),
              size=3,
              colour = text_col,
              hjust='inward',
              vjust='bottom')
        } # if check
      } # for os
    } # if >0

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add branch support values
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (brValue) {
      if ("node.label" %in% names(vals_mytree)) {
        # tree_plot <- ggtree::ggtree(vals_mytree,
        #                             branch.length = branchLen,
        #                             ladderize = ladderize,
        #                             right=right,
        #                             ggplot2::aes(color=as.numeric(label),
        #                                 size =as.numeric(label)
        #                             )) +
        tree_plot <- tree_plot + ggtree::geom_tree(ggplot2::aes(color=as.numeric(`label`)),
                                                   linewidth=2) +
                                                # linewidth=as.numeric(`label`))) +
          # ggtree::geom_rootedge(rootedge = .01) +
          ggplot2::scale_color_continuous(#low='#e3aeea',high='#6D2077',
                                          #low='#72f7cd',high='#004d35',
                                          low='grey85',high='grey30',
                                          na.value='transparent') +
          # ggplot2::scale_size_continuous(range = c(0, 1)) +
          ggplot2::theme(legend.position="none")

        # change the order of plotting support values layers
        # ow they show up over the geom_points etc
        # default 8 layers before adding support values
        tree_plot$layers <- c(tree_plot$layers[1:4],
                              tree_plot$layers[9:10],
                              tree_plot$layers[5:8])
      } else{
        shiny::showNotification(ui = "No Branch Support Value Available.",
                                duration = 30,
                                closeButton = T,
                                type = 'default')
      }
    } # brValue

    tree_plot

  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Get traits for heatmap ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  traitData <- shiny::reactive({

    print('trait data')

    req(nnodes       <- vals$numNodes)
    req(tree_choices <- vals$treeChoices)
    req(species      <- as.numeric(input$species))
    req(target       <- (vals$targetSpecies))
    req(mytree       <- vals$mytree)

    # get degrees of separation - calc using degreesofsep() in functions.R
    pdegsep <- degreesofsep(mytree)
    names(pdegsep)   <- gsub(" ","_",names(pdegsep))
    pdegsep$`Species_` <- gsub(" ","_",pdegsep$`Species_`)
    Tdeg <- data.table::melt(pdegsep[`Species_`==gsub(" ","_",target),],
                             id.vars       = 'Species_',
                             variable.name = 'Species',
                             value.name    = 'Degree of Sep.') # by row
    Tdeg <- Tdeg[,2:3]
    names(Tdeg)[1] <- 'Species_'

    traits_new <- cbind(gsub("_"," ",Tdeg$`Species_`),Tdeg)
    names(traits_new)[1] <- 'Species'

    # read in traits
    if (!demo) {
      if (length(grep(species_names[species],file_traits))>0) {
        # traits <- data.table::fread(file_traits[species])
        traits <- data.table::fread(file_traits[grep(species_names[species],
                                                     file_traits)])
        # rm any duplicates
        traits <- traits[!duplicated(traits),]
        tcols  <- names(traits)[3:ncol(traits)]
        traits[ , (tcols) := lapply(.SD, tools::toTitleCase), .SDcols = tcols]
        # add column data
        traits_sub <- names(traits)[names(traits)!='Species']
        traits_new <- dplyr::left_join(traits_new,traits[,..traits_sub],
                                       by='Species_')
      }
    } else {
      traits <- demo_traits
      # rm any duplicates
      traits <- traits[!duplicated(traits),]
      tcols  <- names(traits)[3:ncol(traits)]
      traits[ , (tcols) := lapply(.SD, tools::toTitleCase), .SDcols = tcols]
      # add column data
      traits_sub <- names(traits)[names(traits)!='Species']
      traits_new <- dplyr::left_join(traits_new,traits[,..traits_sub],
                                     by='Species_')
    }

    # calculate patristic distance
    mytree$tip.label <- gsub(" ","_",mytree$tip.label)
    pdist <- calc_patristic_distance(tree=mytree,type='tips')
    pdist <- data.table::as.data.table(pdist)
    pdist <- cbind(names(pdist),pdist)
    names(pdist)[1] <- 'Species_'

    cols <- c("Species_",gsub(" ","_",target))

    # get patristic distance
    Tdist <- pdist[,..cols]
    Tdist[,2] <- round(Tdist[,2],3)
    names(Tdist)[2] <- 'Patristic Dist.'
    traits_new <- dplyr::left_join(traits_new,Tdist,by='Species_')

    if (!demo) {
      if (length(grep(species_names[species],file_olap))>0) {
        # overlap <- data.table::fread(file_olap[species])
        overlap <- data.table::fread(file_olap[grep(species_names[species],file_olap)])
        names(overlap)[1] <- 'Species'

        # get geographical overlap
        ocols <- c('Species',target)
        Toverlap <- overlap[,..ocols]
        Toverlap[,2] <- Toverlap[,2]*100
        names(Toverlap)[2] <- 'Geo. Overlap'
        traits_new <- dplyr::left_join(traits_new,Toverlap,by='Species')
      }
      #    else {
      #   # calculate geographic overlap if file not present
      #   geo_overlap <- calculate_geographic_overlap(species = species_names[species], target_species = target_species_data)
      #
      #   # assuming the result is a data.table or can be converted to one
      #   geo_overlap <- data.table::as.data.table(geo_overlap)
      #   names(geo_overlap)[1] <- 'Species'
      #
      #   # format the result
      #   ocols <- c('Species', target)
      #   Toverlap <- geo_overlap[, ..ocols]
      #   Toverlap[, 2] <- Toverlap[, 2] * 100
      #   names(Toverlap)[2] <- 'Geo. Overlap'
      #
      #   traits_new <- dplyr::left_join(traits_new, Toverlap, by = 'Species')
      # }
    } #else {
    #   overlap <- demo_overlap
    #   names(overlap)[1] <- 'Species'
    #
    #   # get geographical overlap
    #   ocols <- c('Species',target)
    #   Toverlap <- overlap[,..ocols]
    #   Toverlap[,2] <- Toverlap[,2]*100
    #   names(Toverlap)[2] <- 'Geo. Overlap'
    #   traits_new <- dplyr::left_join(traits_new,Toverlap,by='Species')
    # }


    if (!demo) {
      if (length(grep(species_names[species],file_count))>0) {
        # ccount <- data.table::fread(file_count[species])
        ccount <- data.table::fread(file_count[grep(species_names[species],file_count)])
        ccount <- ccount[,c(1,2)]
        names(ccount) <- c('Species','Cell Count')

        # get cell count
        Tcount <- ccount
        traits_new <- dplyr::left_join(traits_new,Tcount,by='Species')
      }
    } #else {
    #   ccount <- demo_count
    #   ccount <- ccount[,c(1,2)]
    #   names(ccount) <- c('Species','Cell Count')
    #
    #   # get cell count
    #   Tcount <- ccount
    #   traits_new <- dplyr::left_join(traits_new,Tcount,by='Species')
    # }

    col_sub <- names(traits_new)[names(traits_new)!='Species_']
    trait_data <- traits_new[,..col_sub]

    return(trait_data)

  }) # end: traitData

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Click event for node ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  click_data <- shiny::reactive({
    print('click reactive')
    click_event <- plotly::event_data("plotly_click",
                                      source = 'ggtree')
    click_event
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Country lists by Species ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  speciesByCountry <- shiny::reactive({
    print('species')
    req(geo_data <- geoData())
    cnty_list <- list()
    cnty_unq  <- unique(geo_data$country)
    cnty_unq  <- sort(cnty_unq[!is.na(cnty_unq)])
    for (cnty in cnty_unq) {
      sciNames <- sort(unique(geo_data[country == cnty, ]$species))
      cnty_list[[cnty]] <- as.list(paste0(cnty, '/', sciNames))
      names(cnty_list[[cnty]]) <- sciNames
    }
    # print(cnty_list)
    cnty_list
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Country lists by Continent ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  countryByContinent <- shiny::reactive({
    print('country')
    req(geo_data <- geoData())
    con_list <- list()
    con_unq  <- unique(geo_data$continent)
    con_unq  <- sort(con_unq[!is.na(con_unq)])
    for (con in con_unq) {
      ctry <- sort(unique(geo_data[geo_data$continent == con, ]$country))
      con_list[[con]] <- as.list(ctry)
      # con_list[[con]] <- as.list(paste0(con, '/', ctry))
      # names(con_list[[con]]) <- ctry
    }
    con_list
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 6. Observe events ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  observeEvent(input$help,{
    tab <- input$nav

    if (tab=='Phylogeny') {
      bslib::toggle_sidebar(id = 'phylogeny_sidebar',
                            open = TRUE,
                            session = session)
      bslib::accordion_panel_open(id = 'acc',
                                  values = T,
                                  session = session)
      shinyjs::delay(1000,
                     rintrojs::introjs(
                       session,
                       options= list(exitOnOverlayClick = 'false',
                                     overlayOpacity = 0.25,
                                     steps=intro_steps_phylogeny),#[c(-1,-2)]),
                       events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
                     )
      )

    } else if (tab=='Map') {

      bslib::toggle_sidebar(id = 'map_sidebar',
                            open = TRUE,
                            session = session)

      shinyjs::delay(1000,
                     rintrojs::introjs(
                       session,
                       options= list(exitOnOverlayClick = 'false',
                                     overlayOpacity = 0.25,
                                     showBullets = F,
                                     steps=intro_steps_map),
                       events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
                     )
      )

    } else if (tab=='Model') {

      bslib::toggle_sidebar(id = 'model_sidebar',
                            open = TRUE,
                            session = session)

      shinyjs::delay(1000,
                     rintrojs::introjs(
                       session,
                       options= list(exitOnOverlayClick = 'false',
                                     overlayOpacity = 0.25,
                                     showBullets = F,
                                     steps=intro_steps_model),
                       events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
                     )
      )

    } else if (tab=='Report') {

      shinyjs::delay(1000,
                     rintrojs::introjs(
                       session,
                       options= list(exitOnOverlayClick = 'false',
                                     overlayOpacity = 0.25,
                                     showBullets = F,
                                     steps=intro_steps_report),
                       events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
                     )
      )

    } else {

      demo_intro <- ifelse(demo,1,2)
      # demo_intro <- ifelse(demo,-2,-1)
      rintrojs::introjs(
        session,
        options= list(exitOnOverlayClick = 'false',
                      overlayOpacity = 0.25,
                      steps=intro_steps_init[demo_intro]),
        events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
      )

    }

  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Find unique values for ui components ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$species,{
    print('observe species - phylogeny tab')
    mytree <- myTree()

    # find unique species
    tree_choices <- unique(mytree$tip.label)
    tree_choices <- sort(tree_choices[!is.na(tree_choices)])
    # tree_choices <- gsub("^_","",tree_choices)
    # tree_choices <- gsub("_"," ",tree_choices)

    # target species to highlight initially
    species <- as.numeric(input$species)

    # calc height for plot based on number of nodes
    height  <- RoundUp(10*(length(mytree$edge.length)+1),100)

    # work out tip nodes vs internal nodes
    len     <- length(mytree$tip.label)
    # number of tips
    nnodes  <- len+1
    # number of internal nodes
    tnodes  <- length(mytree$edge.length)+1
    # ? important ?
    if (is.odd(len)) {
      len <- len-1
    }

    # set vals$
    vals$treeChoices <- tree_choices
    vals$plotHeight  <- min(max(1000,height),5000)
    vals$numNodes    <- nnodes
    vals$Nodes       <- nnodes
    vals$totNodes    <- tnodes
    vals$lenNodes    <- len

    if (targetSpecies[species]=='') {
      vals$targetSpecies <- tree_choices[1]
    } else {
      vals$targetSpecies <- targetSpecies[species]
    }

    vals$myOG       <- nnodes
    vals$list_nodes <- c(vals$list_nodes,nnodes)
    vals$mytree     <- mytree
    # vals$treeData   <- tree

  }) # end: obs species

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update the traits for ui ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$species,{
    print('update trait name - phylogeny tab')
    traits <- traitData()
    vals$traitNames <- names(traits)[2:length(names(traits))]
    vals$traits     <- NULL
    vals$list_nodes <- vals$numNodes
    vals$collapsed_nodes <- NULL
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update targetSpecies when target changes ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$target,{
    print('update target - phylogeny tab')
    if (vals$targetSpecies!=input$target) {
      vals$targetSpecies <- input$target
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Select node to collapse/expand ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$collapse,{

    print('collapse event - phylogeny tab')

    req(change_node <- vals$Nodes)
    vals_collapsed_nodes <- vals$collapsed_nodes

    if (!is.null(vals_collapsed_nodes)) {
      collapsed_nodes <- c(vals_collapsed_nodes,change_node)
    } else {
      collapsed_nodes <- change_node
    }
    vals$collapsed_nodes <- collapsed_nodes

    shinyjs::disable('reroot')
    shinyjs::disable('collapse')
    vals$first_click <- TRUE

  }) # end: observe collapse

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update node input before updating re-root ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(click_data(),{
    print('click event - phylogeny tab')
    click <- click_data()
    # print(click)
    isolate(brValue <- input$branchValue)
    isolate(numCurves <- vals$numCurves)
    # Tree
    req(tree <- ggTree())
    # brLength <- input$brLength
    first_click <- vals$first_click
    myOG     <- vals$myOG
    Nodes    <- vals$Nodes
    numNodes <- vals$numNodes

    # curveNumbers shift to a large value when support values are added
    if (brValue) {
      curve_shift <- (numCurves - 8)
    } else {
      curve_shift <- 0
    }

    # act only on tree - the tree and nodes are in layer/curve < 10
    if (click$curveNumber <= (curve_shift+10)) {

      n_val <- tree$data[tree$data$x == click$x &
                           tree$data$y == click$y,]
      n_val <- n_val[!is.na(n_val$parent),]

      # diff curve number for diff points: tips (4&5) vs internal(6/7/8)
      if (click$curveNumber > (curve_shift+3)) { # not the lines of the tree
        if (click$curveNumber == (curve_shift+6) |
            click$curveNumber == (curve_shift+7) |
            click$curveNumber == (curve_shift+8)) {
          # For internal nodes
          new_node <- n_val$node
        } else if (click$curveNumber == (curve_shift+4) |
                   click$curveNumber == (curve_shift+5)) {
          # For Tip nodes
          new_node <- n_val$parent
        }

        # keep track of selected node
        vals$Nodes <- new_node
        Nodes <- new_node

        shinyjs::enable('reroot') # enable reroot button
        shinyjs::enable('collapse') # enable reroot button

        # delete previous node position if not the first click
        if (!first_click) {
          plotly::plotlyProxy("ggtree", session) |>
            plotly::plotlyProxyInvoke('deleteTraces',list(as.integer(-1)))
        }
        # update first click - so that when plotproxy removes previous trace it
        # doesn't remove the root node in the first click
        if (myOG != Nodes) {
          vals$first_click <- FALSE
        } else {
          vals$first_click <- TRUE
        }
        # update selected node position
        if (Nodes!=numNodes) {
          # print('proxy')
          plotly::plotlyProxy("ggtree", session) |>
            plotly::plotlyProxyInvoke("addTraces",
                                      list(
                                        x = rep(n_val$x,2),
                                        y = rep(n_val$y,2),
                                        type = 'scatter',
                                        mode = 'markers',
                                        name = 'Selected Node',
                                        # text = 'New Node to Root to',
                                        hoverinfo = 'text',
                                        text = paste('Root Node: ',
                                                     n_val$node,
                                                     '<br>Parent:',
                                                     n_val$parent,
                                                     '</br>'),
                                        marker = list(size = 10,
                                                      symbol = 'star-diamond-dot',
                                                      color = cblues[2],
                                                      line = list(color = cblues[2],
                                                                  width = 2))
                                      ) # list
            ) # add trace
        } # nodes

      } # curve >1
    } # curve <=10

  }) # end: obs click event

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Re-root to selected node ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(vals$myOG,{
    print('myOG: reroot - phylogeny tab')
    vals_myOG       <- vals$myOG
    vals_mytree     <- vals$mytree
    vals_list_nodes <- vals$list_nodes

    vals$collapsed_nodes <- NULL
    vals$mytree     <- ape::root(vals_mytree, node = vals_myOG)
    vals$list_nodes <- c(vals_list_nodes,vals_myOG)
    vals$Nodes <- vals$numNodes
    vals$myOG  <- vals$numNodes
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update node for re-root ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$reroot,{
    print('reroot: update myOG - phylogeny tab')
    vals$myOG <- vals$Nodes
    shinyjs::disable('reroot')
    vals$first_click <- TRUE
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update re-root button based on nodes selected ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(vals$Nodes,{
    print('observe nodes - phylogeny tab')
    if (vals$Nodes!=vals$numNodes) {
      shinyjs::enable('reroot')
      shinyjs::enable('collapse')
    } else {
      shinyjs::disable('reroot')
      shinyjs::disable('collapse')
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enable/disable reset based on tree config ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(vals$mytree,{
    print('mytree updated - phylogeny tab')
    orig <- myTree()
    if (!identical(vals$mytree,orig)) {
      shinyjs::enable('reset')
    } else {
      shinyjs::disable('reset')
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enable/disable reset based on collapsed nodes ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(vals$collapsed_nodes,{
    print('Collapsed node update - phylogeny tab')
    cn <- vals$collapsed_nodes
    if (!is.null(cn)) {
      shinyjs::enable('reset')
    } else {
      shinyjs::disable('reset')
    }
  },ignoreNULL = F)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reset to re-root from nodes with first tree ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$reset,{
    print('reset - phylogeny tab')
    vals$mytree <- myTree()
    vals$myOG <- vals$numNodes
    vals$Nodes <- vals$numNodes

    if (!vals$first_click) {
      plotly::plotlyProxy("ggtree", session) |>
        plotly::plotlyProxyInvoke('deleteTraces',list(as.integer(-1)))
    }

    vals$first_click <- TRUE

    # species <- as.numeric(input$species)
    # if (targetSpecies[species]=='') {
    #   vals$targetSpecies <- tree_choices[1]
    # } else {
    #   vals$targetSpecies <- targetSpecies[species]
    # }

    vals$list_nodes <- vals$numNodes
    vals$collapsed_nodes <- NULL

    shinyjs::disable('reroot')
    shinyjs::disable('collapse')
    shinyjs::disable('reset')
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enable update if highlight is changed ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$highlight,{
    print('observe highlight - phylogeny tab')
    (highlight <- input$highlight)
    if (highlight!=vals$highlight) {
      if (!is.null(vals$traits)) {
        shinyjs::enable('update')
      }
    } else {
      if (identical(vals$traits,input$traits)) {
        shinyjs::disable('update')
      }
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enable update if show_legend is changed ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$show_legend,{
    print('observe show_legend - phylogeny tab')
    (show_legend <- input$show_legend)
    if (show_legend!=vals$show_legend) {
      if (!is.null(vals$traits)) {
        shinyjs::enable('update')
      }
    } else {
      if (identical(vals$traits,input$traits)) {
        shinyjs::disable('update')
      }
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enable update if trait options are changed ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$traits,{
    print('observe traits - phylogeny tab')
    traits <- input$traits
    if (!identical(vals$traits,traits)) {
      shinyjs::enable('update')
    } else {
      shinyjs::disable('update')
    }
    if (is.null(traits)) {
      shinyjs::disable('show_legend')
      shinyjs::disable('highlight')
    } else {
      shinyjs::enable('show_legend')
      shinyjs::enable('highlight')
    }
  },ignoreNULL = F,ignoreInit = T)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Switch for adding heatmap to phylogeny ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$update,{
    print('update heatmap - phylogeny tab')
    if (!identical(vals$traits,input$traits)) {
      vals$heatmap <- T
    }
    if (is.null(input$traits)) {
      vals$heatmap <- F
    }
    vals$traits <- input$traits
    if (input$highlight!=vals$highlight) {
      vals$highlight <- input$highlight
    }
    if (input$show_legend!=vals$show_legend) {
      vals$show_legend <- input$show_legend
    }
    shinyjs::disable('update')
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Filters for map ----
  #   - update country and species dropdowns when switching
  #     between study groups
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$nav,{
    # print('observe tabs')
    req(nav <- input$nav)
    if (nav=='Map') {
      print('species - map tab')
      req(treeChoices   <- vals$treeChoices)
      req(vals_targetSpecies <- vals$targetSpecies)
      # req(species_map1  <- vals$species_map1)
      # req(species_map2  <- vals$species_map2)
      (species_val <- vals$study_group)
      req(species  <- names(species_vec[as.numeric(input$species)]))

      if (!identical(species_val,species)) {
        print('study group diff')

        continents <- countryByContinent()
        vals$country <- 'Australia'
        shinyWidgets::updatePickerInput(session  = session,
                                        inputId  = 'country',
                                        selected = 'Australia',
                                        choices  = continents)

        vals$species_map1 <- vals_targetSpecies
        shinyWidgets::updatePickerInput(session  = session,
                                        inputId  = 'species_map1',
                                        selected = vals_targetSpecies,
                                        choices  = treeChoices)

        vals$species_map2 <- treeChoices[1]
        shinyWidgets::updatePickerInput(session  = session,
                                        inputId  = 'species_map2',
                                        selected = treeChoices[1],
                                        choices  = treeChoices)
        # Study Group - keep track of species selection on phylogeny
        vals$study_group <- species
      }

    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # update species list when country is changed
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$country,{

    print('observe country')

    (cntry <- input$country)
    # (cnty_list <- speciesByCountry())
    (target  <- vals$targetSpecies)
    (choices <- vals$treeChoices)
    req(species_map1  <- input$species_map1)
    req(species_map2  <- input$species_map2)

    req(geo_data <- geoData())
    sciNames  <- sort(unique(geo_data[country %in% cntry, ]$species))

    # print(country)
    # new_list <- cnty_list[names(cnty_list) %in% country]

    # make sure species is in tree
    new_list <- sciNames[sciNames %in% choices]

    # if (target %in% new_list) {
    #   mapSpecies <- target
    # } else {
    #   mapSpecies <- new_list[1]
    # }
    if (!target %in% new_list) {
      shiny::showNotification(ui = 'Note: Target species is not in the
                              countries selected.',
                              duration = 25,
                              type = 'message')
    }
    # make sure target is available and if already selected
    # make first species
    if (species_map1 %in% new_list) {
      mapSpecies1 <- species_map1
    } else {
      shiny::showNotification(ui = 'Warning: Species 1 is not in the
                              countries selected. Reverting to first
                              species in list that is present.',
                              duration = 25,
                              type = 'warning')
      mapSpecies1 <- new_list[1]
    }
    if (species_map2 %in% new_list) {
      mapSpecies2 <- species_map2
    } else {
      shiny::showNotification(ui = 'Warning: Species 2 is not in the
                              countries selected. Reverting to first
                              species in list that is present.',
                              duration = 25,
                              type = 'warning')
      mapSpecies2 <- new_list[1]
    }

    shinyWidgets::updatePickerInput(session  = session,
                                    inputId  = 'species_map1',
                                    selected = mapSpecies1,
                                    # selected = paste0(country,"/",vals$targetSpecies),
                                    choices  = new_list)
    shinyWidgets::updatePickerInput(session  = session,
                                    inputId  = 'species_map2',
                                    selected = mapSpecies2,#sciNames[1],
                                    # selected = paste0(country,"/",vals$targetSpecies),
                                    choices  = new_list)

    if (is.null(cntry)) {
      shinyjs::runjs("$('#species_map1').prop('disabled', true);")
      shinyjs::runjs("$('#species_map1').selectpicker('refresh');")
      shinyjs::runjs("$('#species_map2').prop('disabled', true);")
      shinyjs::runjs("$('#species_map2').selectpicker('refresh');")
    } else {
      shinyjs::runjs("$('#species_map1').prop('disabled', false);")
      shinyjs::runjs("$('#species_map1').selectpicker('refresh');")
      shinyjs::runjs("$('#species_map2').prop('disabled', false);")
      shinyjs::runjs("$('#species_map2').selectpicker('refresh');")
    }

    vals$species_map1 <- mapSpecies1
    vals$species_map2 <- mapSpecies2 #sciNames[1]

  },ignoreNULL = F,ignoreInit = F)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make sure vals is populated
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$country,{
    req(country_val <- vals$country)
    req(country     <- input$country)
    print('country input')
    if (!identical(country_val,country)) {
      vals$country <- country
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make sure vals is populated
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$species_map1,{
    req(species_map_val <- vals$species_map1)
    req(species_map     <- input$species_map1)
    print('species1 input')
    if (species_map_val != species_map) {
      vals$species_map1 <- species_map
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make sure vals is populated
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$species_map2,{
    req(species_map_val <- vals$species_map2)
    req(species_map     <- input$species_map2)
    print('species2 input')
    if (species_map_val != species_map) {
      vals$species_map2 <- species_map
    }
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 7. Render output ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Table for ranking species ----
  # https://stackoverflow.com/questions/58805058/issues-while-updating-cell-in-r-shiny-datatable
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # output$test_list <- DT::renderDataTable({
  #   print('test list')
  #
  #   (treeChoices <- vals$treeChoices)
  #   species_list <- data.table::data.table(Species = treeChoices,
  #                              Rank = rep(0,length(treeChoices)))
  #
  #   DT::datatable(species_list,
  #                 extensions = 'Buttons',
  #                 options = list(pageLength=50,
  #                                dom = 'Bfrtip',
  #                                buttons = c('csv')),
  #                 rownames = F,class = 'compact',
  #                 editable = list(target = 'cell',
  #                                 disable = list(columns = c(0)))
  #   ) |>
  #     DT::formatStyle(columns = c(1,2), fontSize = '12px')
  #
  # })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## GGTREE plot to plotly for interactivity ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$ggtree <- plotly::renderPlotly({

    ladderize <- input$ladderize
    brLength  <- input$brLength
    req(target <- vals$targetSpecies)
    trait_vals <- vals$traits
    trait_data <- traitData()
    isolate(myOG <- vals$myOG)
    (highlight <- vals$highlight)
    (show_legend <- vals$show_legend)
    collapsed_nodes <- vals$collapsed_nodes
    vals_mytree <- vals$mytree
    plotHeight <- vals$plotHeight

    print('plot ggtree')

    # name for png to save
    save_png <- paste0("Target_",gsub(' ','_',tolower(target)),
                       "_phylogeny_", Sys.Date())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Tree
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # get static tree from reactive
    tree_plot <- ggTree()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive Tree
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # make the phylogeny fit in the window
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    max_x <- max(tree_plot$data$x[!is.na(tree_plot$data$x)])
    if (!brLength) {
      # phylogeny
      xrange <- c(-0.01,max_x+
                    nchar(tree_plot$data[grep(max_x,tree_plot$data$x),]$label)/200)
      # max(nchar(tree_plot$data$label))/200)
    } else {
      # cladogram
      xrange <- c(-0.2,RoundUp(max_x,#10))
                               max(nchar(tree_plot$data$label[!is.na(tree_plot$data$label)]))))
    }
    m_margin <- list(t = 80) # gives mode bar space

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # convert ggplot to plotly - note need generic gp_# name so can combine later
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gp_1 <- plotly::ggplotly(tree_plot,tooltip = 'text') |>
      plotly::layout(margin=m_margin,
                     xaxis=list(
                       fixedrange = T, # keep x range same and only allow movement in y
                       autorange=F,
                       range=xrange)
      ) |>
      plotly::style(textposition = "right") |>
      plotly::event_register('plotly_click')

    vals$numCurves <- length(gp_1$x$data)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Heatmaps
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # add traits to tree data
    tree_plot$data <- dplyr::left_join(tree_plot$data,trait_data,
                                       by = c('label'='Species'))

    # find target traits
    target_traits <- tree_plot$data[tree_plot$data$label==target,]
    # use only tip data
    tree_plot_data <- tree_plot$data[tree_plot$data$isTip,]

    # keep track of plots with continuous legends
    legend_continuous <- 0

    # loop over traits when heatmap is added
    if (vals$heatmap) {
      if (!is.null(trait_vals)) {

        # order traits by discrete, continuous type ! important for legends !
        trait_order <- sort((unlist(lapply(tree_plot_data[,trait_vals],typeof))))
        num_discrete   <- sum(trait_order=='character')
        num_continuous <- sum(trait_order!='character')

        sum_len_cat <- 0
        # pos_legd    <- 0
        for (trait in names(trait_order)) {
          indx <- grep(trait,names(trait_order))

          if (typeof(tree_plot_data[[trait]])=='character') {
            num_unq_traits <- length(unique(tree_plot_data[[trait]]))
            # print(num_unq_traits)
            # pos_legd <- pos_legd + 0.12 - ((6-min(num_unq_traits,6))/100)
            # print(pos_legd)
          }

          # flag for discrete or continuous scale and limit discrete category
          # group traits that have more than 5 category
          if (typeof(tree_plot_data[[trait]])=='character') {
            discrete  <- TRUE
            len_cat   <- length(unique(tree_plot_data[[trait]]))
            num_cat       <- tree_plot_data |> dplyr::count(tree_plot_data[[trait]])
            num_cat_order <- num_cat[order(num_cat$n,decreasing = T),]
            if (len_cat>5) {
              shiny::showNotification(ui = paste0('Warning: Number of category for ',
                                                  trait,' exceeds 5.',
                                                  'This means those >5 will be bundled
                                             into one colour/category.'),
                                      duration = 30,closeButton = T,type = 'warning')
              tree_plot_data[tree_plot_data[[trait]] %in%
                               num_cat_order[5:nrow(num_cat_order),][[1]],][[trait]] <- paste0('Others (',trait,')')
            }
          } else {
            discrete <- FALSE
          }

          # add false row so that I can add mock heading for the legend
          if (typeof(tree_plot_data[[trait]])=='character') {
            tree_plot_data2 <- rbind(tree_plot_data,
                                     c(rep(NA,as.numeric(grep('angle',names(tree_plot_data)))),
                                       # names(tree_plot_data)[(grep('angle',names(tree_plot_data))+1):length(names(tree_plot_data))],'.')))
                                       unlist(lapply(names(tree_plot_data)[(grep('angle',names(tree_plot_data))+1):length(names(tree_plot_data))],
                                                     function(x) ifelse(typeof(tree_plot_data[[x]])=='character',paste0('<b> ',x,'</b>'),
                                                                        NA)))))
            tree_plot_data2$y <- as.numeric(tree_plot_data2$y)
          } else {
            tree_plot_data2 <- tree_plot_data
          }

          # handle collapsed nodes
          if (!is.null(collapsed_nodes)) {
            cn <- collapsed_nodes[!duplicated(collapsed_nodes)]
            if (length(cn)>0) {
              cn_data <- tree_plot$data[tree_plot$data$node %in% cn,]
              cn_data$label   <- cn_data$os_count
              tree_plot_data2 <- rbind(tree_plot_data2,cn_data)
            }
          }

          # create a heatmap using geom_tile in ggplot2
          ggplt <- ggplot2::ggplot(tree_plot_data2,
                                   ggplot2::aes(y = `y`,
                                                x = 1,
                                                text=paste('</br>',`trait`,': ',.data[[`trait`]],
                                                           '</br>Species: ',`label`),
                                                fill = .data[[trait]])) +
            ggplot2::geom_tile() +
            ggplot2::theme_minimal()+
            ggplot2::xlab(paste0(`trait`)) +
            ggplot2::ylab("")

          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # add diff colours scales for discrete and continuous with/out legend
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (discrete) {

            len_cat  <- length(unique(tree_plot_data[[trait]]))
            category <- sort(unique(tree_plot_data2[[trait]]))

            # for highlight scale
            if (nrow(num_cat_order)>5) {
              if (target_traits[[trait]][[1]] %in% num_cat_order[5:nrow(num_cat_order),][[1]]) {
                ntrait   <- grep('Others',category)
              } else {
                ntrait   <- grep(target_traits[[trait]][[1]],category)
              }
            } else {
              ntrait   <- grep(target_traits[[trait]][[1]],category)
            }
          }
          if (!show_legend) {
            if (discrete) {
              if (highlight) {
                col_discrete <- rep('#D9D9D9',len_cat)
                col_vec      <- c('white',col_discrete)
                col_vec[ntrait] <- highlight_col
                ggplt <- ggplt +
                  ggplot2::scale_fill_manual(breaks = category,
                                             values = col_vec,
                                             na.value = 'white',
                                             #na.translate = F,
                                             name   = trait)
              } else { # highlight
                col_vec <- c('white',pal_5sec)
                # unname(cbf_2))
                ggplt <- ggplt +
                  ggplot2::scale_fill_manual(breaks = category,
                                             values = col_vec,
                                             na.value = 'white',
                                             #na.translate = F,
                                             name   = trait)
              } # highlight
            } else { # discrete
              ggplt <- ggplt +
                scico::scale_fill_scico(palette = heatmap_pals[1],
                                        begin   = begin_pal,
                                        end     = end_pal,
                                        na.value = 'white',
                                        direction = unname(dir_pal[trait]))
            } # discrete
          } else { # show_legend
            if (discrete) {
              if (highlight) {
                col_discrete <- rep('#D9D9D9',len_cat)
                col_vec      <- c('white',col_discrete)
                col_vec[ntrait] <- highlight_col
                ggplt <- ggplt +
                  ggplot2::scale_fill_manual(breaks = category,
                                             values = col_vec,
                                             na.value = 'white',
                                             #na.translate = F,
                                             name   = trait)
              } else { # highlight
                col_vec <- c('white',
                             pal_5sec[(1+(indx-1)*5):(5+(indx-1)*5)])
                sum_len_cat <- sum_len_cat + len_cat
                ggplt <- ggplt +
                  ggplot2::scale_fill_manual(breaks = category,
                                             values = col_vec,
                                             na.value = 'white',
                                             #na.translate = F,
                                             name   = trait)
              }# highlight
            } else {# discrete
              if (heatmap_pals[indx]=='bilbao') {
                spal <- -1 # to reverse the bilbao scale so red is first
              } else {
                spal <-  1 # all other palettes in correct direction
              }
              ggplt <- ggplt +
                scico::scale_fill_scico(palette = heatmap_pals[indx],
                                        begin   = begin_pal,
                                        end     = end_pal,
                                        na.value = 'white',
                                        direction = unname(dir_pal[trait])*spal)
            } # discrete
          } # show_legend

          if (!show_legend) {
            ggplt <- ggplt +
              ggplot2::theme(legend.position = "none")
          }

          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # add box around target species
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ggplt <- ggplt +
            ggplot2::annotate("rect",
                              xmin=c(0.5),
                              xmax=c(1.5),
                              ymin=c(target_traits$y-0.5),
                              ymax=c(target_traits$y+0.5),
                              colour=mid_fuchsia,
                              fill='transparent',
                              name='target',
                              text=paste('</br>',trait,': ',target_traits[[trait]][[1]],
                                         '</br>Species: ',target_traits$label),
                              size=.5)

          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Interactive: convert to plotly
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ggpltly <- plotly::ggplotly(ggplt,
                                      # legendgroup=grep(trait,names(trait_order))+1,
                                      tooltip = c("text")) |>
            plotly::layout(showlegend = show_legend,
                           legend = list(itemdoubleclick = FALSE,
                                         itemclick=FALSE),
                           # traceorder='grouped'),
                           dragmode='pan',
                           margin=m_margin,
                           xaxis = list(side ="top",
                                        fixedrange = T,
                                        showgrid = FALSE,
                                        zeroline = FALSE,
                                        showline = FALSE,
                                        showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE,
                                        zeroline = FALSE,
                                        showline = FALSE,
                                        showticklabels = FALSE))


          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Combine
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Remove NA from legend when nodes are collapsed
          # https://stackoverflow.com/questions/74127491/hiding-fill-na-from-a-ggplotly-legend
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # for (i in seq_along(ggpltly$x$data)) {
          #   if (length(ggpltly$x$data[[i]]$legendgroup)>0) {
          #     if(ggpltly$x$data[[i]]$legendgroup =="NA" |
          #        ggpltly$x$data[[i]]$legendgroup =="") {
          #       ggpltly$x$data[[i]]$showlegend <- FALSE
          #     }
          #   }
          # }

          for (i in seq_along(ggpltly$x$data)) {
            if (length(ggpltly$x$data[[i]]$name)>0) {
              if(ggpltly$x$data[[i]]$name =="NA" |
                 ggpltly$x$data[[i]]$name =="") {
                ggpltly$x$data[[i]]$showlegend <- FALSE
              }
              # https://stackoverflow.com/questions/59613787/grouping-elements-in-legend-fails-with-ggplotly
              ggpltly$x$data[[i]]$legendgroup <- grep(trait,trait_order)+1
            }
          }

          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # rm hover for annotation
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (typeof(tree_plot_data[[trait]])!='character') {
            ggpltly$x$data[[2]]$hoverinfo <- 'skip'
          } else {
            data_len <- length(ggpltly$x$data)
            ggpltly$x$data[[data_len]]$hoverinfo <- 'skip'
          }
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # change size and location of continuous legend
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (show_legend) {
            if (typeof(tree_plot_data[[trait]])!='character') {

              # legend size
              scale_len_tree <- nrow(tree_plot_data)
              # print(scale_len_tree)
              # add_scale <- 0.1/((scale_len_tree/500)*10)
              len_div <- ceiling((scale_len_tree/500)*10)+1
              # print(len_div)
              # if (scale_len_tree > 100) {
              #   scale_len_tree <- (scale_len_tree/10000)*(-1)
              # } else {
              #   scale_len_tree <- (scale_len_tree/1000)*1
              # }
              # print(scale_len_tree)
              ggpltly[[1]]$data[[3]]$marker$colorbar$len <- ggpltly[[1]]$data[[3]]$marker$colorbar$len/len_div
              # ggpltly[[1]]$data[[3]]$marker$colorbar$len <- 0.1 + add_scale #+ scale_len_tree ??

              # position
              top_legd <- 0.9
              # if (plotHeight <= 1000) {
              #   legd_mul <- 3
              # } else if (plotHeight > 1000 & plotHeight <= 3000) {
              #   legd_mul <- 2
              # } else {
              legd_mul <- 1
              # }
              # legd_mul <- plotHeight/len_div/100
              #legd_mul <- num_discrete+1
              # print(vals$plotHeight)
              if (legend_continuous==0) {
                ggpltly[[1]]$data[[3]]$marker$colorbar$y <- top_legd - num_discrete*(.08*legd_mul)
                #(top_legd/4)*num_continuous #+
                #  ggpltly$x$data[[3]]$marker$colorbar$len/2
              } else {
                ggpltly[[1]]$data[[3]]$marker$colorbar$y <- top_legd - num_discrete*(.08*legd_mul) -
                  #(top_legd/4)*num_continuous -
                  legend_continuous*ggpltly$x$data[[3]]$marker$colorbar$len
              }
              # ggpltly[[1]]$data[[3]]$marker$colorbar$y   <- 1-pos_legd-.05
              # # ggpltly[[1]]$data[[3]]$marker$colorbar$y   <- 1-((indx)*.15)
              # pos_legd <- pos_legd + .12
              legend_continuous <- legend_continuous + 1
            }
          }
          # assign to gp_# variable
          assign(paste0('gp_',grep(trait,names(trait_order))+1),ggpltly)
        } # loop: traits
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # get all plots and subplot - calc widths needed
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        all_plots <- mget(ls(pattern = "gp_"))
        num_hmap   <- length(trait_vals)
        main_width <- 1-num_hmap/10
        remaining_width <- 1-main_width
        width_step <- remaining_width/num_hmap
        other_widths <- rep(width_step,num_hmap)
        # fix last width of heatmap as it's always bigger for some reason
        if (num_hmap!=1) {
          other_widths[num_hmap] <- other_widths[num_hmap]*.8
        }
        map_widths <- c(main_width,other_widths)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # subplot
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        combined_plot <- plotly::subplot(all_plots,
                                         nrows = 1,
                                         widths = map_widths,
                                         shareY = T,
                                         titleX = T,
                                         which_layout = 2)  |>
          plotly::layout(legend=list(title=list(text='Legend')#,
                                     # # add space between legends for diff plots
                                     # tracegroupgap = 50
          )) |>
          plotly::config(displaylogo=F,
                         scrollZoom=T,
                         displayModeBar = TRUE,
                         modeBarButtonsToRemove = c(
                           "sendDataToCloud",
                           "autoScale2d",
                           #"resetScale2d",
                           "toggleSpikelines",
                           "hoverClosestCartesian",
                           "hoverCompareCartesian",
                           #"zoom2d",
                           #"pan2d",
                           "select2d",
                           "lasso2d"#,
                           #"zoomIn2d",
                           #"zoomOut2d"
                         ),
                         toImageButtonOptions= list(filename = save_png))
        # try to add shape to subplot - doesn't work for some reason
        #|>
        # plotly::layout(shape=list(list(type = "rect",
        #                        fillcolor = "transparent",
        #                        line = list(color = mid_fuchsia),
        #                        #opacity = 0.3,
        #                        x0 = .6,
        #                        x1 = 1,
        #                        xref = "x",
        #                        y0 = target_traits$y-0.5,
        #                        y1 = target_traits$y+0.5,
        #                        yref = "y")))
      } else {
        combined_plot <- gp_1 |> # gp_1 is the tree
          plotly::config(displaylogo=F,
                         scrollZoom=T,
                         displayModeBar = TRUE,
                         modeBarButtonsToRemove = c(
                           "sendDataToCloud",
                           "autoScale2d",
                           #"resetScale2d",
                           "toggleSpikelines",
                           "hoverClosestCartesian",
                           "hoverCompareCartesian",
                           #"zoom2d",
                           #"pan2d",
                           "select2d",
                           "lasso2d"#,
                           #"zoomIn2d",
                           #"zoomOut2d"
                         ),
                         toImageButtonOptions= list(filename = save_png))
      }
    } else {
      combined_plot <- gp_1 |> # gp_1 is the tree
        plotly::config(displaylogo=F,
                       scrollZoom=T,
                       displayModeBar = TRUE,
                       modeBarButtonsToRemove = c(
                         "sendDataToCloud",
                         "autoScale2d",
                         #"resetScale2d",
                         "toggleSpikelines",
                         "hoverClosestCartesian",
                         "hoverCompareCartesian",
                         #"zoom2d",
                         #"pan2d",
                         "select2d",
                         "lasso2d"#,
                         #"zoomIn2d",
                         #"zoomOut2d"
                       ),
                       toImageButtonOptions= list(filename = save_png))
    }
    # rm(list = ls(pattern = "gp_"))
    combined_plot$x$source <- "ggtree" # to fix click event for subplot
    combined_plot |> plotly::layout(dragmode='pan')

  }) # end ggtree plot

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Map ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #https://stackoverflow.com/questions/49538465/changing-leaflet-markercluster-icon-color-inheriting-the-rest-of-the-default-cs
  #https://stackoverflow.com/questions/33600021/leaflet-for-r-how-to-customize-the-coloring-of-clusters
  #https://github.com/Leaflet/Leaflet.markercluster
  output$map <- leaflet::renderLeaflet({
    print('map')

    (cntry        <- vals$country)
    (species_map1 <- vals$species_map1)
    (species_map2 <- vals$species_map2)

    min_zoom  <-   2
    max_zoom  <-  18

    req(geo_data <- geoData())
    # if (!is.null(cntry)) {
    geo_data <- geo_data[country %in% cntry,]
    # }
    # print(geo_data)

    if (!is.null(geo_data) & nrow(geo_data)!=0) {

      # if (!is.null(species_map)) {
      #   print(species_map)
      #   # print(strsplit(species_map,'/'))
      #   # spec <- strsplit(species_map,'/')[[1]][2]
      #   # cnty <- strsplit(species_map,'/')[[1]][1]
      #   # geo_data <- geo_data[species %in% spec &
      #   #                             country %in% cnty,]
      #   geo_data <- geo_data[map_filter_col %in% species_map,]
      # }

      if (!is.null(species_map1)) {
        # print(species_map1)
        geo_data1 <- geo_data[species %in% species_map1,]
        # geo_data1 <- geo_data[map_filter_col %in% species_map1,]
        geo_data1$years <- substr(geo_data1$eventDate,1,4)
      }

      if (!is.null(species_map2)) {
        # print(species_map2)
        geo_data2 <- geo_data[species %in% species_map2,]
        # geo_data2 <- geo_data[map_filter_col %in% species_map2,]
        geo_data2$years <- substr(geo_data2$eventDate,1,4)
      }

      # format the year
      # years <- sapply(geo_data$eventDate, function(x) {
      #   if (is.na(x)) {
      #     ""
      #   } else {
      #     substr(x, 1, 4)  # Extract the first 4 characters (the year)
      #   }
      # })
      #
      # geo_data$years <- substr(geo_data$eventDate,1,4)
      # # print(years)

      # # Create a color palette based on unique values in the "species" column
      # unique_scientific_names <- unique(geo_data$species)
      # color_palette <- colorFactor("Set1", domain = unique_scientific_names)

      # calculate the values from geo_data
      buffer_degrees <- 1.0
      if (!is.null(species_map1) & is.null(species_map2)) {
        minLat <- min(geo_data1$decimalLatitude) - buffer_degrees
        maxLat <- max(geo_data1$decimalLatitude) + buffer_degrees
        minLon <- min(geo_data1$decimalLongitude) - buffer_degrees
        maxLon <- max(geo_data1$decimalLongitude) + buffer_degrees
        vLat   <- mean(geo_data1$decimalLatitude)
        vLon   <- mean(geo_data1$decimalLongitude)
      } else if (!is.null(species_map2) & is.null(species_map1)) {
        minLat <- min(geo_data2$decimalLatitude) - buffer_degrees
        maxLat <- max(geo_data2$decimalLatitude) + buffer_degrees
        minLon <- min(geo_data2$decimalLongitude) - buffer_degrees
        maxLon <- max(geo_data2$decimalLongitude) + buffer_degrees
        vLat   <- mean(geo_data2$decimalLatitude)
        vLon   <- mean(geo_data2$decimalLongitude)
      } else if (!is.null(species_map1) & !is.null(species_map2)) {
        minLat <- min(geo_data1$decimalLatitude,
                      geo_data2$decimalLatitude) - buffer_degrees
        maxLat <- max(geo_data1$decimalLatitude,
                      geo_data2$decimalLatitude) + buffer_degrees
        minLon <- min(geo_data1$decimalLongitude,
                      geo_data1$decimalLongitude) - buffer_degrees
        maxLon <- max(geo_data1$decimalLongitude,
                      geo_data2$decimalLongitude) + buffer_degrees
        vLat   <- mean(geo_data1$decimalLatitude)
        vLon   <- mean(geo_data1$decimalLongitude)
      } else {
        minLat <- -90
        maxLat <- 90
        minLon <- -180
        maxLon <- 180
        vLat   <- 0
        vLon   <- 0
      }

      map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = min_zoom,
                                                                maxZoom = max_zoom#,
                                                                # attributionControl = FALSE
      )) |>
        # leaflet::addTiles() |>  # Add the default base map
        leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Positron,
                                  # group = "Carto",
                                  options = leaflet::providerTileOptions(zIndex=-10))

      if (!is.null(species_map1)) {
        map <- map |> leaflet.extras::addHeatmap(data = geo_data1,
                                                 lng = ~decimalLongitude,
                                                 lat = ~decimalLatitude,
                                                 group = ~species,
                                                 gradient = '#007377',
                                                 radius = 8,   # circle size
                                                 minOpacity = .4,
                                                 max = 50,
                                                 cellSize = 1, # no merging
                                                 blur = 1)
      }

      if (!is.null(species_map2)) {
        map <- map |> leaflet.extras::addHeatmap(data = geo_data2,
                                                 lng = ~decimalLongitude,
                                                 lat = ~decimalLatitude,
                                                 gradient = '#6D2077',
                                                 radius = 8,
                                                 minOpacity = .4,
                                                 group = ~species,
                                                 cellSize = 1,
                                                 max = 50,
                                                 blur = 1)
      }

      # add legend
      if (!is.null(species_map1) & is.null(species_map2)) {
        map <- map |>
          leaflet::addLegend(position = 'bottomright',
                             colors   = c('#007377'),
                             labels   = c(species_map1))
      } else if (!is.null(species_map2) & is.null(species_map1)) {
        map <- map |>
          leaflet::addLegend(position = 'bottomright',
                             colors   = c('#6D2077'),
                             labels   = c(species_map2))
      } else if (!is.null(species_map1) & !is.null(species_map2)) {
        map <- map |>
          # https://roh.engineering/posts/2021/05/map-symbols-and-size-legends-for-leaflet/
          # leaflet::addLegend(position = 'bottomright',
          #                    pal      = color_palette,
          #                    values   = geo_data$species)
          leaflet::addLegend(position = 'bottomright',
                             colors   = c('#007377','#6D2077'),
                             labels   = c(species_map1,species_map2))
      }

      map <- map |>
        # leaflet::addCircleMarkers(data  = geo_data,
        #                  lng   = ~decimalLongitude,
        #                  lat   = ~decimalLatitude,
        #                  popup = ~paste0(species, "<br>",
        #                                       years, "<br>",
        #                                       country),
        #                  fillOpacity = 0.7,
        #                  stroke      = FALSE,  # Set marker outlines to transparent
        #                  fillColor = ~color_palette(species),
        #                  clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T,
        #                                                        # freezeAtZoom = 10,
      #                                                        removeOutsideVisibleBounds = T,
      #                                                        disableClusteringAtZoom = 8,
      #                                                        zoomToBoundsOnClick = T)
      #                  ) |>
      leaflet::setView(lng = vLon, lat = vLat, zoom = min_zoom) |>
        leaflet::fitBounds(lng1 = minLon, lat1 = minLat, lng2 = maxLon, lat2 = maxLat) |>
        # leaflet::setMaxBounds(lng1 = minLon, lat1 = minLat, lng2 = maxLon, lat2 = maxLat) |>
        # leaflet::setMaxBounds(lat1 = -90,lat2 = 90,lng1 = -180,lng2 = 180) |>
        leaflet::addScaleBar(position = "bottomleft") |>

        # Add a function to zoom back out
        leaflet::addEasyButton(leaflet::easyButton(
          icon="fa-globe", title="Zoom to Level 2",
          onClick=leaflet::JS("function(btn, map){ map.setZoom(2); }"))) |>

        # Add a locate button
        leaflet::addEasyButton(leaflet::easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=leaflet::JS("function(btn, map){ map.locate({setView: true}); }"))) |>

        ## Add Print functionality for Leaflet

        # https://github.com/rowanwins/leaflet-easyPrint
        # https://github.com/trafficonese/leaflet.extras2/blob/master/inst/examples/easyprint_app.R
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = "Save map",
            position = "topleft",
            sizeModes = list("CurrentSize"),
            defaultSizeTitles = list(
              "CurrentSize" = "The current map extent"#,
              # "A4Landscape" = "A4 (Landscape) extent",
              # "A4Portrait" = "A4  (Portrait) extent"
            ),
            exportOnly = TRUE,
            hideControlContainer = FALSE,
            # hide the buttons when printing
            hideClasses = c('leaflet-control-zoom',
                            'leaflet-control-easyPrint',
                            'easy-button-button'),
            filename = paste0("Species_Occurrence_Map_",
                              gsub(" ","_",species_map1),
                              "_and_",
                              gsub(" ","_",species_map2))))
    } else {
      print('no data')
      map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = min_zoom,
                                                                maxZoom = max_zoom)) |>
        # leaflet::addTiles()
        leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Positron,
                                  # group = "Carto",
                                  options = leaflet::providerTileOptions(zIndex=-10))
    }

  }) # renderLeaflet

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$model <- leaflet::renderLeaflet({
    print('model')
    # print(paste0('model_method: ', input$model_method))
    # print(paste0('model_timespan: ', input$model_timespan))
    # print(paste0('model_species: ', input$model_species))
    # print(paste0('study_group: ', names(species_vec)[as.numeric(input$species)]))

    req(model_species <- input$model_species)
    model_method  <- input$model_method
    species       <- input$species
    target        <- vals$targetSpecies

    if (!demo) {
      model_path <- paste0(data_path,
                           names(species_vec)[as.numeric(species)],
                           "/sdm/" ,
                           model_method,
                           "_results/chelsa2.1_bio/",
                           gsub(" ", "_", model_species), "/")
    }

     if (model_method == "maxent") {

       if (!demo) {
        model_file      <- paste0(model_path, "maxent_predict.grd")
        thresholds_file <- paste0(model_path, "maxent_thresholds.csv")

        # Check if model file and thresholds file exist
        if (!file.exists(model_file) || !file.exists(thresholds_file)) {
          shiny::showNotification("No MaxEnt results found for this species",
                                  type = "warning")
          return(NULL)
        }

        # print(paste0('model_file: ', model_file))
        # print(paste0('thresholds_file: ', thresholds_file))
        thresholds <- utils::read.csv(thresholds_file, header = TRUE)

        model_result_grd <- terra::rast(model_file)
        model_result_grd <- terra::project(model_result_grd, "EPSG:4326")
        model_result_grd <- raster::raster(model_result_grd)

        # Crop raster
        disp_win_wgs84 <- sf::st_sfc(sf::st_point(c(-180, -89)),
                                     sf::st_point(c(180, 89)),
                                     crs = 4326)
        bb <- sf::st_bbox(disp_win_wgs84) #Get the bounding box
        #crop the rasters to the extent
        cropped_model_result_grd <- terra::crop(model_result_grd, bb)

       } else {

         # Check if model file and thresholds file exist
         if (model_species!=target) {
           shiny::showNotification("No MaxEnt results found for this species",
                                   type = "warning")
           return(NULL)
         }

         thresholds       <- demo_thresholds
         # Convert to spatRaster
         # model_result_grd <- methods::as(demo_maxent,'SpatRaster')

         cropped_model_result_grd <- demo_maxent

       }

        selected_columns <- thresholds |>
          dplyr::select('kappa', 'spec_sens', 'equal_sens_spec',
                        'sensitivity_0.9') |>
          tidyr::pivot_longer(cols = tidyr::everything(),
                              names_to = "threshold",
                              values_to = "value") |>
          dplyr::arrange(value)

        # Extract values and labels from the data
        values <- selected_columns$value
        labels <- selected_columns$threshold

        min_zoom  <-  2
        max_zoom  <-  18

        maxent_colors <- leaflet::colorBin(palette = c("transparent",
                                                       "#FFFF00", "#FFA500",
                                                       "#FF0000", "#FF0000"),
                                           domain = c(0, values, 1),
                                           na.color = "transparent",
                                           alpha = TRUE,
                                           reverse = FALSE,
                                           bins = 5)
        # Plot MaxEnt results
        leaflet::leaflet(options = leaflet::leafletOptions(minZoom = min_zoom,
                                                           maxZoom = max_zoom)) |>
          leaflet::setView(lng = 133, lat = -25, zoom = 4) |>
          leaflet::addTiles() |>
          leaflet::addRasterImage(cropped_model_result_grd,
                                  colors = maxent_colors,
                                  opacity = 0.8,
                                  project = TRUE) |>
          leaflet::addLegend(pal = maxent_colors,
                             values = terra::values(cropped_model_result_grd),
                             title = "Presence probability",
                             position = "bottomleft") |>

          # Add a function to zoom back out
          leaflet::addEasyButton(leaflet::easyButton(
            icon="fa-globe", title="Zoom to Level 2",
            onClick=leaflet::JS("function(btn, map){ map.setZoom(2); }"))) |>

          # Add a locate button
          leaflet::addEasyButton(leaflet::easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=leaflet::JS("function(btn, map){ map.locate({setView: true}); }"))) |>

          ## Add Print functionality for Leaflet
          leaflet.extras2::addEasyprint(
            options = leaflet.extras2::easyprintOptions(
              title = "Save map",
              position = "topleft",
              sizeModes = list("CurrentSize"#,
              ),
              defaultSizeTitles = list(
                "CurrentSize" = "The current map extent"#,
              ),
              exportOnly = TRUE,
              hideControlContainer = FALSE,
              # hide the buttons when printing
              hideClasses = c('leaflet-control-zoom',
                              'leaflet-control-easyPrint',
                              'easy-button-button'),
              filename = paste0("Species_SDM_MaxEnt_Map_",
                                gsub(" ","_",model_species)
              )))

      } else if (model_method == "climatch") {

        if (!demo) {

        model_file <- paste0(model_path, "climatch_predict.grd")

        # Check if model file exists
        if (!file.exists(model_file)) {
          shiny::showNotification("No CLIMATCH results found for this species",
                                  type = "warning")
          return(NULL)
        }

        print(paste0('model_file: ', model_file))

        model_result_grd <- terra::rast(model_file)
        model_result_grd <- terra::project(model_result_grd, "EPSG:4326")
        model_result_grd <- raster::raster(model_result_grd)

        # Crop raster
        disp_win_wgs84 <- sf::st_sfc(sf::st_point(c(-180, -89)),
                                     sf::st_point(c(180, 89)),
                                     crs = 4326)
        bb <- sf::st_bbox(disp_win_wgs84) #Get the bounding box
        #crop the rasters to the extent
        cropped_model_result_grd <- terra::crop(model_result_grd, bb)

        } else {

          # Check if model file exists
          if (model_species!=target) {
            shiny::showNotification("No CLIMATCH results found for this species",
                                    type = "warning")
            return(NULL)
          }

          # Convert to spatRaster
          # model_result_grd <- methods::as(demo_clim,'SpatRaster')

          cropped_model_result_grd <- demo_clim

        }

        min_zoom  <-  2
        max_zoom  <-  18

        climatch_colors <- leaflet::colorBin(palette = c("darkgreen",
                                                         "chartreuse4",
                                                         "chartreuse2",
                                                         "yellow",
                                                         "gold1",
                                                         "orange",
                                                         "orange3",
                                                         "red",
                                                         "red4",
                                                         "purple4"),
                                             domain = c(0, 1, 2, 3, 4, 5,
                                                        6, 7, 8, 9, 10),
                                             na.color = "transparent",
                                             alpha = TRUE,
                                             reverse = FALSE,
                                             bins = 10)

        # Plot CLIMATCH results
        leaflet::leaflet(options = leaflet::leafletOptions(minZoom = min_zoom,
                                                           maxZoom = max_zoom)) |>
          leaflet::setView(lng = 133, lat = -25, zoom = 4) |>
          leaflet::addTiles() |>
          leaflet::addRasterImage(cropped_model_result_grd,
                                  colors = climatch_colors,
                                  opacity = 0.8,
                                  project = TRUE) |>
          leaflet::addLegend(pal = climatch_colors,
                             values = terra::values(cropped_model_result_grd),
                             title = "CLIMATCH score",
                             position = "bottomleft") |>

          # Add a function to zoom back out
          leaflet::addEasyButton(leaflet::easyButton(
            icon="fa-globe", title="Zoom to Level 2",
            onClick=leaflet::JS("function(btn, map){ map.setZoom(2); }"))) |>

          # Add a locate button
          leaflet::addEasyButton(leaflet::easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=leaflet::JS("function(btn, map){ map.locate({setView: true}); }"))) |>

          leaflet.extras2::addEasyprint(
            options = leaflet.extras2::easyprintOptions(
              title = "Save map",
              position = "topleft",
              sizeModes = list("CurrentSize"),
              defaultSizeTitles = list(
                "CurrentSize" = "The current map extent"),
              exportOnly = TRUE,
              hideControlContainer = FALSE,
              # hide the buttons when printing
              hideClasses = c('leaflet-control-zoom',
                              'leaflet-control-easyPrint',
                              'easy-button-button'),
              filename = paste0("Species_SDM_CLIMATCH_Map_",
                                gsub(" ","_",model_species)
              )))

      }

    # } else { # demo
    #
    #   leaflet::leaflet() |>
    #     leaflet::addTiles()
    #
    # }

  }) # renderLeaflet

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Report table ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$report_table <- DT::renderDataTable({
    print('report table')

    # Get data from the reactive function
    trait_data <- traitData()

    # Ensure the columns exist in the trait_data
    if (!all(c("Degree of Sep.", "Patristic Dist.") %in% names(trait_data))) {
      warning("Columns 'Degree of Sep.' or 'Patristic Dist.' not found in trait_data.")
      return(NULL)
    }

    # Create the report table
    report_table <- trait_data |>
      dplyr::select('Species', 'Degree of Sep.', 'Patristic Dist.') |>
      dplyr::arrange('Patristic Dist.')

    # Render the data table
    DT::datatable(report_table,
                  extensions = 'Buttons',
                  options = list(
                    pageLength = 100,
                    dom = 'Bfrtip',
                    buttons = list(
                      list(
                        extend = 'csv',
                        text = 'Download CSV',
                        filename = paste0("PhyloControl_test_list_",
                                          gsub(' ','_',vals$targetSpecies),
                                          "_", Sys.Date()),
                        title = NULL
                      )
                    ),
                    autoWidth = TRUE
                  ))
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## How to Files table ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$howto_files <- DT::renderDT({
    
    # Define the data frame
    howto_files_data <- data.frame(
      File = c("{study_group}.tre", "{study_group}_traits.csv", "{study_group}_default_target.txt",
               "{study_group}_occurrences.csv", "sdm/climatch_results", "sdm/maxent_results"),
      Tab = c("Phylogeny", "Phylogeny", "Phylogeny", "Map", "Model", "Model"),
      Requisite = c("Required", "Optional", "Optional", "Optional", "Optional", "Optional"),
      Description = c(
        "The phylogenetic tree data in the text-based Newick format. Can be created via the Quarto notebooks.",
        "Table of trait data, usually categorical, but not limited to, with a column for each trait. The first column should have the header Species_ and the rows should contain all the species in the tree as scientific names with underscores between the genus and species. All character names (column headers) and character states (variables in rows) must be unique. There can be blanks or NAs in the file. This file is manually created by the user.",
        "Text file defining the target species name to highlight in the app. If not present, the app will default to the first species in the phylogeny (alphabetically).",
        "Occurrence data with the latitude and longitude coordinates in decimal degrees. Minimum columns required are species (scientific name with space between genus and species), decimalLatitude, decimalLongitude, and country. This file should be created via the Quarto notebooks to ensure correct format.",
        "Species distribution modelling files using the CLIMATCH methods. Key files are the climatch_predict.grd files for each species modelled. Can be created via the Quarto notebooks.",
        "Species distribution modelling files using the MaxEnt methods. Key files are the maxent_predict.grd files for each species and maxent_thresholds.csv file. Can be created via the Quarto notebooks."
      ),
      stringsAsFactors = FALSE
    )
    
    # Render interactive DataTable
    DT::datatable(
      data     = howto_files_data,
      rownames = FALSE,
      options  = list(
        dom = 't',                # Table only, no search or pagination
        autoWidth = TRUE,          # Adjust column width automatically
        columnDefs = list(
          list(width = '250px', targets = 0)  # Set column width for first column (File)
        ),
        pageLength = 5             # Display 5 rows per page if pagination is enabled
      )
    )
    
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 8. Download handlers ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## For exporting data in app ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$exportData <- downloadHandler(
    filename = function() {
      paste("Target_",gsub(' ','_',tolower(vals$targetSpecies)),
            "_traitData_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      prepCSV(traitData(),vals$list_nodes,file)
    }
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## For exporting tree in app ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$exportTree <- downloadHandler(
    filename = function() {
      paste(names(species_vec)[as.numeric(input$species)],
            "_target_",gsub(' ','_',tolower(vals$targetSpecies)),
            "_phylogeny_", Sys.Date(),
            ".tre",
            sep="")
    },
    content = function(file) {
      ape::write.tree(vals$mytree,file=file)
    }
  )

}
