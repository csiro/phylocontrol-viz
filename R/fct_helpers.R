# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Server Functions ----
# (sourced in server.R)
#
# Description:
#   These require access to reactive values and expressions so
#   are sourced in server.R.
#
# Author: Lauren Stevens, Nunzio Knerr, Alexander Schmidt-Lebuhn
# Date: 2022-10-14
#
# Last Modified by: Lauren Stevens
# Date: 2025-02-03
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Functions:
#
# 1. Plotting
# 1.1. add_scale - adds the colour palette and scale to the heatmaps
#                  based on trait
#
# 2. Dendrograms
# 2.1. descendantlist - to recursively collect descendants of node
# 2.2. degreesofsep - to calculate degrees of phylogenetic separation
# 2.3. calc_patristic_distance - to calculate phylogenetic distance
#
# 3. Tree data
# 3.1. add_labels_for_collapsed_nodes
#
# 4. Spatial
# 4.1 spatial_overlap - calculate spatial overlap between two species
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Plotting ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## 1.1. add_scale
## Description:
##   Add different colour scales to the different trait data.
##   Discrete or Continuous.
## Param:
##   data: the data table from a ggtree object
##   plt: the ggplot to attach the scale to
##   trait: the trait name
##   target: the value of traits for target species
##   highlight: only show the traits that are same as target
# add_scale <- function(data,plt,trait,target,highlight) {
#   if (typeof(data[[trait]])=='character') {
#     # print('Discrete')
#     if (highlight) {
#       len_cat      <- length(unique(data[[trait]]))
#       col_discrete <- rep('#D9D9D9',len_cat)
#       category <- sort(unique(data[[trait]]))
#       ntrait   <- grep(target[[trait]],category)
#       col_vec <- c('white',col_discrete)
#       col_vec[ntrait+1] <- highlight_col
#       plt <- plt +
#         scale_fill_manual(breaks = category,
#                           values = col_vec,
#                           name   = trait)
#     } else {
#       if (trait=='Status') {
#         # status <- unique(data$Status)
#         status <- sort(unique(data$Status),decreasing = T)
#         plt <- plt +
#           scale_fill_manual(breaks=status,
#                             values=c('#007377','#cde0d0','white'), #greens
#                             name=trait)
#       } else if (trait=='Horticultural') {
#         horticult <- sort(unique(data$Horticultural),decreasing = T)
#         plt <- plt +
#           scale_fill_manual(breaks=horticult,
#                             values=c('#768948ff','#dcd7cdff','white'),#greens
#                             name=trait)
#       } else if (trait=='Leaves') {
#         leaves  <- sort(unique(data$Leaves))#,decreasing = T)
#         nleaves <- grep(target$Leaves,leaves)
#         col_pal <- c('white',"#4e705d","#628a76","#91afa0",'white')
#         col_vec <- col_pal
#         col_vec[nleaves] <- highlight_col
#         plt <- plt +
#           scale_fill_manual(breaks=leaves,
#                             values=col_pal,
#                             name=trait)
#       } else if (trait=='Leaf hairs') {
#         leaf_hairs <- sort(unique(data$`Leaf hairs`),decreasing = T)
#         nhairs     <- grep(target$`Leaf hairs`,leaf_hairs)
#         col_pal <- c('#dad7cd', '#a3b18a', '#588157', '#3a5a40',
#                      '#344e41', '#5d7167', '#7d8d85', '#97a49d','white',
#                      '#acb6b1', '#bdc5c1')
#         col_vec <- c(col_pal)
#         col_vec[nhairs] <- highlight_col
#         plt <- plt +
#           scale_fill_manual(breaks=leaf_hairs,
#                             values=col_pal,
#                             name=trait)
#       } else if (trait=='Lifeform') {
#         lifeform  <- sort(unique(data$Lifeform))#,decreasing = T)
#         nlifeform <- grep(target$Lifeform,lifeform)
#         col_pal <- c('white',unname(coolors_pal23))
#         col_vec <- col_pal
#         col_vec[nlifeform] <- highlight_col
#         plt <- plt +
#           scale_fill_manual(breaks=lifeform,
#                             values=col_pal,
#                             name=trait)
#       }
#     } # highlight
#   } else {
#     # print('Continuous')
#     if (trait=='Patristic Dist.') {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = 1) # red-lower
#     } else if (trait=='Degree of Sep.') {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = 1) # red-lower
#     } else if (trait=='Geo. Overlap') {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = -1) # red-higher
#     } else if (trait=='Rank') {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = 1) # red-lower
#     } else if (trait=='Cell Count') {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = 1,
#                          na.value='#9faee5') # red-lower
#     } else {
#       plt <- plt +
#         scale_fill_scico(palette = pal,begin=begin_pal,end = end_pal,direction = -1) # red-higher
#     }
#   } # discrete/continuous
#
#   plt
#
# }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Dendrogram ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.1. descendantlist
# Function to recursively collect descendants of node
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Find the descendants of a node
#'
#' @param thistree a tree of type 'phylo'
#' @param thisnode the node to look at
#'
#' @return thislist
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
descendantlist <- function(thistree, thisnode) {
  if (thisnode <= length(thistree$tip.label))  {
    return (thisnode)
  } else  {
    wherenext <- which(thistree$edge[,1]==thisnode)  # get immediate descendants
    thislist <- NULL
    for (x in 1:length(wherenext)) {
      thislist <- c(thislist, descendantlist(thistree, thistree$edge[wherenext[x],2]))
    }

    return(thislist)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.2. degreesofsep
# Function to calculate degrees of phylogenetic separation between tree tips
# seems to work, but results are, of course and in contrast to phylogenetic
# distances, asymmetric read resulting matrix line-wise: line shows degrees of
# separation of taxa in columns, from perspective of the taxon in the line
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the degrees of separation
#'
#' @param thistree a tree of type 'phylo'
#'
#' @return dos_dt
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
degreesofsep <- function(thistree) {

  dosmatrix <- matrix(0, nrow=length(thistree$tip.label),
                      ncol=length(thistree$tip.label))
  colnames(dosmatrix) <- thistree$tip.label
  rownames(dosmatrix) <- thistree$tip.label

  for (x in 1:length(thistree$tip.label)) {

    # start at present terminal
    prior_y <- x
    # get immediately ancestral node
    y <- thistree$edge[which(thistree$edge[,2]==x),1]
    currentdist <- 0

    # move downtree until root node is found
    while (y != (length(thistree$tip.label)+1)) {
      currentdesc <- which(thistree$edge[,1]==y)
      for (z in 1:length(currentdesc)) {
        if (thistree$edge[currentdesc[z],2]!=prior_y) {
          dosmatrix[x,descendantlist(thistree,
                                     thistree$edge[currentdesc[z],
                                                   2])] <- currentdist
        }
      }
      prior_y <- y
      # get immediately ancestral node
      y <- thistree$edge[which(thistree$edge[,2]==y),1]
      currentdist <- currentdist + 1
    }
    currentdesc <- which(thistree$edge[,1]==y)
    for (z in 1:length(currentdesc)) {
      if (thistree$edge[currentdesc[z],2]!=prior_y) {
        dosmatrix[x,descendantlist(thistree,
                                   thistree$edge[currentdesc[z],
                                                 2])] <- currentdist
      }
    }
  }

  dos_dt <- data.table(dosmatrix)
  row_col <- data.table(dimnames(dosmatrix)[[1]])
  dos_dt <- cbind(row_col,dos_dt)
  # dos_dt[,1] <- gsub(" ","_",dos_dt[,1])
  names(dos_dt)[1] <- 'Species_'
  # return(dosmatrix)
  return(dos_dt)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the patristic distance
#'
#' @author Lauren Stevens, Nunzio Knerr
#' @description
#' Calculate the patristic distance using castor package
#'
#' @param tree a tree of type 'phylo'
#' @param type what nodes to use 'tips', 'nodes', 'all'
#'
#' @importFrom castor get_all_pairwise_distances
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_patristic_distance <- function(tree, type='tips') {

  # number of tips
  Ntips <- length(tree$tip.label)
  # number of internal node
  Nnode <- tree$Nnode

  if (type=='tips') {
    # calculate distances between all tips
    # only_clades <- c(1:Ntips)
    only_clades <- tree$tip.label
  } else if (type=='nodes') {
    # calculate distances between all internal nodes
    only_clades <- c((Ntips+1):(Ntips+Nnode))
  } else if (type=='all') {
    # all nodes
    # only_clades <- NULL
    only_clades <- c(1:(Ntips+Nnode))
  } else {
    stop("Error: type only uses - 'tips','nodes' or 'all'")
  }

  distances <- castor::get_all_pairwise_distances(tree, only_clades)

  if (type=='tips') {
    colnames(distances) <- tree$tip.label
    rownames(distances) <- tree$tip.label
    distancesMordered   <- distances[order(rownames(distances)),
                                     order(rownames(distances))]
  } else {
    distancesMordered <- distances
  }

  distancesMordered <- as.matrix(distancesMordered)

  return(distancesMordered)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Tree ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add labels to collapsed nodes
#'
#' @description
#' Find the number of offspring each node has using tidytree.
#' This will be the new label if that node is collapsed on the tree.
#'
#' @param tree_in .tre file name, ggtree or phylo type
#'
#' @author Lauren Stevens
#' @details
#'   Date: 22 August 2023
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_labels_for_collapsed_nodes <- function(tree_in) {

  # tree
  t_class <- class(tree_in)
  if ('character' %in% t_class) {
    # file name
    mytree  <- ape::read.tree(tree_in)
    tree    <- ggtree::ggtree(mytree)
  } else if ('phylo' %in% t_class) {
    # phylo tree (read in from ape already)
    tree    <- ggtree::ggtree(mytree)
  } else if ('ggtree' %in% t_class) {
    # ggtree tree
    tree <- tree_in
  }

  # data
  data           <- tree$data
  data$os_count  <- ""
  # data$collapsed <- FALSE

  # loop over and find number of offspring - add to data
  for (r in 1:nrow(data)) {
    row <- data[r,]
    if (row$isTip) {
      selected_nodes <- tidytree::offspring(tree, row$parent)$node
    } else {
      selected_nodes <- tidytree::offspring(tree, row$node)$node
    }
    t_offspring    <- data[data$isTip &
                             data$node %in% selected_nodes,]
    data$os_count[r] <- paste(nrow(t_offspring),"species")
  }

  # return tree ?
  # tree$data <- data
  # return(tree)

  # return data
  return(data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
