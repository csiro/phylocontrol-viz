# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Home Tab ----
# (sourced in global.R)
#
# Description: The user interface for the body of home tab.
#
# Author: Louise Ord, Lauren Stevens
# Date: 2021-03-03
#
# Last Modified by: Stephanie Chen
# Date: 2026-05-27
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# 1. Tab ----
# ~~~~~~~~~~~~~

navbar_tab_home <- function(input,output,session) {

  output$navbar_tab_home <- renderUI({

    bslib::page_fluid(
      br(),
      div(HTML(paste0("<p style='text-align: center;'>",
                      "<img src='img/graphic_transparent.png' style='width:40%;'/>",
                      "</p>"
      ))),
      br(),
      bslib::navset_underline(
        bslib::nav_panel(
          title = 'Introduction',
          br(),
          HTML(paste0(
            "<p style='font-size: 14px; text-align: left;'>",
            "This package was made using <code>{golem}</code> and you are running PhyloControl version ",
            as.character(golem::pkg_version()),
            ".",
            "</p>",
            "<br/>"
          )),
          htmltools::includeMarkdown(path = app_sys('app/www/intro.md')),

          HTML(paste0(
            br(),
            h5("Shiny Dashboard Development"),
            # h4("Data collection"),
            # h4("How to Use the Tool"),
            HTML(paste0("<p style='font-size: 14px'>",
                        "Louise Ord (CSIRO IMT, Eveleigh), ",
                        "Lauren Stevens (CSIRO IMT, Clayton)",
                        "</p>",
                        br())),
            
            h5('Author contributions'),
            HTML(paste0("<p style='font-size: 14px'>",
                        "Stephanie Chen (CSIRO NCMI, Canberra), ",
                        "Lauren Stevens (CSIRO IM&T, Clayton), ",
                        "Nunzio Knerr (CSIRO NCMI, Canberra), ",
                        "Ben Gooden (CSIRO H&B, Canberra), ",
                        "Michelle Rafter (CSIRO H&B, Dutton Park), ",
                        "Pete Thrall (CSIRO NCMI, Canberra), ",
                        "Louise Ord (CSIRO IM&T, Eveleigh), and ",
                        "Alexander Schmidt-Lebuhn (CSIRO NCMI, Canberra).",
                        "</p>",
                        "<p style='font-size: 14px;'>",
                        "Alexander Schmidt-Lebuhn, Ben Gooden, and Michelle Rafter conceptualised the project. Stephanie Chen and Nunzio Knerr prepared and tested the Quarto notebooks.  The base framework concept and code were originally developed by Louise Ord with conversion to bslib done by Lauren Stevens. The core PhyloControl application code (Phylogeny and Map tabs) was written and implemented by Lauren Stevens based on the approach and design conceptualised by Louise Ord. Advice on solutions and implementation strategies for technical challenges were provided by Louise Ord and Lauren Stevens. The Model tab was created and developed by Nunzio Knerr and Stephanie Chen. The Report tab was created by Stephanie Chen. Alexander Schmidt-Lebuhn wrote the original functions for calculating degrees of separation and patristic distance. The Erigeron datasets used in this paper were prepared by Stephanie Chen, Nunzio Knerr, and Alexander Schmidt-Lebuhn. Stephanie Chen led the writing of the manuscript. All authors contributed critically to the drafts and gave final approval for publication.",
                        "</p>",
                        br())),
            
            h5('Attribution'),
            HTML(paste0("<p style='font-size: 14px;'>",
                        "The PhyloControl paper is published in <i>Biological Control</i>. DOI: ",
                        "<a href='https://doi.org/10.1016/j.biocontrol.2025.105859' target='_blank'>10.1016/j.biocontrol.2025.105859</a>.",
                        "</p><br>",
                        br()))
          ))

        ),
        bslib::nav_panel(
          title = 'How To Use',
          br(),
          htmltools::includeMarkdown(path = app_sys('app/www/howto.md')),
          DT::DTOutput('howto_files'),
          br(),br()),
        bslib::nav_panel(
          title = 'Licence',
          br(),
          HTML(paste0(
            br(),
            h4('Disclaimer'),
            HTML(paste0("<p style='font-size: 14px'>",
                        "Using this software developed by CSIRO researchers does not
                  give the user approval to use the CSIRO name and logo. The
                  designations employed and the presentation of materials in
                  this website do not imply the expression of any opinion on
                  the part of CSIRO concerning the legal status of any country,
                  area or territory or of its authorities, or concerning the
                  delimitation of its borders. The depiction and use of
                  boundaries, geographic names and related data shown on maps
                  and included in lists, tables, documents, and databases on
                  this website are not warranted to be error free nor do they
                  necessarily imply official endorsement or acceptance by CSIRO.
                  To the extent permitted by law, CSIRO does not accept any
                  liability to any person arising from using this tool and/or
                  any information or material on it.",
                        "</p>",
                        br())),
            h4("Licence"),
            HTML(paste0("<p style='font-size: 14px'>",
                        "GNU General Public Licence v3 (GPLv3). See licence_GPLv3.txt
                  for full details.",
                        br(),
                        "</p>"))
          ))
        ),
        bslib::nav_panel(
          title = 'References',
          br(),
          h4('Third Party Software'),
          p('PhyloControl was developed using the following software and
            their dependencies:'),
          br(),
          HTML(paste0("W. Chang, J. Cheng, J. Allaire, C. Sievert, B. Schloerke, Y. Xie, J.
          Allen, J. McPherson, A. Dipert, B. Borges. shiny: Web Application
          Framework for R. R package version 1.7.4, 2022. https://CRAN.Rproject.
          org/package=shiny",
          br(),br(),
          "C. Sievert, J. Cheng, G. Aden-Buie. _bslib: Custom 'Bootstrap'
          'Sass' Themes for 'shiny' and 'rmarkdown'_. R package version 0.8.0, 2024.
          <https://CRAN.R-project.org/package=bslib>.",
          br(),br(),
          "H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-
          Verlag New York, 2016.",
          br(),br(),
          "C. Sievert. Interactive Web-Based Data Visualization with R, plotly,
          and shiny. Chapman and Hall/CRC Florida, 2020.",
          br(),br(),
          "G. Yu, D. Smith, H. Zhu, Y. Guan, T.T. Lam. ggtree: an R package
          for visualization and annotation of phylogenetic trees with their covariates
          and other associated data. Methods in Ecology and Evolution,
          8(1), 28-36, 2017. doi:10.1111/2041-210X.12628",
          br(),br(),
          "E. Paradis, K. Schliep. ape 5.0: an environment for modern phylogenetics
          and evolutionary analyses in R. Bioinformatics, 35, 526-528,
          2019. doi:10.1093/bioinformatics/bty633. Version: 5.6-2",
          br(),br(),
          "J. Cheng, B. Schloerke, B. Karambelkar, Y. Xie. _leaflet: Create
          Interactive Web Maps with the JavaScript 'Leaflet' Library_. R package
          version 2.2.2, 2024. <https://CRAN.R-project.org/package=leaflet>.",
          br(),br(),
          "T. Pedersen, F. Crameri. scico: Colour Palettes Based on the Scientific
          Colour-Maps_. R package version 1.3.1, 2022. https://CRAN.R-project.
          org/package=scico",
          br(),br(),
          "C. Fay, V. Guyader, S. Rochette, C. Girard. _golem: A Framework for
          Robust Shiny Applications_. R package version 0.5.1, 2024.
          <https://CRAN.R-project.org/package=golem>.")),
          br(),br()
        )
      )
    )

  })

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
