### Welcome to PhyloControl

Phylogenetic distance is a key measure used to develop species lists for host specificity tests that delimit the fundamental and realised host range of candidate weed biocontrol agents. Plant pathogens and insects, even those with broad host ranges, exhibit some degree of phylogenetic conservatism in their host plant associations. Thorough host-specificity testing is crucial to minimise the risk of off-target damage by biocontrol agents to native and economically important plant species. To facilitate this, host test lists need to be developed from an understanding of evolutionary relationships, usually visualised as a phylogenetic tree generated from genetic data, together with plant functional traits and geospatial information. Currently, the process of obtaining a host test list is not standardised, and the manual steps are time-consuming and challenging.

We introduce a user-friendly visualisation tool called PhyloControl to aid researchers in their decision-making during biocontrol risk analysis. PhyloControl integrates taxonomic data, molecular data, spatial data, and plant traits in an intuitive interactive interface, empowering biocontrol practitioners to summarise, visualise and analyse data efficiently.

Comprehensively sampled phylogenetic trees are often unavailable, and older published phylogenies often lack branch resolution and support, which increases uncertainty. PhyloControl includes a workflow implemented through Quarto notebooks in R that allows users to download publicly available DNA sequences and perform phylogenetic analyses. The modular workflow also incorporates species distribution modelling to predict the current and potential extent of target weed species.

PhyloControl will streamline the development of biocontrol host tests lists to support risk analysis and decision making in classical weed biological control.

The PhyloControl visualisation application consists of four main components that can be navigated using tabs displayed on the top bar:

**1) Phylogeny**

The Phylogeny tab displays two components, the phylogeny and heatmaps:

**a)** A phylogenetic tree displaying evolutionary relationships between species based on DNA sequence data. The user can select the phylogeny to be displayed (study group) and nominate the target species from the phylogeny. There are options provided for the phylogeny for re-rooting, collapsing and expanding sections of the tree, and converting it to a cladogram. The phylogeny should be outgroup rooted using the most evolutionarily distant species for correct downstream inferences. Additionally, branch support values can be visualised as shades of grey. Where multiple support values are found in a newick file, the visualisation will show the first set of values.

**b)** Up to four heatmaps as vertical bars may be simultaneously displayed to the right of the phylogenetic tree. Categorical and/or numerical traits provided via a csv file as well as the phylogenetic distance measures degrees of separation and patristic distance, which are calculated in app, may be shown. For categorical data, the discrete colour scale is limited to five categories to facilitate visualisation. 

**2) Map** *(if data available)*

Species occurrence data (from GBIF if generated using the Quarto notebook) is visualised on a world map. Two species at a time can be selected to display occurrence records to assist in the visualisation of spatial overlap. A multiselect menu is available to filter the records to selected countries.

**3) Models** *(if data available)*

The results of species distribution modelling are displayed on a map coloured by MaxEnt presence of probability or CLIMATCH score, depending on the algorithm(s) chosen in the Quarto notebooks.

**4) Report**

A table of species in the tree ranked by the phylogenetic distance measures degrees of separation and patristic distance is available to download as a csv to assist a biocontrol practitioner to create a host test list.

For help on each tab, press <i class='fas fa-circle-question'></i> Help on the navbar.
