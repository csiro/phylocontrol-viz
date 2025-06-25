### How to Use

Check out the demo at <https://shiny.csiro.au/phylocontrol-viz-demo/>. The demo uses a set of curated data for the target weed *Erigeron bonariensis* (flaxleaf fleabane).

Install or clone repo.

```         
# In console

remotes::install_git("address", git = "external")
```

```         
# In terminal or use 'New Project' method

git clone {address}
```

#### Setup

1.  Using the demo within app:

To run app with demo curated *Erigeron* data:

-   Use `phylocontrol.viz::run_app(demo=TRUE)` or use the 'Run App' option from `app.R` with variable `run_demo = TRUE`.
-   Press <i class="fas fa-circle-question"></i> Help to run the walk-through.

Note: The demo within the app is limited. To explore full functionality download DAP data or create own data using Quarto notebooks.

2.  Use own data:

There are three ways to get data:

-   Run the [Quarto notebooks](https://github.com/csiro/phylocontrol-geninput) to create the minimum files to run the app; or,
-   Download curated *Erigeron* data on the [CSIRO Data Access Portal](https://doi.org/10.25919/21fr-hk78); or,
-   Generate your own data in the format shown below.

Before running the app, put the data in a folder structure like: (\* *Optional*)

````         
```         
  - dir_path/ 
    - {study_group1}/
      - {study_group1}.tre
      - {study_group1}_traits.csv *
      - {study_group1}_cell_counts.csv *
      - {study_group1}_cell_overlap.csv *
      - {study_group1}_default_target.txt *
      - {study_group1}_occurrences.csv *
      - sdm/ *
        - climatch_results *
        - maxent_results *
    - {study_group2}/
      - {study_group2}.tre
      - ...
    - ...
```
````

Each study group name must be unique and used in each of the file names within the directory. Check the example <i>Erigeron</i> data for an example of data folder structure and file naming.

Pass the path your data is located to the `dir_path` parameter in `phylocontrol.viz::run_app(dir_path  ="set path here", demo = FALSE)` or set variable in `app.R` and press 'Run App'.

#### Files

Inputs for visualisation with the PhyloControl R Shiny application.
