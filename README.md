
# marr paper evaluations

This repository contains scripts to reproduce all results, tables, and figures in our paper applying the `marr` procedure.
`Paper (in preparation)`

> Ghosh, Tusharkanti, et al. “Reproducibility of Mass Spectrometry based
> Metabolomics Data”

## Contents

To install MSPrep from GitHub, write the following R (>=4.0) commands:
```{r}
library(devtools)
install_github("KechrisLab/MSPrep",ref="feature-generalize")
```
There are two subfolders in this repository: 1) Simulations and 2) Real_Data.

1) In the Simulations folder, there are 5 subfolders. They are described below:

a) Folder 'marr_code_A45' description- There are 12 settings for this simulation design. The scripts 'SIM_B1.R',.. , 'SIM_C1.R',..., 'SIM_D4.R' can generate the corresponding R objects 'B1_sim_results.Rdata',..,'C1_sim_results.Rdata',...,'D4_sim_results.Rdata'. 

Folders b) Folder 'marr_code_A99' and c) Folder 'marr_code_B' descriptions- These folders are similar to a) Folder 'marr_code_A45'. 

d) Folder 'Output' description- In this folder, there are 3 subfolders (MaRRsimA45, MaRRsimA99 and MaRRsimB,) and 3 R scripts ('SIMPLOT_A45.R', 'SIMPLOT_A99.R' and 'SIMPLOT_B.R') that generate the plots for 3 simulation designs.

e) The MaRR functions ('source_MaRR.R') can be sourced from the folder 'src'.

2) In the Real_Data folder, there are 7 subfolders.

a) Folder 'Output' description- In this folder, there is one pdf file which summarizes all the plots and tables (including supplementary materials from the paper) for different pre-processing steps: Imputation- BPCA and kNN and Normalization- quantile, median and RUV. Total 6 pre-processing steps.

b) Folder 'Plot_R_Scripts' description- In this folder, there are 12 R scripts (6 for Operator(Tech) data and 6 for Charmion (BioTech) and Metabolon (Bio) combined) that generate the plots present in the 'Output' folder. These plots are generated based on the saved R objects (present in the 'saved_objects' folder).

c) Folder 'R_Scripts' - In this folder, there are 18 R scripts, i.e., 6 (6 pre-processing steps) R scripts for each data set.

d) Folder 'saved_objects' description- In this folder, there are 18 saved R objects generated from the R scripts of "R_Scripts" folder.

e) Folder 'data' description- This folder contains all the data objects needed to source in all the R scripts (i.e., 18 R scripts) of the folder "R_Scripts" to generate R objects residing in "saved_objects" folder.

f) Folder 'Table_R_Scripts' description- In this folder, there are 6 R scripts for each of the 6 pre-processing steps. These R scripts also generate the latex code using the package "xtable" in the same order as shown in the tables of MaRR_real_results.pdf ('Output' folder). These tables are generated based on the saved R objects (present in the 'saved_objects' folder).

g) The MaRR functions ('source_MaRR.R') can be sourced from the folder 'src'.


## Benchmarking datasets

(Data files for the benchmarking datasets are available from [Metabolomics Workbench]  (https://www.metabolomicsworkbench.org).

Project ID: PR000438 (BioTech dataset) 

Project ID: PR000907 (Bio dataset)


## `marr` R/Bioconductor package 

marr (Maximum Rank Reproducibility) is a nonparametric approach that detects reproducible signals using a maximal rank statistic for high-dimensional biological data. In this R package, we implement functions that measures the reproducibility of features per sample pair and sample pairs per feature in high-dimensional biological replicate experiments. The user-friendly plot functions in this package also plot histograms of the reproducibility of features per sample pair and sample pairs per feature. Furthermore, our approach also allows the users to select optimal filtering threshold values for the identification of reproducible features and sample pairs based on output visualization checks (histograms).

## Installation of `marr` package

The `marr` package is available from [Bioconductor](https://bioconductor.org/packages/release/bioc/html/marr.html). The stable release version can be installed using the Bioconductor installer as follows. Note that installation requires R version 4.0 or later.

```{r}
# Install Bioconductor installer from CRAN if not already installed
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

# Install 'marr' package from Bioconductor
BiocManager::install("marr")
```


For details on the development version of the `marr` package, see the [GitHub page](https://github.com/Ghoshlab/marr).

## Tutorial and examples

For a tutorial and examples of usage, see the Bioconductor [package vignette](https://bioconductor.org/packages/release/bioc/vignettes/marr/inst/doc/MarrVignette.html) (link also available via the main Bioconductor page for the [marr package](http://bioconductor.org/packages/release/bioc/html/marr.html)).

