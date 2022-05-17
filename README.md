# Longitudinal
### Analyzing longitudinal data in R example, 2022.

Includes data manipulation, checking test assumptions, fitting a mixed-effects model, evaluating general linear hypotheses, generating and comparing control data, false discovery rate (FDR) and false coverage-statement rate (FCR) corrections, and data visualization. Within- and between-participant comparisons are made.

This script utilizes a single outcome variable measured at multiple timepoints, but contains everything needed to be applied to multiple variables. Most of its contents can be modified easily to construct a batch processing pipeline.

## Required R packages: 
[suddengains](https://cran.r-project.org/package=suddengains), [reshape2](https://cran.r-project.org/package=reshape2), [dplyr](https://cran.r-project.org/package=dplyr), [lme4](https://cran.r-project.org/package=lme4), [ggplot2](https://cran.r-project.org/package=ggplot2), [glmmTMB](https://cran.r-project.org/package=glmmTMB), [sjPlot](https://cran.r-project.org/package=sjPlot), [MASS](https://cran.r-project.org/package=MASS), [lattice](https://cran.r-project.org/package=lattice), [cowplot](https://cran.r-project.org/package=cowplot), [ggpubr](https://cran.r-project.org/package=ggpubr), [multcomp](https://cran.r-project.org/package=multcomp), [viridis](https://cran.r-project.org/package=viridis)

Beyond R, no downloads are required before starting. All downloads—including retrieving sample data—occur within [the R script](https://github.com/AlexandraBatzdorf/Longitudinal/blob/main/Analysis_Script.R).

## Compatibility:  
Tested on a Mac running OS Monterey 12.1 using R version 4.2.0. All packages are hosted on CRAN and are expected to be compatible with a variety of operating systems.
