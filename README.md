# BCIS-PCI-relative-survival

R script to replicate analysis steps for study on relative survival in BCIS-PCI registry. Relies heavily on the [tidyverse](https://www.tidyverse.org/) and on the [relsurv](https://CRAN.R-project.org/package=relsurv) package (see paper [here](https://doi.org/10.1016/j.compbiomed.2007.04.010))

Launch binder here: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/wjchulme/BCIS-PCI-relative-survival/bindertest)

## Replication instructions

Run files in this order:
- `data preliminaries.R` imports life-table data from ONS and processes (synthetic) BCI-PCI registry data.
- `models.R` Stratifies data by year, sex, and indication and runs relative survival analysis on each stratum. Includes smoothed hazard over time  + smoothed hazard ratio over age.
- `tables... .R` and `figures.R` return tables and figures from objects created in first two files.

- `data synthetic.R` creates synthetic BCIS-PCI registry data using methods from multiple imputation - see the [sythpop package](https://CRAN.R-project.org/package=synthpop) and [accompanying paper](dx.doi.org/10.18637/jss.v074.i11). This allows the analysis steps to be replicated without disclosing real registry data (albeit providing different results).
