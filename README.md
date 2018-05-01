# BCIS-PCI-relative-survival

Code to replicate analysis steps for study on relative survival in BCIS-PCI registry. 

## Replication instructions

Run files in this order:
- `data preliminaries.R` Loads relevant libraries, imports life-table data from ONS and processes BCI-PCI registry data.
- `models.R` Stratifies data by year, sex, and indication and runs relative survival analysis on each stratum. Includes smoothed hazard over time  + smoothed hazard ratio over age.
- `tables... .R` and `figures.R` return tables and figures from objects created in first two files.

- `data synthetic.R` creates synthetic BCIS-PCI registry data using methods from multiple imputation. This allows the analysis steps to be replicated without disclosing real registry data (albeit providing different results).