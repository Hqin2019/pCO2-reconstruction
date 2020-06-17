# Feasibility of reconstructing the basin-scale sea surface partial pressure of carbon dioxide from sparse in situ observations over the South China Sea
## Descrption of the repository
This repo contains replication codes for graphs and other supplementary documentation for Wang et al. Some other information about this project: data sets availability, copyright information, and credits.

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

A copy of the GNU General Public License is available at http://www.gnu.org/licenses/.

## Data and data treatment
Underway sea surface pCO2 data from 35 cruises ranging from 2000-2017 were compiled in this study. Observed sea surface pCO2 in the SCS is mainly distributed on the northern shelf and in the northern basin. Since monthly data are hardly available for each year starting from 2000, we have compiled the data seasonally in which the spring season includes March-May, the summer season is June-August, the fall season is September-November, and the winter season includes December and the next January and February.

## Contents for R codes
Each R code can be run individully. Before running the code, all data files need to be under the same working directory as R locally. Use `getwd()` to check the current working directory and use `setwd(dir)` to change it. The code `pCO2_analysis_figures.R` produces all analysis and figures is also included.
- The code `SummerOB.R` replicates Figure 3.
- The code `SummerRS.R` replicates Figure 4.
- The code `ModeNumber.R` replicates Figure 5.
- The code `SummerRSEOFs.R` replicates Figure 6(a)(b)(c).
- The code `RS_PCs.R` replicates Figure 6(d).
- The code `Reconstrcuted_pCO2.R` replicates the reconstruction implementation and Figure 7 mentioned in the paper.
- The code `Reconstructed_TS.R` replicates Figure 8.
