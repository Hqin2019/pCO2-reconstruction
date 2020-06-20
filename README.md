# Feasibility of reconstructing the basin-scale sea surface partial pressure of carbon dioxide from sparse in situ observations over the South China Sea
## Descrption of the repository
This repo contains replication codes for graphs and other supplementary documentation for Wang et al (2020). 

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

A copy of the GNU General Public License is available at http://www.gnu.org/licenses/.

## Data and data treatment
This repository includes two datasets in the `data` folder: `RSdata.csv` and `OBdata.csv`. They are the input data for the R codes for the paper of Wang et al. (2020). 

The file `OBdata.csv` contains the in situ summer sea surface pCO2 data from 2000 to 2017 in scattered 0.5º×0.5º grid boxes in the (5-25º N, 109-122º E) region that covers most of the South China Sea. The summer data are the June-August mean for each year in 2000-2017 excluding 2002, 2003, 2010, 2011 and 2013. Thus, the observed in situ pCO2 data are for 13 summers in 2000-2017. 

The dataset `RSdata.csv` is the satellite remote-sensing derived sea surface pCO2 for the summers of 2000-2014, also gridded with a 0.5º×0.5º resolution in the (5-25º N, 109-122º E) region.

The original annual data is avaliable to download from XMU's library at https://dspace.xmu.edu.cn/handle/2288/174342.


## Contents for R codes
Each R code can be run individully. The code `pCO2_analysis_figures.R` produces all analysis and figures.
- The code `SummerOB.R` replicates Figure 3.
- The code `SummerRS.R` replicates Figure 4.
- The code `ModeNumber.R` replicates Figure 5.
- The code `SummerRSEOFs.R` plots EOF1, EOF2, EOF3 and replicates Figure 6(a)(b)(c).
- The code `RS_PCs.R` replicates Figure 6(d).
- The code `Reconstrcuted_pCO2.R` replicates the reconstruction implementation and Figure 7 in the paper.
- The code `Reconstructed_TS.R` replicates Figure 8.


## Results tables
The `tables` folder contains the reconstructed results, EOFs, and the data for each figure in the paper. 
- The `Variance.csv` is for Figure 5.
- The `PCs.csv` and EOFs are for Figure 6.
- The `reconstructed_results.csv` is a complete results for the reconstruction implementation.

