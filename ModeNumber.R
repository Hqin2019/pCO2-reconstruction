# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

# Summer pCO2 data analysis for remote sensing data 2000-2014

## load remote sensing data from 2000 to 2014: Summer

load(file="data/RSdata.RData")


## Climatology of Remote Sensing Data
clim_rs=rowMeans(data_rs,na.rm=TRUE)

t1 = 2000
t2 = 2014
ngrid = 1040 #number of grid boxes 
ntime = t2-(t1-1) #15 years
Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution


## Standard Deviation of Remote Sensing Data

sd_rs=rowSds(data_rs,na.rm=TRUE)


## Compute EOFs: The EOF patterns show important spatial patterns of pCO2

clim_rs_std = ( data_rs - clim_rs) / sd_rs #Compute the standardized anomalies
dim(clim_rs_std)
#[1] 1040   15 #all the 1040 grid boxes

clim_rs_std_val = na.omit(clim_rs_std) #only the value boxes
dim(clim_rs_std_val)
#[1] 897   15  #897 of data rows, 15 clumons from 2000 to 2014
#897 + 143 = 1040 rows total = 40X26 

### Plot the standardized anomalies of the RS data

plot(c(clim_rs_std_val), type="l")

### EOF matrices with NaN for map plotting.
svd00=svd(clim_rs_std_val) #SVD for the matrix w/o NaN
#SVD does not work for a matrix with NaN
u00=svd00$u
v00=svd00$v
d00=svd00$d


## Figure 5. Variances and the cumulative variances based on the 2000-2014 summer remote sensing pCO2 data

### Plot variances from SVD eigenvalues

var=d00^2/ntime
cvar=cumsum(var)



par(mar=c(4,4.5,0.5,4.5))
plot(1:ntime,100*var/cvar[ntime], type='o',lwd=2.5,col='red', 
     xlab="", ylab="",cex.axis=1.5, cex.lab=1.5)
mtext("Variance [%]",side=2,line=3, cex=1.5, col="red")
mtext("Mode Number",side=1,line=3, cex=1.5, col="black")
axis(2, col="red", col.ticks="red", col.axis="red", cex.axis=1.5)
par(new=TRUE)
plot(1:ntime, 100*cvar/cvar[ntime],type="o",col="blue",
     ylim=c(0,100),
     lwd=2.5,axes=FALSE,xlab="",ylab="", cex.axis=1.5)
axis(4, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1.5)
mtext("Cumulative Variance [%]",side=4,line=3, cex=1.5, col="blue")
