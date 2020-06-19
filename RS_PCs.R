# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)


## load remote sensing data from 2000 to 2014: Summer
load(file="data/RSdata.RData")

t1 = 2000
t2 = 2014
ngrid = 1040 #number of grid boxes 
ntime = t2-(t1-1) #15 years

## Climatology of Remote Sensing Data
clim_rs=rowMeans(data_rs,na.rm=TRUE)

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


### EOF matrices with NaN for map plotting.
svd00=svd(clim_rs_std_val) #SVD for the matrix w/o NaN
#SVD does not work for a matrix with NaN
u00=svd00$u
v00=svd00$v
d00=svd00$d

var=d00^2/ntime
cvar=cumsum(var)

## Figure 6(d). PCs of the remote sensing derived pCO2 data
### Plot PCs
plot.new()
par(new = "TRUE",las = 1,cex.axis = 1)
plot(2000:2014, -v00[,1], type='l',lwd=2.0,col='black', 
     cex.axis=1.2, cex.lab=1.2,ylim=c(-0.7,0.7),
     xlab="Year", ylab="Scale", lty=1)
lines(2000:2014, v00[,2], type='l',lwd=2.0,col='red',lty=3)
lines(2000:2014, v00[,3], type='l',lwd=2.0,col='blue',lty=5)
legend("topleft", col= "black", legend = NA, title = "(d)", cex=1.5, bty = "n")
legend(2010,0.75, col=c("black","red","blue"),lty=c(1,3,5),lwd=2.0,
       legend=c(paste(' PC1:', round(100*var[1]/cvar[ntime],digits=2), '%'), 
                paste(' PC2:', round(100*var[2]/cvar[ntime],digits=2), '%'), 
                paste(' PC3:', round(100*var[3]/cvar[ntime],digits=2), '%')),
       bty="n",text.font=2,cex=1.2, text.col=c("black","red","blue"))
