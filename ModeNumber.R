# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

# Summer pCO2 data analysis for remote sensing data 2000-2014

## Read remote sensing data from 2000 to 2014: Summer

t1 = 2000
t2 = 2014
ngrid = 1040 #number of grid boxes 
ntime = t2-(t1-1) #15 years
rsname=1: ntime
for (i in 1: ntime){
  rsname[i] = paste("pCO2_rs_summer", 1999+i, ".txt", sep="")
}

### Generate a matrix as the data holder: 1040X15 for (i in 1:ntime)

data_rs = matrix(0, nrow=ngrid, ncol=ntime)
for (i in 1:ntime){
  dat = read.table(rsname[i], header=TRUE)
  data_rs[,i] = dat[,3]
}
dim(data_rs)
#[1] 1040   15
data_rs[1:2,1:4] #This is the remote sensing pCO2 data from 2000 to 2014
#[,1]   [,2]   [,3]   [,4]
#[1,] 408.86 419.31 420.29 421.91
#[2,] 411.80 418.42 419.97 421.91


## Data Pre-processing.

### Remove the available values at the same observation point if there is NaN  

for (i in 1:ntime) {
  n = which(data_rs[ ,i] == "NaN")
  data_rs[n,] <- NA
}

### Remove NA and obtain data with only values

mar1 = is.na(data_rs[,3]) #This is from one of the RS data
length(mar1)
#[1] 1040  the number of True and False entries

mar2 = which(mar1 == TRUE) #renders which positions with NaN
length(mar2)
#[1] 143  the number of NaN rows

mar3 = which(mar1 == FALSE) #renders which positions with values
length(mar3) 
#[1] 897  the number of boxes with values
#897 + 143 = 1040  = 40X26 grid boxes

da0014 = na.omit(data_rs)#remove NaN and leave only values in the matrix da0014
dim(da0014)
#[1] 897   15  # 897 rows 
#897 + 143 = 1040 rows total = 40X26

### Lat and Lon of the grid

data_rs_1=read.table(rsname[1], header=TRUE)

latlon=data_rs_1[,1:2]
dim(latlon)
#[1] 1040    2

latlon_val=latlon[mar3,]
dim(latlon_val)
#[1] 897   2  #latlon for 897 value boxes

latlonnan=latlon[mar2,]
dim(latlonnan)
#[1] 143   2 #latlon for 143 NaN boxes

RSdata = cbind(latlon, data_rs)
write.csv(RSdata,file="~/RSdata.csv") #write data in .csv
clim_rs=rowMeans(data_rs,na.rm=TRUE)

### Plot climatology with NaN

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

UNaN=matrix(0,nrow=1040, ncol=ntime)
UNaN[mar2,]=NaN #mar2 is the NaN rows

for(i in 1:897){
  for (j in 1: ntime ){
    UNaN[mar3[i],j] = u00[i,j] #mar3 is the value rows
  }
}


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
