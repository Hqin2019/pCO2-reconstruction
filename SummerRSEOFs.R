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

## Figure 6(a)(b)(c). EOFs of the remote sensing derived pCO2 data

### Plot EOFs with NaN

### Plot EOF1

plot(UNaN[,1], type="l", ylim=c(-0.1,0.1))
summary(UNaN[,1], na.rm=TRUE)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.04520 -0.03971 -0.03549 -0.03035 -0.02569  0.02975      143  

mapmat=matrix(UNaN[,1],nrow=26)
mapmat=pmax(pmin(mapmat,0.1),-0.1)
int=seq(-0.1,0.1,length.out=41)
rgb.palette = colorRampPalette(c('black','blue','green', 
                                 'yellow','pink','red','maroon'),interpolate='spline')

plot.new()
par(mar=c(4.5,4.5,2,5.5))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim = c(109,122),ylim=c(5,25),
               plot.title=title(main= expression(paste("Summer ", italic(p),CO[2], ~"RS Data EOF1")),
                                xlab = expression(paste("Longtitude (", degree,"E)")),
                                ylab = expression(paste('Latitude (', degree,'N)')), 
                                cex.lab=1.3),
               plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                 map('world2', add=TRUE);grid()},
               key.title=title(main=paste("Scale")),
               key.axes={axis(4, cex.axis=1.5)})
legend("topleft", col= "black", legend = NA, title = "(a)", cex=1.5, bty = "n")

### Plot EOF2

plot(UNaN[,2], type="l", ylim=c(-0.1,0.1))
summary(UNaN[,2], na.rm=TRUE)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.09499 -0.02412  0.00342 -0.00459  0.01853  0.06893      143 

mapmat=matrix(UNaN[,2],nrow=26)
mapmat=pmax(pmin(mapmat,0.1),-0.1)
int=seq(-0.1,0.1,length.out=41)

plot.new()
par(mar=c(4.5,4.5,2,5.5))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim = c(109,122),ylim=c(5,25),
               plot.title=title(main = expression(paste("Summer ", italic(p),CO[2], ~"RS Data EOF2")),
                                xlab = expression(paste("Longtitude (", degree,"E)")),
                                ylab = expression(paste('Latitude (', degree,'N)')),   cex.lab=1.3),
               plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"),
               key.axes={axis(4, cex.axis=1.5)})
legend("topleft", col= "black", legend = NA, title = "(b)", cex=1.5, bty = "n")

### Plot EOF3

plot(UNaN[,3], type="l", ylim=c(-0.1,0.1))
summary(UNaN[,3], na.rm=TRUE)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.08976 -0.02652 -0.00052 -0.00369  0.02032  0.07935      143

mapmat=matrix(UNaN[,3],nrow=26)
mapmat=pmax(pmin(mapmat,0.1),-0.1)
int=seq(-0.1,0.1,length.out=41)
rgb.palette=colorRampPalette(c('black','blue','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')

plot.new()
par(mar=c(4.5,4.5,2,5.5))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim = c(109,122),ylim=c(5,25),
               plot.title=title(main = expression(paste("Summer ", italic(p),CO[2], ~"RS Data EOF3")),
                                xlab = expression(paste("Longtitude (", degree,"E)")),
                                ylab = expression(paste('Latitude (', degree,'N)')),   cex.lab=1.3),
               plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"),
               key.axes={axis(4, cex.axis=1.5)})
legend("topleft", col= "black", legend = NA, title = "(c)", cex=1.5, bty = "n")
