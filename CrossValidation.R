# Guizhi Wang data
# 2019-10-19

# Codes for cross validation 
# 2020-12

#setwd("~/data")

# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)
library(dplyr)
library(Metrics)

## load remote sensing data and observed data.
load(file="data/RSdata.RData")
load(file="data/OBdata.RData")
load(file="data/UNaN.RData")

t1 = 2000
t2 = 2014
ngrid = 1040 #number of grid boxes 
ntime = t2-(t1-1) #15 years
Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution
labels <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)")
years_rs <- seq(t1, t2, length=ntime)
years_obs = c(2000,2001,2004:2009,2012,2014:2017)

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

##  Write the EOF data with header
eofnames=1:ntime
for (i in 1:ntime){
  eofnames[i]=paste("EOF",i, sep="")
}
EOFs=u00
colnames(EOFs) <- eofnames

# Reconstruction

## Compute the standardized anomalies of the observed data with RS climatology and standard deviation

data_obs_anom = (data_obs1 - clim_rs) / sd_rs

data_ocean = data_obs_anom[mar3,]#data over the ocean only

n_ocean = length(mar3) #mar3 are the row numbers of remote sensing data over ocean: 897 boxes 
n_ocean 
#[1] 897

### We intend to reconstruct the data on the 897 ocean boxes

### Determine the max number of modes that can be used and is equal to the 
### number of observed data boxes minus one: k1

n_mode=c()

for (i in 1:13) {
  v=which(complete.cases(data_ocean[,i])) #the boxes with data
  n_mode[i]=length(v)-1 
}

n_mode
#[1]   4  12  70  16  26 230  67 139  84  91  36  34  76

k1 = min(n_mode) 
k2 = 8 #set the number of modes to be 8 except the first year


## Cross Validation 

### data_ocean is the standardized anomalies of the observed data with RS climatology and standard deviation
### data_obs_anom = (data_obs1 - clim_rs) / sd_rs
### data_ocean = data_obs_anom[mar3,]#data over the ocean only
### The above codes are already implemented in the previous reconstruction steps, I cite here for better umderstanding the flow. 

### Determine the max number of modes that can be used and is equal to the 
### number of observed data boxes minus one 

### For the first year, we use 4 modes and for the following years, we use 8 modes.
### In CVs, we have to insist on the umber of modes, so that we cannot remove any observation from the first year.
### Here, we use year 2006 with 25 observations in the CVs. 
n_mode[5] ; years_obs[5] 
# [1] 24 
# 2006

# the number of obs are as follows:  5  13  71  17  25 231  68 138  84  90  37  28  77
 #  rmse for each year 

rmse_cv = c() 

for (i in 2:13){ # n = 13 years, we skip the CV for the first year as the reason described above.
  x = length(which(complete.cases(data_ocean[,i]))) # number of observations in year i
  recon_cv = matrix(0, nrow=n_ocean, ncol=x) # without one of the observations
  obs_data = c()
  cv_data = c()
  for (j in 1:x) { 
    v1=which(complete.cases(data_ocean[,i])) #the boxes with data
    v=v1[-j] #exclude the v[j] observation
    datr=data_ocean[v,i] #Observed data
    eofr=EOFs[v,eofnames[1:k2]]
    df=data.frame(eofr, datr) #regression data
    reg=lm(formula=datr ~ EOF1+EOF2+EOF3+EOF4+EOF5+EOF6+EOF7+EOF8, data=df)#regression with EOFs.
    coe=reg$coefficients
    c1=rep(1,n_ocean)
    res=cbind(c1,EOFs[,eofnames[1:k2]])
    recon_cv[, j]=res%*%coe   # reconstruction without observation v[j]
    
    obs_data[j] = data_ocean[v1[j], i]     
    cv_data[j] = recon_cv[v1[j], j] 
    rmse_cv[i] = rmse(obs_data, cv_data) # compute rmse 
  }
}

rmse_cv

table_cv <- cbind(years_obs, rmse_cv, n_mode+1)#Making the rmse table with years and corresponding obs.
table_cv

colnames(table_cv)[3] <- c("number of obs")
mean(rmse_cv[2:13])
