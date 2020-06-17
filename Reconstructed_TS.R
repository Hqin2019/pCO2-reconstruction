# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

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

## Climatology of Remote Sensing Data
clim_rs=rowMeans(data_rs,na.rm=TRUE)

Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution

summary(clim_rs, na.rm=TRUE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#359.9   413.1   416.1   415.6   419.2   457.8     143 

## Standard Deviation of Remote Sensing Data

sd_rs=rowSds(data_rs,na.rm=TRUE)

summary(sd_rs, na.rm=TRUE)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.096   4.297   5.464   5.931   7.088  30.197     143 

labels <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)")
years_rs <- seq(t1, t2, length=ntime)

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

var=d00^2/ntime
cvar=cumsum(var)

##  Write the EOF data with header
eofnames=1:ntime

for (i in 1:ntime){
  eofnames[i]=paste("EOF",i, sep="")
}

EOFs=u00
colnames(EOFs) <- eofnames

# Summer pCO2 data analysis for insitu data 2000-2017

## Read observed data from 2000 to 2014: Summer

### Gridding by the SOG method

### Step 1: Make a list of file names for the summer obs data files

obsnames=list.files(path = "." , pattern = "pCO2_variance_summer" )
length(obsnames)
#[1] 13 The Summer has 13 files

### Step 2: Read the summer obs data into a single file

t1=1
t2=length(obsnames)
ngrid=1040

#Generate a matrix as the data holder: 1040X13 for (i in 1:ntime)
data_obs=matrix(0, nrow=ngrid, ncol=t2)

for (i in t1:t2){
  dat=read.table(obsnames[i], header=TRUE)
  data_obs[,i] =dat[,8]
}

dim(data_obs)
#[1] 1040   13

## Outlier Detection 

data_obs1=matrix(NA, nrow=ngrid, ncol=t2)
min_obs=c()

for (i in t1:t2){
  sd_obs <- sd(data_obs, na.rm = TRUE)
  min_obs[i] <- mean(data_obs[,i], na.rm = TRUE) - 3*sd_obs 
  data_obs1[which(data_obs[,i] > min_obs[i]), i ] <- data_obs[which(data_obs[,i] > min_obs[i]),i]
}

obslatlon=cbind(latlon,data_obs1)

years_obs = c(2000,2001,2004:2009,2012,2014:2017)


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

for (i in t1:t2) {
  v=which(complete.cases(data_ocean[,i])) #the boxes with data
  n_mode[i]=length(v)-1 
}
n_mode


k1 = min(n_mode) 
k2 = 8 #set the number of modes to be 8 except the first year

#Generate the lm formula 

recon=matrix(0,nrow=n_ocean,ncol=t2)
dim(recon)
#[1] 897   13 #To hold the recon result

for (i in 1) {
  v=which(complete.cases(data_ocean[,i])) #the boxes with data
  datr=data_ocean[v,i] #Observed data
  eofr=EOFs[v,eofnames[1:k1]]
  df=data.frame(eofr, datr) #regression data
  reg=lm(formula=datr ~ EOF1+EOF2+EOF3+EOF4, data=df) 
  #max number of modes in n_mode[1] is 4
  #we use 4 modes in 2000
  coe=reg$coefficients
  c1=rep(1,n_ocean)
  res=cbind(c1,EOFs[,eofnames[1:k1]])
  recon[,i]=res%*%coe
}

for (i in 2:t2) {
  v=which(complete.cases(data_ocean[,i])) #the boxes with data
  #u=data_ocean[v,i]
  datr=data_ocean[v,i] #Observed data
  eofr=EOFs[v,eofnames[1:k2]]
  df=data.frame(eofr, datr) #regression data
  reg=lm(formula=datr ~ EOF1+EOF2+EOF3+EOF4+EOF5+EOF6+EOF7+EOF8, data=df)
  coe=reg$coefficients
  c1=rep(1,n_ocean)
  res=cbind(c1,EOFs[,eofnames[1:k2]])
  recon[,i]=res%*%coe
}

reconfield_smr = recon*sd_rs[mar3] + clim_rs[mar3]
dim(reconfield_smr)
#[1] 897   13 #897 data boxes for 13 summers/13 years
summary(recon)

## Smooth results in reconfield summer

for(i in 2:896){
  for( j in 1:13){
    if (reconfield_smr[i, j] > 480) 
      reconfield_smr[i, j]=(reconfield_smr[ i-1, j]+ reconfield_smr[ i, j ]+ reconfield_smr[ i+1, j ])/3
    else if (reconfield_smr[ i, j ] < 220) 
      reconfield_smr[i, j ]=(reconfield_smr[ i-1, j ]+ reconfield_smr[ i, j ]+ reconfield_smr[ i+1, j ])/3
  }
}

reconlatlon_smr=cbind(latlon_val,reconfield_smr)

# Figure 8. plot the results: space-time averages

gridout = reconlatlon_smr
dim(gridout)
#[1] 897  16 #The 1st column is the grid ID, the 2nd  lat, the 3rd lon, plus 13 years

time_ave=rowMeans(gridout[,3:15]) #Time average 
#Some months have NaN and thus the time ave is NaN for all

area_w=cos((pi/180)*gridout[,1])/sum(cos((pi/180)*gridout[,1]))
#Show area weight variation wrt lat, close to be uniform
plot(area_w, ylim=c(0.0, 0.00113))

wtgrid=area_w*gridout[,3:15] #Area weighted data
space_ave=colSums(wtgrid) #Spatial average

length(space_ave)
#[1] 13
length(time_ave)
#[1] 897


## Model for Figure 8(a)

time2=years_obs - 2000 
model_spatial <-  lm(space_ave ~ time2)
summary(model_spatial) # Model for Figure 8(a)
# 2.383   #trend 2.383/year 
#Coefficients:
#(Intercept)        time2  
#370.856        2.383 


## Model for Figure 8(b)

### Summer sea surface pCO2 at Station HOT .

### read data
HOT_surface_CO2_cal <- read_excel("HOT_surface_CO2_cal.xlsx", sheet = "Summer")
x <- as.Date(HOT_surface_CO2_cal$date[HOT_surface_CO2_cal$`pCO2_insitu(uatm)`>0 & HOT_surface_CO2_cal$date > "2000-01-01"])
y <- HOT_surface_CO2_cal$`pCO2_insitu(uatm)`[HOT_surface_CO2_cal$`pCO2_insitu(uatm)`>0 & HOT_surface_CO2_cal$date > "2000-01-01"]

x_POSIX <- as.POSIXlt(HOT_surface_CO2_cal$date[HOT_surface_CO2_cal$`pCO2_insitu(uatm)`>0 & HOT_surface_CO2_cal$date > "2000-01-01"], format = "%Y-%m-%d")
x_POSIX$year
x_POSIX$mon
x_model <- x_POSIX$year - 100 + (1/12 * (x_POSIX$mon + 1))

HOT_model <- lm(y ~ x_model)
model <- lm(y ~x)
summary(HOT_model)
# 1.976   #trend 1.976/year 
#Coefficients:
#(Intercept)    x_model 
#354.721       1.976 


#Figure 8a. Time series and linear trend of the spatial averages of the reconstructed pCO2 data in the period of 2000-2017. 

#Figure 8b: pco2 trend in the past summers

layout(matrix(c(1,2), 2, 1, byrow = TRUE))
par(mar=c(1,6,2,2))
time1 = years_obs
plot(time1,space_ave,type="b", xlim=c(2000,2018), ylim=c(350,440), yaxt="n", ann = FALSE, cex=1.5, cex.axis = 1.5)
mtext(expression(paste(italic(p),CO[2], ~"(", mu, "atm)")), side= 2, cex= 1.5, line= 3.5 )
axis(2,las=2, cex.axis = 1.5)
abline(lm(space_ave ~ time1), col='red',lwd=2,lty=2) #Plot the trend line
legend( time1[1], 439, text.col='red', legend = expression(paste("Temporal trend: 2.383",~mu, "atm" ,~"per year")),
        bty="n",cex=1.5)
legend( 2000, 432,text.col='red',cex=1.5,
        legend=expression(paste("y = 2.383x + 370.856")), 
        bty="n")
legend( 2000, 425,text.col='red',cex=1.5,
        legend=expression(paste(R^2, " = 44.89%")), 
        bty="n")
legend("topleft", text.col='black', legend = NA,
       title = expression(paste("(a) Spatial Averages of the Reconstructed ", italic(p), CO[2], " Data")),
       bty="n",cex=1.5)

par(mar=c(5.1,6,2,2))
plot(x, y ,  yaxt='n', ann = FALSE,xlim=c(min(as.Date('2000/01/01')),max(as.Date('2018/01/01'))), ylim=c(335,410), cex = 1.5, cex.axis = 1.5)
title(xlab="Year",lwd=2, cex.lab=1.5, cex.axis=1.5)
mtext(expression(paste(italic(p),CO[2], ~"(", mu, "atm)")), side= 2, cex= 1.5, line= 3.5 )
axis(2,las=2, cex.axis = 1.5)
abline(model,col='red',lwd=2,lty=2)
legend(x[1], 401,text.col='red',cex=1.5, 
       legend = expression(paste("y = 1.976x + 354.721")), 
       bty="n")
legend(x[1], 408, text.col='red', 
       legend = expression(paste("Temporal trend: 1.976",~ mu, "atm" ,~" per year")),
       bty="n",cex=1.5)
legend(x[1], 394, text.col='red', 
       legend = expression(paste(R^2," = 68.90%")),
       bty="n",cex=1.5)
legend("topleft", text.col='black', legend = NA,
       title = expression(paste("(b) Sea Surface ", italic(p), CO[2], " at Station HOT")),
       bty="n",cex=1.5, pt.cex=0)


