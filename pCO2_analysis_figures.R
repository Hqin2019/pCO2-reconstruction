# Guizhi Wang data
# 2019-10-19

# Codes for figures
# 2020-06
 
#setwd()

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


## Climatology of Remote Sensing Data

clim_rs=rowMeans(data_rs,na.rm=TRUE)

### Plot climatology with NaN

Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution

par(mfrow=c(1,1))
plot(clim_rs, type="l")

summary(clim_rs, na.rm=TRUE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#359.9   413.1   416.1   415.6   419.2   457.8     143 

mapmat=matrix(clim_rs,nrow=26)
mapmat=pmax(pmin(mapmat,480),220)
int = seq(220,480,length.out=41)
rgb.palette = colorRampPalette(c('black','blue','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')

par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording

filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim = c(109,122),ylim=c(5,25),
               plot.title=title(main = bquote(paste(italic(p),CO[2],~"Summer Climatology: RS Data 2000-2014")),
                                xlab = expression(paste("Longtitude (", degree,"E)")),
                                ylab = expression(paste('Latitude (', degree,'N)')),
                                cex.lab=1.3),
               plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                 map('world2', add=TRUE);grid()},
               key.title=title(main = expression(paste("(", mu, "atm)"))),
               key.axes={axis(4, cex.axis=1.5)})


## Standard Deviation of Remote Sensing Data

sd_rs=rowSds(data_rs,na.rm=TRUE)

### Plot standard deviation of remote sensing data with NaN

plot(sd_rs, type="l")

summary(sd_rs, na.rm=TRUE)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.096   4.297   5.464   5.931   7.088  30.197     143 

mapmat=matrix(sd_rs,nrow=26)
mapmat=pmax(pmin(mapmat,12),2)
int=seq(2,12,length.out=41)
rgb.palette=colorRampPalette(c('black','blue','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')

par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording

filled.contour(Lon, Lat, mapmat, color.palette = rgb.palette, levels=int,
               xlim = c(109,122),ylim = c(5,25),
               plot.title=title(main = bquote(paste(italic(p),CO[2],~"Summer Standard Deviation: RS Data 2000-2014")),
                                xlab = expression(paste("Longtitude (", degree,"E)")),
                                ylab = expression(paste('Latitude (', degree,'N)')),
                                cex.lab=1.3),
               plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                 map('world2', add=TRUE);grid()},
               key.title=title(main=""),
               key.axes={axis(4, cex.axis=1.5)})


## Figure 4. Remote sensing derived sea surface pCO2 in summer over the period of 2000-2017.

## Summer RS data pCO2 animation by 15 frames from 2000 to 2014

labels <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)")
years_rs <- seq(t1, t2, length=ntime)

### set up an empty frame, then add points one by one
par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording

for(i in 1:ntime) {
  mapmat=matrix(data_rs[,i],nrow=26)
  #mapmat=pmax(pmin(mapmat,450,na.rm=TRUE),250, na.rm=TRUE)
  mapmat=pmax(pmin(mapmat,480),220)
  int=seq(220,480,length.out=41)
  rgb.palette=colorRampPalette(c('black','blue','green', 
                                 'yellow','pink','red','maroon'),interpolate='spline')
  #mapmat= mapmat[,seq(length(mapmat[1,]),1)]
  #pdf("pCO2_summer_rs_data.pdf", width = 8, height = 8)
  filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
                 xlim = c(109,122),ylim=c(5,25),
                 width = 480, height = 550,
                 plot.title=title(main = bquote(paste("Summer RS",~italic(p),CO[2] )),
                                  xlab = expression(paste("Longtitude (", degree,"E)")),
                                  ylab = expression(paste('Latitude (', degree,'N)')),  
                                  cex.lab = 1.2, cex.main = 1.4),
                 plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                   map('world2', add=TRUE);grid()},
                 key.title=title(main = expression(paste("(", mu, "atm)"))),
                 key.axes={axis(4, cex.axis=1.5)})
  legend("topleft", col= "black", legend = NA, title = paste(labels[i], years_rs[i]), cex=1.5, bty = "n")
  ani.record() # is: function (reset = FALSE, replay.cur = FALSE) 
}

### Now we can replay it, with an appropriate pause between frames:

### Smaller interval means faster animation. Default: interval=1

oopts = ani.options(interval = 0.5, 
                    ani.width=450, 
                    ani.height=550,
                    title="Summer pCO2 RS Data Animation")

### Animate the frames in the plot window of R Studio

ani.replay() 

### Show the animation on an HTML page

saveHTML(ani.replay(), img.name = "pCO2Summer_RS_animation")

### Create mp4 movie
imgs <- list.files(path="images",pattern="pCO2Summer_RS.*\\.png")

saveVideo({
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }
})


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


## Figure 6(d). EOFs of the remote sensing derived pCO2 data

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


##  Write the EOF data with header and in .csv

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


## Figure 3. In situ observation pCO2 data in the SCS in summer of 2000-2017.

years_obs = c(2000,2001,2004:2009,2012,2014:2017)

## set up an empty frame, then add points one by one
par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording

for (i in 1:t2) {
  mapmat=matrix(data_obs1[,i],nrow=26)
  mapmat=pmax(pmin(mapmat,480),220)
  int=seq(220,480,length.out=41)
  rgb.palette=colorRampPalette(c('black','blue', 'green', 
                                 'yellow','pink','red','maroon'),interpolate='spline')
  filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
                 xlim = c(109,122),ylim=c(5,25),
                 width = 480, height = 550,
                 plot.title=title(main = bquote(paste("Summer Observed",~italic(p),CO[2] )),
                                  xlab = expression(paste("Longtitude (", degree,"E)")),
                                  ylab = expression(paste('Latitude (', degree,'N)')), 
                                  cex.lab=1.3, cex.main=1.4),
                 plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                   map('world2', add=TRUE);grid()},
                 key.title=title(main = expression(paste("(", mu, "atm)"))),
                 key.axes={axis(4, cex.axis=1.5)})
  legend("topleft", col= "black", legend = NA, title = paste(labels[i], years_obs[i]), cex=1.5, bty = "n")
  ani.record() # is: function (reset = FALSE, replay.cur = FALSE) 
}

## Now we can replay it, with an appropriate pause between frames:
## Smaller interval means faster animation. Default: interval=1

oopts = ani.options(interval = 0.5, 
                    ani.width=450, 
                    ani.height=550,
                    title="Summer pCO2 Observed Data Animation")

#Animate the frames in the plot window of R Studio
ani.replay() 

## Show the animation on an HTML page
saveHTML(ani.replay(), img.name = "pCO2SummerObserved")

#Create mp4 movie
imgs <- list.files(path="images",pattern="pCO2SummerObserved.*\\.png")

saveVideo({
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }  
})


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
#[1]   4  12  70  16  26 230  67 139  84  91  36  34  76

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



## Figure 7. Reconstructed pCO2 fields in the SCS. 

n = 13  #13 recon summer pCO2s
idn = 1040
nt = idn*n
reconmat = matrix(rep(NaN,nt),ncol=n)
reconmat[mar3,] = reconfield_smr

### set up an empty frame, then add points one by one
par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording
for (i in 1:n) {
  mapmat=matrix(reconmat[,i],nrow=26)
  mapmat=pmax(pmin(mapmat,480),220)
  int=seq(220,480,length.out=41)
  rgb.palette=colorRampPalette(c('black','blue', 'green', 
                                 'yellow','pink','red','maroon'),interpolate='spline')
  filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
                 xlim = c(109,122),ylim=c(5,25),
                 width = 480, height = 550,
                 plot.title=title(main=bquote(paste("Summer Reconstructed",~italic(p),CO[2] )),
                                  xlab = expression(paste("Longtitude (", degree,"E)")),
                                  ylab = expression(paste('Latitude (', degree,'N)')), 
                                  cex.lab=1.4, cex.main=1.4),
                 plot.axes={axis(1, cex.axis=1.3); axis(2, cex.axis=1.3);
                   map('world2', add=TRUE);grid()},
                 key.title=title(main=expression(paste("(", mu, "atm)"))),
                 key.axes={axis(4, cex.axis=1.5)})
  legend("topleft", col= "black", legend = NA, title = paste(labels[i], years_obs[i]), cex=1.5, bty = "n")
  ani.record() # is: function (reset = FALSE, replay.cur = FALSE) 
}

## Now we can replay it, with an appropriate pause between frames:
## Smaller interval means faster animation. Default: interval=1
oopts = ani.options(interval = 0.5, 
                    ani.width=450, 
                    ani.height=550,
                    title="Summer pCO2 Reconstructed Data Animation"
)

#Animate the frames in the plot window of R Studio
ani.replay() 

#Show the animation on an HTML page
saveHTML(ani.replay(), img.name = "pCO2SummerRecon_animation")

#Create mp4 movie

imgs <- list.files(path="images",pattern="pCO2.*\\.png")

saveVideo({
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }  
})

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
