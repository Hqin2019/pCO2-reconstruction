# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

## load remote sensing data and observed data.
load(file="data/RSdata.RData")
load(file="data/OBdata.RData")
load(file="data/UNAN.RData")

## Climatology of Remote Sensing Data
clim_rs=rowMeans(data_rs,na.rm=TRUE)
summary(clim_rs, na.rm=TRUE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#359.9   413.1   416.1   415.6   419.2   457.8     143 

## Standard Deviation of Remote Sensing Data
sd_rs=rowSds(data_rs,na.rm=TRUE)
summary(sd_rs, na.rm=TRUE)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.096   4.297   5.464   5.931   7.088  30.197     143 

t1 = 2000
t2 = 2014
ngrid = 1040 #number of grid boxes 
ntime = t2-(t1-1) #15 years
Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution
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

##  Write the EOF data with header
eofnames=1:ntime

for (i in 1:ntime){
  eofnames[i]=paste("EOF",i, sep="")
}

EOFs=u00
colnames(EOFs) <- eofnames

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

for (i in 1:13) {
  v=which(complete.cases(data_ocean[,i])) #the boxes with data
  n_mode[i]=length(v)-1 
}
n_mode


k1 = min(n_mode) 
k2 = 8 #set the number of modes to be 8 except the first year

#Generate the lm formula 

recon=matrix(0,nrow=n_ocean,ncol=13)
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

for (i in 2:13) {
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

