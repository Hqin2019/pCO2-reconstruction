# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

#Set up for plotting Figure 3.
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

Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution

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
#End of the set up for plotting Figure 3.


## Figure 3. In situ observation pCO2 data in the SCS in summer of 2000-2017.
labels <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)")
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

