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

load(file="data/data_rs.RData")

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


