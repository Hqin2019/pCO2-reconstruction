# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)


## load observed data. 
load(file="data/OBdata.RData")

Lat=seq(5.25, 24.75, len=40) #0.5-by-0.5 deg resolution
Lon=seq(109.25,121.75,len=26) #0.5-by-0.5 deg resolution


## Figure 3. In situ observation pCO2 data in the SCS in summer of 2000-2017.
labels <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)")
years_obs = c(2000,2001,2004:2009,2012,2014:2017)

## set up an empty frame, then add points one by one
par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording
plot.new()
for (i in 1:13) {
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

