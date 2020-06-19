# install.packages() and load the package
library(matrixStats)
library(maps)
library(animation)
library(reshape2)
library(ggplot2)
library(readxl)
library(scales)

#Load reconstruction results.
load(file="data/reconsmr.RData")

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
years_obs = c(2000,2001,2004:2009,2012,2014:2017)
time2=years_obs - 2000 
model_spatial <-  lm(space_ave ~ time2)
summary(model_spatial) # Model for Figure 8(a)
# 2.383   #trend 2.383/year 
#Coefficients:
#(Intercept)        time2  
#370.856        2.383 


## Model for Figure 8(b)
### Summer sea surface pCO2 at Station HOT .
### loaddata
load(file="data/HOT.RData")
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


