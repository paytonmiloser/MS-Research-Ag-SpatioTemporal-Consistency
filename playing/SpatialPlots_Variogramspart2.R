#Fixing Spatial Plots from last time and Playing around with stuff again
#By Payton Miloser

#packages
library(sf)
library(sp)
library(gstat)

#Data
space.1 <- read.csv("C://Users//pmilo//OneDrive//Desktop//iowa state university//Masters CC//Data//DixonMadeUp1.csv", header = TRUE)
space.2 <- read.csv("C://Users//pmilo//OneDrive//Desktop//iowa state university//Masters CC//Data//DixonMadeUp2.csv", header=TRUE)
space.3 <- read.csv("C://Users//pmilo//OneDrive//Desktop//iowa state university//Masters CC//Data//DixonMadeUp3.csv", header=TRUE)

#getting spatial objects
space.1.sf <- st_as_sf(space.1, coords = c("x", "y"))
grid.space.1 <- st_make_grid(space.1.sf)
space.2.sf <- st_as_sf(space.2, coords = c("x", "y"))
space.3.sf <- st_as_sf(space.3, coords = c("x", "y"))

#plot of interest
plot(space.1.sf["yield1"],  graticule = TRUE, pch = 15, main= "Year 1, Space 1")

#fitting A variogram
TheVariogram = variogram(yield1~1, data=space.1.sf) #variogram for space 1, yield 1
TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)

FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel, ylim=c(25,36), main="Year 1, Plot 1")
#3 parameter model
#4 for the Matern

#getting correlations? - the Gaussian Model
semivars <- TheVariogram$gamma
correlation.distances <- TheVariogram$dist 

FittedModel
Gau.var <- 26.08557 + 7.39171 #nugget variance (aka the variance at lag 0)
correlation.plot <- data.frame(semivars, correlation.distances)

correlation.plot$maybe.corr <- 1 - (correlation.plot$semivars/Gau.var) 
correlation.plot$backtosemi <- Gau.var*(1 - correlation.plot$maybe.corr)
plot(correlation.plot$correlation.distances, correlation.plot$maybe.corr, xlab="lag distance", ylab="Correlations", main="Correlations under the Gaussian Fit", pch=19, col="purple")

attr(FittedModel, "SSErr") #grabs the SSE from the model I need
#should be able to calc the log-likelihood because the variogram will have a distribution no matter what

#treat n as total number of observations for degrees of freedom (Not variogram points) - total number of parameters
#ignore the weights and use the number of points in the variograms -> PREFERRED n

#Fitting the exponential model
TheVariogramModel <- vgm(psill=0.15, model="Exp", nugget=0.0001, range=5)

FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel, ylim=c(25,36), main="Year 1, Plot 1")
#abline(h=34.8)

FittedModel
Exp.var <- 24.57514 + 10.257 #nugget variance (aka the variance at lag 0)
correlation.plot <- data.frame(semivars, correlation.distances)

correlation.plot$maybe.corr <- 1 - (correlation.plot$semivars/Exp.var) 
correlation.plot$backtosemi <- Exp.var*(1 - correlation.plot$maybe.corr)
plot(correlation.plot$correlation.distances, correlation.plot$maybe.corr, xlab="lag distance", ylab="Correlations", main="Correlations under the Exponential Fit", pch=19, col="pink")



#Fitting the Sphere Model
TheVariogramModel <- vgm(psill=0.15, model="Sph", nugget=0.0001, range=5)

FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel, ylim=c(25,36), main="Year 1, Plot 1")

FittedModel
Sph.var <- 25.360942 #nugget variance (aka the variance at lag 0)
correlation.plot <- data.frame(semivars, correlation.distances)

correlation.plot$maybe.corr <- 1 - (correlation.plot$semivars/Sph.var) 
correlation.plot$backtosemi <- Sph.var*(1 - correlation.plot$maybe.corr)
plot(correlation.plot$correlation.distances, correlation.plot$maybe.corr, xlab="lag distance", ylab="Correlations", main="Correlations under the Sphere Fit", pch=19, col="navy")


#Fitting the Mat Model
TheVariogramModel <- vgm(psill=0.15, model="Mat", nugget=0.0001, range=5)

FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel, ylim=c(25,36), main="Year 1, Plot 1")

FittedModel
Mat.var <- 24.57514 #nugget variance (aka the variance at lag 0)
correlation.plot <- data.frame(semivars, correlation.distances)

correlation.plot$maybe.corr <- 1 - (correlation.plot$semivars/Mat.var) 
correlation.plot$backtosemi <- Mat.var*(1 - correlation.plot$maybe.corr)
plot(correlation.plot$correlation.distances, correlation.plot$maybe.corr, xlab="lag distance", ylab="Correlations", main="Correlations under the Mat Fit", pch=19, col="darkgreen")


#something is not quite right with the correlations - the trend is expected, but the negative values are not? I Think?
#New Note: negative correlations mean something different spatially so verify this with Dixon tomorrow
#Essentially, as we get further away (lag distance increases) the negative correlation increases which spatially implies -> dissimilar the further away we go?

#Which model fits the best? Either Mat or Exp. Sph & Gau both plateau too early. 
#Quantitative way to verify this? -> check the MSE of the model and use the smallest MSE similar to how we evaluate the linear fit of a model in regression?



