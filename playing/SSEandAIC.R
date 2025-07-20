#fit the models, get the parameters, compare SSE and AIC
#log likelihood tests (likelihood ratio) -> nested models (Problem testing and comparing models)
#AIC does not require nested hypotheses -> can't test so must rely on the comparison of SSE and AIC from the models

#work out the likelihood for independent normal errors
#(x-mu)^2 connection back to the variance -> connect back to SSE and calculate 


#non-linear least squares problem!!!!

#ARC info shape file -> read data file into R
#packages
library(sf)
library(sp)
library(gstat)

#fitting a Gaussian Model
TheVariogram = variogram(yield1~1, data=space.1.sf) #variogram for space 1, yield 1
TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)

FittedModelGau <- fit.variogram(TheVariogram, model=TheVariogramModel)    
attr(FittedModelGau, "SSErr") #grabs the SSE from the model I need
#should be able to calc the log-likelihood because the variogram will have a distribution no matter what
#treat n as total number of observations for degrees of freedom (Not variogram points) - total number of parameters
#ignore the weights and use the number of points in the variograms -> PREFERRED n


#Fitting the exponential model
TheVariogramModel <- vgm(psill=0.15, model="Exp", nugget=0.0001, range=5)
FittedModelExp <- fit.variogram(TheVariogram, model=TheVariogramModel) 
attr(FittedModelExp, "SSErr")

#Fitting the Sphere Model
TheVariogramModel <- vgm(psill=0.15, model="Sph", nugget=0.0001, range=5)
FittedModelSph <- fit.variogram(TheVariogram, model=TheVariogramModel)  
attr(FittedModelSph, "SSErr")

#Fitting the Mat Model
TheVariogramModel <- vgm(psill=0.15, model="Mat", nugget=0.0001, range=5)
FittedModelMat <- fit.variogram(TheVariogram, model=TheVariogramModel)  
attr(FittedModelMat, "SSErr")
summary(FittedModelMat)

#Matern and exponential has the same SSErr because the Matern is just the exponential distribution by the Kappa
#they also have the smallest SSErr

#can get from the Matern (3 param - kappa gets fixed which makes this 2 param model) to the Exponential model (2 param)
#checking both AIC and SSE allows us to see the comparison between Matern and Exp

#Note to self: Keep in mind that AIC = 2*k - 2*ln(L) penalizes a model for more parameters, where k = number of predictors and L is your max likelihood. 
#So, AIC shouldn't be the only metric you use for assessing goodness of fit.

#figure out how to get AIC and log likelihood functions

#n = 15 for all models (sample variogram)

AIC(FittedModelGau)
AIC(FittedModelExp)
AIC(FittedModelSph)
AIC(FittedModelMat)


#log.lik.exp <- 






