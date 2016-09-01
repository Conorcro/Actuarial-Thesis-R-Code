### Example with dummy Dublin dataset


## Note: .D is used after each variable to represent that the data comes from the Dublin dataset


#### Load in packages ####
# Note: Packages may need to be installed
library('spBayes')
library('MBA')
library('ROCR')
library(rgdal)




#### Run required functions ####


# Load function to perform k fold cross validation
# Function name is kfold
source("C:/Users/Conor Cronin/Documents/College/Thesis/Thesis Code/kfold_function_v4.R")


# Load plot method for the kfold class
source("C:/Users/Conor Cronin/Documents/College/Thesis/Thesis Code/plot_kfold_function_v2.R")


# Load summary method for the kfold class
source("C:/Users/Conor Cronin/Documents/College/Thesis/Thesis Code/summary_kfold_function_v2.R")


# Load maxtau function which calculates tau which maximises sensitivity plus specificity
source("C:/Users/Conor Cronin/Documents/College/Thesis/Thesis Code/maxtau_function.R")


# Load function which measure the performance of a model
source("C:/Users/Conor Cronin/Documents/College/Thesis/Thesis Code/perform_function_v2.R")





#### Load in data ####

# Load in dummy data set
dummy.D = read.csv('C:/Users/Conor Cronin/Documents/College/Thesis/Geocoding dataset/dummy_lapse_dataset_dublin_5000.csv')
View(dummy.D)



## Load in shape file for dublin counties
shape <- readOGR(dsn = "C:/Users/Conor Cronin/Documents/College/Thesis/Geocoding dataset/Census2011_Admin_Counties_generalised20m", "Census2011_Admin_Counties_generalised20m")
# Check units of measurement of coordinates
proj4string(shape) 

# Convert Shape coordinates to longitude/latitude coordinates
shape = spTransform(shape, CRS("+proj=longlat +datum=WGS84"))
# check
proj4string(shape)


## Extract boundary coordinates for  Dublin
polys = slot(shape,"polygons")
dub.names = shape$COUNTYNAME[17:20]
boundary = NULL
boundary.list = list()
for(i in 1:length(dub.names)){
  boundary = rbind(boundary, slot(slot(polys[[which(shape$COUNTYNAME==dub.names[i])]],"Polygons")[[1]],"coords"  ))
  boundary.list[[paste(dub.names[i])]] =  slot(slot(polys[[which(shape$COUNTYNAME==dub.names[i])]],"Polygons")[[1]],"coords"  )
}
colnames(boundary)=c('lng','lat')
boundary=as.data.frame(boundary)




#### Data Cleaning ####


# Check for NA values
sapply(dummy.D, function(x) anyNA(x))


# Check Max and Min (check for outlier or unusual values)
sapply(dummy.D[c('unique_id','lng','lat','lapse','age','bmi')], function(x) min(x))
sapply(dummy.D[c('unique_id','lng','lat','lapse','age','bmi')], function(x) max(x))


# Check lapse
table(dummy.D$lapse)
table(dummy.D$gender)


# Visual check
plot(dummy.D$lapse, ylab='Lapse', xlab='Index', main='Plot of Lapse')
hist(dummy.D$lapse, xlab='Lapse', main='Histogram of Lapse')
plot(dummy.D$age, ylab = 'Age', xlab = 'Index', main='Plot of Age', ylim=c(20,120))
hist(dummy.D$age, xlab = 'Age', main='Histogram of Age')
plot(dummy.D$bmi, ylab = 'BMI', xlab = 'Index', main='Plot of BMI', ylim=c(10,40))
hist(dummy.D$bmi, xlab = 'BMI', main='Histogram of BMI')


# Check lng & lat
plot(boundary, type='n', main='Observation Location', xlab='longitude', ylab = 'latitude')
for(i in 1:length(boundary.list)) lines(boundary.list[[i]])
points(dummy.D$lng,dummy.D$lat, col='cyan', pch=16)
legend('topleft', title='County', legend = paste(dub.names), pch=rep(16,4), col= sort(unique(dummy.D$county))+1,cex=0.8)


# Check county and countyname are in agreement
table(dummy.D$countyname, dummy.D$county)


# Check levels of factor variables
sapply(dummy.D[c('gender')], function(x) levels(x))


# Check for duplicated Unique ID (i.e. is there any policy holders that have been added twice)
any(duplicated(dummy.D$uniqueID))



## Note: Model will not work if there is at least 2 observations with the exact same coordinates

# Check if there are duplicated coordinates
any(duplicated(dummy.D[c('lat','lng')]))

# Remove Observations with the same coordinates
dummy.D = dummy.D[!duplicated(dummy.D[c('lat','lng')]),]
View(dummy.D)




#### Extract data ####


# Set n equal to the number of observations
n.D = nrow(dummy.D)

# Set lapse (the response variable) equal to the lapse variable
lapse.D = dummy.D$lapse

# Extract Coordinates
coords.D = as.matrix(dummy.D[c('lng', 'lat')])




#### Summary of data ####


# Summary of data
summary(dummy.D)
summary(factor(lapse.D))


## Other summar statistics


# Lapse rates for males
summary(factor(lapse.D[dummy.D$gender=='Male'])); summary(factor(lapse.D[dummy.D$gender=='Male']))[2]/sum(summary(factor(lapse.D[dummy.D$gender=='Male'])))

# Lapse rates for females
summary(factor(lapse.D[dummy.D$gender=='Female'])); summary(factor(lapse.D[dummy.D$gender=='Female']))[2]/sum(summary(factor(lapse.D[dummy.D$gender=='Female'])))



# Create categorical variable for age such that those which an age above the median are classified as old and those below the median are classified as young
F.age.D = ifelse(dummy.D$age>=median(dummy.D$age),'Old','Young')
dummy.D$ageF = F.age.D

# Lapse rates for old policyholders
summary(factor(lapse.D[F.age.D=='Old'])); summary(factor(lapse.D[F.age.D=='Old']))[2]/sum(summary(factor(lapse.D[F.age.D=='Old'])))

# Lapse rates for young policyholders
summary(factor(lapse.D[F.age.D=='Young'])); summary(factor(lapse.D[F.age.D=='Young']))[2]/sum(summary(factor(lapse.D[F.age.D=='Young'])))



# Create categorical variable for bmi such that those which a bmi above 25 are classified as overweight and those below 25 are classified as normal
F.bmi.D = ifelse(dummy.D$bmi>=25,'Overweight','Normal')
dummy.D$bmiF = F.bmi.D

# Lapse rates for overweight policyholders
summary(factor(lapse.D[F.bmi.D=='Overweight'])); summary(factor(lapse.D[F.bmi.D=='Overweight']))[2]/sum(summary(factor(lapse.D[F.bmi.D=='Overweight'])))

# Lapse rates for normal weight policyholders
summary(factor(lapse.D[F.bmi.D=='Normal'])); summary(factor(lapse.D[F.bmi.D=='Normal']))[2]/sum(summary(factor(lapse.D[F.bmi.D=='Normal'])))




#### Take a quick look ####

par(mfrow=c(1, 2))


# Create vector to store colours indicator for plot (colours to represent lapse at a coordinate)
colour.D = ifelse(lapse.D==0, 3, 2)


# Plot coordinates and add legend
plot(coords.D, col=colour.D, pch=16,  main='Lapse by Location\n(Dublin)', xlab='Longitude', ylab='Latitude', cex=0.75)
legend('topleft', title='Lapse', legend=c('False','True'), pch=rep(16, 2), col=c(3, 2), cex=0.75)

## Alternative plot which includes Dublin boundary lines
#plot(boundary, type='n',  main='Lapse by Location\n(Dublin)', xlab='Longitude', ylab='Latitude')
#for(i in 1:length(boundary.list)) lines(boundary.list[[i]])
#points(coords.D, col=colour.D, pch=16, cex=0.75)
#legend('topleft', title='Lapse', legend=c('False','True'), pch=rep(16, 2), col=c(3, 2), cex=0.75)

# Contour/Heat plot of lapse rate
surf.D = mba.surf(cbind(coords.D, lapse.D), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image(surf.D, col=rev(heat.colors(200)), main="Interpolated Lapse Rate\n(Dublin)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.D, add=TRUE,nlevels=3) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)
###points(coords.D, pch=16, cex=0.5)




#### Exploratory data analysis ####


# Take a quick look to see how variables depend on each other

# Plot of all variables
plot(dummy.D[,6:9])

# Plot lapse against age
boxplot(dummy.D$age ~ dummy.D$lapse, main="Lapse against Age", xlab="Lapse", ylab="Age (last birthday)")

# Plot lapse againt gender
plot(factor(dummy.D$lapse),dummy.D$gender, main="Lapse against Gender", xlab="Lapse", ylab="Gender")

# Plot lapse against bmi
boxplot(dummy.D$bmi ~ dummy.D$lapse, main="Lapse against BMI", xlab="Lapse", ylab="BMI")

# Plot age against bmi
plot(dummy.D$bmi,dummy.D$age, main="BMI against Age", xlab="BMI", ylab="Age")

# Plot gender againt bmi
boxplot(dummy.D$bmi ~ dummy.D$gender, main="Gender against BMI", xlab="Gender", ylab="BMI")




#### Code categorical predcitor variables using indicator variables and extract design matrix ####


# TO create an indicator variable for gender (label Male), Male = 1 if gender = Male
# Use as.numeric(dummy.D$gender=='Male')

# Extract design matrix
x.D = as.matrix(rep(1, n.D))

## General code to construct design matrix
##x.D = cbind(rep(1, n.D), dummy.D$age, as.numeric(dummy.D$gender=='Male'), ifelse(dummy.D$smoker=='Yes', 1, 0), dummy.D$blood, ifelse(dummy.D$drink=='Non', 1, 0), ifelse(dummy.D$drink=='Normal', 1, 0))




#### Preprocessing ####


# Fit a binomial glm to lapse (excluding the intercept)
fit.D = glm(lapse ~ 1, family='binomial', data=dummy.D)
summary(fit.D)

##step(fit.D, direction="both")

# Extract beta calculated in above glm to use as starting beta value for spGLM
beta.starting.D = coefficients(fit.D)

# Use the lower triangular choleski decomposition of the varaince-covariance matrix of the beta values of the above glm as the beta tuning values
beta.tuning.D = t(chol(vcov(fit.D))) # transpose to give the lower triangular


# Select to use amcmc or not
use.amcmc = T


# Select to use knots plus the number of knots and their locations
use.knots = T

#### Choose the number of knots and their locations

# Select number of knots
n.knots = 15^2

# Select number of x and y gridpoints
n.x = sqrt(n.knots); n.y = sqrt(n.knots)

#Grid Increments
x.inc = (max(coords.D[,1])-min(coords.D[,1]))/n.x
y.inc = (max(coords.D[,2])-min(coords.D[,2]))/n.y

# Create grid
grid.x=as.matrix(rep(seq(min(coords.D[,1])+x.inc/2,max(coords.D[,1])-x.inc/2,length=n.x),n.y))
grid.y=as.matrix(rep(seq(min(coords.D[,2])+y.inc/2,max(coords.D[,2])-y.inc/2,length=n.y),each=n.x))
knots = cbind(grid.x,grid.y)


## quick look at the knots
par(mfrow=c(1,1))
plot(coords.D, col=colour.D, pch=16,  main='Lapse by Location\n(Dublin)', xlab='Longitude', ylab='Latitude', cex=0.75)
points(knots)
legend('topleft', title='Lapse', legend=c('False','True','knots'), pch=c(16, 16, 1), col=c(3, 2, 1), cex=0.75)
##




#### Begin Analysis ####


# Select model
formula.sp =  lapse ~ 1


# Select priors for mcmc
priors.sp = list('beta.Normal'=list(beta.starting.D,diag(vcov(fit.D))), 'phi.Unif'=c(0.1, 100), 'sigma.sq.IG'=c(0.1, 100), 'nu.unif'=c(0.20,100))


# Select starting values for mcmc
starting.sp = list('beta.starting'=beta.starting.D, 'phi'=10, 'sigma.sq'=0.5, 'w'=0, 'nu'=0.5)


# Select tuning values for mcmc
tuning.sp = list('beta'=beta.tuning.D, 'phi'=0.1, 'sigma.sq'=0.1, 'w'=0.1, 'nu'=0.1)


# Select covariogram function
cov.model = 'matern'


# Set number of samples to be generated by mcmc 
n.samples.sp = 5000


# Choose to print verbose
verbose.sp = TRUE



### Run spGLM

ptm <- proc.time()
if(use.amcmc){
  batch.length = 10
  n.batch = n.samples.sp/batch.length
  
  if(use.knots){
    spfit = spGLM(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                  starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                  amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.25),
                  knots=knots,
                  n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  } else {
    spfit = spGLM(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                  starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                  amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.25),
                  n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  }
  
} else {
  
  if(use.knots){
    spfit = spGLM(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                  starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                  knots=knots,
                  n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  } else {
    spfit = spGLM(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                  starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                  n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  }
  
}
proc.time() - ptm




#### Summary of fitted model ####


# Useless summary
summary(spfit)


# Check model parameter names
colnames(spfit$p.beta.theta.samples)


# Plot MCMC samples for parameters
## par(mfrow=c(1,ncol(spfit$p.beta.theta.samples)))
par(mfrow=c(2,2))
for(i in 1:ncol(spfit$p.beta.theta.samples)){
  plot(as.vector(spfit$p.beta.theta.samples[,i]),main=paste('MCMC trace plot for ',colnames(spfit$p.beta.theta.samples)[i],'',sep=''),xlab='Iterations',type='l',ylab='')
}


# Set burn in period
burn.in = 0.2*n.samples.sp


# Plot acf of mcmc samples
par(mfrow=c(2,2))
for(i in 1:ncol(spfit$p.beta.theta.samples)){
  acf(as.vector(spfit$p.beta.theta.samples[burn.in:n.samples.sp,i]),lag.max=100,main=paste('ACF for ',colnames(spfit$p.beta.theta.samples)[i],'',sep=''))
}


# Set thinning value
thin.value = 10


# Thin samples
sub.samps = seq(burn.in, n.samples.sp, thin.value)


# Print to screen a summary of the mcmc samples of each parameter after the burn in period
# Summary includes mean and quantiles
summary(window(spfit$p.beta.theta.samples, start=burn.in, thin=thin.value))


# Extract mean of fixed effects (after burn in)
beta.sp = apply(as.matrix(spfit$p.beta.theta.samples[sub.samps, 1:ncol(x.D)]), 2, mean)
names(beta.sp) = colnames(spfit$p.beta.theta.samples)[1:ncol(x.D)]
beta.sp

# Extract 95% confidence interval for fixed effects
conf.int.sp = t(apply(as.matrix(spfit$p.beta.theta.samples[sub.samps, 1:ncol(x.D)]), 2, quantile, probs=c(0.025,0.975)))
rownames(conf.int.sp) = colnames(spfit$p.beta.theta.samples)[1:ncol(x.D)]
conf.int.sp


# Odds ratios for fixed effects (after burn in period)
exp(beta.sp)

# 95% confidence interval for odds ratio
exp(conf.int.sp)




#### Graphical summary ####


# Extract the mcmc samples for beta & w after the burn in period
beta.MCMC = as.matrix(spfit$p.beta.theta.samples[sub.samps, 1:ncol(x.D)]) # Extract simulated beta hat values
w.MCMC = spfit$p.w.samples[, sub.samps] # Extract simulated w hat values 


# Back transform posterior linear predictor into posterior predicted probabilities (only mcmc samples after the burn in period)
phat.MCMC = 1/(1 + exp(-(x.D%*%t(beta.MCMC) + w.MCMC))) # Calculate simulated probabilities


# Calculate posterior mean of predicted probability for each location
phat.sp = apply(phat.MCMC, 1, mean)


# Calculate posterior sample variance of predicted probability
p.var.sp = apply(phat.MCMC, 1, var)



### Take a look

par(mfrow=c(1, 2))


# Contour and heat plot for posterior mean of probability of lapse
surf.sp = mba.surf(cbind(coords.D, phat.sp), no.X=100, no.Y=100, extend=TRUE)$xyz.est # Generate surface for contour/heat plot
image(surf.sp, zlim=c(0,1), col=rev(heat.colors(200)), main="Interpolated Posterior Mean of\nProbability of Lapse\n(Dublin, spBayes)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.sp, add=TRUE, nlevels=4) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)

# Plot of actually number of lapse (same plot from line 66)
plot(coords.D, col=colour.D, pch=16,  main='Lapse by Location\n(Dublin)', xlab='Longitude', ylab='Latitude', cex=0.75)
legend('topleft', title='Lapse', legend=c('False','True'), pch=rep(16, 5), col=c(3, 2), cex=0.75)


## Additional plot to compare results with empirical lapse rate

# Contour and heat plot for posterior mean of probability of lapse
image(surf.sp, zlim=c(0,1), col=rev(heat.colors(200)), main="Interpolated Posterior Mean of\nProbability of Lapse\n(Dublin, spBayes)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.sp, add=TRUE, nlevels=4) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)

# Contour/Heat plot of empirical lapse rate (same plot from line 75)
image(surf.D, col=rev(heat.colors(200)), main="Interpolated Observed Lapse Rate\n(Dublin)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.D, add=TRUE,nlevels=4) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)


##

# Optional: Contour and heat plot for variance of posterior probability of lapse
###surf.var = mba.surf(cbind(coords.D,p.var.sp),no.X=100, no.Y=100, extend=TRUE)$xyz.est
###image(surf.var, col=rev(heat.colors(200)), main="Interpolated Variance of Posterior\nProbability of Lapse\n(Dublin)",xlab='Longitude',ylab='Latitude')
###contour(surf.var, add=TRUE, nlevels=3)
###text(coords.D, label=paste("(",lapse.D,")",sep=""))

##




#### Model Validation ####


### Check model DIC
spDiag(spfit, start=burn.in, verbose=TRUE)



### Performance measures


# Select choice of tau for classification (Note: We dont have to choose tau which maximises sensitivity plus specificity)
tau.sp = maxtau(pred=phat.sp, response=lapse.D)$max

# Look at plot of sens plus spec with tau
par(mfrow=c(1,1))
ss.sp = maxtau(pred=phat.sp, response=lapse.D)$SensPlusSpec
plot(seq(0,1,length=length(ss.sp)), ss.sp, main='Choice of Threshold Tau', xlab='tau', ylab='Sensitivity + Specificity', pch=3)
points(tau.sp, max(ss.sp), col='red', pch=3, cex=1.5, lwd=2.5)
legend('topleft', legend=paste('Tau = ',tau.sp,'',sep=''), pch=3, col='red')

# Alternatively set tau='random' to use a bernoulli random generator for classification
tau.sp = 'random'

# Alternatively set tau=0.5
tau.sp=0.5


# Calculate predicted lapse values
if(tau.sp!='random'){
  yhat.sp = ifelse(phat.sp >= tau.sp, 1, 0)
} else if(tau.sp == 'random') {
  yhat.sp = rbinom(length(phat.sp), rep(1,length(phat.sp)), phat.sp)
}



# Calculate performance of model (if tau = 'random' then yhat.sp is classified 20 times and the average rates etc are used)
performance.sp = perform(p.pred=phat.sp, response=lapse.D, tau=tau.sp)

# Add legend with AUC value to ROC curve
legend('bottomright',legend = paste('AUC ',round(performance.sp$AUC,5),'',sep=''),cex=1.5)

# Misclassification table
performance.sp$Table



# Plot ROC curve again
par(mfrow=c(1,1))
plot(performance.sp$plot, main='ROC Curve')



### Perform kfold cross validation (use same parameters/arguements as in spGLM)


# Select kfold type
type = 'sp'


# Select the number of folds
k.sp=2


# Run kfold cross validation (if tau = 'random' then yhat.sp is classified 20 times and the average rates etc are used)
ptm <- proc.time()
if(use.amcmc){
  batch.length = 10
  n.batch = n.samples.sp/batch.length
  
  if(use.knots){
    kfold.sp = kfold(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                     response = lapse.D, covars=x.D, k=k.sp, tau=tau.sp,
                     starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                     amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.25),
                     knots=knots,
                     type=type, n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  } else {
    kfold.sp = kfold(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                     response = lapse.D, covars=x.D, k=k.sp, tau=tau.sp,
                     starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                     amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.25),
                     type=type, n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  }
  
} else {
  
  if(use.knots){
    kfold.sp = kfold(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                     response = lapse.D, covars=x.D, k=k.sp, tau=tau.sp,
                     starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                     knots=knots,
                     type=type, n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  } else {
    kfold.sp = kfold(formula=formula.sp, family='binomial', coords=coords.D, data=dummy.D,
                     response = lapse.D, covars=x.D, k=k.sp, tau=tau.sp,
                     starting=starting.sp, tuning=tuning.sp, priors=priors.sp,
                     type=type, n.samples=n.samples.sp, cov.model=cov.model, verbose=verbose.sp)
  }
  
}
proc.time() - ptm



## Look at plots
# The default plot is to return ROC for each fold (this is type='ROC)
# type = 'ROC single' plots the ROC for each fold on top of each other in a single plot
# mfrow defines the plot screen order ( default is mfrow=c(sqrt(number of folds),sqrt(number of folds)))


# Take a look
plot(kfold.sp,mfrow=c(1,2))
plot(kfold.sp, type='ROC single')

# Plot roc for second fold
plot(kfold.sp$ROC$Fold.2, main='ROC Curve Fold 2')


## Summary of kfold
summary(kfold.sp)


# Look at each output
kfold.sp$Error
kfold.sp$Mean_Error
kfold.sp$Accuracy
kfold.sp$Mean_Accuracy
kfold.sp$Table
kfold.sp$Rates
kfold.sp$Mean_Rates
kfold.sp$AUC
kfold.sp$Mean_AUC
kfold.sp$ROC




#### Predicting with spBayes ####

## Suppose we wanted to predict the last 30 observations for dummy.D
## To do this we need to have all information about these observation except whether they lapsed or not
## prediction in spBayes is easily done by using the function spPredict

## This is done as follows

# Suppose we have run spGLM and excluded the last 30 observations when fitting the model
# Now we wish to predict these 30 observations
prediction.MCMC = spPredict(spfit, pred.coords=coords.D[(n.D-30):n.D,], pred.covars=x.D[(n.D-30):n.D,], verbose=verbose.sp)
prediction.sp = apply(prediction.MCMC$p.y.predictive.samples,1,mean)

#### 95% confident interval for predicted values
CI.prediction.sp = t(apply(as.matrix(prediction.MCMC$p.y.predictive.samples), 1, quantile, probs=c(0.025,0.975)))
rownames(conf.int.sp) = colnames(spfit$p.beta.theta.samples)[1:ncol(x.D)]
conf.int.sp




########### Rough work ####


accuracy[1,2] # use to calc tpr etc

rm(list = ls())

names(spfit.D)

write.csv(dummy.D, file = "dummy_dublin_reorganised.csv", row.names=FALSE)

save(spfit.exp, file='spfit_exp.RData')

### Writing to csv file

# Create data frame of predictions 
csv.sp = data.frame(phat=phat.sp, yhat=yhat.sp)

# Write csv file, dont forget to change file name
write.csv(csv.sp, file = "Output_spBayes_model1.csv", row.names=FALSE)
