### Example with dummy Dublin dataset


## Note: .D is used after each variable to represent that the data comes from the Dublin dataset


#### Load in packages ####
# Note: Packages may need to be installed
library(INLA)
library(MBA)
library(ROCR)
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



# Check if there are duplicated coordinates (only required when comparing to spBayes)
any(duplicated(dummy.D[c('lat','lng')]))

# Optional: Remove Observations with the same coordinates (only required when comparing to spBayes)
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




#### Create grid ####


# Define grid dimensions
nrow=ncol=100


# Extract lng and lat coordinates
lng = coords.D[,1]; lat = coords.D[,2]


# xrange and yrange stores to be the maximum and minimum longitude and latitude values respecively 
xrange=c(min(lng),max(lng))	#predefined ranges
yrange=c(min(lat),max(lat))	#predefined ranges


## Optional: better for predicting

#### xrange and yrange stores to be the maximum and minimum longitude and latitude values respecively 
##xrange=c(min(boundary$lng),max(boundary$lng))	#predefined ranges
##yrange=c(min(boundary$lat),max(boundary$lat))	#predefined ranges

##


# divide space into regular increments i.e. increments for grid
cutpointsx=seq((xrange[1]-0.000001),(xrange[2]+0.000001),length.out=nrow+1)
cutpointsy=seq((yrange[1]-0.000001),(yrange[2]+0.000001),length.out=ncol+1)



### Take a quick look
par(mfrow=c(1,1))
plot(cutpointsx,cutpointsy,type='n', main='Quick look at grid', xlab='Longitude', ylab='Latitude')
abline(v=cutpointsx)
abline(h=cutpointsy)
points(coords.D, col=colour.D, pch=16, cex=0.75)
legend('topleft', title='Lapse', legend=c('False','True'), pch=rep(16, 2), col=c(3, 2), cex=0.75)




#### Preprocessing of data ####


# Index data into grid
grid1 = as.numeric(cut(lng,cutpointsx))
grid2 = as.numeric(cut(lat,cutpointsy))
grid=cbind(grid1,grid2)

# Take a quick look
#plot(grid, main='Quick look at indexed data')


# Assign each observation to the node it belongs 
node = as.vector(inla.lattice2node.mapping(nrow=nrow,ncol=ncol)[grid])

# Make sure there are no NA values in node
anyNA(node)


# Add column representing node values
dummy.D$node = node



# Fit a binomial glm to lapse (excluding the intercept)
fit.D = glm(lapse ~ 1, family='binomial', data=dummy.D)
summary(fit.D)

##step(fit.D, direction="both")

# Extract beta calculated in above glm to use as starting beta value for spGLM
beta.starting.D = coefficients(fit.D)

# Extract variance of coefficients
beta.var.D = vcov(fit.D)




#### Run INLA ####


# Select priors for spatial process (parameters for log gamma are c(shape,rate))
priors.matern = list(range = list(param =c(0.5, 10), prior = "loggamma"),
                     prec = list(param=c(1, 0.01), prior = 'loggamma'))


# Select nu value
nu.INLA = 1


# Select parameters for normal prior given to fixed effects
prior.covars = list(mean.intercept=beta.starting.D[1], prec.intercept=1/beta.var.D[1,1])
##prior.covars = list(mean=beta.starting.D[2:length(beta.starting.D)], mean.intercept=beta.starting.D[1], prec=diag(beta.var.D)[2:nrow(beta.var.D)], prec.intercept=1/beta.var.D[1,1])
##prior.covars = list(mean=0, mean.intercept=0, prec=0.01, prec.intercept=0.01)


# Select model to run
formula.INLA = lapse ~ 1 + f(node, model = "matern2d", nrow=nrow, ncol=ncol, hyper=priors.matern, nu=nu.INLA)
# Node index must be labelled node for kfold to work


# Select to print verbose
verbose.INLA=TRUE


# Select link function
link = 'logit'



### Run INLA
INLAfit = inla(formula.INLA, family = "binomial", data = dummy.D, verbose = verbose.INLA,
               control.family = list(link = link),
               control.fixed = prior.covars,
               control.predictor = list(compute=TRUE,link=1), # compute = TRUE tells inla to compute the linear predictor
               control.compute = list(dic=TRUE))




#### Summary of fitted model ####


summary(INLAfit)


# Summary of fixed effects
INLAfit$summary.fixed


# 95% confidence interval
conf.int.INLA = INLAfit$summary.fixed[c('0.025quant','0.975quant')]
conf.int.INLA


# Plot posterior marginal density
plot(INLAfit$marginals.fixed$'(Intercept)', type='l', main = 'Posterior Marginal Density\nIntercept')
##plot(INLAfit$marginals.hyperpar$'Precision for node', type='l', main = 'Posterior Marginal Density\nPrecision', xlim=c(0,10))


# Summary of hyperparameters
INLAfit$summary.hyperpar

# Summary for sigma.sq
summary.sig = 1/INLAfit$summary.hyperpar[1,c('mean', '0.025quant', '0.5quant','0.975quant')]
summary.sig[2:4] = summary.sig[4:2]
rownames(summary.sig) = 'sigma.sq'
summary.sig

# Summary for phi
summary.phi = sqrt(8*nu.INLA)/INLAfit$summary.hyperpar[2,c('mean', '0.025quant', '0.5quant','0.975quant')]
summary.phi[2:4] = summary.phi[4:2]
rownames(summary.phi) = 'phi'
summary.phi


# Summary of spatial process at observed locations
View(INLAfit$summary.random$node[node,])


# Odds ratios for fixed effects
exp(sum(INLAfit$summary.fixed['mean'][2:4,]))

# 95% confidence interval for odds ratio
exp(conf.int.INLA)




#### Graphical summary ####


# Extract predicted linear predictor values
eta = INLAfit$summary.linear.predictor$mean


# Extract predicted probability
phat.INLA = INLAfit$summary.fitted.values$mean


# Extract predicted probability
pvar.INLA = INLAfit$summary.fitted.values$sd^2



### Take a look

par(mfrow=c(1, 2))


# Contour and heat plot for posterior mean of probability of lapse
surf.INLA = mba.surf(cbind(coords.D, phat.INLA), no.X=100, no.Y=100, extend=TRUE)$xyz.est # Generate surface for contour/heat plot
image(surf.INLA, zlim=c(0,1), col=rev(heat.colors(200)), main="Interpolated Posterior Mean of\nProbability of Lapse\n(Dublin, INLA)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.INLA, add=TRUE, nlevels=5) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)

# Plot of actually number of lapse (same plot from line 66)
plot(coords.D, col=colour.D, pch=16,  main='Lapse by Location\n(Dublin)', xlab='Longitude', ylab='Latitude', cex=0.75)
legend('topleft', title='Lapse', legend=c('False','True'), pch=rep(16, 5), col=c(3, 2), cex=0.75)


## Additional plot to compare results with empirical lapse rate

# Contour and heat plot for posterior mean of probability of lapse
image(surf.INLA, zlim=c(0,1), col=rev(heat.colors(200)), main="Interpolated Posterior Mean of\nProbability of Lapse\n(Dublin, INLA)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.INLA, add=TRUE, nlevels=4) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)

# Contour/Heat plot of empirical lapse rate (same plot from line 75)
image(surf.D, col=rev(heat.colors(200)), main="Interpolated Observed Lapse Rate\n(Dublin)", xlab='Longitude', ylab='Latitude') # Heat map
contour(surf.D, add=TRUE,nlevels=4) # Add contours to plot
legend('topleft', title='Lapse Rate', legend=c('0','1'), pch=15, col=c(7,2), cex=0.75)




#### Model Validation ####



### Deviance information criterion
INLAfit$dic$dic



### Performance measures


# Select choice of tau for classification (Note: We dont have to choose tau which maximises sensitivity plus specificity)
tau.INLA = maxtau(pred=phat.INLA, response=lapse.D)$max

# Look at plot of sens plus spec with tau
par(mfrow=c(1,1))
ss.INLA = maxtau(pred=phat.INLA, response=lapse.D)$SensPlusSpec
plot(seq(0,1,length=length(ss.INLA)), ss.INLA, main='Choice of Threshold Tau', xlab='tau', ylab='Sensitivity + Specificity', pch=3)
points(tau.INLA, max(ss.INLA), col='red', pch=3, cex=1.5, lwd=2.5)
legend('topleft', legend=paste('Tau = ',tau.INLA,'',sep=''), pch=3, col='red')

# Alternatively set tau='random' to use a bernoulli random generator for classification
tau.INLA = 'random'

# Alternatively set tau=0.5
tau.INLA = 0.5



# Calculate predicted lapse values
if(tau.INLA!='random'){
yhat.INLA = ifelse(phat.INLA >= tau.INLA, 1, 0)
} else if(tau.INLA == 'random') {
yhat.INLA = rbinom(length(phat.INLA), rep(1,length(phat.INLA)), phat.INLA)
}



# Calculate performance of model (if tau = 'random' then yhat.sp is classified 20 times and the average rates etc are used)
performance.INLA = perform(p.pred=phat.INLA, response=lapse.D, tau=tau.INLA)

# Add legend with AUC value to ROC curve
legend('bottomright',legend = paste('AUC ',round(performance.INLA$AUC,5),'',sep=''),cex=1.5)

# Misclassification table
performance.INLA$Table

# Note if using tau.INLA = 'random' the Misclassification table becomes
#performance.INLA$Table/20


# Plot ROC curve again
par(mfrow=c(1,1))
plot(performance.INLA$plot, main='ROC Curve')
#lines(c(0,0,1), c(0,1,1), col='red', lty=2)
#legend('bottomright', legend=paste('AUC = ',performance.INLA$AUC,'',sep=''),cex=1.25)


### Perform kfold cross validation (use same parameters/arguements as in INLAfit)
# Note: tau.INLA = 'random' also works for kfold

# Select the number of folds
k.INLA=10


# Run kfold cross validation (if tau = 'random' then yhat.sp is classified 20 times and the average rates etc are used)
ptm <- proc.time()
kfold.INLA = kfold(formula=formula.INLA, family='binomial', data=dummy.D, k=k.INLA, type='INLA',
                   response = lapse.D, name.response='lapse', verbose=verbose.INLA, tau=tau.INLA, # name.response is the name of the response varible in data
                   control.family = list(link = link),
                   control.fixed = prior.covars)
proc.time() - ptm



## Look at plots
# The default plot is to return ROC for each fold (this is type='ROC)
# typr = 'ROC single' plots the ROC for each fold on top of each other in a single plot
# mfrow defines the plot screen order ( default is mfrow=c(sqrt(number of folds),sqrt(number of folds)))


# Take a look
plot(kfold.INLA,mfrow=c(2,5))
plot(kfold.INLA, type='ROC single')



## Summary of kfold
summary(kfold.INLA)


# Look at each output
kfold.INLA$Error
kfold.INLA$Mean_Error
kfold.INLA$Accuracy
kfold.INLA$Mean_Accuracy
kfold.INLA$Table
kfold.INLA$Rates
kfold.INLA$Mean_Rates
kfold.INLA$AUC
kfold.INLA$Mean_AUC





#### Predicting with INLA ####

## Suppose we wanted to predict the last 30 observations for dummy.D
## To do this we need to have all information about these observation except whether they lapsed or not
## To do this in INLA we need to re-run the inla function with NA values in place for the observation that we wish to predict
## Of course we only put NA values in for the response variable and keep the required information for the explanatory variables

## This is done as follows

# Define our new test data set
dummy.test = dummy.D

# Enter NA values for the data that we wish to predict (note that the node indexes remain the same as the data we wish to predict remain in the same locations)
dummy.test[(n.D-30):n.D,'lapse'] = rep(NA,30)

# Run INLA
INLA.pred = inla(formula.INLA, family = "binomial", data = dummy.test, verbose = verbose.INLA,
                 control.family = list(link = link),
                 control.fixed = prior.covars,
                 control.predictor = list(compute=TRUE,link=1))

# Extract predicted value (predicted values will still be located at (n.D-30):n.D in the vector of all fitted values)
prediction.INLA = INLA.pred$summary.fitted.values$mean[(n.D-30):n.D]

# 95% confidence interval for predicted values
CI.prediction.INLA = INLA.pred$summary.fitted.values[(n.D-30):n.D,c('0.025quant','0.975quant')]




#### Additonal ####

### performance of non spatial 



# Predicted prob
phat.glm = c(fit.D$fitted.values)


# Select choice of tau for classification (Note: We dont have to choose tau which maximises sensitivity plus specificity)
tau.glm = maxtau(pred=phat.glm, response=lapse.D)$max


# Look at plot of sens plus spec with tau
par(mfrow=c(1,1))
ss.glm = maxtau(pred=phat.glm, response=lapse.D)$SensPlusSpec
plot(seq(0,1,length=length(ss.glm)), ss.glm, main='Choice of Threshold Tau', xlab='tau', ylab='Sensitivity + Specificity', pch=3)
points(tau.glm, max(ss.glm), col='red', pch=3, cex=1.5, lwd=2.5)
legend('topleft', legend=paste('Tau = ',tau.glm,'',sep=''), pch=3, col='red')

# Alternatively set tau='random' to use a bernoulli random generator for classification
tau.glm = 'random'
tau.glm = 0.5

# Calculate predicted lapse values
if(tau.glm!='random'){
  yhat.glm = ifelse(phat.glm >= tau.glm, 1, 0)
} else if(tau.glm == 'random') {
  yhat.glm = rbinom(length(phat.glm), rep(1,length(phat.glm)), phat.glm)
}


# Calculate performance of model (if tau = 'random' then yhat.sp is classified 20 times and the average rates etc are used)
performance.glm = perform(p.pred=phat.glm, response=lapse.D, tau=tau.glm)

# Add legend with AUC value to ROC curve
legend('bottomright',legend = paste('AUC ',round(performance.glm$AUC,5),'',sep=''),cex=1.5)

# Misclassification table
performance.glm$Table

# Note if using tau.INLA = 'random' the Misclassification table becomes
#performance.INLA$Table/20

# Plot ROC curve again
par(mfrow=c(1,1))
plot(performance.glm$plot, main='ROC Curve')
#lines(c(0,0,1), c(0,1,1), col='red', lty=2)
#legend('bottomright', legend=paste('AUC = ',performance.INLA$AUC,'',sep=''),cex=1.25)




#### Rough Work ####

#Mean
image(matrix(INLAfit.D$summary.random$node$mean,nrow,ncol))

#sd
image(matrix(r.nb2$summary.random$i1$sd,nrow,ncol))

### Writing to csv file

# Create data frame of predictions 
csv.INLA = data.frame(phat=phat.INLA, yhat=yhat.INLA)

# Write csv file, dont forget to change file name
write.csv(csv.INLA, file = "Output_INLA_model1.csv", row.names=FALSE)
