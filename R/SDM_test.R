library(dismo)
library(maptools)
data(wrld_simpl)

#Find out how many records of the white rhino there are
gbif('Ceratotherium','simum',geo=FALSE,download=FALSE)
#download 100 of them
rhino <- gbif('Ceratotherium','simum',geo=FALSE,download=TRUE,end=100)
#all have geo data so don't need to use geocode to find lon/lat data based on locality information

plot(wrld_simpl,xlim=c(0,50),ylim=c(-30,10),col='light blue',lwd=2)
points(rhino$lon,rhino$lat,pch=20,col='red')

files <-list.files(path=paste(system.file(package="dismo"),'/ex',sep=''),pattern='grd',full.names = TRUE)
predictors <- stack(files)
plot(predictors) #plots all the environmental factors

file<-paste(system.file(package="dismo"),"/ex/bradypus.csv",sep="")
bradypus <- read.table(file,header=TRUE,sep=',')
bradypus <- bradypus[,-1] #removes first column i.e. species name

plot(predictors,1) #only the first layer of the raster stack rather than all layers
plot(wrld_simpl,add=TRUE)
points(bradypus,col='blue')
# i.e. a map of environmental data country borders provided by wrld_simpl and species presence plotting on top
# this predictor data is from WorldClim database and terrestial biome data from WWF
# You can download WorldClim data using the getData function from the raster package!

# find the value of the environmental predictors at the locations of the data:
presence_vals <- extract(predictors,bradypus)

#create background data using the range of the predictors
#set seed to have same random data
set.seed(0)
backgr <- randomPoints(predictors,500)
back_vals <- extract(predictors,backgr)

#denote pb with 1 if presence point and 0 if background
pb <-c(rep(1,nrow(presence_vals)),rep(0,nrow(back_vals)))
#combine into dataframe and make sure biome is a categorical variable
sdmdata <-data.frame(cbind(pb,rbind(presence_vals,back_vals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])

pairs(sdmdata[,2:5],cex=0.1)
#can use this pairs plot to assess collinearity in the data

######### Modelling

#most of the models work the same way without needing to input specific formulas

#use glm function
#specific environmental predictors used
m1 <- glm(pb ~ bio1 + bio5 + bio12, data = sdmdata)
summary(m1)
#use all predictors
m2 <- glm(pb ~ ., data = sdmdata)
summary(m2)

#if we use the BioClim model instead of glm, need only presence data
bc <- bioclim(presence_vals[,c('bio1','bio5','bio12')])
class(bc)
bc
pairs(bc)


########## Model Predictions

#Even though the models produce different objects as outputs, you can use the predict function with dismo to make predictions
# for new combinations of environmental data

#3 new records for bio1, bio5 and  bio12
bio1 = c(40,150,200)
bio5 = c(60,115,290)
bio12 = c(600,1600,1700)
pd <- data.frame(cbind(bio1,bio5,bio12))

#use glm:
m1pred<- predict(m1,pd)
#use bioclim
bcpred<-predict(bc,pd)

#can use response function to plot predicted pb value (i.e large means more likely to be present?) against predictors
response(bc)


#we can also produce a map for the whole area we have environmental data from, using the points we have presence/background
# need to have variable names in model object same as raster layers - ie keep good naming practice

p<-predict(predictors,m1)
plot(p)
#so this is a map of likelihood of species occurrence?


######## TRYING OUT DIFFERENT MODELS

#some models can't have categorial variables
pred_nf <- dropLayer(predictors, 'biome')

#training and testing sets of data
#use k groups - train on k-1 and test on 1, then switch around all k
#or just use it to randomly assign things to groups
group<- kfold(bradypus,5)  #5 groups
pres_train <- bradypus[group != 1,]
pres_test <- bradypus[group ==1,]

#restricting the data geographically - numbers are rectangular extent
ext = extent(-90,-32,-33,23)

#creating background data, use pred_nf to determine that background points fall well within the raster grid ie one point per cell
#further restrict background points to be 12.5% of specified extent
backg <- randomPoints(pred_nf, n=1000, ext=ext,extf = 1.25)
colnames(backg) <- c('lon','lat')
group <- kfold(backg,5)
backg_train <- backg[group != 1,]
backg_test <- backg[group == 1,]


#plot the environment raster (raster level 1), plot the extent we look at with a red box, add the presence and background train/test data
r = raster(pred_nf,1)
plot(!is.na(r),col=c('white','light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch='+', cex=0.5, col='green')
points(pres_test, pch='+', cex=0.5, col='blue')


# fitting the bioclim model simply
bc2 <- bioclim(pred_nf, pres_train) #it creates the model for us - we do not need to extract the environmental data at our presence points
plot(bc2, a=1, b=2, p=0.85)
plot(bc2, a=2, b=3) #a and b determine which predictors to plot and p the size of the box around it (where presence is 85%?)

#evaluate the model
e <- evaluate(pres_test, backg_test, bc, pred_nf) #use the model on presence/background test data with the predictors
#create a threshold (for presence?)
thresh <- threshold(e, 'spec_sens')
#where does spec_sens come from? threshold says spec_sens calculates the threshold at which the sum of the sensivity and specificity are highest
# ie threshold is when you get good true positives and true negatives

#prediction
pb <- predict(pred_nf, bc, ext=ext, progress='')

#plot our predictions result as well as test data on top
par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(wrld_simpl, add=TRUE, border = 'dark grey')
#plot the predictions that are above the threshold to indicate likely presence
plot(pb > thresh, main = 'presence/absence')
plot(wrld_simpl, add=TRUE, border = 'dark grey')
points(pres_train, pch='+')


##### further model work - regression models
## glm first
### these models require you to extract the environmental data values yourself:

train <- rbind(pres_train,  backg_train)
pb_train <- c(rep(1,nrow(pres_train)), rep(0,nrow(backg_train))) #presence/background indicator for train
envtrain <- extract(predictors, train) #predictor values at the training points
envtrain <- data.frame(cbind(pa=pb_train, envtrain))
envtrain[,'biome'] = factor(envtrain[,'biome'], levels=1:14) #ensuring biome is categorical
head(envtrain)

#similarly setting up the test environmental conditions. but keep these separate
testpres <- data.frame(extract(predictors, pres_test))
testbackg <- data.frame(extract(predictors, backg_test))
testpres[,'biome'] = factor(testpres[,'biome'], levels=1:14)
testbackg[,'biome'] = factor(testbackg[,'biome'], levels=1:14)

#glm with logistic regression
glm1 <-glm(pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17, family = binomial(link="logit"), data = envtrain)
#could have used pa~. or does that imply interactions?
summary(glm1)

#glm with gaussian regression
glm2 <- glm(pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17, family = gaussian(link="identity"), data=envtrain)
summary(glm2)

#test the two models
evaluate(testpres, testbackg, glm1)
ge2 <- evaluate(testpres, testbackg, glm2)
#bin model has higher AUC but has a negative max TPR+TNR (true pos results + true neg results), what does that mean?

#plot predictions of gaussian model
pg <- predict(predictors, glm2, ext=ext)
par(mfrow=c(1,2))
plot(pg, main='GLM/gaussian, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
thresh <- threshold(ge2, 'spec_sens')
plot(pg > thresh, main = 'presence/absence')
plot(wrld_simpl, add=TRUE, border = 'dark grey')
points(pres_train, pch = '+')
points(backg_train, pch = '-', cex = 0.25)
#why have we plotted the training data rather than the testing data here?

#can use GAMs instead of GLMs but they provide no examples for this other than stating that GAMs are implemented in the mgcv package

#### Move on to machine learning methods
#### MaxEnt is the most popular of all the SDM methods. But others include Random Forests, Boosted Regression Trees, Artificial Neural Networks etc
### To run MaxEnt you need to dowwnlaod a java standalone program and save it in the same location as the dismo package (in java folder)
