library(mice)
library(mgcv)
library(ggplot2)
library(lattice)

#Import the dataset
diabetes <- read.csv("/Users/Owen/Important/Math/Stat_690/Project/diabetes.csv")

#Convert 0's to NA
diabetes$SkinThickness <- replace(diabetes$SkinThickness, diabetes$SkinThickness == 0, NA)
#diabetes$Pregnancies <- replace(diabetes$Pregnancies, diabetes$Pregnancies == 0, NA)
diabetes$Insulin <- replace(diabetes$Insulin, diabetes$Insulin == 0, NA)
diabetes$BloodPressure <- replace(diabetes$BloodPressure, diabetes$BloodPressure == 0, NA)
diabetes$Glucose <- replace(diabetes$Glucose, diabetes$Glucose ==0, NA)
diabetes$BMI <- replace(diabetes$BMI, diabetes$BMI == 0, NA)

dim(diabetes)
#Check the Missingness Pattern
md.pattern(diabetes)

#Create Boolean Variable for NA to be used in the histograms
skin.thickness.na <- is.na(diabetes$SkinThickness)
pregnacies.na <- is.na(diabetes$Pregnancies)
insulin.na <- is.na(diabetes$Insulin)

#Create Histograms to see relationship of missing data
# For Skin Thickness there appears to be no real relationship
histogram(~Age | skin.thickness.na, data = diabetes)
histogram(~BMI | skin.thickness.na, data = diabetes)
histogram(~Glucose | skin.thickness.na, data = diabetes)


#See the base predictor matrix from MICE
test.mice <- mice(diabetes, maxit = 0, print = F)
pred.mat <- test.mice$pred

#Remove Insulin, Skin Thickness, and Insulin from being used as predictors on each other in MICE
pred.mat[, c("Outcome")] <- 0

#Use Mice Package with the new prediction matrix to impute for SkinThickness, Pregnancies, and Insulins
pmm.imp <- mice(diabetes, predictorMatrix = pred.mat, seed = 123)


#Check the summary and the plot
summary(pmm.imp)
plot(pmm.imp)

# Increase the number of chains by 35 to confirm convergence
imp40 <- mice.mids(pmm.imp, maxit = 35, print = F)
plot(imp40) #The plot appears to show solid convergence and good bouncing around the space

#Strip plot checks to see if imputed values are similar to the known values
#Blue Dots are the known values and red values are the unknown values
stripplot(pmm.imp, Pregnancies ~ .imp)
stripplot(pmm.imp, SkinThickness ~ .imp)
stripplot(pmm.imp, Insulin ~ .imp)




#Create model 1
pmm.mod=with(pmm.imp,gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
           s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial))

# Pool the the results together
pool.pmm <- pool(pmm.mod)

pool.pmm
summary(pool.pmm)


# Attempt to pool the results ourselves
# See https://stats.stackexchange.com/questions/100245/how-to-summarize-gam-model-result-from-multiple-imputation-data-in-r

#Get all the Coefficients 
beta.hat <- pmm.mod$analyses[[1]]$coefficients

reps = 5
for(i in 2:reps){
  beta.hat = beta.hat + pmm.mod$analyses[[i]]$coefficients
}

#Take average of number of coefficients
beta.hat.avg <- beta.hat / reps

#Get the variance-covariance matrix for all the imputations
W = pmm.mod$analyses[[1]]$Vp

for(i in 2:reps){
  W = W + pmm.mod$analyses[[i]]$Vp
}

#Take the average to be used later
W.avg <- W / reps

#Compute how far away each beta hat is from the average for all 5 imputations
#This is a correction for confidence intervals

B = (pmm.mod$analyses[[1]]$coefficients - beta.hat.avg) %*% t(pmm.mod$analyses[[1]]$coefficients - beta.hat.avg)

#Loop through and get B for each imputation
for(i in 2:reps){
  B = B + (pmm.mod$analyses[[i]]$coefficients - beta.hat.avg) %*% t(pmm.mod$analyses[[i]]$coefficients - beta.hat.avg)
}

#Get B in its final form
B <- B / (reps-1)

#Create the final variance-covariance matrix
var.cov <- W + (1 + (1/reps)) * B

# Get average of df for the residuals
df.r <- pmm.mod$analyses[[1]]$df.residual

for(i in 2:reps){
  df.r = df.r + pmm.mod$analyses[[i]]$df.residual
}

#Get average of the degrees of freedom
df.r <- df.r / reps

#Create variable that has same properties then overwrite the pooled data
pooled.mod <- pmm.mod$analyses[[1]]

#Enter the pooled data into the gam object
pooled.mod$coefficients <- beta.hat.avg
pooled.mod$Vp <- var.cov
pooled.mod$df.residuals <- df.r

#Get Summary of model
summary(pooled.mod)

# Get the 5 datasets
data1 <- complete(pmm.imp,1)
data2 <- complete(pmm.imp,2)
data3 <- complete(pmm.imp,3)
data4 <- complete(pmm.imp,4)
data5 <- complete(pmm.imp,5)

# Fit the model for all 5
mod1 <- gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
      s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial, data = data1)
mod2 <- gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
              s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial, data = data2)
mod3 <- gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
              s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial, data = data3)
mod4 <- gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
              s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial, data = data4)
mod5 <- gam(Outcome~ s(Age) +s(DiabetesPedigreeFunction)+s(Glucose)+ s(BMI)+ s(Insulin)+
              s(SkinThickness)+ s(BloodPressure)+s(Pregnancies), family = binomial, data = data5)

#Summary of all 5
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
