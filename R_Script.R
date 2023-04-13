

### Load Required Packages
library(MASS)
library(mgcv)


# Load necessary libraries
library(tidyverse)
library(magrittr)
library(ggpubr)
library(ggplot2)

# Load diabetes dataset
diabetes <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv",
                     header = FALSE,
                     col.names = c("pregnancies", "glucose", "blood_pressure", "skin_thickness", "insulin", "bmi", "dpfunction", "age", "outcome"))


# Print summary of the dataset
summary(diabetes)

# Print the first few rows of the dataset
head(diabetes)


# Check for missing values
sum(is.na(diabetes))


# Create correlation matrix
corr_matrix <- cor(diabetes)
library(ggcorrplot)
# Plot correlation matrix
ggcorrplot(corr_matrix, type = "upper", hc.order = TRUE, lab = TRUE)


# Create histograms of the variables
diabetes %>% 
  dplyr::select(pregnancies, glucose, blood_pressure, skin_thickness,
                insulin, bmi, age) %>% 
  gather()%>% 
  ggplot(aes(x = value,fill=diabetes$outcome)) + 
  facet_wrap(~key, scales = "free") +
  geom_histogram(binwidth = 5, color = "black", fill = "white") + 
  labs(x = "", y = "Count") +
  theme_bw()


diabetes %>% dplyr::select(insulin, outcome) %>%
  dplyr::filter(insulin==0) %>% group_by(outcome) %>%
  summarise(count=n()/768)





## Histogram of diabetes pedigree function
diabetes%>%
  dplyr::select(dpfunction)%>%
  gather()%>%
  ggplot((aes(x=value)))+
  geom_histogram(color="black", fill="white")+
  labs(x="", y="Count")+
  ggtitle("Histogram of dpFunction")



# Create box plots of the variables by outcome
diabetes %>% 
  gather(key = "variable", value = "value", -outcome) %>% 
  ggplot(aes(x = factor(outcome), y = value, fill = factor(outcome))) +
  facet_wrap(~variable, scales = "free") +
  geom_boxplot() +
  labs(x = "Outcome", y = "") +
  theme_bw()




### Determine relationship

library(ggplot2)

# create scatter plot with smoothed line
ggplot(diabetes, aes(x = age, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = pregnancies, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = glucose, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = bmi, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = blood_pressure, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = skin_thickness, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = insulin, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = dpfunction, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")


# diabetes %>% 
#   dplyr::select(pregnancies, glucose, blood_pressure, skin_thickness,
#                 insulin, bmi, age) %>% 
#   gather() %>% 
#   ggplot(aes(x = value, y=outcome)) + 
#   facet_wrap(~key, scales = "free") +
#   geom_smooth(method = "loess")  


#### Dealing with missing values

## Zero skinthickness does not make sense
### Zero BMI does not make sense
## Zero systolic blood pressure is not sensible


## Zero glucose make sense but questionable
## Zero Insulin concentration also make sense for people fasting but it is still
## questionable
## Zero Pregnancies if great.


## Dealing with Zero Skinthickness

skinth =ifelse(diabetes$skin_thickness==0, 1, 0)
skinMod=gam(diabetes$outcome~skinth, family = binomial)
summary(skinMod)

## result shows that there is not significant effect of having skinthickness 
## of non-zero vs being skinless on diabetes.

## So i think a very good replacement for the skinless people will be some values
##around the mean of the rest of the dataset

## imputation
## Mean of the non-zero skinthickness
xb=diabetes %>% dplyr::select(skin_thickness) %>%
  dplyr::filter(skin_thickness!=0)%>% map(mean)

## sd of the non-zero skinthickness
ssd=diabetes %>% dplyr::select(skin_thickness) %>%
  dplyr::filter(skin_thickness!=0)%>% map(sd)

## Fact 2sd away from the mean covers about 95% of the entire datasset.

grid1=runif(n=1,xb$skin_thickness-2*ssd$skin_thickness
            ,xb$skin_thickness+2*ssd$skin_thickness)

set.seed(23)
for(i in 1:length(diabetes$skin_thickness)){
  
  if(diabetes$skin_thickness[i]==0){
    
    diabetes$skin_thickness[i]=runif(n=1,xb$skin_thickness-2*ssd$skin_thickness
                                     ,xb$skin_thickness+2*ssd$skin_thickness)
  }
}





## Dealing with Zero BMI

BMI =ifelse(diabetes$bmi==0, 1, 0)
BMIMod=gam(diabetes$outcome~BMI, family = binomial)
summary(BMIMod)

## result shows that there is not significant effect of having bmi
## of non-zero vs having zero bmi on diabetes.

## So i think a very good replacement for the zero bmi will be some values
##around the mean of the rest of the dataset

## imputation
## Mean of the non-zero bmi
xb1=diabetes %>% dplyr::select(bmi) %>%
  dplyr::filter(bmi!=0)%>% map(mean)

## sd of the non-zero bmi
ssd1=diabetes %>% dplyr::select(bmi) %>%
  dplyr::filter(bmi!=0)%>% map(sd)

## Fact 2sd away from the mean covers about 95% of the entire datasset.

grid1=runif(n=1,xb1$bmi-2*ssd1$bmi
            ,xb1$bmi+2*ssd1$bmi)

set.seed(23)
for(i in 1:length(diabetes$bmi)){
  
  if(diabetes$bmi[i]==0){
    
    diabetes$bmi[i]=runif(n=1,xb1$bmi-2*ssd1$bmi
                          ,xb1$bmi+2*ssd1$bmi)
  }
}



## Dealing with Zero Insulin

INsulin=ifelse(diabetes$insulin==0, 1, 0)
InsuMod=gam(diabetes$outcome~INsulin, family = binomial)
summary(InsuMod)

## result shows that there is not significant effect of having Insulin concentration
## of non-zero vs having zero Insulin concentration on diabetes.

## So i think a very good replacement for the zero Insulin will be some values
##around the mean of the rest of the dataset

## imputation
## Mean of the non-zero insulin
xb2=diabetes %>% dplyr::select(insulin) %>%
  dplyr::filter(insulin!=0)%>% map(mean)

## sd of the non-zero bmi
ssd2=diabetes %>% dplyr::select(insulin) %>%
  dplyr::filter(insulin!=0)%>%map(sd)

## Fact 2sd away from the mean covers about 95% of the entire datasset.

grid1=runif(n=1,xb2$insulin-2*ssd2$insulin
            ,xb2$insulin+2*ssd2$insulin)

set.seed(23)
for(i in 1:length(diabetes$insulin)){
  
  if(diabetes$insulin[i]==0){
    
    diabetes$insulin[i]=runif(n=1,xb2$insulin-2*ssd2$insulin
                              ,xb2$insulin+2*ssd2$insulin)
  }
}



## Dealing with Zero Systolic blood pressure

Bp =ifelse(diabetes$blood_pressure==0, 1, 0)
BpMod=gam(diabetes$outcome~Bp, family = binomial)
summary(BpMod)

## result shows that there is not significant effect of having bp
## of non-zero vs having zero bp on diabetes.

## So i think a very good replacement for the zero bp will be some values
##around the mean of the rest of the dataset

## imputation
## Mean of the non-zero bp
xb3=diabetes %>% dplyr::select(blood_pressure) %>%
  dplyr::filter(blood_pressure!=0)%>% map(mean)

## sd of the non-zero bp
ssd3=diabetes %>% dplyr::select(blood_pressure) %>%
  dplyr::filter(blood_pressure!=0)%>% map(sd)

## Fact 2sd away from the mean covers about 95% of the entire datasset.

grid1=runif(n=1,xb3$blood_pressure-2*ssd3$blood_pressure
            ,xb3$blood_pressure+2*ssd3$blood_pressure)

set.seed(23)
for(i in 1:length(diabetes$bmi)){
  
  if(diabetes$blood_pressure[i]==0){
    
    diabetes$blood_pressure[i]=runif(n=1,xb3$blood_pressure-2*ssd3$blood_pressure
                                     ,xb3$blood_pressure+2*ssd3$blood_pressure)
  }
}


## Dealing with Zero Glucose

glucose =ifelse(diabetes$glucose==0, 1, 0)
gluMod=gam(diabetes$outcome~glucose, family = binomial)
summary(gluMod)

## result shows that there is not significant effect of having glu
## of non-zero vs having zero glu on diabetes.

## So i think a very good replacement for the zero glu will be some values
##around the mean of the rest of the dataset

## imputation
## Mean of the non-zero glu
xb4=diabetes %>% dplyr::select(glucose) %>%
  dplyr::filter(glucose!=0)%>% map(mean)

## sd of the non-zero bmi
ssd4=diabetes %>% dplyr::select(glucose) %>%
  dplyr::filter(glucose!=0)%>% map(sd)

## Fact 2sd away from the mean covers about 95% of the entire datasset.

grid1=runif(n=1,xb4$glucose-2*ssd4$glucose
            ,xb4$glucose+2*ssd4$glucose)

set.seed(23)
for(i in 1:length(diabetes$glucose)){
  
  if(diabetes$glucose[i]==0){
    
    diabetes$glucose[i]=runif(n=1,xb4$glucose-2*ssd4$glucose
                              ,xb4$glucose+2*ssd4$glucose)
  }
}




summary(diabetes)


# Create histograms of the variables
diabetes %>% 
  dplyr::select(pregnancies, glucose, blood_pressure, skin_thickness,
                insulin, bmi, age) %>% 
  gather()%>% 
  ggplot(aes(x = value,fill=diabetes$outcome)) + 
  facet_wrap(~key, scales = "free") +
  geom_histogram(binwidth = 5, color = "black", fill = "white") + 
  labs(x = "", y = "Count") +
  theme_bw()








### Checking the relation between age and outcome
d_age1=glm(outcome~age,family=binomial, data=diabetes)






### Modeling Ussing Gams
library(gratia)

## First Model
Mod1=gam(outcome~s(age)+s(dpfunction)+s(glucose)+ s(bmi)+ s(insulin)+
           s(skin_thickness)+ s(blood_pressure)+s(Pregnancies), family = binomial,
         data = diabetes)
summary(Mod1)
draw(Mod1)
appraise(Mod1)
draw(derivatives(Mod1))



## First Model
Mod2=gam(outcome~s(age)+s(dpfunction)+s(glucose)+ s(bmi), family = binomial,
         data = diabetes)
summary(Mod2)
draw(Mod2)
appraise(Mod2)
draw(derivatives(Mod2))


## Second Model
Mod3=gam(outcome~
           s(dpfunction) + te(age, glucose, bmi), 
         method = 'REML',
         family = binomial, data = diabetes)
summary(Mod3)
draw(Mod3)
appraise(Mod3)
#draw(derivatives(Mod3))




#### GLM Methods/ Using Logistic regression
Mod4=glm(outcome~dpfunction+age, family =binomial(link = "logit")
         , data = diabetes)
summary(Mod4)
attach(diabetes)
new_age=seq(20, 80, length=length(diabetes$age))
new_dpf=seq(min(dpfunction),
            max(dpfunction),
            length=length(min(dpfunction)))
X=matrix(c(rep(1, length(new_age)),
           seq(min(dpfunction),max(dpfunction),
               length=length(dpfunction)),
           seq(20, 80, length=length(diabetes$age))
           
),ncol = 3)

#### Creating new fitted values for ploting
new_fit=X%*%matrix(coef(summary(Mod4))[1:3], ncol=1)
orig_fit=exp(new_fit)/(1+exp(new_fit))               


### Doing Same using predict.glm
fit1=predict.glm(Mod4, newdata = data.frame(X), type = "response")

### Plot the 
plot(diabetes$dpfunction, predict(Mod4, type = "response"), ylim = c(0,1))
#plot(new_age, fit1, lwd=1)

plot(Mod4)




Mod5=gam(outcome~s(dpfunction, k=25)+s(age, k=27), family =binomial(link = "logit")
         , data = diabetes)

summary(Mod5)

plot(diabetes$age, predict(Mod5, type = "response"))
plot(diabetes$dpfunction, predict(Mod5, type = "response"))

plot(Mod5)




## Selected Model Based on a lot of statistics

Mod3

plot(diabetes$age, predict(Mod3, type = "response"))
plot(diabetes$dpfunction, predict(Mod3, type = "response"))


###. Separating the data into two for training and testing
set.seed(1237)
dat <- sample(nrow(diabetes), .90*nrow(diabetes), replace = FALSE)
trainSet <- diabetes[dat,]
testSet <- diabetes[-dat,]

## Rebuilding Model
Mod_gam=gam(outcome~
              s(dpfunction) + te(age, glucose, bmi), 
            method = 'REML',
            family = binomial, data = trainSet)

### Measuring Prediction Accuracy

prediction <- ifelse(predict(Mod_gam, testSet, type='response') > 0.5, "diabetic", "nondiabetic")

confusion  <- table(testSet$outcome, prediction)

confusion  <- cbind(confusion, c( confusion[1,1]/(confusion[1,1]+confusion[1,2]),  confusion[2,2]/(confusion[2,1]+confusion[2,2])))


confusion  <- as.data.frame(confusion)
names(confusion) <- c('diabetic', 'nondiabetic', 'misclassification')
confusion








### Machine Learning####

diabetes$outcome <- ifelse(diabetes$outcome==1, "diabetic", "non_diabetic")
#Data Splitting
head(diabetes)
set.seed(1237)
tr.dat <- sample(nrow(diabetes), .90*nrow(diabetes), replace = FALSE)
TrainSet <- diabetes[tr.dat,]
TestSet <- diabetes[-tr.dat,]

#Checking the proportions of the splitted data
#The splitting proportion is 80/20 percent
prop.table(table(diabetes$outcome))
prop.table(table(TrainSet$outcome))
prop.table(table(TestSet$outcome))



#Tuning Parameters
fitControl <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 10, classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "grid", 
                           preProcOptions = c("center", "scale"))

#Random Forest Model
randf <- caret::train(outcome ~ ., data = TrainSet, method = "rf",
                      trControl = fitControl, metric = "ROC")
randf
## Variable importance plot.
plot(varImp(randf))



#Predicting
pred.rf <- predict(randf,TestSet)  
head(pred.rf)
#levels(pred.rf)[1] <- "Diabetes"
tab.rf <-table(factor(TestSet$outcome), pred.rf)
tab.rf<- cbind(tab.rf, c( tab.rf[1,2]/(tab.rf[1,1]+tab.rf[1,2]), 
                          tab.rf[2,1]/(tab.rf[2,1]+tab.rf[2,2])))
colnames(tab.rf) <- c('diabetic', 'non_diabetic', 'misclassification')
tab.rf



#Naive Bayes
nav <- caret::train(outcome ~., data = TrainSet, method ="naive_bayes"  , 
                    trControl = fitControl, metric = "ROC")

nav
#plot(varImp(nav))
#Predicting
pred.nav <- predict(nav,TestSet)  
head(pred.nav)


tab.nav<-table(factor(TestSet$outcome), pred.nav)
tab.nav


#Stochastic Gradient Boost
sgb <- train(outcome ~., data = TrainSet, method = "gbm", 
             metric = "ROC", trControl = fitControl)
sgb
#Predicting
pred.sgb <- predict(sgb,TestSet)  
head(pred.sgb)

tab.sgb<-table(factor(TestSet$outcome), pred.sgb)
tab.sgb


#K Nearest Neighbor
knn <- train(outcome ~., data = TrainSet, method = "knn", 
             metric = "ROC", trControl = fitControl)
knn
#Predicting
pred.knn <- predict(knn,TestSet)  
head(pred.knn)
#levels(pred.rf)[1] <- "Diabetes"
tab.knn <-table(factor(TestSet$outcome), pred.knn)
tab.knn


################# END OF PROJECT CODE ##################################










### Load Required Packages
library(MASS)
library(mgcv)


# Load necessary libraries
library(tidyverse)
library(magrittr)
library(ggpubr)
library(ggplot2)

# Load diabetes dataset
diabetes <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv",
                     header = FALSE,
                     col.names = c("pregnancies", "glucose", "blood_pressure", "skin_thickness", "insulin", "bmi", "dpfunction", "age", "outcome"))


# Print summary of the dataset
summary(diabetes)

# Print the first few rows of the dataset
head(diabetes)


# Check for missing values
sum(is.na(diabetes))


# Create correlation matrix
corr_matrix <- cor(diabetes)
library(ggcorrplot)
# Plot correlation matrix
ggcorrplot(corr_matrix, type = "upper", hc.order = TRUE, lab = TRUE)


# Create histograms of the variables
diabetes %>% 
dplyr::select(pregnancies, glucose, blood_pressure, skin_thickness,
           insulin, bmi, age) %>% 
  gather()%>% 
  ggplot(aes(x = value,fill=diabetes$outcome)) + 
  facet_wrap(~key, scales = "free") +
  geom_histogram(binwidth = 5, color = "black", fill = "white") + 
  labs(x = "", y = "Count") +
  theme_bw()


diabetes %>% dplyr::select(insulin, outcome) %>%
  dplyr::filter(insulin==0) %>% group_by(outcome) %>%
  summarise(count=n()/768)

length(insulin)








## Histogram of diabetes pedigree function
diabetes%>%
  dplyr::select(dpfunction)%>%
  gather()%>%
  ggplot((aes(x=value)))+
  geom_histogram(color="black", fill="white")+
  labs(x="", y="Count")+
  ggtitle("Histogram of dpFunction")



# Create box plots of the variables by outcome
diabetes %>% 
  gather(key = "variable", value = "value", -outcome) %>% 
  ggplot(aes(x = factor(outcome), y = value, fill = factor(outcome))) +
  facet_wrap(~variable, scales = "free") +
  geom_boxplot() +
  labs(x = "Outcome", y = "") +
  theme_bw()




### Determine relationship

library(ggplot2)

# create scatter plot with smoothed line
ggplot(diabetes, aes(x = age, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = pregnancies, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = glucose, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = bmi, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = blood_pressure, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = skin_thickness, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = insulin, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(diabetes, aes(x = dpfunction, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess")


# diabetes %>% 
#   dplyr::select(pregnancies, glucose, blood_pressure, skin_thickness,
#                 insulin, bmi, age) %>% 
#   gather() %>% 
#   ggplot(aes(x = value, y=outcome)) + 
#   facet_wrap(~key, scales = "free") +
#   geom_smooth(method = "loess")  




### Checking the relation between age and outcome
d_age1=glm(outcome~age,family=binomial, data=diabetes)

plot(d_age)#
df <- diabetes %>% dplyr::filter(
  bmi > 0, bmi <= 50, age < 80, glucose > 50, dpfunction < 1.5)
d_age2=gam(outcome~
            s(dpfunction) + te(age, glucose, bmi), 
           method = 'REML',
           family = binomial, data = df)

library(gratia)
summary(d_age2)
draw(d_age2)
appraise(d_age2)
draw(derivatives(d_age2))

plot(d_age2)
gam.check(d_age2)


#### GLM Methods/ Using Logistic regression
Mod2=glm(outcome~dpfunction+age, family =binomial(link = "logit")
         , data = diabetes)
summary(Mod2)
attach(diabetes)
new_age=seq(20, 80, length=length(diabetes$age))
new_dpf=seq(min(dpfunction),
            max(dpfunction),
            length=length(min(dpfunction)))
X=matrix(c(rep(1, length(new_age)),
             seq(min(dpfunction),max(dpfunction),
            length=length(dpfunction)),
  seq(20, 80, length=length(diabetes$age))
                     
             ),ncol = 3)

#### Creating new fitted values for ploting
new_fit=X%*%matrix(coef(summary(Mod2))[1:3], ncol=1)
orig_fit=exp(new_fit)/(1+exp(new_fit))               
               

### Doing Same using predict.glm
fit1=predict.glm(Mod2, newdata = data.frame(X), type = "response")

### Plot the 
plot(diabetes$dpfunction, predict(Mod2, type = "response"), ylim = c(0,1))
#plot(new_age, fit1, lwd=1)

plot(Mod2)




Mod1=gam(outcome~s(dpfunction, k=25)+s(age, k=27), family =binomial(link = "logit")
         , data = diabetes)

summary(Mod1)

plot(diabetes$age, predict(Mod1, type = "response"))
plot(diabetes$dpfunction, predict(Mod1, type = "response"))

plot(Mod1)
