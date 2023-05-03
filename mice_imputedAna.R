

### Load Required Packages
library(MASS)
library(mgcv)
library(caret)

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
#### Doing conditional distribution imputation 

# Replace 0s with NA in all columns except pregnancies and outcome
diabetes[, !(names(diabetes) %in% 
               c("pregnancies","dpfunction","age", "outcome"))] <- 
  lapply(diabetes[, !(names(diabetes)%in% c("pregnancies","dpfunction","age", "outcome"))], function(x) ifelse(x == 0, NA, x))
summary(diabetes)


### Imputing the NA's using mice

library(mice)

mids =mice(diabetes, m=5, method = c(" ","pmm","pmm",
                                     "pmm","pmm","pmm"," ", " ", " "))

stripplot(mids)
stripplot(mids, insulin ~ .imp)

imputed_data=complete(mids)

summary(imputed_data)

diabetes=imputed_data
# Create correlation matrix
corr_matrix <- cor(diabetes)


pairs(diabetes)


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




### Modeling Ussing Gams
library(gratia)

## First Model
Mod1=gam(outcome~s(age)+s(dpfunction)+s(glucose)+ s(bmi)+ s(insulin)+
           s(skin_thickness)+ s(blood_pressure)+s(pregnancies), family = binomial,
         data = diabetes)

summary(Mod1)
draw(Mod1)
appraise(Mod1)
draw(derivatives(Mod1))

Modtest <- gam(outcome ~ te(age,blood_pressure), family = binomial, data = diabetes)
summary(Modtest)
draw(Modtest)


## Second Model
Mod3=gam(outcome~
           s(dpfunction) + te(age, glucose, bmi)+s(insulin)+s(skin_thickness)+
           s(blood_pressure)+s(pregnancies), 
         method = 'REML',
         family = binomial, data = diabetes)
summary(Mod3)
draw(Mod3)
appraise(Mod3)


###. Separating the data into two for training and testing
#Data Splitting
head(diabetes)
set.seed(1237)
tr.dat <- sample(nrow(diabetes), .90*nrow(diabetes), replace = FALSE)
TrainSet <- diabetes[tr.dat,]
TestSet <- diabetes[-tr.dat,]

## Rebuilding Model
Mod_gam=gam(outcome~
              s(dpfunction) + te(age, glucose, bmi)+(insulin)+s(skin_thickness)+
              s(blood_pressure)+s(pregnancies),
            method = 'REML',
            family = binomial, data = TrainSet)

### Measuring Prediction Accuracy
summary(Mod_gam)
prediction <- ifelse(predict(Mod_gam, TestSet, type='response') > 0.5, "diabetic", "nondiabetic")

confusion  <- table(TestSet$outcome, prediction)

confusion  <- cbind(confusion, c( confusion[1,1]/(confusion[1,1]+confusion[1,2]),  confusion[2,2]/(confusion[2,1]+confusion[2,2])))


confusion  <- as.data.frame(confusion)
names(confusion) <- c('diabetic', 'nondiabetic', 'misclassification')
rownames(confusion) <- c('Non-Diabetic', 'Diabetic')
View(confusion)








### Machine Learning####
TrainSet$outcome <- ifelse(TrainSet$outcome==1, "diabetic", "non_diabetic")

TestSet$outcome <- ifelse(TestSet$outcome==1, "diabetic", "non_diabetic")

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
tab.nav<- cbind(tab.nav, c( tab.nav[1,2]/(tab.nav[1,1]+tab.nav[1,2]), 
                          tab.nav[2,1]/(tab.nav[2,1]+tab.nav[2,2])))
colnames(tab.nav) <- c('diabetic', 'non_diabetic', 'misclassification')
tab.nav


#Stochastic Gradient Boost
sgb <- train(outcome ~., data = TrainSet, method = "gbm", 
             metric = "ROC", trControl = fitControl)
sgb
#Predicting
pred.sgb <- predict(sgb,TestSet)  
head(pred.sgb)

tab.sgb<-table(factor(TestSet$outcome), pred.sgb)
tab.sgb<- cbind(tab.sgb, c( tab.sgb[1,2]/(tab.sgb[1,1]+tab.sgb[1,2]), 
                          tab.sgb[2,1]/(tab.sgb[2,1]+tab.sgb[2,2])))
colnames(tab.sgb) <- c('diabetic', 'non_diabetic', 'misclassification')
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
tab.knn<- cbind(tab.knn, c( tab.knn[1,2]/(tab.knn[1,1]+tab.knn[1,2]), 
                          tab.knn[2,1]/(tab.knn[2,1]+tab.knn[2,2])))
colnames(tab.knn) <- c('diabetic', 'non_diabetic', 'misclassification')
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
