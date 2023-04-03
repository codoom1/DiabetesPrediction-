

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
