library(MASS)
library(car)
library(rgl)


library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4) #main package but only w/ resduals indep and homosk
library(insight)

library(ggplot2)

###############
### COUNTRY ###
###############

studentsData= read.table(file = "student_eur.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)
studentsData <- studentsData[,-10]
studentsData$immigration= as.factor(studentsData$immigration)
studentsData$school_id= as.factor(studentsData$school_id)
studentsData$country= as.factor(studentsData$country)

#math
x11()
ggplot(data=studentsData, aes(x=as.factor(country), y=math, fill=as.factor(country))) +
  geom_boxplot() +
  labs(x='Country', y='Math Achievement') +
  ggtitle('Boxplot of math achievements among countries') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.15)),axis.title=element_text(size=rel(1.5)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')

#read
x11()
ggplot(data=studentsData, aes(x=as.factor(country), y=read, fill=as.factor(country))) +
  geom_boxplot() +
  labs(x='Country', y='Read Achievement') +
  ggtitle('Boxplot of math achievements among countries') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.15)),axis.title=element_text(size=rel(1.5)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')


#studentsDataNative <- studentsData[which(studentsData$immigration==0),]
#studentsDataImmigrant <- studentsData[which(studentsData$immigration==1),]



##--------------##
## Linear Model ##
##--------------##

# We start with a standard linear regression model, neglecting the dependence structure

# MODEL: achiev_i = beta_0 + beta_1*gender_i+ beta_2*ESCS_status_i + eps_i
# eps_i ~ N(0, sigma2_eps)

lm1 = lm(math ~ immigration + ESCS_status, data = studentsData) #provo con modelli più complessi trovati da luci
summary(lm1)

plot(studentsData$ESCS_status,studentsData$math, col='blue')
abline(512.5298,29.2562, col='green', lw=4)          # native
abline(512.5298 -23.8581,29.2562, col='orange', lw=4)  # immigrants

plot(lm1$residuals)

boxplot(lm1$residuals ~ studentsData$country, col='orange', xlab='country', ylab='Residuals')
## residuals don't differ a lot across countries


#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at country --> dependency among students within the same country

# MODEL: achiev_ij = beta_0 + beta_1*gender_ij + beta_2*ESCS_status_ij + b_i + eps_ij
# eps_ij ~ N(0, sigma2_eps)
# b_i ~ N(0, sigma2_b)

lmm1 = lmer(math ~ immigration + ESCS_status + (1|country), 
            data = studentsData)
summary(lmm1)


# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE)
fixef(lmm1)

# The fixed effects tell us there is a negative effect of being immigrant on achievement, 
# and on average, students with higher ESCS_status are associated to higher achievement scores.



# Variance components
#--------------------
# One thing that's new compared to the standard regression output is the estimated 
# variance/standard deviation of the country effect.
# This tells us how much, on average, achievement bounces around as we move from country to country 
# In other words, even after making a prediction based on student covariates, each country has its
# own unique deviation, and that value (in terms of the standard deviation) is the estimated 
# average deviation across countries 

print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
help(get_variance)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b

# Another way to interpret the variance output is to note percentage of the student variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# masci: PVRE = 41.8% is very high!
# PVRE = 0.01561511 piuttosto bassa per il nostro modello

# Random effects: b_0i
#----------------------------
ranef(lmm1)

# The dotplot shows the point and interval estimates for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)

x11()
dotplot(ranef(lmm1))

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)
head(coef(lmm1)$country)


## visualization of the coefficients, non ho molto capito
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm1)$country[1]),
     xlab='country i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts + fixed intercepts')
abline(h=fixef(lmm1)[1], lty=2, col='red', lwd=2)
legend(30, 14, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$country[2]),
     xlab='country i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for gender')
abline(h=fixef(lmm1)[2], lty=2, col='red', lwd=2)
legend(30, -0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$country[3]),
     xlab='country i', ylab=expression(beta[2]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for ESCS_status')
abline(h=fixef(lmm1)[3], lty=2, col='red', lwd=2)
legend(30, 2.35, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(studentsData$ESCS_status[studentsData$immigration==0], studentsData$math[studentsData$immigration==0],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for natives')
    
for(i in 1:50){
  abline(coef(lmm1)$country[i,1], coef(lmm1)$country[i,3])
}

## MALES
plot(studentsData$ESCS_status[studentsData$immigration==1], studentsData$math[studentsData$immigration==1],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for immigrants')
      
for(i in 1:50){
  abline(coef(lmm1)$country[i,1] + coef(lmm1)$country[i,2], coef(lmm1)$country[i,3])
}


# Diagnostic plots 
#------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm1)

x11()
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm1)$country), main='Normal Q-Q Plot - Random Effects for Primary studentsData')
qqline(unlist(ranef(lmm1)$country), col='red', lwd=2)

#--------------------------------------------------#
# Linear Mixed Model with Random Intercept & Slope #
#--------------------------------------------------#
graphics.off()

## We now consider the possibility that the association between ESCS_status and student achievements differs across studentsDatas.
## We include a random slope for the ESCS_status to model this additional source of heterogeneity. 

# MODEL:  achiev_ij = beta_0 + b_0i + (beta_1 + b_1i)*ESCS_status_i + eps_i --> homoscedastic residuals 

# eps_i ~ N(0, sigma2_eps)
# Random effects: b_i ~ N(0, Sigma)

# To allow both the intercept, represented by 1, and the slope, represented by ESCS_status,
# to vary by student we can add the term:
#   - (1+ESCS_status|studentsData_id)
# or, in alternative, without 1
#   - (ESCS_status|studentsData_id)

lmm2 = lmer(math ~ immigration + ESCS_status + (1 + ESCS_status|country), 
            data = studentsData)
summary(lmm2)

confint(lmm2, oldNames=TRUE)

# Note that the mean slope for the ESCS_status effect, our fixed effect, is 1.84, but 
# from studentsData to studentsData it bounces around. 

# Yet another point of interest is the correlation of the intercepts and slopes. In this case it's 0.16. 
# That's pretty small, but the interpretation is the same as with any correlation. 

# Variance components
#--------------------
# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.

print(vc <- VarCorr(lmm2), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm2))  ## it automatically computes Var(b0,b1)
# 4.3228 + 2*0.164*2.0791*1.6451* mean(studentsData$ESCS_status, na.rm=T) + 2.7063*mean(studentsData$ESCS_status^2, na.rm=T)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

#masci: PVRE = 56%
#0.01820131

# Estimates of fixed and random effects
#--------------------------------------

# Fixed effects: (beta_0, beta_1, beta_2)
fixef(lmm2)

# Random effects: (b_0i, b_1i) for i=1,...,200
ranef(lmm2)
head(ranef(lmm2)$country)

x11()
dotplot(ranef(lmm2))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm2)
head(coef(lmm2)$studentsData)

## Visualization of random effects 
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm2)$country[1]),
     xlab='country i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts')
abline(h=fixef(lmm2)[1], lty=2, col='red', lwd=2)
legend(30, 13.5, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[2]),
     xlab='country i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slope for gender')
abline(h=fixef(lmm2)[2], lty=2, col='red', lwd=2)
legend(30,-0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[3]),
     xlab='Student i', ylab=expression(beta[2]+b['1i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random slopes for ESCS_status')
abline(h=fixef(lmm2)[3], lty=2, col='red', lwd=2)
legend(30, 5, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)


# Lines Visualization
#---------------------


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(studentsData$ESCS_status[studentsData$immigration==0], studentsData$math[studentsData$immigration==0],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for females')
       

for(i in 1:50){
  abline(coef(lmm2)$country[i,1], coef(lmm2)$country[i,3])
}

## MALES
plot(studentsData$ESCS_status[studentsData$immigration==1], studentsData$math[studentsData$immigration==1],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for males')

for(i in 1:50){
  abline(coef(lmm2)$country[i,1] + coef(lmm2)$country[i,2], coef(lmm2)$country[i,3])
}


# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm2)

x11()
qqnorm(resid(lmm2))
qqline(resid(lmm2), col='red', lwd=2)


# 2) Assessing Assumption on the Random Effects
x11()
par(mfrow=c(1,2))
qqnorm(unlist(ranef(lmm2)$country[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm2)$country[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm2)$country[2]), main='Normal Q-Q Plot - Random Effects on ESCS_status')
qqline(unlist(ranef(lmm2)$country[2]), col='red', lwd=2)

x11()
plot(unlist(ranef(lmm2)$country[2]),unlist(ranef(lmm2)$country[1]),
     ylab=expression(paste('Intercept  ', b['0i'])),
     xlab=expression(paste('ESCS_status  ', b['1i'])), col='dodgerblue2',
     main='Scatterplot of estimated random effects')
abline(v=0,h=0)
# Alternative plot(ranef(lmm2))


# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(lmm1, lmm2)

# The p-value for the test is essentially zero -> we prefer lmm2

################

lmm3 = lmer(math ~ immigration + ESCS_status + (1 + immigration|country), 
            data = studentsData)
summary(lmm3)

confint(lmm3, oldNames=TRUE)#escs non significativo

# Variance components
#--------------------
# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.

print(vc <- VarCorr(lmm3), comp = c("Variance", "Std.Dev."))

sigma3_eps <- as.numeric(get_variance_residual(lmm3))
sigma3_eps
sigma3_b <- as.numeric(get_variance_random(lmm3))  ## it automatically computes Var(b0,b1)
# 4.3228 + 2*0.164*2.0791*1.6451* mean(studentsData$ESCS_status, na.rm=T) + 2.7063*mean(studentsData$ESCS_status^2, na.rm=T)
sigma3_b

PVRE <- sigma3_b/(sigma3_b+sigma3_eps)
PVRE

#masci: PVRE = 56%
#0.01820131

# Estimates of fixed and random effects
#--------------------------------------

# Fixed effects: (beta_0, beta_1, beta_2)
fixef(lmm3)

# Random effects: (b_0i, b_1i) for i=1,...,200
ranef(lmm3)
head(ranef(lmm3)$country)

x11()
dotplot(ranef(lmm3))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm3)
head(coef(lmm3)$country)

## Visualization of random effects 
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm2)$country[1]),
     xlab='country i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts')
abline(h=fixef(lmm2)[1], lty=2, col='red', lwd=2)
legend(30, 13.5, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[2]),
     xlab='country i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slope for gender')
abline(h=fixef(lmm2)[2], lty=2, col='red', lwd=2)
legend(30,-0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)

plot(unlist(coef(lmm2)$country[3]),
     xlab='Student i', ylab=expression(beta[2]+b['1i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random slopes for ESCS_status')
abline(h=fixef(lmm2)[3], lty=2, col='red', lwd=2)
legend(30, 5, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)


# Lines Visualization
#---------------------


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(studentsData$ESCS_status[studentsData$immigration==0], studentsData$math[studentsData$immigration==0],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for females')


for(i in 1:50){
  abline(coef(lmm2)$country[i,1], coef(lmm2)$country[i,3])
}

## MALES
plot(studentsData$ESCS_status[studentsData$immigration==1], studentsData$math[studentsData$immigration==1],col='blue',
     xlab='ESCS_status', ylab='achievement',main='Data and regression lines for males')

for(i in 1:50){
  abline(coef(lmm2)$country[i,1] + coef(lmm2)$country[i,2], coef(lmm2)$country[i,3])
}


# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm2)

x11()
qqnorm(resid(lmm2))
qqline(resid(lmm2), col='red', lwd=2)


# 2) Assessing Assumption on the Random Effects
x11()
par(mfrow=c(1,2))
qqnorm(unlist(ranef(lmm2)$country[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm2)$country[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm2)$country[2]), main='Normal Q-Q Plot - Random Effects on ESCS_status')
qqline(unlist(ranef(lmm2)$country[2]), col='red', lwd=2)

x11()
plot(unlist(ranef(lmm2)$country[2]),unlist(ranef(lmm2)$country[1]),
     ylab=expression(paste('Intercept  ', b['0i'])),
     xlab=expression(paste('ESCS_status  ', b['1i'])), col='dodgerblue2',
     main='Scatterplot of estimated random effects')
abline(v=0,h=0)
# Alternative plot(ranef(lmm2))


# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(lmm1, lmm3)

# The p-value for the test is essentially zero -> we prefer lmm2






