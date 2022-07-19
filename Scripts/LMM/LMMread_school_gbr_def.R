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

#################
### SCHOOL_ID ###
#################
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features2")
studentsData= read.table(file = "student_gbr.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)
#studentsData$immigration= as.factor(studentsData$immigration)
studentsData$school_id= as.factor(studentsData$school_id)

#math
x11()
ggplot(data=studentsData, aes(x=as.factor(school_id), y=math, fill=as.factor(school_id))) +
  geom_boxplot() +
  labs(x='school_id', y='Math Achievement') +
  ggtitle('Boxplot of math achievements among countries') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.15)),axis.title=element_text(size=rel(1.5)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')


##--------------##
## Linear Model ##
##--------------##
# We start with a standard linear regression model, neglecting the dependence structure
lm1 <- lm(math ~ gender + hisced + fear_failure + bullied + ESCS_status + 
            + learn_time_math, data = studentsData)
summary(lm1)

plot(lm1$residuals)

boxplot(lm1$residuals ~ studentsData$school_id, col='orange', xlab='studentsData ID', ylab='Residuals')
## residuals differ a lot across schools

#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at primary studentsData --> dependency among students within the same studentsData

lmm1 = lmer(math ~ gender + hisced + fear_failure + bullied + ESCS_status + 
              + learn_time_math + (1|school_id), 
            data = studentsData)
summary(lmm1)


# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE) #hisced e learn_time_math non significant al 95% probabilmente al 99% si
#forse meglio rifare modello senza
fixef(lmm1)

# Variance components
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b

#Percentage of Variance explained by the Random Effect (PVRE).
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #0.135587

# Random effects: b_0i
#----------------------------
ranef(lmm1) #uso per clustering

x11()
dotplot(ranef(lmm1))

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)

## visualization of the coefficients
x11()
par(mfrow=c(1,3))
plot(unlist(coef(lmm1)$school_id[1]),
     xlab='school_id i', ylab=expression(beta[0]+b['0i']),
     pch=19, lwd=2, col='darkblue',
     main='Estimated random intercepts + fixed intercepts')
abline(h=fixef(lmm1)[1], lty=2, col='red', lwd=2)
legend(30, 14, legend=expression(paste('Fixed intercept ',beta[0])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$school_id[2]),
     xlab='school_id i', ylab=expression(beta[1]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for gender')
abline(h=fixef(lmm1)[2], lty=2, col='red', lwd=2)
legend(30, -0.6, legend=expression(paste('Fixed slope ',beta[1])), lwd=2, lty=2, col='red', x.intersp=0.5)
plot(unlist(coef(lmm1)$school_id[3]),
     xlab='school_id i', ylab=expression(beta[2]),
     pch=19, lwd=2, col='darkblue',
     main='Estimated fixed slopes for ESCS_status')
abline(h=fixef(lmm1)[3], lty=2, col='red', lwd=2)
legend(30, 2.35, legend=expression(paste('Fixed slope ',beta[2])), lwd=2, lty=2, col='red', x.intersp=0.5)

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
qqnorm(unlist(ranef(lmm1)$school_id), main='Normal Q-Q Plot - Random Effects for Primary studentsData')
qqline(unlist(ranef(lmm1)$school_id), col='red', lwd=2)


#clustering
rr <- ranef(lmm1)
dd <- as.data.frame(rr)
intervals <- transform(dd, lwr=condval-1.96*condsd, upr=condval+1.96*condsd)
neg_schools <- intervals$grp[which(intervals$lwr<0 & intervals$upr<0)]
pos_schools <- intervals$grp[which(intervals$lwr>0 & intervals$upr>0)]

studentsData_neg_schools <- studentsData[which(studentsData$school_id %in% neg_schools),]
studentsData_pos_schools <- studentsData[which(studentsData$school_id %in% pos_schools),]
summary(studentsData_neg_schools)
summary(studentsData_pos_schools)

table(studentsData_neg_schools$private)
table(studentsData_pos_schools$private)
