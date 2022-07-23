# MULTINOMIAL LOGIT REGRESSION for math

#a) Create school clusters through LMM

library(MASS)
library(car)
library(rgl)

library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)       # Main package but only with independent and homoschedastic residuals
library(insight)

library(ggplot2)
library(nnet)


#################
### SCHOOL_ID ###
#################
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")

# DNK

studentsData= read.table(file = "student_dnk.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)
#studentsData$immigration= as.factor(studentsData$immigration)
studentsData$school_id= as.factor(studentsData$school_id)


#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at primary studentsData --> dependency among students within the same studentsData

lmm1 = lmer(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + 
              + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
              + immigration:ESCS_status + (1|school_id), 
            data = studentsData)
summary(lmm1)

# Clustering
rr <- ranef(lmm1)
dd <- as.data.frame(rr)
intervals <- transform(dd, lwr=condval-1.96*condsd, upr=condval+1.96*condsd)
neg_schools <- intervals$grp[which(intervals$lwr<0 & intervals$upr<0)]
pos_schools <- intervals$grp[which(intervals$lwr>0 & intervals$upr>0)]
neut_schools <- intervals$grp[which(intervals$lwr<0 & intervals$upr>0)]

studentsData_neg_schools_dnk <- studentsData[which(studentsData$school_id %in% neg_schools),]
studentsData_pos_schools_dnk <- studentsData[which(studentsData$school_id %in% pos_schools),]
studentsData_neut_schools_dnk <- studentsData[which(studentsData$school_id %in% neut_schools),]
summary(studentsData_neg_schools_dnk)
summary(studentsData_pos_schools_dnk)
summary(studentsData_neut_schools_dnk)


# GBR

studentsData= read.table(file = "student_gbr.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)
#studentsData$immigration= as.factor(studentsData$immigration)
studentsData$school_id= as.factor(studentsData$school_id)


#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at primary studentsData --> dependency among students within the same studentsData

lmm1 = lmer(math ~ gender + fear_failure + bullied + ESCS_status + 
              + (1|school_id), 
            data = studentsData)
summary(lmm1)

rr <- ranef(lmm1)
dd <- as.data.frame(rr)
intervals <- transform(dd, lwr=condval-1.96*condsd, upr=condval+1.96*condsd)
neg_schools <- intervals$grp[which(intervals$lwr<0 & intervals$upr<0)]
pos_schools <- intervals$grp[which(intervals$lwr>0 & intervals$upr>0)]
neut_schools <- intervals$grp[which(intervals$lwr<0 & intervals$upr>0)]

studentsData_neg_schools_gbr <- studentsData[which(studentsData$school_id %in% neg_schools),]
studentsData_pos_schools_gbr <- studentsData[which(studentsData$school_id %in% pos_schools),]
studentsData_neut_schools_gbr <- studentsData[which(studentsData$school_id %in% neut_schools),]

summary(studentsData_neg_schools_gbr)
summary(studentsData_pos_schools_gbr)
summary(studentsData_neut_schools_gbr)

# Create dataframe sof interest
school_neg_dnk <- data.frame(studentsData_neg_schools_dnk$fear_failure,
                         studentsData_neg_schools_dnk$belonging,
                         studentsData_neg_schools_dnk$bullied,
                         studentsData_neg_schools_dnk$teacher_support,
                         studentsData_neg_schools_dnk$emo_sup,
                         studentsData_neg_schools_dnk$class_size,
                         studentsData_neg_schools_dnk$stud_teach_ratio,
                         studentsData_neg_schools_dnk$short_edu_mat,
                         studentsData_neg_schools_dnk$short_edu_staff,
                         studentsData_neg_schools_dnk$stu_behav,
                         studentsData_neg_schools_dnk$teach_behav,
                         studentsData_neg_schools_dnk$ESCS_status,
                         studentsData_neg_schools_dnk$immigration,
                         group = -1 )

school_pos_dnk <- data.frame(studentsData_pos_schools_dnk$fear_failure,
                         studentsData_pos_schools_dnk$belonging,
                         studentsData_pos_schools_dnk$bullied,
                         studentsData_pos_schools_dnk$teacher_support,
                         studentsData_pos_schools_dnk$emo_sup,
                         studentsData_pos_schools_dnk$class_size,
                         studentsData_pos_schools_dnk$stud_teach_ratio,
                         studentsData_pos_schools_dnk$short_edu_mat,
                         studentsData_pos_schools_dnk$short_edu_staff,
                         studentsData_pos_schools_dnk$stu_behav,
                         studentsData_pos_schools_dnk$teach_behav,
                         studentsData_pos_schools_dnk$ESCS_status,
                         studentsData_pos_schools_dnk$immigration,
                         group = 1)

school_neut_dnk <- data.frame(studentsData_neut_schools_dnk$fear_failure,
                             studentsData_neut_schools_dnk$belonging,
                             studentsData_neut_schools_dnk$bullied,
                             studentsData_neut_schools_dnk$teacher_support,
                             studentsData_neut_schools_dnk$emo_sup,
                             studentsData_neut_schools_dnk$class_size,
                             studentsData_neut_schools_dnk$stud_teach_ratio,
                             studentsData_neut_schools_dnk$short_edu_mat,
                             studentsData_neut_schools_dnk$short_edu_staff,
                             studentsData_neut_schools_dnk$stu_behav,
                             studentsData_neut_schools_dnk$teach_behav,
                             studentsData_neut_schools_dnk$ESCS_status,
                             studentsData_neut_schools_dnk$immigration,
                             group = 0)

school_neg_gbr <- data.frame(studentsData_neg_schools_gbr$fear_failure,
                             studentsData_neg_schools_gbr$belonging,
                             studentsData_neg_schools_gbr$bullied,
                             studentsData_neg_schools_gbr$teacher_support,
                             studentsData_neg_schools_gbr$emo_sup,
                             studentsData_neg_schools_gbr$class_size,
                             studentsData_neg_schools_gbr$stud_teach_ratio,
                             studentsData_neg_schools_gbr$short_edu_mat,
                             studentsData_neg_schools_gbr$short_edu_staff,
                             studentsData_neg_schools_gbr$stu_behav,
                             studentsData_neg_schools_gbr$teach_behav,
                             studentsData_neg_schools_gbr$ESCS_status,
                             studentsData_neg_schools_gbr$immigration,
                             group = -1 )

school_pos_gbr <- data.frame(studentsData_pos_schools_gbr$fear_failure,
                             studentsData_pos_schools_gbr$belonging,
                             studentsData_pos_schools_gbr$bullied,
                             studentsData_pos_schools_gbr$teacher_support,
                             studentsData_pos_schools_gbr$emo_sup,
                             studentsData_pos_schools_gbr$class_size,
                             studentsData_pos_schools_gbr$stud_teach_ratio,
                             studentsData_pos_schools_gbr$short_edu_mat,
                             studentsData_pos_schools_gbr$short_edu_staff,
                             studentsData_pos_schools_gbr$stu_behav,
                             studentsData_pos_schools_gbr$teach_behav,
                             studentsData_pos_schools_gbr$ESCS_status,
                             studentsData_pos_schools_gbr$immigration,
                             group = 1)

school_neut_gbr <- data.frame(studentsData_neut_schools_gbr$fear_failure,
                             studentsData_neut_schools_gbr$belonging,
                             studentsData_neut_schools_gbr$bullied,
                             studentsData_neut_schools_gbr$teacher_support,
                             studentsData_neut_schools_gbr$emo_sup,
                             studentsData_neut_schools_gbr$class_size,
                             studentsData_neut_schools_gbr$stud_teach_ratio,
                             studentsData_neut_schools_gbr$short_edu_mat,
                             studentsData_neut_schools_gbr$short_edu_staff,
                             studentsData_neut_schools_gbr$stu_behav,
                             studentsData_neut_schools_gbr$teach_behav,
                             studentsData_neut_schools_gbr$ESCS_status,
                             studentsData_neut_schools_gbr$immigration,
                             group = 0 )

table(studentsData_neg_schools_gbr$immigration)
table(studentsData_pos_schools_gbr$immigration)
table(studentsData_neg_schools_dnk$immigration)
table(studentsData_pos_schools_dnk$immigration)

# GBR

X <- as.data.frame(rbind(as.matrix(school_pos_gbr),as.matrix(school_neg_gbr),as.matrix(school_neut_gbr)))
X.values <- X[-14]
X.groups<- factor(X$group, labels=c("neg","neut","pos"))
X[14] <- X.groups

# logit
X$group2 <- relevel(X$group, ref="neut")
test <- multinom(group2 ~ . -group, data = X)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# DNK

X <- as.data.frame(rbind(as.matrix(school_pos_dnk),as.matrix(school_neg_dnk),as.matrix(school_neut_dnk)))
X.values <- X[-14]
X.groups<- factor(X$group, labels=c("neg","neut","pos"))
X[14] <- X.groups

# logit
X$group2 <- relevel(X$group, ref="neut")
test <- multinom(group2 ~ . -group, data = X)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Manova
group1 <- X.values[which(X.groups=="pos"),]
group2 <- X.values[which(X.groups=="neg"),]
g <- 2
p <- dim(group1)[2]
n1 <- dim(group1)[1]
n2 <- dim(group2)[1]
n <- n1+n2
k <- p*g*(g-1)/2
man <- manova(as.matrix(X.values) ~ X.groups)
SSres <- summary.manova(man)$SS$Residuals
Spooled <- SSres/(n-g)
mean.g1 <- sapply(group1,mean)
mean.g2 <- sapply(group2,mean)
alpha <- 0.01
qT <- qt(1-alpha/2/k,n-g)
conf.int.B <- cbind(inf=mean.g1-mean.g2 - qT*sqrt(diag(Spooled)*(1/n1+1/n2)),mean.dif=mean.g1-mean.g2, sup=mean.g1-mean.g2 + qT*sqrt(diag(Spooled)*(1/n1+1/n2)  ))
conf.int.B  
