#PCA for each feature category - Italy 

library(haven)

# Read OECD Pisa 2018 student file and select the data about ITALY
student <- read_sas("cy07_msu_stu_qqq.sas7bdat")
ITA = student[which(student$CNTRYID==380),]


## Wealth features 

student_wealth_features = data.frame(ITA$HOMEPOS,ITA$WEALTH,ITA$CULTPOSS,ITA$HEDRES,ITA$ICTRES,ITA$ESCS) #ITA$ICTHOME in boxplot ? troppo diversa dalle altre
student_wealth_features = na.omit(student_wealth_features)    # there are almost all the data (we delete few rows)
boxplot(student_wealth_features, las=2, col='lightgreen')           # => we can avoid the standardization

sw = princomp(student_wealth_features)
summary(sw)
# The first principal component explains the 60% of the variability and is almost the mean of all the variables 
# The second principal component explains the 70% and is a contrast between hedres and the other variables 

x11()
plot(cumsum(sw$sde^2)/sum(sw$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(student_wealth_features),labels=1:ncol(student_wealth_features),las=2)

sw$loadings 
x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(sw$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

student_wealth = sw$scores[,1]     


## Parents Education features

parents_edu_features = data.frame(ITA$HISCED, ITA$FISCED, ITA$MISCED)
parents_edu_features = na.omit(parents_edu_features)
boxplot(parents_edu_features, las=2, col='lightgreen')

pe = princomp(parents_edu_features)
summary(pe) 
# The first principal component explains the 78% and it's exactly the mean of the three variables 

pe$loadings
x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(pe$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

parents_education = pe$scores[,1]


## Open Minded Parents features

open_mindp_features = data.frame(ITA$GCAWAREP,ITA$INTCULTP, ITA$JOYREADP) 
open_mindp_features = na.omit(open_mindp_features) 
# There are many na data 
# apply(X = is.na(open_mindp_features), MARGIN = 2, FUN = sum)
boxplot(open_mindp_features, las=2, col = 'lightgreen')

omp = princomp(open_mindp_features)
summary(omp) 
# The first principal component explains the 56% of the variability  
# The second principal component explains the 81% of the variability

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(omp$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

open_mind = omp$scores[,1]


## Open Minded Student features

open_minds_features = data.frame(ITA$GLOBMIND,ITA$AWACOM,ITA$ATTIMM,
                                 ITA$RESPECT,ITA$COGFLEX,ITA$PERSPECT,ITA$INTCULT,ITA$GCAWARE,ITA$GCSELFEFF)
open_minds_features = na.omit(open_minds_features) 
# There are many na data 
# apply(X = is.na(open_minds_features), MARGIN = 2, FUN = sum)
boxplot(open_minds_features, las=2, col = 'lightgreen')

oms = princomp(open_minds_features)
summary(oms) 
# The first three principal components explain the 60% of the variability
# The first six principal components explain the 80% of the variability

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(oms$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

open_mind = om$scores[,1] 


## School Environment features

school_envir_features = data.frame(ITA$PERCOMP, ITA$PERCOOP, ITA$BELONG, ITA$TEACHINT, ITA$TEACHSUP)
school_envir_features = na.omit(school_envir_features)
boxplot(school_envir_features, las=2, col='lightgreen')

se = princomp(school_envir_features)
summary(se)
# The first two principal components explain the 55,8% of the variability 
# The first four principal components explain the 88,9% of the variability

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(se$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

parents_education = se$scores[,1]


## Soft skills features

soft_skills_features = data.frame(ITA$RESILIENCE,ITA$WORKMAST, ITA$COMPETE, ITA$ATTLNACT, ITA$METASPAM, ITA$METASUM, ITA$UNDREM)
apply(X = is.na(soft_skills_features), MARGIN = 2, FUN = sum)
soft_skills_features = na.omit(soft_skills_features)

sk = princomp(soft_skills_features)
summary(sk)
# The first three principal components explain the 60% of the variability

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(sk$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

soft_skills = sk$scores[,c(1,2,3)]


## Negative Feelings features

negative_feelings_features = data.frame(ITA$BEINGBULLIED,ITA$GFOFAIL)
# apply(X = is.na(negative_feelings_features), MARGIN = 2, FUN = sum)
negative_feelings_features = na.omit(negative_feelings_features)

nf = princomp(negative_feelings_features)
summary(nf)
# The first principal component explains the 56% of the variability

x11()
par(mar = c(2,2,2,1), mfrow=c(2,1))
for(i in 1:3)barplot(nf$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

negative_feelings = nf$scores[,1]




# Altri tentativi 

## Wealth features
student_wealth_features = data.frame(ITA$HOMEPOS,ITA$WEALTH)
sw = princomp(na.omit(student_wealth_features))
summary(sw)
sw$loadings
student_wealth = sw$scores[,1]

## Emotional features
student_emotion_features = data.frame(ITA$COMPETE,ITA$GFOFAIL,ITA$RESILIENCE,ITA$BELONG,
                                      ITA$BEINGBULLIED)
se = princomp(na.omit(student_emotion_features))
summary(se)
se$loadings
student_emotion = se$scores[,c(1,2,3,4)]

#Home and Family features
student_homefamily_features = data.frame(ITA$MISCED,ITA$FISCED,ITA$HISCED,ITA$CULTPOSS,ITA$HEDRES,ITA$EMOSUPP,
                                         ITA$INTCULTP,ITA$ESCS) 
shf = princomp(na.omit(student_homefamily_features))
summary(shf)
shf$loadings
student_homefamily = shf$scores[,c(1,2,3,4,5)]

