#PCA for each feature category (UNSUCCESFUL UP TO NOW)

## Wealth features
student_wealth_features = data.frame(ITA$HOMEPOS,ITA$WEALTH,ITA$CULTPOSS,ITA$HEDRES,ITA$ICTRES,ITA$ESCS) #ITA$ICTHOME in boxplot è troppo diversa dalle altre
student_wealth_features = na.omit(student_wealth_features) #bene, ci sono quas tutti i dati
boxplot(student_wealth_features, las=2, col='gold') #ok, non standardizzo
sw = princomp(student_wealth_features)
summary(sw)

plot(cumsum(sw$sde^2)/sum(sw$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(student_wealth_features),labels=1:ncol(student_wealth_features),las=2)

sw$loadings #prima pc spiega il 60% ed è una media di tutte le var, con la seconda pc arriviamo a 70% ed è un confronto tra hedres e altre var
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(sw$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

student_wealth = sw$scores[,1] #prendo solo il primo

## Parents Education features
parents_edu_features = data.frame(ITA$HISCED, ITA$FISCED, ITA$MISCED)
parents_edu_features = na.omit(parents_edu_features)
boxplot(parents_edu_features, las=2, col='gold')
pe = princomp(parents_edu_features)
summary(pe) #già 80% con prima pc, che è esattamente media dei tre
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(pe$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

parents_education = pe$scores[,1]

## Open Minded Parents features
open_mindp_features = data.frame(ITA$GCAWAREP,ITA$INTCULTP, ITA$JOYREADP) #ITA$ATTIMP
open_mindp_features = na.omit(open_mindp_features) #molti na, controllo altri paesi
#apply(X = is.na(open_mindp_features), MARGIN = 2, FUN = sum)
boxplot(open_mindp_features, las=2, col = 'gold')

omp = princomp(open_mindp_features)
summary(omp) #56% con 1 (media), 80% con 2
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(omp$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

open_mind = omp$scores[,1]

## Open Minded Student features
open_minds_features = data.frame(ITA$GLOBMIND,ITA$AWACOM,ITA$ATTIMM,
                                 ITA$RESPECT,ITA$COGFLEX,ITA$PERSPECT,ITA$INTCULT,ITA$GCAWARE,ITA$GCSELFEFF)
open_minds_features = na.omit(open_minds_features) #molti na, controllo altri paesi
#apply(X = is.na(open_minds_features), MARGIN = 2, FUN = sum)
boxplot(open_minds_features, las=2, col = 'gold')

oms = princomp(open_minds_features)
summary(oms) #60% con 3, 80% con 6
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(oms$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

open_mind = om$scores[,1]#capisco cosa prendere

##School Environment features
school_envir_features = data.frame(ITA$PERCOMP, ITA$PERCOOP, ITA$BELONG, ITA$TEACHINT, ITA$TEACHSUP)
school_envir_features = na.omit(school_envir_features)
boxplot(school_envir_features, las=2, col='gold')
se = princomp(school_envir_features)
summary(se)
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(se$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

parents_education = se$scores[,1]


## Soft skills features
soft_skills_features = data.frame(ITA$RESILIENCE,ITA$WORKMAST, ITA$COMPETE, ITA$ATTLNACT, ITA$METASPAM, ITA$METASUM, ITA$UNDREM)
apply(X = is.na(soft_skills_features), MARGIN = 2, FUN = sum)
soft_skills_features = na.omit(soft_skills_features)
sk = princomp(soft_skills_features)
summary(sk)#60% con 3
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(sk$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

soft_skills = sk$scores[,c(1,2,3)]


##Negative Feelings features
negative_feelings_features = data.frame(ITA$BEINGBULLIED,ITA$GFOFAIL)
#apply(X = is.na(negative_feelings_features), MARGIN = 2, FUN = sum)
negative_feelings_features = na.omit(negative_feelings_features)
nf = princomp(negative_feelings_features)
summary(nf)#56% con 1
par(mar = c(2,2,2,1), mfrow=c(2,1))
for(i in 1:3)barplot(nf$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

negative_feelings = nf$scores[,1]

#Positive Feelings
ITA$EUDMO






#SEBA

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

#Immigration


