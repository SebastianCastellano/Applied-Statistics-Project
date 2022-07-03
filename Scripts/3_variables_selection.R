#Script to extract the most relevant features

#Import Labraries
#library(sas7bdat)
library(labstatR)
#install.packages("naniar")
#library(naniar)

#########################################
############## COUNTRIES ################
#########################################
#Import the selected countries (countries with at least 1000 immig studs within the study)
#setwd("../txt - files/European countries raw")

ITA= read.table(file = "italy.txt", header = T)
AUT= read.table(file = "austria.txt", header = T)
BEL= read.table(file = "belgium.txt", header = T)
DNK= read.table(file = "denmark.txt", header = T)
DEU= read.table(file = "germany.txt", header = T)
LUX= read.table(file = "luxembourg.txt", header = T)
ESP= read.table(file = "spain.txt", header = T)
SWE= read.table(file = "sweden.txt", header = T)
CHE= read.table(file = "swiss.txt", header = T)
GBR= read.table(file = "greatbrit.txt", header = T)


########################################
######## Extracting variables ##########
######################################## 

#List of countries to iterate through
countries = list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)

#List of names of countries
name_countries = c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
name_files = c('student_ita.txt', 'student_aut.txt', 'student_bel.txt', 'student_dnk.txt', 'student_deu.txt', 'student_lux.txt', 'student_esp.txt',
               'student_swe.txt', 'student_che.txt', 'student_gbr.txt')
n_countries = length(countries) 

for (i in 1:n_countries) {
  # Categorical variables
  # school ID
  school_id = as.character(countries[[i]]$CNTSCHID)
  # Gender (male=1, female=0)
  gender = as.character(countries[[i]]$ST004D01T - 1)
  # Immigration
  immigration = as.character(countries[[i]]$IMMIG)
  #grade repetition
  grade_rep = countries[[i]]$REPEAT

  student = data.frame(school_id = school_id, 
                       gender = gender, 
                       immigration = immigration, 
                       language = countries[[i]]$LANGN, #language spoken at home
                       hisced = countries[[i]]$HISCED,  #highest education of parent
                       grade_rep = grade_rep, 
                       joy_read = countries[[i]]$JOYREAD, 
                       pisa_difficulty = countries[[i]]$PISADIFF, #perception of difficulty of the pisa test
                       competitiveness = countries[[i]]$COMPETE,
                       fear_failure = countries[[i]]$GFOFAIL, 
                       resilience = countries[[i]]$RESILIENCE, 
                       belonging = countries[[i]]$BELONG, #sense of belonging in school
                       bullied = countries[[i]]$BEINGBULLIED,
                       home_poss = countries[[i]]$HOMEPOS, 
                       cult_poss = countries[[i]]$CULTPOSS, 
                       edu_resources = countries[[i]]$HEDRES, #home educational resources
                       family_wealth = countries[[i]]$WEALTH,  
                       ESCS_status = countries[[i]]$ESCS, #Index of economic, social and cultural status
                       teacher_support = countries[[i]]$TEACHSUP, #Teacher support in test language lessons
                       emo_sup = countries[[i]]$EMOSUPS, #Parents' emotional support perceived by student
                       math = countries[[i]]$PV3MATH, 
                       read = countries[[i]]$PV3READ,
                       scie = countries[[i]]$PV3SCIE)
  
  write.table(student, file=name_files[i])
  
}

#Check that all countries have enough values for each feature
ITA= read.table(file = "student_ita.txt", header = T)
AUT= read.table(file = "student_aut.txt", header = T)
BEL= read.table(file = "student_bel.txt", header = T)
DNK= read.table(file = "student_dnk.txt", header = T)
DEU= read.table(file = "student_deu.txt", header = T)
LUX= read.table(file = "student_lux.txt", header = T)
ESP= read.table(file = "student_esp.txt", header = T)
SWE= read.table(file = "student_swe.txt", header = T)
CHE= read.table(file = "student_che.txt", header = T)
GBR= read.table(file = "student_gbr.txt", header = T)

countries = list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)

#in order to see the number of NA
#apply(X = is.na(student), MARGIN = 2, FUN = sum)
library(visdat)
for (i in 1:10){
  #x11()
  #vis_miss(countries[[i]])
  print(name_countries[i])
  print(names(countries[[i]])[which(apply(X=is.na(countries[[i]]),MARGIN=2,FUN=sum)>0.8*dim(countries[[i]])[1])])
}

#Missing features before:
# [1] "ITA"
# character(0)
# [1] "AUT"
# [1] "morning_study"   "afternoon_study"
# [1] "BEL"
# [1] "immig_att"
# [1] "DNK"
# [1] "immig_att"
# [1] "DEU"
# [1] "morning_study"   "afternoon_study"
# [1] "LUX"
# [1] "morning_study"   "afternoon_study" "immig_att"      
# [1] "ESP"
# character(0)
# [1] "SWE"
# [1] "morning_study"   "afternoon_study" "immig_att"      
# [1] "CHE"
# [1] "morning_study"   "afternoon_study"
# [1] "GBR"
# [1] "immig_att"

#removed because too many NA: countries[[i]]$EMOSUPP,  countries[[i]]$DISCRIM, stratum, ITA$WB032Q01NA, ITA$WB031Q01NA, ITA$SWBP, ITA$WB154Q04HA, ITA$WB154Q05HA,
#ITA$WB154Q06HA, ITA$WB154Q07HA, ITA$WB154Q08HA, ITA$WB154Q09HA
#possibile to add: countries[[i]]$COBN_M, countries[[i]]$COBN_F, countries[[i]]$COBN_S


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


