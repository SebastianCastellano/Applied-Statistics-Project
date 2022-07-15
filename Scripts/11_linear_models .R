## LINEAR MODELS 

library(MASS)
library(car)
library(rgl)

library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
install.packages("corrplot")
library(corrplot)

## Italy 

studentsData= read.table(file = "student_ita.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)


# ------------------------------------------------------------------------------

## Models  

attach(studentsData)

fm1 <- lm(math ~ immigration + language + hisced + ESCS_status)
summary(fm1)
# R^2 molto basso
# language non è significativa, la tolgo 
fm1b <- lm(math ~ immigration + hisced + ESCS_status)
summary(fm1b)
# R^2 rimane molto basso

fm1_int <- lm(math ~ immigration + language + hisced + ESCS_status + immigration:ESCS_status)
summary(fm1_int)

fm1 <- lm(math ~ immigration + ESCS_status + family_wealth + cult_poss + home_poss)
summary(fm1)

fm1_int <- lm(math ~ immigration + ESCS_status + family_wealth + cult_poss + home_poss + immigration:ESCS_status)
summary(fm1_int)

fm1 <- lm(math ~ immigration + ESCS_status + competitiveness)
summary(fm1)

fm2 <- lm(scie ~ immigration + language + hisced + ESCS_status)
summary(fm2)
# R^2 molto basso
# language non è significativa, la tolgo 
fm2b <- lm(scie ~ immigration + hisced + ESCS_status)
summary(fm2b)
# R^2 cambia poco ma rimane molto basso

fm3 <- lm(read ~ immigration + language + hisced + ESCS_status)
summary(fm3)
# R^2 molto basso
# immStatus non è molto significativa, provo a toglierla 
fm3b <- lm(read ~ language + hisced + ESCS_status)
summary(fm3b)
# R^2 cambia molto poco e rimane molto basso

fm4 <- lm(escs ~ immigration+ language + hisced)
summary(fm4)
# Tutti molto significativi, da verificare se sono correlati (se così fosse 
# sarebbe spiegato il fatto che siano tutt molto significativi)

fm5 <- lm(math ~ scie + read)
summary(fm5)
# R^2 buono, non eccezionale 
# tutti significativi 

fm <- lm(math ~ teacher_support)
summary(fm)

detach(studentsData)


# ------------------------------------------------------------------------------

## Mixed Models 

fm16.1mer <- lmer(math ~ ESCS_status + (1|immigration), data = studentsData)
summary(fm16.1mer)


# ------------------------------------------------------------------------------

## Modelli divisi per native e immigrant 

studentsDataNative <- studentsData[which(studentsData$immigration==0),]
studentsDataImmigrant <- studentsData[which(studentsData$immigration==1),]

# Native

mod <- lm(studentsDataNative$math ~ as.factor(studentsDataNative$language) + studentsDataNative$ESCS_status + studentsDataNative$family_wealth + studentsDataNative$teacher_support)
summary(mod)

mod <- lm(studentsDataNative$read ~ as.factor(studentsDataNative$language) + studentsDataNative$ESCS_status + studentsDataNative$family_wealth + studentsDataNative$teacher_support)
summary(mod)

x11()
par(mfrow=c(2,2))
plot(mod)

#Immigrant 

mod <- lm(studentsDataImmigrant$math ~ as.factor(studentsDataImmigrant$language) + studentsDataImmigrant$ESCS_status + studentsDataImmigrant$family_wealth + studentsDataImmigrant$teacher_support)
summary(mod)

mod <- lm(studentsDataImmigrant$read ~ as.factor(studentsDataImmigrant$language) + studentsDataImmigrant$ESCS_status + studentsDataImmigrant$family_wealth + studentsDataImmigrant$teacher_support)
summary(mod)

mod <- lm(studentsDataImmigrant$math ~ as.factor(studentsDataImmigrant$language) + studentsDataImmigrant$ESCS_status + studentsDataImmigrant$bullied + studentsDataImmigrant$teacher_support)
summary(mod)

mod <- lm(studentsDataImmigrant$math ~ as.factor(studentsDataImmigrant$language) + studentsDataImmigrant$ESCS_status + studentsDataImmigrant$bullied + studentsDataImmigrant$pisa_difficulty)
summary(mod)




corrplot(cor(studentsData))




## Modello completo (con tutte le variabili)
attach(studentsData)
model <- lm(math ~ gender + immigration + as.factor(language) + as.factor(hisced) + grade_rep + joy_read + pisa_difficulty + competitiveness + fear_failure + resilience + belonging + bullied + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup)
summary(model)
# R^2 = 0.2652
# tolgo fear_failure, resilience, edu_resources, emo_sup
model_bis <- lm(math ~ gender + immigration + as.factor(language) + as.factor(hisced) + grade_rep + joy_read + pisa_difficulty + competitiveness + belonging + bullied + home_poss + cult_poss + family_wealth + ESCS_status + teacher_support)
summary(model_bis)
# R^2 = 0.2646
# come posso togliere solo as.factor(language)3 e non anche as.factor(language)1 e as.factor(language)2
# (stessa cosa per hisced)
detach(studentsData)

attach(studentsDataNative)
# non metto immigration 
model <- lm(math ~ gender + as.factor(language) + as.factor(hisced) + grade_rep + joy_read + pisa_difficulty + competitiveness + fear_failure + resilience + belonging + bullied + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup)
summary(model)
# R^2 = 0.254
# tolgo hisced, fear_failure, resilience, edu_resources, emo_sup
# come posso togliere solo as.factor(language)3 e non anche as.factor(language)1 e as.factor(language)2??
model_bis <- lm(math ~ gender + as.factor(language) + grade_rep + joy_read + pisa_difficulty + competitiveness + belonging + bullied + home_poss + cult_poss + family_wealth + ESCS_status + teacher_support)
summary(model_bis)
# R^2 = 0.2375 
detach(studentsDataNative)

attach(studentsDataImmigrant)
# non metto immigration 
model <- lm(math ~ gender + as.factor(language) + as.factor(hisced) + grade_rep + joy_read + pisa_difficulty + competitiveness + fear_failure + resilience + belonging + bullied + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup)
summary(model)
# R^2 = 0.3097
# tolgo language, joy_read, fear_failure, resilience, cult_poss, edu_resources, ESCS_status, teacher_support, emo_sup
# come posso tenere solo as.factor(hisced)5 e togliere gli altri hisced?
model_bis <- lm(math ~ gender + grade_rep + pisa_difficulty + competitiveness + belonging + bullied + home_poss + family_wealth)
summary(model_bis)
# R^2 = 0.2824
detach(studentsDataImmigrant)




