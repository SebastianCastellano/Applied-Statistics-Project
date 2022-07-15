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
library(corrplot)

## Italy 

studentsData= read.table(file = "student_ita.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)

attach(studentsData)


#-------------------------------------------------------------------------------

## Modello per MATH con tutte le variabili riferite allo studente

gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math)
summary(gm1)

# Tolgo immigration:learn_time_math
gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes)
summary(gm1)

# Tolgo immigration:grade_rep 
gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes)
summary(gm1)

# Tolgo immigration:cult_poss 
gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes)
summary(gm1)

# Tolgo fear_failure
gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes)
summary(gm1)

# Tolgo immigration:emo_sup 
gm1 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo as.factor(language)
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:as.factor(language) 
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:family_wealth 
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:hisced 
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:gender + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:gender
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)


# continuareeeee


#-------------------------------------------------------------------------------

## Modello per READ con tutte tutte le variabili (anche quelle riferite alla scuola)

gr2 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)
x11()
par(mfrow=c(2,2))
plot(gr2)

# Tolgo alcune delle interaazioni di immigration con le altre variabili:
# immigration:gender + immigration:language + immigration:hisced + immigration:grade_rep + 
# + immigration:fear_failure + immigration:belonging + immigration:bullied + immigration:home_poss + 
# + immigration:cult_poss + immigration:family_wealth + immigration:teacher_support + 
# immigration:emo_sup + immigration:school_changes + immigration:learn_time_read + immigration:class_size +
# + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav 
gr2 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:edu_resources + immigration:ESCS_status +
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:teach_multicult)
summary(gr2)
x11()
par(mfrow=c(2,2))
plot(gr2)

# Tolgo alcune variabili che sono molto poco significative: teacher_support + teach_multicult 
gr2 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:edu_resources + immigration:ESCS_status +
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:teach_multicult)
summary(gr2)
x11()
par(mfrow=c(2,2))
plot(gr2)

# Riduco ulteriormente togliendo: belonging + stud_teach_ratio + immigration:ESCS_status + 
# + immigration:short_edu_mat + immigration:teach_multicult
gr2 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:edu_resources + immigration:stud_teach_ratio)
summary(gr2)
x11()
par(mfrow=c(2,2))
plot(gr2)

detach(studentsData)







#-------------------------------------------------------------------------------

# Proviamo a realizzare dei modelli separando gli immigrati e i nativi 

studentsDataNative <- studentsData[which(studentsData$immigration==0),]
studentsDataImmigrant <- studentsData[which(studentsData$immigration==1),]

#-------------------------------------------------------------------------------

# Native 

attach(studentsDataNative)

gn2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:language + 
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gn2)
x11()
par(mfrow=c(2,2))
plot(gn2)

# Tolgo: ESCS_status:gender + ESCS_status:language + ESCS_status:fear_failure + ESCS_status:bullied +
# + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources + ESCS_status:family_wealth + 
# + ESCS_status:teacher_support + ESCS_status:school_changes + ESCS_status:class_size 
# + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff + ESCS_status:teach_behav + ESCS_status:teach_multicult
gn2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:hisced + ESCS_status:grade_rep + 
            + ESCS_status:belonging + ESCS_status:emo_sup + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio +
            + ESCS_status:stu_behav)
summary(gn2)
x11()
par(mfrow=c(2,2))
plot(gn2)

# Tolgo: fear_failure + class_size + stud_teach_ratio + emo_sup
gn2 <- lm(math ~ gender + language + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:hisced + ESCS_status:grade_rep + 
            + ESCS_status:belonging + ESCS_status:emo_sup + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio +
            + ESCS_status:stu_behav)
summary(gn2)
x11()
par(mfrow=c(2,2))
plot(gn2)

detach(studentsDataNative)

#-------------------------------------------------------------------------------

# Immigrant 

attach(studentsDataImmigrant)

gi2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + grade_rep:gender + grade_rep:language + 
            + grade_rep:hisced + ESCS_status:grade_rep + grade_rep:fear_failure + grade_rep:belonging +
            + grade_rep:bullied + grade_rep:home_poss + grade_rep:cult_poss + grade_rep:edu_resources +
            + grade_rep:family_wealth + grade_rep:teacher_support + grade_rep:emo_sup + 
            + grade_rep:school_changes + grade_rep:learn_time_math + grade_rep:class_size + 
            + grade_rep:stud_teach_ratio + grade_rep:short_edu_mat + grade_rep:short_edu_staff +
            + grade_rep:stu_behav + grade_rep:teach_behav + grade_rep:teach_multicult)
summary(gi2)
x11()
par(mfrow=c(2,2))
plot(gi2)





