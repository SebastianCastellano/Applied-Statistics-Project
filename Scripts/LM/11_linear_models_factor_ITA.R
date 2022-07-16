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

## Italy 

studentsData= read.table(file = "student_ita.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)

attach(studentsData)
#-------------------------------------------------------------------------------
## Modello per MATH con tutte le variabili riferite allo studente (usando as.factor su language)
#-------------------------------------------------------------------------------

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

# Tolgo immigration:bullied 
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:fear_failure + immigration:belonging + 
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:belonging
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:fear_failure +  
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes)
summary(gm1)

# Tolgo immigration:teacher_support 
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:fear_failure +  
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:school_changes)
summary(gm1)

# Tolgo immigration:edu_resources
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:fear_failure +  
            + immigration:home_poss + immigration:ESCS_status + immigration:school_changes)
summary(gm1)

# Tolgo immigration:school_changes
gm1 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + immigration:fear_failure +  
            + immigration:home_poss + immigration:ESCS_status)
summary(gm1)

# R^2 = 0.2188

x11()
par(mfrow=c(2,2))
plot(gm1)
# ok

gm1$coefficients

#-------------------------------------------------------------------------------
## Modello per MATH con tutte tutte le variabili (anche quelle riferite alla scuola) 
## (usando as.factor su language)
#-------------------------------------------------------------------------------

gm2 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gm2)
x11()
par(mfrow=c(2,2))
plot(gm2)

# Tolgo immigration:teach_behav
gm2 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:stu_behav 
gm2 <- lm(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo as.factor(language) 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:as.factor(language) 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:family_wealth 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:cult_poss 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:emo_sup 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:school_changes + immigration:learn_time_math + immigration:class_size + 
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:learn_time_math
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:school_changes + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:grade_rep 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:school_changes + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:belonging 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:bullied + immigration:home_poss + immigration:edu_resources +
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo stud_teach_ratio
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:bullied + immigration:home_poss + immigration:edu_resources +
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo class_size 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:hisced + 
            + immigration:fear_failure + immigration:bullied + immigration:home_poss + immigration:edu_resources +
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:gender 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:hisced + 
            + immigration:fear_failure + immigration:bullied + immigration:home_poss + immigration:edu_resources +
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:edu_resources
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:hisced + 
            + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:hisced
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff + immigration:teach_multicult)
summary(gm2)

# Tolgo immigration:teach_multicult
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat + 
            + immigration:short_edu_staff)
summary(gm2)

# Tolgo immigration:short_edu_staff
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat)
summary(gm2)

# Tolgo fear_failure 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat)
summary(gm2)

# Tolgo emo_sup 
gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support +  
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat)
summary(gm2)

# R^2 = 0.3005

x11()
par(mfrow=c(2,2))
plot(gm2)
# ok

gm2$coefficients

# Procedo a togliere anche quelle poco significative? (ovvero quelle che hanno .)
# Togliendole vedo che l'R^2 si abbassa leggerissimamente 

#-------------------------------------------------------------------------------
## Modello per READ con tutte tutte le variabili (anche quelle riferite alla scuola) 
## (usando as.factor su language)
#-------------------------------------------------------------------------------

gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)
x11()
par(mfrow=c(2,2))
plot(gr2)
# ok

# Tolgo immigration:family_wealth 
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:grade_rep 
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:fear_failure
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:emo_sup  
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo teacher_support 
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo teach_multicult 
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:learn_time_read 
gr2 <- lm(read ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo as.factor(language)
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:teach_behav
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:as.factor(language) + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:as.factor(language) 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:hisced + 
            + immigration:belonging + immigration:bullied + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:gender 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat +
            + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:belonging 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:bullied + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:school_changes + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:school_changes 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:bullied + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:stu_behav 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:bullied + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + immigration:teacher_support + 
            + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:teacher_support 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:bullied + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + 
            + immigration:class_size + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:class_size 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:bullied + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + 
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:bullied 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + school_changes + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + 
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo cult_poss 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + edu_resources + family_wealth + ESCS_status + emo_sup + school_changes +
            + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:hisced + immigration:home_poss + 
            + immigration:cult_poss + immigration:edu_resources + immigration:ESCS_status + 
            + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:hisced 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + edu_resources + family_wealth + ESCS_status + emo_sup + school_changes +
            + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:home_poss + immigration:cult_poss + 
            + immigration:edu_resources + immigration:ESCS_status + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:home_poss 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + edu_resources + family_wealth + ESCS_status + emo_sup + school_changes +
            + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:cult_poss + 
            + immigration:edu_resources + immigration:ESCS_status + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# Tolgo immigration:ESCS_status 
gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + edu_resources + family_wealth + ESCS_status + emo_sup + school_changes +
            + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:cult_poss + immigration:edu_resources + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)

# R^2 = 0.3014

x11()
par(mfrow=c(2,2))
plot(gr2)
# ok

# Continuo a togliere le variabili poco significative (che hanno .)? Perchè
# l'R^2 si abbassarebbe leggermente 

gr2$coefficients

detach(studentsData)
#-------------------------------------------------------------------------------
## Proviamo a realizzare dei modelli separando gli immigrati e i nativi 

studentsDataNative <- studentsData[which(studentsData$immigration==0),]
studentsDataImmigrant <- studentsData[which(studentsData$immigration==1),]

#-------------------------------------------------------------------------------
##Native (usando as.factor su language)
#-------------------------------------------------------------------------------
attach(studentsDataNative)
#-------------------------------------------------------------------------------

gN2 <- lm(math ~ gender + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:as.factor(language) + 
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)
x11()
par(mfrow=c(2,2))
plot(gN2)
# ok

# Tolgo ESCS_status:as.factor(language)
gN2 <- lm(math ~ gender + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:bullied 
gN2 <- lm(math ~ gender + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:home_poss 
gN2 <- lm(math ~ gender + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:short_edu_staff 
gN2 <- lm(math ~ gender + as.factor(language) + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo as.factor(language) 
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo stud_teach_ratio 
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:edu_resources
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender +  
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:gender
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:class_size
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math +  
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:school_changes 
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:teacher_support
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo class_size 
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:cult_poss + ESCS_status:family_wealth + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:cult_poss
gN2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            +  ESCS_status:family_wealth + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo fear_failure 
gN2 <- lm(math ~ gender + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult +   
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            +  ESCS_status:family_wealth + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gN2)

# Tolgo ESCS_status:teach_multicult
gN2 <- lm(math ~ gender + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:hisced +   
            + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:family_wealth + ESCS_status:emo_sup + 
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav)
summary(gN2)

# Tolgo ESCS_status:short_edu_mat 
gN2 <- lm(math ~ gender + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:hisced + ESCS_status:grade_rep +   
            + ESCS_status:fear_failure + ESCS_status:belonging + ESCS_status:family_wealth + ESCS_status:emo_sup +
            + ESCS_status:learn_time_math + ESCS_status:stud_teach_ratio + ESCS_status:stu_behav + ESCS_status:teach_behav) 
summary(gN2)

# R^2 = 0.2938

gN2$coefficients

# Continuo a togliere le variabili poco significative (che hanno .)? Perchè
# l'R^2 si abbassarebbe leggermente 

#-------------------------------------------------------------------------------
detach(studentsDataNative)
#-------------------------------------------------------------------------------
# Immigrant (usando as.factor su language)  -> non soddisfacente 
#-------------------------------------------------------------------------------
attach(studentsDataImmigrant)
#-------------------------------------------------------------------------------

gI2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:language + 
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + ESCS_status:short_edu_staff +
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gI2)
x11()
par(mfrow=c(2,2))
plot(gI2)
# ok

# Tolgo ESCS_status:short_edu_staff
gI2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:language + 
            + ESCS_status:hisced + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gI2)

# Tolgo ESCS_status:hisced 
gI2 <- lm(math ~ gender + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:language + 
            + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gI2)

# Tolgo language 
gI2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + ESCS_status:language + 
            + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gI2)

# Tolgo ESCS_status:language 
gI2 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + school_changes + learn_time_math + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + teach_multicult + ESCS_status:gender + 
            + ESCS_status:grade_rep + ESCS_status:fear_failure + ESCS_status:belonging +
            + ESCS_status:bullied + ESCS_status:home_poss + ESCS_status:cult_poss + ESCS_status:edu_resources +
            + ESCS_status:family_wealth + ESCS_status:teacher_support + ESCS_status:emo_sup + 
            + ESCS_status:school_changes + ESCS_status:learn_time_math + ESCS_status:class_size + 
            + ESCS_status:stud_teach_ratio + ESCS_status:short_edu_mat + 
            + ESCS_status:stu_behav + ESCS_status:teach_behav + ESCS_status:teach_multicult)
summary(gI2)

# Sarebbe da togliere ESCS_status, ma allora poi che senso avrebbe tutto?

gI2$coefficients

#-------------------------------------------------------------------------------
detach(studentsDataImmigrant)
#-------------------------------------------------------------------------------

