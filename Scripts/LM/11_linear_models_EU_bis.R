# LINEAR MODELS FOR EUROPEAN DATA (avendo tolto family_wealth, ...)

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

studentsDataEU <- read.table(file = "student_eur.txt", header = T)
studentsDataEU=na.omit(studentsDataEU)

studentsDataEU$immigration[which(studentsDataEU$immigration==1)] = 0;
studentsDataEU$immigration[which(studentsDataEU$immigration==2 + I(studentsDataEU$immigration==3))] = 1;
table(studentsDataEU$immigration)

attach(studentsDataEU)

#-------------------------------------------------------------------------------
## Modello per MATH con tutte tutte le variabili 
#-------------------------------------------------------------------------------

# variabili studente 

gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:gender
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:bullied 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:learn_time_math
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:belonging 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo fear_failure 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:ESCS_status
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:teacher_support + immigration:emo_sup)
summary(gm4)


# Tolgo immigration:teacher_support
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:hisced 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:emo_sup)
summary(gm4)

#-------------------------------------------------------------------------------

# tutte le variabili (no country)

gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:gender 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:ESCS_status 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:teach_behav
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm3)

# Tolgo immigration:bullied 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm3)

# Tolgo immigration:learn_time_math 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm3)

# Tolgo fear_failure
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm3)

# Tolgo immigration:belonging 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm3)

#-------------------------------------------------------------------------------
## Modello per READ con tutte tutte le variabili 
#-------------------------------------------------------------------------------

# variabili studente 

gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + 
            + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_read)
summary(gm4)

# Tolgo immigration:learn_time_read
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + 
            + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:language
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + 
            + immigration:gender + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:hisced 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + immigration:gender + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:ESCS_status
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + immigration:gender + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:gender 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:bullied
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging +  
            + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

#-------------------------------------------------------------------------------

# READ - tutte le variabili 

gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_read + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:ESCS_status 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_read + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:hisced 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_read + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo short_edu_mat 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_read + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:learn_time_read
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:gender 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:short_edu_staff 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:teach_behav
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:stu_behav)
summary(gm3)

# Tolgo immigration:language 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:bullied + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:stu_behav)
summary(gm3)

# Tolgo immigration:bullied
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + learn_time_read + short_edu_staff +
            + stu_behav + teach_behav + immigration:grade_rep + immigration:fear_failure + immigration:belonging + 
            + immigration:teacher_support + immigration:emo_sup +  
            + immigration:short_edu_mat + immigration:stu_behav)
summary(gm3)

# R^2 = 0.219

x11()
par(mfrow=c(2,2))
plot(gm3)
# ok

#-------------------------------------------------------------------------------

detach(studentsDataEU)
