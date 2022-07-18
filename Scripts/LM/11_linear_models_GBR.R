# LINEAR MODELS FOR GREAT BRITAIN (avendo tolto family_wealth, ...)

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

studentsDataGBR <- read.table(file = "student_gbr.txt", header = T)
studentsDataGBR=na.omit(studentsDataGBR)

studentsDataGBR$immigration[which(studentsDataGBR$immigration==1)] = 0;
studentsDataGBR$immigration[which(studentsDataGBR$immigration==2 + I(studentsDataGBR$immigration==3))] = 1;
table(studentsDataGBR$immigration)

attach(studentsDataGBR)

#-------------------------------------------------------------------------------

# Solo variabili studente 

gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo school_changes
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:hisced
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo language
gm4 <- lm(math ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration 
gm4 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:ESCS_status 
gm4 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo teacher_support
gm4 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + 
            + learn_time_math + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:language
gm4 <- lm(math ~ gender + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo grade_rep 
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:fear_failure 
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:belonging + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:belonging 
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:bullied + 
            + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:emo_sup
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + emo_sup + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:bullied + immigration:teacher_support +
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo emo_sup 
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + learn_time_math + immigration:gender +   
            + immigration:grade_rep + immigration:bullied + immigration:teacher_support +
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:gender 
gm4 <- lm(math ~ gender + hisced + fear_failure + belonging + bullied + 
            + ESCS_status + learn_time_math +   
            + immigration:grade_rep + immigration:bullied + immigration:teacher_support +
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo belonging
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + 
            + ESCS_status + learn_time_math +   
            + immigration:grade_rep + immigration:bullied + immigration:teacher_support +
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:bullied 
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + 
            + ESCS_status + learn_time_math +   
            + immigration:grade_rep + immigration:teacher_support +
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:teacher_support
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + 
            + ESCS_status + learn_time_math + immigration:grade_rep + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:learn_time_math
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + 
            + ESCS_status + learn_time_math + immigration:grade_rep + 
            + immigration:school_changes)
summary(gm4)

# Tolgo immigration:school_changes
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + 
            + ESCS_status + learn_time_math + immigration:grade_rep)
summary(gm4)

# Tolgo immigration:grade_rep
gm4 <- lm(math ~ gender + hisced + fear_failure + bullied + ESCS_status + learn_time_math)
summary(gm4)

x11()
par(mfrow=c(2,2))
plot(gm4)
# ok

shapiro.test(residuals(gm4))
# pvalue = 0.04053



#-------------------------------------------------------------------------------

# tutte le variabili (no country) DA RIFARE con variabili nuove 

gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)




x11()
par(mfrow=c(2,2))
plot(gm3)

shapiro.test(residuals(gm3))

#-------------------------------------------------------------------------------

detach(studentsDataGBR)





