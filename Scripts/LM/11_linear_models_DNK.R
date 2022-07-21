# LINEAR MODELS FOR DENMARK (avendo tolto family_wealth, ...)

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

studentsDataDNK <- read.table(file = "student_dnk.txt", header = T)
studentsDataDNK=na.omit(studentsDataDNK)

studentsDataDNK$immigration[which(studentsDataDNK$immigration==1)] = 0;
studentsDataDNK$immigration[which(studentsDataDNK$immigration==2 + I(studentsDataDNK$immigration==3))] = 1;
table(studentsDataDNK$immigration)

attach(studentsDataDNK)

#-------------------------------------------------------------------------------
# MATH - Solo variabili studente 
#-------------------------------------------------------------------------------

gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:teacher_support 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:belonging 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:gender 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_math)
summary(gm4)

# Tolgo immigration:learn_time_math
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:school_changes)
summary(gm4)

# Tolgo immigration:fear_failure 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_math + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:school_changes)
summary(gm4)

# Tolgo emo_sup 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:hisced + immigration:grade_rep + immigration:bullied + immigration:ESCS_status + 
            + immigration:emo_sup + immigration:school_changes)
summary(gm4)

# Tolgo immigration:school_changes
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:hisced + immigration:grade_rep + immigration:bullied + immigration:ESCS_status + 
            + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:hisced
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status + 
            + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:emo_sup
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status)
summary(gm4)

# Tolgo bullied
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status)
summary(gm4)

# Tolgo immigration:grade_rep 
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:bullied + immigration:ESCS_status)
summary(gm4)

# Tolgo immigration:bullied
gm4 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + immigration:language + 
            + immigration:ESCS_status)
summary(gm4)

x11()
par(mfrow=c(2,2))
plot(gm4)
# ok
shapiro.test(residuals(gm4))
# pvalue = 0.00115

#-------------------------------------------------------------------------------
# MATH - Tutte le variabili
#-------------------------------------------------------------------------------

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

# Tolgo immigration:teacher_support
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:belonging
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:stu_behav
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:fear_failure
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo short_edu_staff
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo emo_sup 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:short_edu_staff 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:short_edu_mat 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:gender
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:school_changes
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:learn_time_math + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:class_size
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:learn_time_math + immigration:stud_teach_ratio +
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:learn_time_math 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo short_edu_mat
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:hisced + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:teach_behav
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:hisced + immigration:grade_rep + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo immigration:hisced 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo immigration:emo_sup
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status +  
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo bullied
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + class_size + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status +  
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo class_size 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + stud_teach_ratio + stu_behav + teach_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status +  
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo teach_behav 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + stud_teach_ratio + stu_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status +  
            + immigration:stud_teach_ratio)
summary(gm3)

# Tolgo fear_failure 
gm3 <- lm(math ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + school_changes + learn_time_math + 
            + stud_teach_ratio + stu_behav + immigration:language + 
            + immigration:grade_rep + immigration:bullied + immigration:ESCS_status +  
            + immigration:stud_teach_ratio)
summary(gm3)

x11()
par(mfrow=c(2,2))
plot(gm3)
# ok

shapiro.test(residuals(gm3))
# pvalue = 0.0004476

#-------------------------------------------------------------------------------
# READ - Solo variabili studente 
#-------------------------------------------------------------------------------

gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_read)
summary(gm4)

# Tolgo immigration:hisced 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_read)
summary(gm4)

# Tolgo bullied 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes + immigration:learn_time_read)
summary(gm4)

# Tolgo immigration:learn_time_read
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:school_changes)
summary(gm4)

# Tolgo immigration:school_changes
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:gender + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:gender 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes +
            + learn_time_read + immigration:language + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:language
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:teacher_support
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:belonging 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup)
summary(gm4)

# Tolgo fear_failure
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:grade_rep
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:fear_failure + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:fear_failure 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:bullied + immigration:ESCS_status + immigration:emo_sup)
summary(gm4)

# Tolgo immigration:emo_sup
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:bullied + immigration:ESCS_status)
summary(gm4)

# Tolgo belonging 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:bullied + immigration:ESCS_status)
summary(gm4)

# Tolgo immigration:bullied 
gm4 <- lm(read ~ gender + immigration + language + hisced + grade_rep +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + immigration:ESCS_status)
summary(gm4)

#-------------------------------------------------------------------------------
# READ - Tutte le variabili 
#-------------------------------------------------------------------------------

gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:learn_time_read + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:learn_time_read 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo short_edu_staff
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:stu_behav 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:short_edu_mat
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo bullied
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:hisced
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stud_teach_ratio + short_edu_mat + 
            + stu_behav + teach_behav + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:school_changes +
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo stud_teach_ratio 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + short_edu_mat + stu_behav + teach_behav + immigration:gender + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:gender 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + short_edu_mat + stu_behav + teach_behav + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio +
            + immigration:short_edu_staff + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:short_edu_staff 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + short_edu_mat + stu_behav + teach_behav + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo short_edu_mat 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stu_behav + teach_behav + immigration:language +  
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:language
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stu_behav + teach_behav +   
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:teacher_support 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + class_size + stu_behav + teach_behav +   
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo class_size 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:grade_rep + immigration:fear_failure + 
            + immigration:belonging + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo fear_failure 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:grade_rep + immigration:fear_failure + 
            + immigration:belonging + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:fear_failure 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:grade_rep + 
            + immigration:belonging + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:class_size 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:grade_rep + 
            + immigration:belonging + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:belonging
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:grade_rep + 
            + immigration:bullied + immigration:ESCS_status + immigration:emo_sup + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:grade_rep 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + belonging +  
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:bullied + immigration:ESCS_status + 
            + immigration:emo_sup + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo belonging 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:bullied + immigration:ESCS_status + 
            + immigration:emo_sup + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:emo_sup 
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:bullied + immigration:ESCS_status + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

# Tolgo immigration:bullied
gm3 <- lm(read ~ gender + immigration + language + hisced + grade_rep + 
            + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_read + 
            + stu_behav + teach_behav + immigration:ESCS_status + 
            + immigration:stud_teach_ratio + immigration:teach_behav)
summary(gm3)

x11()
par(mfrow=c(2,2))
plot(gm3)
# ok

shapiro.test(residuals(gm3))
# pvalue = 0.00453

#-------------------------------------------------------------------------------

detach(studentsDataDNK)






