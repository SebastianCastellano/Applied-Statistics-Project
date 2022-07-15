# LINEAR MODELS FOR EUROPEAN DATA

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

studentsDataEU <- read.table(file = "student_eur.txt", header = T)
studentsDataEU=na.omit(studentsDataEU)

studentsDataEU$immigration[which(studentsDataEU$immigration==1)] = 0;
studentsDataEU$immigration[which(studentsDataEU$immigration==2 + I(studentsDataEU$immigration==3))] = 1;
table(studentsDataEU$immigration)

studentsDataEU$country[which(studentsDataEU$country=='DEU' | studentsDataEU$country=='CHE' |studentsDataEU$country=='GBR')] = 'TOT'


attach(studentsDataEU)

# deu, che, gbr 
#-------------------------------------------------------------------------------

## Modello per MATH con tutte tutte le variabili (anche quelle riferite alla scuola)

gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:teach_behav + immigration:as.factor(country))
summary(gm2)

# Tolgo immigration:teach_behav  
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:gender + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:as.factor(country))
summary(gm2)

# Tolgo immigration:gender 
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav + 
            + immigration:as.factor(country))
summary(gm2)

# Tolgo immigration:as.factor(country)
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + immigration:learn_time_math + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo immigration:learn_time_math
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:belonging + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo immigration:belonging
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo belonging
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + as.factor(country) + immigration:language + immigration:hisced + 
            + immigration:grade_rep + immigration:fear_failure + immigration:bullied + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo  immigration:bullied
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + as.factor(country) + 
            + immigration:language + immigration:hisced + immigration:grade_rep + immigration:fear_failure + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo immigration:hisced
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + as.factor(country) + 
            + immigration:language + immigration:grade_rep + immigration:fear_failure + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo teacher_support
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + as.factor(country) + 
            + immigration:language + immigration:grade_rep + immigration:fear_failure + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo as.factor(country)
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + fear_failure + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav +
            + immigration:language + immigration:grade_rep + immigration:fear_failure + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

# Tolgo fear_failure
gm2 <- lm(math ~ gender + immigration + language + hisced + grade_rep + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + emo_sup + 
            + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav +
            + immigration:language + immigration:grade_rep + immigration:fear_failure + 
            + immigration:home_poss + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:stu_behav)
summary(gm2)

detach(studentsDataEU)





