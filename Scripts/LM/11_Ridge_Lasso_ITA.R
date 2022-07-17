# RIDGE REGRESSION for Italy 

library(MASS)
library(car)
library(rgl)

library(glmnet)

## Italy 

studentsData= read.table(file = "student_ita.txt", header = T)
studentsData=na.omit(studentsData)

studentsData$immigration[which(studentsData$immigration==1)] = 0;
studentsData$immigration[which(studentsData$immigration==2 + I(studentsData$immigration==3))] = 1;
table(studentsData$immigration)

attach(studentsData)

#-------------------------------------------------------------------------------

# Dallo script '11_linear_models_factor_ITA' abbiamo ricavato i seguenti modelli:

gm2 <- lm(math ~ gender + immigration + hisced + grade_rep + belonging + bullied + 
            + home_poss + cult_poss + edu_resources + family_wealth + ESCS_status + teacher_support +  
            + school_changes + learn_time_math + short_edu_mat + short_edu_staff + stu_behav + teach_behav + 
            + teach_multicult + immigration:fear_failure + immigration:bullied + immigration:home_poss + 
            + immigration:ESCS_status + immigration:teacher_support + immigration:school_changes + 
            + immigration:class_size + immigration:stud_teach_ratio + immigration:short_edu_mat)
summary(gm2)
# R^2 = 0.3005

gr2 <- lm(read ~ gender + immigration + hisced + grade_rep + fear_failure + belonging + bullied + 
            + home_poss + edu_resources + family_wealth + ESCS_status + emo_sup + school_changes +
            + learn_time_read + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff +
            + stu_behav + teach_behav + immigration:cult_poss + immigration:edu_resources + immigration:stud_teach_ratio + 
            + immigration:short_edu_mat + immigration:short_edu_staff + immigration:teach_multicult)
summary(gr2)
# R^2 = 0.3014

# Proviamo invece a utilizzare la Ridge Regression per vedere se troviamo un modello migliore 

#-------------------------------------------------------------------------------
## Ridge Regression
#-------------------------------------------------------------------------------

lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + 
                        + belonging + bullied + home_poss + cult_poss + edu_resources + family_wealth + 
                        + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
                        + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff + stu_behav + 
                        + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + 
                        + immigration:hisced + immigration:grade_rep + immigration:fear_failure + 
                        + immigration:belonging + immigration:bullied + immigration:home_poss + 
                        + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
                        + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
                        + immigration:school_changes + immigration:learn_time_math + immigration:class_size + 
                        + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + 
                        + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult, lambda=lambda.c)
summary(fit.ridge)

lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]     # or select(fit.ridge)
lambda.opt

#-------------------------------------------------------------------------------

# Il problema è che in questo modo in realtà non sto selezionando nessun modello, non sto facendo una selezione 
# delle variabili significative 
# Se il nostro obiettivo è ridurre il modello magari è meglio utilizzare la Lasso Regression 

#-------------------------------------------------------------------------------
## Lasso Regression
#-------------------------------------------------------------------------------

x<- model.matrix(math ~ gender + immigration + as.factor(language) + hisced + grade_rep + fear_failure + 
                        + belonging + bullied + home_poss + cult_poss + edu_resources + family_wealth + 
                        + ESCS_status + teacher_support + emo_sup + school_changes + learn_time_math + 
                        + class_size + stud_teach_ratio + short_edu_mat + short_edu_staff + stu_behav + 
                        + teach_behav + teach_multicult + immigration:gender + immigration:as.factor(language) + 
                        + immigration:hisced + immigration:grade_rep + immigration:fear_failure + 
                        + immigration:belonging + immigration:bullied + immigration:home_poss + 
                        + immigration:cult_poss + immigration:edu_resources + immigration:family_wealth + 
                        + immigration:ESCS_status + immigration:teacher_support + immigration:emo_sup + 
                        + immigration:school_changes + immigration:learn_time_math + immigration:class_size + 
                        + immigration:stud_teach_ratio + immigration:short_edu_mat + immigration:short_edu_staff + 
                        + immigration:stu_behav + immigration:teach_behav + immigration:teach_multicult)[,-21]
y <- math 
lambda.grid <- seq(0,10,0.01)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 

x11()
plot(fit.lasso,xvar='lambda',label=TRUE)

# Per ottenere la lambda ottimale:
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) 
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

x11()
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)
# 1st vertical line (black) : minimum cross validation error 
# 2nd vertical line (---) : biggest lambda, between the one we tested, that is such that the error poles within one 
#                           standard deviation of the error of the minimum of the lambda 

# Quindi il modello è:
fit.lasso.opt <- glmnet(x,y, lambda = bestlam.lasso)  
coef(fit.lasso.opt)

fit.lasso.opt$dev.ratio
# 0.3067

#-------------------------------------------------------------------------------

detach(studentsData)



