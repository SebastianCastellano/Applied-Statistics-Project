school_neg_dnk <- data.frame(studentsData_neg_schools_dnk$fear_failure,
                         studentsData_neg_schools_dnk$belonging,
                         studentsData_neg_schools_dnk$bullied,
                         studentsData_neg_schools_dnk$teacher_support,
                         studentsData_neg_schools_dnk$emo_sup,
                         studentsData_neg_schools_dnk$class_size,
                         studentsData_neg_schools_dnk$stud_teach_ratio,
                         studentsData_neg_schools_dnk$short_edu_mat,
                         studentsData_neg_schools_dnk$short_edu_staff,
                         studentsData_neg_schools_dnk$stu_behav,
                         studentsData_neg_schools_dnk$teach_behav,
                         studentsData_neg_schools_dnk$ESCS_status,
                         studentsData_neg_schools_dnk$immigration,
                         group = 0 )

school_pos_dnk <- data.frame(studentsData_pos_schools_dnk$fear_failure,
                         studentsData_pos_schools_dnk$belonging,
                         studentsData_pos_schools_dnk$bullied,
                         studentsData_pos_schools_dnk$teacher_support,
                         studentsData_pos_schools_dnk$emo_sup,
                         studentsData_pos_schools_dnk$class_size,
                         studentsData_pos_schools_dnk$stud_teach_ratio,
                         studentsData_pos_schools_dnk$short_edu_mat,
                         studentsData_pos_schools_dnk$short_edu_staff,
                         studentsData_pos_schools_dnk$stu_behav,
                         studentsData_pos_schools_dnk$teach_behav,
                         studentsData_pos_schools_dnk$ESCS_status,
                         studentsData_pos_schools_dnk$immigration,
                         group = 1)

school_neg_dnk <- data.frame(studentsData_neg_schools_dnk$fear_failure,
                             studentsData_neg_schools_dnk$belonging,
                             studentsData_neg_schools_dnk$bullied,
                             studentsData_neg_schools_dnk$teacher_support,
                             studentsData_neg_schools_dnk$emo_sup,
                             studentsData_neg_schools_dnk$class_size,
                             studentsData_neg_schools_dnk$stud_teach_ratio,
                             studentsData_neg_schools_dnk$short_edu_mat,
                             studentsData_neg_schools_dnk$short_edu_staff,
                             studentsData_neg_schools_dnk$stu_behav,
                             studentsData_neg_schools_dnk$teach_behav,
                             studentsData_neg_schools_dnk$ESCS_status,
                             studentsData_neg_schools_dnk$immigration,
                             group = 0 )

school_pos_gbr <- data.frame(studentsData_pos_schools_gbr$fear_failure,
                             studentsData_pos_schools_gbr$belonging,
                             studentsData_pos_schools_gbr$bullied,
                             studentsData_pos_schools_gbr$teacher_support,
                             studentsData_pos_schools_gbr$emo_sup,
                             studentsData_pos_schools_gbr$class_size,
                             studentsData_pos_schools_gbr$stud_teach_ratio,
                             studentsData_pos_schools_gbr$short_edu_mat,
                             studentsData_pos_schools_gbr$short_edu_staff,
                             studentsData_pos_schools_gbr$stu_behav,
                             studentsData_pos_schools_gbr$teach_behav,
                             studentsData_pos_schools_gbr$ESCS_status,
                             studentsData_pos_schools_gbr$immigration,
                             group = 1)

school_neg_gbr <- data.frame(studentsData_neg_schools_gbr$fear_failure,
                             studentsData_neg_schools_gbr$belonging,
                             studentsData_neg_schools_gbr$bullied,
                             studentsData_neg_schools_gbr$teacher_support,
                             studentsData_neg_schools_gbr$emo_sup,
                             studentsData_neg_schools_gbr$class_size,
                             studentsData_neg_schools_gbr$stud_teach_ratio,
                             studentsData_neg_schools_gbr$short_edu_mat,
                             studentsData_neg_schools_gbr$short_edu_staff,
                             studentsData_neg_schools_gbr$stu_behav,
                             studentsData_neg_schools_gbr$teach_behav,
                             studentsData_neg_schools_gbr$ESCS_status,
                             studentsData_neg_schools_gbr$immigration,
                             group = 0 )

table(studentsData_neg_schools_gbr$immigration)
table(studentsData_pos_schools_gbr$immigration)
table(studentsData_neg_schools_dnk$immigration)
table(studentsData_pos_schools_dnk$immigration)


X <- as.data.frame(rbind(as.matrix(school_pos_gbr),as.matrix(school_neg_dnk)))
X.values <- X[-14]
X.groups<- factor(X$group, labels=c("neg","pos"))

group1 <- X.values[which(X.groups=="pos"),]
group2 <- X.values[which(X.groups=="neg"),]
g <- 2
p <- dim(group1)[2]
n1 <- dim(group1)[1]
n2 <- dim(group2)[1]
n <- n1+n2
k <- p*g*(g-1)/2
man <- manova(as.matrix(X.values) ~ X.groups)
SSres <- summary.manova(man)$SS$Residuals
Spooled <- SSres/(n-g)
mean.g1 <- sapply(group1,mean)
mean.g2 <- sapply(group2,mean)
alpha <- 0.01
qT <- qt(1-alpha/2/k,n-g)
conf.int.B <- cbind(inf=mean.g1-mean.g2 - qT*sqrt(diag(Spooled)*(1/n1+1/n2)),mean.dif=mean.g1-mean.g2, sup=mean.g1-mean.g2 + qT*sqrt(diag(Spooled)*(1/n1+1/n2)  ))
conf.int.B  
