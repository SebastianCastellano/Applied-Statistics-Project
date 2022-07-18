school_neg <- data.frame(studentsData_neg_schools$fear_failure,
                         studentsData_neg_schools$belonging,
                         studentsData_neg_schools$bullied,
                         studentsData_neg_schools$teacher_support,
                         studentsData_neg_schools$emo_sup,
                         studentsData_neg_schools$class_size,
                         studentsData_neg_schools$stud_teach_ratio,
                         studentsData_neg_schools$short_edu_mat,
                         studentsData_neg_schools$short_edu_staff,
                         studentsData_neg_schools$stu_behav,
                         studentsData_neg_schools$teach_behav,
                         group = "neg" )

school_pos <- data.frame(studentsData_pos_schools$fear_failure,
                         studentsData_pos_schools$belonging,
                         studentsData_pos_schools$bullied,
                         studentsData_pos_schools$teacher_support,
                         studentsData_pos_schools$emo_sup,
                         studentsData_pos_schools$class_size,
                         studentsData_pos_schools$stud_teach_ratio,
                         studentsData_pos_schools$short_edu_mat,
                         studentsData_pos_schools$short_edu_staff,
                         studentsData_pos_schools$stu_behav,
                         studentsData_pos_schools$teach_behav,
                         group = "pos")
X <- as.data.frame(rbind(as.matrix(school_pos),as.matrix(school_neg)))
X.values <- X[-12]
X.groups<- as.factor(X$group)

group1 <- school_pos
group2 <- school_neg
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
