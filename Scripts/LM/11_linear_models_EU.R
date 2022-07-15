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
studentsData=na.omit(studentsDataEU)

studentsDataEU$immigration[which(studentsDataEU$immigration==1)] = 0;
studentsDataEU$immigration[which(studentsDataEU$immigration==2 + I(studentsDataEU$immigration==3))] = 1;
table(studentsDataEU$immigration)

attach(studentsDataEU)

