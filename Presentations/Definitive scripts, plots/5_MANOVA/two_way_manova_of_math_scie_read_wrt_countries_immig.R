library(mvnormtest)
library(MASS)
load('~/GitHub/Applied-Statistics-Project/scripts/mcshapiro.test.RData')

#Import datasets
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
ITA <- read.table(file = "student_ita.txt", header = T)
AUT <- read.table(file = "student_aut.txt", header = T)
BEL <- read.table(file = "student_bel.txt", header = T)
DNK <- read.table(file = "student_dnk.txt", header = T)
DEU <- read.table(file = "student_deu.txt", header = T)
LUX <- read.table(file = "student_lux.txt", header = T)
ESP <- read.table(file = "student_esp.txt", header = T)
SWE <- read.table(file = "student_swe.txt", header = T)
CHE <- read.table(file = "student_che.txt", header = T)
GBR <- read.table(file = "student_gbr.txt", header = T)
EUR <- read.table(file = "student_eur.txt", header = T)

name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE")
n_countries <- length(name_countries) 

#a) Two-way Manova on math, science, reading scores. group1=immigration status, group2=country
X <- EUR
X.values <- data.frame(X$math, X$scie,  X$read)
names(X.values) <- c("math","scie","read")
X.groups1 <- factor(X$immigration, labels = c("native","2nd gen","1st gen"))
X.groups2 <- factor(X$country)
man <- manova(as.matrix(X.values) ~ X.groups1 + X.groups2 + X.groups1:X.groups2)
summary(man)  #all significant
summary.aov(man) 

#Check assumptions: Gaussianity and same covariance structure
#Gaussianity (residuals analysis)
man.res <- man$residuals
x11()
qqnorm(man.res)
qqline(man.res)
set.seed(30)
shapiro.test(sample(man.res,5000))$p #not super gaussian

#Covariance structure
interact.groups <- interaction(X.groups1,X.groups2)
bartlett.test(X.values,interact.groups.fact) # ok if pvalue big , here it is not
#Boxplot the groups (graphical qualitative analysis)
col <- c("red","green3","blue")
#Math
x11()
par(mfrow=c(2,1), mar=c(2,4,2,2))
boxplot(X.values[[1]]~X.groups1, main="Math", ylab = "Math", col=col)
par(mar=c(3,4,2,2))
boxplot(X.values[[1]]~X.groups2,ylab = "Math", las=3, col="gold")
#Scie
x11()
par(mfrow=c(2,1), mar=c(2,4,2,2))
boxplot(X.values[[2]]~X.groups1, main="Sience", ylab = "Science", col=col)
par(mar=c(3,4,2,2))
boxplot(X.values[[2]]~X.groups2,ylab = "Science", las=3, col="gold")
#Math
x11()
par(mfrow=c(2,1), mar=c(2,4,2,2))
boxplot(X.values[[3]]~X.groups1, main="Reading", ylab = "Reading", col=col)
par(mar=c(3,4,2,2))
boxplot(X.values[[3]]~X.groups2,ylab = "Reading", las=3, col="gold")
# The variances look pretty comparable

#b) Group means
math.mean <- data.frame(matrix(data = tapply(X.values[[1]],interact.groups,mean), nrow = 3, ncol = 10))
names(math.mean) <- levels(X.groups2)
row.names(math.mean) <- levels(X.groups1)
math.mean 
scie.mean <- data.frame(matrix(data = tapply(X.values[[2]],interact.groups,mean), nrow = 3, ncol = 10))
names(scie.mean) <- levels(X.groups2)
row.names(scie.mean) <- levels(X.groups1)
scie.mean 
read.mean <- data.frame(matrix(data = tapply(X.values[[3]],interact.groups,mean), nrow = 3, ncol = 10))
names(read.mean) <- levels(X.groups2)
row.names(read.mean) <- levels(X.groups1)
read.mean 

gap.math <- sort(math.mean[1,]-math.mean[2,])
gap.scie <- sort(scie.mean[1,]-scie.mean[2,])
gap.read <- sort(read.mean[1,]-read.mean[2,])
x11(width = 18, height = 15)
par(mfrow = c(3,1))
barplot(as.matrix(gap.math), ylab = "Native vs 2nd gen. imm.", main = "MATH", col = "red")
barplot(as.matrix(gap.scie), ylab = "Native vs  2nd gen. imm.", main = "SCIENCE", col = "green2")
barplot(as.matrix(gap.read), ylab = "Native vs  2nd gen. imm.", main = "READING", col = "blue")

