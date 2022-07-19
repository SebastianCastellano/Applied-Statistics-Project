# Analyze if the differences captured by being immig is just due to poorer conditions
# Compare poor native and immig and see if the diff is still there.

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

# Oserve immig and escs
boxplot(EUR$ESCS_status ~ EUR$immigration) #immig have lower escs
summary(EUR$ESCS_status)

#Create a grouping of ESCS based on quartiles
ESCs.groups <- rep(3,length(EUR$ESCS_status))
ESCs.groups[which(EUR$ESCS_status<0.8707)] <- 2
ESCs.groups[which(EUR$ESCS_status<0.1933)] <- 1
ESCs.groups[which(EUR$ESCS_status<I(-0.5399))] <- 0
ESCs.groups <- as.factor(ESCs.groups )

boxplot(EUR$ESCS_status ~ ESCs.groups)
tapply(EUR$ESCS_status,ESCs.groups,length)

# Perform two way anova
#a) Two-way Manova on math, reading scores. group1=immigration status, group2=country
X <- EUR
X.values <- data.frame(X$math, X$read)
names(X.values) <- c("math","read")
X$immigration[which(X$immigration==3)] <-2
X.groups1 <- factor(X$immigration, labels = c("native","immig"))
X.groups2 <- ESCs.groups
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
col <- c("red","blue")
#Math
x11()
par(mfrow=c(2,1), mar=c(2,4,2,2))
boxplot(X.values[[1]]~X.groups1, main="Math", ylab = "Math", col=col)
par(mar=c(5,4,2,2))
boxplot(X.values[[1]]~X.groups2, ylab = "Math", xlab="ESCS quartiles", las=3, col="gold")
#Read
x11()
par(mfrow=c(2,1), mar=c(2,4,2,2))
boxplot(X.values[[2]]~X.groups1, main="Reading", ylab = "Reading", col=col)
par(mar=c(5,4,2,2))
boxplot(X.values[[2]]~X.groups2, ylab = "Reading", xlab="ESCS quartiles", las=3, col="gold")
# The variances look pretty comparable

#b) Group means
math.mean <- data.frame(matrix(data = tapply(X.values[[1]],interact.groups,mean), nrow = 2, ncol = 4))
names(math.mean) <- levels(X.groups2)
row.names(math.mean) <- levels(X.groups1)
math.mean 
read.mean <- data.frame(matrix(data = tapply(X.values[[2]],interact.groups,mean), nrow = 2, ncol = 4))
names(read.mean) <- levels(X.groups2)
row.names(read.mean) <- levels(X.groups1)
read.mean 

gap.math <- sort(math.mean[1,]-math.mean[2,])
gap.read <- sort(read.mean[1,]-read.mean[2,])
x11(width = 18, height = 15)
par(mfrow = c(2,1))
barplot(as.matrix(gap.math), ylab = "Native vs immig", xlab="ESCS quartiles", main = "MATH", col = "red")
barplot(as.matrix(gap.read), ylab = "Native vs  immig", xlab="ESCS quartiles", main = "READING", col = "blue")

