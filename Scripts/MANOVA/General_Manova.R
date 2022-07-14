library(mvnormtest)
library(MASS)

load('~/GitHub/Applied-Statistics-Project/scripts/mcshapiro.test.RData')

#Import datasets
# Directory with raw datasets
setwd("D:/APPLIED/GITHUB/Applied-Statistics-Project/txt - files/European countries raw")

ITA <- read.table(file = "italy.txt", header = T)
AUT <- read.table(file = "austria.txt", header = T)
BEL <- read.table(file = "belgium.txt", header = T)
DNK <- read.table(file = "denmark.txt", header = T)
DEU <- read.table(file = "germany.txt", header = T)
LUX <- read.table(file = "luxembourg.txt", header = T)
ESP <- read.table(file = "spain1.txt", header = T)
SWE <- read.table(file = "sweden.txt", header = T)
CHE <- read.table(file = "swiss.txt", header = T)
GBR <- read.table(file = "greatbrit.txt", header = T)
EUR <- rbind(ITA,AUT,BEL,DNK,DEU,LUX,ESP,SWE,CHE,GBR)

name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE")
n_countries <- length(name_countries) 


#a) Two-way Manova on Math, Sciencence, reading scores. group1=IMMIG status, group2=CNT

############################# INPUTS ################################
varID = c('PV3MATH','PV3SCIE', 'PV3READ')
#####################################################################

X <- na.omit(EUR[, c(varID, c('IMMIG', 'CNT'))])

X.values <-- X[,varID]

names(X.values) <- c("Math","Science","Reading")

X.groups1 <- factor(X$IMMIG, labels = c("native","2nd gen","1st gen"))
n1 = length(levels(X.groups1)) 
X.groups2 <- factor(X$CNT)
n2 = length(levels(X.groups2))
interact.groups <- interaction(X.groups1,X.groups2)
ni = n1*n2

man <- manova(as.matrix(X.values) ~ X.groups1 + X.groups2 + X.groups1:X.groups2)
summary(man)  #all significant
summary.aov(man) 


# Who cares about hypothesis? ----
# #Check assumptions: Gaussianity and same covariance structure
# #Gaussianity (residuals analysis)
# man.res <- man$residuals
# x11()
# qqnorm(man.res)
# qqline(man.res)
# set.seed(30)
# shapiro.test(sample(man.res,5000))$p #not super gaussian
# 
# #Covariance structure
# interact.groups <- interaction(X.groups1,X.groups2)
# cov(X.values[which(X.groups=="Yes"),])
# cov(X.values[which(X.groups=="No"),])
# bartlett.test(X.values,interact.groups.fact) # ok if pvalue big , here it is not
# #Boxplot (graphical qualitative analysis)
# col <- c("red","green3","blue")

# #Math
# x11()
# par(mfrow=c(2,1), mar=c(2,4,2,2))
# boxplot(X.values[[1]]~X.groups1, main="Math", ylab = "Math", col=col)
# par(mar=c(3,4,2,2))
# boxplot(X.values[[1]]~X.groups2,ylab = "Math", las=3, col="gold")
# #Science
# x11()
# par(mfrow=c(2,1), mar=c(2,4,2,2))
# boxplot(X.values[[2]]~X.groups1, main="Sience", ylab = "Science", col=col)
# par(mar=c(3,4,2,2))
# boxplot(X.values[[2]]~X.groups2,ylab = "Science", las=3, col="gold")
# #Reading
# x11()
# par(mfrow=c(2,1), mar=c(2,4,2,2))
# boxplot(X.values[[3]]~X.groups1, main="Reading", ylab = "Reading", col=col)
# par(mar=c(3,4,2,2))
# boxplot(X.values[[3]]~X.groups2,ylab = "Reading", las=3, col="gold")
# # The variances look pretty comparable

#----
#b) Group means
Math.mean <- data.frame(matrix(data = tapply(X.values[[1]],interact.groups,mean), nrow = n1, ncol = n2))
names(Math.mean) <- levels(X.groups2)
row.names(Math.mean) <- levels(X.groups1)
Math.mean 
Science.mean <- data.frame(matrix(data = tapply(X.values[[2]],interact.groups,mean), nrow = n1, ncol = n2))
names(Science.mean) <- levels(X.groups2)
row.names(Science.mean) <- levels(X.groups1)
Science.mean 
Reading.mean <- data.frame(matrix(data = tapply(X.values[[3]],interact.groups,mean), nrow = n1, ncol = n2))
names(Reading.mean) <- levels(X.groups2)
row.names(Reading.mean) <- levels(X.groups1)
Reading.mean 

gap.Math <- sort(Math.mean[1,]-Math.mean[2,])
gap.Science <- sort(Science.mean[1,]-Science.mean[2,])
gap.Reading <- sort(Reading.mean[1,]-Reading.mean[2,])
x11(width = 18, height = 15)
par(mfrow = c(3,1))
barplot(as.matrix(gap.Math), ylab = "Native vs 2nd gen. imm.", main = "Math", col = "red")
barplot(as.matrix(gap.Science), ylab = "Native vs  2nd gen. imm.", main = "Science", col = "green2")
barplot(as.matrix(gap.Reading), ylab = "Native vs  2nd gen. imm.", main = "Reading", col = "blue")

