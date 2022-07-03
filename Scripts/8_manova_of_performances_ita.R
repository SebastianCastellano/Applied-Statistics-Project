#### MANOVA ON PERFORMANCE OF IMMIGRANT AND NATIVE STUDENTS ####


# Load data-frame
studentsData= read.table(file = "student_ita.txt", header = T)
studentsData=na.omit(studentsData)

X = data.frame(studentsData$math, studentsData$scie, studentsData$read)
names(X) = c("math", "scie", "read")
immStatus = studentsData$immigration

## Extract Indeces
nativeIndex = which(immStatus==1)
n_native = length(nativeIndex)

immIndex_1 = which(immStatus==3)
nImm_1 = length(immIndex_1)

immIndex_2 = which(immStatus==2)
nImm_2 = length(immIndex_2)

## Build dataframe
X_native = X[nativeIndex,]
X_imm1 = X[immIndex_1,]
X_imm2 = X[immIndex_2,]

par(mfrow=c(1,3))
boxplot(X_native, ylim = c(150,800))
boxplot(X_imm1, ylim = c(150,800))
boxplot(X_imm2, ylim = c(150,800))

#________________________________________________________________________________
###################### CHECK HYPOTHESIS FOR MANOVA ##############################

#### Gaussianity #####
######################

#-------------------------------------------------------------------------------
### NATIVE
x11()
par(mfrow = c(2,2))

math = X_native$math
hist(math, prob=T, ylab='density', xlab='Math', main='Native Math score')
lines(min(math):max(math), dnorm(min(math):max(math),mean(math),sd(math)), col='blue', lty=2)

scie = X_native$scie
hist(scie, prob=T, ylab='density', xlab='Science', main='Native Science score')
lines(min(scie):max(scie), dnorm(min(scie):max(scie),mean(scie),sd(scie)), col='blue', lty=2)

read = X_native$read
hist(read, prob=T, ylab='density', xlab='Reading', main='Native Reading score')
lines(min(read):max(read), dnorm(min(read):max(read),mean(read),sd(read)), col='blue', lty=2)

d2 <- mahalanobis(X_native, colMeans(X_native), cov(X_native))

# plot of the distribution of d2
hist(d2, prob=T, ylab = 'density', xlab = 'Malhanobis distance')
lines(min(d2):max(d2), dchisq(min(d2):max(d2), mean(d2), sd(d2)), col='blue', lty=2)

# Test on the chi-squared fitting
d2.class <- cut(d2, qchisq((0:10)/10, df = 2))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/10, 10), simulate.p.value = T)

install.packages("mvnormtest")
library(mvnormtest)
load('D:/magistrale/1/secondo semestre/applied statistics/progetto/data selection/manova/mcshapiro.test.RData')
mcshapiro.test(X_native[sample(5000),])

# ------------------------------------------------------------------------------

## First Gen Immigrant students
x11()
par(mfrow = c(2,2))

math = X_imm1$math
hist(math, prob=T, ylab='density', xlab='Math', main='1° Gen Imm Math score')
lines(min(math):max(math), dnorm(min(math):max(math),mean(math),sd(math)), col='blue', lty=2)

scie = X_imm1$scie
hist(scie, prob=T, ylab='density', xlab='Science', main='1° Gen Imm Science score')
lines(min(scie):max(scie), dnorm(min(scie):max(scie),mean(scie),sd(scie)), col='blue', lty=2)

read = X_imm1$read
hist(read, prob=T, ylab='density', xlab='Reading', main='1° Gen Imm Reading score')
lines(min(read):max(read), dnorm(min(read):max(read),mean(read),sd(read)), col='blue', lty=2)

d2 <- mahalanobis(X_imm1, colMeans(X_imm1), cov(X_imm1))

# Test on the chi-squared fitting
d2.class <- cut(d2, qchisq((0:10)/10, df = 2))
d2.freq  <- table(d2.class)

# plot of the distribution of d2
hist(d2, prob=T, ylab = 'density', xlab = 'Malhanobis distance')
lines(min(d2):max(d2), dchisq(min(d2):max(d2), mean(d2), sd(d2)), col='blue', lty=2)

chisq.test(x = d2.freq, p = rep(1/10, 10), simulate.p.value = T)
#--------------------------------------------------------------------------------------------

## Second Gen Immigrant Student
x11()
par(mfrow = c(2,2))

math = X_imm2$math
hist(math, prob=T, ylab='density', xlab='Math', main='2° Gen Imm Math score')
lines(min(math):max(math), dnorm(min(math):max(math),mean(math),sd(math)), col='blue', lty=2)

scie = X_imm2$scie
hist(scie, prob=T, ylab='density', xlab='Science', main='2° Gen Imm Science score')
lines(min(scie):max(scie), dnorm(min(scie):max(scie),mean(scie),sd(scie)), col='blue', lty=2)

read = X_imm2$read
hist(read, prob=T, ylab='density', xlab='Reading', main='2° Gen Imm Reading score')
lines(min(read):max(read), dnorm(min(read):max(read),mean(read),sd(read)), col='blue', lty=2)

d2 <- mahalanobis(X_imm2, colMeans(X_imm2), cov(X_imm2))

# plot of the distribution of d2
hist(d2, prob=T, ylab = 'density', xlab = 'Malhanobis distance')
lines(min(d2):max(d2), dchisq(min(d2):max(d2), mean(d2), sd(d2)), col='blue', lty=2)

# Test on the chi-squared fitting
d2.class <- cut(d2, qchisq((0:10)/10, df = 2))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/10, 10), simulate.p.value = T)

mcshapiro.test(X_imm2)
mcshapiro.test(X_imm1)
#-----------------------------------------------------------------------------------------

##### SAME COVARIANCE MATRIX #####

S1 = cov(X_native)
S2 = cov(X_imm1)
S3 = cov(X_imm2)

x11(width=21)
par(mfrow=c(1,3))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))

dev.off()

norm(S1 - S2, type="F")/norm(S1, type = "F")
norm(S1 - S3, type="F")/norm(S1, type = "F")


#________________________________________________________________________________
################################ MANOVA TEST ####################################

n1 <- dim(X_native)[1]
n2 <- dim(X_imm1)[1]
n3 <- dim(X_imm2)[1]
n  <- n1+n2+n3

g  <- 3
p  <- 3

immig.status <- factor(studentsData$immigration, labels=c('native','immig2','immig1'))

fit <- manova(as.matrix(X) ~ immig.status)
summary.manova(fit,test="Wilks")

summary.aov(fit)


alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(X,mean)         # estimates mu
m1 <- sapply(X[nativeIndex,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(X[immIndex_1,],mean)    # estimates mu.2=mu+tau.2
m3 <- sapply(X[immIndex_2,],mean)    # estimates mu.3=mu+tau.3

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )

CI <- list(native_immig1=cbind(inf12, sup12), native_immig2=cbind(inf13, sup13), immig1_immig2=cbind(inf23, sup23))
CI

x11()
par(mfrow=c(2,3))
boxplot(X[,1]~immig.status, col = rainbow(3))
boxplot(X[,2]~immig.status, col = rainbow(3))
boxplot(X[,3]~immig.status, col = rainbow(3))

mg <- rbind(m1,m2,m3)
sp.name <- c('Math','Read', 'Scie')
for(k in 1:3){
  plot(c(1,g*(g-1)/2), ylim=c(-20,70), xlim=c(1,3), pch='', 
       xlab='pairs treat', ylab=paste('CI tau',k), 
       main=paste('CI tau',sp.name[k]))
  lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
  points(1, mg[1,k]-mg[2,k], pch=16); 
  points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
  points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
  lines (c(2,2), c(CI[[2]][k,1],CI[[2]][k,2])); 
  points(2, mg[1,k]-mg[3,k], pch=16);
  points(2, CI[[2]][k,1], col=rainbow(g)[3], pch=16); 
  points(2, CI[[2]][k,2], col=rainbow(g)[1], pch=16);
  lines (c(3,3), c(CI[[3]][k,1],CI[[3]][k,2])); 
  points(3, mg[2,k]-mg[3,k], pch=16);
  points(3, CI[[3]][k,1], col=rainbow(g)[3], pch=16); 
  points(3, CI[[3]][k,2], col=rainbow(g)[2], pch=16);  
  abline(h=0)
}
