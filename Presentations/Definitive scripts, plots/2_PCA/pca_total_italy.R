## PCA - Italy ##

stud_ita = read.table(file='student_ita.txt', header = T)
head(stud_ita)
dim(stud_ita)
stud_ita.cat <- stud_ita[,c(1,2,4)]
stud_ita <- stud_ita[-c(1,2,4)]

n <- dim(stud_ita)[1]
p <- dim(stud_ita)[2]

x11()
par(mar=c(8,3,3,3))
boxplot(stud_ita, las=2, col='lightgreen')

stud_ita.sd <- scale(stud_ita)
stud_ita.sd <- data.frame(stud_ita.sd)

head(stud_ita.sd)

x11()
par(mar=c(8,3,3,3))
boxplot(stud_ita.sd, las=2, col='lightgreen',main="Scaled dataset")

pc.stud_ita <- princomp(stud_ita.sd, scores=T)
summary(pc.stud_ita)


x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.stud_ita, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(stud_ita.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.stud_ita$sde^2)/sum(pc.stud_ita$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(stud_ita.sd),labels=1:ncol(stud_ita.sd),las=2)

load.stud <- pc.stud_ita$loadings
load.stud


x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.stud[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))


scores.stud <- pc.stud_ita$scores
scores.stud

# How our data are positioned on the first two principal components 
x11()
plot(scores.stud[,1:2]) 
abline(h=0, v=0, lty=2, col='grey')

x11()
layout(matrix(c(1,2),2))
boxplot(stud_ita.sd, las=2, col='lightgreen', main='Standardized variables')
scores.stud <- data.frame(scores.stud)
boxplot(scores.stud, las=2, col='lightgreen', main='Principal components')

x11()
biplot(pc.stud_ita)

