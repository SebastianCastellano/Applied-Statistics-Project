library(mvnormtest)
library(rgl)
#load('mcshapiro.test.RData')

#Import datasets
ITA <- read.table(file = "student_ita.txt", header = T)
AUT <- read.table(file = "student_aut.txt", header = T)
BEL <- read.table(file = "student_bel.txt", header = T)
DNK <- read.table(file = "student_dnk.txt", header = T)
DEU <- read.table(file = "student_deu.txt", header = T)
LUX <-read.table(file = "student_lux.txt", header = T)
ESP <- read.table(file = "student_esp.txt", header = T)
SWE <- read.table(file = "student_swe.txt", header = T)
CHE <- read.table(file = "student_che.txt", header = T)
GBR <- read.table(file = "student_gbr.txt", header = T)
EUR <- read.table(file = "student_eur.txt", header = T)

countries <- list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR, EUR)
name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR", "EUR")
n_countries <- length(countries) 

#Select subsets of the datasets (optional)
#EUR.immig = ITA[which(ITA$immigration!=1),]

#Proceed with clustering
cluster_country = EUR
X <- data.frame(math = cluster_country$math, 
                read = cluster_country$read, 
                scie = cluster_country$scie
                #escs = EUR.immig$ESCS_status,
                #wealth = EUR.immig$family_wealth,
                #bullied = EUR.immig$bullied,
                #home_poss = EUR.immig$home_poss      
                )
X <- X[sample(10000),]

#scale?
scale = 0
if(scale){
  X<-data.frame(scale(X))
}

D <- dist(X)
clust1 <- hclust(D, method='single')
clust2 <- hclust(D, method='average')
clust3 <- hclust(D, method='complete')

# plot of the dendrograms
x11()
par(mfrow=c(1,3))
plot(clust1, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust1,k=2)
plot(clust2, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust2,k=2)
plot(clust3, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust3,k=2)

clust3_cutree <- cutree(clust3,k=2)
x11()
plot(X, col=ifelse(clust3_cutree==1,'red','blue'), pch=19)
plot3d(X, size=3, col=clust3_cutree+1, aspect = F) 

coph3 <- cophenetic(clust3)
cor3 <- cor(D,coph3)
cor3



