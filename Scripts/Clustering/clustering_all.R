
# CLUSTERING ANALYSIS OF ALL EUROPEAN COUNTRIES

setwd("D:/APPLIED/PROGETTO/DATA")

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



countries <- list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)
name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
n_countries <- length(countries)

varID = c('math', 'ESCS_status', 'family_wealth')

# DEU 5 --> 63% reduction in tot.withinns
# ITA 5 --> 61%
# AUT 5 --> 61%
# BEL 5 --> 63%
# GBR 5 --> 63%

ww = rep(0,15)
X = data.frame(scale(ITA[,varID]))
for (uu in 1:15){
  C = kmeans(X,uu)
  ww[uu] = C$tot.withinss/C$totss
}
xx = seq(1,15)
x11()
plot(xx,ww,xlab = 'n. of cluster', ylab = 'entropy')

C = kmeans(X,5)
clusters = C$cluster
clusterSize = C$size
table(LUX$immigration[clusters == 2])
for (j in 1:5) {
  immigFrac[j] = (table(LUX$immigration[clusters == j])[2] + 
                    table(LUX$immigration[clusters == j])[3])/clusterSize[j]
}
immigFrac

orderGain = rep(0,n_countries)

for (i in 1:n_countries) {
  
  #X = data.frame(scale(data.frame(countries[[i]]$math, countries[[i]]$ESCS_status,  countries[[i]]$edu_resources,
                 #countries[[i]]$family_wealth, countries[[i]]$cult_poss)))
  #names(X) = c("math","ESCS_status", "edu_resourcess", "family_wealth", "cult_poss")
  
  CNT = countries[[i]]
  X = data.frame(scale(CNT[,varID]))
  
  n = dim(X)[1]
  
  ##################################### K-MEANS CLUSTERING #########################################
  
  k = 5 # number of clusters
  C = kmeans(X,k)
  orderGain[i] = 100 - C$tot.withinss/C$totss*100
  clusters = C$cluster
  # Graphical Representation

  clusterSize = C$size  
  # Percentage of immigrants in the cluster
  immigFrac = rep(0,k)
  for (j in 1:k) {
    immigFrac[j] = (table(CNT$immigration[clusters == j])[2] + 
                      table(CNT$immigration[clusters == j])[3])/clusterSize[j]
  }
  Y = X
  Y$cluster = C$cluster
  x11()
  par(mfrow = c(2,2))
  boxplot(math~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(family_wealth~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(ESCS_status~cluster, Y, col = "gold", main = name_countries[i])
  barplot(immigFrac, names.arg = c('1','2','3','4','5')
          , ylim = c(0,1), main = 'Fraction of Immigrants in each cluster')
  
  
  
  
  
  # # Build data-frame of the 2 clusters
  # clust1 = countries[[i]][which(C$cluster==1),]
  # clust2 = countries[[i]][which(C$cluster==2),]
  # 
  # # Analysis of cluster 1
  # m1math = mean(clust1$math)
  # m1family_wealth = mean(clust1$family_wealth)
  # 
  # per1 = dim(clust1)[1]/n
  # 
  # immig_frac1 = ( table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] ) / 
  #   (table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] + 
  #      table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]])
  # 
  # # Analysis of cluster 2  
  # m2math = mean(clust2$math)
  # m2family_wealth = mean(clust2$family_wealth)
  # 
  # per2 = dim(clust2)[1]/n
  # 
  # immig_frac2 = ( table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]] ) / 
  #   (table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] + 
  #      table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]])
  # 
  # print("___________________________________________________________________________")
  # print(name_countries[i])
  # 
  # print("Analysis of cluster 1:")
  # print(paste("Percetange of Units in Cluster 1: ", per1))
  # print(paste("Average math score of cluster 1 =", m1math))
  # print(paste("Average family wealth of cluster 1 =", m1family_wealth))
  # print(paste("Fraction of immigrants contained in Cluster 1: ", immig_frac1))
  # 
  # print("-----------------------------------------------------------------------------")
  # 
  # print("Analysis of cluster 2:")
  # print(paste("Percetange of Units in Cluster 2: ",per2 ))
  # print(paste("Average math score of cluster 2 =", m2math))
  # print(paste("Average family wealth of cluster 2 =", m2family_wealth))
  # print(paste("Fraction of immigrants contained in Cluster 2: ", immig_frac2))
  
}
