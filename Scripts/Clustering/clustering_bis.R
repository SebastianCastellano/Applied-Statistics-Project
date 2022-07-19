# CLUSTERING ANALYSIS OF ALL EUROPEAN COUNTRIES 
# k-means with 'math', 'ESCS_status' and 'learn_time_math'

# CLEANE DATASETS
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

countries <- list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR, EUR)
name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR",'EUR')
n_countries <- length(countries)

varID = c('math', 'ESCS_status', 'learn_time_math')

# MATH AND WEALTH VARIABLES----

# For choosing k

b <- NULL
w <- NULL
for(k in 1:10){
  result.k <- kmeans(dataset_dnk, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
}
x11()
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)

k=3

# How much the clustering reduces the entropy
orderGain = rep(0,n_countries)
for (i in 1:n_countries) {
  
  CNT = countries[[i]]
  X = data.frame(scale(CNT[,varID]))
  n = dim(X)[1]
  
  ##################################### K-MEANS CLUSTERING #########################################
  C = kmeans(X,k)
  
  orderGain[i] = 100 - C$tot.withinss/C$totss*100
  clusters = C$cluster
  clusterSize = C$size  
  
  # Percentage of immigrants in the cluster
  immigFrac = rep(0,k)
  for (j in 1:k) {
    immigFrac[j] = 1-table(CNT$immigration[clusters==j])[1]/clusterSize[j]
  }
  Y = X
  Y$cluster = clusters
  
  # Graphical Representation
  x11()
  par(mfrow = c(2,2))
  boxplot(math~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(learn_time_math~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(ESCS_status~cluster, Y, col = "gold", main = name_countries[i])
  barplot(immigFrac, names.arg = c('1','2','3')
          , ylim = c(0,1), main = 'Fraction of immigrants in each cluster')
}

