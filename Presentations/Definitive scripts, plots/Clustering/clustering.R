# CLUSTERING ANALYSIS OF ALL EUROPEAN COUNTRIES 
# k-means with 'math', 'ESCS_status' and 'learn_time_math'

# Datasets
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

varID = c('math', 'ESCS_status', 'learn_time_math', 'short_edu_mat', 'read', 'learn_time_read', 'short_edu_staff', 'stu_behav', 'teach_behav')


# For choosing k

b <- NULL
w <- NULL
for(k in 1:10){
  result.k <- kmeans(DNK, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
}
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)

k=3
perc_immig <- as.data.frame(matrix(ncol=11,nrow=6))
names(perc_immig) <- name_countries
num_clusters <- as.data.frame(matrix(ncol=11,nrow=3))
names(num_clusters) <- name_countries

orderGain = rep(0,n_countries)
for (i in 1:n_countries) {
  
  CNT = countries[[i]]
  X = data.frame(scale(CNT[,varID]))
  n = dim(X)[1]
  
  ## K-MEANS CLUSTERING ##
  
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
  
  num_clusters[1,i] <- dim(CNT[which(clusters==1),])[1]
  num_clusters[2,i] <- dim(CNT[which(clusters==2),])[1]
  num_clusters[3,i] <- dim(CNT[which(clusters==3),])[1]
  
  tot_immig <- length(which(CNT$immigration==2 | CNT$immigration==3))
  immig_1cl <- CNT$immigration[clusters==1]
  immig_2cl <- CNT$immigration[clusters==2]
  immig_3cl <- CNT$immigration[clusters==3]
  immig1 <- length(which(immig_1cl==2 | immig_1cl==3))
  immig2 <- length(which(immig_2cl==2 | immig_2cl==3))
  immig3 <- length(which(immig_3cl==2 | immig_3cl==3))
  perc_immig[1,i] = immig1/tot_immig
  perc_immig[2,i] = immig2/tot_immig
  perc_immig[3,i] = immig3/tot_immig
  
  tot_native <- length(which(CNT$immigration==1))
  native_1cl <- CNT$immigration[clusters==1]
  native_2cl <- CNT$immigration[clusters==2]
  native_3cl <- CNT$immigration[clusters==3]
  native1 <- length(which(native_1cl==1))
  native2 <- length(which(native_2cl==1))
  native3 <- length(which(native_3cl==1))
  perc_immig[4,i] = native1/tot_native
  perc_immig[5,i] = native2/tot_native
  perc_immig[6,i] = native3/tot_native
  
  # Graphical Representation
  color = c('pink', 'khaki', 'lightgreen')
  par(mfrow = c(2,2), cex.main = 2)
  boxplot(math~cluster, Y, col = color, main = 'Math scores', xlab = '', ylab = '')
  boxplot(read~cluster, Y, col = color, main = 'Reading scores', xlab = '', ylab = '')
  boxplot(ESCS_status~cluster, Y, col = color, main = 'ESCS status', xlab = '', ylab = '')
  barplot(perc_immig[1:3,i], names.arg = c('1','2','3')
          , ylim = c(0,1), col = color, main = 'Percentage of immigrant students', xlab = '', ylab = '')
  boxplot(-short_edu_staff~cluster, Y, col = color, main = 'Educational staff', xlab = '', ylab = '')
  boxplot(-short_edu_mat~cluster, Y, col = color, main = 'Educational material', xlab = '', ylab = '')
  boxplot(-stu_behav~cluster, Y, col = color, main = 'Student behaviour', xlab = '', ylab = '')
  boxplot(-teach_behav~cluster, Y, col = color, main = 'Teachers behaviour', xlab = '', ylab = '')
 
}


