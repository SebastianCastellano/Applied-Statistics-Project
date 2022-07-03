
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



countries <- list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR, EUR)
name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR", "EUR")
n_countries <- length(countries)

for (i in 1:n_countries) {
  
  X = data.frame(scale(data.frame(countries[[i]]$math, countries[[i]]$ESCS_status,  countries[[i]]$edu_resources,
                 countries[[i]]$family_wealth, countries[[i]]$cult_poss)))
  names(X) = c("math","ESCS_status", "edu_resourcess", "family_wealth", "cult_poss")
  n = dim(X)[1]
  
  ##################################### K-MEANS CLUSTERING #########################################
  C = kmeans(X,2)
  
  # Build data-frame of the 2 clusters
  clust1 = countries[[i]][which(C$cluster==1),]
  clust2 = countries[[i]][which(C$cluster==2),]
  
  # Analysis of cluster 1
  m1math = mean(clust1$math)
  m1family_wealth = mean(clust1$family_wealth)
  
  per1 = dim(clust1)[1]/n
  
  immig_frac1 = ( table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] ) / 
    (table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] + 
       table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]])
  
  # Analysis of cluster 2  
  m2math = mean(clust2$math)
  m2family_wealth = mean(clust2$family_wealth)
  
  per2 = dim(clust2)[1]/n
 
  immig_frac2 = ( table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]] ) / 
    (table(clust1$immigration)[[2]] + table(clust1$immigration)[[3]] + 
       table(clust2$immigration)[[2]] + table(clust2$immigration)[[3]])
  
  print("___________________________________________________________________________")
  print(name_countries[i])
  
  print("Analysis of cluster 1:")
  print(paste("Percetange of Units in Cluster 1: ", per1))
  print(paste("Average math score of cluster 1 =", m1math))
  print(paste("Average family wealth of cluster 1 =", m1family_wealth))
  print(paste("Fraction of immigrants contained in Cluster 1: ", immig_frac1))
  
  print("-----------------------------------------------------------------------------")
  
  print("Analysis of cluster 2:")
  print(paste("Percetange of Units in Cluster 2: ",per2 ))
  print(paste("Average math score of cluster 2 =", m2math))
  print(paste("Average family wealth of cluster 2 =", m2family_wealth))
  print(paste("Fraction of immigrants contained in Cluster 2: ", immig_frac2))
  
}
