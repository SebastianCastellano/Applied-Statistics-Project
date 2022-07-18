
# CLUSTERING ANALYSIS OF ALL EUROPEAN COUNTRIES



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

varID = c('math', 'ESCS_status', 'family_wealth')

# MATH AND WEALTH VARIABLES----

# How much the clustering reduces the entropy
orderGain = rep(0,n_countries)
for (i in 1:n_countries) {
  
  CNT = countries[[i]]
  X = data.frame(scale(CNT[,varID]))
  n = dim(X)[1]
  
  ##################################### K-MEANS CLUSTERING #########################################
  k = 5 # number of clusters
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
  boxplot(family_wealth~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(ESCS_status~cluster, Y, col = "gold", main = name_countries[i])
  barplot(immigFrac, names.arg = c('1','2','3','4','5')
          , ylim = c(0,1), main = 'Fraction of immigrants in each cluster')
}

# MATH AND ----

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

countries <- list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR, EUR)
name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR", "EUR")
n_countries <- length(countries)

varID = c('PV3MATH','WEALTH','ESCS')

#'ST186Q08HA','PA009Q08NA','PA009Q03NA')
varNames = c('math','family_wealth','ESCS_status')
 #         'sadness', 'parent_language_skills','parent_free_time')

x11()
plot(X, col = clusters)


for (i in 1:n_countries) {
  
  CNT = countries[[i]]
  CNT = na.omit(CNT[,c(varID,'PA007Q14NA')])
  X = data.frame(scale(na.omit(CNT[,varID])))
  names(X) = varNames
  n = dim(X)[1]
  
  if(n!=0) {
  ##################################### K-MEANS CLUSTERING #########################################
  
    
  k = 5 # number of clusters
  C = kmeans(X,k)
  clusters = C$cluster
  # Graphical Representation
  clusterSize = C$size  
  # Percentage of immigrants in the cluster
  immigFrac = rep(0,k)
  for (j in 1:k) {
    immigFrac[j] = table(CNT$PA007Q14NA[clusters==j])[1]/clusterSize[j]
  }
  Y = X
  Y$cluster = C$cluster
  x11()
  par(mfrow = c(2,2))
  boxplot(math~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(family_wealth~cluster, Y, col = "gold", main = name_countries[i])
  boxplot(ESCS_status~cluster, Y, col = "gold", main = name_countries[i])
  #boxplot(sadness~cluster, Y, col = "gold", main = name_countries[i])
  #boxplot(parent_language_skills~cluster, Y, col = "gold", main = name_countries[i])
  #boxplot(parent_free_time~cluster, Y, col = "gold", main = name_countries[i])
  barplot(immigFrac, names.arg = c('1','2','3','4','5')
          , ylim = c(0,1), main = 'Homework Help')
  }
}
