library(mvnormtest)
load('mcshapiro.test.RData')

#Import datasets
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
AUT <- read.table(file = "student_aut.txt", header = T)
BEL <- read.table(file = "student_bel.txt", header = T)
CHE <- read.table(file = "student_che.txt", header = T)
DEU <- read.table(file = "student_deu.txt", header = T)
DNK <- read.table(file = "student_dnk.txt", header = T)
ESP <- read.table(file = "student_esp.txt", header = T)
GBR <- read.table(file = "student_gbr.txt", header = T)
ITA <- read.table(file = "student_ita.txt", header = T)
LUX <- read.table(file = "student_lux.txt", header = T)
SWE <- read.table(file = "student_swe.txt", header = T)

library(plyr)
EUR <- rbind.fill(AUT,BEL,CHE,DEU,DNK,ESP,GBR,ITA,LUX,SWE)

countries <- list(AUT, BEL, CHE, DEU, DNK, ESP, GBR, ITA, LUX, SWE, EUR)
name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE", "EUR")
n_countries <- length(countries) 

#Manova of math, scie, read for each country
for (i in 1:n_countries) {
  X = data.frame(countries[[i]]$math, countries[[i]]$read,  countries[[i]]$scie)
  names(X) = c("math","read", "scie")
  immStatus = countries[[i]]$immigration
  
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
  
  print(name_countries[i])
  # p_native = mcshapiro.test(X_native[sample(min(300,n_native)),])$p
  # p_imm1 = mcshapiro.test(X_imm1)$p
  # p_imm2 = mcshapiro.test(X_imm2)$p
  # 
  # if(p_native<0.05 || p_imm1<0.05 || p_imm2<0.05)
  #   print("normality not guarenteed")
  # 
  #check homogeneity assumptions
  
  #MANOVA
  n1 <- dim(X_native)[1]
  n2 <- dim(X_imm1)[1]
  n3 <- dim(X_imm2)[1]
  n  <- n1+n2+n3
  
  g  <- 3
  p  <- 3
  
  immig.status <- factor(countries[[i]]$immigration, labels=c('native','immig2','immig1'))

  fit <- manova(as.matrix(X) ~ immig.status)
  p_val = summary.manova(fit)$stats[1,6]
  print(paste("Manova pvalue = ", p_val))


  alpha <- 0.05
  k <- p*g*(g-1)/2
  qT <- qt(1-alpha/(2*k), n-g)

  W <- summary.manova(fit)$SS$Residuals
  m  <- sapply(X,mean)         # estimates mu
  m1 <- sapply(X[nativeIndex,],mean)    # estimates mu.1=mu+tau.1
  m2 <- sapply(X[immIndex_1,],mean)    # estimates mu.2=mu+tau.2
  m3 <- sapply(X[immIndex_2,],mean)    # estimates mu.3=mu+tau.3
  
  #Intervals for native vs immig 1
  inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  pval12 <- (1- pt(abs(m1-m2)/sqrt( diag(W)/(n-g) * (1/n1+1/n2) ),n-g))*(2*k)
  
  #Intervals for native vs immig 1
  inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
  sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
  pval13 <- (1- pt(abs(m1-m3)/sqrt( diag(W)/(n-g) * (1/n1+1/n3) ),n-g))*(2*k)
  
  #Intervals for native vs immig 1
  inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
  sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
  pval23 <- (1- pt(abs(m2-m3)/sqrt( diag(W)/(n-g) * (1/n2+1/n3) ),n-g))*(2*k)
  
  #Math
  if(inf12[1]<0 && sup12[1]>0)
    print(paste("Math native vs immig1 NO significative difference, pval = ",pval12[1]))
  if(inf13[1]<0 && sup13[1]>0)
    print(paste("Math native vs immig2 NO significative difference, pval = ",pval13[1]))
  if(inf23[1]<0 && sup23[1]>0)
    print(paste("Math immig1 vs immig2 NO significative difference, pval = ",pval23[1]))
  
  #Science
  if(inf12[2]<0 && sup12[2]>0)
    print(paste("Science native vs immig1 NO significative difference, pval = ",pval12[2]))
  if(inf13[2]<0 && sup13[2]>0)
    print(paste("Sience native vs immig2 NO significative difference, pval = ",pval13[2]))
  if(inf23[2]<0 && sup23[2]>0)
    print(paste("Sience immig1 vs immig2 NO significative difference, pval = ",pval23[2]))
  
  #Reading
  if(inf12[3]<0 && sup12[3]>0)
    print(paste("Reading native vs immig1 NO significative difference, pval = ",pval12[3]))
  if(inf13[3]<0 && sup13[3]>0)
    print(paste("Reading native vs immig2 NO significative difference, pval = ",pval13[3]))
  if(inf23[3]<0 && sup23[3]>0)
    print(paste("Reading immig1 vs immig2 NO significative difference, pval = ",pval23[3]))
  
  writeLines("\n")
   }

#Two-ways Manova between countries

current_country <- EUR

XX <- data.frame(immig = factor(current_country$immigration, labels=c("native","immig2","immig1")),
                 country = factor(current_country$country),
                 math = current_country$math,
                 scie = current_country$scie,
                 read = current_country$read
                 )

# Verificare normality assumption separatamente per ciascuna combinazione di country/immig?
#Oppure basta averla verificata in principio

fit.country.immig <- manova( as.matrix(XX[,3:5]) ~ immig + country + immig:country, data = XX)
summary.manova(fit.country.immig, test="Wilks")



#Comparing the test scores of immigrants in different countries
EUR.immig.only <- EUR[which(EUR$immigration!=1),]

X = data.frame(math = EUR.immig.only$math,
               scie = EUR.immig.only$scie,
               read = EUR.immig.only$read)
## Extract Indeces
X_AUT <- X[which(EUR.immig.only$country=="AUT"),]
X_BEL <- X[which(EUR.immig.only$country=="BEL"),]
X_CHE <- X[which(EUR.immig.only$country=="CHE"),]
X_DEU <- X[which(EUR.immig.only$country=="DEU"),]
X_DNK <- X[which(EUR.immig.only$country=="DNK"),]
X_ESP <- X[which(EUR.immig.only$country=="ESP"),]
X_GBR <- X[which(EUR.immig.only$country=="GBR"),]
X_ITA <- X[which(EUR.immig.only$country=="ITA"),]
X_LUX <- X[which(EUR.immig.only$country=="LUX"),]
X_SWE <- X[which(EUR.immig.only$country=="SWE"),]

name_test <- c("math","scie", "read")

n1 <- dim(X_AUT)[1]
n2 <- dim(X_BEL)[1]
n3 <- dim(X_CHE)[1]
n4 <- dim(X_DEU)[1]
n5 <- dim(X_DNK)[1]
n6 <- dim(X_ESP)[1]
n7 <- dim(X_GBR)[1]
n8 <- dim(X_ITA)[1]
n9 <- dim(X_LUX)[1]
n10 <- dim(X_SWE)[1]
n  <- n1+n2+n3+n4+n5+n6+n7+n8+n9+n10

g  <- 10

country.status <- factor(EUR.immig.only$country)
country.means.immig.only <- matrix(nrow=10,ncol=3)

col <- 1
for (i in name_test) {
  
  print(i)
  # p_native = mcshapiro.test(X_native[sample(min(300,n_native)),])$p
  # p_imm1 = mcshapiro.test(X_imm1)$p
  # p_imm2 = mcshapiro.test(X_imm2)$p
  # 
  # if(p_native<0.05 || p_imm1<0.05 || p_imm2<0.05)
  #   print("normality not guarenteed")
  # 
  #check homogeneity assumptions
  
  #ANOVA
  X.curr <- X[i]
  fit <- aov(as.matrix(X.curr) ~ country.status)
  p_val  <- summary(fit)[[1]][["Pr(>F)"]][1]
  print(paste("Anova pvalue = ", p_val))
  
  alpha <- 0.05
  k <- g*(g-1)/2
  qT <- qt(1-alpha/(2*k), n-g)
  
  W <- summary.aov(fit)$SS$Residuals
  m  <- sapply(X.curr,mean) # estimates mu
  m1 <- mean(X_AUT[,i])
  m2 <- mean(X_BEL[,i])
  m3 <- mean(X_CHE[,i])
  m4 <- mean(X_DEU[,i])
  m5 <- mean(X_DNK[,i])
  m6 <- mean(X_ESP[,i])
  m7 <- mean(X_GBR[,i])
  m8 <- mean(X_ITA[,i])
  m9 <- mean(X_LUX[,i])
  m10 <- mean(X_SWE[,i])
  

  country.means.immig.only[1,col] <- m1
  country.means.immig.only[2,col] <- m2
  country.means.immig.only[3,col] <- m3
  country.means.immig.only[4,col] <- m4
  country.means.immig.only[5,col] <- m5
  country.means.immig.only[6,col] <- m6
  country.means.immig.only[7,col] <- m7
  country.means.immig.only[8,col] <- m8
  country.means.immig.only[9,col] <- m9
  country.means.immig.only[10,col] <- m10
  
  col <- col +1                        
  # 
  # #Intervals for native vs immig 1
  # inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  # sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
  # pval12 <- (1- pt(abs(m1-m2)/sqrt( diag(W)/(n-g) * (1/n1+1/n2) ),n-g))*(2*k)
  # 
  # #Intervals for native vs immig 1
  # inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
  # sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
  # pval13 <- (1- pt(abs(m1-m3)/sqrt( diag(W)/(n-g) * (1/n1+1/n3) ),n-g))*(2*k)
  # 
  # #Intervals for native vs immig 1
  # inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
  # sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
  # pval23 <- (1- pt(abs(m2-m3)/sqrt( diag(W)/(n-g) * (1/n2+1/n3) ),n-g))*(2*k)
  # 
  # #Math
  # if(inf12[1]<0 && sup12[1]>0)
  #   print(paste("Math native vs immig1 NO significative difference, pval = ",pval12[1]))
  # if(inf13[1]<0 && sup13[1]>0)
  #   print(paste("Math native vs immig2 NO significative difference, pval = ",pval13[1]))
  # if(inf23[1]<0 && sup23[1]>0)
  #   print(paste("Math immig1 vs immig2 NO significative difference, pval = ",pval23[1]))
  # 
  # #Science
  # if(inf12[2]<0 && sup12[2]>0)
  #   print(paste("Science native vs immig1 NO significative difference, pval = ",pval12[2]))
  # if(inf13[2]<0 && sup13[2]>0)
  #   print(paste("Sience native vs immig2 NO significative difference, pval = ",pval13[2]))
  # if(inf23[2]<0 && sup23[2]>0)
  #   print(paste("Sience immig1 vs immig2 NO significative difference, pval = ",pval23[2]))
  # 
  # #Reading
  # if(inf12[3]<0 && sup12[3]>0)
  #   print(paste("Reading native vs immig1 NO significative difference, pval = ",pval12[3]))
  # if(inf13[3]<0 && sup13[3]>0)
  #   print(paste("Reading native vs immig2 NO significative difference, pval = ",pval13[3]))
  # if(inf23[3]<0 && sup23[3]>0)
  #   print(paste("Reading immig1 vs immig2 NO significative difference, pval = ",pval23[3]))
  # 
  # writeLines("\n")
}


country.means <- matrix(nrow=10,ncol=3)

for (i in 1:10){
  country.curr <- countries[[i]]
  country.tests.only <- country.curr[c("math","scie","read")]
  country.means[i,] <- sapply(country.tests.only, mean)
  
}



