library(mvnormtest)
library(MASS)
load('~/GitHub/Applied-Statistics-Project/scripts/mcshapiro.test.RData')

#Import datasets
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
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

countries <- list(AUT, BEL, CHE, DEU, DNK, ESP, GBR, ITA, LUX, SWE, EUR)
name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE", "EUR")
n_countries <- length(countries) 

variables <- c("CULTURAL POSSESSIONS", "FAMILY WEALTH", "ESCS STATUS", 
               "EDUCATIONAL RESOURCES, HOME POSSESSIONS")

cp_scores <- rep(0, n_countries)
fw_scores <- rep(0, n_countries)
es_scores <- rep(0, n_countries)
er_scores <- rep(0, n_countries)
hp_scores <- rep(0, n_countries)

for (i in 1:n_countries) {
  
  # BUILD DATAFRAMES ----
  
  X = data.frame(countries[[i]]$cult_poss, countries[[i]]$family_wealth,  countries[[i]]$ESCS_status, 
                 countries[[i]]$edu_resources, countries[[i]]$home_poss)
  names(X) = c("cult_poss","family_wealth", "ESCS_status", "edu_resources", "home_poss")
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
  
  X_imm = X[-nativeIndex,]
  
  print("________________________________________________________________________________________________")
  print(paste("                               ", name_countries[i]))
  
  
  # HYPOTHESIS CHECK ---- 
  # p_native = mcshapiro.test(X_native[sample(min(300,n_native)),])$p
  # p_imm1 = mcshapiro.test(X_imm1)$p
  # p_imm2 = mcshapiro.test(X_imm2)$p
  # 
  # if(p_native<0.05 || p_imm1<0.05 || p_imm2<0.05)
  #   print("normality not guarenteed")
  # #check homogeneity assumptions
  
  # MANOVA ----
  n1 <- dim(X_native)[1]
  n2 <- dim(X_imm1)[1]
  n3 <- dim(X_imm2)[1]
  n  <- n1+n2+n3
  
  g  <- 3
  p  <- dim(X)[2]
  
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
  m3 <- sapply(X[immIndex_2,],mean)# estimates mu.3=mu+tau.3
  mImm <- sapply(X_imm, mean)
  
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
  
  
  # PRINT RESULTS ----
  
  
  
  ############## CULTURAL POSSESSIONS
  
  print("CULTURAL POSSESSIONS:")
  cp_scores[i] = m1[1] - mImm[1]
  if(inf12[1]>0 || sup12[1]<0){
    print("Native - 1° Generation Immigrants")
    print(paste("Native mean = ", m1[1]))
    print(paste("Immig1 mean = ", m2[1]))
    print(paste("p-value: ", pval12[1]))
    }
  
  if(inf13[1]>0 || sup13[1]<0){
    print("Native - 2° Generation Immigrants")
    print(paste("Native mean = ", m1[1]))
    print(paste("Immig2 mean = ", m3[1]))
    print(paste("p-value: ", pval13[1]))
  }
  
  if(inf23[1]>0 || sup23[1]<0){
    print("1° Generation Immigrants - 2° Generation Immigrants")
    print(paste("Immig1 mean = ", m2[1]))
    print(paste("Immig2 mean = ", m3[1]))
    print(paste("p-value: ", pval23[1]))
  }
  
  
  ############## FAMILY WEALTH
  fw_scores[i] = m1[2] - mImm[2]
  print("FAMILY WEALTH")
  if(inf12[2]>0 || sup12[2]<0){
    print("Native - 1° Generation Immigrants")
    print(paste("Native mean = ", m1[2]))
    print(paste("Immig1 mean = ", m2[2]))
    print(paste("p-value: ", pval12[2]))
  }
  
  if(inf13[2]>0 || sup13[2]<0){
    print("Native - 2° Generation Immigrants")
    print(paste("Native mean = ", m1[2]))
    print(paste("Immig2 mean = ", m3[2]))
    print(paste("p-value: ", pval13[2]))
  }
  
  if(inf23[2]>0 || sup23[2]<0){
    print("1° Generation Immigrants - 2° Generation Immigrants")
    print(paste("Immig1 mean = ", m2[2]))
    print(paste("Immig2 mean = ", m3[2]))
    print(paste("p-value: ", pval23[2]))
  }
  
  
  ############## ESCS STATUS
  es_scores[i] = m1[3] - mImm[3]
  print("ESCS STATUS")
  if(inf12[3]>0 || sup12[3]<0){
    print("Native - 1° Generation Immigrants")
    print(paste("Native mean = ", m1[3]))
    print(paste("Immig1 mean = ", m2[3]))
    print(paste("p-value: ", pval12[3]))
  }
  
  if(inf13[3]>0 || sup13[3]<0){
    print("Native - 2° Generation Immigrants")
    print(paste("Native mean = ", m1[3]))
    print(paste("Immig2 mean = ", m3[3]))
    print(paste("p-value: ", pval13[3]))
  }
  
  if(inf23[3]>0 || sup23[3]<0){
    print("1° Generation Immigrants - 2° Generation Immigrants")
    print(paste("Immig1 mean = ", m2[3]))
    print(paste("Immig2 mean = ", m3[3]))
    print(paste("p-value: ", pval23[3]))
  }
  
  ############## EDUCATIONAL RESOURCES
  er_scores[i] = m1[4] - mImm[4]
  print("EDUCATIONAL RESOURCES")
  if(inf12[4]>0 || sup12[4]<0){
    print("Native - 1° Generation Immigrants")
    print(paste("Native mean = ", m1[4]))
    print(paste("Immig1 mean = ", m2[4]))
    print(paste("p-value: ", pval12[4]))
  }
  
  if(inf13[2]>0 || sup13[2]<0){
    print("Native - 2° Generation Immigrants")
    print(paste("Native mean = ", m1[4]))
    print(paste("Immig2 mean = ", m3[4]))
    print(paste("p-value: ", pval13[4]))
  }
  
  if(inf23[4]>0 || sup23[4]<0){
    print("1° Generation Immigrants - 2° Generation Immigrants")
    print(paste("Immig1 mean = ", m2[4]))
    print(paste("Immig2 mean = ", m3[4]))
    print(paste("p-value: ", pval23[4]))
  }
  
  ############## HOME POSSESSIONS
  hp_scores[i] = m1[5] - mImm[5]
  print("HOME POSSESSIONS")
  if(inf12[5]>0 || sup12[5]<0){
    print("Native - 1° Generation Immigrants")
    print(paste("Native mean = ", m1[5]))
    print(paste("Immig1 mean = ", m2[5]))
    print(paste("p-value: ", pval12[5]))
  }
  
  if(inf13[5]>0 || sup13[5]<0){
    print("Native - 2° Generation Immigrants")
    print(paste("Native mean = ", m1[5]))
    print(paste("Immig2 mean = ", m3[5]))
    print(paste("p-value: ", pval13[5]))
  }
  
  if(inf23[5]>0 || sup23[5]<0){
    print("1° Generation Immigrants - 2° Generation Immigrants")
    print(paste("Immig1 mean = ", m2[5]))
    print(paste("Immig2 mean = ", m3[5]))
    print(paste("p-value: ", pval23[5]))
  }
}

cp_order = order(cp_scores)
fw_order = order(fw_scores)
es_order = order(es_scores)
er_order = order(er_scores)
hp_order = order(hp_scores)

cp_rank = name_countries[cp_order]
fw_rank = name_countries[fw_order]
es_rank = name_countries[es_order]
er_rank = name_countries[er_order]
hp_rank = name_countries[hp_order]


# GRAPHICAL VISUALIZATION ----
x11(width = 18, height = 15)
par(mfrow = c(3,2))
barplot(sort(cp_scores), names.arg = cp_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "CULTURAL POSSESSIONS", col = "red", cex.names = 0.9)
barplot(sort(fw_scores), names.arg = fw_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "FAMILY WEALTH", col = "blue", cex.names = 0.9)
barplot(sort(es_scores), names.arg = es_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "ESCS STATUS", col = "green", cex.names = 0.9)
barplot(sort(er_scores), names.arg = er_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "EDUCATIONAL RESOURCES", col = "gold", cex.names = 0.9)
barplot(sort(hp_scores), names.arg = hp_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "HOME POSSESSIONS", col = "pink", cex.names = 0.9)

## GRAPH WAS SAVED AS : barplot_wealth_countries

graphics.off()

