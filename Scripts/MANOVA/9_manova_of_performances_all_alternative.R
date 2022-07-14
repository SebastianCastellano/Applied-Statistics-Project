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
LUX <-read.table(file = "student_lux.txt", header = T)
ESP <- read.table(file = "student_esp.txt", header = T)
SWE <- read.table(file = "student_swe.txt", header = T)
CHE <- read.table(file = "student_che.txt", header = T)
GBR <- read.table(file = "student_gbr.txt", header = T)
EUR <- read.table(file = "student_eur.txt", header = T)


countries <- list(AUT, BEL, CHE, DEU, DNK, ESP, GBR, ITA, LUX, SWE, EUR)
name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE", "EUR")
n_countries <- length(countries) 

variables <- c("CULTURAL POSSESSIONS", "FAMILY WEALTH", "ESCS STATUS", 
               "EDUCATIONAL RESOURCES", "HOME POSSESSIONS")

m_scores <- rep(0, n_countries)
s_scores <- rep(0, n_countries)
r_scores <- rep(0, n_countries)

for (i in 1:n_countries) {
  
  # BUILD DATAFRAMES ----
  
  X = data.frame(countries[[i]]$math, countries[[i]]$scie,  countries[[i]]$read)
  names(X) = c("math","scie", "read")
  immStatus = countries[[i]]$immigration
  
  ## Extract Indeces
  nativeIndex = which(immStatus==1)
  n_native = length(nativeIndex)
  
  immIndex_1 = which(immStatus==3)
  nImm_1 = length(immIndex_1)
  
  immIndex_2 = which(immStatus==2)
  nImm_2 = length(immIndex_2)
  
  # immIndex = which(immStatus==2 + I(immStatus==3))
  # nImm = length(immIndex)
  
  ## Build dataframe
  X_native = X[nativeIndex,]
  X_imm1 = X[immIndex_1,]
  X_imm2 = X[immIndex_2,]
  
  X_imm = X[-nativeIndex,]
  
  print("________________________________________________________________________________________________")
  print(paste("                                            ", name_countries[i]))
  
  
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
  # n4 <- dim(X_imm)[1]
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
  
  
  
  ############## MATH
  print("MATH")
  m_scores[i] = (m1[1] - mImm[1])/m1[1]
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
  

  ############## SCIENCE
  s_scores[i] = m1[2] - mImm[2]
  print("SCIENCE")
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
  
  
  ############## READING
  r_scores[i] = m1[3] - mImm[3]
  print("READING")
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
  
}

m_order = order(m_scores) # dovremmo normalizzare in modo da avere una differenza percentuale?
s_order = order(s_scores)
r_order = order(r_scores)


m_rank = name_countries[m_order]
s_rank = name_countries[s_order]
r_rank = name_countries[r_order]

# GRAPHICAL VISUALIZATION ----
x11(width = 18, height = 15)
par(mfrow = c(3,1))
barplot(sort(m_scores), names.arg = m_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "MATH", col = "red")
barplot(sort(s_scores), names.arg = s_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "SCIENCE", col = "green2")
barplot(sort(r_scores), names.arg = r_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = "READING", col = "blue")


## GRAPH WAS SAVED AS : barplot_performances_countries

graphics.off()


  
 #  print("Math:")
 #  if(inf12[1]<0 && sup12[1]>0)
 #    print(paste("Native vs immig1 NO significative difference, pval = ",pval12[1]))
 #  if(inf13[1]<0 && sup13[1]>0)
 #    print(paste("Native vs immig2 NO significative difference, pval = ",pval13[1]))
 #  if(inf23[1]<0 && sup23[1]>0)
 #    print(paste("Immig1 vs immig2 NO significative difference, pval = ",pval23[1]))
 #  
 #  print("Science:")
 #  if(inf12[2]<0 && sup12[2]>0)
 #    print(paste("Native vs immig1 not significative, pval = ",pval12[2]))
 #  if(inf13[2]<0 && sup13[2]>0)
 #    print(paste("Native vs immig2 not significative, pval = ",pval13[2]))
 #  if(inf23[2]<0 && sup23[2]>0)
 #    print(paste("Immig1 vs immig2 not significative, pval = ",pval23[2]))
 #  
 #  print("Reading:")
 #  if(inf12[3]<0 && sup12[3]>0)
 #    print(paste("Native vs immig1 not significative, pval = ",pval12[3]))
 #  if(inf13[3]<0 && sup13[3]>0)
 #    print(paste("Native vs immig2 not significative, pval = ",pval13[3]))
 #  if(inf23[3]<0 && sup23[3]>0)
 #    print(paste("Immig1 vs immig2 not significative, pval = ",pval23[3]))
 #  
 # }
















