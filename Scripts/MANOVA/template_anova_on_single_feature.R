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

# MODIFY BELOW BASE ON SELECTED VARIABLE
variable <- "Belonging" #for print purposes
variable_scores <- rep(0, n_countries) #will fill with native-immig mean scores for each country

for (i in 1:n_countries) {
  
  # BUILD DATAFRAMES ----
  
  X = data.frame(countries[[i]]$belonging) # Change this according to variable of interest
  names(X) = c("belonging")
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
  X_imm = X[-nativeIndex,] #1st gen and 2nd gen aggregate
  
  #Print to identify country
  print("________________________________________________________________________________________________")
  print(paste("                               ", name_countries[i]))
  
  
  # # HYPOTHESIS CHECK ---- 
  # nshap <- 5000
  # p_native = shapiro.test(X_native[sample(min(nshap,n_native))])$p
  # p_imm1 = mcshapiro.test(X_imm1)$p
  # p_imm2 = mcshapiro.test(X_imm2)$p
  # # 
  # if(p_native<0.05 || p_imm1<0.05 || p_imm2<0.05)
  #    print("** normality not guarenteed **")
  # #check homogeneity assumptions ...
  
  # MANOVA ----
  n1 <- length(X_native)
  n2 <- length(X_imm1)
  n3 <- length(X_imm2)
  n  <- n1+n2+n3
  g  <- 3
  p  <- 1
  
  immig.status <- factor(countries[[i]]$immigration, labels=c('native','immig2','immig1'))
  fit <- aov(as.matrix(X) ~ immig.status)
  p_val <- summary(fit)[[1]][["Pr(>F)"]][1]
  print(paste("Anova pvalue = ", p_val))
  
  alpha <- 0.05
  k <- p*g*(g-1)/2*5 # the *10 term is added since we assume that we will do around 10 different features check in total
  qT <- qt(1-alpha/(2*k), n-g)
  
  W <- sum(fit$residuals^2)
  m  <- sapply(X,mean)         # estimates mu
  m1 <- mean(X_native)    # estimates mu.1=mu+tau.1
  m2 <- mean(X_imm1)    # estimates mu.2=mu+tau.2
  m3 <- mean(X_imm2)# estimates mu.3=mu+tau.3
  mImm <- mean(X_imm)
  
  #Intervals for native vs immig 1
  inf12 <- m1-m2 - qT * sqrt( W/(n-g) * (1/n1+1/n2) )
  sup12 <- m1-m2 + qT * sqrt( W/(n-g) * (1/n1+1/n2) )
  pval12 <- (1- pt(abs(m1-m2)/sqrt( W/(n-g) * (1/n1+1/n2) ),n-g))*(2*k)
  
  #Intervals for native vs immig 1
  inf13 <- m1-m3 - qT * sqrt( W/(n-g) * (1/n1+1/n3) )
  sup13 <- m1-m3 + qT * sqrt( W/(n-g) * (1/n1+1/n3) )
  pval13 <- (1- pt(abs(m1-m3)/sqrt( W/(n-g) * (1/n1+1/n3) ),n-g))*(2*k)
  
  #Intervals for native vs immig 1
  inf23 <- m2-m3 - qT * sqrt( W/(n-g) * (1/n2+1/n3) )
  sup23 <- m2-m3 + qT * sqrt( W/(n-g) * (1/n2+1/n3) )
  pval23 <- (1- pt(abs(m2-m3)/sqrt( W/(n-g) * (1/n2+1/n3) ),n-g))*(2*k)
  
  
  # PRINT RESULTS ----
  
  print(variable)
  variable_scores[i] = m1[1] - mImm[1]
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
}

variable_order <- order(variable_scores)
variable_rank = name_countries[variable_order]

# GRAPHICAL VISUALIZATION ----
x11()
par(las=2)
barplot(sort(variable_scores), names.arg = variable_rank, xlab ="Countries",
        ylab = "Native vs IMM", main = variable, col = "blue")


