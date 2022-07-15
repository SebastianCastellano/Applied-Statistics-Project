#RANKING OF COUNTRIES BASED ON MATH, SCIE, READ PERFORMANCE (all students)

setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
EUR <- read.table(file = "student_eur.txt", header = T)
name_countries <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "GBR", "ITA", "LUX", "SWE")
n_countries <- length(name_countries)

#a) Considering all students
X.values <- data.frame(math=EUR$math, scie=EUR$scie, read=EUR$read)
X.groups <- as.factor(EUR$country)

mean_countries <- aggregate(X.values,list(Country=X.groups),mean) #mean of scie,scie,read in each country

idxOrder_math= order(mean_countries$math)
meanOrder_math = sort(mean_countries$math)
nameOrder_math = levels(X.groups)[idxOrder_math] #re-order the labels
x11()
barplot(round(meanOrder_math), names.arg = nameOrder_math, xlab = 'Countries', 
        ylab = 'Mean math Score', ylim = c(0,600), main = 'Math Score Ranking - All Students', col = 'red',
        las=2)

idxOrder_scie = order(mean_countries$scie)
meanOrder_scie = sort(mean_countries$scie)
nameOrder_scie = levels(X.groups)[idxOrder_scie] #re-order the labels
x11()
barplot(round(meanOrder_scie), names.arg = nameOrder_scie, xlab = 'Countries', 
        ylab = 'Mean scie Score', ylim = c(0,600), main = 'Scie Score Ranking - All Students', col = 'green3',
        las=2)

idxOrder_read = order(mean_countries$read)
meanOrder_read = sort(mean_countries$read)
nameOrder_read = levels(X.groups)[idxOrder_read] #re-order the labels
x11()
barplot(round(meanOrder_read), names.arg = nameOrder_read, xlab = 'Countries', 
        ylab = 'Mean read Score', ylim = c(0,600), main = 'Read Score Ranking - All Students', col = 'blue',
        las=2)

# X = data.frame('scie' = EUR$scie, 'country' = as.factor(EUR$country))
# Y = data.frame()
# for (name in nameOrder)
# {
#   Y = rbind(Y, X[X$country==name,])
# }
# levels(Y$country)<- idxOrder_scie
# 
# x11()
# boxplot(Y$scie~Y$country,xlab=nameOrder, col = 'gold',las=2)


#b) Ranking of countries based on mean scie score (IMMIGRANTS students only)
EUR_imm = EUR[EUR$immigration!=1,]
X.values.imm <- data.frame(math=EUR_imm$math, scie=EUR_imm$scie, read=EUR_imm$read)
X.groups.imm <- as.factor(EUR_imm$country)

mean_countries <- aggregate(X.values.imm,list(Country=X.groups.imm),mean) #mean of scie,scie,read in each country

idxOrder_math_immig= order(mean_countries$math)
meanOrder = sort(mean_countries$math)
nameOrder = levels(X.groups)[idxOrder_math] #re-order the labels

x11()
barplot(round(meanOrder), names.arg = nameOrder, xlab = 'Countries', 
        ylab = 'Mean math Score', ylim = c(0,600), main = 'Math Score Ranking - Immig Students', col = 'red',
        las=2)

idxOrder_scie_immig = order(mean_countries$scie)
meanOrder = sort(mean_countries$scie)
nameOrder = levels(X.groups)[idxOrder_scie] #re-order the labels

x11()
barplot(round(meanOrder), names.arg = nameOrder, xlab = 'Countries', 
        ylab = 'Mean scie Score', ylim = c(0,600), main = 'Scie Score Ranking - Immig Students', col = 'green3',
        las=2)

idxOrder_read_immig = order(mean_countries$read)
meanOrder = sort(mean_countries$read)
nameOrder = levels(X.groups)[idxOrder_read] #re-order the labels

x11()
barplot(round(meanOrder), names.arg = nameOrder, xlab = 'Countries', 
        ylab = 'Mean read Score', ylim = c(0,600), main = 'Read Score Ranking - Immig Students', col = 'blue',
        las=2)

#c) Anova to see if the differences in mean between countries are significant
# Math - All students
for (i in 1:(n_countries-1)){
X1.math <- EUR[which(EUR$country==nameOrder_math[i] | EUR$country==nameOrder_math[i+1]),]$math
X1.country<- EUR[which(EUR$country==nameOrder_math[i] | EUR$country==nameOrder_math[i+1]),]$country
aov.mod <- aov(X1.math ~ X1.country)
print(paste(nameOrder_math[i]," and ",nameOrder_math[i+1]))
print(summary(aov.mod)[[1]][["Pr(>F)"]][1]) #extract p-value
} #from this we see that at level 95% (simultaneous) DEU,CHE and GBR have the same mean
#Therefore we can update our ranking and graphs:
meanOrder_math_updated <- meanOrder_math[1:8]
meanOrder_math_updated[8] <- mean(EUR$math[which(EUR$country=="DEU"|EUR$country=="CHE"|EUR$country=="GBR")])
nameOrder_math_updated <- nameOrder_math[1:8]
nameOrder_math_updated[8] <- "DEU, CHE, GBR"
x11()
par(mar=c(8,4,4,2))
barplot(round(meanOrder_math_updated), names.arg = nameOrder_math_updated, xlab = 'Countries', 
        ylab = 'Mean math Score', ylim = c(0,600), main = 'Math Score Ranking - All Students - After ANOVA', col = 'red',
        las=2)

# Scie - All students
for (i in 1:(n_countries-1)){
        X1.scie <- EUR[which(EUR$country==nameOrder_scie[i] | EUR$country==nameOrder_scie[i+1]),]$scie
        X1.country<- EUR[which(EUR$country==nameOrder_scie[i] | EUR$country==nameOrder_scie[i+1]),]$country
        aov.mod <- aov(X1.scie ~ X1.country)
        print(paste(nameOrder_scie[i]," and ",nameOrder_scie[i+1]))
        print(summary(aov.mod)[[1]][["Pr(>F)"]][1]) #extract p-value
} #from this we see that at level 95% (simultaneous) ITA,LUX,DNK,CHE,ESP,AUT have the same mean, and also BEL,GBR,DEU
#Therefore we can update our ranking and graphs:
meanOrder_scie_updated <- meanOrder_scie[c(1,7,8)]
meanOrder_scie_updated[1] <- mean(EUR$scie[which(EUR$country==nameOrder_scie[1]|EUR$country==nameOrder_scie[2]|
                                                 EUR$country==nameOrder_scie[3]|EUR$country==nameOrder_scie[4]|
                                                 EUR$country==nameOrder_scie[5]|EUR$country==nameOrder_scie[6])])
meanOrder_scie_updated[3] <- mean(EUR$scie[which(EUR$country==nameOrder_scie[8]|EUR$country==nameOrder_scie[9]|
                                                 EUR$country==nameOrder_scie[10])])
nameOrder_scie_updated <- nameOrder_scie[c(1,7,8)]
nameOrder_scie_updated[1] <- "ITA,LUX,DNK,CHE,ESP,AUT"
nameOrder_scie_updated[3] <- "BEL,GBR,DEU"
x11()
par(mar=c(10,4,4,2))
barplot(round(meanOrder_scie_updated), names.arg = nameOrder_scie_updated, xlab = 'Countries', 
        ylab = 'Mean Scie Score', ylim = c(0,600), main = 'Science Ranking - All Students - After ANOVA', col = 'green3',
        las=1)

# Read - All students
for (i in 1:(n_countries-1)){
        X1.read <- EUR[which(EUR$country==nameOrder_read[i] | EUR$country==nameOrder_read[i+1]),]$read
        X1.country<- EUR[which(EUR$country==nameOrder_read[i] | EUR$country==nameOrder_read[i+1]),]$country
        aov.mod <- aov(X1.read ~ X1.country)
        print(paste(nameOrder_read[i]," and ",nameOrder_read[i+1]))
        print(summary(aov.mod)[[1]][["Pr(>F)"]][1]) #extract p-value
} #from this we see that at level 95% (simultaneous) ITA,CHE,ESP have the same mean, and also AUT,DEN and also SWE,GBR,DEU
#Therefore we can update our ranking and graphs:
meanOrder_read_updated <- meanOrder_read[c(1,2,5,7,8)]
meanOrder_read_updated[2] <- mean(EUR$read[which(EUR$country==nameOrder_read[2]|EUR$country==nameOrder_read[3]|
                                                         EUR$country==nameOrder_read[4])])
meanOrder_read_updated[3] <- mean(EUR$read[which(EUR$country==nameOrder_read[5]|EUR$country==nameOrder_read[6])])
meanOrder_read_updated[5] <- mean(EUR$read[which(EUR$country==nameOrder_read[8]|EUR$country==nameOrder_read[9]|
                                                         EUR$country==nameOrder_read[10])])
nameOrder_read_updated <- nameOrder_read[c(1,2,5,7,8)]
nameOrder_read_updated[2] <- "ITA,CHE,ESP"
nameOrder_read_updated[3] <- "AUT,DEN"
nameOrder_read_updated[5] <- "SWE,GBR,DEU"
x11()
par(mar=c(4,4,4,2))
barplot(round(meanOrder_read_updated), names.arg = nameOrder_read_updated, xlab = 'Countries', 
        ylab = 'Mean Reading Score', ylim = c(0,600), main = 'Reading Ranking - All Students - After ANOVA', col = 'blue',
        las=1, cex.names=0.9)