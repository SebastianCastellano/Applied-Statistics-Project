############################# MANOVA COUNTRIES #################################
# Differences among countries

setwd("D:/APPLIED/PROGETTO/DATA")

EUR <- read.table(file = "student_eur.txt", header = T)

countries = as.factor(EUR$country)

name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
n_countries <- length(name_countries)

X = data.frame(EUR$math)
names(X) = "Math"

fit = aov(as.matrix(X) ~ countries)

studCountr = rep(0,n_countries)
nStud = dim(EUR)[1]
Spooled = 0

for (i in 1:n_countries)
{
  studCountr[i] = sum(EUR$country == name_countries[i])
  Spooled = Spooled + var(EUR$math[EUR$country == name_countries[i]])*(studCountr[i]-1)/(nStud-n_countries)
}

mIta = mean(EUR$math[EUR$country== "ITA"])
nIta = studCountr[1]
centers = rep(0,n_countries-1)
intLength = rep(0,n_countries-1)

p=1
alpha = 0.05

for(i in 2:n_countries)
{
  centers[i-1] = mean(EUR$math[EUR$country== name_countries[i]]) - mIta 
  intLength[i-1] = sqrt( (nStud-n_countries)*p/(nStud-n_countries-p+1)*qf(1-alpha,p,nStud-n_countries-p+1)
                    *Spooled*(1/nIta + 1/studCountr[i]))
}

x11()
plot(centers, xaxt = "n", pch = 19, ylim = c(-10,35), 
     xlab = "Countries", ylab = "CI 95% Diff of Mean", main = "Math Score of All Students (vs Italy)")
points(centers+intLength, pch = 19)
points(centers-intLength, pch = 19)
axis(1,at = seq(1,n_countries-1) ,labels = name_countries[2:n_countries])
abline(h=0)
for(i in 1:(n_countries-1)){
  lines(c(i,i), c(centers[i]-intLength[i],centers[i]+intLength[i]), col = "red");
}

graphics.off()

##########################################################################################################

# ON IMMIGRANTS


EUR_imm = EUR[EUR$immigration!=1,]
countries = as.factor(EUR_imm$country)

name_countries <- c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
n_countries <- length(name_countries)

X = data.frame(EUR_imm$math)
names(X) = "Math"

fit = aov(as.matrix(X) ~ countries)

studCountr = rep(0,n_countries)
nStud = dim(EUR)[1]
Spooled = 0

for (i in 1:n_countries)
{
  studCountr[i] = sum(EUR_imm$country == name_countries[i])
  Spooled = Spooled + var(EUR_imm$math[EUR_imm$country == name_countries[i]])*(studCountr[i]-1)/(nStud-n_countries)
}

mIta = mean(EUR_imm$math[EUR_imm$country== "ITA"])
nIta = studCountr[1]
centers = rep(0,n_countries-1)
intLength = rep(0,n_countries-1)

p=1
alpha = 0.05

for(i in 2:n_countries)
{
  centers[i-1] = mean(EUR_imm$math[EUR_imm$country== name_countries[i]]) - mIta 
  intLength[i-1] = sqrt( (nStud-n_countries)*p/(nStud-n_countries-p+1)*qf(1-alpha,p,nStud-n_countries-p+1)
                         *Spooled*(1/nIta + 1/studCountr[i]))
}

x11()
plot(centers, xaxt = "n", pch = 19, ylim = c(-25,45), 
     xlab = "Countries", ylab = "CI 95% Diff of Mean", main = "Math Score of Immigrants (vs Italy)")
points(centers+intLength, pch = 19)
points(centers-intLength, pch = 19)
axis(1,at = seq(1,n_countries-1) ,labels = name_countries[2:n_countries])
abline(h=0)
for(i in 1:(n_countries-1)){
  lines(c(i,i), c(centers[i]-intLength[i],centers[i]+intLength[i]), col = "red");
}


#########################################################################################################

# RANKING OF COUNTRIES BASED ON MATH PERFORMANCE

EUR <- read.table(file = "student_eur.txt", header = T)


mean_countries = rep(0,n_countries)


for(i in 1:n_countries)
{
  currName = levels(countries)[i]
  mean_countries[i] = mean(EUR$math[EUR$country==currName])
}

idxOrder = order(mean_countries)
meanOrder = sort(mean_countries)
nameOrder = levels(countries)[idxOrder]

x11()
barplot(round(meanOrder), names.arg = nameOrder, xlab = 'Countries', 
        ylab = 'Mean Math Score', ylim = c(0,600), main = 'Math Score Ranking', col = 'gold')


X = data.frame('math' = EUR$math, 'country' = as.factor(EUR$country))
Y = data.frame()
for (name in nameOrder)
{
  Y = rbind(Y, X[X$country==name,])
}

x11()
boxplot(math ~  country, Y, col = 'gold')

#---------------------------------------------------------
EUR_imm = EUR[EUR$immigration!=1,]

mean_countries_imm = rep(0,n_countries)


for(i in 1:n_countries)
  
{
  currName = levels(countries)[i]
  mean_countries_imm[i] = mean(EUR_imm$math[EUR_imm$country==currName])
}

idxOrder_imm = order(mean_countries_imm)
meanOrder_imm = sort(mean_countries_imm)
nameOrder_imm = levels(countries)[idxOrder_imm]

x11()
barplot(round(meanOrder_imm), names.arg = nameOrder_imm, xlab = 'Countries', 
        ylab = 'Mean Math Score', ylim = c(0,600), main = 'Math Score Ranking of Immigrants', col = 'gold')

