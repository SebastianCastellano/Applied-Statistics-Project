# SCRIPT TO VISUALIZE DATA - Italy 
# without science

# Load data-frame
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
ita = read.table(file = "student_ita.txt", header = T)
n_cols = dim(ita)[2]

attach(ita) 

math_immig = ita[which(ita$immigration==2 | ita$immigration==3),]$math
read_immig = ita[which(ita$immigration==2 | ita$immigration==3),]$read
math_native = ita[which(ita$immigration==1),]$math
read_native = ita[which(ita$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="ITALY - Native vs Immigrant Scores", xlab="math, eading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(ita[which(ita$immigration==2 | ita$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(ita)


## AUSTRIA ##

aut = read.table(file = "student_aut.txt", header = T)
n_cols = dim(aut)[2]

attach(aut) 

math_immig = aut[which(aut$immigration==2 | aut$immigration==3),]$math
read_immig = aut[which(aut$immigration==2 | aut$immigration==3),]$read
math_native = aut[which(aut$immigration==1),]$math
read_native = aut[which(aut$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="AUSTRIA - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(aut[which(aut$immigration==2 | aut$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(aut)


## BELGIUM ##

bel = read.table(file = "student_bel.txt", header = T)
n_cols = dim(bel)[2]

attach(bel)  

math_immig = bel[which(bel$immigration==2 | bel$immigration==3),]$math
read_immig = bel[which(bel$immigration==2 | bel$immigration==3),]$read
math_native = bel[which(bel$immigration==1),]$math
read_native = bel[which(bel$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="BELGIUM - Native vs Immigrant Scores", xlab="math,reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(aut[which(bel$immigration==2 | bel$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(bel)


## DENMARK ##

dnk = read.table(file = "student_dnk.txt", header = T)
n_cols = dim(dnk)[2]

attach(dnk) 

math_immig = dnk[which(dnk$immigration==2 | dnk$immigration==3),]$math
read_immig = dnk[which(dnk$immigration==2 | dnk$immigration==3),]$read
math_native = dnk[which(dnk$immigration==1),]$math
read_native = dnk[which(dnk$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="DENMARK - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(dnk[which(bel$immigration==2 | dnk$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(dnk)


## GERMANY ##

deu = read.table(file = "student_deu.txt", header = T)
n_cols = dim(deu)[2]

attach(deu)  

math_immig = deu[which(deu$immigration==2 | deu$immigration==3),]$math
read_immig = deu[which(deu$immigration==2 | deu$immigration==3),]$read
math_native = deu[which(deu$immigration==1),]$math
read_native = deu[which(deu$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="GERMANY - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(deu[which(bel$immigration==2 | deu$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(deu)


## LUXEMBOURG ##

lux = read.table(file = "student_lux.txt", header = T)
n_cols = dim(lux)[2]

attach(lux)  

math_immig = lux[which(lux$immigration==2 | lux$immigration==3),]$math
read_immig = lux[which(lux$immigration==2 | lux$immigration==3),]$read
math_native = lux[which(lux$immigration==1),]$math
read_native = lux[which(lux$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="LUXEMBOURG - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(lux[which(bel$immigration==2 | lux$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(lux)


## SPAIN ##

esp = read.table(file = "student_esp.txt", header = T)
n_cols = dim(esp)[2]

attach(esp)  

math_immig = esp[which(esp$immigration==2 | esp$immigration==3),]$math
read_immig = esp[which(esp$immigration==2 | esp$immigration==3),]$read
math_native = esp[which(esp$immigration==1),]$math
read_native = esp[which(esp$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="SPAIN - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(esp[which(bel$immigration==2 | esp$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(esp)


## SWEDEN ##

swe = read.table(file = "student_swe.txt", header = T)
n_cols = dim(swe)[2]

attach(swe) 

math_immig = swe[which(swe$immigration==2 | swe$immigration==3),]$math
read_immig = swe[which(swe$immigration==2 | swe$immigration==3),]$read
math_native = swe[which(swe$immigration==1),]$math
read_native = swe[which(swe$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="SWEDEN - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(swe[which(bel$immigration==2 | swe$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(swe)


## SWISS ##

che = read.table(file = "student_che.txt", header = T)
n_cols = dim(che)[2]

attach(che) 

math_immig = che[which(che$immigration==2 | che$immigration==3),]$math
read_immig = che[which(che$immigration==2 | che$immigration==3),]$read
math_native = che[which(che$immigration==1),]$math
read_native = che[which(che$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="SWISS - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(che[which(bel$immigration==2 | che$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(che)


## GREAT BRITAIN ##

gbr = read.table(file = "student_gbr.txt", header = T)
n_cols = dim(gbr)[2]

attach(gbr)  

math_immig = gbr[which(gbr$immigration==2 | gbr$immigration==3),]$math
read_immig = gbr[which(gbr$immigration==2 | gbr$immigration==3),]$read
math_native = gbr[which(gbr$immigration==1),]$math
read_native = gbr[which(gbr$immigration==1),]$read
color = c('lightgreen', 'pink', 'lightgreen', 'pink')
x11()
boxplot(main="GREATBRIT - Native vs Immigrant Scores", xlab="math, reading tests", ylab = "scores", math_native, math_immig, read_native, read_immig, col=color)

num_immig = dim(gbr[which(gbr$immigration==2 | gbr$immigration==3),])[1]
num_native = length(math_native)
x = runif(num_immig,1,num_native)
x11()
plot(math_immig, read_immig, col="blue")
points(math_native[x], read_native[x], col="red")

detach(gbr)

