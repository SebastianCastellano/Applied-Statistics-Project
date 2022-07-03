#Script to decide the countries we want to focus on

#Import countries data
ITA = read.table(file='italy.txt', header = T)
ALB = read.table(file='albania.txt', header = T)
AUT = read.table(file='austria.txt', header = T)
BEL = read.table(file='belgium.txt', header = T)
BIH = read.table(file='bosnia.txt', header = T)
BLR = read.table(file='belarus.txt', header = T)
BGR = read.table(file='bulgaria.txt', header = T)
HRV = read.table(file='croatia.txt', header = T)
CZE = read.table(file='czechrep.txt', header = T)
DNK = read.table(file='denmark.txt', header = T)
EST = read.table(file='estonia.txt', header = T)
FIN = read.table(file='finland.txt', header = T)
FRA = read.table(file='france.txt', header = T)
DEU = read.table(file='germany.txt', header = T)
GRC = read.table(file='greece.txt', header = T)
HUN = read.table(file='hungary.txt', header = T)
ISL = read.table(file='iceland.txt', header = T)
KSV = read.table(file='kosovo.txt', header = T)
LVA = read.table(file='latvia.txt', header = T)
LTU = read.table(file='lithuania.txt', header = T)
LUX = read.table(file='luxembourg.txt', header = T)
MLT = read.table(file='malta.txt', header = T)
MDA = read.table(file='moldova.txt', header = T)
MNE = read.table(file='montenegro.txt', header = T)
NDL = read.table(file='netherlands.txt', header = T)
NOR = read.table(file='norway.txt', header = T)
POL = read.table(file='poland.txt', header = T)
PRT = read.table(file='portugal.txt', header = T)
ROU = read.table(file='romania.txt', header = T)
SRB = read.table(file='serbia.txt', header = T)
SVK = read.table(file='slovakia.txt', header = T)
SVN = read.table(file='slovenia.txt', header = T)
ESP = read.table(file='spain.txt', header = T)
SWE = read.table(file='sweden.txt', header = T)
CHE = read.table(file='swiss.txt', header = T)
TUR = read.table(file='turkey.txt', header = T)
UKR = read.table(file='ukraine.txt', header = T)
MKD = read.table(file='northmac.txt', header = T)
GBR = read.table(file='greatbrit.txt', header = T)


#List of countries
countries = list(ITA, ALB, AUT, BEL, BIH, BGR, BLR, HRV, CZE, DNK, EST, FIN, FRA, DEU, GRC, HUN, ISL, KSV, LVA,
                 LTU, LUX, MLT, MDA, MNE, NDL, NOR, POL, PRT, ROU, SRB, SVK, SVN, ESP, SWE, CHE, TUR, UKR, MKD, GBR)

#List of names of countries
name_countries = c('ITA', 'ALB', 'AUT', 'BEL', 'BIH', 'BGR', 'BLR', 'HRV', 'CZE', 'DNK', 'EST', 'FIN', 'FRA',
                   'DEU', 'GRC', 'HUN', 'ISL', 'KSV', 'LVA', 'LTU', 'LUX', 'MLT', 'MDA', 'MNE', 'NDL', 'NOR',
                   'POL', 'PRT', 'ROU', 'SRB', 'SVK', 'SVN', 'ESP', 'SWE', 'CHE', 'TUR', 'UKR', 'MKD', 'GBR')


#Initialize params & vectors
n_countries = length(countries)  # there are 39 european countries
stud_country = rep(0,n_countries)
immig_stud_country = rep(0,n_countries)
immig_stud_1stgen_country = rep(0,n_countries)
immig_stud_2ndgen_country = rep(0,n_countries)

#Extract lengths and num of immig stud per country (the variable IMMIG has 1 = native, 2 = second gen, 3 = first gen)
for (i in 1:n_countries) {
  stud_country[i] = dim(countries[[i]])[1]  #notice: this number does not account for missing values
  immig_stud_country[i] = sum(na.omit(countries[[i]]$IMMIG==2 | countries[[i]]$IMMIG==3))
  immig_stud_1stgen_country[i] = sum(na.omit(countries[[i]]$IMMIG==3))
  immig_stud_2ndgen_country[i] = sum(na.omit(countries[[i]]$IMMIG==2))
}

#Plot
x11()
df_bar <- barplot(stud_country, main = "Students per Country - OECD Pisa 2018", ylab = "Number of students", xlab = "Countries",  
                  ylim = c(0,12000), names.arg = name_countries, las=2) #las=2 -> rotates names.arg
points(x = df_bar, y=immig_stud_country, col="red", pch=19)
points(x = df_bar, y=immig_stud_1stgen_country, col="blue", pch=19)
points(x = df_bar, y=immig_stud_2ndgen_country, col="purple", pch=19)
legend(n_countries/2, 12000, c("Immig stud total","First gen immig stud","Second gen immig stud"), 
       pch = c(19,19,19), col = c("red","blue","purple"), cex = 1)
abline(h=1000) 

#Select countries with enough sample size
countries_selected = countries[which(immig_stud_country>1000)]
name_countries_selected = name_countries[which(immig_stud_country>1000)]
#We select 10 countries:
# name_countries_selected = list("ITA", "AUT", "BEL", "DNK", "DEU",
#                                  "LUX", "ESP", "SWE", "CHE", "GBR")

