## SCRIPT TO EXTRACT ALL STUDENT DATA FROM ALL EUROPEAN COUNTRIES 

library(haven)

# Read OECD Pisa 2018 student file
student <- read_sas("cy07_msu_stu_qqq.sas7bdat")
dim(student)

# Extract all European Countries
ALB = student[which(student$CNTRYID==8),]
AUT = student[which(student$CNTRYID==40),]
BEL = student[which(student$CNTRYID==56),]
BIH = student[which(student$CNTRYID==70),]
BGR = student[which(student$CNTRYID==100),]
BLR = student[which(student$CNTRYID==112),]
HRV = student[which(student$CNTRYID==191),]
CZE = student[which(student$CNTRYID==203),]
DNK = student[which(student$CNTRYID==208),]
EST = student[which(student$CNTRYID==233),]
FIN = student[which(student$CNTRYID==246),]
FRA = student[which(student$CNTRYID==250),]
DEU = student[which(student$CNTRYID==276),]
GRC = student[which(student$CNTRYID==300),]
HUN = student[which(student$CNTRYID==348),]
ITA = student[which(student$CNTRYID==380),]
ISL = student[which(student$CNTRYID==352),]
KSV = student[which(student$CNTRYID==383),]
LVA = student[which(student$CNTRYID==428),]
LTU = student[which(student$CNTRYID==440),]
LUX = student[which(student$CNTRYID==442),]
MLT = student[which(student$CNTRYID==470),]
MDA = student[which(student$CNTRYID==498),]
MNE = student[which(student$CNTRYID==499),]
NDL = student[which(student$CNTRYID==528),]
NOR = student[which(student$CNTRYID==578),]
POL = student[which(student$CNTRYID==616),]
PRT = student[which(student$CNTRYID==620),]
ROU = student[which(student$CNTRYID==642),]
SRB = student[which(student$CNTRYID==688),]
SVK = student[which(student$CNTRYID==703),]
SVN = student[which(student$CNTRYID==705),]
ESP = student[which(student$CNTRYID==724),]
SWE = student[which(student$CNTRYID==752),]
CHE = student[which(student$CNTRYID==756),]
TUR = student[which(student$CNTRYID==792),]
UKR = student[which(student$CNTRYID==804),]
MKD = student[which(student$CNTRYID==807),]
GBR = student[which(student$CNTRYID==826),]

# List of countries
countries = list(ITA, ALB, AUT, BEL, BIH, BGR, BLR, HRV, CZE, DNK, EST, FIN, FRA, DEU, GRC, HUN, ISL, KSV, LVA,
                 LTU, LUX, MLT, MDA, MNE, NDL, NOR, POL, PRT, ROU, SRB, SVK, SVN, ESP, SWE, CHE, TUR, UKR, MKD, GBR)

# List of names of countries
name_countries = c('ITA', 'ALB', 'AUT', 'BEL', 'BIH', 'BGR', 'BLR', 'HRV', 'CZE', 'DNK', 'EST', 'FIN', 'FRA',
                   'DEU', 'GRC', 'HUN', 'ISL', 'KSV', 'LVA', 'LTU', 'LUX', 'MLT', 'MDA', 'MNE', 'NDL', 'NOR',
                   'POL', 'PRT', 'ROU', 'SRB', 'SVK', 'SVN', 'ESP', 'SWE', 'CHE', 'TUR', 'UKR', 'MKD', 'GBR')


## Selection of countries with enough immigrant data

# Initialization of parameters and vectors
n_countries = length(countries)    # 39
stud_country = rep(0,n_countries)
immig_stud_country = rep(0,n_countries)
immig_stud_1stgen_country = rep(0,n_countries)
immig_stud_2ndgen_country = rep(0,n_countries)

# Extract lengths and number of immigrant students per country (the variable IMMIG has 1 = native, 
# 2 = second generation immigrant, 3 = first generation immigrant)
for (i in 1:n_countries) {
  stud_country[i] = dim(countries[[i]])[1]  #notice: this number does not account for missing values
  immig_stud_country[i] = sum(na.omit(countries[[i]]$IMMIG==2 | countries[[i]]$IMMIG==3))
  immig_stud_1stgen_country[i] = sum(na.omit(countries[[i]]$IMMIG==3))
  immig_stud_2ndgen_country[i] = sum(na.omit(countries[[i]]$IMMIG==2))
}

# Plot
x11()
df_bar <- barplot(stud_country, main = "Students per Country - OECD Pisa 2018", ylab = "Number of students", xlab = "Countries",  
                  ylim = c(0,12000), names.arg = name_countries, las=2) #las=2 -> rotates names.arg
points(x = df_bar, y=immig_stud_country, col="red", pch=19)
points(x = df_bar, y=immig_stud_1stgen_country, col="blue", pch=19)
points(x = df_bar, y=immig_stud_2ndgen_country, col="purple", pch=19)
legend(n_countries/2, 12000, c("Immig stud total","First gen immig stud","Second gen immig stud"), 
       pch = c(19,19,19), col = c("red","blue","purple"), cex = 1)
abline(h=1000) 

# Select countries with enough sample size
countries_selected = countries[which(immig_stud_country>1000)]
name_countries_selected = name_countries[which(immig_stud_country>1000)]

# => we select 10 countries:
#    name_countries_selected = list("ITA", "AUT", "BEL", "DNK", "DEU",
#                                     "LUX", "ESP", "SWE", "CHE", "GBR")


# Write tables (already run):
# write.table(ITA,file='italy.txt')
# write.table(AUT,file='austria.txt')
# write.table(BEL,file='belgium.txt')
# write.table(DNK,file='denmark.txt')
# write.table(DEU,file='germany.txt')
# write.table(LUX,file='luxembourg.txt')
# write.table(ESP,file='spain.txt')
# write.table(SWE,file='sweden.txt')
# write.table(CHE,file='swiss.txt')
# write.table(GBR,file='greatbrit.txt')
