# Script to translate languages from id codes to categories (with respect to the belonging country): 
#0=country language; 1=dialect; 2=foreign language; 3=other

#Import datasets
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
ITA= read.table(file = "student_ita.txt", header = T)
AUT= read.table(file = "student_aut.txt", header = T)
BEL= read.table(file = "student_bel.txt", header = T)
DNK= read.table(file = "student_dnk.txt", header = T)
DEU= read.table(file = "student_deu.txt", header = T)
LUX= read.table(file = "student_lux.txt", header = T)
ESP= read.table(file = "student_esp.txt", header = T)
SWE= read.table(file = "student_swe.txt", header = T)
CHE= read.table(file = "student_che.txt", header = T)
GBR= read.table(file = "student_gbr.txt", header = T)

#Italy
table(ITA$language) #ITAlian: 200; dialetto: 611, 896; strano: 624, 826
ITA$language[which(ITA$language==200)] = 0;
ITA$language[which(ITA$language==611 + I(ITA$language==896))] = 1;
ITA$language[which(ITA$language==624 + I(ITA$language==826))] = 3;
ITA$language[which(ITA$language>4)] = 2;

#Austria
table(AUT$language) #german: 148; strano: 663, 802
AUT$language[which(AUT$language==148)] = 0;
AUT$language[which(AUT$language==663 + I(AUT$language==802))] = 3;
AUT$language[which(AUT$language>4)] = 2;

#Belgium
table(BEL$language) #dutch: 322; dialetto: 621; strano: 804
BEL$language[which(BEL$language==322)] = 0;
BEL$language[which(BEL$language==621)] = 1;
BEL$language[which(BEL$language==804)] = 3;
BEL$language[which(BEL$language>4)] = 2;

#Denmark
table(DNK$language) #danish: 264; strano: 265, 618, 813
DNK$language[which(DNK$language==264)] = 0;
DNK$language[which(DNK$language==265 + I(DNK$language==618) + I(DNK$language==813))] = 3;
DNK$language[which(DNK$language>4)] = 2;

#Germany
table(DEU$language) #german: 148; strano: 818
DEU$language[which(DEU$language==148)] = 0;
DEU$language[which(DEU$language==818)] = 3;
DEU$language[which(DEU$language>4)] = 2;

#Luxemburg
table(LUX$language) #luxembourgish: 507; strano: 834
LUX$language[which(LUX$language==507)] = 0;
LUX$language[which(LUX$language==834)] = 3;
LUX$language[which(LUX$language>4)] = 2;

#Spain
table(ESP$language) #spanish: 156; dialetto: 160, 474, 608, 668; strano: 852
ESP$language[which(ESP$language==156)] = 0;
ESP$language[which(ESP$language==160 + I(ESP$language==474) + I(ESP$language==608) + I(ESP$language==668))] = 1;
ESP$language[which(ESP$language==852)] = 3;
ESP$language[which(ESP$language>4)] = 2;

#Sweden
table(SWE$language) #swedish: 494; strano: 853
SWE$language[which(SWE$language==494)] = 0;
SWE$language[which(SWE$language==853)] = 3;
SWE$language[which(SWE$language>4)] = 2;

#Switzerland
table(CHE$language) #swiss german: 648; german:148; italian: 200; french: 493; dialetto: 133, 666; strano: 854
CHE$language[which(CHE$language==648 + I(CHE$language==148) + I(CHE$language==200) + I(CHE$language==493))] = 0;
CHE$language[which(CHE$language==133 + I(CHE$language==666))] = 1;
CHE$language[which(CHE$language==854)] = 3;
CHE$language[which(CHE$language>4)] = 2;

#Great Britain
table(GBR$language) #english: 313; welsh: 379, irish: 434; scots: 195; dialetto: 382, 626; strano: 605, 829, 848
GBR$language[which(GBR$language==313)] = 0;
GBR$language[which(GBR$language==195 + I(GBR$language==379) + I(GBR$language==434) + I(GBR$language==382) + I(GBR$language==626))] = 1;
GBR$language[which(GBR$language==605 + I(GBR$language==829) + I(GBR$language==848))] = 3;
GBR$language[which(GBR$language>4)] = 2

#Write results
#List of countries
countries = list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)

#List of names of countries
name_countries = c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
name_files = c('student_ita.txt', 'student_aut.txt', 'student_bel.txt', 'student_dnk.txt', 'student_deu.txt', 'student_lux.txt', 'student_esp.txt',
               'student_swe.txt', 'student_che.txt', 'student_gbr.txt')
n_countries = length(countries) 

for (i in 1:n_countries) {
  write.table(countries[[i]], file=name_files[i])
}
