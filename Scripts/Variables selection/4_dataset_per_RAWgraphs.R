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
EUR= read.table(file = "student_eur.txt", header = T)


write.table(ITA,file="student_ita.txt", row.names = FALSE, sep = ";") 
write.table(AUT,file="student_aut.txt", row.names = FALSE, sep = ";") 
write.table(BEL,file="student_bel.txt", row.names = FALSE, sep = ";") 
write.table(DNK,file="student_dnk.txt", row.names = FALSE, sep = ";") 
write.table(DEU,file="student_deu.txt", row.names = FALSE, sep = ";") 
write.table(LUX,file="student_lux.txt", row.names = FALSE, sep = ";") 
write.table(ESP,file="student_esp.txt", row.names = FALSE, sep = ";") 
write.table(SWE,file="student_swe.txt", row.names = FALSE, sep = ";") 
write.table(CHE,file="student_che.txt", row.names = FALSE, sep = ";") 
write.table(GBR,file="student_gbr.txt", row.names = FALSE, sep = ";") 
write.table(EUR,file="student_eur.txt", row.names = FALSE, sep = ";") 

#only immigrants
ITA$immigration[which(ITA$immigration==1)] = 0;
ITA$immigration[which(ITA$immigration==2 + I(ITA$immigration==3))] = 1;
ITA <- ITA[which(ITA$immigration==1),]
ITA <- ITA[,-3]

AUT$immigration[which(AUT$immigration==1)] = 0;
AUT$immigration[which(AUT$immigration==2 + I(AUT$immigration==3))] = 1;
AUT <- AUT[which(AUT$immigration==1),]
AUT <- AUT[,-3]

BEL$immigration[which(BEL$immigration==1)] = 0;
BEL$immigration[which(BEL$immigration==2 + I(BEL$immigration==3))] = 1;
BEL <- BEL[which(BEL$immigration==1),]
BEL <- BEL[,-3]

DNK$immigration[which(DNK$immigration==1)] = 0;
DNK$immigration[which(DNK$immigration==2 + I(DNK$immigration==3))] = 1;
DNK <- DNK[which(DNK$immigration==1),]
DNK <- DNK[,-3]

DEU$immigration[which(DEU$immigration==1)] = 0;
DEU$immigration[which(DEU$immigration==2 + I(DEU$immigration==3))] = 1;
DEU <- DEU[which(DEU$immigration==1),]
DEU <- DEU[,-3]

LUX$immigration[which(LUX$immigration==1)] = 0;
LUX$immigration[which(LUX$immigration==2 + I(LUX$immigration==3))] = 1;
LUX <- LUX[which(LUX$immigration==1),]
LUX <- LUX[,-3]

ESP$immigration[which(ESP$immigration==1)] = 0;
ESP$immigration[which(ESP$immigration==2 + I(ESP$immigration==3))] = 1;
ESP <- ESP[which(ESP$immigration==1),]
ESP <- ESP[,-3]

SWE$immigration[which(SWE$immigration==1)] = 0;
SWE$immigration[which(SWE$immigration==2 + I(SWE$immigration==3))] = 1;
SWE <- SWE[which(SWE$immigration==1),]
SWE <- SWE[,-3]

CHE$immigration[which(CHE$immigration==1)] = 0;
CHE$immigration[which(CHE$immigration==2 + I(CHE$immigration==3))] = 1;
CHE <- CHE[which(CHE$immigration==1),]
CHE <- CHE[,-3]

GBR$immigration[which(GBR$immigration==1)] = 0;
GBR$immigration[which(GBR$immigration==2 + I(GBR$immigration==3))] = 1;
GBR <- GBR[which(GBR$immigration==1),]
GBR <- GBR[,-3]

EUR$immigration[which(EUR$immigration==1)] = 0;
EUR$immigration[which(EUR$immigration==2 + I(EUR$immigration==3))] = 1;
EUR <- EUR[which(EUR$immigration==1),]
EUR <- EUR[,-3]

write.table(ITA,file="student_ita_immig.txt", row.names = FALSE, sep = ";") 
write.table(AUT,file="student_aut_immig.txt", row.names = FALSE, sep = ";") 
write.table(BEL,file="student_bel_immig.txt", row.names = FALSE, sep = ";") 
write.table(DNK,file="student_dnk_immig.txt", row.names = FALSE, sep = ";") 
write.table(DEU,file="student_deu_immig.txt", row.names = FALSE, sep = ";") 
write.table(LUX,file="student_lux_immig.txt", row.names = FALSE, sep = ";") 
write.table(ESP,file="student_esp_immig.txt", row.names = FALSE, sep = ";") 
write.table(SWE,file="student_swe_immig.txt", row.names = FALSE, sep = ";") 
write.table(CHE,file="student_che_immig.txt", row.names = FALSE, sep = ";") 
write.table(GBR,file="student_gbr_immig.txt", row.names = FALSE, sep = ";") 
write.table(EUR,file="student_eur_immig.txt", row.names = FALSE, sep = ";")
