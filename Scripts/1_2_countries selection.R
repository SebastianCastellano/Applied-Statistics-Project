#Script to extract all student data from all european countries

#Import libraries
#library(sas7bdat)  #Prof Masci library - didn't work anymore
#student = read.sas7bdat(file='cy07_msu_stu_qqq.sas7bdat')
install.packages("haven")
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

#Write tables
write.table(ITA,file='italy.txt')
write.table(ALB,file='albania.txt')
write.table(AUT,file='austria.txt')
write.table(BEL,file='belgium.txt')
write.table(BGR,file='bulgaria.txt')
write.table(BIH,file='bosnia.txt')
write.table(BLR,file='belarus.txt')
write.table(HRV,file='croatia.txt')
write.table(CZE,file='czechrep.txt')
write.table(DNK,file='denmark.txt')
write.table(EST,file='estonia.txt')
write.table(FIN,file='finland.txt')
write.table(FRA,file='france.txt')
write.table(DEU,file='germany.txt')
write.table(GRC,file='greece.txt')
write.table(HUN,file='hungary.txt')
write.table(ISL,file='iceland.txt')
write.table(KSV,file='kosovo.txt')
write.table(LVA,file='latvia.txt')
write.table(LTU,file='lithuania.txt')
write.table(LUX,file='luxembourg.txt')
write.table(MLT,file='malta.txt')
write.table(MDA,file='moldova.txt')
write.table(MNE,file='montenegro.txt')
write.table(NDL,file='netherlands.txt')
write.table(NOR,file='norway.txt')
write.table(POL,file='poland.txt')
write.table(PRT,file='portugal.txt')
write.table(ROU,file='romania.txt')
write.table(SRB,file='serbia.txt')
write.table(SVK,file='slovakia.txt')
write.table(SVN,file='slovenia.txt')
write.table(ESP,file='spain.txt')
write.table(SWE,file='sweden.txt')
write.table(CHE,file='swiss.txt')
write.table(TUR,file='turkey.txt')
write.table(UKR,file='ukraine.txt')
write.table(MKD,file='northmac.txt')
write.table(GBR,file='greatbrit.txt')

#In script countries_selection.R we proceed with selecting the subset of countries to use in our analysis


