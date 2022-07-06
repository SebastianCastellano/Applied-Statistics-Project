
# This script creates new txt-file with the desired features
# N.B. Il dataset è filtrato dagli na quindi potrebbe avere un numero ridotto di osservazioni


setwd("D:/APPLIED/GITHUB/Applied-Statistics-Project/txt - files/European countries raw")

fileName = 'italy.txt'

fullData = read.table(fileName, header = TRUE)

# Inserire il codice indentificativo delle variabili di interesse (as strings)
VARIABLES_ID = c('CNT', 'ST006Q01TA')

# Inserire il nome delle variabili di interesse (as strings)
VARIABLES_NAMES = c('var1', 'var2')

newData = fullData[,VARIABLES_ID]
names(newData) = VARIABLES_NAMES


# Nome file txt da creare
outputName ='newIta.txt'

write.table(na.omit(newData), file= outputName)
