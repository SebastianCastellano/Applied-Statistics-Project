
# NEW FEATURES EXPLORATION
# Parent Help: immig vs Native

setwd("D:/APPLIED/GITHUB/Applied-Statistics-Project/txt - files/stud_school_features/")

fileName = 'italy.txt'

fullData = read.table(fileName, header = TRUE)

# Inserire il codice indentificativo delle variabili di interesse (as strings)
VARIABLES_ID = c('PV3MATH','IMMIG', 'PA003Q04HA')

# Inserire il nome delle variabili di interesse (as strings)
VARIABLES_NAMES = c('math','immig', 'parent_help')


newData = na.omit(fullData[,VARIABLES_ID])
names(newData) = VARIABLES_NAMES

Xn = newData[newData$immig == 1,]
Xi = newData[newData$immig != 1,]

mN = mean(Xn$parent_help)
mI = mean(Xi$parent_help)


fit = lm(log(math) ~ parent_help, data = newData)
summary(fit)


x11()
plot(Xi$math, Xi$parent_help)



