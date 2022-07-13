
# NEW FEATURES EXPLORATION
# The purpose of this script is to see if there is significant difference between Native 
# and immigrant students (anova of each variable of interests)

# Folder Containing RAW Datasets
setwd("D:/APPLIED/GITHUB/Applied-Statistics-Project/txt - files/European countries raw/")

##################################################################
# Inserire ID e nome della variabile
varID = 'PA003Q04HA'
varName = 'parent_help' # must be inserted also when calling ANOVA
##################################################################

countries = c('austria', 'belgium', 'denmark', 'germany', 'greatbrit', 'italy', 
              'luxembourg', 'spain1', 'sweden', 'swiss') 


for (countName in countries) {
  
  fileName = paste(countName,'.txt')
  fullData = read.table(fileName, header = TRUE)
  newData = na.omit(fullData[,c(varID, 'IMMIG')])
  names(newData) = c(varName, 'immig')
  
  newData$immig = as.factor(newData$immig)
  
  # ANOVA between Native and Immigrants
  fit = aov(parent_help ~ immig, newData)
  # Printing results......
  
}








