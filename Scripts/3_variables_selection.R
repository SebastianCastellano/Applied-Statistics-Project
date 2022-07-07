#Script to extract the most relevant features

#Import Labraries
#library(sas7bdat)
library(labstatR)
#install.packages("naniar")
#library(naniar)


#Import the students data of selected countries
setwd("~/GitHub/Applied-Statistics-Project/txt - files/European countries raw")
ITA= read.table(file = "italy.txt", header = T)
AUT= read.table(file = "austria.txt", header = T)
BEL= read.table(file = "belgium.txt", header = T)
DNK= read.table(file = "denmark.txt", header = T)
DEU= read.table(file = "germany.txt", header = T)
LUX= read.table(file = "luxembourg.txt", header = T)

ESP1= read.table(file = "spain1.txt", header = T) #spain is divided in three files because github can't handle files bigger than 100mb
ESP2= read.table(file = "spain2.txt", header = T)
ESP3= read.table(file = "spain3.txt", header = T)
ESP = rbind(ESP1,ESP2,ESP3)
rm(ESP1,ESP2,ESP3)

SWE= read.table(file = "sweden.txt", header = T)
CHE= read.table(file = "swiss.txt", header = T)
GBR= read.table(file = "greatbrit.txt", header = T)

#List of countries to iterate through
countries = list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)

#List of names of countries
name_countries = c("ITA", "AUT", "BEL", "DNK", "DEU", "LUX", "ESP", "SWE", "CHE", "GBR")
name_files = c('student_ita.txt', 'student_aut.txt', 'student_bel.txt', 'student_dnk.txt', 'student_deu.txt', 'student_lux.txt', 'student_esp.txt',
               'student_swe.txt', 'student_che.txt', 'student_gbr.txt')
n_countries = length(countries) 

#Import school data
library(haven)
school <- read_sas("C:/Users/sebas/OneDrive/Desktop/Poli 2021-2022/Applied Statistics/Project/SCH/cy07_msu_sch_qqq.sas7bdat")

#Create school data.frame (then we will merge it with student data in the loop below)
school_features <- data.frame(school_id = as.character(school$CNTSCHID),
                    class_size = school$CLSIZE, 
                    stud_teach_ratio = school$STRATIO,
                    short_edu_mat = school$EDUSHORT, #shortage of education material (WLE)
                    short_edu_staff = school$STAFFSHORT, #shortage in staff (WLE)
                    stu_behav = school$STUBEHA, #student behavior hindering learning (WLE)
                    teach_behav = school$TEACHBEHA, #teacher behavior hindering learning (WLE)
                    teach_multicult = school$SCMCEG #School principal's view on teachers' multicultural and egalitarian beliefs (WLE)
                    )

#Variable selection
setwd("~/GitHub/Applied-Statistics-Project/txt - files/stud_school_features")
for (i in 1:n_countries) {
  # Categorical variables:
  # school ID
  school_id = as.character(countries[[i]]$CNTSCHID)
  # Gender (male=1, female=0)
  gender = as.character(countries[[i]]$ST004D01T - 1)
  # Immigration
  immigration = as.character(countries[[i]]$IMMIG)
  #grade repetition
  grade_rep = countries[[i]]$REPEAT

  student = data.frame(school_id = school_id, 
                       gender = gender, 
                       immigration = immigration, 
                       language = countries[[i]]$LANGN, #language spoken at home
                       hisced = countries[[i]]$HISCED,  #highest education of parent
                       grade_rep = grade_rep, 
                       joy_read = countries[[i]]$JOYREAD, 
                       pisa_difficulty = countries[[i]]$PISADIFF, #perception of difficulty of the pisa test
                       competitiveness = countries[[i]]$COMPETE,
                       fear_failure = countries[[i]]$GFOFAIL, 
                       resilience = countries[[i]]$RESILIENCE, 
                       belonging = countries[[i]]$BELONG, #sense of belonging in school
                       bullied = countries[[i]]$BEINGBULLIED,
                       home_poss = countries[[i]]$HOMEPOS, 
                       cult_poss = countries[[i]]$CULTPOSS, 
                       edu_resources = countries[[i]]$HEDRES, #home educational resources
                       family_wealth = countries[[i]]$WEALTH,  
                       ESCS_status = countries[[i]]$ESCS, #Index of economic, social and cultural status
                       teacher_support = countries[[i]]$TEACHSUP, #Teacher support in test language lessons
                       emo_sup = countries[[i]]$EMOSUPS, #Parents' emotional support perceived by student
                       
                       school_changes = countries[[i]]$SCCHANGE, #respect for people from other cultures
                       learn_time_math = countries[[i]]$MMINS, #minutes per week studying math
                       learn_time_read = countries[[i]]$LMINS, #minutes per week studying test language
                       learn_time_scie = countries[[i]]$SMINS, #minutes per week studying science
                       
                       math = countries[[i]]$PV3MATH, 
                       read = countries[[i]]$PV3READ,
                       scie = countries[[i]]$PV3SCIE)
  
  student <- merge(student,school_features,by="school_id")
  
  #write.table(student, file=name_files[i]) # commented because already done
  
}

#Check that all countries have enough values for each feature
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

countries = list(ITA, AUT, BEL, DNK, DEU, LUX, ESP, SWE, CHE, GBR)

#in order to see the number of NA: apply(X = is.na(student), MARGIN = 2, FUN = sum)
library(visdat)
for (i in 1:n_countries){
  #x11()
  #vis_miss(countries[[i]])
  print(name_countries[i])
  print(names(countries[[i]])[which(apply(X=is.na(countries[[i]]),MARGIN=2,FUN=sum)>0.8*dim(countries[[i]])[1])])
}

#Missing features for each country:
#  "ITA"
#  no feature largely missing
#  "AUT"
#  "morning_study"   "afternoon_study" "school_changes" "stud_teach_ratio" "teach_multicult"
#  "BEL"
#  "immig_att" "aware_int_com" "respect" "teach_multicult"
#  "DNK"
#  "immig_att" "aware_int_com" "respect" "teach_multicult"
#  "DEU"
#  "morning_study"   "afternoon_study"
#  "LUX"
#  "morning_study"   "afternoon_study" "immig_att" "aware_int_com"  "respect" "school_changes"  "teach_multicult"     
#  "ESP"
#  no feature largely missing
#  "SWE"
#  "morning_study"   "afternoon_study" "immig_att" "aware_int_com"  "respect"  "school_changes"  ""teach_multicult" "class_size"  
#  "CHE"
#  "morning_study"   "afternoon_study" "school_changes"  
#  "GBR"
#  "immig_att" "aware_int_com" "respect"  "teach_multicult"

#other features removed because too many NA: 
# countries[[i]]$EMOSUPP,  countries[[i]]$DISCRIM, stratum, ITA$WB032Q01NA, ITA$WB031Q01NA, ITA$SWBP, ITA$WB154Q04HA, ITA$WB154Q05HA,
# ITA$WB154Q06HA, ITA$WB154Q07HA, ITA$WB154Q08HA, ITA$WB154Q09HA
# possibile to add: countries[[i]]$COBN_M, countries[[i]]$COBN_F, countries[[i]]$COBN_S

#remove NA and re-save the datasets
for (i in 1:n_countries){
  dim1 <- dim(countries[[i]])[1]
  miss_feat <- which(apply(X=is.na(countries[[i]]),MARGIN=2,FUN=sum)>0.8*dim1)
  if (length(miss_feat)>0)
    countries[[i]]<-countries[[i]][-miss_feat]
  print(name_countries[i])
  print(dim(countries[[i]])[2])#write.table(countries[[i]], file=name_files[i])
  #write.table(countries[[i]], file=name_files[i]) # commented because already done
}


