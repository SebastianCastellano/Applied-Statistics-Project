## SCHOOL (creation of our dataset)

library(haven)
school <- read_sas("cy07_msu_sch_qqq.sas7bdat")
dim(school)

# Countries: Italy, Austria

#-------------------------------------------------------------------------------
## EXTRACTION OF DATA RELATED TO ITALY                

ITA = school[which(school$CNTRYID==380),]
write.table(ITA,file='italy.txt')
ITA = read.table(file='italy.txt', header = T)

## selection of the variables of the school questionnaires that we want to use

# id = number that identifies the school 
id = ITA$CNTSCHID

# region = region from which the school comes from
region = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if(is.na(ITA$Region[i])==F) {
    if(ITA$Region[i]==38000) region[i]='Italy'
    else if(ITA$Region[i]==38001) region[i]='Bolzano'
    else if(ITA$Region[i]==38002) region[i]='Toscana'
    else if(ITA$Region[i]==38004) region[i]='Sardegna'
    else if(ITA$Region[i]==38012) region[i]='Trento'
  }
}
table(region)

# type_city = type of community the school is set into
type_city = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if(is.na(ITA$SC001Q01TA[i])==F) {
    if(ITA$SC001Q01TA[i]==1) type_city[i]='village'
    else if(ITA$SC001Q01TA[i]==2) type_city[i]='small town'
    else if(ITA$SC001Q01TA[i]==3) type_city[i]='town'
    else if(ITA$SC001Q01TA[i]==4) type_city[i]='city'
    else if(ITA$SC001Q01TA[i]==5) type_city[i]='large city'
  } 
}
table(type_city)

# type_school = is the school private or public?
type_school = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC013Q01TA[i]) )==F ) {
    if(ITA$SC013Q01TA[i]==1) type_school[i]=0   # it is public
    else if(ITA$SC013Q01TA[i]==2) type_school[i]=1   # it is private 
  }
}
table(type_school)

# fund1 = money given by the government 
fund1 = ITA$SC016Q01TA
# fund2 = money given by the parents of the students
fund2 = ITA$SC016Q02TA
# fund3 = money given by donations ??
fund3 = ITA$SC016Q03TA
# fund4 = money given by others 
fund4 = ITA$SC016Q04TA

# info1 = the number of digital devices connected to the Internet is sufficient
info1 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC155Q01HA[i]) )==F ) {
    if(ITA$SC155Q01HA[i]==1) info1[i]='strongly disagree'  
    else if(ITA$SC155Q01HA[i]==2) info1[i]='disagree'
    else if(ITA$SC155Q01HA[i]==3) info1[i]='agree'
    else if(ITA$SC155Q01HA[i]==4) info1[i]='strongly agree'
  }
}
table(info1)

# info2 = teachers have the [...] skills to integrate digital devices in instruction
info2 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC155Q06HA[i]) )==F ) {
    if(ITA$SC155Q06HA[i]==1) info2[i]='strongly disagree'  
    else if(ITA$SC155Q06HA[i]==2) info2[i]='disagree'
    else if(ITA$SC155Q06HA[i]==3) info2[i]='agree'
    else if(ITA$SC155Q06HA[i]==4) info2[i]='strongly agree'
  }
}
table(info2)

# info3 = the school has sufficient qualified technical assistant staff
info3 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC155Q11HA[i]) )==F ) {
    if(ITA$SC155Q11HA[i]==1) info3[i]='strongly disagree'  
    else if(ITA$SC155Q11HA[i]==2) info3[i]='disagree'
    else if(ITA$SC155Q11HA[i]==3) info3[i]='agree'
    else if(ITA$SC155Q11HA[i]==4) info3[i]='strongly agree'
  }
}
table(info3)

# requirement1 = student's record of academic performance (including placement tests)
requirement1 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC012Q01TA[i]) )==F ) {
    if(ITA$SC012Q01TA[i]==1) requirement1[i]='never'  
    else if(ITA$SC012Q01TA[i]==2) requirement1[i]='sometimes'
    else if(ITA$SC012Q01TA[i]==3) requirement1[i]='always'
  }
}
table(requirement1)

# requirement2 = whether the student requires or is interested in a special programme
requirement2 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC012Q04TA[i]) )==F ) {
    if(ITA$SC012Q04TA[i]==1) requirement2[i]='never'  
    else if(ITA$SC012Q04TA[i]==2) requirement2[i]='sometimes'
    else if(ITA$SC012Q04TA[i]==3) requirement2[i]='always'
  }
}
table(requirement2)

# valuation1 = internal evaluation/Self-evaluation
valuation1 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC037Q01TA[i]) )==F ) {
    if(ITA$SC037Q01TA[i]==1) valuation1[i]='mandatory'  
    else if(ITA$SC037Q01TA[i]==2) valuation1[i]='school initiative'
    else if(ITA$SC037Q01TA[i]==3) valuation1[i]='no'
  }
}
table(valuation1)

# valuation2 = external evaluation
valuation2 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC037Q02TA[i]) )==F ) {
    if(ITA$SC037Q02TA[i]==1) valuation2[i]='mandatory'  
    else if(ITA$SC037Q02TA[i]==2) valuation2[i]='school initiative'
    else if(ITA$SC037Q02TA[i]==3) valuation2[i]='no'
  }
}
table(valuation2)

# exchange = our school offers an exchange programme with schools in other countries
exchange = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC165Q06HA[i]) )==F ) {
    if(ITA$SC165Q06HA[i]==1) exchange[i]=1   # yes, the school has it
    else if(ITA$SC165Q06HA[i]==2) exchange[i]=0   # no, the school hasn't it
  }
}
table(exchange)

# culture = our school adopts different approaches to educate students about cultural differences [...]
culture = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC165Q10HA[i]) )==F ) {
    if(ITA$SC165Q10HA[i]==1) culture[i]=1   # yes, the school does
    else if(ITA$SC165Q10HA[i]==2) culture[i]=0   # no, the school doesn't
  }
}
table(culture)

# intercultural_experiences = curriculum for the following: openness to intercultural experiences
intercultural_experiences = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC167Q03HA[i]) )==F ) {
    if(ITA$SC167Q03HA[i]==1) intercultural_experiences[i]=1   # yes, the school has it
    else if(ITA$SC167Q03HA[i]==2) intercultural_experiences[i]=0   # no, the school hasn't it
  }
}
table(intercultural_experiences)

# foreign_languages = curriculum for the following: foreign languages
foreign_languages = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC167Q05HA[i]) )==F ) {
    if(ITA$SC167Q05HA[i]==1) foreign_languages[i]=1   # yes, the school has it
    else if(ITA$SC167Q05HA[i]==2) foreign_languages[i]=0   # no, the school hasn't it
  }
}
table(foreign_languages)

# gender1 = number of male students
gender1 = ITA$SC002Q01TA
# gender2 = number of female students 
gender2 = ITA$SC002Q02TA

# soc_eco_disadvantage = percentage of students from socioeconomically disadvantaged homes
soc_eco_disadvantage = ITA$SC048Q03NA

# num_grade = number of students in the grade of interest 
num_grade = ITA$SC004Q01TA

# computer_stud = number of computer that are available for students 
computer_stud = ITA$SC004Q02TA

# connection = number of computers connected to internet
connection = ITA$SC004Q03TA

# portable = number of portable devices (laptop, tablets, ...)
portable = ITA$SC004Q04NA

# whiteboards = number of whiteboards that are available for educational purpose 
whiteboards = ITA$SC004Q05NA

# projectors = number of projectors that are available for educational purpose 
projectors = ITA$SC004Q06NA

# computer_teac = number of computers that are available for teachers 
computer_teac = ITA$SC004Q07NA


# class = average size of a class 
class = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC003Q01TA[i]) )==F ) {
    if(ITA$SC003Q01TA[i]==1) class[i]='15 students or fewer'  
    else if(ITA$SC003Q01TA[i]==2) class[i]='16-20 students'
    else if(ITA$SC003Q01TA[i]==3) class[i]='21-25 students'
    else if(ITA$SC003Q01TA[i]==4) class[i]='26-30 students'
    else if(ITA$SC003Q01TA[i]==5) class[i]='31-35 students'
    else if(ITA$SC003Q01TA[i]==6) class[i]='36-40 students'
    else if(ITA$SC003Q01TA[i]==7) class[i]='41-45 students'
    else if(ITA$SC003Q01TA[i]==8) class[i]='46-50 students'
    else if(ITA$SC003Q01TA[i]==9) class[i]='more than 50 students'
  }
}
table(class)

# left = percentage of students that left the school in their last year without a certificate 
left = ITA$SC164Q01HA

# language_lessons = presence of additional language lessons 
language_lessons = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC152Q01HA[i]) )==F ) {
    if(ITA$SC152Q01HA[i]==1) language_lessons[i]=1   # yes, the school has it
    else if(ITA$SC152Q01HA[i]==2) language_lessons[i]=0   # no, the school hasn't it
  }
}
table(language_lessons)

# help1 = does the school provide rooms where the students can study do their homework?
help1 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC052Q01NA[i]) )==F ) {
    if(ITA$SC052Q01NA[i]==1) help1[i]=1   # yes, the school has it
    else if(ITA$SC052Q01NA[i]==2) help1[i]=0   # no, the school hasn't it
  }
}
table(help1)

# help2 = does the school provide staff that helps the students with homework?
help2 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC052Q02NA[i]) )==F ) {
    if(ITA$SC052Q02NA[i]==1) help2[i]=1   # yes, the school has it
    else if(ITA$SC052Q02NA[i]==2) help2[i]=0   # no, the school hasn't it
  }
}
table(help2)

# help3 = does the school provide a peer-to-peer tutoring?
help3 = rep(NA,dim(ITA)[1])
for(i in 1:dim(ITA)[1]){
  if((is.na(ITA$SC052Q03HA[i]) )==F ) {
    if(ITA$SC052Q03HA[i]==1) help3[i]=1   # yes, the school has it
    else if(ITA$SC052Q03HA[i]==2) help3[i]=0   # no, the school hasn't it
  }
}
table(help3)


## creation of the dataframe (version 1)

SCHOOL_ITA = data.frame(id, region, type_city, type_school, fund1, fund2, fund3, fund4,
                        info1, info2, info3, requirement1, requirement2, valuation1, 
                        valuation2, exchange, culture, intercultural_experiences, foreign_languages,
                        gender1, gender2, soc_eco_disadvantage, num_grade, computer_stud,
                        connection, portable, whiteboards, projectors, computer_teac, 
                        class, left, language_lessons, help1, help2, help3)

names(SCHOOL_ITA)=c("id","region","type_city","type_school","fund1","fund2","fund3","fund4","info1","info2","info3",
                    "requirement1","requirement2","valuation1","valuation2","exchange","culture","intercultural_experiences",
                    "foreign_languages","gender1","gender2","soc_eco_disadvantage","num_grade","computer_stud",
                    "connection","portable","whiteboards","projectors","computer_teac",
                    "class", "left","language_lessons","help1","help2","help3")

write.table(SCHOOL_ITA, file='school_ita.txt')
SCHOOL_ITA = read.table(file='school_ita.txt')


## creation of the dataframe (version 2)
# 
# SCHOOL_ITA = data.frame(ITA$CNTSCHID,ITA$Region,ITA$SC001Q01TA,ITA$SC013Q01TA,ITA$SC016Q01TA,ITA$SC016Q02TA,
#                         ITA$SC016Q03TA,ITA$SC016Q04TA,ITA$SC155Q01HA,ITA$SC155Q06HA,ITA$SC155Q11HA,
#                         ITA$SC012Q01TA,ITA$SC012Q04TA,ITA$SC037Q01TA,ITA$SC037Q02TA,ITA$SC165Q06HA,
#                         ITA$SC165Q10HA,ITA$SC167Q03HA,ITA$SC167Q05HA,ITA$SC002Q01TA,ITA$SC002Q02TA,
#                         ITA$SC048Q03NA,ITA$SC004Q01TA,ITA$SC004Q02TA,ITA$SC004Q03TA,ITA$SC004Q04NA,
#                         ITA$SC004Q05NA,ITA$SC004Q06NA,ITA$SC004Q07NA,ITA$SC003Q01TA,ITA$SC164Q01HA,
#                         ITA$SC152Q01HA,ITA$SC052Q01NA,ITA$SC052Q02NA,ITA$SC052Q03HA)
# 
# names(SCHOOL_ITA)=c("id","region","type_city","type_school","fund1","fund2","fund3","fund4","info1","info2","info3",
#                     "requirement1","requirement2","valuation1","valuation2","exchange","culture","intercultural_experiences",
#                     "foreign_languages","gender1","gender2","soc_eco_disadvantage","num_grade","computer_stud",
#                     "connection","portable","whiteboards","projectors","computer_teac","teach_f1","class",
#                     "left","language_lessons","help1","help2","help3")
# 
# write.table(SCHOOL_ITA, file='school_ita.txt')
# SCHOOL_ITA = read.table(file='school_ita.txt')



#-------------------------------------------------------------------------------
## EXTRACTION OF DATA RELATED TO AUSTRIA

AUT = school[which(school$CNTRYID==40),]
write.table(AUT,file='austria.txt')
AUT = read.table(file='austria.txt', header = T)

## selection of the variables of the school questionnaires that we want to use

# id = number that identifies the school 
id = AUT$CNTSCHID

# region = region from which the school comes from
region = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if(is.na(AUT$Region[i])==F) {
    if(AUT$Region[i]==38000) region[i]='Italy'
    else if(AUT$Region[i]==38001) region[i]='Bolzano'
    else if(AUT$Region[i]==38002) region[i]='Toscana'
    else if(AUT$Region[i]==38004) region[i]='Sardegna'
    else if(AUT$Region[i]==38012) region[i]='Trento'
  }
}
table(region)

# type_city = type of community the school is set into
type_city = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if(is.na(AUT$SC001Q01TA[i])==F) {
    if(AUT$SC001Q01TA[i]==1) type_city[i]='village'
    else if(AUT$SC001Q01TA[i]==2) type_city[i]='small town'
    else if(AUT$SC001Q01TA[i]==3) type_city[i]='town'
    else if(AUT$SC001Q01TA[i]==4) type_city[i]='city'
    else if(AUT$SC001Q01TA[i]==5) type_city[i]='large city'
  } 
}
table(type_city)

# type_school = is the school private or public?
type_school = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC013Q01TA[i]) )==F ) {
    if(AUT$SC013Q01TA[i]==1) type_school[i]=0   # it is public
    else if(AUT$SC013Q01TA[i]==2) type_school[i]=1   # it is private 
  }
}
table(type_school)

# fund1 = money given by the government 
fund1 = AUT$SC016Q01TA
# fund2 = money given by the parents of the students
fund2 = AUT$SC016Q02TA
# fund3 = money given by donations ??
fund3 = AUT$SC016Q03TA
# fund4 = money given by others 
fund4 = AUT$SC016Q04TA

# info1 = the number of digital devices connected to the Internet is sufficient
info1 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC155Q01HA[i]) )==F ) {
    if(AUT$SC155Q01HA[i]==1) info1[i]='strongly disagree'  
    else if(AUT$SC155Q01HA[i]==2) info1[i]='disagree'
    else if(AUT$SC155Q01HA[i]==3) info1[i]='agree'
    else if(AUT$SC155Q01HA[i]==4) info1[i]='strongly agree'
  }
}
table(info1)

# info2 = teachers have the [...] skills to integrate digital devices in instruction
info2 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC155Q06HA[i]) )==F ) {
    if(AUT$SC155Q06HA[i]==1) info2[i]='strongly disagree'  
    else if(AUT$SC155Q06HA[i]==2) info2[i]='disagree'
    else if(AUT$SC155Q06HA[i]==3) info2[i]='agree'
    else if(AUT$SC155Q06HA[i]==4) info2[i]='strongly agree'
  }
}
table(info2)

# info3 = the school has sufficient qualified technical assistant staff
info3 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC155Q11HA[i]) )==F ) {
    if(AUT$SC155Q11HA[i]==1) info3[i]='strongly disagree'  
    else if(AUT$SC155Q11HA[i]==2) info3[i]='disagree'
    else if(AUT$SC155Q11HA[i]==3) info3[i]='agree'
    else if(AUT$SC155Q11HA[i]==4) info3[i]='strongly agree'
  }
}
table(info3)

# requirement1 = student's record of academic performance (including placement tests)
requirement1 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC012Q01TA[i]) )==F ) {
    if(AUT$SC012Q01TA[i]==1) requirement1[i]='never'  
    else if(AUT$SC012Q01TA[i]==2) requirement1[i]='sometimes'
    else if(AUT$SC012Q01TA[i]==3) requirement1[i]='always'
  }
}
table(requirement1)

# requirement2 = whether the student requires or is interested in a special programme
requirement2 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC012Q04TA[i]) )==F ) {
    if(AUT$SC012Q04TA[i]==1) requirement2[i]='never'  
    else if(AUT$SC012Q04TA[i]==2) requirement2[i]='sometimes'
    else if(AUT$SC012Q04TA[i]==3) requirement2[i]='always'
  }
}
table(requirement2)

# valuation1 = internal evaluation/Self-evaluation
valuation1 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC037Q01TA[i]) )==F ) {
    if(AUT$SC037Q01TA[i]==1) valuation1[i]='mandatory'  
    else if(AUT$SC037Q01TA[i]==2) valuation1[i]='school initiative'
    else if(AUT$SC037Q01TA[i]==3) valuation1[i]='no'
  }
}
table(valuation1)

# valuation2 = external evaluation
valuation2 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC037Q02TA[i]) )==F ) {
    if(AUT$SC037Q02TA[i]==1) valuation2[i]='mandatory'  
    else if(AUT$SC037Q02TA[i]==2) valuation2[i]='school initiative'
    else if(AUT$SC037Q02TA[i]==3) valuation2[i]='no'
  }
}
table(valuation2)

# exchange = our school offers an exchange programme with schools in other countries
exchange = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC165Q06HA[i]) )==F ) {
    if(AUT$SC165Q06HA[i]==1) exchange[i]=1   # yes, the school has it
    else if(AUT$SC165Q06HA[i]==2) exchange[i]=0   # no, the school hasn't it
  }
}
table(exchange)

# culture = our school adopts different approaches to educate students about cultural differences [...]
culture = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC165Q10HA[i]) )==F ) {
    if(AUT$SC165Q10HA[i]==1) culture[i]=1   # yes, the school does
    else if(AUT$SC165Q10HA[i]==2) culture[i]=0   # no, the school doesn't
  }
}
table(culture)

# intercultural_experiences = curriculum for the following: openness to intercultural experiences
intercultural_experiences = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC167Q03HA[i]) )==F ) {
    if(AUT$SC167Q03HA[i]==1) intercultural_experiences[i]=1   # yes, the school has it
    else if(AUT$SC167Q03HA[i]==2) intercultural_experiences[i]=0   # no, the school hasn't it
  }
}
table(intercultural_experiences)

# foreign_languages = curriculum for the following: foreign languages
foreign_languages = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC167Q05HA[i]) )==F ) {
    if(AUT$SC167Q05HA[i]==1) foreign_languages[i]=1   # yes, the school has it
    else if(AUT$SC167Q05HA[i]==2) foreign_languages[i]=0   # no, the school hasn't it
  }
}
table(foreign_languages)

# gender1 = number of male students
gender1 = AUT$SC002Q01TA
# gender2 = number of female students 
gender2 = AUT$SC002Q02TA

# soc_eco_disadvantage = percentage of students from socioeconomically disadvantaged homes
soc_eco_disadvantage = AUT$SC048Q03NA

# num_grade = number of students in the grade of interest 
num_grade = AUT$SC004Q01TA

# computer_stud = number of computer that are available for students 
computer_stud = AUT$SC004Q02TA

# connection = number of computers connected to internet
connection = AUT$SC004Q03TA

# portable = number of portable devices (laptop, tablets, ...)
portable = AUT$SC004Q04NA

# whiteboards = number of whiteboards that are available for educational purpose 
whiteboards = AUT$SC004Q05NA

# projectors = number of projectors that are available for educational purpose 
projectors = AUT$SC004Q06NA

# computer_teac = number of computers that are available for teachers 
computer_teac = AUT$SC004Q07NA


# class = average size of a class 
class = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC003Q01TA[i]) )==F ) {
    if(AUT$SC003Q01TA[i]==1) class[i]='15 students or fewer'  
    else if(AUT$SC003Q01TA[i]==2) class[i]='16-20 students'
    else if(AUT$SC003Q01TA[i]==3) class[i]='21-25 students'
    else if(AUT$SC003Q01TA[i]==4) class[i]='26-30 students'
    else if(AUT$SC003Q01TA[i]==5) class[i]='31-35 students'
    else if(AUT$SC003Q01TA[i]==6) class[i]='36-40 students'
    else if(AUT$SC003Q01TA[i]==7) class[i]='41-45 students'
    else if(AUT$SC003Q01TA[i]==8) class[i]='46-50 students'
    else if(AUT$SC003Q01TA[i]==9) class[i]='more than 50 students'
  }
}
table(class)

# left = percentage of students that left the school in their last year without a certificate 
left = AUT$SC164Q01HA

# language_lessons = presence of additional language lessons 
language_lessons = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC152Q01HA[i]) )==F ) {
    if(AUT$SC152Q01HA[i]==1) language_lessons[i]=1   # yes, the school has it
    else if(AUT$SC152Q01HA[i]==2) language_lessons[i]=0   # no, the school hasn't it
  }
}
table(language_lessons)

# help1 = does the school provide rooms where the students can study do their homework?
help1 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC052Q01NA[i]) )==F ) {
    if(AUT$SC052Q01NA[i]==1) help1[i]=1   # yes, the school has it
    else if(AUT$SC052Q01NA[i]==2) help1[i]=0   # no, the school hasn't it
  }
}
table(help1)

# help2 = does the school provide staff that helps the students with homework?
help2 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC052Q02NA[i]) )==F ) {
    if(AUT$SC052Q02NA[i]==1) help2[i]=1   # yes, the school has it
    else if(AUT$SC052Q02NA[i]==2) help2[i]=0   # no, the school hasn't it
  }
}
table(help2)

# help3 = does the school provide a peer-to-peer tutoring?
help3 = rep(NA,dim(AUT)[1])
for(i in 1:dim(AUT)[1]){
  if((is.na(AUT$SC052Q03HA[i]) )==F ) {
    if(AUT$SC052Q03HA[i]==1) help3[i]=1   # yes, the school has it
    else if(AUT$SC052Q03HA[i]==2) help3[i]=0   # no, the school hasn't it
  }
}
table(help3)


## creation of the dataframe (version 1)

SCHOOL_AUT = data.frame(id, region, type_city, type_school, fund1, fund2, fund3, fund4,
                        info1, info2, info3, requirement1, requirement2, valuation1, 
                        valuation2, exchange, culture, intercultural_experiences, foreign_languages,
                        gender1, gender2, soc_eco_disadvantage, num_grade, computer_stud,
                        connection, portable, whiteboards, projectors, computer_teac, 
                        class, left, language_lessons, help1, help2, help3)

names(SCHOOL_AUT)=c("id","region","type_city","type_school","fund1","fund2","fund3","fund4","info1","info2","info3",
                    "requirement1","requirement2","valuation1","valuation2","exchange","culture","intercultural_experiences",
                    "foreign_languages","gender1","gender2","soc_eco_disadvantage","num_grade","computer_stud",
                    "connection","portable","whiteboards","projectors","computer_teac",
                    "class", "left","language_lessons","help1","help2","help3")

write.table(SCHOOL_AUT, file='school_aut.txt')
SCHOOL_AUT = read.table(file='school_aut.txt')


## creation of the dataframe (version 2)
# 
# SCHOOL_AUT = data.frame(ITA$CNTSCHID,ITA$Region,ITA$SC001Q01TA,ITA$SC013Q01TA,ITA$SC016Q01TA,ITA$SC016Q02TA,
#                         ITA$SC016Q03TA,ITA$SC016Q04TA,ITA$SC155Q01HA,ITA$SC155Q06HA,ITA$SC155Q11HA,
#                         ITA$SC012Q01TA,ITA$SC012Q04TA,ITA$SC037Q01TA,ITA$SC037Q02TA,ITA$SC165Q06HA,
#                         ITA$SC165Q10HA,ITA$SC167Q03HA,ITA$SC167Q05HA,ITA$SC002Q01TA,ITA$SC002Q02TA,
#                         ITA$SC048Q03NA,ITA$SC004Q01TA,ITA$SC004Q02TA,ITA$SC004Q03TA,ITA$SC004Q04NA,
#                         ITA$SC004Q05NA,ITA$SC004Q06NA,ITA$SC004Q07NA,ITA$SC003Q01TA,ITA$SC164Q01HA,
#                         ITA$SC152Q01HA,ITA$SC052Q01NA,ITA$SC052Q02NA,ITA$SC052Q03HA)
# 
# names(SCHOOL_AUT)=c("id","region","type_city","type_school","fund1","fund2","fund3","fund4","info1","info2","info3",
#                     "requirement1","requirement2","valuation1","valuation2","exchange","culture","intercultural_experiences",
#                     "foreign_languages","gender1","gender2","soc_eco_disadvantage","num_grade","computer_stud",
#                     "connection","portable","whiteboards","projectors","computer_teac","teach_f1","class",
#                     "left","language_lessons","help1","help2","help3")
# 
# write.table(SCHOOL_AUT, file='school_aut.txt')
# SCHOOL_AUT = read.table(file='school_aut.txt')
