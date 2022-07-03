# CREATE A SINGLE DATAFRAME WITH STUDENT FEATURES AND SCHOOL FEATURES

#Import seprate dataframes
school = read.table(file="school_ita.txt", header = T)
stud = read.table(file="student_ita.txt", header = T)

n_stud = dim(stud)[1]  #num of rows of stud
n_school = dim(school)[1]   #num of rows of school



for(l in 2:dim(school)[2]) {    #for each school feature (exceppt first which is school id)
  newCol = rep(0,n_stud)
  for(i in 1:n_stud) {                # for each row of stud
    current_sch_id=stud$school_id[i]
  
    for(j in 1:n_school){             # for each row of school look for same school_id and copy current feature
      if(current_sch_id==school$id[j]) {
        newCol[i] = school[j,l]
        break
      }
    }
  }
  newName = names(school)[l]
  stud[newName] = newCol
}

#Save merged dataframe
write.table(stud, file="complete_stud.txt")
COMPLETE = read.table(file="complete_stud.txt")
View(COMPLETE)
