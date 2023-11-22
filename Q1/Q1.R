# Ensure that the "Q1" subfolder exists
dir.create("Q1", showWarnings = FALSE)

# Set the working directory to the "Q1" subfolder
setwd("Q1")

#read data from file
CCS592 <- readLines("Q1_CCS592.txt")
CDS501 <- readLines("Q1_CDS501.txt")
CDS506 <- readLines("Q1_CDS506.txt")
CDS512 <- readLines("Q1_CDS512.txt")
CDS521 <- readLines("Q1_CDS521.txt")
CDS523 <- readLines("Q1_CDS523.txt")

#a
num_student <- c( CCS592 = length(CCS592), CDS501 = length(CDS501), CDS506 = length(CDS506), CDS512 = length(CDS512), CDS521 = length(CDS521), CDS523 = length(CDS523) )
#Calculate highest number of the student
max_course <- names(num_student)[which.max(num_student)]
max_student <- max(num_student)

cat("Course with the highest number of students:", max_course, "\n")
cat("Number of students:", max_student, "\n")

#Calculate lowest number of the student
min_course <- names(num_student)[which.min(num_student)]
min_student <- min(num_student)

cat("Course with the lowest number of students:", min_course, "\n")
cat("Number of students:", min_student, "\n")

#b
#Combine student of six courses
total_student_course <- c(CCS592, CDS501, CDS506, CDS512, CDS521, CDS523)

# Calculate distinct students
distinct_student <- unique(total_student_course)
student_count <- length(distinct_student)
cat("Number of distinct students across all six courses:", student_count, "\n")

#c
course_registed_student <- function(student_name){
  
  course <- c("CCS592", "CDS501", "CDS506", "CDS512", "CDS521", "CDS523")
  student_name_registered_course <- list(CCS592, CDS501, CDS506, CDS512, CDS521, CDS523)
  student_course_link <- setNames(student_name_registered_course, course)
  
  # initial student_course
  student_courses <- character(0)
  #check all the course that registered by student in each course
  for (course in course) {
    if (student_name %in% student_course_link[[course]]) {
      student_courses <- c(student_courses, course)
    }
  }
  #print the student name with course registered
  cat("Courses registered by", student_name, ":", paste(student_courses, collapse = ", "), "\n")
  
}
course_registed_student("NAME003")
#d
student_CDS512_CDS521 <- intersect(CDS512,CDS521)
cat("Students registered for both CDS512 and CDS521:", paste(student_CDS512_CDS521, collapse = ", "), "\n")
#e
student_CDS512_NO_CDS521 <- setdiff(CDS512,CDS521)
cat("Students registered for CDS512 but not CDS521:", paste(student_CDS512_NO_CDS521, collapse = ", "), "\n")
#f
student_CDS521_NO_CDS512 <- setdiff(CDS521,CDS512)
cat("Students registered for CDS521 but not CDS512:", paste(student_CDS521_NO_CDS512, collapse = ", "), "\n")
#g
CDS512_CDS521 <- union(CDS512, CDS521)
all_course_without_CDS512_CDS521 <- union(union(CCS592, CDS501), union(CDS506, CDS523))
student_NO_CDS521_NO_CDS512 <- setdiff(all_course_without_CDS512_CDS521, CDS512_CDS521)
cat("Students who did not register for CDS512 nor CDS521:", paste(student_NO_CDS521_NO_CDS512, collapse = ", "), "\n")
#h
# Create a data frame to store the counts of courses for each student
course_counts <- data.frame(Student = distinct_student, Count = 0)

# Use a for loop to count the number of courses for each student
for (i in seq_along(course_counts$Student)) {
  student <- course_counts$Student[i]
  count <- sum(student %in% CDS501, student %in% CDS506, student %in% CDS512, student %in% CDS521, student %in% CDS523, student %in% CCS592)
  course_counts$Count[i] <- count
}
students_in_three_courses <- course_counts$Student[course_counts$Count == 3]
cat("Students who registered for three courses:", paste(students_in_three_courses, collapse = ", "), "\n")
