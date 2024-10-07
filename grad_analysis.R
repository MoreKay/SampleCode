library(tidyverse)
library(excel.link)
library(ggplot2)
library(writexl)
library(lubridate)

#Upload files
setwd("~/Kinder_graduation Analysis")
kinder <- xl.read.file("Kinder_grad Data_2.xlsx", xl.sheet = "Kinder_03-04")
View(kinder)
hs <- xl.read.file("Kinder_grad Data_2.xlsx", xl.sheet = "Leave data")
View(hs)

#Remove unneeded variables
kinder <- kinder[,-c(5:8)]
hs <- hs[,-c(8:11)]

#Left join kinder to hs
kinder_hs <- kinder %>%
  left_join(hs, by = c("STATE_STUDENT_ID"="SASID"),suffix = c(".k",".h"))

#Clean up data and fix data types
kinder_hs [kinder_hs =="NULL"] = NA 
kinder_hs$ANTICIPATED_GRAD_YEAR <- as.numeric(kinder_hs$ANTICIPATED_GRAD_YEAR )
kinder_hs$LEAVE_DATE <- ymd(kinder_hs$LEAVE_DATE)
kinder_hs$ENTER_DATE <- ymd(kinder_hs$ENTER_DATE)

#Save joined file
write_xlsx(kinder_hs, path = 'kinder_hs.xlsx')

#Count of all kinders 
kinder_hs %>%
  group_by(GENDER,CDE_ETHNIC_CODE) %>%
  summarise(UniStu = n_distinct(STATE_STUDENT_ID))

#Remove leave codes for students to not include in analysis 
kinder_hs %>%
  na.omit(CDE_LEAVE_CODE) %>%
  filter(!CDE_LEAVE_CODE %in% c(02,05,06,13:16)) %>%
  group_by(GENDER,CDE_ETHNIC_CODE) %>%
  summarise(UniStu = n_distinct(STATE_STUDENT_ID))

kinder_hs %>%
  count(is.na(CDE_LEAVE_CODE))
#344 are missing leave codes, probably a difference between October count and CDE datasets, those students probably left in kinder, don't count them
kinder_hs_2 <- kinder_hs %>%
  na.omit(CDE_LEAVE_CODE) %>%
  filter(!CDE_LEAVE_CODE %in% c(02,05,06,13:16))

#count leave code = 90, 95, or 96 on or before anticipated grad year as graduated high school
kinder_hs %>%
  filter(CDE_LEAVE_CODE %in% c(90,95,96), SCHOOL_YEAR.h <= ANTICIPATED_GRAD_YEAR) %>%
  group_by(GENDER,CDE_ETHNIC_CODE) %>%
  summarise(UniStu = n_distinct(STATE_STUDENT_ID))

#Add college data 
#enrollment data
college_en <- xl.read.file("college.xlsx", xl.sheet = "COLLEGE_ENROLLMENT")
View(college_en)
#graduation data
college_gr <- xl.read.file("college.xlsx", xl.sheet = "COLLEGE_GRADUATION")
View(college_gr)

#check and clean data
#enrollment - convert column 2 to date
college_en$FIRST_COLLEGE_START_DATE <- as.character(college_en$FIRST_COLLEGE_START_DATE)
college_en$FIRST_COLLEGE_START_DATE <- ymd(college_en$FIRST_COLLEGE_START_DATE)

#grad 
college_gr$GRADUATION_DATE <- as.character(college_gr$GRADUATION_DATE)
college_gr$GRADUATION_DATE <- ymd(college_gr$GRADUATION_DATE)

#Join kinder_hs data to college data
kinder_co_en <- kinder_hs_2 %>%
  left_join(college_en, by = "PERMNUM")
kinder_co_gr <- kinder_hs_2 %>%
  left_join(college_gr, by = "PERMNUM")

#Count college enrollments
#kinders who when to college (including not in DPS HS)
kinder_co_en %>%
  filter(!is.na(FIRST_COLLEGE_START_DATE)) %>%
  group_by(GENDER,CDE_ETHNIC_CODE) %>%
  summarise(UniStu = n_distinct(PERMNUM))
#kinders who graduated college within 6 years 
kinder_co_gr %>%
  filter(!is.na(GRADUATION_DATE)) %>%
  group_by(GENDER,CDE_ETHNIC_CODE) %>%
  summarise(UniStu = n_distinct(PERMNUM))
