#Load packages
library(tidyverse) #install common packages: ggplot2, dplyr, tidyr, readr, purrr,tibble, stringr, forcats
library(lmtest) #for linear regression
library('fastDummies') #create dummy variables
library(moderndive) #tidyverse-friendly linear regression
library(skimr) #summary stats
library(googlesheets4) #To access Google sheets
library(excel.link) #data exchange with Excel
library(readxl) #read Excel files
library(mediation) #for causal mediation analysis
library(marginaleffects) #predictions, comparisons, slopes, marginal means, hypothesis tests

#load data from Excel
setwd("~/")
#open schools data
Schools <- xl.read.file("MasterSchoolList_2324.xlsx")
View(Schools)
#keep only needed variables
Schools <- Schools[c(2,3)]
Schools$`School number`<- as.numeric(Schools$`School number`)

setwd("~/CMAS_Interims")
#Interim data
interims <- xl.read.file("Interim_2023-4_data_with_iReady.xlsx",xl.sheet = "data") #default sheet name is sheet1, need to add sheet name if not sheet1
#CMAS data
cmas <- xl.read.file("CMAS_2023-24_ELA_Math.xlsx")
#optin data
optin <- xl.read.file("optins.xlsx",xl.sheet = "Optins")
#Remove unneeded rows 
interims <- interims[c(2,4:9,11)]
cmas <- cmas[c(3:10)]

#Merge/join datasets 
#specify many to many because students have multiple rows
cmas_interim <- cmas %>%
  full_join(interims, by = c("PERMNUM","MEASURECODE"),suffix = c("_CMAS","_interim"), relationship = "many-to-many") %>%
  left_join(Schools, by = c("SCHOOLNUM"="School number"))
cmas_interim <- cmas_interim %>%
  left_join(optin, by = c("SCHOOLNUM" = "Schoolnum"))

#Reorder variables to put student and school info at the beginning 
cmas_interim <- cmas_interim %>%
  relocate(PERMNUM,.before = TESTNAME_CMAS) %>%
  relocate(SCHOOLNUM,.before = TESTNAME_CMAS) %>%
  relocate(`CDE School Name`,.before = TESTNAME_CMAS)

#Rename variables
#New name is first, add CMAS or interim so easier to interpret (these names don't overlap so the suffix wasn't added in the join)
cmas_interim <- cmas_interim %>%
  rename(SS_CMAS = SS,PROFLVL_CMAS=PROFLVL,SEMESTER_interim=SEMESTER,RS_interim=RS,PTS_POSS_interim=PTS_POSS,PCT_interim=PCT) 
  
#Save nulls as NAs 
cmas_interim [cmas_interim =='NULL'] <- NA 

#recode interim proflevels 
#Make new variable with proficieny levels on a scale from 1 to 5 
cmas_interim <- cmas_interim %>%
  mutate(PROFLVL_interim = ifelse(PROFLVL_DESCRIPTION_interim == "Did Not Yet Meet Expectations" | PROFLVL_DESCRIPTION_interim == "3 or More Grade Levels Below",1,
                                  ifelse(PROFLVL_DESCRIPTION_interim == "Partially Met Expectations"| PROFLVL_DESCRIPTION_interim == "2 Grade Levels Below",2, 
                                         ifelse(PROFLVL_DESCRIPTION_interim == "Approached Expectations"| PROFLVL_DESCRIPTION_interim == "1 Grade Level Below",3,
                                                ifelse(PROFLVL_DESCRIPTION_interim == "Met Expectations"| PROFLVL_DESCRIPTION_interim == "Early On Grade Level",4,
                                                       ifelse(PROFLVL_DESCRIPTION_interim == "Exceeded Expectations"| PROFLVL_DESCRIPTION_interim == "Mid or Above Grade Level",5,NA))))))
#1 if proficiency level is 4 or 5 
cmas_interim <- cmas_interim %>% 
  mutate (PROF_cmas = ifelse(PROFLVL_CMAS < 4,0,1)) %>%
  mutate (PROF_interim = ifelse(PROFLVL_interim < 4,0,1)) 

#Make separate iReady data set for separate analysis
cmas_interim_iReady <- cmas_interim %>%
  filter(TESTNAME_interim == "i-Ready Math")
#remove iReady from other data set
cmas_interim <- cmas_interim %>%
  filter(TESTNAME_interim != "i-Ready Math")

#remove semester variable to replace
#Old semester variable doesn't have the right information, make a new one to identify assessments 1,2, and 3
cmas_interim <- cmas_interim[-10]
#identify assessments 1,2,3 
cmas_interim <- cmas_interim %>%
  mutate(SEMESTER_interim = ifelse(grepl("Assessment 1",TESTNAME_interim)|grepl("Interim 1",TESTNAME_interim),"F",
                                   ifelse(grepl("Assessment 2",TESTNAME_interim),"M",
                                          ifelse(grepl("Assessment 3",TESTNAME_interim),"S",NA))))
                                

#Save as csv file for later
write.csv(cmas_interim,"C:\\Users\\kayla_morehead\\Documents\\CMAS_Interims\\cmas_interim_2324_070324.csv",row.names=FALSE)
write.csv(cmas_interim_iReady,"C:\\Users\\kayla_morehead\\Documents\\CMAS_Interims\\cmas_interim_iReady_2324_070224.csv",row.names=FALSE)

#Open file if don't need to run through all this again
setwd("~/CMAS_Interims")
cmas_interim <- xl.read.file("cmas_interim_2324_070324.csv")
View(cmas_interim)
cmas_interim_iReady <- xl.read.file("cmas_interim_iReady_2324_070224.csv")
view(cmas_interim_iReady)

#makes a bunch of characters so need to make numeric
cmas_interim$SCHOOLNUM <- as.numeric(cmas_interim$SCHOOLNUM)
cmas_interim$GRADE <- as.numeric(cmas_interim$GRADE)
cmas_interim$PROFLVL_CMAS <- as.numeric(cmas_interim$PROFLVL_CMAS)
cmas_interim$PROFLVL_interim <- as.numeric(cmas_interim$PROFLVL_interim)
cmas_interim$PROF_cmas <- as.numeric(cmas_interim$PROF_cmas)
#make nas, remove unneeded variable, and remove NAs
cmas_interim [cmas_interim =='NA'] <- NA 
cmas_interim <- cmas_interim[-21]
cmas_interim <- data.frame(na.omit(cmas_interim))

#Same as above for iReady data 
cmas_interim_iReady$SCHOOLNUM <- as.numeric(cmas_interim_iReady$SCHOOLNUM)
cmas_interim_iReady$GRADE <- as.numeric(cmas_interim_iReady$GRADE)
cmas_interim_iReady$PROFLVL_CMAS <- as.numeric(cmas_interim_iReady$PROFLVL_CMAS)
cmas_interim_iReady$PROF_cmas <- as.numeric(cmas_interim_iReady$PROF_cmas)
cmas_interim_iReady [cmas_interim_iReady =='NA'] <- NA 
cmas_interim_iReady <- cmas_interim_iReady[-c(12:14)]
cmas_interim_iReady <- data.frame(na.omit(cmas_interim_iReady))

#plot interims and CMAS on a scatterplot, jitter because a lot of overlap
cmas_interim %>%
  ggplot(aes(PROFLVL_interim,PROFLVL_CMAS))+
  geom_point(position = "jitter") +
  facet_grid(MEASURECODE ~ SEMESTER_interim) #Makes separate charts by ELA/math and semester

#correlate interim and CMAS results 
#group to perform correlations separately by ELA/math and semester
#distinct because some students took ELA and SLA, this takes the first value in alphabetical order, so excludes SLA 
#summarize: makes columns for percent proficient on interim and CMAS, and correlates proficiency levels, include number of students
correlations <- cmas_interim %>%
  group_by(MEASURECODE,SEMESTER_interim) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"),
            studentCount = n_distinct(PERMNUM))

#Demographic Results by school
#Same as above also broken out by school 
bySchool_demos <- cmas_interim %>%
  filter(OptIn == 1) %>%
  group_by(`CDE.School.Name`,MEASURECODE,SEMESTER_interim) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(PctProf_interim = mean(PROF_interim,na.rm = TRUE), 
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,
                              use = "na.or.complete"),studentCount = n_distinct(PERMNUM))
bySchool_demos [bySchool_demos =='NaN'] <- NA 

#save new school data
write.csv(bySchool_demos,"C:\\Users\\kayla_morehead\\Documents\\CMAS_Interims\\schooldata_2324_071124.csv",row.names=FALSE)

#open schools file
bySchool_demos <- xl.read.file("schooldata_2324_071124.csv")
bySchool_demos$correlation <- as.numeric(bySchool_demos$correlation)
bySchool_demos [bySchool_demos =='NA'] <- NA 

#graph results
bySchool_demos %>% 
  ggplot(aes(PctProf_interim,PctProf_CMAS))+
  geom_point(position = "jitter")+
  facet_grid(MEASURECODE ~ SEMESTER_interim)
bySchool_demos %>% 
  ggplot(aes(`CDE School Name`,correlation))+
  geom_point(position = "jitter")+
  facet_grid(MEASURECODE ~ SEMESTER_interim)+
  ylim(c(0,1))

#Grade level and bands
#same as above by grade level
correlationsgrades <- cmas_interim %>%
  group_by(MEASURECODE,SEMESTER_interim,GRADE) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"),
            n_distinct(PERMNUM))

#make grade bands. lower = 3-5, upper = 6-8
cmas_interim <- cmas_interim %>%
  mutate(gradeBands = ifelse(GRADE<6,"Lower",ifelse(GRADE>5,"Upper","")))

#Same correlation for grade bands
correlationsbands <- cmas_interim %>%
  group_by(MEASURECODE,SEMESTER_interim,gradeBands) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"),
            n_distinct(PERMNUM))

#Proficiency categories
#Calculate the number of students in each proficiency level
profLVls <- cmas_interim %>%
  group_by(MEASURECODE,SEMESTER_interim,PROFLVL_interim,PROFLVL_CMAS)%>%
  summarize(n_distinct(PERMNUM))


#iReady results
#Repeat the steps above for iReady 

#correlate iReady and CMAS results 
cmas_interim_iReady %>%
  ggplot(aes(PROFLVL_interim,PROFLVL_CMAS))+
  geom_point(position = "jitter") +
  facet_wrap(SEMESTER_interim)

correlations <- cmas_interim_iReady %>%
  group_by(SEMESTER_interim) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(studentCount = n_distinct(PERMNUM),PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"))

#Demographic Results by school
bySchool_demos <- cmas_interim_iReady %>%
  group_by(`CDE.School.Name`,SEMESTER_interim) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(studentCount = n_distinct(PERMNUM),PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            PctProf_interim = mean(PROF_interim,na.rm = TRUE), 
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,
                              use = "na.or.complete"))
bySchool_demos [bySchool_demos =='NaN'] <- NA 
write.csv(bySchool_demos,"C:\\Users\\kayla_morehead\\Documents\\CMAS_Interims\\schooldata_iReady_2324_071124.csv",row.names=FALSE)

bySchool_demos %>%
  ggplot(aes(PctProf_interim,PctProf_CMAS))+
  geom_point(position = "jitter") +
  facet_wrap(SEMESTER_interim)

#Grade level and bands
correlationsgrades <- cmas_interim_iReady %>%
  group_by(SEMESTER_interim,GRADE) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(n_distinct(PERMNUM),
            PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"))

cmas_interim_iReady <- cmas_interim_iReady %>%
  mutate(gradeBands = ifelse(GRADE<6,"Lower",ifelse(GRADE>5,"Upper","")))

correlationsbands <- cmas_interim_iReady %>%
  group_by(SEMESTER_interim,gradeBands) %>%
  distinct(PERMNUM,.keep_all = TRUE) %>%
  summarize(n_distinct(PERMNUM),
            PctProf_interim = mean(PROF_interim,na.rm = TRUE),
            PctProf_CMAS = mean(PROF_cmas,na.rm = TRUE),
            correlation = cor(PROFLVL_CMAS,PROFLVL_interim,use = "na.or.complete"))

#Proficiency categories
profLVls <- cmas_interim_iReady %>%
  group_by(SEMESTER_interim,PROFLVL_interim,PROFLVL_CMAS)%>%
  summarize(n_distinct(PERMNUM))
