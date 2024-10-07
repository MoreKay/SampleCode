library(tidyverse)
library(excel.link)
library(ggplot2)
library(writexl)
library(lubridate)
library(rsample) #to create training and testing groups
library(randomForest)
library(caret)
library(cem)
library(MatchIt)
library(yardstick)

#upload files
schools <- xl.read.file("MasterSchoolList_2324.xlsx")
setwd("~/DIBELS")
readScores <- xl.read.file("dibels_testscores.xlsx", xl.sheet = "READAct")
View(readScores)
Demos <- xl.read.file("dibels_testscores.xlsx", xl.sheet = "Demos")
View(Demos)
CMAS <- xl.read.file("dibels_testscores.xlsx", xl.sheet = "CMAS")
View(CMAS)
FRL <- xl.read.file("dibels_testscores.xlsx", xl.sheet = "FRL")
View(FRL)

#clean data
Demos <- Demos %>%
  filter(SCHOOL_YEAR == 2023)
CMAS <- CMAS %>%
  filter(SCHOOLYEAR == '2023-2024')
FRL <- FRL %>%
  filter(SCHOOLYEAR == '2023-2024')
FRL <- FRL[,-1]

#Check for and remove errors
readScores <- readScores[,-3]
unique(readScores$SCHOOLYEAR)
readScores <- readScores %>%
  filter(SCHOOLYEAR != "Montessori")

#READ scores for 23-24 F only
readScores_24 <- readScores %>%
  filter(SCHOOLYEAR == '2023-2024',SEMESTER == 'F')
readScores_24 <- readScores_24[,-c(1,2)]
readScores_24$GT_STATUS[readScores_24$GT_STATUS == 'NULL'] <- 'NGT'
#Check for duplicate students
Demos_check %>%
  filter(duplicated(PERMNUM) == TRUE)

#ID schools who transitioned from iStation to DIBELS
schoolCheck2 <- readScores2 %>%
  group_by(`SCHOOL_NAME_2022-2023_F`,`SCHOOL_NAME_2023-2024_F`,`TESTNAME_2022-2023_F`,`TESTNAME_2023-2024_F`) %>%
  filter(`SCHOOL_NAME_2022-2023_F` == `SCHOOL_NAME_2023-2024_F`,`TESTNAME_2022-2023_F` != `TESTNAME_2023-2024_F`) %>%
  summarise(studentcount = n_distinct(PERMNUM)) %>%
  filter(studentcount > 10)
switchedSchools <- c('school1', 'school2', 'school3', 'school4', 'school5','school6','school7','school8','school9')
#Join FRL data 
readScores_24 <- readScores_24 %>%
  left_join(FRL,by = 'PERMNUM')
#save data
write.csv(readScores_24,"readscores_2324_F.csv")

#Create DIBELS dataset
dibels <- readScores_24 %>%
  filter(TESTNAME == "DIBELS 8") 
#Count of students
dibels %>%
  summarise(n_distinct(PERMNUM))
#clean data
dibels$SS <- as.numeric(dibels$SS)
dibels [dibels =="NA"] = NA 

#create dataset of other assessments
non_dibels <- readScores_24 %>%
  filter(TESTNAME != "DIBELS 8") 
#count of students
non_dibels %>%
  summarise(n_distinct(PERMNUM))
#clean as needed
non_dibels$SS <- as.numeric(non_dibels$SS)
non_dibels [non_dibels =="NA"] = NA 

#checks and final set up
unique(dibels$RACE_ETHNICITY)
dibels$RACE_ETHNICITY <- factor(dibels$RACE_ETHNICITY)
dibels$RACE_ETHNICITY <- relevel(dibels$RACE_ETHNICITY, ref = "Hispanic")
#Create training and testing sets 
splitdata <- initial_split(dibels, prop = .75)
traindata <- training(splitdata)
testdata <- testing(splitdata)
#save data for training and testing
write.csv(traindata,"traindibels.csv")
write.csv(testdata,"testdibels.csv")

#ggplot
ggplot(dibels,aes(log(SS))) +
  geom_histogram()
#Analysis
#Inspect data and decide on model and transformations
linearmodel <- lm(SS ~ GRADE + GENDER + RACE_ETHNICITY + ELL_STATUS + FRL_CODE + SPED_STATUS + SECTION_504_STATUS + 0, traindata)
summary(linearmodel)
#predict using testing sets
explanatory_data <- tibble(testdata[,c(5,8:12,16)])
prediction_Data <- explanatory_data %>%
  mutate(prediction = predict(linearmodel,testdata)) %>%
  mutate(actual_SS = testdata$SS)

ggplot(prediction_Data,aes(prediction,actual_SS)) +
  geom_point() + 
  geom_smooth(method = "lm") 

#Repeat analysis with group who switched assessments
switch <- readScores2 %>%
  filter(`SCHOOL_NAME_2023-2024_F` %in% switchedSchools)
unique(switch$`SCHOOL_NAME_2023-2024_F`)
#keep only necessary variables
colnames(switch)
switch <- switch[,c(1,4,5,10,11,17,22,23,28,29,34,35,41,47,53,59,65,71,76,77,83)]
#keep only students who stayed at the same school
switch <- switch %>%
  filter(`SCHOOL_NAME_2022-2023_S` == `SCHOOL_NAME_2023-2024_F`)
#check
unique(switch$`TESTNAME_2022-2023_S`)
unique(switch$`TESTNAME_2023-2024_F`)
#Join FRL data
switch <- switch %>%
  left_join(FRL,by = 'PERMNUM')
#Change data ypes as needed
switch$'SS_2023-2024_F' <- as.numeric(switch$'SS_2023-2024_F')
switch$`SS_2022-2023_S` <- as.numeric(switch$`SS_2022-2023_S`)
switch$`RACE_ETHNICITY_2023-2024_F` <- factor(switch$`RACE_ETHNICITY_2023-2024_F`)
switch$`RACE_ETHNICITY_2023-2024_F` <- relevel(switch$`RACE_ETHNICITY_2023-2024_F`, ref = "Hispanic")
switch$`GT_STATUS_2023-2024_F` [switch$`GT_STATUS_2023-2024_F` =="NULL"] = NA 
switch$FRL_CODE [switch$FRL_CODE =="NA"] = NA 
#Run model predicting fall scores
linearmodel <- lm(`SS_2023-2024_F` ~ `SS_2022-2023_S`+ `GRADE_2023-2024_F` + `GENDER_2023-2024_F` + `ELL_STATUS_2023-2024_F` + `SPED_STATUS_2023-2024_F` + `SECTION_504_STATUS_2023-2024_F` +  FRL_CODE, data = switch)
summary(linearmodel)

#predict 
explanatory_data <- tibble(switch[,c(6,9,15:17,22)])
prediction_Data <- explanatory_data %>%
  mutate(prediction = predict(linearmodel,switch)) %>%
  mutate(actual_SS = switch$`SS_2023-2024_F`)

ggplot(prediction_Data,aes(actual_SS,prediction)) +
  geom_point() + 
  geom_smooth(method = "lm") 

#Save files
write.csv(dibels,"dibels.csv")
write.csv(switch,"switchedschools.csv")

#open saved files
setwd("~/DIBELS")
dibels <- xl.read.file("dibels.csv")
switch <- xl.read.file("switchedschools.csv")

#Explore students on READplans
#setup
readScores_24 %>%
  group_by(TESTNAME, READplans) %>%
  summarise(n_distinct(PERMNUM)) %>%
  print(n=14)
readScores_24 <- na.omit(readScores_24)
#DIBELS vs not DIBELS groups
readScores_24 <- readScores_24 %>%
  mutate(dibels = case_when(TESTNAME == 'DIBELS 8' ~ "dibels", TESTNAME != 'DIBELS 8' ~ 'not_dibels'))

#Matched analysis
#coarsened exact matching 
require(cem)
matchthat <- matchit(dibels ~ + GRADE + GENDER + RACE_ETHNICITY + ELL_STATUS + SPED_STATUS + SECTION_504_STATUS + GT_STATUS + FRL_CODE, data = readScores_24, method = 'cem', estimand = 'ATT')
#print descriptive data and how many students were included in matching 
summary (matchthat, un=FALSE)
matched_dib <-match.data(matchthat) %>% arrange(subclass,dibels)
#relevel for dummys
matched_dib$RACE_ETHNICITY <- factor(matched_dib$RACE_ETHNICITY)
matched_dib$RACE_ETHNICITY <- relevel(matched_dib$RACE_ETHNICITY, ref = "Hispanic")
matched_dib$dibels <- factor(matched_dib$dibels)
matched_dib$dibels <- relevel(matched_dib$dibels, ref = "not_dibels")

#Estimates with logistic regression
matchmodel <- glm(READplans ~ dibels +  GRADE + GENDER + RACE_ETHNICITY + ELL_STATUS + SPED_STATUS + SECTION_504_STATUS + GT_STATUS + FRL_CODE,
                  data = matched_dib,weights = weights, family = binomial)
summary(matchmodel)

#check model fit
chival <- matchmodel$null.deviance - matchmodel$deviance
Dfval <- matchmodel$df.null - matchmodel$df.residual
pchisq(chival,18-Dfval,lower.tail = FALSE)

#effects
exp(coef(matchmodel))/(1 + exp(coef(matchmodel)))

#matched but only analyze test type
matchmodel2 <- glm(READplans ~ dibels,
                   data = matched_dib,weights = weights, family = binomial)
summary(matchmodel2)

#effects
exp(matchmodel2$coefficients)
exp(confint(matchmodel2))

#check model fit
chival <- matchmodel2$null.deviance - matchmodel2$deviance
Dfval <- matchmodel2$df.null - matchmodel2$df.residual
pchisq(chival,18-Dfval,lower.tail = FALSE)

#repeat model with just dibels group and try to predict READplans
#Create training and testing sets 
splitdata <- initial_split(dibels, prop = .75)
traindata <- training(splitdata)
testdata <- testing(splitdata)
#save data for training and testing
write.csv(traindata,"traindibels.csv")
write.csv(testdata,"testdibels.csv")


#relevel for dummys
dibels$RACE_ETHNICITY <- factor(dibels$RACE_ETHNICITY)
dibels$RACE_ETHNICITY <- relevel(dibels$RACE_ETHNICITY, ref = "Hispanic")

#Estimates
dibsonlymodel <- glm(READplans ~ GRADE + GENDER + RACE_ETHNICITY + ELL_STATUS + SPED_STATUS + SECTION_504_STATUS + GT_STATUS + FRL_CODE,
                     data = traindata,family = binomial)
summary(dibsonlymodel)

#change data type as needed
testdata <- testdata %>%
  filter(GENDER == "M"| GENDER == "F")
#predictions, test model
explanatory_data <- tibble(testdata[,c(5,8:13,17)])
prediction_Data <- explanatory_data %>%
  mutate(prediction = round(predict(dibsonlymodel,testdata, type = "response"))) %>%
  mutate(actual = testdata$READplans) %>%
  mutate(PERMNUM = testdata$PERMNUM)
#build confusion matrix
outcomes <- table(prediction_Data$prediction,prediction_Data$actual)
confusion <- conf_mat(outcomes)
autoplot(confusion)
summary(confusion, event_level = "second")

prediction_Data %>%
  group_by(prediction,actual) %>%
  summarise(n_distinct(PERMNUM))

#Predict for non-dibels takers
non_dibels_data <-tibble(non_dibels[,c(5,8:13,17)])
prediction_final_Data <- non_dibels_data %>%
  mutate(prediction = round(predict(dibsonlymodel,non_dibels_data, type = "response"))) %>%
  mutate(actual = non_dibels$READplans)%>%
  mutate(PERMNUM = non_dibels$PERMNUM)
#build confusion matrix
outcomes <- table(prediction_final_Data$prediction,prediction_final_Data$actual)
confusion <- conf_mat(outcomes)
print(confusion)
autoplot(confusion)
summary(confusion, event_level = "second")
#predict outcomes for non-Dibels takers
prediction_final_Data %>%
  group_by(prediction,actual) %>%
  summarise(n_distinct(PERMNUM))