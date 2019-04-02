setwd("G:\\IIIT-Upgrad-PGDDS\\HR Analytics\\PA-I_Case_Study_HR_Analytics")

rm(list=ls())
#Loading Libraries

library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(caTools)
library(e1071)
library("MASS")
library(car)

#install.packages("pbkrtest", dependencies = TRUE)
#install.packages("ModelMetrics")
#install.packages("RcppRoll")
#install.packages("backports")
#install.packages("ddalpha")
#install.packages("DEoptimR")
#install.packages("dimRed")
#install.packages("gower")
library(caret)
#Loading Data Sets

general<-read.csv("general_data.csv",stringsAsFactors = FALSE)
employee_survey<-read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
manager_survey<-read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
in_time<-read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time<-read.csv("out_time.csv",stringsAsFactors = FALSE)

#Structure of Data Sets

str(general) # 4410 obs.of 24 variables
str(employee_survey) # 4410 obs.of 4 variables
str(manager_survey) # 4410 obs.of 3 variables


length(unique(general$EmployeeID)) # 4410, confirming EmployeeId is key 
length(unique(employee_survey$EmployeeID))# 4410, confirming EmployeeId is key 
length(unique(manager_survey$EmployeeID))# 4410, confirming EmployeeId is key 

setdiff(general$EmployeeID,employee_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general$EmployeeID,manager_survey$EmployeeID)# Identical EmployeeID across these datasets
setdiff(general$EmployeeID,in_time$X)# Identical EmployeeID across these datasets
setdiff(general$EmployeeID,out_time$X)# Identical EmployeeID across these datasets

#Converting in_time from wide format into long format

in_time<-gather(in_time,Date,Time,X2015.01.01:X2015.12.31,na.rm = TRUE)

# Checking for missing values after conversion
sapply(in_time, function(x) sum(is.na(x)))

in_time<-separate(in_time,Time, into=c("In_Date","In_Time"),sep=" ")
in_time<-in_time[,-2]
colnames(in_time)[1]<-"EmployeeID"

#Converting out_time from wide format into long format

out_time<-gather(out_time,Date,Time,X2015.01.01:X2015.12.31,na.rm = TRUE)

# Checking for missing values after conversion
sapply(out_time, function(x) sum(is.na(x)))

out_time<-separate(out_time,Time, into=c("Out_Date","Out_Time"),sep=" ")
out_time<-out_time[,-2]
colnames(out_time)[1]<-"EmployeeID"

in_time<-cbind(in_time,out_time$Out_Time)
colnames(in_time)[4]<-"Out_Time"

#Converting In_Time and Out_time into proper format

in_time$In_Time<-parse_date_time(x= in_time$In_Time, orders = "H:M:S",locale = "eng")
in_time$Out_Time<-parse_date_time(x= in_time$Out_Time, orders = "H:M:S",locale = "eng")

View(in_time)

#Calculating total working hours
in_time$WorkingHours<-difftime(in_time$Out_Time,in_time$In_Time)

#Calculating Average Working Hours
Average_WorkingHours<-in_time %>% group_by(in_time$EmployeeID) %>% summarise(AvgHours = as.numeric(round(mean(WorkingHours,na.rm = T),2)))
colnames(Average_WorkingHours)[1]<-"EmployeeID"

# Collate the data together in one single file
   
employee<-merge(general,employee_survey,by="EmployeeID")
employee<-merge(employee,manager_survey,by="EmployeeID")
employee<-merge(employee,Average_WorkingHours,by="EmployeeID")

View(employee)

#Check the number of missing values per column in master file

sapply(employee, function(x) sum(is.na(x))) # 5 columns(TotalWorkingYears,NumCompaniesWorked,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance ) contains missing values which needs to be removed.


#Check the number of unique values per column in master file

sapply(general, function(x) length(unique(x))) 

# As employeecount(column:9), Over18(column:16) and Standard hours(column:18) have single value. Therefore,these columns can be removed.

employee<-employee[,c(-9,-16,-18)]

write.csv(employee,"employee.csv",row.names = FALSE)

# Checking for Blanks

sapply(general, function(x) length(which(x==""))) # No Blanks

# Missing values in categorical variables can be replaced by mode.

getmode <- function(v) {
       uniqv <- unique(v)
       uniqv[which.max(tabulate(match(v, uniqv)))]
   }

employee$TotalWorkingYears[which(is.na(employee$TotalWorkingYears))]<-getmode(employee$TotalWorkingYears)

employee$NumCompaniesWorked[which(is.na(employee$NumCompaniesWorked))]<-getmode(employee$NumCompaniesWorked)

employee$EnvironmentSatisfaction[which(is.na(employee$EnvironmentSatisfaction))]<-getmode(employee$EnvironmentSatisfaction)

employee$JobSatisfaction[which(is.na(employee$JobSatisfaction))]<-getmode(employee$JobSatisfaction)

employee$WorkLifeBalance[which(is.na(employee$WorkLifeBalance))]<-getmode(employee$WorkLifeBalance)

# Again check for missing values

sapply(employee, function(x) sum(is.na(x)))


cols <- c("BusinessTravel","Department","Education","EducationField","Gender","JobLevel","JobRole","MaritalStatus","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","TrainingTimesLastYear","StockOptionLevel","PerformanceRating")
employee[cols] <- lapply(employee[cols], factor)

# Barcharts for categorical features with stacked employee information

bar_theme1<- theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))

bar_theme2<-geom_text(stat='count',position = position_dodge(0.9), aes(label=..count..),vjust=-0.3)

plot_grid(ggplot(employee, aes(x=Department,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2, 
          ggplot(employee, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2 ,align="h")


plot_grid(ggplot(employee, aes(x=Education,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
          ggplot(employee, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,align="h")

plot_grid(ggplot(employee, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
          ggplot(employee, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2, align = "h")   

plot_grid(ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2, 
          ggplot(employee, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,align="h")

plot_grid(ggplot(employee, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
          ggplot(employee, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,align="h")

plot_grid(ggplot(employee, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
          ggplot(employee, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
          ggplot(employee, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2 ,align="h" )

plot_grid( ggplot(employee, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2,
           ggplot(employee, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1+bar_theme2 ,align="h")


##Checking for outliers through boxplot visualisation

ggplot(employee,aes(x="",y=Age)) + geom_boxplot()
ggplot(employee,aes(x="",y=DistanceFromHome)) + geom_boxplot()
ggplot(employee,aes(x="",y=MonthlyIncome)) + geom_boxplot()
ggplot(employee,aes(x="",y=NumCompaniesWorked)) + geom_boxplot()
ggplot(employee,aes(x="",y=PercentSalaryHike)) + geom_boxplot()
ggplot(employee,aes(x="",y=TotalWorkingYears)) + geom_boxplot()
ggplot(employee,aes(x="",y=YearsAtCompany)) + geom_boxplot()
ggplot(employee,aes(x="",y=YearsSinceLastPromotion)) + geom_boxplot()
ggplot(employee,aes(x="",y=YearsWithCurrManager)) + geom_boxplot()
ggplot(employee,aes(x="",y=AvgHours)) + geom_boxplot()


# Boxplots of numeric variables relative to Attrition

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() +theme(legend.position="none"),
          ggplot(employee, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=AvgHours, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          align = "v",nrow = 1)


plot_grid(ggplot(employee, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ coord_flip() +theme(legend.position="none"),
          ggplot(employee, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(employee, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() +theme(legend.position="none"),
          ggplot(employee, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+coord_flip() + box_theme_y,
          align = "v",nrow = 1)

## Age, DistancefromHome, PercentSalaryHike does not contain any outliers.
## NumCompaniesWorked, AvgHours contains outlier which can be ignored.

## Outlier Treatment for MonthlyIncome,TotalWorkingYears YearsAtCompany, YearsSinceLastPromotion,YearsWithCurrManager--


#1.MonthlyIncome: There is a jump between 90% and 91% so cap all the values above 91% to 137756.

quantile(employee$MonthlyIncome,seq(0,1,0.01))
employee$MonthlyIncome[which(employee$MonthlyIncome >137756)]<-137756

#2. TotalWorkingYears: There is a jump between 94% and 95% so cap all the values above 94% to 26.

quantile(employee$TotalWorkingYears,seq(0,1,0.01))
employee$TotalWorkingYears[which(employee$TotalWorkingYears >26)]<-26

#3.YearsAtCompany: There is a jump between 92% and 93% so cap all the values above 92% to 17.

quantile(employee$YearsAtCompany,seq(0,1,0.01))
employee$YearsAtCompany[which(employee$YearsAtCompany >17)]<-17

#4.YearsSinceLastPromotion:There is a jump between 95% and 96% so cap all the values above 95% to 9.

quantile(employee$YearsSinceLastPromotion,seq(0,1,0.01))
employee$YearsSinceLastPromotion[which(employee$YearsSinceLastPromotion >9)]<-9

#5. YearsWithCurrManager: There is a jump between 99% and 100% so cap all the values above 99% to 14.

quantile(employee$YearsWithCurrManager,seq(0,1,0.01))
employee$YearsWithCurrManager[which(employee$YearsWithCurrManager >14)]<-14

#Standardazing Scales

employee$Age<-scale(employee$Age)
employee$DistanceFromHome<-scale(employee$DistanceFromHome)
employee$MonthlyIncome<-scale(employee$MonthlyIncome)
employee$NumCompaniesWorked<-scale(employee$NumCompaniesWorked)
employee$PercentSalaryHike<-scale(employee$PercentSalaryHike)
employee$TotalWorkingYears<-scale(employee$TotalWorkingYears)
employee$YearsAtCompany<-scale(employee$YearsAtCompany)
employee$YearsSinceLastPromotion<-scale(employee$YearsSinceLastPromotion)
employee$YearsWithCurrManager<-scale(employee$YearsWithCurrManager)
employee$AvgHours<-scale(employee$AvgHours)


# converting target variable Attrition from No/Yes character to factor with levels 0/1 
employee$Attrition<- ifelse(employee$Attrition=="Yes",1,0)

# Checking Attrition rate of employees
Attrition <- sum(employee$Attrition)/nrow(employee)
Attrition
#16.1%

# creating a dataframe of categorical features
employee_chr<-employee[,-c(1,2,3,6,13,14,15,17,19,20,21,27)]
str(employee_chr)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_chr, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_chr))[,-1]))

employee_final<-cbind(employee[,c(2,3,6,13,14,15,17,19,20,21,27)],dummies)

View(employee_final)

# splitting the data between train and test

set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)


# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)


# There is no variable with high p value and high vif
#Removing variables on the basis of p value.
# Removing EducationField.xMarketing

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x3 + Education.x4 + Education.x5 + 
               EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)

# Removing EducationField.xOther
model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x3 + Education.x4 + Education.x5 + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_4)

# Removing TrainingTimesLastYear.x3

model_5<-glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x3 + Education.x4 + Education.x5 + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_5)

# Removing DistanceFromHome 

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x3 + Education.x4 + Education.x5 + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_6)

# Removing Education.x3

model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
                Education.x4 + Education.x5 + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_7)

# Removing Education.x4

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x5 + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_8)

# Removing Education.x5

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)
summary(model_9)

# Removing JobLevel.x2

model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
               TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train)

summary(model_10)


#StockOptionLevel.x1   

model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)

summary(model_11)

# Removing TrainingTimesLastYear.x1 

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)
summary(model_12)

# Removing JobRole.xLaboratory.Technician

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)
summary(model_13)


# Removing JobInvolvement.x3 

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                , family = "binomial", data = train)
summary(model_14)


# Removing JobRole.xResearch.Scientist 

model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director +  
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_15)


# Removing TrainingTimesLastYear.x4  

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director +  
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_16)

#Removing TrainingTimesLastYear.x5 

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director +  
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_17)

# Removing JobRole.xResearch.Director

model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_18)


# Removing JobRole.xSales.Executive     

model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle + 
                TrainingTimesLastYear.x6 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_19)

# Removing TrainingTimesLastYear.x6

model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_20)

# Removing JobSatisfaction.x3 

model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 +  JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_21)


# Removing JobSatisfaction.x2

model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              , family = "binomial", data = train)
summary(model_22)


# Removing WorkLifeBalance.x4

model_23<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 
              , family = "binomial", data = train)
summary(model_23)

# Removing WorkLifeBalance.x2

model_24<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 
              , family = "binomial", data = train)
summary(model_24)
vif(model_24)

#Removing the Age

model_25<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 
              , family = "binomial", data = train)
summary(model_25)

#removing WorkLifeBalance.x3

model_26<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train)
summary(model_26)

#Removing BusinessTravel.xTravel_Rarely

model_27 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgHours + BusinessTravel.xTravel_Frequently  + 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train)
summary(model_27)

#Removing YearsWithCurrManager 

model_28 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion  + 
                 AvgHours + BusinessTravel.xTravel_Frequently  + 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train)
summary(model_28)

#Remove YearsSinceLastPromotion 
model_29 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 AvgHours + BusinessTravel.xTravel_Frequently  + 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train)
summary(model_29)

#Remove Department.xResearch...Development 
model_30 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 AvgHours + BusinessTravel.xTravel_Frequently  + 
                 Department.xSales + 
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train)
summary(model_30)

#Remove Department.xSales 
model_31 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 AvgHours + BusinessTravel.xTravel_Frequently  + 
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train)
summary(model_31)
vif(model_31)

#Final model with 9 variables

final_model<-model_31

#Model Evaluation

test_pred = predict(final_model, type = "response", newdata = test[,-2])
                    
summary(test_pred)

test$prob <- test_pred

View(test)

# Let's use the probability cutoff of 50%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes") #Accuracy is 0.8571

table(test_actual_Attrition,test_pred_Attrition)

# Let's use the probability cutoff of 40%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")

test_conf # Accuracy is still close i.e. 0.8511,  the sensitivity is increased fairly and specificity has decreased a bit

#################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
# Creating cutoff values from 0.01 to 0.80 for plotting and initializing the matrix .

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)
s

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}



plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#let us choose the cutoff as 0.1616162
test_cutoff_attrition <- factor(ifelse(test_pred >=0.1616162, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

##################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)
Attrition_decile

