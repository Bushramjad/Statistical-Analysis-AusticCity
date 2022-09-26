library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(data.table)
library(dplyr)
library(corrplot)
library(BSDA)
library(graphics)

CityPayrollDataset <- read.csv("~/Downloads/CityPayrollDataset.csv")

#___________________________________________________________________________________________________
# Data Preprocessing
#___________________________________________________________________________________________________

# Removing $ sign from all numerical fields
CityPayrollDataset$Temporary.Bonus.Pay <- gsub('[$]','',CityPayrollDataset$Temporary.Bonus.Pay)

# Convert the Payment & Salary  columns into numerical data type
CityPayrollDataset$Temporary.Bonus.Pay <- as.numeric(gsub(',','',CityPayrollDataset$Temporary.Bonus.Pay))

# Handling Missing Values
# Check if there are missing values in required columns
sum(is.na(CityPayrollDataset))
# 53545

sapply(CityPayrollDataset, function(x)sum(is.na(x)))

# _______________________________________ Question 1 _______________________________________________
# Do the employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay?
#___________________________________________________________________________________________________

# ---SUMMARY---
# Null Hypothesis Ho : mu = PopMean (Population Mean = Mean of Temporary Bonus Pay  )
# Alternate Hypothesis Ha : mu>PopMean
# Confidence Level 95% 
# alpha = 0.05
# t = 5.0895
# df = 99
# p-value = 8.55e-07
# The critical value for this one tailed test is 1.66
# Right Tailed Test
# Reject Null Hypothesis 
#Conclusion
# Hence, employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay


x <- as.character(CityPayrollDataset$Temporary.Bonus.Pay)   # Converting into Character  
x <- gsub('[$]','', x)             # Removing '$' sign
Tem_Bon_pay <- as.numeric(x)  

DepData = data.frame(CityPayrollDataset$Row.ID, CityPayrollDataset$Job.Class.Title,Tem_Bon_pay)
PopMean = mean(DepData$Tem_Bon_pay)
DepData = DepData[CityPayrollDataset$Job.Class.Title=="Police Officer II",]
hist(DepData$Tem_Bon_pay, main = "Department Data", xlab = "Data", col = "purple") # Sampling is not random

# T stat Calculations# T stat Calculationsxlab = 
n<-100
SampleData = sample(DepData$Tem_Bon_pay,n)
SampleMean = mean(SampleData)
StdS = sd(DepData$Tem_Bon_pay)
ttest = (SampleMean-PopMean)/(StdS/sqrt(n))
ttest
t = t.test(SampleData,mu=PopMean,alternative = c("greater"))
t


# _______________________________________ Question 2 _______________________________________________
# Question 2. Employees who get Permanent Bonus Pay are most likely to be from 
# Public Works- Sanitation Department?
# __________________________________________________________________________________________________

# ---SUMMARY---
# Null Hypothesis Ho : mu<=PopMean (Pop Mean=Mean of Permanent Bonus Pay)
# Alternate Hypothesis Ha : mu> PopMean
# Confidence Level 95% 
# Significance level = a = 0.05
# t = -0.80886
# df = 99
# p-value = 0.7897
# The critical value for this one tailed test is t= 1.66
# Right Tailed Test
# Reject Null Hypothesis 
#Conclusion
# Employees who get Permanent Bonus Pay are from Public Works- Sanitation Department

x <- as.character(CityPayrollDataset$Permanent.Bonus.Pay)   # Converting into Character  
x <- gsub('[$]','', x)             # Removing '$' sign
Per_Bon_pay <- as.numeric(x)          # converting character to string

DepData=data.frame(CityPayrollDataset$Row.ID, CityPayrollDataset$Department.Title,Per_Bon_pay)
PopMean=mean(DepData$Per_Bon_pay)
DepData=DepData[data$Department.Title=="Public Works - Sanitation",]
hist(DepData$Per_Bon_pay)


# T stat Calculations
n<-100
SampleData = sample(DepData$Per_Bon_pay,n)
SampleMean = mean(SampleData)
StdS = sd(DepData$Per_Bon_pay)
ttest = (SampleMean-PopMean)/(StdS/sqrt(n))
t.test(SampleData,mu=PopMean,alternative=c("greater"))


# _______________________________________ Question 3 _______________________________________________
# Question 3. Do the employees working in Water and Power (DWP) 
# Department have a better chance of being employed overtime?
# __________________________________________________________________________________________________

# Null Hypothesis Ho : mu<= Popmean  (No Better chance of being employed overtime)
# Alternate Hypothesis Ha : mu> Popmean (Better chance of being employed overtime)
# Confidence Level 95% 
# Significance level = a = 0.05
# The critical value for this one tailed test is t= 1.66
# Right Tailed Test
# Reject Null Hypothesis 
# Conclusion
# Hence, employees working in Water and Power (DWP) Department have a better chance of being employed overtime


unique(CityPayrollDataset$Employment.Type)    # 
x <- as.character(CityPayrollDataset$Overtime.Pay)   # Converting into Character  
x <- gsub('[$]','', x)             # Removing '$' sign
overtime <- as.numeric(x)          # converting character to string

DepData=data.frame(CityPayrollDataset$Row.ID, CityPayrollDataset$Department.Title,CityPayrollDataset$Employment.Type,overtime)
table(is.na(DepData$overtime)) # Testing NA in overtime 
DepData$overtime[is.na(DepData$overtime)]=0  # Convting NA vales to 0 
PopMean=mean(DepData$overtime)
DepData=DepData[data$Department.Title=="Water And Power (DWP)",]
hist(DepData$overtime)


# T stat Calculations
n<-100
SampleData=sample(DepData$overtime,n)
SampleMean=mean(SampleData)
StdS=sd(DepData$overtime)
ttest=(SampleMean-PopMean)/(StdS/sqrt(n))
t.test(SampleData,mu=PopMean,alternative = c("greater"))

#plot
ggplot(dataset, aes(x = Department.Title, fill =OChances)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


# _______________________________________ Question 4 _______________________________________________
# Question 4. Do the employees who work Part Time instead of Full Time have a better 
# chance to be from the Airports (LAWA) Department?


# Null Hypothesis Ho : mu>= Popmean  (employees Who Work Part Time have Better chance)
# Alternate Hypothesis Ha : mu<Popmean (Don't  have  a Better chance)
# Confidence Level 95% 
# alpha = 0.05
# df = 99
# The critical value for this one tailed test is t= 1.66
# left Tailed Test
# Reject Null Hypothesis 
# Conclusion
#employees who work Part Time instead of Full Time doesn't have a better chance to be from the Airports (LAWA) Department

DepData=data.frame(CityPayrollDataset$Row.ID, CityPayrollDataset$Department.Title,CityPayrollDataset$Employment.Type)
#table(is.na(DepData$data.Employment.Type)) # Testing NA in overtime 
DepData$data.Employment.Type[DepData$data.Employment.Type=="Part Time"]=1
DepData$data.Employment.Type[DepData$data.Employment.Type=="Full Time"]=0
DepData=DepData[DepData$data.Employment.Type!="Per Event",]
table(DepData$data.Employment.Type)
DepData=DepData[DepData$data.Department.Title=="Airports (LAWA)",]

x <- as.character(DepData$data.Employment.Type)   # Converting into Character  
x <- gsub('[$]','', x)             # Removing '$' sign
DepData$data.Employment.Type<- as.numeric(x) 

PopMean=mean(DepData$data.Employment.Type)

# T stat Calculations
n<-100
SampleData=sample(DepData$data.Employment.Type,n)
SampleMean=mean(SampleData)
StdS=sd(DepData$data.Employment.Type)
ttest=(SampleMean-PopMean)/(StdS/sqrt(n))
t.test(SampleData,mu=PopMean,alternative = c("less"))

#plot
ggplot(dataset, aes(x = Department.Title, fill =OChances)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# _______________________________________ Question 5 _______________________________________________
# After 2013, Police (LAPD) Department has experienced the highest pay raise as 
# compared to other departments?
# __________________________________________________________________________________________________


# Null Hypothesis Ho : mu> Popmean  
# Alternate Hypothesis Ha : mu< Popmean 
# Confidence Level 95% 
# alpha = 0.05
# df = 99
# The critical value for this one tailed test is t= 1.66
# Right Tailed Test
# Reject Null Hypothesis
#Conclusion
# Hence, Police (LAPD) Department has experienced the highest pay raise as compared to other departments


x <- as.character(CityPayrollDataset$Base.Pay)   # Converting into Character  
x <- gsub('[$]','', x)             # Removing '$' sign
Base.Pay <- as.numeric(x)          # converting character to string

DepData=data.frame(CityPayrollDataset$Row.ID,CityPayrollDataset$Year,CityPayrollDataset$Department.Title,CityPayrollDataset$Employment.Type,Base.Pay)
DepData=DepData[DepData$data.Year!="2013",]     #Removing 2013 values from data set
#unique(DepData$data.Year) 
DepData$Base.Pay[is.na(DepData$Base.Pay)]=0  # Converting NA vales to 0
hist(DepData$Base.Pay)
DepData2=DepData[DepData$data.Department.Title!="Police (LAPD)",]
PopMean=mean(DepData2$Base.Pay)
DepData=DepData[DepData$data.Department.Title=="Police (LAPD)",]


# t Stat Calculations
n<-100
SampleData=sample(DepData$Base.Pay,n)
SampleMean=mean(SampleData)
StdS=sd(DepData$Base.Pay)
ttest=(SampleMean-PopMean)/(StdS/sqrt(n))
t.test(SampleData,mu=PopMean,alternative = c("less"))

#plot
ggplot(dataset, aes(x = Department.Title, fill =OChances)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# _________________________________________________________________________________________________

