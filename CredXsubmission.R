rm(list=ls())
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
install.packages("ggplot2")
install.packages("Information")
install.packages("backports")
install.packages("pbkrtest", dependencies = TRUE) 
install.packages("dummies")
install.packages("unbalanced")
install.packages("DMwR")
install.packages("woeBinning")
library(unbalanced)
library(DMwR)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Information)
options(scipen=10)
library(RcppRoll)
library(ddalpha)
library(dimRed)
library(gower)
library(backports)
library(RcppRoll)
library(ddalpha)
library(dimRed)
library(gower)
library(backports)
library(dummies)
library(pbkrtest)
library(Matrix)
library(lme4)
library(nlme)
library(lattice)
require(dplyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library("rattle")
library(reshape2)
library("woeBinning")
library(randomForest)
library(ROCR)

demographic_data <- read.csv("Demographic_data.csv")
credit_bureau_data <- read.csv("Credit_Bureau_data.csv")
##Application ID is the common column in both database
sum({demographic_data$Application.ID == credit_bureau_data$Application.ID})
#[1] 71295
sum({demographic_data$Application.ID != credit_bureau_data$Application.ID})
#[1] 0
#Both the databases have Application.ID in the same order
##Performance Tag is also a common column in both databases
sum(demographic_data$Performance.Tag == credit_bureau_data$Performance.Tag,na.rm=T)
#[1] 69870
sum(demographic_data$Performance.Tag != credit_bureau_data$Performance.Tag,na.rm=T)
#[1] 0
sum(is.na(demographic_data$Performance.Tag))
#[1] 1425
sum(is.na(credit_bureau_data$Performance.Tag))
#[1] 1425
##Both databases have same value in Performance.Tag . 
## The NA entires in the Performance.Tag correspond to rejected candidates

###Duplicate Entry Analysis
###Application.ID
summary(demographic_data$Application.ID)
##Integers ranging from 100450 to 1000084142
length(demographic_data$Application.ID)
# 71295
length(unique(demographic_data$Application.ID))
#71292
#There are 3 duplicate Application.ID
which(duplicated(demographic_data$Application.ID))
#Rows 27587 42638 59023
demographic_data$Application.ID[27587]
#[1] 765011468
which(demographic_data$Application.ID == 765011468)
#[1] 24387 27587
which(credit_bureau_data$Application.ID == 765011468)
#[1] 24387 27587
##we need new Application.ID for row 24387
demographic_data$Application.ID[42638]
#[1] 653287861
which(demographic_data$Application.ID == 653287861)
which(credit_bureau_data$Application.ID == 653287861)
#[1]  5244 42638
## we need new Application.ID for row 5244
demographic_data$Application.ID[59023]
#[1] 671989187
which(demographic_data$Application.ID == 671989187)
which(credit_bureau_data$Application.ID == 671989187)
#[1] 48603 59023
## we need new Application.ID for row 48603
#we need to give unique Application.ID for the rows 24387,5244 and 48603
#Assigning the application id numbers 1000084143,1000084144,1000084145 to these rows in both databases
demographic_data$Application.ID[24387] <- 1000084143
credit_bureau_data$Application.ID[24387] <- 1000084143
demographic_data$Application.ID[5244] <- 1000084144
credit_bureau_data$Application.ID[5244] <- 1000084144
demographic_data$Application.ID[48603] <- 1000084145
credit_bureau_data$Application.ID[48603] <- 1000084145
### Null Analysis
any(is.na(demographic_data))
#[1] TRUE
sum(is.na(demographic_data))
#[1] 1428
###demographic data columns with null values
sapply (demographic_data, function (x) sum(is.na(x)))
#No.of.dependents 
#3
###Analyzing each column
sum(is.na(demographic_data$No.of.dependents))
#[1] 3
## 3 missing values
summary(as.factor(demographic_data$No.of.dependents))
#1     2     3     4     5  NA's 
#15387 15289 16279 12222 12115     3 
which(is.na(demographic_data$No.of.dependents))
#[1]  4675 43416 69027
##Need to assign a value to missing ; can we add 3(maximum number of applications have 3 dependents)
demographic_data$No.of.dependents[4675] <- 3
demographic_data$No.of.dependents[43416] <- 3
demographic_data$No.of.dependents[69027] <- 3
sum(is.na(demographic_data$Performance.Tag))
#1425 missing ; checking in the credit bureau data
#Checking the same feild in the credit bureau data
sum(is.na(credit_bureau_data$Performance.Tag))
#1425 missing (same Application IDs)
#These are rejected candidates. 
###Demographic_data with Empty values
sapply (demographic_data, function (x) sum(x==""|x==" "))
#Gender has 2 empty fields
which(demographic_data$Gender == "")
#[1] 39502 55579
summary(demographic_data$Gender)
#     F     M 
#2 16837 54456 
#Assigning the gender of the larger number of applicants M
demographic_data$Gender[39502] <- "M"
demographic_data$Gender[55579] <- "M"
#Marital.Status..at.the.time.of.application. 
#6 empty feilds
which(demographic_data$Marital.Status..at.the.time.of.application. == "")
#[1]  6381 35520 48405 50735 59296 68044
summary(demographic_data$Marital.Status..at.the.time.of.application.)
#   Married  Single 
#6   60730   10559 
#Assigning the Marital status of large number of applicants
demographic_data$Marital.Status..at.the.time.of.application.[6381] <- "Married"
demographic_data$Marital.Status..at.the.time.of.application.[35520] <- "Married"
demographic_data$Marital.Status..at.the.time.of.application.[48405] <- "Married"
demographic_data$Marital.Status..at.the.time.of.application.[50735] <- "Married"
demographic_data$Marital.Status..at.the.time.of.application.[59296] <- "Married"
demographic_data$Marital.Status..at.the.time.of.application.[68044] <- "Married"
#Profession 
#14 empty entries
which(demographic_data$Profession == "")
#[1]  1693 23661 31899 43275 44127 49200 57858 61808 63996 64797 67523 68044 69297
#[14] 71228
summary(demographic_data$Profession)
#     SAL      SE SE_PROF 
#14   40439   14307   16535 
#Assigning the SAL to all the empty values
demographic_data$Profession[1693] <- "SAL"
demographic_data$Profession[23661] <- "SAL"
demographic_data$Profession[31899] <- "SAL"
demographic_data$Profession[43275] <- "SAL"
demographic_data$Profession[44127] <- "SAL"
demographic_data$Profession[49200] <- "SAL"
demographic_data$Profession[57858] <- "SAL"
demographic_data$Profession[61808] <- "SAL"
demographic_data$Profession[63996] <- "SAL"
demographic_data$Profession[64797] <- "SAL"
demographic_data$Profession[67523] <- "SAL"
demographic_data$Profession[68044] <- "SAL"
demographic_data$Profession[69297] <- "SAL"
demographic_data$Profession[71228] <- "SAL"
#Type.of.residence 
#8 empty entries
which(demographic_data$Type.of.residence=="")
#[1]     6  7164  9437 15601 44217 49433 51836 60645
summary(as.factor(demographic_data$Type.of.residence))
#Company provided Living with Parents              Others 
#8                1630                1818                 199 
#Owned              Rented 
#14243               53397 
#Assigning to Rented as it is the most frequent
demographic_data$Type.of.residence[6] <- "Rented"
demographic_data$Type.of.residence[7164] <- "Rented"
demographic_data$Type.of.residence[9437] <- "Rented"
demographic_data$Type.of.residence[15601] <- "Rented"
demographic_data$Type.of.residence[44217] <- "Rented"
demographic_data$Type.of.residence[49433] <- "Rented"
demographic_data$Type.of.residence[51836] <- "Rented"
demographic_data$Type.of.residence[60645] <- "Rented"
#summary(demographic_data$Education)
#         Bachelor      Masters       Others          Phd Professional 
#119        17697        23970          121         4549        24839 
##Since Education has large number of Empty entries; we propose to do a WOE analysis and missing value imputation

###Credit Bureau data Null values
sum(is.na(credit_bureau_data))
#3028
sapply (credit_bureau_data, function (x) sum(is.na(x)))
#Avgas.CC.Utilization.in.last.12.months 
#1058
##the average credit card utilization NA values are 1058; so using WOE analysis to impute the missing values
#No.of.trades.opened.in.last.6.months 
#1
#Assigning 0 instead of an NA value as intuitively NA means no trade which is 0
which(is.na(credit_bureau_data$No.of.trades.opened.in.last.6.months))
#[1] 66596
credit_bureau_data$No.of.trades.opened.in.last.6.months[66596] <- 0
###Checking Empty values
sapply (credit_bureau_data, function (x) sum(x==""|x==" "))
#There are no empty values 

###After the imputations a few missing values remain. They are imputed using WOE analysis
# 
#credit_bureau_data$Presence.of.open.home.loan 
#272 missing
#credit_bureau_data$Outstanding.Balance 
#272 missing
#credit_bureau_data$Avgas.CC.Utilization.in.last.12.months 
#1058 
#demographic_data$Education
#119

###Conveting categorical variables to factors in demographic_data
demographic_data$Gender <- as.factor(demographic_data$Gender)
demographic_data$Marital.Status..at.the.time.of.application. <- as.factor(demographic_data$Marital.Status..at.the.time.of.application.)
demographic_data$No.of.dependents <- as.factor(demographic_data$No.of.dependents)
demographic_data$Education <- as.factor(demographic_data$Education)
demographic_data$Profession <- as.factor(demographic_data$Profession)
demographic_data$Type.of.residence <- as.factor(demographic_data$Type.of.residence)
###Numerical variables in demographic_data are Age,Income,No.of.months.in.current.company,No.of.months.in.current.residence
###These values need analysis
### Age has negative and zero value
###Assuming that the minimum Age for credit card is 18; dropping all other Applicants
lower_age_indices <- as.array(which(demographic_data$Age < 18))
credit_bureau_data <- credit_bureau_data[-lower_age_indices,]
demographic_data <- demographic_data[-lower_age_indices,]
### Negative and zero income rows are removed 107 rows
invalid_income_indices <- as.array(which(demographic_data$Income <= 0))
credit_bureau_data <- credit_bureau_data[-invalid_income_indices,]
demographic_data <- demographic_data[-invalid_income_indices,]
###Conveting categorical variables to factors in credit_bureau_data
credit_bureau_data$Presence.of.open.auto.loan <- as.factor(credit_bureau_data$Presence.of.open.auto.loan)
credit_bureau_data$Presence.of.open.home.loan <- as.factor(credit_bureau_data$Presence.of.open.home.loan)
credit_bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(credit_bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months)
credit_bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(credit_bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months)
credit_bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(credit_bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months)
credit_bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(credit_bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months)
credit_bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(credit_bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months)
credit_bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(credit_bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months)
#Using WOE analysis to impute missing values for Avgas.CC.Utilization.in.last.12.months
### Credit card Utilization cannot be > 100 since it is Outstanding Balance / Total credit line
### Capping the values to 100%
sum(credit_bureau_data$Avgas.CC.Utilization.in.last.12.months > 100,na.rm=T)
# 3627
credit_bureau_data$Avgas.CC.Utilization.in.last.12.months[credit_bureau_data$Avgas.CC.Utilization.in.last.12.months > 100 ] <- 100
#Creating the master file
master_file <- merge(demographic_data,credit_bureau_data)
##Separating the master_file for Accepted and Rejected Candidates
master_file_accepted <- master_file[-which(is.na(master_file$Performance.Tag)),]
master_file_rejected <- master_file[which(is.na(master_file$Performance.Tag)),]
##Similarly for demographic_data
demographic_data_accepted <- master_file[-which(is.na(demographic_data$Performance.Tag)),]
demographic_data_rejected <- master_file[which(is.na(demographic_data$Performance.Tag)),]
##Similarly for credit_bureau_data
credit_bureau_data_accepted <- master_file[-which(is.na(credit_bureau_data$Performance.Tag)),]
credit_bureau_data_rejected <- master_file[which(is.na(credit_bureau_data$Performance.Tag)),]
###########################
#Regular EDA on master_file_accepted
# Creating a column non_default for EDA
master_file_accepted$non_default <- ifelse((master_file_accepted$Performance.Tag == 0), 1, 0)
#Age
ggplot(master_file_accepted,aes(Age))+geom_histogram()
boxplot(master_file_accepted$Age)
master_file_accepted$binning.age <- as.factor(cut(master_file_accepted$Age, breaks = c(10, 20, 30, 40, 50, 60, 70, 80)))
aggregate(non_default ~ binning.age, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.age))
Age_response <- cbind(aggregate(non_default ~ binning.age, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.age))[-7,-1])
colnames(Age_response) <- c("binning.age","non_default","count")
for (i in 1:6) {Age_response$non_default_rate[i] <- (Age_response$non_default[i]/Age_response$count[i])}
ggplot(Age_response, aes(binning.age,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:6) {Age_response$default_rate[i] <- (1-(Age_response$non_default[i]/Age_response$count[i]))}
ggplot(Age_response, aes(binning.age,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
##The age bin 10-20 has 2.7% default; lower than other bins; highest is 4.45% 30-40 bin
#Gender
ggplot(master_file_accepted,aes(Gender))+geom_bar()
aggregate(non_default ~ Gender, master_file_accepted,sum)
data.frame(table(master_file_accepted$Gender))
Gender_response <- cbind(aggregate(non_default ~ Gender, master_file_accepted,sum),data.frame(table(master_file_accepted$Gender))[-1,-1])
colnames(Gender_response) <- c("Gender","non_default","count")
for (i in 1:2) {Gender_response$non_default_rate[i] <- (Gender_response$non_default[i]/Gender_response$count[i])}
ggplot(Gender_response, aes(Gender,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:2) {Gender_response$default_rate[i] <- (1-(Gender_response$non_default[i]/Gender_response$count[i]))}
ggplot(Gender_response, aes(Gender,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
## females have slightly (4.35%) higher default rate than males (4.18%)
#Marital.Status
aggregate(non_default ~ Marital.Status..at.the.time.of.application., master_file_accepted,sum)
data.frame(table(master_file_accepted$Marital.Status..at.the.time.of.application.))
MaritalStatus_response <- cbind(aggregate(non_default ~ Marital.Status..at.the.time.of.application., master_file_accepted,sum),data.frame(table(master_file_accepted$Marital.Status..at.the.time.of.application.))[-1,-1])
colnames(MaritalStatus_response) <- c("MaritalStatus","non_default","count")
for (i in 1:2) {MaritalStatus_response$non_default_rate[i] <- (MaritalStatus_response$non_default[i]/MaritalStatus_response$count[i])}
ggplot(MaritalStatus_response, aes(MaritalStatus,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:2) {MaritalStatus_response$default_rate[i] <- (1-(MaritalStatus_response$non_default[i]/MaritalStatus_response$count[i]))}
ggplot(MaritalStatus_response, aes(MaritalStatus,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of Single is slightly higher(4.31%) than married (4.209%) 
#No.of.dependents
aggregate(non_default ~ No.of.dependents, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.dependents))
Dependent_response <- cbind(aggregate(non_default ~ No.of.dependents, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.dependents))[,-1])
colnames(Dependent_response) <- c("Dependents","non_default","count")
for (i in 1:5) {Dependent_response$non_default_rate[i] <- (Dependent_response$non_default[i]/Dependent_response$count[i])}
ggplot(Dependent_response, aes(Dependents,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:5) {Dependent_response$default_rate[i] <- (1-(Dependent_response$non_default[i]/Dependent_response$count[i]))}
ggplot(Dependent_response, aes(Dependents,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 3 dependents highest 4.45% and 2 dependents lowest 3.89%
#Income
summary(master_file_accepted$Income)
master_file_accepted$binning.income <- as.factor(cut(master_file_accepted$Income, breaks = c(0,5,10,15,20,25,30, 40, 50, 60)))
aggregate(non_default ~ binning.income, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.income))
Income_response <- cbind(aggregate(non_default ~ binning.income, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.income))[,-1])
colnames(Income_response) <- c("binning.income","non_default","count")
for (i in 1:8) {Income_response$non_default_rate[i] <- (Income_response$non_default[i]/Income_response$count[i])}
ggplot(Income_response, aes(binning.income,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:8) {Income_response$default_rate[i] <- (1-(Income_response$non_default[i]/Income_response$count[i]))}
ggplot(Income_response, aes(binning.income,count,label = round((default_rate*100),2))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate decreses as income increses. Income is an important factor. lowest income 5-10 have 
#5.688%; similarly highest income group has 5.688% . The lowest default rate is 3.542% in the income range 30-40
#Education
aggregate(non_default ~ Education, master_file_accepted,sum)
data.frame(table(master_file_accepted$Education))
Education_response <- cbind(aggregate(non_default ~ Education, master_file_accepted,sum),data.frame(table(master_file_accepted$Education))[,-1])
colnames(Education_response) <- c("Education","non_default","count")
for (i in 1:5) {Education_response$non_default_rate[i] <- (Education_response$non_default[i]/Education_response$count[i])}
ggplot(Education_response, aes(Education,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:5) {Education_response$default_rate[i] <- (1-(Education_response$non_default[i]/Education_response$count[i]))}
ggplot(Education_response, aes(Education,count,label = round((default_rate*100),2))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#The NA group has a default rate similar to the Professional group. Highest default is for the "Others" 6.77% and lowest is for the "Professional" 4.23%
#Profession
aggregate(non_default ~ Profession, master_file_accepted,sum)
data.frame(table(master_file_accepted$Profession))
Profession_response <- cbind(aggregate(non_default ~ Profession, master_file_accepted,sum),data.frame(table(master_file_accepted$Profession))[-1,-1])
colnames(Profession_response) <- c("Profession","non_default","count")
for (i in 1:3) {Profession_response$non_default_rate[i] <- (Profession_response$non_default[i]/Profession_response$count[i])}
ggplot(Profession_response, aes(Profession,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:3) {Profession_response$default_rate[i] <- (1-(Profession_response$non_default[i]/Profession_response$count[i]))}
ggplot(Profession_response, aes(Profession,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#Not much variation in Profession. The SAL have lowest default rate 4.11% and highest default rate 4.612% is of SE
#Type.of.residence
aggregate(non_default ~ Type.of.residence, master_file_accepted,sum)
data.frame(table(master_file_accepted$Type.of.residence))
residence_response <- cbind(aggregate(non_default ~ Type.of.residence, master_file_accepted,sum),data.frame(table(master_file_accepted$Type.of.residence))[-1,-1])
colnames(residence_response) <- c("residence","non_default","count")
for (i in 1:5) {residence_response$non_default_rate[i] <- (residence_response$non_default[i]/residence_response$count[i])}
ggplot(residence_response, aes(residence,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:5) {residence_response$default_rate[i] <- (1-(residence_response$non_default[i]/residence_response$count[i]))}
ggplot(residence_response, aes(residence,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#Type.of.residence 4.5625% highest default rate for company provided accomodation,others 2.525%
#4.2% Rented
#No.of.months.in.current.residence
master_file_accepted$binning.residence_months <- as.factor(cut(master_file_accepted$No.of.months.in.current.residence, breaks = c(5,25,45,65,85,105,125,145)))
aggregate(non_default ~ binning.residence_months, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.residence_months))
residence_months_response <- cbind(aggregate(non_default ~ binning.residence_months, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.residence_months))[,-1])
colnames(residence_months_response) <- c("binning.residence_months","non_default","count")
for (i in 1:7) {residence_months_response$non_default_rate[i] <- (residence_months_response$non_default[i]/residence_months_response$count[i])}
ggplot(residence_months_response, aes(binning.residence_months,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:7) {residence_months_response$default_rate[i] <- (1-(residence_months_response$non_default[i]/residence_months_response$count[i]))}
ggplot(residence_months_response, aes(binning.residence_months,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#Default rate lowest in 5-25 months range 3.75% ;Highest in 125-145 months range 8.5714% and 25-45 months range 5.856%
#No.of.months.in.current.company
master_file_accepted$binning.company_months <- as.factor(cut(master_file_accepted$No.of.months.in.current.company, breaks = c(0,20,40,60,80,100,120,140)))
aggregate(non_default ~ binning.company_months, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.company_months))
company_months_response <- cbind(aggregate(non_default ~ binning.company_months, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.company_months))[,-1])
colnames(company_months_response) <- c("binning.company_months","non_default","count")
for (i in 1:7) {company_months_response$non_default_rate[i] <- (company_months_response$non_default[i]/company_months_response$count[i])}
ggplot(company_months_response, aes(binning.company_months,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:7) {company_months_response$default_rate[i] <- (1-(company_months_response$non_default[i]/company_months_response$count[i]))}
ggplot(company_months_response, aes(binning.company_months,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#Default rate highest in 0-20 months range - 4,91% lowest in 40-60 months range - 3.472%
#Oustanding Balance
master_file_accepted$binning.outstanding.balance <- as.factor(cut(master_file_accepted$Outstanding.Balance, breaks = c(0,200000,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000)))
aggregate(non_default ~ binning.outstanding.balance, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.outstanding.balance))
outstanding_balance_response <- cbind(aggregate(non_default ~ binning.outstanding.balance, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.outstanding.balance))[,-1])
colnames(outstanding_balance_response) <- c("binning.outstanding","non_default","count")
for (i in 1:12) {outstanding_balance_response$non_default_rate[i] <- (outstanding_balance_response$non_default[i]/outstanding_balance_response$count[i])}
ggplot(outstanding_balance_response, aes(binning.outstanding,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:12) {outstanding_balance_response$default_rate[i] <- (1-(outstanding_balance_response$non_default[i]/outstanding_balance_response$count[i]))}
ggplot(outstanding_balance_response, aes(binning.outstanding,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
ggplot(outstanding_balance_response, aes(binning.outstanding,round((default_rate*100),2),label = count )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
##Few brackets that have high default rates are 50-55L 16.6% ,45-50L 6.2% 
#Low default rates are in 0-2L 1.856% and 25L-30L 1.583% ranges 
#outstanding_balance has a significant impact on default rate
#Utilization
master_file_accepted$binning.utilization <- as.factor(cut(master_file_accepted$Avgas.CC.Utilization.in.last.12.months, breaks = c(0,10,20,30,40,50,60,70,80,90,100)))
aggregate(non_default ~ binning.utilization, master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.utilization))
utilization_response <- cbind(aggregate(non_default ~ binning.utilization, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.utilization))[,-1])
colnames(utilization_response) <- c("binning.utilization","non_default","count")
for (i in 1:10) {utilization_response$non_default_rate[i] <- (utilization_response$non_default[i]/utilization_response$count[i])}
ggplot(utilization_response, aes(binning.utilization,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:10) {utilization_response$default_rate[i] <- (1-(utilization_response$non_default[i]/utilization_response$count[i]))}
ggplot(utilization_response, aes(binning.utilization,count,label = round((default_rate*100),2))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#40-50% utilization has highest default rates 7.72%,70-80% utilization  7.85% and 10-15% utilization have lowest default rates 1.91%
#Utilization is also a significant factor
#No.of.times.(90).DPD.or.worse.in.last.12.months
aggregate(non_default ~ No.of.times.90.DPD.or.worse.in.last.12.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.90.DPD.or.worse.in.last.12.months))
Dpd_90_12_response <- cbind(aggregate(non_default ~ No.of.times.90.DPD.or.worse.in.last.12.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.90.DPD.or.worse.in.last.12.months))[,-1])
colnames(Dpd_90_12_response) <- c("Dpd_90_12","non_default","count")
for (i in 1:6) {Dpd_90_12_response$non_default_rate[i] <- (Dpd_90_12_response$non_default[i]/Dpd_90_12_response$count[i])}
ggplot(Dpd_90_12_response, aes(Dpd_90_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:6) {Dpd_90_12_response$default_rate[i] <- (1-(Dpd_90_12_response$non_default[i]/Dpd_90_12_response$count[i]))}
ggplot(Dpd_90_12_response, aes(Dpd_90_12,count,label = round((default_rate*100),2 ))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 5-90dpd highest at 13.88% 0-90DPD lowest 2.99%. rises to 6.826 at 1-90DPD ; 90 DPD is a strong indicator of default as suggested in problem statement
#No.of.times.(60).DPD.or.worse.in.last.12.months
aggregate(non_default ~ No.of.times.60.DPD.or.worse.in.last.12.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.60.DPD.or.worse.in.last.12.months))
Dpd_60_12_response <- cbind(aggregate(non_default ~ No.of.times.60.DPD.or.worse.in.last.12.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.60.DPD.or.worse.in.last.12.months))[,-1])
colnames(Dpd_60_12_response) <- c("Dpd_60_12","non_default","count")
for (i in 1:8) {Dpd_60_12_response$non_default_rate[i] <- (Dpd_60_12_response$non_default[i]/Dpd_60_12_response$count[i])}
ggplot(Dpd_60_12_response, aes(Dpd_60_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:8) {Dpd_60_12_response$default_rate[i] <- (1-(Dpd_60_12_response$non_default[i]/Dpd_60_12_response$count[i]))}
ggplot(Dpd_60_12_response, aes(Dpd_60_12,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 6-60dpd highest at 9.04% 0-60DPD lowest 3%. rises to 5.17% at 1-60 DPD
#60 DPD has a similar trend as 90 DPD...default % are higher at 90 DPD
#No.of.times.(30).DPD.or.worse.in.last.12.months
aggregate(non_default ~ No.of.times.30.DPD.or.worse.in.last.12.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.30.DPD.or.worse.in.last.12.months))
Dpd_30_12_response <- cbind(aggregate(non_default ~ No.of.times.30.DPD.or.worse.in.last.12.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.30.DPD.or.worse.in.last.12.months))[,-1])
colnames(Dpd_30_12_response) <- c("Dpd_30_12","non_default","count")
for (i in 1:10) {Dpd_30_12_response$non_default_rate[i] <- (Dpd_30_12_response$non_default[i]/Dpd_30_12_response$count[i])}
ggplot(Dpd_30_12_response, aes(Dpd_30_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:10) {Dpd_30_12_response$default_rate[i] <- (1-(Dpd_30_12_response$non_default[i]/Dpd_30_12_response$count[i]))}
ggplot(Dpd_30_12_response, aes(Dpd_30_12,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 5-30dpd highest at 10.44% 0-30DPD lowest 2.9%. rises to 4.5204% at 1-30 DPD
#30 DPD has a similar trend as 60 DPD...default % are higher at 60 DPD

#No.of.times.(90).DPD.or.worse.in.last.6.months
aggregate(non_default ~ No.of.times.90.DPD.or.worse.in.last.6.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.90.DPD.or.worse.in.last.6.months))
Dpd_90_6_response <- cbind(aggregate(non_default ~ No.of.times.90.DPD.or.worse.in.last.6.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.90.DPD.or.worse.in.last.6.months))[,-1])
colnames(Dpd_90_6_response) <- c("Dpd_90_6","non_default","count")
for (i in 1:4) {Dpd_90_6_response$non_default_rate[i] <- (Dpd_90_6_response$non_default[i]/Dpd_90_6_response$count[i])}
ggplot(Dpd_90_6_response, aes(Dpd_90_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:4) {Dpd_90_6_response$default_rate[i] <- (1-(Dpd_90_6_response$non_default[i]/Dpd_90_6_response$count[i]))}
ggplot(Dpd_90_6_response, aes(Dpd_90_6,count,label = round((default_rate*100),2 ))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 3-90dpd highest at 11.05% 0-90DPD lowest 3.285%. rises to 7.3488% at 1-90DPD ; 90 DPD is a strong indicator of default as suggested in problem statement
#No.of.times.(60).DPD.or.worse.in.last.6.months
aggregate(non_default ~ No.of.times.60.DPD.or.worse.in.last.6.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.60.DPD.or.worse.in.last.6.months))
Dpd_60_6_response <- cbind(aggregate(non_default ~ No.of.times.60.DPD.or.worse.in.last.6.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.60.DPD.or.worse.in.last.6.months))[,-1])
colnames(Dpd_60_6_response) <- c("Dpd_60_6","non_default","count")
for (i in 1:6) {Dpd_60_6_response$non_default_rate[i] <- (Dpd_60_6_response$non_default[i]/Dpd_60_6_response$count[i])}
ggplot(Dpd_60_6_response, aes(Dpd_60_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:6) {Dpd_60_6_response$default_rate[i] <- (1-(Dpd_60_6_response$non_default[i]/Dpd_60_6_response$count[i]))}
ggplot(Dpd_60_6_response, aes(Dpd_60_6,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 3-60dpd highest at 10.08% 0-60DPD lowest 3%. rises to 7.05% at 1-60 DPD
#60 DPD has a similar trend as 90 DPD...default % are higher at 90 DPD
#No.of.times.(30).DPD.or.worse.in.last.6.months
aggregate(non_default ~ No.of.times.30.DPD.or.worse.in.last.6.months, master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.times.30.DPD.or.worse.in.last.6.months))
Dpd_30_6_response <- cbind(aggregate(non_default ~ No.of.times.30.DPD.or.worse.in.last.6.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.times.30.DPD.or.worse.in.last.6.months))[,-1])
colnames(Dpd_30_6_response) <- c("Dpd_30_6","non_default","count")
for (i in 1:8) {Dpd_30_6_response$non_default_rate[i] <- (Dpd_30_6_response$non_default[i]/Dpd_30_6_response$count[i])}
ggplot(Dpd_30_6_response, aes(Dpd_30_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:8) {Dpd_30_6_response$default_rate[i] <- (1-(Dpd_30_6_response$non_default[i]/Dpd_30_6_response$count[i]))}
ggplot(Dpd_30_6_response, aes(Dpd_30_6,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of 5-30dpd highest at 11.19% 0-30DPD lowest 2.9%. rises to 6.56% at 1-30 DPD
#30 DPD has a similar trend as 60 DPD 

#Home loan
aggregate(non_default ~ Presence.of.open.home.loan, master_file_accepted,sum)
data.frame(table(master_file_accepted$Presence.of.open.home.loan))
Home_loan_response <- cbind(aggregate(non_default ~ Presence.of.open.home.loan, master_file_accepted,sum),data.frame(table(master_file_accepted$Presence.of.open.home.loan))[,-1])
colnames(Home_loan_response) <- c("Home_loan","non_default","count")
for (i in 1:2) {Home_loan_response$non_default_rate[i] <- (Home_loan_response$non_default[i]/Home_loan_response$count[i])}
ggplot(Home_loan_response, aes(Home_loan,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:2) {Home_loan_response$default_rate[i] <- (1-(Home_loan_response$non_default[i]/Home_loan_response$count[i]))}
ggplot(Home_loan_response, aes(Home_loan,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of those with open home loan is 3.3685% while with no home loan is 4.53%. Home loan takers are less likely to default
# Auto loan
aggregate(non_default ~ Presence.of.open.auto.loan, master_file_accepted,sum)
data.frame(table(master_file_accepted$Presence.of.open.auto.loan))
Auto_loan_response <- cbind(aggregate(non_default ~ Presence.of.open.auto.loan, master_file_accepted,sum),data.frame(table(master_file_accepted$Presence.of.open.auto.loan))[,-1])
colnames(Auto_loan_response) <- c("Auto_loan","non_default","count")
for (i in 1:2) {Auto_loan_response$non_default_rate[i] <- (Auto_loan_response$non_default[i]/Auto_loan_response$count[i])}
ggplot(Auto_loan_response, aes(Auto_loan,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:2) {Auto_loan_response$default_rate[i] <- (1-(Auto_loan_response$non_default[i]/Auto_loan_response$count[i]))}
ggplot(Auto_loan_response, aes(Auto_loan,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of those with open auto loan is 3.70% while with no auto loan is 4.273%. Auto loan takers are less likely to default; similar trend to home loan

##Inquiries for loan 12
aggregate(non_default ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
Inquiries_12_response<- cbind(aggregate(non_default ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))[,-1])
colnames(Inquiries_12_response) <- c("Inquiries_12","non_default","count")
for (i in 1:21) {Inquiries_12_response$non_default_rate[i] <- (Inquiries_12_response$non_default[i]/Inquiries_12_response$count[i])}
ggplot(Inquiries_12_response, aes(Inquiries_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:21) {Inquiries_12_response$default_rate[i] <- (1-(Inquiries_12_response$non_default[i]/Inquiries_12_response$count[i]))}
ggplot(Inquiries_12_response, aes(Inquiries_12,count,label = round((default_rate*100),2 ))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of those with 0 inquiries is the lowest with 1.48% while with 5 inquiries highest at 7.35%

##Inquiries for loan 6
aggregate(non_default ~ No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))
Inquiries_6_response<- cbind(aggregate(non_default ~ No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))[,-1])
colnames(Inquiries_6_response) <- c("Inquiries_6","non_default","count")
for (i in 1:11) {Inquiries_6_response$non_default_rate[i] <- (Inquiries_6_response$non_default[i]/Inquiries_6_response$count[i])}
ggplot(Inquiries_6_response, aes(Inquiries_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:11) {Inquiries_6_response$default_rate[i] <- (1-(Inquiries_6_response$non_default[i]/Inquiries_6_response$count[i]))}
ggplot(Inquiries_6_response, aes(Inquiries_6,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rate of those with 0 inquiries is the lowest with 2.1% while with 4 inquiries highest at 7.13%; This trend continues in the 12 months data

#No.of.PL.trades.opened.in.last.12.months
aggregate(non_default ~ No.of.PL.trades.opened.in.last.12.months,master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.PL.trades.opened.in.last.12.months))
PL_12_response<- cbind(aggregate(non_default ~ No.of.PL.trades.opened.in.last.12.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.PL.trades.opened.in.last.12.months))[,-1])
colnames(PL_12_response) <- c("PL_12","non_default","count")
for (i in 1:13) {PL_12_response$non_default_rate[i] <- (PL_12_response$non_default[i]/PL_12_response$count[i])}
ggplot(PL_12_response, aes(PL_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:13) {PL_12_response$default_rate[i] <- (1-(PL_12_response$non_default[i]/PL_12_response$count[i]))}
ggplot(PL_12_response, aes(PL_12,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
# default rate of those with 0 PL trades is lowest at 1.76% and those with 5 is highest at 6.77%
# Those with 0 PL trades have very low default rate
#No.of.PL.trades.opened.in.last.6.months
aggregate(non_default ~ No.of.PL.trades.opened.in.last.6.months,master_file_accepted,sum)
data.frame(table(master_file_accepted$No.of.PL.trades.opened.in.last.6.months))
PL_6_response<- cbind(aggregate(non_default ~ No.of.PL.trades.opened.in.last.6.months, master_file_accepted,sum),data.frame(table(master_file_accepted$No.of.PL.trades.opened.in.last.6.months))[,-1])
colnames(PL_6_response) <- c("PL_6","non_default","count")
for (i in 1:7) {PL_6_response$non_default_rate[i] <- (PL_6_response$non_default[i]/PL_6_response$count[i])}
ggplot(PL_6_response, aes(PL_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:7) {PL_6_response$default_rate[i] <- (1-(PL_6_response$non_default[i]/PL_6_response$count[i]))}
ggplot(PL_6_response, aes(PL_6,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
# default rate of those with 0 PL trades is lowest at 2.25% and those with 2 is highest at 6.39%

#No.of.trades.opened.in.last.12.months

master_file_accepted$binning.trades12 <- as.factor(cut(master_file_accepted$No.of.trades.opened.in.last.12.months, breaks = c(0,5,15,20,25,30),include.lowest = TRUE))
aggregate(non_default ~ binning.trades12,master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.trades12))
T_12_response<- cbind(aggregate(non_default ~ binning.trades12, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.trades12))[,-1])
colnames(T_12_response) <- c("T_12","non_default","count")
for (i in 1:5) {T_12_response$non_default_rate[i] <- (T_12_response$non_default[i]/T_12_response$count[i])}
ggplot(T_12_response, aes(T_12,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:5) {T_12_response$default_rate[i] <- (1-(T_12_response$non_default[i]/T_12_response$count[i]))}
ggplot(T_12_response, aes(T_12,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
# default rate of those with 0-5 T12 trades is lowest at 2.863% and those with 5-15 is highest at 6.39%

#No.of.trades.opened.in.last.6.months
##1 NA value needs imputation 
master_file_accepted$No.of.trades.opened.in.last.6.months[1803] <- 2
master_file_accepted$binning.trades6 <- as.factor(cut(master_file_accepted$No.of.trades.opened.in.last.6.months, breaks = c(0,2,4,6,8,10,12),include.lowest = TRUE))
aggregate(non_default ~ binning.trades6,master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.trades6))
T_6_response<- cbind(aggregate(non_default ~ binning.trades6, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.trades6))[,-1])
colnames(T_6_response) <- c("T_6","non_default","count")
for (i in 1:6) {T_6_response$non_default_rate[i] <- (T_6_response$non_default[i]/T_6_response$count[i])}
ggplot(T_6_response, aes(T_6,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:6) {T_6_response$default_rate[i] <- (1-(T_6_response$non_default[i]/T_6_response$count[i]))}
ggplot(T_6_response, aes(T_6,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
# default rate of those with 8-10 and 10-12 buckets  T6 trades is lowest at 2.63%/3.271% and those with 2-4 is highest at 6.59%

#Total.No.of.Trades
master_file_accepted$binning.total.trades <- as.factor(cut(master_file_accepted$Total.No.of.Trades, breaks = c(0,5,10,15,20,25,30,35,40,45),include.lowest = TRUE))
aggregate(non_default ~ binning.total.trades,master_file_accepted,sum)
data.frame(table(master_file_accepted$binning.total.trades))
Total_trades_response<- cbind(aggregate(non_default ~ binning.total.trades, master_file_accepted,sum),data.frame(table(master_file_accepted$binning.total.trades))[,-1])
colnames(Total_trades_response) <- c("Total_trades","non_default","count")
for (i in 1:9) {Total_trades_response$non_default_rate[i] <- (Total_trades_response$non_default[i]/Total_trades_response$count[i])}
ggplot(Total_trades_response, aes(Total_trades,count,label = (non_default_rate*100) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
for (i in 1:9) {Total_trades_response$default_rate[i] <- (1-(Total_trades_response$non_default[i]/Total_trades_response$count[i]))}
ggplot(Total_trades_response, aes(Total_trades,count,label = round((default_rate*100),2) )) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)
#default rates are highest in the 10-15 trades bucket 7.14% and lowest in the bucket 0-5 trades 2.442% and 35-40 trades 1.92%


##################################################################################################
#Missing data imputed based on WOE value
# The following feilds had significant missing data 
#Education,binning.utilization,Presence.of.open.home.loan,binning.outstanding.balance
#Education 
######Education     N     Percent          WOE               IV
#1                118 0.001692994  0.002955497 0.00000001480827
#2     Bachelor 17268 0.247751044  0.014744398 0.00005424020753
#3      Masters 23433 0.336202815  0.008281161 0.00007738379863
#4       Others   118 0.001692994  0.499866579 0.00061155485318
#5          Phd  4453 0.063889008 -0.023293726 0.00064585361354
#6 Professional 24309 0.348771144 -0.017554707 0.00075247412736
# 
#Education_info_table <- create_infotables(data=master_file_accepted_IV[,c(1,7)], y="Performance.Tag")
#Education_info_table[["Tables"]][["Education"]]
#### Thw NA values' WOE is 0.002955497, closer to Masters 0.008281161
#master_file_accepted_IV$Education[which(master_file_accepted_IV$Education == "")] <- "Masters"
#binning.utilization
#binning.utilization_info_table <- create_infotables(data=master_file_accepted_IV[,c(1,34)], y="Performance.Tag")
#binning.utilization_info_table[["Tables"]][["binning.utilization"]]
#The WOE of the NA values (0.08224809) is closest to (90,100] bin (0.18151567)
#master_file_accepted_IV$binning.utilization[which(is.na(master_file_accepted_IV$binning.utilization))] <- "(90,100]"
#Presence.of.open.home.loan
#Presence.of.open.home.loan_info_table <- create_infotables(data=master_file_accepted_IV[,c(1,25)], y="Performance.Tag")
#Presence.of.open.home.loan_info_table[["Tables"]][["Presence.of.open.home.loan"]]
#WOE is closest to 1; so replacing all NA values with 1
#master_file_accepted_IV$Presence.of.open.home.loan[which(master_file_accepted_IV$Presence.of.open.home.loan == "")] <- "1"
#binning.outstanding.balance
#binning.outstanding.balance_info_table <- create_infotables(data=master_file_accepted_IV[,c(1,33)], y="Performance.Tag")
#binning.outstanding.balance_info_table[["Tables"]][["binning.outstanding.balance"]]
# WOE of NA (-0.032219044) is closest to the bin (1.5e+06,2e+06] (-0.018989951)
#master_file_accepted_IV$binning.outstanding.balance[which(master_file_accepted_IV$binning.outstanding.balance == "")] <- "(1.5e+06,2e+06]"

###Bivariate
#Bivariate Analysis: Treemaps
### Imputing the NA values based on the WOE using create_infotables...commented section above
master_file_accepted_bivariate <- master_file_accepted[,c(-1,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39)]
master_file_accepted_bivariate$Presence.of.open.home.loan[which(is.na(master_file_accepted_bivariate$Presence.of.open.home.loan))] <- 0
master_file_accepted_bivariate$Outstanding.Balance[which(is.na(master_file_accepted_bivariate$Outstanding.Balance))] <- 1.75e+06
master_file_accepted_bivariate$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_file_accepted_bivariate$Avgas.CC.Utilization.in.last.12.months))] <- 95
#sapply(master_file_accepted_bivariate,function(x) x <- as.numeric(x))
master_file_accepted_bivariate$Performance.Tag <- as.numeric(master_file_accepted_bivariate$Performance.Tag)
master_file_accepted_bivariate$Gender <- as.numeric(master_file_accepted_bivariate$Gender)
master_file_accepted_bivariate$Marital.Status..at.the.time.of.application <- as.numeric(master_file_accepted_bivariate$Marital.Status..at.the.time.of.application)
master_file_accepted_bivariate$No.of.dependents <- as.numeric(master_file_accepted_bivariate$No.of.dependents)
master_file_accepted_bivariate$Education <- as.numeric(master_file_accepted_bivariate$Education)
master_file_accepted_bivariate$Profession <- as.numeric(master_file_accepted_bivariate$Profession)
master_file_accepted_bivariate$Type.of.residence <- as.numeric(master_file_accepted_bivariate$Type.of.residence)
master_file_accepted_bivariate$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_bivariate$No.of.times.90.DPD.or.worse.in.last.6.months)
master_file_accepted_bivariate$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_bivariate$No.of.times.60.DPD.or.worse.in.last.6.months)
master_file_accepted_bivariate$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_bivariate$No.of.times.30.DPD.or.worse.in.last.6.months)
master_file_accepted_bivariate$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_bivariate$No.of.times.90.DPD.or.worse.in.last.12.months)
master_file_accepted_bivariate$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_bivariate$No.of.times.60.DPD.or.worse.in.last.12.months)
master_file_accepted_bivariate$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_bivariate$No.of.times.30.DPD.or.worse.in.last.12.months)
master_file_accepted_bivariate$Presence.of.open.home.loan <- as.numeric(master_file_accepted_bivariate$Presence.of.open.home.loan)
master_file_accepted_bivariate$Presence.of.open.auto.loan <- as.numeric(master_file_accepted_bivariate$Presence.of.open.auto.loan)
master_file_accepted_bivariate$Age <- as.numeric(master_file_accepted_bivariate$Age)
master_file_accepted_bivariate$No.of.months.in.current.residence <- as.numeric(master_file_accepted_bivariate$No.of.months.in.current.residence)
master_file_accepted_bivariate$No.of.months.in.current.company <- as.numeric(master_file_accepted_bivariate$No.of.months.in.current.company)
master_file_accepted_bivariate$Outstanding.Balance <- as.numeric(master_file_accepted_bivariate$Outstanding.Balance)
master_file_accepted_bivariate$No.of.trades.opened.in.last.12.months <- as.numeric(master_file_accepted_bivariate$No.of.trades.opened.in.last.12.months)
master_file_accepted_bivariate$No.of.PL.trades.opened.in.last.6.months <- as.numeric(master_file_accepted_bivariate$No.of.PL.trades.opened.in.last.6.months)
master_file_accepted_bivariate$No.of.PL.trades.opened.in.last.12.months <- as.numeric(master_file_accepted_bivariate$No.of.PL.trades.opened.in.last.12.months)
master_file_accepted_bivariate$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.numeric(master_file_accepted_bivariate$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
master_file_accepted_bivariate$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.numeric(master_file_accepted_bivariate$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
master_file_accepted_bivariate$Total.No.of.Trades <- as.numeric(master_file_accepted_bivariate$Total.No.of.Trades)
master_file_accepted_bivariate$Avgas.CC.Utilization.in.last.12.months <- as.numeric(master_file_accepted_bivariate$Avgas.CC.Utilization.in.last.12.months)
master_file_accepted_bivariate$Outstanding.Balance <- as.numeric(master_file_accepted_bivariate$Outstanding.Balance)
master_file_accepted_bivariate <- master_file_accepted_bivariate[,c(-4)]
cormat <- round(cor(master_file_accepted_bivariate),2)
head(cormat,17)

melted_cormat <- melt(cormat)
head(melted_cormat)
#TReemap Expand the window to see the labels
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Correlation matrix

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)



ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))  

###Done!


###Regular EDA Completed ######
#WOE and information value analysis on master_file_accepted
#For info tables we dont need Application.ID,non_default
### Removing all bins created during the EDA
#####Logistic regression#############
master_file_accepted_IV <- master_file_accepted[,c(-1,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39)]

master_file_accepted_IV$Gender <- as.factor(master_file_accepted_IV$Gender)
master_file_accepted_IV$Marital.Status..at.the.time.of.application <- as.factor(master_file_accepted_IV$Marital.Status..at.the.time.of.application)
master_file_accepted_IV$No.of.dependents <- as.numeric(master_file_accepted_IV$No.of.dependents)
master_file_accepted_IV$Education <- as.factor(master_file_accepted_IV$Education)
master_file_accepted_IV$Profession <- as.factor(master_file_accepted_IV$Profession)
master_file_accepted_IV$Type.of.residence <- as.factor(master_file_accepted_IV$Type.of.residence)
master_file_accepted_IV$Presence.of.open.home.loan <- as.factor(master_file_accepted_IV$Presence.of.open.home.loan)
master_file_accepted_IV$Presence.of.open.auto.loan <- as.factor(master_file_accepted_IV$Presence.of.open.auto.loan)
master_file_accepted_IV$Age <- as.numeric(master_file_accepted_IV$Age)

master_file_accepted_IV$No.of.dependents <- as.numeric(master_file_accepted_IV$No.of.dependents)

master_file_accepted_IV$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_IV$No.of.times.90.DPD.or.worse.in.last.6.months)
master_file_accepted_IV$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_IV$No.of.times.60.DPD.or.worse.in.last.6.months)
master_file_accepted_IV$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(master_file_accepted_IV$No.of.times.30.DPD.or.worse.in.last.6.months)
master_file_accepted_IV$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_IV$No.of.times.90.DPD.or.worse.in.last.12.months)
master_file_accepted_IV$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_IV$No.of.times.60.DPD.or.worse.in.last.12.months)
master_file_accepted_IV$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(master_file_accepted_IV$No.of.times.30.DPD.or.worse.in.last.12.months)

master_file_accepted_IV$Age <- as.numeric(master_file_accepted_IV$Age)
master_file_accepted_IV$No.of.months.in.current.residence <- as.numeric(master_file_accepted_IV$No.of.months.in.current.residence)
master_file_accepted_IV$No.of.months.in.current.company <- as.numeric(master_file_accepted_IV$No.of.months.in.current.company)
master_file_accepted_IV$Outstanding.Balance <- as.numeric(master_file_accepted_IV$Outstanding.Balance)
master_file_accepted_IV$No.of.trades.opened.in.last.12.months <- as.numeric(master_file_accepted_IV$No.of.trades.opened.in.last.12.months)
master_file_accepted_IV$No.of.PL.trades.opened.in.last.6.months <- as.numeric(master_file_accepted_IV$No.of.PL.trades.opened.in.last.6.months)
master_file_accepted_IV$No.of.PL.trades.opened.in.last.12.months <- as.numeric(master_file_accepted_IV$No.of.PL.trades.opened.in.last.12.months)
master_file_accepted_IV$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.numeric(master_file_accepted_IV$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
master_file_accepted_IV$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.numeric(master_file_accepted_IV$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
master_file_accepted_IV$Total.No.of.Trades <- as.numeric(master_file_accepted_IV$Total.No.of.Trades)
master_file_accepted_IV$Avgas.CC.Utilization.in.last.12.months <- as.numeric(master_file_accepted_IV$Avgas.CC.Utilization.in.last.12.months)
master_file_accepted_IV$Outstanding.Balance <- as.numeric(master_file_accepted_IV$Outstanding.Balance)

master_file_accepted_IV$Presence.of.open.home.loan[which(is.na(master_file_accepted_IV$Presence.of.open.home.loan))] <- 0
master_file_accepted_IV$Outstanding.Balance[which(is.na(master_file_accepted_IV$Outstanding.Balance))] <- 1.75e+06
master_file_accepted_IV$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_file_accepted_IV$Avgas.CC.Utilization.in.last.12.months))] <- 95


binning <- woe.tree.binning(master_file_accepted_IV,"Performance.Tag",master_file_accepted_IV)
woe.binning.plot(binning,multiple.plots = "FALSE")
master_file_accepted_woe <- woe.binning.deploy(master_file_accepted_IV,binning,add.woe.or.dum.var="woe")

master_file_accepted_IV <- master_file_accepted_woe[,c(1,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83)]

###Create demographic_data_accepted_IV from master_file_accepted_IV
demographic_data_accepted_IV <- master_file_accepted_IV[,c(1,17:19,21:23,25:28)]
set.seed(101)

split_indices <- sample.split(demographic_data_accepted_IV$Performance.Tag , SplitRatio = 0.70)
train_demographic_file <- demographic_data_accepted_IV[split_indices, ]
test_demographic_file <- demographic_data_accepted_IV[!split_indices, ]
train_demographic_file$Performance.Tag <- as.factor(train_demographic_file$Performance.Tag)
test_demographic_file$Performance.Tag <- as.factor(test_demographic_file$Performance.Tag)


#The data is unbalanced as the 1s are 2062 while 0s are 46728 ; 4.412% 1s and 95.58% 0s
##using SMOTE to balance data before modelling

train_demographic_smote <- SMOTE(Performance.Tag ~ .,train_demographic_file, perc.over = 800,perc.under=120) 

logistic_demo_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_demographic_smote)
summary(logistic_demo_1)
vif(logistic_demo_1)

#Removing the woe.Marital.Status..at.the.time.of.application.binned as p value is 0.98656

logistic_demo_2 <- glm(Performance.Tag ~ woe.No.of.months.in.current.residence.binned + woe.Income.binned + woe.No.of.months.in.current.company.binned + woe.Age.binned + 
                         woe.No.of.dependents.binned + woe.Profession.binned + woe.Type.of.residence.binned + woe.Education.binned + woe.Gender.binned, family = "binomial", data = train_demographic_smote)
summary(logistic_demo_2)
vif(logistic_demo_2)

#Removing the woe.Education.binned as p value is 0.62812

logistic_demo_3 <- glm(Performance.Tag ~ woe.No.of.months.in.current.residence.binned + woe.Income.binned + woe.No.of.months.in.current.company.binned + woe.Age.binned + 
                         woe.No.of.dependents.binned + woe.Profession.binned + woe.Type.of.residence.binned + woe.Gender.binned, family = "binomial", data = train_demographic_smote)
summary(logistic_demo_3)
vif(logistic_demo_3)
### 9 variables of significance

predictions_logit_demo <- predict(logistic_demo_3, newdata = test_demographic_file[,-1], type = "response")
summary(predictions_logit_demo)
#Mean 0.4667
#--------------------------------------------------------- 
###### Model Evaluation: Logistic Regression using demographic data

# Let's use the probability cutoff of 46.444% (close to the mean )

predicted_response_demo <- factor(ifelse(predictions_logit_demo >= 0.4667, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response_demo, test_demographic_file$Performance.Tag, positive = "1")

conf
#Accuracy : 0.5404   Sensitivity : 0.63080  Specificity : 0.53640
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response_demo <- factor(ifelse(predictions_logit_demo >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response_demo, test_demographic_file$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
#---------------------------------------------------------    

# Creating cutoff values from 0 to 1 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0,1,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.035)]


# Let's choose a cutoff value of 48.48% for final model

predicted_response_demo <- factor(ifelse(predictions_logit_demo >= 0.48, "1", "0"))

conf_final <- confusionMatrix(predicted_response_demo, test_demographic_file$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#Accuracy 
#0.5791764 
sens
#Sensitivity 
#0.5889015 
spec
#Specificity 
#0.5787476


######Discriminatory power of model

test_demographic_file <- cbind(test_demographic_file,predictions_logit_demo)
arrange(test_demographic_file,desc(predictions_logit_demo))
test_demographic_file$predicted_response_demo <- factor(ifelse(test_demographic_file$predictions_logit_demo >= 0.47, "1", "0"))

pred_object_test<- prediction(as.numeric(test_demographic_file$predicted_response_demo),as.numeric(test_demographic_file$Performance.Tag))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
### 0.1684814
##the K statistic is 16.85% 
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 

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
arrange(test_demographic_file,desc(predictions_logit_demo))

attrition_decile = lift(test_demographic_file$Performance.Tag, test_demographic_file$predictions_logit_demo, groups = 10)

ggplot(attrition_decile,aes(x=bucket,y=Gain)) + geom_line()

ggplot(attrition_decile,aes(x=bucket,y=Cumlift)) + geom_line()

### Splitting between train and test master_file
set.seed(101)

split_indices <- sample.split(master_file_accepted_IV$Performance.Tag , SplitRatio = 0.70)
train_master_file <- master_file_accepted_IV[split_indices, ]
test_master_file <- master_file_accepted_IV[!split_indices, ]
train_master_file$Performance.Tag <- as.factor(train_master_file$Performance.Tag)
test_master_file$Performance.Tag <- as.factor(test_master_file$Performance.Tag)
#The data is unbalanced as the 1s are 2062 while 0s are 46728 ; 4.412% 1s and 95.58% 0s
##using SMOTE to balance data before modelling

train_master_smote <- SMOTE(Performance.Tag ~ .,train_master_file, perc.over = 800,perc.under=120) 
#> nrow(train_master_smote)
#[1] 38353
#> length(which(train_master_smote$Performance.Tag==0))
#[1] 19795
#> length(which(train_master_smote$Performance.Tag==1))
#[1] 18558
###Logistic regression on the master_file
logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_master_smote)
summary(logistic_1)
vif(logistic_1)
#Using stepAIC to reduce the variables
logistic_2 <- stepAIC(logistic_1, direction = "both")
logistic_3 <- glm(Performance.Tag ~ woe.Avgas.CC.Utilization.in.last.12.months.binned + 
  woe.No.of.PL.trades.opened.in.last.12.months.binned + woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned + 
  woe.No.of.trades.opened.in.last.12.months.binned + woe.Outstanding.Balance.binned + 
  woe.Total.No.of.Trades.binned + woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned + 
  woe.No.of.PL.trades.opened.in.last.6.months.binned + woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned + 
  woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned + 
  woe.No.of.trades.opened.in.last.6.months.binned + woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned + 
  woe.Income.binned + woe.No.of.months.in.current.company.binned + 
  woe.Age.binned + woe.No.of.dependents.binned + woe.Profession.binned + 
  woe.Presence.of.open.auto.loan.binned + woe.Type.of.residence.binned + 
  woe.Marital.Status..at.the.time.of.application.binned, family = "binomial", data = train_master_smote)
summary(logistic_3)
vif(logistic_3)
#woe.No.of.trades.opened.in.last.12.months.binned  vif = 6.200693  and p = 0.138457 
logistic_3 <- glm(Performance.Tag ~ woe.Avgas.CC.Utilization.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.12.months.binned + woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned + 
                    + woe.Outstanding.Balance.binned + 
                    woe.Total.No.of.Trades.binned + woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.6.months.binned + woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned + 
                    woe.No.of.trades.opened.in.last.6.months.binned + woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned + 
                    woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                    woe.Age.binned + woe.No.of.dependents.binned + woe.Profession.binned + 
                    woe.Presence.of.open.auto.loan.binned + woe.Type.of.residence.binned + 
                    woe.Marital.Status..at.the.time.of.application.binned, family = "binomial", data = train_master_smote)
summary(logistic_3)
vif(logistic_3)
#Removing woe.Total.No.of.Trades.binned as p = 0.236918
logistic_4 <- glm(Performance.Tag ~ woe.Avgas.CC.Utilization.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.12.months.binned + woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned + 
                    + woe.Outstanding.Balance.binned + woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.6.months.binned + woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned + 
                    woe.No.of.trades.opened.in.last.6.months.binned + woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned + 
                    woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                    woe.Age.binned + woe.No.of.dependents.binned + woe.Profession.binned + 
                    woe.Presence.of.open.auto.loan.binned + woe.Type.of.residence.binned + 
                    woe.Marital.Status..at.the.time.of.application.binned, family = "binomial", data = train_master_smote)
summary(logistic_4)
vif(logistic_4)
#Removing woe.No.of.trades.opened.in.last.6.months.binned as p = 0.067902
logistic_5 <- glm(Performance.Tag ~ woe.Avgas.CC.Utilization.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.12.months.binned + woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned + 
                    + woe.Outstanding.Balance.binned + woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.6.months.binned + woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned + 
                    woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned + 
                    woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                    woe.Age.binned + woe.No.of.dependents.binned + woe.Profession.binned + 
                    woe.Presence.of.open.auto.loan.binned + woe.Type.of.residence.binned + 
                    woe.Marital.Status..at.the.time.of.application.binned, family = "binomial", data = train_master_smote)
summary(logistic_5)
vif(logistic_5)
#Removing woe.Profession.binned as p = 0.063271
logistic_6 <- glm(Performance.Tag ~ woe.Avgas.CC.Utilization.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.12.months.binned + woe.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..binned + 
                    + woe.Outstanding.Balance.binned + woe.No.of.times.30.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.PL.trades.opened.in.last.6.months.binned + woe.No.of.times.60.DPD.or.worse.in.last.12.months.binned + 
                    woe.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..binned + 
                    woe.No.of.times.90.DPD.or.worse.in.last.6.months.binned + 
                    woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                    woe.Age.binned + woe.No.of.dependents.binned + 
                    woe.Presence.of.open.auto.loan.binned + woe.Type.of.residence.binned + 
                    woe.Marital.Status..at.the.time.of.application.binned, family = "binomial", data = train_master_smote)
summary(logistic_6)
vif(logistic_6)
### 16 variables of 3* significance

predictions_logit <- predict(logistic_6, newdata = test_master_file[,-1], type = "response")
summary(predictions_logit)
#Mean 0.43960
#--------------------------------------------------------- 
###### Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 43.96% (close to the mean )

predicted_response <- factor(ifelse(predictions_logit >= 0.43960, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test_master_file$Performance.Tag, positive = "1")

conf
#Accuracy : 0.518  Sensitivity : 0.76331  Specificity :  0.50714
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, test_master_file$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
#---------------------------------------------------------    

# Creating cutoff values from 0 to 1 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0,1,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.035)]


# Let's choose a cutoff value of 52.02% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.52, "1", "0"))

conf_final <- confusionMatrix(predicted_response, test_master_file$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#Accuracy 
#0.6192549
sens
#Sensitivity 
#0.6330691
spec
#Specificity 
#0.6186458 


######Discriminatory power of model

test_master_file <- cbind(test_master_file,predictions_logit)
arrange(test_master_file,desc(predictions_logit))
test_master_file$predicted_response <- factor(ifelse(test_master_file$predictions_logit >= 0.52, "1", "0"))

pred_object_test<- prediction(as.numeric(test_master_file$predicted_response),as.numeric(test_master_file$Performance.Tag))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
###0.2517148
##the K statistic is 25.17% 
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 

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
arrange(test_master_file,desc(predictions_logit))

attrition_decile = lift(test_master_file$Performance.Tag, test_master_file$predictions_logit, groups = 10)


ggplot(attrition_decile,aes(x=bucket,y=Gain)) + geom_line()

ggplot(attrition_decile,aes(x=bucket,y=Cumlift)) + geom_line()
# A tibble: 10 x 6
#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2091       170     170  19.3    1.93
#2      2  2091       160     330  37.4    1.87
#3      3  2091       124     454  51.4    1.71
#4      4  2091       116     570  64.6    1.61
#5      5  2091       101     671  76.0    1.52
#6      6  2091        74     745  84.4    1.41
#7      7  2091        50     795  90.0    1.29
#8      8  2091        37     832  94.2    1.18
#9      9  2091        32     864  97.8    1.09
#10     10  2090        19     883 100      1   



#### Binary Trees and Random forest families of models #######

tree1 <-  rpart(Performance.Tag ~ ., data=train_master_smote, method= "class",control=rpart.control(minsplit=20,cp=0.0001))
plot(tree1)
prp(tree1)
fancyRpartPlot(tree1)
tree1$variable.importance
print(tree1)
summary(tree1,file="tree1file")
### It looks like a highly overfitted tree
#max depth = 77
#size = 515
tree2 <-  rpart(Performance.Tag ~ ., data=train_master_smote, method= "class",control=rpart.control(minsplit=50,cp=0.001))
plot(tree2)
prp(tree2)
fancyRpartPlot(tree2)
tree2$variable.importance
print(tree2)
summary(tree2,file="tree2file")
### Thee tree is smaller but still big
#max depth =33
#size = 89
tree3 <-  rpart(Performance.Tag ~ ., data=train_master_smote, method= "class",control=rpart.control(minsplit=70,cp=0.005))
plot(tree3)
prp(tree3)
library("rattle")
fancyRpartPlot(tree3)
tree3$variable.importance
print(tree3)
summary(tree3,file="tree3file")
#max depth =12
#size = 31
# Predict response for test_master_file
tree_pred3 <- predict(tree3, test_master_file[,-1], type = "prob")
summary(tree_pred3)
#Mean = 0.26919
#---------------------------------------------------------    
# Cutoff for tree3 to assign yes or no

perform_fn_tree <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(tree_pred3[, 2] >= cutoff, 1, 0))
  conf <- confusionMatrix(predicted_response, test_master_file$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 1 for plotting and initialising a matrix of size 1000x4
s = seq(0,1,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_tree(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.09)]
mean(cutoff_rf)
#0.3131313
predicted_response_22 <- factor(ifelse(tree_pred3[, 2] >= 0.3131313 , "1", "0"))
conf_forest <- confusionMatrix(predicted_response_22, test_master_file[, 1], positive = "1")

# Sensitivity
conf_forest$byClass[1]
#Sensitivity 
# 0.6421291 

# Specificity 
conf_forest$byClass[2]
#Specificity 
#0.5840407

# Accuracy 
conf_forest$overall[1]
#0.5864939 


### lift, gain and KS statistic

######Discriminatory power of model

test_master_file <- cbind(test_master_file,tree_pred3[, 2])
colnames(test_master_file)[31] <- "tree_pred3"
arrange(test_master_file,desc(tree_pred3))
test_master_file$predicted_response_tree3 <- factor(ifelse(test_master_file$tree_pred3 >= 0.3131313,"1","0"))

pred_object_test<- prediction(as.numeric(test_master_file$predicted_response_tree3),as.numeric(test_master_file$Performance.Tag))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
 (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
###0.2261699
##the K statistic is 22.61699% 
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 

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
arrange(test_master_file,desc(tree_pred3))

attrition_decile = lift(test_master_file$Performance.Tag, test_master_file$tree_pred3, groups = 10)
# A tibble: 10 x 6
#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2091       179     179  20.3    2.03
#2      2  2091       131     310  35.1    1.76
#3      3  2091       125     435  49.3    1.64
#4      4  2091       111     546  61.8    1.55
#5      5  2091        78     624  70.7    1.41
#6      6  2091       103     727  82.3    1.37
#7      7  2091        32     759  86.0    1.23
#8      8  2091        50     809  91.6    1.15
#9      9  2091        31     840  95.1    1.06
#10     10  2090        43     883 100      1   

ggplot(attrition_decile,aes(x=bucket,y=Gain)) + geom_line()

ggplot(attrition_decile,aes(x=bucket,y=Cumlift)) + geom_line()



#### Random forests
rf2 <- randomForest(Performance.Tag ~., data = train_master_smote, proximity = F, do.trace = T, mtry = 5)
rf_pred <- predict(rf2, test_master_file[, -1], type = "prob")
summary(rf_pred)
#Mean = 0.1247
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, 1, 0))
  conf <- confusionMatrix(predicted_response, test_master_file$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 1 for plotting and initialising a matrix of size 1000x4
s = seq(0,1,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.065)]
mean(cutoff_rf)
#random forests cutoff = 0.1262626

# The plot shows that cutoff value of around 12.62% optimises sensitivity and accuracy
predicted_response_rf <- factor(ifelse(rf_pred[, 2] >=0.1262 , "1", "0"))

conf_forest <- confusionMatrix(predicted_response_rf, test_master_file[, 1], positive = "1")

conf_forest

#Accuracy : 0.6022       
#Sensitivity : 0.57984  
#Specificity : 0.60317       


### lift , gain and KS statistic

######Discriminatory power of model
test_master_file <- cbind(test_master_file,rf_pred[,2])
colnames(test_master_file)[33] <- "rf_pred"
arrange(test_master_file,desc(rf_pred))
test_master_file$predicted_response_rf <- factor(ifelse(test_master_file$rf_pred >= 0.1262,"1","0"))

pred_object_test<- prediction(as.numeric(test_master_file$predicted_response_rf),as.numeric(test_master_file$Performance.Tag))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
###0.1830073
##the K statistic is 18.30073% 
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 

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
arrange(test_master_file,desc(rf_pred))

attrition_decile = lift(test_master_file$Performance.Tag, test_master_file$rf_pred, groups = 10)
# A tibble: 10 x 6
#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2091       138     138  15.6    1.56
#2      2  2091       142     280  31.7    1.59
#3      3  2091       117     397  45.0    1.50
#4      4  2091       111     508  57.5    1.44
#5      5  2091       107     615  69.6    1.39
#6      6  2091        88     703  79.6    1.33
#7      7  2091        64     767  86.9    1.24
#8      8  2091        52     819  92.8    1.16
#9      9  2091        42     861  97.5    1.08
#10     10  2090        22     883 100      1 

ggplot(attrition_decile,aes(x=bucket,y=Gain)) + geom_line()

ggplot(attrition_decile,aes(x=bucket,y=Cumlift)) + geom_line()

####Financial benefit analysis using logistic regression model
##Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points
##Step 1 convert the probability of default to good to bad odds
test_master_file$odds <- (1-test_master_file$predictions_logit)/test_master_file$predictions_logit

## Linear relationship exists between the ln(odds) and Application Score y=mx+c; y= Application Score and x = ln (odds of good to bad)
## m = 28.854 ; c =  333.561
test_master_file$Application_score <- 28.854*log(test_master_file$odds) + 333.561
summary(test_master_file$Application_score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#298.1   323.4   340.3   341.6   363.0   408.6 
## The application cutoff score corresponding to probability score cutoff 0.5202 is 
cutoff_odds <- (1-0.5202)/0.5202
## cutoff odds = 0.9223
Application_score_cutoff <- 28.854*log(cutoff_odds) + 333.561
## cutoff_Application_score = 331.23
#View(test_master_file$Application_score)

ggplot(test_master_file,aes(Application_score)) + geom_histogram()

##Generating scores for training data
##train_master_file
#Predict an outcome
predictions_logit <- predict(logistic_6, newdata = train_master_file, type = "response")
summary(predictions_logit)
#Mean = 0.43716
#Convert to score
train_master_file <- cbind(train_master_file,predictions_logit)
##Similar to the test_master_file, we calculate the application score with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points
##Step 1 convert the probability of default to good to bad odds

train_master_file$odds <- (1-train_master_file$predictions_logit)/train_master_file$predictions_logit

## Linear relationship exists between the ln(odds) and Application Score y=mx+c; y= Application Score and x = ln (odds of good to bad)
## m = 28.854 ; c =  333.561
train_master_file$Application_score <- 28.854*log(train_master_file$odds) + 333.561
summary(train_master_file$Application_score)
###Mean = 341.9
#train_master_file$Application_score <- as.factor(cut(train_master_file$Application_score, breaks = c(295,305,315,325,335,345,355,365,375,385,395,405,415,425)))
#table(train_master_file$Application_score)
ggplot(train_master_file,aes(Application_score)) + geom_histogram()

###Generating scores of rejected candidates
#master_file_rejected
master_file_rejected$Gender <- as.factor(master_file_rejected$Gender)
master_file_rejected$Marital.Status..at.the.time.of.application <- as.factor(master_file_rejected$Marital.Status..at.the.time.of.application)
master_file_rejected$No.of.dependents <- as.numeric(master_file_rejected$No.of.dependents)
master_file_rejected$Education <- as.factor(master_file_rejected$Education)
master_file_rejected$Profession <- as.factor(master_file_rejected$Profession)
master_file_rejected$Type.of.residence <- as.factor(master_file_rejected$Type.of.residence)
master_file_rejected$Presence.of.open.home.loan <- as.factor(master_file_rejected$Presence.of.open.home.loan)
master_file_rejected$Presence.of.open.auto.loan <- as.factor(master_file_rejected$Presence.of.open.auto.loan)
master_file_rejected$Age <- as.numeric(master_file_rejected$Age)

master_file_rejected$No.of.dependents <- as.numeric(master_file_rejected$No.of.dependents)

master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months)
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months)
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months)
master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months)
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months)
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months)

master_file_rejected$Age <- as.numeric(master_file_rejected$Age)
master_file_rejected$No.of.months.in.current.residence <- as.numeric(master_file_rejected$No.of.months.in.current.residence)
master_file_rejected$No.of.months.in.current.company <- as.numeric(master_file_rejected$No.of.months.in.current.company)
master_file_rejected$Outstanding.Balance <- as.numeric(master_file_rejected$Outstanding.Balance)
master_file_rejected$No.of.trades.opened.in.last.12.months <- as.numeric(master_file_rejected$No.of.trades.opened.in.last.12.months)
master_file_rejected$No.of.PL.trades.opened.in.last.6.months <- as.numeric(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)
master_file_rejected$No.of.PL.trades.opened.in.last.12.months <- as.numeric(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)
master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.numeric(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.numeric(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
master_file_rejected$Total.No.of.Trades <- as.numeric(master_file_rejected$Total.No.of.Trades)
master_file_rejected$Avgas.CC.Utilization.in.last.12.months <- as.numeric(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)
master_file_rejected$Outstanding.Balance <- as.numeric(master_file_rejected$Outstanding.Balance)

master_file_rejected$Presence.of.open.home.loan[which(is.na(master_file_rejected$Presence.of.open.home.loan))] <- 0
master_file_rejected$Outstanding.Balance[which(is.na(master_file_rejected$Outstanding.Balance))] <- 1.75e+06
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_file_rejected$Avgas.CC.Utilization.in.last.12.months))] <- 95

#Replacing the actual value by the WOE value 
master_file_rejected_woe <- woe.binning.deploy(master_file_rejected,binning,add.woe.or.dum.var="woe")
#Predict an outcome
predictions_logit <- predict(logistic_6, newdata = master_file_rejected_woe, type = "response")
summary(predictions_logit)
#Mean = 0.6496
#Convert to score
master_file_rejected_woe <- cbind(master_file_rejected_woe,predictions_logit)
##Similar to the test_master_file, we calculate the application score with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points
##Step 1 convert the probability of default to good to bad odds

master_file_rejected_woe$odds <- (1-master_file_rejected_woe$predictions_logit)/master_file_rejected_woe$predictions_logit

## Linear relationship exists between the ln(odds) and Application Score y=mx+c; y= Application Score and x = ln (odds of good to bad)
## m = 28.854 ; c =  333.561
master_file_rejected_woe$Application_score <- 28.854*log(master_file_rejected_woe$odds) + 333.561
summary(master_file_rejected_woe$Application_score)
#Mean 315.5
ggplot(master_file_rejected_woe,aes(Application_score)) + geom_histogram()

### If model is used : number of candidates automatically accepted/rejected can be derived from Train,Test and Rejected(originally) databases

len_rejected_App_score <- length(master_file_rejected_woe$Application_score)
#len = 1425

###Length of master_file_rejected_below_cutoff

len_rejected_below_cutoff <- length(which(master_file_rejected_woe$Application_score < 331.23 ))
#len below cutoff = 1362

len_test_master_App_score <- length(test_master_file$Application_score)
#20909

###Length of test file below cutoff


len_test_master_below_cutoff <- length(which(test_master_file$Application_score < 331.23 ))
#8194

###Length of train file 
len_train_master_App_score <- length(train_master_file$Application_score)
#48790
###Length of train file below cutoff
len_train_master_below_cutoff <- length(which(train_master_file$Application_score < 331.23 ))
#19036

Percentage_rejected_with_model <- sum(len_test_master_below_cutoff,len_train_master_below_cutoff,len_rejected_below_cutoff)/sum(len_test_master_App_score,len_train_master_App_score,len_rejected_App_score)
Percentage_rejected_with_model

###0.4020


#No. of customers rejected by the model
No_rejected_by_model <- sum(len_test_master_below_cutoff,len_train_master_below_cutoff,len_rejected_below_cutoff)
No_rejected_by_model
#28592


######################
###Revenue loss: due to more rejected applicants
Percentage_rejected_with_no_model <- len_rejected_App_score/sum(len_test_master_App_score,len_train_master_App_score,len_rejected_App_score)
#0.02003543
Percentage_rejected_with_model
#0.4020021
###38.20% revenue loss due to additional customer rejections
#######################
###Addition of  revenue from good applicants in the originally rejected group
###Number of applicants in originally rejected group (Performance.Tag == NA)= 1425
len_rejected_above_cutoff  <- length(which(master_file_rejected_woe$Application_score >= 331.23 ))
#63
Percentage_good_applicants_in_originally_rejected_group <- len_rejected_above_cutoff/len_rejected_App_score * 100
# 4.421053%
#############################################
Defaults_test <- filter(test_master_file,test_master_file$Performance.Tag == "1")
Defaults_test_length <- nrow(Defaults_test)
#883
Defaults_train <- filter(train_master_file,train_master_file$Performance.Tag == "1")
Defaults_train_length<- nrow(Defaults_train)
#2062
Percentage_defaults_without_model <- sum(Defaults_test_length,Defaults_train_length)/sum(len_test_master_App_score,len_train_master_App_score)
#0.04225312 4.23%
Number_of_defaults_with_model = sum(length(which(Defaults_test$Application_score >= 331.23)),length(which(Defaults_train$Application_score >= 331.23)))
#1070
Percentage_defaults_with_model <- Number_of_defaults_with_model/sum(len_test_master_App_score,len_train_master_App_score)
#0.01535173 1.54%
Number_of_defaults_rejected_by_model <- sum(length(which(Defaults_test$Application_score < 331.23)),length(which(Defaults_train$Application_score < 331.23)))
Percentage_of_defaults_avoided_by_using_model <- Number_of_defaults_rejected_by_model/sum(Defaults_test_length,Defaults_train_length)
#0.6366723  63.67%
##Average credit loss per default 
AV_CR_loss_per_default <- mean(master_file_accepted$Outstanding.Balance[which(master_file_accepted$Performance.Tag == 1)],na.rm = T)
AV_CR_loss_per_default

#1260675

gross_amt_saved_by_model <-Number_of_defaults_rejected_by_model * AV_CR_loss_per_default
gross_amt_saved_by_model
#2363765384
##2.36 bn
######################################################





