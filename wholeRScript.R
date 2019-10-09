#install.packages("readxl")
#install.packages("lubridate")
library("lubridate", lib.loc="~/R/win-library/3.3")

#To clean the Data of visa issuance.
library("readxl", lib.loc="~/R/win-library/3.3")
#Set the work Directory
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
visaData <- read_excel("statistic_id536014_number-of-eb-3-visa-issuances-by-the-us-state-department-2010-to-2018.xlsx", sheet = 2)
colnames(visaData)[1]<- "Year"
colnames(visaData)[2]<- "NumberOfVisaIssues"
visaData <- visaData[-c(1,2),]
visaData$Year <- as.numeric(visaData$Year)
visaData$NumberOfVisaIssues <- as.numeric(visaData$NumberOfVisaIssues)
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(visaData, file = "visaData.csv", row.names = FALSE)

#To Clean the Data of povertyData file
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
povertyData <- read_xlsx("statistic_id233138_number-of-people-living-below-the-poverty-line-in-the-us-1990-2017.xlsx", sheet = 2)
#Renamed the columns
colnames(povertyData)[1] <- "Year"
colnames(povertyData)[2] <- "PovertyCountInMillions"
povertyData$`PovertyCountInMillions` <- as.numeric(povertyData$`PovertyCountInMillions`)
povertyData <- povertyData[-c(1,2),]
povertyData$Year <- as.Date(povertyData$Year, "%Y")
povertyData$Year <- lubridate::year(povertyData$Year)
#To convert year into integer from char
povertyData$Year <- as.numeric(povertyData$Year)
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(povertyData, file = "povertyData.csv", row.names = FALSE)



#To clean the Data of publicOpinion file
#install.packages("readxl")
library("readxl", lib.loc="~/R/win-library/3.3")
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
public_opinion <- read_excel("statistic_id217972_public-opinion-on-the-level-of-immigration-into-the-us-2001-2018.xlsx", sheet = 2)
#Renaming the columns
colnames(public_opinion)[1] <- "Year"
colnames(public_opinion)[2] <- "Satisfied"
colnames(public_opinion)[3] <- "Dissatisfied"
public_opinion$Year <- as.Date(public_opinion$Year, "%Y")
public_opinion$Year <- lubridate::year(public_opinion$Year)
public_opinion$Year <- as.factor(public_opinion$Year)
#Removing Extra Rows
public_opinion <- public_opinion[-c(1,2),]
#Removing unrequired Columns
public_opinion$X__3 <- NULL
public_opinion$Satisfied <- as.numeric(public_opinion$Satisfied)
public_opinion$Dissatisfied <- as.numeric(public_opinion$Dissatisfied)
public_opinion$Year <- as.numeric(as.character(public_opinion$Year))
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(public_opinion, file = "public_opinion.csv", row.names = FALSE)


#To scrape and clean data from wikipedia
library(htmltab)
wiki <- htmltab("https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations", 1)
#Editing the column names
colnames(wiki)[1] <- "States"
colnames(wiki)[4] <- "Abbrevation"
wiki <- wiki[,(-c(2,3,5:length(wiki)))]
#Identifying the null records
nullColumns <- is.na(wiki$Abbrevation)
#Removing the null records
wiki <- wiki[!nullColumns,]
#Filtering the irrelevant characters
wiki$States <- gsub(substring(wiki$States,1,2), "", wiki$States)
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(wiki, file = "StateAbbrevations.csv", row.names = FALSE)


#To clean the Data of studentEnrollment file
library("readxl", lib.loc="~/R/win-library/3.3")
#install.packages("reshape2")
library("readxl", lib.loc="~/R/win-library/3.3")
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
StudentsEnrollment <- read_xlsx("International-Students-Enrollment.xlsx", sheet = 3)
#Removing the unwanted rows from the data
StudentsEnrollment <- StudentsEnrollment[-c(1:3,8),]
StudentsEnrollment$`Table 2`[is.na(StudentsEnrollment$`Table 2`)] <- "Year"
#Need to pivot the table
StudentsEnrollment1 <- t(StudentsEnrollment)
StudentsEnrollment1 <- as.data.frame(StudentsEnrollment1)
StudentsEnrollment <- StudentsEnrollment1
rm(StudentsEnrollment1)
#Renaming the columns
colnames(StudentsEnrollment)[1] <- "Years"
colnames(StudentsEnrollment)[2] <- "Undergraduate"
colnames(StudentsEnrollment)[3] <- "Graduate"
colnames(StudentsEnrollment)[4] <- "Non-Degree"
colnames(StudentsEnrollment)[5] <- "Total"
StudentsEnrollment <- StudentsEnrollment[-c(1,13),]
StudentsEnrollment$Years <- gsub("/.*", "", StudentsEnrollment$Years)
StudentsEnrollment$Years <- as.Date(StudentsEnrollment$Years, "%Y")
StudentsEnrollment$Years <- lubridate::year(StudentsEnrollment$Years)
StudentsEnrollment$Years <- as.numeric(StudentsEnrollment$Years)
StudentsEnrollment$Undergraduate <- as.numeric(as.character(StudentsEnrollment$Undergraduate))
StudentsEnrollment$Graduate <- as.numeric(as.character(StudentsEnrollment$Graduate))
StudentsEnrollment$`Non-Degree` <- as.numeric(as.character(StudentsEnrollment$`Non-Degree`))
StudentsEnrollment$Total <- as.numeric(as.character(StudentsEnrollment$Total))
StudentsEnrollment$Years <- StudentsEnrollment$Years+1
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(StudentsEnrollment, file = "StudentEnrollment.csv", row.names = FALSE)


#To clean the data of USBestColleges file
#install.packages("readxl")
library("readxl", lib.loc="~/R/win-library/3.3")
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
USBestCollege <- read_excel("statistic_id220568_forbes-ranking-of-the-best-us-colleges-in-2018-by-overall-payoff.xlsx", sheet = 2)
colnames(USBestCollege)[1] <- "Colleges"
colnames(USBestCollege)[2] <- "TuitionCost"
USBestCollege <- USBestCollege[-c(1,2),]
USBestCollege$X__2 <- NULL
USBestCollege$State <- sub("\\).*", "", sub(".*\\(", "", USBestCollege$Colleges))
USBestCollege$Colleges <- gsub("\\(.*", "", USBestCollege$Colleges)
USBestCollege$TuitionCost <- as.numeric(USBestCollege$TuitionCost)
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(USBestCollege, file = "USBestCollege.csv", row.names = FALSE)


#To extract the UniversityAcceptanceData
library(htmltab)
library(tm)
ivy <- htmltab("https://www.ivyacademiccoach.com/blog/2018/11/16/college-admissions-data-2018",1)
colnames(ivy)[0] <- "UniversityName"
ivy$`2015`[is.na(ivy$`2015`)] <- 0
ivy$`2016`[is.na(ivy$`2016`)] <- 0
ivy$`2017`[is.na(ivy$`2017`)] <- 0
ivy$`2018`[is.na(ivy$`2018`)] <- 0


ivynew <-t(as.matrix(ivy[,2:5]))
newdf <- cbind(ivy[rep(1:nrow(ivy), each=4),1],
               Year = c(2018:2015),
               Acceptance=as.vector(ivynew[1:4,]))
rownames(ivynew)<-NULL

UniversityAcceptanceData <- newdf
UniversityAcceptanceData <- data.frame(UniversityAcceptanceData)
colnames(UniversityAcceptanceData)[1] <- "University"
UniversityAcceptanceData$University <- as.character(UniversityAcceptanceData$University)
UniversityAcceptanceData$Year <- as.numeric(as.character(UniversityAcceptanceData$Year))
UniversityAcceptanceData$Acceptance <- as.numeric(as.character(UniversityAcceptanceData$Acceptance))
UniversityAcceptanceData$Acceptance[is.na(UniversityAcceptanceData$Acceptance)] <- 0 
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(UniversityAcceptanceData, "UniversityAcceptanceData.csv", row.names = FALSE)


#To Clean the Data of CostOfLiving file
library("readxl", lib.loc="~/R/win-library/3.3")
#Set the work Directory
setwd("C:/Users/MOLAP/Desktop/Data - Copy")
LivingIndex <- read.csv("Cost_of_living_index.csv")
#Filtering the cities of United STates Only
country = "United States"
LivingIndex_USA <- grep(country, LivingIndex$City, perl = TRUE)
#Overwriting
LivingIndex = LivingIndex[LivingIndex_USA,]
#Making new column as State to compare with the university State
LivingIndex$StateAbbrevation <-gsub("(.*),.*", "\\1",LivingIndex$City)
LivingIndex$StateAbbrevation <-gsub(".*,", "", LivingIndex$StateAbbrevation)
LivingIndex$StateAbbrevation <-gsub(" ","", LivingIndex$StateAbbrevation)
LivingIndex$Rank <- NULL
setwd("C:/Users/MOLAP/Desktop/Data - Copy/RCodes")
write.csv(LivingIndex, file = "LivingIndex.csv", row.names = FALSE)


