###--------Import libraries (R)--------###
library(highcharter)
library(leaflet)
library(ggmap)
library(readxl)
library(tidyr)
library(stringr)
library(rgeos)
library(maptools)
library(rgdal)
library(cluster)
library(plyr)
library(dplyr)
library(stats)
library(GISTools)
library(flexclust)
library(tibble)
library(lubridate)
library(forecast)
library(mefa)
library(reshape2)
library(wskm)
library(shiny)
library(shinythemes)
library(stringi)
library(tmap)
library(markdown)
library(rsconnect)


###------------Set Working Directory--------------------###
setwd("C:/Users/kenne/GIT/Capstone")

##-------SNF Aggragregate Report Data (2013, 2014, 2015)-------##
SNF_Location <- read_excel('Data/Inspection_Cycle/Inspection_Cycle_Deficiencies_SNF.xlsx')

SNF_Location  <- SNF_Location[c(1,2,13)]

names(SNF_Location) <- c("Provider_ID", "Facility_Name", "Location")

#View(SNF_Report_2015)

SNF_Report_2013 <- read_excel('Data/SNF/SNF_aggregate_report_2013.xlsx', sheet = "Provider")
SNF_Report_2013  <- SNF_Report_2013[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2013) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2013$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2013$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2013$Total_Stays))

SNF_Report_2014 <- read_excel('Data/SNF/SNF_aggregate_report_2014.xlsx', sheet = "Provider")
SNF_Report_2014  <- SNF_Report_2014[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2014) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2014$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2014$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2014$Total_Stays))

SNF_Report_2015 <- read_excel('Data/SNF/SNF_aggregate_report_2015.xlsx', sheet = "SNF_2015")
SNF_Report_2015  <- SNF_Report_2015[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2015) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2015$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2015$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2015$Total_Stays))


SNF_Report_2013 <- merge(SNF_Report_2013,SNF_Location,by="Provider_ID")
SNF_Report_2014 <- merge(SNF_Report_2014,SNF_Location,by="Provider_ID")
SNF_Report_2015 <- merge(SNF_Report_2015,SNF_Location,by="Provider_ID")


SNF_2013_TN <- subset(SNF_Report_2013, State == "TN")
SNF_2014_TN <- subset(SNF_Report_2014, State == "TN")
SNF_2015_TN <- subset(SNF_Report_2015, State == "TN")

SNF_2013_TN <- unique(SNF_2013_TN)
SNF_2014_TN <- unique(SNF_2014_TN)
SNF_2015_TN <- unique(SNF_2015_TN)

SNF_2013_TN <- separate(SNF_2013_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2013_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2013_TN$Lat))
SNF_2013_TN$Long <- as.numeric(gsub("\\)", "", SNF_2013_TN$Long))

SNF_2014_TN <- separate(SNF_2014_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2014_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2014_TN$Lat))
SNF_2014_TN$Long <- as.numeric(gsub("\\)", "", SNF_2014_TN$Long))

SNF_2015_TN <- separate(SNF_2015_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2015_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2015_TN$Lat))
SNF_2015_TN$Long <- as.numeric(gsub("\\)", "", SNF_2015_TN$Long))

##---------Census_Tract_Data---------##

##--2010--##

TN_Tract_Population_2010 = read.csv("Data/TN_Tract_Data/ACS_10_5YR_DP05_with_ann.csv")

TN_Tract_Population_2010 <- TN_Tract_Population_2010[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2010) <- c("ID1", "ID2", "Tract_Name_2010", "Tract_Population_2010", "Tract_Population_65_to_74_2010", "Tract_Population_75_to_84_2010", "Tract_Population_Over_85_2010")

TN_Tract_Population_2010 <- TN_Tract_Population_2010[-c(1), ]

rownames(TN_Tract_Population_2010) <- seq(length=nrow(TN_Tract_Population_2010)) 

TN_Tract_Population_2010$Tract_Population_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_2010))

TN_Tract_Population_2010$Tract_Population_2010 <- as.numeric(TN_Tract_Population_2010$Tract_Population_2010)

TN_Tract_Population_2010$Tract_Population_2010[which(TN_Tract_Population_2010$Tract_Population_2010 == 0)] <-  mean(TN_Tract_Population_2010$Tract_Population_2010)

TN_Tract_Population_2010$Tract_Population_65_to_74_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_65_to_74_2010))

TN_Tract_Population_2010$Tract_Population_75_to_84_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_75_to_84_2010))

TN_Tract_Population_2010$Tract_Population_Over_85_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_Over_85_2010))

TN_Tract_Population_2010$Target_Demographic_Population_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_65_to_74_2010+TN_Tract_Population_2010$Tract_Population_75_to_84_2010+TN_Tract_Population_2010$Tract_Population_Over_85_2010))

TN_Tract_Population_2010$Tract_Population_Density_2010 <- round((TN_Tract_Population_2010$Target_Demographic_Population_2010/TN_Tract_Population_2010$Tract_Population_2010), 2)

TN_Tract_Population_2010$Tract_Population_Density_2010[which(TN_Tract_Population_2010$Tract_Population_Density_2010 > 1)] <-  1


##--2011--##

TN_Tract_Population_2011 = read.csv("Data/TN_Tract_Data/ACS_11_5YR_DP05_with_ann.csv")

TN_Tract_Population_2011 <- TN_Tract_Population_2011[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2011) <- c("ID1", "ID2", "Tract_Name_2011", "Tract_Population_2011", "Tract_Population_65_to_74_2011", "Tract_Population_75_to_84_2011", "Tract_Population_Over_85_2011")

TN_Tract_Population_2011 <- TN_Tract_Population_2011[-c(1), ]

rownames(TN_Tract_Population_2011) <- seq(length=nrow(TN_Tract_Population_2011)) 

TN_Tract_Population_2011$Tract_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_2011))

TN_Tract_Population_2011$Tract_Population_2011[which(TN_Tract_Population_2011$Tract_Population_2011 == 0)] <-  mean(TN_Tract_Population_2011$Tract_Population_2011)

TN_Tract_Population_2011$Tract_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_2011))

TN_Tract_Population_2011$Tract_Population_65_to_74_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_65_to_74_2011))

TN_Tract_Population_2011$Tract_Population_75_to_84_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_75_to_84_2011))

TN_Tract_Population_2011$Tract_Population_Over_85_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_Over_85_2011))

TN_Tract_Population_2011$Target_Demographic_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_65_to_74_2011+TN_Tract_Population_2011$Tract_Population_75_to_84_2011+TN_Tract_Population_2011$Tract_Population_Over_85_2011))

TN_Tract_Population_2011$Tract_Population_Density_2011 <- round((TN_Tract_Population_2011$Target_Demographic_Population_2011/TN_Tract_Population_2011$Tract_Population_2011), 2)

TN_Tract_Population_2011$Tract_Population_Density_2011[which(TN_Tract_Population_2011$Tract_Population_Density_2011 > 1)] <-  1


##--2012--##

TN_Tract_Population_2012 = read.csv("Data/TN_Tract_Data/ACS_12_5YR_DP05_with_ann.csv")

TN_Tract_Population_2012 <- TN_Tract_Population_2012[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2012) <- c("ID1", "ID2", "Tract_Name_2012", "Tract_Population_2012", "Tract_Population_65_to_74_2012", "Tract_Population_75_to_84_2012", "Tract_Population_Over_85_2012")

TN_Tract_Population_2012 <- TN_Tract_Population_2012[-c(1), ]

rownames(TN_Tract_Population_2012) <- seq(length=nrow(TN_Tract_Population_2012)) 

TN_Tract_Population_2012$Tract_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_2012))

TN_Tract_Population_2012$Tract_Population_2012[which(TN_Tract_Population_2012$Tract_Population_2012 == 0)] <-  mean(TN_Tract_Population_2012$Tract_Population_2012)

TN_Tract_Population_2012$Tract_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_2012))

TN_Tract_Population_2012$Tract_Population_65_to_74_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_65_to_74_2012))

TN_Tract_Population_2012$Tract_Population_75_to_84_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_75_to_84_2012))

TN_Tract_Population_2012$Tract_Population_Over_85_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_Over_85_2012))

TN_Tract_Population_2012$Target_Demographic_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_65_to_74_2012+TN_Tract_Population_2012$Tract_Population_75_to_84_2012+TN_Tract_Population_2012$Tract_Population_Over_85_2012))

TN_Tract_Population_2012$Tract_Population_Density_2012 <- round((TN_Tract_Population_2012$Target_Demographic_Population_2012/TN_Tract_Population_2012$Tract_Population_2012), 2)

TN_Tract_Population_2012$Tract_Population_Density_2012[which(TN_Tract_Population_2012$Tract_Population_Density_2012 > 1)] <-  1


##--2013--##

TN_Tract_Population_2013 = read.csv("Data/TN_Tract_Data/ACS_13_5YR_DP05_with_ann.csv")

TN_Tract_Population_2013 <- TN_Tract_Population_2013[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2013) <- c("ID1", "ID2", "Tract_Name_2013", "Tract_Population_2013", "Tract_Population_65_to_74_2013", "Tract_Population_75_to_84_2013", "Tract_Population_Over_85_2013")

TN_Tract_Population_2013 <- TN_Tract_Population_2013[-c(1), ]

rownames(TN_Tract_Population_2013) <- seq(length=nrow(TN_Tract_Population_2013)) 

TN_Tract_Population_2013$Tract_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_2013))

TN_Tract_Population_2013$Tract_Population_2013[which(TN_Tract_Population_2013$Tract_Population_2013 == 0)] <-  mean(TN_Tract_Population_2013$Tract_Population_2013)

TN_Tract_Population_2013$Tract_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_2013))

TN_Tract_Population_2013$Tract_Population_65_to_74_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_65_to_74_2013))

TN_Tract_Population_2013$Tract_Population_75_to_84_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_75_to_84_2013))

TN_Tract_Population_2013$Tract_Population_Over_85_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_Over_85_2013))

TN_Tract_Population_2013$Target_Demographic_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_65_to_74_2013+TN_Tract_Population_2013$Tract_Population_75_to_84_2013+TN_Tract_Population_2013$Tract_Population_Over_85_2013))

TN_Tract_Population_2013$Tract_Population_Density_2013 <- round((TN_Tract_Population_2013$Target_Demographic_Population_2013/TN_Tract_Population_2013$Tract_Population_2013), 2)

TN_Tract_Population_2013$Tract_Population_Density_2013[which(TN_Tract_Population_2013$Tract_Population_Density_2013 > 1)] <-  1


##--2014--##

TN_Tract_Population_2014 = read.csv("Data/TN_Tract_Data/ACS_14_5YR_DP05_with_ann.csv")

TN_Tract_Population_2014 <- TN_Tract_Population_2014[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2014) <- c("ID1", "ID2", "Tract_Name_2014", "Tract_Population_2014", "Tract_Population_65_to_74_2014", "Tract_Population_75_to_84_2014", "Tract_Population_Over_85_2014")

TN_Tract_Population_2014 <- TN_Tract_Population_2014[-c(1), ]

rownames(TN_Tract_Population_2014) <- seq(length=nrow(TN_Tract_Population_2014)) 

TN_Tract_Population_2014$Tract_Population_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_2014))

TN_Tract_Population_2014$Tract_Population_2014[which(TN_Tract_Population_2014$Tract_Population_2014 == 0)] <-  mean(TN_Tract_Population_2014$Tract_Population_2014)

TN_Tract_Population_2014$Tract_Population_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_2014))

TN_Tract_Population_2014$Tract_Population_65_to_74_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_65_to_74_2014))

TN_Tract_Population_2014$Tract_Population_75_to_84_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_75_to_84_2014))

TN_Tract_Population_2014$Tract_Population_Over_85_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_Over_85_2014))

TN_Tract_Population_2014$Target_Demographic_Population_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_65_to_74_2014+TN_Tract_Population_2014$Tract_Population_75_to_84_2014+TN_Tract_Population_2014$Tract_Population_Over_85_2014))

TN_Tract_Population_2014$Tract_Population_Density_2014 <- round((TN_Tract_Population_2014$Target_Demographic_Population_2014/TN_Tract_Population_2014$Tract_Population_2014), 2)

TN_Tract_Population_2014$Tract_Population_Density_2014[which(TN_Tract_Population_2014$Tract_Population_Density_2014 > 1)] <-  1

#View(TN_Tract_Population_2014)

##--2015--##

TN_Tract_Population_2015 = read.csv("Data/TN_Tract_Data/ACS_15_5YR_DP05_with_ann.csv")

TN_Tract_Population_2015 <- TN_Tract_Population_2015[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2015) <- c("ID1", "ID2", "Tract_Name_2015", "Tract_Population_2015", "Tract_Population_65_to_74_2015", "Tract_Population_75_to_84_2015", "Tract_Population_Over_85_2015")

TN_Tract_Population_2015 <- TN_Tract_Population_2015[-c(1), ]

rownames(TN_Tract_Population_2015) <- seq(length=nrow(TN_Tract_Population_2015)) 

TN_Tract_Population_2015$Tract_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_2015))

TN_Tract_Population_2015$Tract_Population_2015[which(TN_Tract_Population_2015$Tract_Population_2015 == 0)] <-  mean(TN_Tract_Population_2015$Tract_Population_2015)

TN_Tract_Population_2015$Tract_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_2015))

TN_Tract_Population_2015$Tract_Population_65_to_74_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_65_to_74_2015))

TN_Tract_Population_2015$Tract_Population_75_to_84_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_75_to_84_2015))

TN_Tract_Population_2015$Tract_Population_Over_85_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_Over_85_2015))

TN_Tract_Population_2015$Target_Demographic_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_65_to_74_2015+TN_Tract_Population_2015$Tract_Population_75_to_84_2015+TN_Tract_Population_2015$Tract_Population_Over_85_2015))

TN_Tract_Population_2015$Tract_Population_Density_2015 <- round((TN_Tract_Population_2015$Target_Demographic_Population_2015/TN_Tract_Population_2015$Tract_Population_2015), 2)

TN_Tract_Population_2015$Tract_Population_Density_2015[which(TN_Tract_Population_2015$Tract_Population_Density_2015 > 1)] <-  1

#View(TN_Tract_Population_2015)

##--2016--#
TN_Tract_Population_2016 = read.csv("Data/TN_Tract_Data/ACS_16_5YR_DP05_with_ann.csv")

TN_Tract_Population_2016 <- TN_Tract_Population_2016[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2016) <- c("ID1", "ID2", "Tract_Name_2016", "Tract_Population_2016", "Tract_Population_65_to_74_2016", "Tract_Population_75_to_84_2016", "Tract_Population_Over_85_2016")

TN_Tract_Population_2016 <- TN_Tract_Population_2016[-c(1, 2), ]

rownames(TN_Tract_Population_2016) <- seq(length=nrow(TN_Tract_Population_2016)) 

TN_Tract_Population_2016$Tract_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_2016))

TN_Tract_Population_2016$Tract_Population_2016[which(TN_Tract_Population_2016$Tract_Population_2016 == 0)] <-  mean(TN_Tract_Population_2016$Tract_Population_2016)

TN_Tract_Population_2016$Tract_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_2016))

TN_Tract_Population_2016$Tract_Population_65_to_74_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_65_to_74_2016))

TN_Tract_Population_2016$Tract_Population_75_to_84_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_75_to_84_2016))

TN_Tract_Population_2016$Tract_Population_Over_85_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_Over_85_2016))

TN_Tract_Population_2016$Target_Demographic_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_65_to_74_2016+TN_Tract_Population_2016$Tract_Population_75_to_84_2016+TN_Tract_Population_2016$Tract_Population_Over_85_2016))

TN_Tract_Population_2016$Tract_Population_Density_2016 <- round((TN_Tract_Population_2016$Target_Demographic_Population_2016/TN_Tract_Population_2016$Tract_Population_2016), 2)

TN_Tract_Population_2016$Tract_Population_Density_2016[which(TN_Tract_Population_2016$Tract_Population_Density_2016 > 1)] <-  1

#View(TN_Tract_Population_2016)

##--Total--##
TN_Tract_Population <- cbind(TN_Tract_Population_2011, TN_Tract_Population_2012, TN_Tract_Population_2013, TN_Tract_Population_2014, TN_Tract_Population_2015, TN_Tract_Population_2016)

#View(TN_Tract_Population)

##--Tract_Target_Demographic_Population_Predictions
Tract_Demographic_Predictions <- data.frame(
  Target_Demographic_Population_2017=numeric(),
  Target_Demographic_Population_2018=numeric(),
  Target_Demographic_Population_2019 = numeric(),
  Target_Demographic_Population_2020=numeric(),
  Target_Demographic_Population_2021=numeric())

Tract_Raw <- data.frame(
  ID2 = numeric(),
  Target_Demographic_Population_2011=numeric(),
  Target_Demographic_Population_2012=numeric(),
  Target_Demographic_Population_2013=numeric(),
  Target_Demographic_Population_2014=numeric(),
  Target_Demographic_Population_2015=numeric(),
  Target_Demographic_Population_2016=numeric())

Tract_Name <- data.frame(
  ID = numeric(),
  County_Name = character())

Median_2011 <- median(TN_Tract_Population$Target_Demographic_Population_2011)

Median_2012 <- median(TN_Tract_Population$Target_Demographic_Population_2012)

Median_2013 <- median(TN_Tract_Population$Target_Demographic_Population_2013)

Median_2014 <- median(TN_Tract_Population$Target_Demographic_Population_2014)

Median_2015 <- median(TN_Tract_Population$Target_Demographic_Population_2015)

Median_2016 <- median(TN_Tract_Population$Target_Demographic_Population_2016)

optim.control = list(maxit = 2000) 

for(i in unique(TN_Tract_Population$ID2[1:1497])) 
{
  TN_Tract_Selection <- subset(TN_Tract_Population, ID2 == i)
  
  
  if (TN_Tract_Selection$Target_Demographic_Population_2011 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2012 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2013 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2014 == 0 &  
      TN_Tract_Selection$Target_Demographic_Population_2015 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2016 == 0)  
    
  {
    TN_Tract_Selection$Target_Demographic_Population_2011 <-  Median_2011
    
    TN_Tract_Selection$Target_Demographic_Population_2012 <-  Median_2012
    
    TN_Tract_Selection$Target_Demographic_Population_2013 <-  Median_2013
    
    TN_Tract_Selection$Target_Demographic_Population_2014 <-  Median_2014
    
    TN_Tract_Selection$Target_Demographic_Population_2015 <-  Median_2015
    
    TN_Tract_Selection$Target_Demographic_Population_2016 <-  Median_2016
  }
  
  median(TN_Tract_Selection$Target_Demographic_Population_2016)
  
  TN_Tract_Selection <- as.data.frame(t(TN_Tract_Selection[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")]))
  
  names(TN_Tract_Selection) <- c("Population")
  
  x_tract <- arima(TN_Tract_Selection$Population, order=c(0,2,2), method="ML")
  
  y_tract <- as.data.frame(predict(x_tract, n.ahead=5))
  
  names(y_tract) <- c("Population", "Standard_Error")
  
  y_tract$Population <- round(as.numeric(as.character(y_tract$Population)), 0)
  
  z_tract <- as.data.frame(t(y_tract$Population))
  
  names(z_tract) <- c("Target_Demographic_Population_2017", "Target_Demographic_Population_2018", "Target_Demographic_Population_2019","Target_Demographic_Population_2020", "Target_Demographic_Population_2021")
  
  Tract_Demographic_Predictions <- rbind(Tract_Demographic_Predictions, z_tract)
}

TN_Tract_Population <- cbind(TN_Tract_Population, Tract_Demographic_Predictions)

##--Tract_Population_Predictions
Tract_Population_Predictions <- data.frame(
  Tract_ID = numeric(),
  Tract_Population_2017=numeric(),
  Tract_Population_2018=numeric(),
  Tract_Population_2019 = numeric(),
  Tract_Population_2020=numeric(),
  Tract_Population_2021=numeric())

Tract_Raw <- data.frame(
  ID2 = numeric(),
  Target_Demographic_Population_2011=numeric(),
  Target_Demographic_Population_2012=numeric(),
  Target_Demographic_Population_2013=numeric(),
  Target_Demographic_Population_2014=numeric(),
  Target_Demographic_Population_2015=numeric(),
  Target_Demographic_Population_2016=numeric())

Tract_Name <- data.frame(
  ID = numeric(),
  Tract_Name = character())

Median_2011 <- median(TN_Tract_Population$Tract_Population_2011)

Median_2012 <- median(TN_Tract_Population$Tract_Population_2012)

Median_2013 <- median(TN_Tract_Population$Tract_Population_2013)

Median_2014 <- median(TN_Tract_Population$Tract_Population_2014)

Median_2015 <- median(TN_Tract_Population$Tract_Population_2015)

Median_2016 <- median(TN_Tract_Population$Tract_Population_2016)

TN_Tract_Population$Tract_Name <- str_sub(TN_Tract_Population$Tract_Name_2011,1,-12)

optim.control = list(maxit = 2000) 

for(i in unique(TN_Tract_Population$ID2[1:1497])) 
{
  TN_Tract_Selection <- subset(TN_Tract_Population, ID2 == i)
  
  
  if (TN_Tract_Selection$Tract_Population_2011 == 0 &
      TN_Tract_Selection$Tract_Population_2012 == 0 &
      TN_Tract_Selection$Tract_Population_2013 == 0 &
      TN_Tract_Selection$Tract_Population_2014 == 0 &  
      TN_Tract_Selection$Tract_Population_2015 == 0 &
      TN_Tract_Selection$Tract_Population_2016 == 0)  
    
  {
    TN_Tract_Selection$Tract_Population_2011 <-  Median_2011
    
    TN_Tract_Selection$Tract_Population_2012 <-  Median_2012
    
    TN_Tract_Selection$Tract_Population_2013 <-  Median_2013
    
    TN_Tract_Selection$Tract_Population_2014 <-  Median_2014
    
    TN_Tract_Selection$Tract_Population_2015 <-  Median_2015
    
    TN_Tract_Selection$Tract_Population_2016 <-  Median_2016
  }
  

  TN_Tract_Raw <- as.data.frame((TN_Tract_Selection[c("Tract_Population_2011", "Tract_Population_2012", "Tract_Population_2013", "Tract_Population_2014", "Tract_Population_2015", "Tract_Population_2016")]))
  
  TN_Tract_Pivot <- as.data.frame(t(TN_Tract_Selection[c("Tract_Population_2011", "Tract_Population_2012", "Tract_Population_2013", "Tract_Population_2014", "Tract_Population_2015", "Tract_Population_2016")]))
  
  names(TN_Tract_Pivot) <- c("Population")
  
  x_tract <- arima(TN_Tract_Pivot$Population, order=c(0,2,2), method="ML")
  
  y_tract <- as.data.frame(predict(x_tract, n.ahead=5))
  
  names(y_tract) <- c("Population", "Standard_Error")
  
  y_tract$Population <- round(as.numeric(as.character(y_tract$Population)), 0)
  
  z_tract <- as.data.frame(t(y_tract$Population))
  
  names(z_tract) <- c("Tract_Population_2017", "Tract_Population_2018", "Tract_Population_2019","Tract_Population_2020", "Tract_Population_2021")
  
  Tract_Raw <- rbind(subset(TN_Tract_Population[c("ID2","Target_Demographic_Population_2011", "Target_Demographic_Population_2012", 
                                                  "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", 
                                                  "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")], ID2 == i), Tract_Raw)
  
  
  Tract_Population_Predictions <- rbind(Tract_Population_Predictions, z_tract)
  
  Tract_Name <- rbind(subset(TN_Tract_Population[c("Tract_Name", "ID2")], ID2 == i), Tract_Name)
  
}


Tract_Name <- Tract_Name[order(Tract_Name),]
Tract_Name <- Tract_Name %>% drop_na()

Tract_Raw <- Tract_Raw[order(Tract_Raw$ID2),]

Tract_Population_Predictions <- cbind(Tract_Name,Tract_Raw, Tract_Population_Predictions)

Tract_Population_Predictions <- Tract_Population_Predictions[-c(3)]

#View(TN_Tract_Population)

TN_Tract_Population <- cbind(TN_Tract_Population, Tract_Population_Predictions)

TN_Tract_Population$Tract_Population_Density_2017 <- round((TN_Tract_Population$Target_Demographic_Population_2017/TN_Tract_Population$Tract_Population_2017), 2)

TN_Tract_Population$Tract_Population_Density_2017[which(TN_Tract_Population$Tract_Population_Density_2017 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2018 <- round((TN_Tract_Population$Target_Demographic_Population_2018/TN_Tract_Population$Tract_Population_2018), 2)

TN_Tract_Population$Tract_Population_Density_2018[which(TN_Tract_Population$Tract_Population_Density_2018 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2019 <- round((TN_Tract_Population$Target_Demographic_Population_2019/TN_Tract_Population$Tract_Population_2019), 2)

TN_Tract_Population$Tract_Population_Density_2019[which(TN_Tract_Population$Tract_Population_Density_2019 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2020 <- round((TN_Tract_Population$Target_Demographic_Population_2020/TN_Tract_Population$Tract_Population_2020), 2)

TN_Tract_Population$Tract_Population_Density_2020[which(TN_Tract_Population$Tract_Population_Density_2020 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2021 <- round((TN_Tract_Population$Target_Demographic_Population_2021/TN_Tract_Population$Tract_Population_2021), 2)

TN_Tract_Population$Tract_Population_Density_2021[which(TN_Tract_Population$Tract_Population_Density_2021 > 1)] <-  1

#View(TN_Tract_Population)

##---------County_Data---------##

##--2011--##
TN_County_Population_2011 = read.csv("Data/TN_counties/PEP_2011_PEPAGESEX_with_ann.csv")

#View(TN_County_Population_2011)

TN_County_Population_2011<- TN_County_Population_2011[,c("GEO.id","GEO.id2","GEO.display.label","est72011sex0_age999","est72011sex0_age65to69","est72011sex0_age70to74","est72011sex0_age75to79","est72011sex0_age80to84","est72011sex0_age85plus")]

names(TN_County_Population_2011) <- c("ID1", "ID2", "County_Name_2011", "County_Population_2011", "County_Population_65_to_69_2011", "County_Population_70_to_74_2011", "County_Population_75_to_79_2011","County_Population_80_to_84_2011","County_Population_Over_85_2011")

TN_County_Population_2011<- TN_County_Population_2011[-c(1), ]

rownames(TN_County_Population_2011) <- seq(length=nrow(TN_County_Population_2011)) 

TN_County_Population_2011$County_Population_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_2011))

TN_County_Population_2011$County_Population_2011[which(TN_County_Population_2011$County_Population_2011 == 0)] <-  mean(TN_County_Population_2011$County_Population_2011)

TN_County_Population_2011$County_Population_65_to_69_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_65_to_69_2011))

TN_County_Population_2011$County_Population_70_to_74_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_70_to_74_2011))

TN_County_Population_2011$County_Population_75_to_79_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_75_to_79_2011))

TN_County_Population_2011$County_Population_80_to_84_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_80_to_84_2011))

TN_County_Population_2011$County_Population_Over_85_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_Over_85_2011))

TN_County_Population_2011$Target_Demographic_Population_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_65_to_69_2011+TN_County_Population_2011$County_Population_70_to_74_2011+TN_County_Population_2011$County_Population_75_to_79_2011+TN_County_Population_2011$County_Population_80_to_84_2011+TN_County_Population_2011$County_Population_Over_85_2011))

TN_County_Population_2011$County_Population_Density_2011 <- round((TN_County_Population_2011$Target_Demographic_Population_2011/TN_County_Population_2011$County_Population_2011), 2)

TN_County_Population_2011$Year_2011 <- '2011'

#View(TN_County_Population_2011)

##--2012--##
TN_County_Population_2012 = read.csv("Data/TN_counties/PEP_2012_PEPAGESEX_with_ann.csv")

#View(TN_County_Population_2012)

TN_County_Population_2012<- TN_County_Population_2012[,c("GEO.id","GEO.id2","GEO.display.label","est72012sex0_age999","est72012sex0_age65to69","est72012sex0_age70to74","est72012sex0_age75to79","est72012sex0_age80to84","est72012sex0_age85plus")]

names(TN_County_Population_2012) <- c("ID1", "ID2", "County_Name_2012", "County_Population_2012", "County_Population_65_to_69_2012", "County_Population_70_to_74_2012", "County_Population_75_to_79_2012","County_Population_80_to_84_2012","County_Population_Over_85_2012")

TN_County_Population_2012<- TN_County_Population_2012[-c(1), ]

rownames(TN_County_Population_2012) <- seq(length=nrow(TN_County_Population_2012)) 

TN_County_Population_2012$County_Population_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_2012))

TN_County_Population_2012$County_Population_2012[which(TN_County_Population_2012$County_Population_2012 == 0)] <-  mean(TN_County_Population_2012$County_Population_2012)

TN_County_Population_2012$County_Population_65_to_69_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_65_to_69_2012))

TN_County_Population_2012$County_Population_70_to_74_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_70_to_74_2012))

TN_County_Population_2012$County_Population_75_to_79_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_75_to_79_2012))

TN_County_Population_2012$County_Population_80_to_84_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_80_to_84_2012))

TN_County_Population_2012$County_Population_Over_85_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_Over_85_2012))

TN_County_Population_2012$Target_Demographic_Population_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_65_to_69_2012+TN_County_Population_2012$County_Population_70_to_74_2012+TN_County_Population_2012$County_Population_75_to_79_2012+TN_County_Population_2012$County_Population_80_to_84_2012+TN_County_Population_2012$County_Population_Over_85_2012))

TN_County_Population_2012$County_Population_Density_2012 <- round((TN_County_Population_2012$Target_Demographic_Population_2012/TN_County_Population_2012$County_Population_2012), 2)

TN_County_Population_2012$Year_2012 <- '2012'


##--2013--##
TN_County_Population_2013 = read.csv("Data/TN_counties/PEP_2013_PEPAGESEX_with_ann.csv")

TN_County_Population_2013<- TN_County_Population_2013[,c("GEO.id","GEO.id2","GEO.display.label","est72013sex0_age999","est72013sex0_age65to69","est72013sex0_age70to74","est72013sex0_age75to79","est72013sex0_age80to84","est72013sex0_age85plus")]

names(TN_County_Population_2013) <- c("ID1", "ID2", "County_Name_2013", "County_Population_2013", "County_Population_65_to_69_2013", "County_Population_70_to_74_2013", "County_Population_75_to_79_2013","County_Population_80_to_84_2013","County_Population_Over_85_2013")

TN_County_Population_2013<- TN_County_Population_2013[-c(1), ]

rownames(TN_County_Population_2013) <- seq(length=nrow(TN_County_Population_2013)) 

TN_County_Population_2013$County_Population_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_2013))

TN_County_Population_2013$County_Population_2013[which(TN_County_Population_2013$County_Population_2013 == 0)] <-  mean(TN_County_Population_2013$County_Population_2013)

TN_County_Population_2013$County_Population_65_to_69_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_65_to_69_2013))

TN_County_Population_2013$County_Population_70_to_74_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_70_to_74_2013))

TN_County_Population_2013$County_Population_75_to_79_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_75_to_79_2013))

TN_County_Population_2013$County_Population_80_to_84_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_80_to_84_2013))

TN_County_Population_2013$County_Population_Over_85_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_Over_85_2013))

TN_County_Population_2013$Target_Demographic_Population_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_65_to_69_2013+TN_County_Population_2013$County_Population_70_to_74_2013+TN_County_Population_2013$County_Population_75_to_79_2013+TN_County_Population_2013$County_Population_80_to_84_2013+TN_County_Population_2013$County_Population_Over_85_2013))

TN_County_Population_2013$County_Population_Density_2013 <- round((TN_County_Population_2013$Target_Demographic_Population_2013/TN_County_Population_2013$County_Population_2013), 2)

TN_County_Population_2013$Year_2013 <- '2013'

##--2014--##
TN_County_Population_2014 = read.csv("Data/TN_counties/PEP_2014_PEPAGESEX_with_ann.csv")

TN_County_Population_2014<- TN_County_Population_2014[,c("GEO.id","GEO.id2","GEO.display.label","est72014sex0_age999","est72014sex0_age65to69","est72014sex0_age70to74","est72014sex0_age75to79","est72014sex0_age80to84","est72014sex0_age85plus")]

names(TN_County_Population_2014) <- c("ID1", "ID2", "County_Name_2014", "County_Population_2014", "County_Population_65_to_69_2014", "County_Population_70_to_74_2014", "County_Population_75_to_79_2014","County_Population_80_to_84_2014","County_Population_Over_85_2014")

TN_County_Population_2014<- TN_County_Population_2014[-c(1), ]

rownames(TN_County_Population_2014) <- seq(length=nrow(TN_County_Population_2014)) 

TN_County_Population_2014$County_Population_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_2014))

TN_County_Population_2014$County_Population_2014[which(TN_County_Population_2014$County_Population_2014 == 0)] <-  mean(TN_County_Population_2014$County_Population_2014)

TN_County_Population_2014$County_Population_65_to_69_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_65_to_69_2014))

TN_County_Population_2014$County_Population_70_to_74_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_70_to_74_2014))

TN_County_Population_2014$County_Population_75_to_79_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_75_to_79_2014))

TN_County_Population_2014$County_Population_80_to_84_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_80_to_84_2014))

TN_County_Population_2014$County_Population_Over_85_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_Over_85_2014))

TN_County_Population_2014$Target_Demographic_Population_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_65_to_69_2014+TN_County_Population_2014$County_Population_70_to_74_2014+TN_County_Population_2014$County_Population_75_to_79_2014+TN_County_Population_2014$County_Population_80_to_84_2014+TN_County_Population_2014$County_Population_Over_85_2014))

TN_County_Population_2014$County_Population_Density_2014 <- round((TN_County_Population_2014$Target_Demographic_Population_2014/TN_County_Population_2014$County_Population_2014), 2)

TN_County_Population_2014$Year_2014 <- '2014'

##--2015--##
TN_County_Population_2015 = read.csv("Data/TN_counties/PEP_2015_PEPAGESEX_with_ann.csv")

TN_County_Population_2015<- TN_County_Population_2015[,c("GEO.id","GEO.id2","GEO.display.label","est72015sex0_age999","est72015sex0_age65to69","est72015sex0_age70to74","est72015sex0_age75to79","est72015sex0_age80to84","est72015sex0_age85plus")]

names(TN_County_Population_2015) <- c("ID1", "ID2", "County_Name_2015", "County_Population_2015", "County_Population_65_to_69_2015", "County_Population_70_to_74_2015", "County_Population_75_to_79_2015","County_Population_80_to_84_2015","County_Population_Over_85_2015")

TN_County_Population_2015<- TN_County_Population_2015[-c(1), ]

rownames(TN_County_Population_2015) <- seq(length=nrow(TN_County_Population_2015)) 

TN_County_Population_2015$County_Population_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_2015))

TN_County_Population_2015$County_Population_2015[which(TN_County_Population_2015$County_Population_2015 == 0)] <-  mean(TN_County_Population_2015$County_Population_2015)

TN_County_Population_2015$County_Population_65_to_69_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_65_to_69_2015))

TN_County_Population_2015$County_Population_70_to_74_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_70_to_74_2015))

TN_County_Population_2015$County_Population_75_to_79_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_75_to_79_2015))

TN_County_Population_2015$County_Population_80_to_84_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_80_to_84_2015))

TN_County_Population_2015$County_Population_Over_85_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_Over_85_2015))

TN_County_Population_2015$Target_Demographic_Population_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_65_to_69_2015+TN_County_Population_2015$County_Population_70_to_74_2015+TN_County_Population_2015$County_Population_75_to_79_2015+TN_County_Population_2015$County_Population_80_to_84_2015+TN_County_Population_2015$County_Population_Over_85_2015))

TN_County_Population_2015$County_Population_Density_2015 <- round((TN_County_Population_2015$Target_Demographic_Population_2015/TN_County_Population_2015$County_Population_2015), 2)

TN_County_Population_2015$Year_2015 <- '2015'

##--2016--##
TN_County_Population_2016 = read.csv("Data/TN_counties/PEP_2016_PEPAGESEX_with_ann.csv")

TN_County_Population_2016<- TN_County_Population_2016[,c("GEO.id","GEO.id2","GEO.display.label","est72016sex0_age999","est72016sex0_age65to69","est72016sex0_age70to74","est72016sex0_age75to79","est72016sex0_age80to84","est72016sex0_age85plus")]

names(TN_County_Population_2016) <- c("ID1", "ID2", "County_Name_2016", "County_Population_2016", "County_Population_65_to_69_2016", "County_Population_70_to_74_2016", "County_Population_75_to_79_2016","County_Population_80_to_84_2016","County_Population_Over_85_2016")

TN_County_Population_2016<- TN_County_Population_2016[-c(1), ]

rownames(TN_County_Population_2016) <- seq(length=nrow(TN_County_Population_2016)) 

TN_County_Population_2016$County_Population_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_2016))

TN_County_Population_2016$County_Population_2016[which(TN_County_Population_2016$County_Population_2016 == 0)] <-  mean(TN_County_Population_2016$County_Population_2016)

TN_County_Population_2016$County_Population_65_to_69_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_65_to_69_2016))

TN_County_Population_2016$County_Population_70_to_74_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_70_to_74_2016))

TN_County_Population_2016$County_Population_75_to_79_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_75_to_79_2016))

TN_County_Population_2016$County_Population_80_to_84_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_80_to_84_2016))

TN_County_Population_2016$County_Population_Over_85_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_Over_85_2016))

TN_County_Population_2016$Target_Demographic_Population_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_65_to_69_2016+TN_County_Population_2016$County_Population_70_to_74_2016+TN_County_Population_2016$County_Population_75_to_79_2016+TN_County_Population_2016$County_Population_80_to_84_2016+TN_County_Population_2016$County_Population_Over_85_2016))

TN_County_Population_2016$County_Population_Density_2016 <- round((TN_County_Population_2016$Target_Demographic_Population_2016/TN_County_Population_2016$County_Population_2016), 2)

TN_County_Population_2016$Year_2016 <- '2016'

##--2017--##
TN_County_Population_2017 = read.csv("Data/TN_counties/PEP_2017_PEPAGESEX_with_ann.csv")

TN_County_Population_2017 <- TN_County_Population_2017[,c("GEO.id","GEO.id2","GEO.display.label","est72010sex0_age999","est72017sex0_age65to69","est72017sex0_age70to74","est72017sex0_age75to79","est72017sex0_age80to84","est72017sex0_age85plus")]

names(TN_County_Population_2017) <- c("ID1", "ID2", "County_Name_2017", "County_Population_2017", "County_Population_65_to_69_2017", "County_Population_70_to_74_2017", "County_Population_75_to_79_2017","County_Population_80_to_84_2017","County_Population_Over_85_2017")

TN_County_Population_2017 <- TN_County_Population_2017[-c(1, 2), ]

rownames(TN_County_Population_2017) <- seq(length=nrow(TN_County_Population_2017)) 

TN_County_Population_2017$County_Population_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_2017))

TN_County_Population_2017$County_Population_2017[which(TN_County_Population_2017$County_Population_2017 == 0)] <-  mean(TN_County_Population_2017$County_Population_2017)

TN_County_Population_2017$County_Population_65_to_69_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_65_to_69_2017))

TN_County_Population_2017$County_Population_70_to_74_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_70_to_74_2017))

TN_County_Population_2017$County_Population_75_to_79_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_75_to_79_2017))

TN_County_Population_2017$County_Population_80_to_84_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_80_to_84_2017))

TN_County_Population_2017$County_Population_Over_85_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_Over_85_2017))

TN_County_Population_2017$Target_Demographic_Population_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_65_to_69_2017+TN_County_Population_2017$County_Population_70_to_74_2017+TN_County_Population_2017$County_Population_75_to_79_2017+TN_County_Population_2017$County_Population_80_to_84_2017+TN_County_Population_2017$County_Population_Over_85_2017))

TN_County_Population_2017$County_Population_Density <- round((TN_County_Population_2017$Target_Demographic_Population_2017/TN_County_Population_2017$County_Population_2017), 2)

TN_County_Population_2017$Year_2017 <- '2017'

#View(TN_County_Population_2017)

##--Total--##

TN_County_Population <- cbind(TN_County_Population_2011, TN_County_Population_2012, TN_County_Population_2013, TN_County_Population_2014, TN_County_Population_2015, TN_County_Population_2016, TN_County_Population_2017)

#View(TN_County_Population)

##--County_Target_Demographic_Population_Predictions

County_Predictions <- data.frame(
  County_ID = numeric(),
  Target_Demographic_Population_2017=numeric(),
  Target_Demographic_Population_2018=numeric(),
  Target_Demographic_Population_2019 = numeric(),
  Target_Demographic_Population_2020=numeric(),
  Target_Demographic_Population_2021=numeric())

County_Raw <- data.frame(
  ID2 = numeric(),
  Target_Demographic_Population_2011=numeric(),
  Target_Demographic_Population_2012=numeric(),
  Target_Demographic_Population_2013=numeric(),
  Target_Demographic_Population_2014=numeric(),
  Target_Demographic_Population_2015=numeric(),
  Target_Demographic_Population_2016=numeric())

County_Name <- data.frame(
  ID = numeric(),
  County_Name = character())

Median_2011 <- median(TN_County_Population$Target_Demographic_Population_2011)

Median_2012 <- median(TN_County_Population$Target_Demographic_Population_2012)

Median_2013 <- median(TN_County_Population$Target_Demographic_Population_2013)

Median_2014 <- median(TN_County_Population$Target_Demographic_Population_2014)

Median_2015 <- median(TN_County_Population$Target_Demographic_Population_2015)

Median_2016 <- median(TN_County_Population$Target_Demographic_Population_2016)

TN_County_Population$County_Name <- str_sub(TN_County_Population$County_Name_2011,1,-12)

optim.control = list(maxit = 2000) 

#View(TN_County_Population)

#str(TN_County_Population)

for(i in unique(TN_County_Population$ID2[1:95])) 
{
  TN_County_Selection <- subset(TN_County_Population, ID2 == i)
  
  
  if (TN_County_Selection$Target_Demographic_Population_2011 == 0 &
      TN_County_Selection$Target_Demographic_Population_2012 == 0 &
      TN_County_Selection$Target_Demographic_Population_2013 == 0 &
      TN_County_Selection$Target_Demographic_Population_2014 == 0 &  
      TN_County_Selection$Target_Demographic_Population_2015 == 0 &
      TN_County_Selection$Target_Demographic_Population_2016 == 0)  
    
  {
    TN_County_Selection$Target_Demographic_Population_2011 <-  Median_2011
    
    TN_County_Selection$Target_Demographic_Population_2012 <-  Median_2012
    
    TN_County_Selection$Target_Demographic_Population_2013 <-  Median_2013
    
    TN_County_Selection$Target_Demographic_Population_2014 <-  Median_2014
    
    TN_County_Selection$Target_Demographic_Population_2015 <-  Median_2015
    
    TN_County_Selection$Target_Demographic_Population_2016 <-  Median_2016
  }
  
  TN_County_Raw <- (TN_County_Selection[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")])
  
  TN_County_Pivot <- as.data.frame(t(TN_County_Selection[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")]))
  
  names(TN_County_Pivot) <- c("Population")
  
  x_County <- arima(TN_County_Pivot$Population, order=c(0,2,2), method="ML")
  
  y_County <- as.data.frame(predict(x_County, n.ahead=5))
  
  names(y_County) <- c("Population", "Standard_Error")
  
  y_County$Population <- round(as.numeric(as.character(y_County$Population)), 0)
  
  z_County <- as.data.frame(t(y_County$Population))
  
  names(z_County) <- c("Target_Demographic_Population_2017", "Target_Demographic_Population_2018", "Target_Demographic_Population_2019","Target_Demographic_Population_2020", "Target_Demographic_Population_2021")
  
  County_Raw <- rbind(subset(TN_County_Population[c("ID2","Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")], ID2 == i), County_Raw)
  
  County_Predictions <- rbind(County_Predictions, z_County)
  
  County_Name <- rbind(subset(TN_County_Population[c("County_Name", "ID2")], ID2 == i), County_Name)
  
  #County_Name <- order(County_Name)
}

County_Name <- County_Name[order(County_Name),]
County_Name <- County_Name %>% drop_na()

County_Raw <- County_Raw[order(County_Raw$ID2),]

County_Predictions <- cbind(County_Name,County_Raw, County_Predictions)

County_Predictions <- County_Predictions[-c(3)]

#View(TN_County_Population)


##---------Quality_Data---------##

Quality_Score = read.csv("Data/Quality_Measure_Data/Star_Ratings.csv")

Quality_Score <- Quality_Score[c(1,2,3,4)]

Quality_Score_TN <- subset(Quality_Score, Provider.State == "TN")

rownames(Quality_Score_TN) <- seq(length=nrow(Quality_Score_TN))

names(Quality_Score_TN) <- c("Provider_ID","Facility_Name","State","Quality_Rating")

#View(Quality_Score_TN)

SNF_2013_TN <- merge(SNF_2013_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2014_TN <- merge(SNF_2014_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2015_TN <- merge(SNF_2015_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

##--Quality_Subsets--##

SNF_2013_TN_Quality_Rating_1 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==1)
SNF_2013_TN_Quality_Rating_1 <- SNF_2013_TN_Quality_Rating_1 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_1) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_1)) 

SNF_2013_TN_Quality_Rating_2 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==2)
SNF_2013_TN_Quality_Rating_2 <- SNF_2013_TN_Quality_Rating_2 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_2) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_2)) 

SNF_2013_TN_Quality_Rating_3 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==3)
SNF_2013_TN_Quality_Rating_3 <- SNF_2013_TN_Quality_Rating_3 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_3) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_3)) 

SNF_2013_TN_Quality_Rating_4 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==4)
SNF_2013_TN_Quality_Rating_4 <- SNF_2013_TN_Quality_Rating_4 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_4) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_4)) 

SNF_2013_TN_Quality_Rating_5 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==5)
SNF_2013_TN_Quality_Rating_5 <- SNF_2013_TN_Quality_Rating_5 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_5) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_5)) 

##--
SNF_2014_TN_Quality_Rating_1 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==1)
SNF_2014_TN_Quality_Rating_1 <- SNF_2014_TN_Quality_Rating_1 %>% drop_na()
rownames(SNF_2014_TN_Quality_Rating_1) <- seq(length=nrow(SNF_2014_TN_Quality_Rating_1)) 

SNF_2014_TN_Quality_Rating_2 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==2)
SNF_2014_TN_Quality_Rating_2 <- SNF_2014_TN_Quality_Rating_2 %>% drop_na()
rownames(SNF_2014_TN_Quality_Rating_2) <- seq(length=nrow(SNF_2014_TN_Quality_Rating_2)) 

SNF_2014_TN_Quality_Rating_3 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==3)
SNF_2014_TN_Quality_Rating_3 <- SNF_2014_TN_Quality_Rating_3 %>% drop_na()
rownames(SNF_2014_TN_Quality_Rating_3) <- seq(length=nrow(SNF_2014_TN_Quality_Rating_3)) 

SNF_2014_TN_Quality_Rating_4 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==4)
SNF_2014_TN_Quality_Rating_4 <- SNF_2014_TN_Quality_Rating_4 %>% drop_na()
rownames(SNF_2014_TN_Quality_Rating_4) <- seq(length=nrow(SNF_2014_TN_Quality_Rating_4)) 

SNF_2014_TN_Quality_Rating_5 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==5)
SNF_2014_TN_Quality_Rating_5 <- SNF_2014_TN_Quality_Rating_5 %>% drop_na()
rownames(SNF_2014_TN_Quality_Rating_5) <- seq(length=nrow(SNF_2014_TN_Quality_Rating_5)) 

##--
SNF_2015_TN_Quality_Rating_1 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==1)
SNF_2015_TN_Quality_Rating_1 <- SNF_2015_TN_Quality_Rating_1 %>% drop_na()
rownames(SNF_2015_TN_Quality_Rating_1) <- seq(length=nrow(SNF_2015_TN_Quality_Rating_1)) 

SNF_2015_TN_Quality_Rating_2 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==2)
SNF_2015_TN_Quality_Rating_2 <- SNF_2015_TN_Quality_Rating_2 %>% drop_na()
rownames(SNF_2015_TN_Quality_Rating_2) <- seq(length=nrow(SNF_2015_TN_Quality_Rating_2)) 

SNF_2015_TN_Quality_Rating_3 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==3)
SNF_2015_TN_Quality_Rating_3 <- SNF_2015_TN_Quality_Rating_3 %>% drop_na()
rownames(SNF_2015_TN_Quality_Rating_3) <- seq(length=nrow(SNF_2015_TN_Quality_Rating_3)) 

SNF_2015_TN_Quality_Rating_4 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==4)
SNF_2015_TN_Quality_Rating_4 <- SNF_2015_TN_Quality_Rating_4 %>% drop_na()
rownames(SNF_2015_TN_Quality_Rating_4) <- seq(length=nrow(SNF_2015_TN_Quality_Rating_4)) 

SNF_2015_TN_Quality_Rating_5 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==5)
SNF_2015_TN_Quality_Rating_5 <- SNF_2015_TN_Quality_Rating_5 %>% drop_na()
rownames(SNF_2015_TN_Quality_Rating_5) <- seq(length=nrow(SNF_2015_TN_Quality_Rating_5)) 

##---------Census Tract Shape File---------##
TN = readOGR("Data/TN_County_Shp/TN_counties.shp")
TN_Tracts = readOGR("Data/cb_2017_47_tract_500k.shp")

TN_Tract_Centroids <- as.data.frame(SpatialPointsDataFrame(gCentroid(TN_Tracts, byid=TRUE), 
                                                           TN_Tracts@data, match.ID=FALSE))

View(TN_Tract_Centroids)

TN_Tract_Centroids <- TN_Tract_Centroids[, c("GEOID","x","y")]

names(TN_Tract_Centroids) <- c("id","long", "lat")

#View(TN_Tract_Centroids)

Centroid_Population_Data <- merge(TN_Tract_Centroids, TN_Tract_Population, by.x=c("id"), by.y=c(2), all.x=TRUE) 

#View(Centroid_Population_Data)

TN_Tract_Centroids <- merge(TN_Tract_Centroids, Centroid_Population_Data, by.x=("id"), by.y=c("id"))

TN_Tract_Centroids <- TN_Tract_Centroids[, c("Tract_Name_2011", "id","long.x","lat.y")]

TN_Tract_Centroids <- separate(TN_Tract_Centroids, Tract_Name_2011, into = c("Tract_Name", "County_Name"), sep = ",")

#View(TN_Tract_Centroids)

names(TN_Tract_Centroids) <- c("Tract_Name","County_Name","ID","long", "lat")

##-------Plotting Tools------##

ggtract<-fortify(TN_Tracts, region = "GEOID")

#centroid <- fortify(getSpPPolygonsLabptSlots(TN_Tracts), region = "GEOID")

ggtract<-merge(ggtract, TN_Tract_Population, by.x=c("id"), by.y=c(2), all.x=TRUE) 
#View(ggtract)

polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>% 
    dplyr::select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

#View(tracts)
tracts_2018 <- unique(ggtract[c("id","Tract_Population_Density_2018")])
tractname <- tracts_2018$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df_2018.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                          data=data.frame(row.names=tractname, tracts_2018))
df_2018.polygon <- df_2018.polygon[order(df_2018.polygon$Tract_Population_Density_2018),]

#####-----Quality Patches----#####

df_2018.polygon$Tract_Population_Density_2018[which(df_2018.polygon$Tract_Population_Density_2018 < 0)] <- df_2018.polygon$Tract_Population_Density_2018*(-1)

df_2018.polygon$Tract_Population_Density_2018[which(df_2018.polygon$Tract_Population_Density_2018 > 1)] <- 1

#View(df_2018.polygon$Tract_Population_Density_2018)
#####-------#####

tracts_2019 <- unique(ggtract[c("id","Tract_Population_Density_2019")])
tractname <- tracts_2019$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df_2019.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                          data=data.frame(row.names=tractname, tracts_2019))
df_2019.polygon <- df_2019.polygon[order(df_2019.polygon$Tract_Population_Density_2019),]

#####-----Quality Patches----#####

df_2019.polygon$Tract_Population_Density_2019[which(df_2019.polygon$Tract_Population_Density_2019 < 0)] <- df_2019.polygon$Tract_Population_Density_2019*(-1)

df_2019.polygon$Tract_Population_Density_2019[which(df_2019.polygon$Tract_Population_Density_2019 > 1)] <- 1

#View(df_2019.polygon$Tract_Population_Density_2019)
#####-------#####


tracts_2020 <- unique(ggtract[c("id","Tract_Population_Density_2020")])
tractname <- tracts_2020$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df_2020.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                          data=data.frame(row.names=tractname, tracts_2020))
df_2020.polygon <- df_2020.polygon[order(df_2020.polygon$Tract_Population_Density_2020),]

#####-----Quality Patches----#####

df_2020.polygon$Tract_Population_Density_2020[which(df_2020.polygon$Tract_Population_Density_2020 < 0)] <- df_2020.polygon$Tract_Population_Density_2020*(-1)

df_2020.polygon$Tract_Population_Density_2020[which(df_2020.polygon$Tract_Population_Density_2020 > 1)] <- 1

#View(df_2020.polygon$Tract_Population_Density_2020)
#####-------#####


tracts_2021 <- unique(ggtract[c("id","Tract_Population_Density_2021")])
tractname <- tracts_2021$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df_2021.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                          data=data.frame(row.names=tractname, tracts_2021))
df_2021.polygon <- df_2021.polygon[order(df_2021.polygon$Tract_Population_Density_2021),]

#####-----Quality Patches----#####

df_2021.polygon$Tract_Population_Density_2021[which(df_2021.polygon$Tract_Population_Density_2021 < 0)] <- df_2021.polygon$Tract_Population_Density_2021*(-1)

df_2021.polygon$Tract_Population_Density_2021[which(df_2021.polygon$Tract_Population_Density_2021 > 1)] <- 1

#View(df_2021.polygon$Tract_Population_Density_2021)
#####-------#####

pal <- colorNumeric(
  palette = "YlGnBu", ##try viridis
  domain = df_2018.polygon$Tract_Population_Density)

pal <- colorNumeric(
  palette = "YlGnBu", ##try viridis
  domain = df_2018.polygon$Tract_Population_Density)

#View(ggtract)

#--Tract_Population_Density_Subsets--##

#hist(subset(ggtract$Tract_Population_Density_2017, ggtract$Tract_Population_Density_2021>0))

GG_Tract_2013_Low <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2013 < 0.1))

GG_Tract_2013_Mid <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2013 & Centroid_Population_Data$Tract_Population_Density_2013 <= 0.2))

GG_Tract_2013_High <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2013 > 0.2))


GG_Tract_2014_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2014 < 0.1)

GG_Tract_2014_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2014 & Centroid_Population_Data$Tract_Population_Density_2014 <= 0.2)

GG_Tract_2014_High <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2014 > 0.2)


GG_Tract_2015_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2015 < 0.1)

GG_Tract_2015_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2015 & Centroid_Population_Data$Tract_Population_Density_2015 <= 0.2)

GG_Tract_2015_High <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2015 > 0.2)


GG_Tract_2016_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2016 < 0.1)

GG_Tract_2016_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2016 & ggtract$Tract_Population_Density_2016 <= 0.2)

GG_Tract_2016_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2016 > 0.2)


GG_Tract_2017_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2017 < 0.1)

GG_Tract_2017_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2017 & ggtract$Tract_Population_Density_2017 <= 0.2)

GG_Tract_2017_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2017 > 0.2)


GG_Tract_2018_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2018 < 0.1)

GG_Tract_2018_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2018 & ggtract$Tract_Population_Density_2018 <= 0.2)

GG_Tract_2018_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2018 > 0.2)


GG_Tract_2019_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2019 < 0.1)

GG_Tract_2019_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2019 & ggtract$Tract_Population_Density_2019 <= 0.2)

GG_Tract_2019_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2019 > 0.2)


GG_Tract_2020_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2020 < 0.1)

GG_Tract_2020_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2020 & ggtract$Tract_Population_Density_2020 <= 0.2)

GG_Tract_2020_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2020 > 0.2)


GG_Tract_2021_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2021 < 0.1)

GG_Tract_2021_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2021 & ggtract$Tract_Population_Density_2021 <= 0.2)

GG_Tract_2021_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2021 > 0.2)


##---Weighted Clustering sandbox---##

cl1 <- (cclust(SNF_2015_TN_Quality_Rating_1, k=38, weights =c(3,3),method="hardcl"))
cl1 <- as.data.frame(parameters(cl1))
names(cl1) <- c("Long","Lat")

#plot(cl1)

cl2 <- (cclust(SNF_2015_TN_Quality_Rating_2, k=46, weights =c(1,1),method="hardcl"))
cl2 <- as.data.frame(parameters(cl2))
names(cl2) <- c("Long","Lat")

#plot(cl2)

cl3 <- (cclust(SNF_2015_TN_Quality_Rating_3, k=53, weights =c(0.25,0.25),method="hardcl"))
cl3 <- as.data.frame(parameters(cl3))
names(cl3) <- c("Long","Lat")

#plot(cl3)

cl4 <- (cclust(SNF_2015_TN_Quality_Rating_4, k=60, weights =c(0.20,0.20),method="hardcl"))
cl4 <- as.data.frame(parameters(cl4))
names(cl4) <- c("Long","Lat")

#plot(cl4)

cl5 <- (cclust(SNF_2015_TN_Quality_Rating_5, k=37, weights =c(0.01,0.01),method="hardcl"))
cl5 <- as.data.frame(parameters(cl5))
names(cl5) <- c("Long","Lat")

#plot(cl5)


###-----2018-----###
cl6_2018 <- cclust(GG_Tract_2018_Low, k=95, weights =c(0.05,0.05),method="hardcl")
cl6_2018 <- as.data.frame(parameters(cl6_2018))
names(cl6_2018) <- c("Long","Lat")

#plot(cl6)

cl7_2018 <- cclust(GG_Tract_2018_Mid, k=95, weights =c(0.5,0.5),method="hardcl")
cl7_2018 <- as.data.frame(parameters(cl7_2018))
names(cl7_2018) <- c("Long","Lat")

#plot(cl7)

cl8_2018 <- cclust(GG_Tract_2018_High, k=95, weights =c(5,5),method="hardcl")
cl8_2018 <- as.data.frame(parameters(cl8_2018))
names(cl8_2018) <- c("Long","Lat")

#plot(cl8)
###--------------###

###-----2019-----###
cl6_2019 <- cclust(GG_Tract_2019_Low, k=95, weights =c(0.05,0.05),method="hardcl")
cl6_2019 <- as.data.frame(parameters(cl6_2019))
names(cl6_2019) <- c("Long","Lat")

#plot(cl6_2019)

cl7_2019 <- cclust(GG_Tract_2019_Mid, k=95, weights =c(0.5,0.5),method="hardcl")
cl7_2019 <- as.data.frame(parameters(cl7_2019))
names(cl7_2019) <- c("Long","Lat")

#plot(cl7_2019)

cl8_2019 <- cclust(GG_Tract_2019_High, k=95, weights =c(5,5),method="hardcl")
cl8_2019 <- as.data.frame(parameters(cl8_2019))
names(cl8_2019) <- c("Long","Lat")

#plot(cl8_2019)
###--------------###

###-----2020-----###
cl6_2020 <- cclust(GG_Tract_2020_Low, k=95, weights =c(0.05,0.05),method="hardcl")
cl6_2020 <- as.data.frame(parameters(cl6_2020))
names(cl6_2020) <- c("Long","Lat")

#plot(cl6_2020)

cl7_2020 <- cclust(GG_Tract_2020_Mid, k=95, weights =c(0.5,0.5),method="hardcl")
cl7_2020 <- as.data.frame(parameters(cl7_2020))
names(cl7_2020) <- c("Long","Lat")

#plot(cl7_2020)

cl8_2020 <- cclust(GG_Tract_2020_High, k=95, weights =c(5,5),method="hardcl")
cl8_2020 <- as.data.frame(parameters(cl8_2020))
names(cl8_2020) <- c("Long","Lat")

#plot(cl8_2020)
###--------------###

###-----2021-----###
cl6_2021 <- cclust(GG_Tract_2021_Low, k=95, weights =c(0.05,0.05),method="hardcl")
cl6_2021 <- as.data.frame(parameters(cl6_2021))
names(cl6_2021) <- c("Long","Lat")

#plot(cl6_2021)

cl7_2021 <- cclust(GG_Tract_2021_Mid, k=95, weights =c(0.5,0.5),method="hardcl")
cl7_2021 <- as.data.frame(parameters(cl7_2021))
names(cl7_2021) <- c("Long","Lat")

#plot(cl7_2021)

cl8_2021 <- cclust(GG_Tract_2021_High, k=95, weights =c(5,5),method="hardcl")
cl8_2021 <- as.data.frame(parameters(cl8_2021))
names(cl8_2021) <- c("Long","Lat")

#plot(cl8_2021)
###--------------###

###-----2018-----###

clusters_2018 <- rbind(cl1, cl2, cl3, cl4, cl5, cl6_2018, cl7_2018, cl8_2018)

km_2018 <- kmeans(clusters_2018, 95)

km_2018_lng <- km_2018$centers[,c(1)]
  
km_2018_lat <- km_2018$centers[,c(2)]

###--------------###

###-----2019-----###

clusters_2019 <- rbind(cl1, cl2, cl3, cl4, cl5, cl6_2019, cl7_2019, cl8_2019)

km_2019 <- kmeans(clusters_2019, 95)

km_2019_lng <- km_2019$centers[,c(1)]

km_2019_lat <- km_2019$centers[,c(2)]
###--------------###

###-----2020-----###

clusters_2020 <- rbind(cl1, cl2, cl3, cl4, cl5, cl6_2020, cl7_2020, cl8_2020)

km_2020 <- kmeans(clusters_2020, 95)

km_2020_lng <- km_2020$centers[,c(1)]

km_2020_lat <- km_2020$centers[,c(2)]

###--------------###

###-----2021-----###

clusters_2021 <- rbind(cl1, cl2, cl3, cl4, cl5, cl6_2021, cl7_2021, cl8_2021)

km_2021 <- kmeans(clusters_2021, 95)

km_2021_lng <- km_2021$centers[,c(1)]

km_2021_lat <- km_2021$centers[,c(2)]

###--------------###


##---------Define body of dashboard---------##

##---------Parameter Lists-------##

Tract_list <- as.list(unique(TN_Tract_Population$Tract_Name))

names(Tract_list) <- unique(TN_Tract_Population$Tract_Name)

County_list <- as.list(unique(TN_County_Population$County_Name))
names(County_list) <- unique(TN_County_Population$County_Name)

Year_list <- as.list(c("2018", "2019", "2020", "2021"))

##---------Data Sets to Files-------##
#write.csv(Tract_list, 'Tract_list.csv')
#write.csv(df_2018.polygon, 'df_2018.polygon.csv')

##---------App Definition-------##

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
      "SNF Optimal Locator",
      tabPanel("TN State Map",
               sidebarPanel(
                 h4(strong("Select Map View")),
                 h6(strong("                                                                                     ")),
                 h6(HTML(text = c("&#9679;")),strong("Population Density Map:"), "Shows a Population Density map of those aged 65 or higher per Census Tract"),
                 h6(HTML(text = c("&#9679;")),strong("Traffic View Map:"),"Shows a map with roads, building, and geographical boundaries"),
                 h6(strong("                                                                                     ")),
                 h4(strong("Select Predictive Year")),
                 selectInput(
                   inputId ="Year",
                   label="Year  :", 
                   choices = Year_list, 
                   selected = "Anderson County")
               ),
               mainPanel(
                 tabsetPanel(
                  tabPanel("Population Density Map", leafletOutput("population_view"),
                           absolutePanel(
                             top = -3, left = "auto", right = -10, bottom = "auto",
                             width = 200, height = 100,draggable = FALSE,
                             style = "opacity: 1; z-index: 10;",
                             HTML(markdownToHTML(fragment.only=TRUE, text=c(
                               "<span style='color:red'>&#9679;</span>",":","<b>", "Existing SNFs","</b>", "<br>",
                               "<span style='color:blue'>&#9679;</span>",":","<b>", "Predicted Optimals","</b>")))
                           )),
                   tabPanel("Traffic View Map", leafletOutput("traffic_view"),
                            absolutePanel(
                              top = -3, left = "auto", right = -10, bottom = "auto",
                              width = 200, height = 100,draggable = FALSE,
                              style = "opacity: 1; z-index: 10;",
                              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                "<span style='color:red'>&#9679;</span>",":","<b>", "Existing SNFs","</b>", "<br>",
                                "<span style='color:blue'>&#9679;</span>",":","<b>", "Predicted Optimals","</b>")))
                            ))
            
                 )
               )
      ),
      tabPanel("TN County Chart", 
               sidebarPanel(
                 h4(strong("Select County")),
                 h6(HTML(text = c("&#9679;")),strong("Note:"), "Population Forecasts are for the", strong("age 65"), "and above demographic"),
                 selectInput(
                   inputId ="County",
                   label="County  :", 
                   choices = County_list))
               ,
               mainPanel(
                 tabsetPanel(
                   tabPanel("County Population Forecast",
                            highchartOutput("county_population")
                            
                   )
                 )
               )
      ),
      tabPanel("TN Tract Chart", 
               sidebarPanel(
                 h4(strong("Select Tract")),
                 h6(HTML(text = c("&#9679;")),strong("Note:"), "Population Forecasts are for the", strong("age 65"), "and above demographic"),
                 selectInput(
                   inputId ="Tract",
                   label="Tract  :", 
                   choices = Tract_list, 
                   selected = "Anderson County : Census Tract 201"))
               ,
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tract Population Forecast",
                            highchartOutput("tract_population")
                            
                   )
                 )
               )
      )
 
      
      
      
      
      )),
  

  server <- function(input, output) {
    
    HC_Year_Series = list(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,2019, 2020, 2021)
    
    
    km_lng <- reactive({
      if (input$Year=="2018")
      {km_lng <- km_2018_lng}
      else if (input$Year=="2019")
      {km_lng <- km_2019_lng}
      else if (input$Year=="2020")
      {km_lng <- km_2020_lng}
      else
      {km_lng <- km_2021_lng}
      })


    km_lat <- reactive({
      if (input$Year=="2018")
      {km_lat <- km_2018_lat}
      else if (input$Year=="2019")
      {km_lat <- km_2019_lat}
      else if (input$Year=="2020")
      {km_lat <- km_2020_lat}
      else
      {km_lat <- km_2021_lat}
    })


    mapdata <- reactive({
    if (input$Year=="2018") 
    {data <- df_2018.polygon} 
    else if (input$Year=="2019") 
    {data <- df_2019.polygon}  
    else if (input$Year=="2020")
    {data <- df_2020.polygon}  
    else 
    {data <- df_2021.polygon} 
      })
    
    pallete <- reactive({
      if (input$Year=="2018")
      {palette <- colorNumeric(
        palette = "YlGnBu",
        domain = df_2018.polygon$Tract_Population_Density_2018)}
      else if (input$Year=="2019")
      {pallete <- colorNumeric(
        palette = "YlGnBu",
        domain = df_2019.polygon$Tract_Population_Density_2019)}
      else if (input$Year=="2020")
      {pallete <- colorNumeric(
        palette = "YlGnBu",
        domain = df_2020.polygon$Tract_Population_Density_2020)}
      else
      {pallete <- colorNumeric(
        palette = "YlGnBu",
        domain = df_2021.polygon$Tract_Population_Density_2021)}
      })

    values <- reactive({
      if (input$Year=="2018")
      {values <- df_2018.polygon$Tract_Population_Density_2018}
      else if (input$Year=="2019")
      {values <- df_2019.polygon$Tract_Population_Density_2019}
      else if (input$Year=="2020")
      {values <- df_2020.polygon$Tract_Population_Density_2020}
      else
      {values <- df_2021.polygon$Tract_Population_Density_2021}
    })
    
    fillcolor <- reactive({
      if (input$Year=="2018") 
      {fillcolor = df_2018.polygon$Tract_Population_Density_2018} 
      else if (input$Year=="2019") 
      {fillcolor = df_2019.polygon$Tract_Population_Density_2019}  
      else if (input$Year=="2020")
      {fillcolor = df_2020.polygon$Tract_Population_Density_2020}  
      else 
      {fillcolor = df_2021.polygon$Tract_Population_Density_2021} 
    })
    

    tract_data_raw <- reactive({
      tractdata <- TN_Tract_Population[c("Tract_Name","Target_Demographic_Population_2011", "Target_Demographic_Population_2012",
                                         "Target_Demographic_Population_2013", "Target_Demographic_Population_2014",
                                         "Target_Demographic_Population_2015", "Target_Demographic_Population_2016",
                                         "Target_Demographic_Population_2017", "Target_Demographic_Population_2018",
                                         "Target_Demographic_Population_2019", "Target_Demographic_Population_2020",
                                         "Target_Demographic_Population_2021")] %>%
                                          filter(Tract_Name == input$Tract)

      tractdata <- tractdata[-c(1)]
      tractdata <- as.data.frame(t(tractdata))
      names(tractdata) <- c("Population")
      rownames(tractdata) <- seq(length=nrow(tractdata))
      tractdata <- tractdata$Population
      tractdata <- tractdata[1:7]
      #View(tractdata)
      
    })
    
    
    tract_data_forecast <- reactive({
      tractdata <- TN_Tract_Population[c("Tract_Name","Target_Demographic_Population_2011", "Target_Demographic_Population_2012",
                                         "Target_Demographic_Population_2013", "Target_Demographic_Population_2014",
                                         "Target_Demographic_Population_2015", "Target_Demographic_Population_2016",
                                         "Target_Demographic_Population_2017", "Target_Demographic_Population_2018",
                                         "Target_Demographic_Population_2019", "Target_Demographic_Population_2020",
                                         "Target_Demographic_Population_2021")] %>%
                                          filter(Tract_Name == input$Tract)
      
      tractdata <- tractdata[-c(1)]
      tractdata <- as.data.frame(t(tractdata))
      names(tractdata) <- c("Population")
      rownames(tractdata) <- seq(length=nrow(tractdata))
      tractdata <- tractdata$Population
      #View(tractdata)
      
    })
    
    #View(Tract_Population_Predictions)

    
    county_data_raw <- reactive({
      countydata <- County_Predictions[c("County_Name","Target_Demographic_Population_2011", "Target_Demographic_Population_2012",
                                                "Target_Demographic_Population_2013", "Target_Demographic_Population_2014",
                                                "Target_Demographic_Population_2015", "Target_Demographic_Population_2016",
                                                "Target_Demographic_Population_2017", "Target_Demographic_Population_2018",
                                                "Target_Demographic_Population_2019", "Target_Demographic_Population_2020",
                                                "Target_Demographic_Population_2021")] %>% 
                                                filter(County_Name == input$County)
      
      countydata <- countydata[-c(1)]
      countydata <- as.data.frame(t(countydata))
      rownames(countydata) <- seq(length=nrow(countydata))
      names(countydata) <- c("Population")
      countydata <- countydata$Population
      countydata <- countydata[1:7]
      #View(countydata)
      
    })
    
    county_data_forecast <- reactive({
      countydata <- County_Predictions[c("County_Name","Target_Demographic_Population_2011", "Target_Demographic_Population_2012",
                                         "Target_Demographic_Population_2013", "Target_Demographic_Population_2014",
                                         "Target_Demographic_Population_2015", "Target_Demographic_Population_2016",
                                         "Target_Demographic_Population_2017", "Target_Demographic_Population_2018",
                                         "Target_Demographic_Population_2019", "Target_Demographic_Population_2020",
                                         "Target_Demographic_Population_2021")] %>% 
                                          filter(County_Name == input$County)
      
      countydata <- countydata[-c(1)]
      countydata <- as.data.frame(t(countydata))
      rownames(countydata) <- seq(length=nrow(countydata))
      names(countydata) <- c("Population")
      countydata <- countydata$Population
      #View(countydata)
      
    })
    

    
    output$population_view <- renderLeaflet({
      leaflet(SNF_2015_TN) %>%
        setView(-86.5804, 35.5175, zoom = 7) %>%
        addTiles() %>%
        addPolygons(data=mapdata(), fillColor = ~pal(fillcolor()),color = "#b2aeae", fillOpacity = 1,
                    weight = 0.3,
                    smoothFactor = 0.2) %>%
        addPolygons(data=TN,weight=0.5,col = 'black', fillColor = "Transparent") %>%
        addCircles(lng = SNF_2015_TN$Long,lat = SNF_2015_TN$Lat, color = "red",
                   popup = paste("<b>","Facility:","</b>", "<i>",SNF_2015_TN$Facility_Name.x,"</i>", "<br>",
                                 "<b>","Quality Rating:","</b>", SNF_2015_TN$Quality_Rating, "<br>")) %>%
        addCircles(lng = km_lng(), lat = km_lat(), color = "blue",
                   popup = paste("<b>", km_lng(), ",",km_lat())) %>%
        addCircles(lng = TN_Tract_Centroids$long, lat = TN_Tract_Centroids$lat, color = "grey", opacity = 0.01,
                   popup = paste(TN_Tract_Centroids$County_Name, ":",TN_Tract_Centroids$Tract_Name)) %>%
        addLegend(pal = colorNumeric(
                  palette = "YlGnBu",
                  domain = values()*100),
                  values = values()*100,
                  position = "bottomleft",
                  title = paste("Population Density","<br>", 
                  "(age 65 and above)", "<br>"),
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
        
        
    })
    
    
    output$traffic_view <- renderLeaflet({
      leaflet(SNF_2015_TN) %>%
        setView(-86.5804, 35.5175, zoom = 7) %>%
        addTiles() %>%
        addPolygons(data=mapdata(), fillColor = "Transparent", color = "#b2aeae", fillOpacity = 1,
                    weight = 0.3,
                    smoothFactor = 0.2) %>%
        addPolygons(data=TN,weight=0.5,col = 'black', fillColor = "Transparent") %>%
        addCircles(lng = SNF_2015_TN$Long,lat = SNF_2015_TN$Lat, color = "red",
                   popup = paste("<b>","Facility:","</b>", "<i>",SNF_2015_TN$Facility_Name.x,"</i>", "<br>",
                                 "<b>","Quality Rating:","</b>", SNF_2015_TN$Quality_Rating, "<br>")) %>%
        addCircles(lng = km_lng(), lat = km_lat(), color = "blue",
                   popup = paste("<b>", km_lng(), ",",km_lat())) %>%
        addCircles(lng = TN_Tract_Centroids$long, lat = TN_Tract_Centroids$lat, color = "grey", opacity = 0.01,
                   popup = paste(TN_Tract_Centroids$County_Name, ":",TN_Tract_Centroids$Tract_Name)) 

    })
    
    
    output$county_population <- renderHighchart({
       highchart() %>% 
        hc_title(text = input$County, align="center") %>% 
        hc_xAxis(categories = HC_Year_Series) %>% 
        hc_add_series(name="Forecasted", data = county_data_forecast(), color = "red") %>%
        hc_add_series(name="Static", data = county_data_raw(), color = "green") %>%
        hc_add_theme(hc_theme_db())
      
      })
    
    output$tract_population <- renderHighchart({
    highchart() %>% 
      hc_title(text = input$Tract, align="center") %>% 
      hc_xAxis(categories = HC_Year_Series) %>% 
      hc_add_series(name="Forecasted", data = tract_data_forecast(), color = "red") %>%
      hc_add_series(name="Static", data = tract_data_raw(), color = "green") %>%
      hc_add_theme(hc_theme_db())
    })
      

    }
  )
    
