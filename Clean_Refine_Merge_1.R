#---
#title: Merge, Clean, and Refine
#author: Hanna Davis
#date: 4/19/2020
#---

#load packages
library(plyr)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

#---

#set working directory
setwd("/Users/hannadavis/Documents/github/MEM_Project/data")

#merge the raw Sample Event Report .csv files from each park
##list files
mydir = "rawsampleeventreports"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

##create a merged dataframe
ALL_SampleEventReports_RAW = ldply(myfiles, read_csv)

#---

#remove duplicate sample event records
ALL_SampleEventReports_RAW <- ALL_SampleEventReports_RAW[!grepl("Y", ALL_SampleEventReports_RAW$Multi_PU),]

#---

#drop and rename variables
##drop unnecessary variables, Multi_PU and Visited
ALL_SampleEventReports_RAW = subset(ALL_SampleEventReports_RAW, select = -c(Multi_PU, Visited))

##rename the other variables
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "RegistrationUnit_Name"] <- "Park"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "ProjectUnit_Name"] <- "Project_Unit"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "SampleEvent_Date"] <- "Sample_Date"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "SampleEvent_DefaultMonitoringStatus"] <- "PM_MonStat_FFI"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "MonitoringStatus_Name"] <- "RA_MonStat_FFI"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "SampleEvent_TreatmentUnit"] <- "Treatment_Unit"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "ProjectUnit_Name"] <- "Project_Unit"
names(ALL_SampleEventReports_RAW)[names(ALL_SampleEventReports_RAW) == "MacroPlot_Name"] <- "MacroPlot_Name_FFI"

#---

#consolidate the rows and create a new variable for each protocol
##subset and create a new dataframe for each protocol
SampleEventReports_withTrees <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Trees - Individuals (metric)")
names(SampleEventReports_withTrees)[names(SampleEventReports_withTrees) == "Protocols"] <- "Protocol_Trees"

SampleEventReports_withSeedlings <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Density - Quadrats (metric)")
names(SampleEventReports_withSeedlings)[names(SampleEventReports_withSeedlings) == "Protocols"] <- "Protocol_Seedlings"

SampleEventReports_withShrubs <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Density - Belts (metric)")
names(SampleEventReports_withShrubs)[names(SampleEventReports_withShrubs) == "Protocols"] <- "Protocol_Shrubs"

SampleEventReports_withVeg <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Cover - Points (metric)")
names(SampleEventReports_withVeg)[names(SampleEventReports_withVeg) == "Protocols"] <- "Protocol_Veg"

SampleEventReports_withFuels <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Surface Fuels")
names(SampleEventReports_withFuels)[names(SampleEventReports_withFuels) == "Protocols"] <- "Protocol_Fuels"

SampleEventReports_withPostburn <- filter(ALL_SampleEventReports_RAW, ALL_SampleEventReports_RAW$Protocols == "Post Burn Severity (metric)")
names(SampleEventReports_withPostburn)[names(SampleEventReports_withPostburn) == "Protocols"] <- "Protocol_Postburn"

##merge the dataframes
SampleEventReports_ProtocolsTS <- merge(SampleEventReports_withTrees, SampleEventReports_withSeedlings, by=c("Park", "Project_Unit", "MacroPlot_Name_FFI", "Sample_Date", "PM_MonStat_FFI", "RA_MonStat_FFI", "Treatment_Unit"), all = TRUE)
SampleEventReports_ProtocolsTSS <- merge(SampleEventReports_ProtocolsTS, SampleEventReports_withShrubs, by=c("Park", "Project_Unit", "MacroPlot_Name_FFI", "Sample_Date", "PM_MonStat_FFI", "RA_MonStat_FFI", "Treatment_Unit"), all = TRUE)
SampleEventReports_ProtocolsTSSV <- merge(SampleEventReports_ProtocolsTSS, SampleEventReports_withVeg, by=c("Park", "Project_Unit", "MacroPlot_Name_FFI", "Sample_Date", "PM_MonStat_FFI", "RA_MonStat_FFI", "Treatment_Unit"), all = TRUE)
SampleEventReports_ProtocolsTSSVF <- merge(SampleEventReports_ProtocolsTSSV, SampleEventReports_withFuels, by=c("Park", "Project_Unit", "MacroPlot_Name_FFI", "Sample_Date", "PM_MonStat_FFI", "RA_MonStat_FFI", "Treatment_Unit"), all = TRUE)
SampleEventReports_AllProtocols <- merge(SampleEventReports_ProtocolsTSSVF, SampleEventReports_withPostburn, by=c("Park", "Project_Unit", "MacroPlot_Name_FFI", "Sample_Date", "PM_MonStat_FFI", "RA_MonStat_FFI", "Treatment_Unit"), all = TRUE)

##change the protocols values to 1 if surveyed and 0 if not surveyed
SampleEventReports_AllProtocols$Protocol_Trees <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Trees == "Trees - Individuals (metric)",]$Protocol_Trees)
SampleEventReports_AllProtocols$Protocol_Seedlings <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Seedlings == "Density - Quadrats (metric)",]$Protocol_Seedlings)
SampleEventReports_AllProtocols$Protocol_Shrubs <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Shrubs == "Density - Belts (metric)",]$Protocol_Shrubs)
SampleEventReports_AllProtocols$Protocol_Veg <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Veg == "Cover - Points (metric)",]$Protocol_Veg)
SampleEventReports_AllProtocols$Protocol_Fuels <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Fuels == "Surface Fuels",]$Protocol_Fuels)
SampleEventReports_AllProtocols$Protocol_Postburn <- 1*!is.na(SampleEventReports_AllProtocols[SampleEventReports_AllProtocols$Protocol_Postburn == "Post Burn Severity (metric)",]$Protocol_Postburn)

#---
  
#remove sample event records where both trees and fuels weren't read
SampleEventReports_PlotsTreesFuels <- SampleEventReports_AllProtocols
SampleEventReports_PlotsTreesFuels <- SampleEventReports_PlotsTreesFuels[!grepl("0", SampleEventReports_PlotsTreesFuels$Protocol_Trees),]
SampleEventReports_PlotsTreesFuels <- SampleEventReports_PlotsTreesFuels[!grepl("0", SampleEventReports_PlotsTreesFuels$Protocol_Fuels),]

#---

#check for inconsistencies in Park values - None found
unique(SampleEventReports_PlotsTreesFuels$Park)
##change Park to a factor
SampleEventReports_PlotsTreesFuels$Park <- factor(SampleEventReports_PlotsTreesFuels$Park)

#---

#check for inconsistencies in Project_Unit values - replace spaces with underscores
unique(SampleEventReports_PlotsTreesFuels$Project_Unit)
SampleEventReports_PlotsTreesFuels$Project_Unit <- gsub("[[:space:]]", "_", SampleEventReports_PlotsTreesFuels$Project_Unit)
##change Project_Unit to a factor
SampleEventReports_PlotsTreesFuels$Project_Unit <- factor(SampleEventReports_PlotsTreesFuels$Project_Unit)

#---
  
#check for inconsistencies in MacroPlot_Name
##create a new variable, MacroPlot_Name_HD
 SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD <- SampleEventReports_PlotsTreesFuels$MacroPlot_Name_FFI

##BAND
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BAND",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BAND",]$MacroPlot_Name_HD <- gsub("B_", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BAND"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BAND",]$MacroPlot_Name_HD <- gsub("B:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BAND"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BAND",]$MacroPlot_Name_HD <- gsub("_", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BAND"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BAND",]$MacroPlot_Name_HD <- gsub(":", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BAND"]
###all BAND MacroPlot_Name_HD values are now in the correct format

##BRCA
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BRCA",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BRCA",]$MacroPlot_Name_HD <- gsub("PIPO-", "_PIPO____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BRCA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "BRCA",]$MacroPlot_Name_HD <- gsub("ABCO-", "_ABCO____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "BRCA"]
##used underscores for missing values, Circle and Sunset Fuels are not in the correct format

##CRLA
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "CRLA",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "CRLA",]$MacroPlot_Name_HD <- gsub("B:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "CRLA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "CRLA",]$MacroPlot_Name_HD <- gsub("C:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "CRLA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "CRLA",]$MacroPlot_Name_HD <- gsub(":", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "CRLA"]
###all CRLA MacroPlot_Name_HD values are now in the correct format

##ELMA
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ELMA",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ELMA",]$MacroPlot_Name_HD <- gsub("B:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ELMA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ELMA",]$MacroPlot_Name_HD <- gsub("B_", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ELMA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ELMA",]$MacroPlot_Name_HD <- gsub(":", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ELMA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ELMA",]$MacroPlot_Name_HD <- gsub("_", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ELMA"]
###all ELMA MacroPlot_Name_HD values are now in the correct format

##GRCA
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD <- gsub("PIAB ", "_PIAB____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "GRCA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD <- gsub("PIED ", "_PIED____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "GRCA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD <- gsub("PIEN ", "_PIEN____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "GRCA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD <- gsub("PIPN ", "_PIPN____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "GRCA"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "GRCA",]$MacroPlot_Name_HD <- gsub("PIPO ", "_PIPO____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "GRCA"]
##used underscores for missing values, but there are many values that could not be reformatted

##LABE
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LABE",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LABE",]$MacroPlot_Name_HD <- gsub("B:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LABE"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LABE",]$MacroPlot_Name_HD <- gsub("C:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LABE"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LABE",]$MacroPlot_Name_HD <- gsub("T:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LABE"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LABE",]$MacroPlot_Name_HD <- gsub(":", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LABE"]
###all LABE MacroPlot_Name_HD values are now in the correct format

##LAVO
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LAVO",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LAVO",]$MacroPlot_Name_HD <- gsub("B:", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LAVO"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "LAVO",]$MacroPlot_Name_HD <- gsub(":", "", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "LAVO"]
###all LAVO MacroPlot_Name_HD values are now in the correct format

##WHIS
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "WHIS",]$MacroPlot_Name_HD)
### =all WHIS MacroPlot_Name_HD values are in the correct format, no edits were necessary

##YOSE
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "YOSE",]$MacroPlot_Name_HD)
###some WHIS MacroPlot_Name_HD values are in the correct format, but Frog are not in the correct format

##ZION
unique(SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD)
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD <- gsub("QUGA-", "_QUGA____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ZION"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD <- gsub("PIPO-", "_PIPO____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ZION"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD <- gsub("ABCO-", "_ABCO____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ZION"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD <- gsub("JUOS-", "_JUOS____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ZION"]
SampleEventReports_PlotsTreesFuels[SampleEventReports_PlotsTreesFuels$Park == "ZION",]$MacroPlot_Name_HD <- gsub("POTR-", "_POTR____", SampleEventReports_PlotsTreesFuels$MacroPlot_Name_HD)[SampleEventReports_PlotsTreesFuels$Park == "ZION"]
##used underscores for missing values

#draft - fix inconsitencies in Sample_Date
#SampleEventReports_PlotsTreesFuels$Sample_Date <- parse_date_time(SampleEventReports_PlotsTreesFuels$Sample_Date, orders=c("%m%d%Y", "%m-%d-%Y %I:%M:%S %p"), exact=TRUE)

#---

#fix monitoring status values
##create a new dataframe and copy PM_MonStat_FFI and RA_MonStat_FFI to two new variables, PM_MonStat_HD and RA_MonStat_HD for modification
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_PlotsTreesFuels
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- SampleEventReports_AllProtocols_MonStat$PM_MonStat_FFI
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- SampleEventReports_AllProtocols_MonStat$RA_MonStat_FFI

SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[ grep("_alt", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, invert = TRUE) , ]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[ grep("_alt", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, invert = TRUE) , ]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[ grep("_ALT", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, invert = TRUE) , ]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[ grep("_ALT", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, invert = TRUE) , ]


##fix PM_MonStat_HD values
###first two digits are the burn cycle, how many times has the plot been treated, 
###the 4 letter word is categorical for how many years before or after the treatment it is - POST for immediate treatment (0-1yr.), PREE for a read before the first treatment, and YEAR for a reread after treatment (1-x yrs)
###the last two number represent how many years after the treatment the plot is being read. PREE and POST will be 00. 
###PREE with digits other than 00 after it represent the order in which they were read before the treatment (these won't be used unless there's something wrong with the 01PREEE00 data and can be adressed individually at that time)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- toupper(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, "YR", "YEAR")
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, "PRE", "PREE")
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("[[:space:]]", "", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, "RE0", "0")

###fix PM_MonStat_HD values that need to be adressed individually
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "YEAR1"]<- "01YEAR01"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "YEAR2"]<- "02YEAR02"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "YEAR5"]<- "02YEAR05"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "POST01YEAR01"]<- "01YEAR01"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "POST01"]<- "01POST00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "POST02"]<- "02POST00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "POST03"]<- "03POST00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "PREE00YEAR01"]<- "00PREE01"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "00PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "PREE00"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "PREE03"]<- "02YEAR08"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "THIN01POST"]<- "01POST00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "THIN01YEAR01"]<- "02YEAR01"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "01THINYEAR10"]<- "01YEAR10"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "00YEAR02"]<- "01YEAR02"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "00YEAR01"]<- "01YEAR01"

###fix PM_MonStat_HD values - add 00 after PREE
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "01PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD[SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD== "02PREE"]<- "02PREE00"

###fix PM_MonStat_HD values - add 00 after POST
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("POST", "POST00", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("POST0000", "POST00", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)

###fix PM_MonStat_HD values - replace PR with PREE
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("PR", "PREE", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("PREEEE", "PREE", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)

###fix PM_MonStat_HD values - replace burn cycle 00 with 01
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD <- gsub("00YEAR", "01YEAR", SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD)

###fix PM_MonStat_HD values - if there's only one digit after year, insert 0 before the the last number (e.g. 01YEAR5 to 01YEAR05)
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo <- substring(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, 7, 8) #get the last two digits
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo <- paste(0, SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo, sep="") #add 0 in front of it
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo <- substring(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo, nchar(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo) - 1, nchar(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo)) #get the last two digits
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_firstsix <- substring(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD, 1, 6) #get the first six digits
SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_HD <- paste(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_firstsix, SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD_lasttwo, sep="") #paste the first six and the last two together
SampleEventReports_AllProtocols_MonStat = subset(SampleEventReports_AllProtocols_MonStat, select = -c(PM_MonStat_HD_lasttwo, PM_MonStat_HD_firstsix,PM_MonStat_HD))
names(SampleEventReports_AllProtocols_MonStat)[names(SampleEventReports_AllProtocols_MonStat) == "PM_MonStat_HD_HD"] <- "PM_MonStat_HD"


##fix similar problems with RA_MonStat_HD values
###first two digits are the burn cycle, how many times has the plot been treated, 
###the 4 letter word is categorical for how many years before or after the treatment it is - POST for immediate treatment (0-1yr.), PREE for a read before the first treatment, and YEAR for a reread after treatment (1-x yrs)
###the last two number represent how many years after the treatment the plot is being read. PREE and POST will be 00
###PREE with digits other than 00 after it represent the order in which they were read before the treatment (these won't be used unless there's something wrong with the 01PREEE00 data and can be adressed individually at that time)
unique(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- toupper(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, "YR", "YEAR")
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, "PRE", "PREE")
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("[[:space:]]", "", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- str_replace_all(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, "RE0", "0")

###fix RA_MonStat_HD values that need to be adressed individually
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE00YEAR01"]<- "00PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE00YEAR02"]<- "01YEAR02"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE00"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "00PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "00PRO1"]<- "01PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE1996"]<- "01PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE1997"]<- "01PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE1998"]<- "01PREE01"  
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE1999"]<- "01PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "PREE2003"]<- "01PREE01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "POST01"]<- "01POST00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "POST02"]<- "02POST00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "POST03"]<- "03POST00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "THIN01POST"]<- "01POST00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "THIN01YEAR01"]<- "01YEAR01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "THIN01YEAR02"]<- "01YEAR02"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01THINYEAR10"]<- "01YEAR10"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "02BURNYEAR1"]<- "02YEAR01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01T&BYEAR06"]<- "01YEAR06"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01POSTTHIN"]<- "01POST00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01THINYEAR01"]<- "01YEAR01"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "00PREETHIN"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01THINYEAR06"]<- "01YEAR06"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESOCT2003"]<- "02YEAR06"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESNOV2004"]<- "02YEAR07"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESDEC2005"]<- "02YEAR08"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESOCT2006"]<- "02YEAR09"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESOCT2008"]<- "02YEAR11"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "TREESSEPT2009"]<- "02YEAR10"

###remove spring reads from Bandelier Trees samples
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[!grepl("TREESMAY2003", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD),]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[!grepl("TREESMAY2004", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD),]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[!grepl("TREESAPR2005", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD),]
SampleEventReports_AllProtocols_MonStat <- SampleEventReports_AllProtocols_MonStat[!grepl("TREESMAY2006", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD),]

###fix RA_MonStat_HD values - add 00 after PREE
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "01PREE"]<- "01PREE00"
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD[SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD== "02PREE"]<- "02PREE00"

###fix RA_MonStat_HD values - add 00 after POST
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("POST", "POST00", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("POST0000", "POST00", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)

###fix RA_MonStat_HD values - replace PR with PREE
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("PR", "PREE", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("PREEEE", "PREE", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)

###fix RA_MonStat_HD values - replace burn cycle 00 with 01
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("00YEAR", "01YEAR", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD <- gsub("00PREE", "01PREE", SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD)

###fix RA_MonStat_HD values - if there's only one digit after year, insert 0 before the the last number (e.g. 01YEAR5 to 01YEAR05)
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo <- substring (SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, 7, 8) #get the last two digits
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo <- paste (0, SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo, sep="") #add 0 in front of it
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo <- substring (SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo, nchar(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo) - 1, nchar(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo)) #get the last two digits
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_firstsix <- substring (SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD, 1, 6) #get the first six digits
SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_HD <- paste (SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_firstsix, SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD_lasttwo, sep="") #paste the first six and the last two together
SampleEventReports_AllProtocols_MonStat = subset(SampleEventReports_AllProtocols_MonStat, select = -c(RA_MonStat_HD_lasttwo, RA_MonStat_HD_firstsix, RA_MonStat_HD))
names(SampleEventReports_AllProtocols_MonStat)[names(SampleEventReports_AllProtocols_MonStat) == "RA_MonStat_HD_HD"] <- "RA_MonStat_HD"


