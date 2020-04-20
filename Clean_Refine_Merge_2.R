#---
#title: Merge, Clean, and Refine
#author: Hanna Davis
#date: 4/19/2020
#---

#merge the raw Macroplot Report .csv files from each park
##list files
mydir2 = "rawmacroplotreports"
myfiles = list.files(path=mydir2, pattern="*.csv", full.names=TRUE)

##create a merged dataframe
ALL_MacroplotReports_RAW = ldply(myfiles, read_csv)
ALL_MacroplotReports_RAW

#---

#remove the sample event records, leaving only the plot information
ALL_MacroplotReports_RAW <- ALL_MacroplotReports_RAW[is.na(ALL_MacroplotReports_RAW$SampleEventDate),]

#---

#drop and rename variables
##drop unnecessary variables, Multi_PU and Visited
ALL_MacroplotReports_RAW = subset(ALL_MacroplotReports_RAW, select = -c(MetaData, Comments, Directions, StartPoint, DateOut, FutureVisit, DateIn, Precision, PDOP, Purpose, LocatedBy, MonStatus, DefaultMonStatus, MonStatusOrd, SampleEventDate, SampleEventComment, SampleEventTeam, UV1, UV2, UV3, UV4, UV5, UV6, UV7, UV8))

##delete ELevation_Unit - all units ft
unique(ALL_MacroplotReports_RAW$Elevation_Unit)
ALL_MacroplotReports_RAW = subset(ALL_MacroplotReports_RAW, select = -c(ElevationUnits))

##rename the other variables
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Macroplot"] <- "MacroPlot_Name_FFI"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Type"] <- "Plot_Type"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Elevation"] <- "Elevation_ft"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Aspect"] <- "Aspect_Azimuth_deg"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "SlopeHill"] <- "Aspect_Slope_pct"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Azimuth"] <- "Plot_Azimuth_deg"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "SlopeTransect"] <- "Plot_Slope_pct"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Latitude"] <- "Latitude_dd"
names(ALL_MacroplotReports_RAW)[names(ALL_MacroplotReports_RAW) == "Longitude"] <- "Longitude_dd"

#---

#check for inconsistencies in Plot_Type values - None found
unique(ALL_MacroplotReports_RAW$Plot_Type)

#---

#check for inconsistencies in Latitude_dd values - None found (all decimal degrees)
unique(ALL_MacroplotReports_RAW$Latitude_dd)
## replace 0 with NA
ALL_MacroplotReports_RAW$Latitude_dd <- gsub("0.00000", "NA", ALL_MacroplotReports_RAW$Latitude_dd)
##draft - check if any numbers are less than 1 and if they're all decimal degrees

#---

#check for inconsistencies in Longitude_dd values
unique(ALL_MacroplotReports_RAW$Longitude_dd)
## replace 0 with NA
ALL_MacroplotReports_RAW$Longitude_dd <- gsub("0.0000", "NA", ALL_MacroplotReports_RAW$Longitude_dd)
##draft - check if any numbers are greater than 1 and if they're all decimal degrees

#---

#check for inconsistencies in UTM_X values
unique(ALL_MacroplotReports_RAW$UTM_X)
## replace 0 with NA
ALL_MacroplotReports_RAW$UTM_X <- gsub("0", "NA", ALL_MacroplotReports_RAW$UTM_X)
##draft - check number of digits and if they're all greater than 1

#---

#check for inconsistencies in UTM_Y values
unique(ALL_MacroplotReports_RAW$UTM_Y)
## replace 0 with NA
ALL_MacroplotReports_RAW$UTM_Y <- gsub("0", "NA", ALL_MacroplotReports_RAW$UTM_Y)
##draft - check number of digits and if they're all greater than 1

#---

#check for inconsistencies in UTM_Zone values
unique(ALL_MacroplotReports_RAW$UTM_Zone)
as.factor(ALL_MacroplotReports_RAW$UTM_Zone)
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "10N"]<- "10"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "10T"]<- "10"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11n"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11N"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11 N"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11 n"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11S"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "11 S"]<- "11"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "12S"]<- "12"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "12N"]<- "12"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "12 S"]<- "12"
ALL_MacroplotReports_RAW$UTM_Zone[ALL_MacroplotReports_RAW$UTM_Zone== "13S"]<- "13"
## draft - check values "7636" and "125"

#---

#check for inconsistencies in Elevation_ft values
unique(ALL_MacroplotReports_RAW$Elevation_ft)
## replace 0 with NA
ALL_MacroplotReports_RAW$Elevation_ft <- gsub("0", "NA", ALL_MacroplotReports_RAW$Elevation_ft)
##draft - check for values greater than 10000ft

#---

#check for inconsistencies in Plot_Azimuth values
unique(ALL_MacroplotReports_RAW$Plot_Azimuth)
## replace 0 with NA
ALL_MacroplotReports_RAW$Plot_Azimuth <- gsub("0", "NA", ALL_MacroplotReports_RAW$Plot_Azimuth)
##draft - check for values greater than 360

#---

#check for inconsistencies in Aspect_Azimuth values
unique(ALL_MacroplotReports_RAW$Aspect_Azimuth)
## replace 0 with NA
ALL_MacroplotReports_RAW$Aspect_Azimuth <- gsub("0", "NA", ALL_MacroplotReports_RAW$Aspect_Azimuth)
##draft - check for values greater than 360

#---

#check for inconsistencies in Aspect_Slope values
unique(ALL_MacroplotReports_RAW$Aspect_Slope)
## replace 0 with NA
ALL_MacroplotReports_RAW$Aspect_Slope <- gsub("0", "NA", ALL_MacroplotReports_RAW$Aspect_Slope)
##draft - check for values greater than 65

#---

#check for inconsistencies in Plot_Slope values
unique(ALL_MacroplotReports_RAW$plot_Slope)
## replace 0 with NA
ALL_MacroplotReports_RAW$Plot_Slope <- gsub("0", "NA", ALL_MacroplotReports_RAW$Plot_Slope)
##draft - check for values greater than 65

#---

#merge SampleEventReports_AllProtocols_MonStat and ALL_MacroplotReports_RAW
as.character(SampleEventReports_AllProtocols_MonStat$Park)
as.character(SampleEventReports_AllProtocols_MonStat$MacroPlot_Name_FFI)
SampleEvent_Macroplot_Merge <- merge(SampleEventReports_AllProtocols_MonStat, ALL_MacroplotReports_RAW, by.x = c("Park", "MacroPlot_Name_FFI"), by.y = c("Park", "MacroPlot_Name_FFI"))
#fix this - there are 11 more rows in SampleEvent_Macroplot_Merge than in SampleEventReports_AllProtocols_MonStat

#---

#change column order
data.frame(SampleEvent_Macroplot_Merge)
SampleEvent_Macroplot_Merge[,c(1,2,14,4,6,16,5,15,3,7,17,18,19,20,21,22,23,24,26,27,25,28,8,9,10,11,12,13)]

#---

#change the variable classes
as.factor(SampleEvent_Macroplot_Merge$Park)
as.factor(SampleEvent_Macroplot_Merge$MacroPlot_Name_FFI)
as.factor(SampleEvent_Macroplot_Merge$Project_Unit)
as.factor(SampleEvent_Macroplot_Merge$Treatment_Unit)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Trees)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Seedlings)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Shrubs)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Veg)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Fuels)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Postburn)
as.factor(SampleEvent_Macroplot_Merge$MacroPlot_Name_HD)
as.factor(SampleEvent_Macroplot_Merge$PM_MonStat_HD)
as.factor(SampleEvent_Macroplot_Merge$RA_MonStat_HD)
as.factor(SampleEvent_Macroplot_Merge$Plot_Type)
as.numeric(SampleEvent_Macroplot_Merge$Latitude)
#as.numeric(SampleEvent_Macroplot_Merge$Longitude) #draft- coercion error
#as.numeric(SampleEvent_Macroplot_Merge$UTM_X) #draft- coercion error
#as.numeric(SampleEvent_Macroplot_Merge$UTM_Y) #draft- coercion error
#as.numeric(SampleEvent_Macroplot_Merge$Elevation_ft) #draft- coercion error
as.numeric(SampleEvent_Macroplot_Merge$Plot_Azimuth_deg)
as.numeric(SampleEvent_Macroplot_Merge$Aspect_Azimuth_deg)
as.numeric(SampleEvent_Macroplot_Merge$Aspect_Slope_pct)
as.numeric(SampleEvent_Macroplot_Merge$Plot_Slope_pct)




#--- DRAFT ----------------------------------------------------------------------------------------------------

##HELP create a new column called MonStat_HD to show new fixed monitoring status
##I will go back and look at the "CHECK"s in the next section
#SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD <- SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD == SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD


#SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$MonStat_HD <- SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$PM_MonStat_HD
#SampleEventReports_AllProtocols_MonStat[!SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$MonStat_HD <- "check"
#SampleEventReports_AllProtocols_MonStat[is.na(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD) & is.na(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD),]$MonStat_HD <- "NA check"
#SampleEventReports_AllProtocols_MonStat[!is.na(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD) & is.na(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD),]$MonStat_HD <- SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$PM_MonStat_HD
