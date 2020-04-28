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

#---

##create a new data frame to edit
ALL_MacroplotReports <- ALL_MacroplotReports_RAW

#---

#remove the sample event records, leaving only the plot information
ALL_MacroplotReports <- ALL_MacroplotReports[is.na(ALL_MacroplotReports$SampleEventDate),]

#---

#drop and rename variables
##drop unnecessary variables, Multi_PU and Visited
ALL_MacroplotReports = subset(ALL_MacroplotReports, select = -c(MetaData, Comments, Directions, StartPoint, DateOut, FutureVisit, DateIn, Precision, PDOP, Purpose, LocatedBy, MonStatus, DefaultMonStatus, MonStatusOrd, SampleEventDate, SampleEventComment, SampleEventTeam, UV1, UV2, UV3, UV4, UV5, UV6, UV7, UV8))

##delete ELevation_Unit_FFI - all units ft
unique(ALL_MacroplotReports$Elevation_Units)
ALL_MacroplotReports = subset(ALL_MacroplotReports, select = -c(ElevationUnits))

##rename the other variables
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Macroplot"] <- "MacroPlot_Name_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Type"] <- "Plot_Type_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Elevation"] <- "Elevation_ft_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Aspect"] <- "Aspect_Azimuth_deg_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "SlopeHill"] <- "Aspect_Slope_pct_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Azimuth"] <- "Plot_Azimuth_deg_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "SlopeTransect"] <- "Plot_Slope_pct_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Latitude"] <- "Latitude_dd_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Longitude"] <- "Longitude_dd_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "UTM_Zone"] <- "UTM_Zone_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "UTM_X"] <- "UTM_X_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "UTM_Y"] <- "UTM_Y_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Datum"] <- "Datum_FFI"
names(ALL_MacroplotReports)[names(ALL_MacroplotReports) == "Dat"] <- "Datum_FFI"

#---

#check for inconsistencies in Plot_Type_FFI values - None found
unique(ALL_MacroplotReports$Plot_Type_FFI)

#---

#check for inconsistencies in Latitude_dd_FFI values - None found (all decimal degrees)
unique(ALL_MacroplotReports$Latitude_dd_FFI)
## replace 0 with NA
as.numeric(ALL_MacroplotReports$Latitude_dd_FFI)
ALL_MacroplotReports$Latitude_dd_FFI[ALL_MacroplotReports$Latitude_dd_FFI== 0]<- NA
##draft - check if any numbers are less than 1 and if they're all decimal degrees

#---

#check for inconsistencies in Longitude_dd_FFI values
unique(ALL_MacroplotReports$Longitude_dd_FFI)
## replace 0 with NA
as.numeric(ALL_MacroplotReports$Longitude_dd_FFI)
ALL_MacroplotReports$Longitude_dd_FFI[ALL_MacroplotReports$Longitude_dd_FFI== 0]<- NA
##draft - check if any numbers are greater than 1 and if they're all decimal degrees

#---

#check for inconsistencies in UTM_X_FFI values
unique(ALL_MacroplotReports$UTM_X_FFI)
## replace 0 with NA
as.numeric(ALL_MacroplotReports$Longitude_dd_FFI)
ALL_MacroplotReports$UTM_X_FFI[ALL_MacroplotReports$UTM_X_FFI== 0]<- NA
##draft - check number of digits and if they're all greater than 1

#---

#check for inconsistencies in UTM_Y_FFI values
unique(ALL_MacroplotReports$UTM_Y_FFI)
## replace 0 with NA
as.numeric(ALL_MacroplotReports$UTM_Y_FFI)
ALL_MacroplotReports$UTM_Y_FFI[ALL_MacroplotReports$UTM_Y_FFI== 0]<- NA
##draft - check number of digits and if they're all greater than 1

#---

#check for inconsistencies in UTM_Zone_FFI values
unique(ALL_MacroplotReports$UTM_Zone_FFI)
as.factor(ALL_MacroplotReports$UTM_Zone_FFI)
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "10N"]<- "10"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "10T"]<- "10"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11n"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11N"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11 N"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11 n"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11S"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "11 S"]<- "11"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "12S"]<- "12"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "12N"]<- "12"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "12 S"]<- "12"
ALL_MacroplotReports$UTM_Zone_FFI[ALL_MacroplotReports$UTM_Zone_FFI== "13S"]<- "13"
## draft - check values "7636" and "125"

#---

#check for inconsistencies in Elevation_ft_FFI values
unique(ALL_MacroplotReports$Elevation_ft_FFI)
## replace 0 with NA
as.numeric(ALL_MacroplotReports$Elevation_ft_FFI)
ALL_MacroplotReports$Elevation_ft_FFI[ALL_MacroplotReports$Elevation_ft_FFI== 0]<- NA
##draft - check for values greater than 10000ft

#---

#check for inconsistencies in Plot_Azimuth_deg_FFI values
unique(ALL_MacroplotReports$Plot_Azimuth_deg_FFI)
##draft - check for values greater than 360

#---

#check for inconsistencies in Aspect_Azimuth_deg_FFI values
unique(ALL_MacroplotReports$Aspect_Azimuth_deg_FFI)
##draft - check for values greater than 360

#---

#check for inconsistencies in Aspect_Slope_pct_FFI values
unique(ALL_MacroplotReports$Aspect_Slope_pct_FFI)
##draft - check for values greater than 65

#---

#check for inconsistencies in Plot_Slope_pct_FFI values
unique(ALL_MacroplotReports$Plot_Slope_pct_FFI)
##draft - check for values greater than 65

#---

#merge SampleEventReports_AllProtocols_MonStat and ALL_MacroplotReports
as.character(SampleEventReports_AllProtocols_MonStat$Park)
as.character(SampleEventReports_AllProtocols_MonStat$MacroPlot_Name_FFI)
SampleEvent_Macroplot_Merge <- merge(SampleEventReports_AllProtocols_MonStat, ALL_MacroplotReports, by = c("Park", "MacroPlot_Name_FFI"), all.x = TRUE)
#fix this - there are 11 more rows in SampleEvent_Macroplot_Merge than in SampleEventReports_AllProtocols_MonStat

#---

#draft change column order
#SampleEvent_Macroplot_Merge[,c(1,2,14,4,6,16,5,15,3,7,17,18,19,20,21,22,23,24,26,27,25,28,8,9,10,11,12,13)]

#---

#change the variable classes
as.factor(SampleEvent_Macroplot_Merge$Park)
as.factor(SampleEvent_Macroplot_Merge$MacroPlot_Name_FFI)
as.factor(SampleEvent_Macroplot_Merge$Project_Unit_FFI)
as.factor(SampleEvent_Macroplot_Merge$Treatment_Unit_FFI)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Trees)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Seedlings)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Shrubs)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Veg)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Fuels)
as.factor(SampleEvent_Macroplot_Merge$Protocol_Postburn)
as.factor(SampleEvent_Macroplot_Merge$MacroPlot_Name_HD)
as.factor(SampleEvent_Macroplot_Merge$PM_MonStat_HD)
as.factor(SampleEvent_Macroplot_Merge$RA_MonStat_HD)
as.factor(SampleEvent_Macroplot_Merge$Plot_Type_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Latitude_dd_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Longitude_dd_FFI)
as.numeric(SampleEvent_Macroplot_Merge$UTM_X_FFI)
as.numeric(SampleEvent_Macroplot_Merge$UTM_Y_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Elevation_ft_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Plot_Azimuth_deg_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Aspect_Azimuth_deg_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Aspect_Slope_pct_FFI)
as.numeric(SampleEvent_Macroplot_Merge$Plot_Slope_pct_FFI)






#--- DRAFT ----------------------------------------------------------------------------------------------------

##create a new column called MonStat_HD to show new fixed monitoring status
##I will go back and look at the "CHECK"s in the next section
#SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD <- SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD == SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD


#SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$MonStat_HD <- SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$PM_MonStat_HD
#SampleEventReports_AllProtocols_MonStat[!SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$MonStat_HD <- "check"
#SampleEventReports_AllProtocols_MonStat[is.na(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD) & is.na(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD),]$MonStat_HD <- "NA check"
#SampleEventReports_AllProtocols_MonStat[!is.na(SampleEventReports_AllProtocols_MonStat$RA_MonStat_HD) & is.na(SampleEventReports_AllProtocols_MonStat$PM_MonStat_HD),]$MonStat_HD <- SampleEventReports_AllProtocols_MonStat[SampleEventReports_AllProtocols_MonStat$MonStat_Equal_HD,]$PM_MonStat_HD
