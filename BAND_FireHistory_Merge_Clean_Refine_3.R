#---
#title: Merge, Clean, and Refine
#author: Hanna Davis
#date: 4/19/2020
#---

#set working directory
setwd("/Users/hannadavis/Documents/github/MEM_Project/data/firehistory/fromaccdb")

#load the .csv files
BAND_Fire_History <- read.csv(file = 'BAND_Fire_History.csv')
BAND_Plot_Info <- read.csv(file = 'BAND_Plot_Info.csv')

#---

#fix Treatment_Unit_FH
as.character(BAND_Plot_Info$Treatment_Unit_FH)
unique(BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("01", "1", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("05", "5", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("05", "5", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("07", "7", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("08", "8", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("09", "9", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("HWY 4", "HWY_4", BAND_Plot_Info$Treatment_Unit_FH)

#merge AND_Fire_History and BAND_Plot_Info
BAND_FireHistory_PlotInfo_RAW <- merge(BAND_Fire_History, BAND_Plot_Info, by ="Treatment_Unit_FH", all.y = TRUE)

#---

#create a new data frame to edit
BAND_FireHistory_PlotInfo <- BAND_FireHistory_PlotInfo_RAW

#---

#change Frst_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Frst_Fire_FH)
BAND_FireHistory_PlotInfo$Frst_Fire_FH <- gsub("10/15/96", "Oct-96", BAND_FireHistory_PlotInfo$Frst_Fire_FH)
parse_date_time(BAND_FireHistory_PlotInfo$Frst_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
#as.Date(BAND_FireHistory_PlotInfo$Frst_Fire_FH, "Y(!)-m(!*)-d(!)", tz="UTC") #draft returns NAs

#---

#change Scnd_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Scnd_Fire_FH)
BAND_FireHistory_PlotInfo$Scnd_Fire_FH <- gsub("4/23/00", "Apr-00", BAND_FireHistory_PlotInfo$Scnd_Fire_FH)
parse_date_time(BAND_FireHistory_PlotInfo$Scnd_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
#as.Date(BAND_FireHistory_PlotInfo$Scnd_Fire_FH, "Y(!)-m(!*)-d(!)", tz="UTC") #draft returns NAs

#---

#change Thrd_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Thrd_Fire_FH)
parse_date_time(BAND_FireHistory_PlotInfo$Thrd_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
#as.Date(BAND_FireHistory_PlotInfo$Thrd_Fire_FH, "Y(!)-m(!*)-d(!)", tz="UTC") #draft returns NAs

#---

#find the plots that burned in the Las Conchas Fire
as.character(BAND_FireHistory_PlotInfo$Frst_Fire_FH)
as.character(BAND_FireHistory_PlotInfo$Scnd_Fire_FH)
as.character(BAND_FireHistory_PlotInfo$Thrd_Fire_FH)

Las_Conchas <- c("Jun-11", "11-Jun")
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Frst_Fire[BAND_FireHistory_PlotInfo$Frst_Fire_FH %in% Las_Conchas] <- TRUE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Frst_Fire[!BAND_FireHistory_PlotInfo$Frst_Fire_FH %in% Las_Conchas] <- FALSE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Scnd_Fire[BAND_FireHistory_PlotInfo$Scnd_Fire_FH %in% Las_Conchas] <- TRUE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Scnd_Fire[!BAND_FireHistory_PlotInfo$Scnd_Fire_FH %in% Las_Conchas] <- FALSE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Thrd_Fire[BAND_FireHistory_PlotInfo$Thrd_Fire_FH %in% Las_Conchas] <- TRUE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Thrd_Fire[!BAND_FireHistory_PlotInfo$Thrd_Fire_FH %in% Las_Conchas] <- FALSE

BAND_FireHistory_PlotInfo$BAND_Las_Conchas[BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Frst_Fire == TRUE | BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Scnd_Fire == TRUE | BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Thrd_Fire == TRUE]<- TRUE
BAND_FireHistory_PlotInfo$BAND_Las_Conchas[BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Frst_Fire == FALSE & BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Scnd_Fire == FALSE & BAND_FireHistory_PlotInfo$BAND_Las_Conchas_Thrd_Fire == FALSE]<- FALSE

#---

#subset BAND_FireHistory_PlotInfo$BAND_Las_Conchas for plots that burned in the Las Conchas Fire
BAND_FireHistory_PlotInfo_Las_Conchas <- BAND_FireHistory_PlotInfo[ which(BAND_FireHistory_PlotInfo$BAND_Las_Conchas == TRUE), ]

#---

#merge BAND_SampleEvent_Macroplot_Merge_Las_Conchas and BAND_FireHistory_PlotInfo, and keep only the plots that burned in the Las Conchas fire
BAND_FireHistory_PlotInfo_Las_Conchas$Park_FH <- "BAND"
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas <- merge(SampleEvent_Macroplot_Merge, BAND_FireHistory_PlotInfo_Las_Conchas, by.x= c("Park", "MacroPlot_Name_HD"), by.y= c("Park_FH", "MacroPlot_Name_FH"), all.x= TRUE)

#---

#find the best pre Las Conchas data
##find the time between the Las Conchas fire and each sample event; number of days in LasConchas_Difftime
LasConchas_Date <- as.Date("2011-6-26")
format(LasConchas_Date, format="m(!*)/d(!)/Y(!)")
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$Sample_Date_FFI <- parse_date_time(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$Sample_Date_FFI, c("m(!*)/d(!)/y(!*)", "m(!*)/d(!)/Y(!)", "m(!*)/d(!)/y(!*) H(!):M(!)", "m(!*)/d(!)/y(!*) I(!):M(!):S(!) Op(!*)", "m(!*)/d(!)/Y(!) H(!):M(!)", "m(!*)/d(!)/Y(!) I(!):M(!):S(!) Op(!*)"))
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$Sample_Date_FFI <- as.Date(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$Sample_Date_FFI, "%Y-%m-%d %H:%M:%S", tz="UTC")
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$LasConchas_DiffTime <- difftime(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$Sample_Date_FFI, LasConchas_Date, units = c("days"))

##PRE - rank the sample events before Las Conchas (1= closest date to fire)
BAND_LasConchas_PRE <- BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas
BAND_LasConchas_PRE <- BAND_LasConchas_PRE %>% 
  group_by(Park, MacroPlot_Name_HD) %>%
  filter(LasConchas_DiffTime < 0) %>%
  mutate(BAND_LasConchas_PRE_Rank = rank(-LasConchas_DiffTime))

BAND_LasConchas_PRE <- dplyr::select(BAND_LasConchas_PRE, Park, MacroPlot_Name_HD, Sample_Date_FFI, BAND_LasConchas_PRE_Rank)
as.data.frame(BAND_LasConchas_PRE)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas <- merge(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas, BAND_LasConchas_PRE, by = c("Park", "MacroPlot_Name_HD", "Sample_Date_FFI"), all= TRUE)

#POST- rank the sample events closest to TWO YEARS post Las Conchas (1= closest date to fire)
BAND_LasConchas_POST <-BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas
BAND_LasConchas_POST <- BAND_LasConchas_POST %>% 
  group_by(Park, MacroPlot_Name_HD) %>%
  filter(LasConchas_DiffTime > 0) %>%
  mutate(LasConchas_DiffTime_POST = (LasConchas_DiffTime + 730))  %>% #add two years worth of days
  mutate(BAND_LasConchas_POST_Rank = rank(LasConchas_DiffTime_POST))

BAND_LasConchas_POST <- dplyr::select(BAND_LasConchas_POST, Park, MacroPlot_Name_HD, Sample_Date_FFI, BAND_LasConchas_POST_Rank)
as.data.frame(BAND_LasConchas_POST)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas <- merge(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas, BAND_LasConchas_POST, by = c("Park", "MacroPlot_Name_HD", "Sample_Date_FFI"), all= TRUE)

##create a new variable, LasConchas_Data that that shows which sample event will be used for pre (1) and which will be used for post (2)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$LasConchas_Data <- NA
as.numeric(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$BAND_LasConchas_PRE_Rank)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$LasConchas_Data[BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$BAND_LasConchas_PRE_Rank == 1] <- 1
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$LasConchas_Data[BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas$BAND_LasConchas_POST_Rank == 1] <- 2

#remove the unnecessary variables
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas = subset(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas, select = -c(BAND_Las_Conchas_Frst_Fire, BAND_Las_Conchas_Scnd_Fire, BAND_Las_Conchas_Thrd_Fire, LasConchas_DiffTime, BAND_LasConchas_PRE_Rank, BAND_LasConchas_POST_Rank))

#---

#reorder the columns
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas <-BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_Las_Conchas[,c(2, 4, 8, 29, 39, 3, 1, 17, 24, 41, 25, 26, 40, 27, 28, 18, 19, 20, 21, 22, 23, 5, 6, 15, 7, 16, 9, 10, 11, 12, 13 , 14, 30, 31, 44, 32, 33, 45, 34, 35, 46, 36, 37, 38, 42, 43, 47:73)]

#---

#load BAND Trees .csv file
setwd("/Users/hannadavis/Documents/github/MEM_Project/data/protocolsdata/trees")
All_BAND_Trees_Raw <- read.csv(file = 'All_BAND_Trees_Raw_Data.csv')

#---



