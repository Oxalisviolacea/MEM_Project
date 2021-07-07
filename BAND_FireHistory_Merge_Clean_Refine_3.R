#---
#title: Merge, Clean, and Refine
#author: Hanna Davis
#date: 4/19/2020
#---

#set working directory
setwd("/Users/hannadavis/Documents/github/MEM_Project/data/firehistory/fromaccdb")

#load the .csv files
BAND_Fire_History <- read.csv(file = 'BAND_Fire_History.csv', stringsAsFactors = FALSE)
BAND_Plot_Info <- read.csv(file = 'BAND_Plot_Info.csv', stringsAsFactors = FALSE)

#---

#fix Treatment_Unit_FH
as.character(BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("01", "1", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("05", "5", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("05", "5", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("07", "7", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("08", "8", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("09", "9", BAND_Plot_Info$Treatment_Unit_FH)
BAND_Plot_Info$Treatment_Unit_FH <- gsub("HWY 4", "HWY_4", BAND_Plot_Info$Treatment_Unit_FH)

#merge BAND_Fire_History and BAND_Plot_Info
BAND_FireHistory_PlotInfo_RAW <- merge(BAND_Fire_History, BAND_Plot_Info, by ="Treatment_Unit_FH", all.y = TRUE)

#---

#create a new data frame to edit
BAND_FireHistory_PlotInfo <- BAND_FireHistory_PlotInfo_RAW

#---

#change Frst_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Frst_Fire_FH)
BAND_FireHistory_PlotInfo$Frst_Fire_FH <- gsub("10/15/96", "Oct-96", BAND_FireHistory_PlotInfo$Frst_Fire_FH)
BAND_FireHistory_PlotInfo$Frst_Fire_FH <- parse_date_time(BAND_FireHistory_PlotInfo$Frst_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
BAND_FireHistory_PlotInfo$Frst_Fire_FH <- as.yearmon(BAND_FireHistory_PlotInfo$Frst_Fire_FH)

#---

#change Scnd_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Scnd_Fire_FH)
BAND_FireHistory_PlotInfo$Scnd_Fire_FH <- gsub("4/23/00", "Apr-00", BAND_FireHistory_PlotInfo$Scnd_Fire_FH)
BAND_FireHistory_PlotInfo$Scnd_Fire_FH <- parse_date_time(BAND_FireHistory_PlotInfo$Scnd_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
BAND_FireHistory_PlotInfo$Scnd_Fire_FH <- as.yearmon(BAND_FireHistory_PlotInfo$Scnd_Fire_FH)

#---

#change Thrd_Fire_FH to a date
as.character(BAND_FireHistory_PlotInfo$Thrd_Fire_FH)
BAND_FireHistory_PlotInfo$Thrd_Fire_FH <- parse_date_time(BAND_FireHistory_PlotInfo$Thrd_Fire_FH, c("y(!*)-b(!)", "b(!)-y(!*)"))
BAND_FireHistory_PlotInfo$Thrd_Fire_FH <- as.yearmon(BAND_FireHistory_PlotInfo$Thrd_Fire_FH)

#---

#find the plots that burned in the Las Conchas Fire
BAND_FireHistory_PlotInfo_LasConchas <- BAND_FireHistory_PlotInfo
as.character(BAND_FireHistory_PlotInfo_LasConchas$Frst_Fire_FH)
as.character(BAND_FireHistory_PlotInfo_LasConchas$Scnd_Fire_FH)
as.character(BAND_FireHistory_PlotInfo_LasConchas$Thrd_Fire_FH)

##create a new T/F variable for the first, second, and third fires; if there was a fire in June 2011, then the column will be TRUE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Frst_Fire[BAND_FireHistory_PlotInfo_LasConchas$Frst_Fire_FH == "Jun 2011"] <- TRUE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Frst_Fire[!BAND_FireHistory_PlotInfo_LasConchas$Frst_Fire_FH == "Jun 2011"] <- FALSE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Scnd_Fire[BAND_FireHistory_PlotInfo_LasConchas$Scnd_Fire_FH == "Jun 2011"] <- TRUE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Scnd_Fire[!BAND_FireHistory_PlotInfo_LasConchas$Scnd_Fire_FH == "Jun 2011"] <- FALSE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Thrd_Fire[BAND_FireHistory_PlotInfo_LasConchas$Thrd_Fire_FH == "Jun 2011"] <- TRUE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Thrd_Fire[!BAND_FireHistory_PlotInfo_LasConchas$Thrd_Fire_FH == "Jun 2011"] <- FALSE

##create a new T/F variable if the plot burned in the Las Conchas fire
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas[BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Frst_Fire == TRUE | BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Scnd_Fire == TRUE | BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Thrd_Fire == TRUE]<- TRUE
BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas[BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Frst_Fire == FALSE & BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Scnd_Fire == FALSE & BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_Thrd_Fire == FALSE]<- FALSE

#---

#merge BAND_SampleEvent_Macroplot_Merge_LasConchas and BAND_FireHistory_PlotInfo
BAND_FireHistory_PlotInfo_LasConchas$Park_FH <- "BAND"
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- merge(SampleEvent_Macroplot_Merge, BAND_FireHistory_PlotInfo_LasConchas, by.x= c("Park", "MacroPlot_Name_HD"), by.y= c("Park_FH", "MacroPlot_Name_FH"), all.x= TRUE)

#---

#subset BAND_FireHistory_PlotInfo_LasConchas$BAND_LasConchas for plots that burned in the Las Conchas Fire
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas %>%
  subset(BAND_LasConchas == TRUE) %>%
  drop_na(BAND_LasConchas)

#---

#find the best pre Las Conchas data
##find the time between the Las Conchas fire and each sample event; number of days in LasConchas_Difftime
LasConchas_Date <- as.Date("2011-6-26")
format(LasConchas_Date, format="m(!*)/d(!)/Y(!)")
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI <- parse_date_time(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI, c("m(!*)/d(!)/y(!*)", "m(!*)/d(!)/Y(!)", "m(!*)/d(!)/y(!*) H(!):M(!)", "m(!*)/d(!)/y(!*) I(!):M(!):S(!) Op(!*)", "m(!*)/d(!)/Y(!) H(!):M(!)", "m(!*)/d(!)/Y(!) I(!):M(!):S(!) Op(!*)"))
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI <- as.Date(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI, "%Y-%m-%d %H:%M:%S", tz="UTC")
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$LasConchas_DiffTime <- difftime(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI, LasConchas_Date, units = c("days"))

##PRE - rank the sample events before Las Conchas (1= closest date to fire), ,and not a postburn read because some protocols aren't collected
BAND_LasConchas_PRE <- BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas
BAND_LasConchas_PRE <- BAND_LasConchas_PRE %>% 
  group_by(Park, MacroPlot_Name_HD) %>%
  filter(Protocol_Postburn == 0) %>%
  filter(LasConchas_DiffTime < 0) %>%
  mutate(BAND_LasConchas_PRE_Rank = rank(-LasConchas_DiffTime))

BAND_LasConchas_PRE <- dplyr::select(BAND_LasConchas_PRE, Park, MacroPlot_Name_HD, Sample_Date_FFI, BAND_LasConchas_PRE_Rank)
as.data.frame(BAND_LasConchas_PRE)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- merge(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas, BAND_LasConchas_PRE, by = c("Park", "MacroPlot_Name_HD", "Sample_Date_FFI"), all= TRUE)


#POST- rank the sample events closest to TWO YEARS post Las Conchas (1= closest date to fire),and not a postburn read because some protocols aren't collected
BAND_LasConchas_POST <-BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas
BAND_LasConchas_POST <- BAND_LasConchas_POST %>% 
  group_by(Park, MacroPlot_Name_HD) %>%
  filter(Protocol_Postburn == 0) %>%
  filter(LasConchas_DiffTime > 0) %>%
  mutate(LasConchas_DiffTime_POST = (LasConchas_DiffTime + 730))  %>% #add two years worth of days
  mutate(BAND_LasConchas_POST_Rank = rank(LasConchas_DiffTime_POST))

BAND_LasConchas_POST <- dplyr::select(BAND_LasConchas_POST, Park, MacroPlot_Name_HD, Sample_Date_FFI, BAND_LasConchas_POST_Rank)
as.data.frame(BAND_LasConchas_POST)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- merge(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas, BAND_LasConchas_POST, by = c("Park", "MacroPlot_Name_HD", "Sample_Date_FFI"), all= TRUE)

##create a new variable, LasConchas_Data that that shows which sample event will be used for pre (1) and which will be used for post (2)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$LasConchas_Data <- NA
as.numeric(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_PRE_Rank)
as.numeric(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_POST_Rank)
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$LasConchas_Data[BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_PRE_Rank == 1] <- 1
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$LasConchas_Data[BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$BAND_LasConchas_POST_Rank == 1] <- 2

#---

#delete plots or sample events with issues
##delete all FPSME1T0809 records - there aren't any trees in FPSME1T0809 4/28/2008, and 12/5/05 was sampled data before another fire
##delete all FPSME1T0838 records - post trees have status X
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas %>%
  filter(!(MacroPlot_Name_HD == "FPSME1T0809" | MacroPlot_Name_HD == "FPSME1T0838"))

#---

#remove the unnecessary variables
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas = subset(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas, select = -c(BAND_LasConchas_Frst_Fire, BAND_LasConchas_Scnd_Fire, BAND_LasConchas_Thrd_Fire, LasConchas_DiffTime, BAND_LasConchas_PRE_Rank, BAND_LasConchas_POST_Rank))

#---

#reorder the columns
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <-BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas[,c(2, 4, 8, 29, 39, 3, 1, 17, 24, 41, 25, 26, 40, 27, 28, 18, 19, 20, 21, 22, 23, 5, 6, 15, 7, 16, 9, 10, 11, 12, 13 , 14, 30, 31, 44, 32, 33, 45, 34, 35, 46, 36, 37, 38, 42, 43, 47:73)]

#---

#subset BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas to only include PRE and POST data
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas <- BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas %>%
  subset(LasConchas_Data == 1 | LasConchas_Data == 2) %>%
  drop_na(LasConchas_Data)

#---

#load BAND Trees .csv file
setwd("/Users/hannadavis/Documents/github/MEM_Project/data/protocolsdata/trees")
All_BAND_Trees_Raw <- read.csv(file = 'All_BAND_Trees_Raw_Data.csv', stringsAsFactors=FALSE)

#---

#create a new dataframe with the plot information
as.numeric(All_BAND_Trees_Raw$MacroPlotSize)
BAND_Trees_PlotInfo <- All_BAND_Trees_Raw %>%
  filter(MacroPlotSize == 0.10 | MacroPlotSize == 0.05 | MacroPlotSize == 0.50) %>%
  select(Date, MacroPlot.Name, MacroPlotSize)

#---

#create a dataframe with just the trees
All_BAND_JustTrees <- All_BAND_Trees_Raw %>%
  filter(is.na(MacroPlotSize)) %>%
  select(Date, SampleEvent_TreatmentUnit,SampleEvent_DefaultMonitoringStatus, MacroPlot.Name, Species.Symbol, Index, QTR, SubFrac, TagNo, Status, DBH, Ht, CrwnRto, CrwnCl, LiCrBHt, CharHt, ScorchHt, CrScPct, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5)

#---

#merge the plot information dataframe and the trees data frame
All_BAND_Trees <- merge(BAND_Trees_PlotInfo, All_BAND_JustTrees, by= c("MacroPlot.Name", "Date"), all.y=TRUE)
as.factor(All_BAND_Trees$MacroPlot.Name)
as.factor(All_BAND_Trees$Date)
as.numeric(All_BAND_Trees$TagNo)

#---

#this tree was entered twice, once on the wrong plot. FPIPO1G0903 should not have any trees
All_BAND_Trees <- filter(All_BAND_Trees, !(MacroPlot.Name == "B_FPIPO1G09_03" & Date == "10/16/14" & TagNo == "7104"))

#---

#change All_BAND_Trees$Date, BAND_Trees_PlotInfo, and BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI to matching characters
as.character(All_BAND_Trees$Date)
parse_date_time(All_BAND_Trees$Date, c("m(!*)/d(!)/Y(!) I(!):M(!):S(!) Op(!*)"))
as.character(All_BAND_Trees$Date)
All_BAND_Trees$Date <- format(as.POSIXct(All_BAND_Trees$Date,format= "%m/%d/%Y %H:%M:%S %p"),format='%Y-%m-%d')
BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI <- as.character(BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas$Sample_Date_FFI)

#---

#merge BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas and All_BAND_Trees
BAND_Trees_LasConchas_Merge <- merge(All_BAND_Trees, BAND_SampleEvent_Macroplot_FireHistory_PlotInfo_LasConchas, by.x = c("MacroPlot.Name", "Date"), by.y= c("MacroPlot_Name_FFI", "Sample_Date_FFI"), all.y = TRUE)

#---

#subset BAND_Trees_LasConchas to only include PRE and POST data
BAND_Trees_LasConchas_Merge <- BAND_Trees_LasConchas_Merge %>%
  subset(LasConchas_Data == 1 | LasConchas_Data == 2) %>%
  drop_na(LasConchas_Data)

#create a new data frame for PRE and POST data
#PRE - PA 14 trees all died and recived new tags, so keep them in the pre data, but not the post data
##remove species that are not tagged and species that are not trees (QUEGAM, QUEUND, ROBNEO, ACEGLA, POAANN, PSEJAM, ACENEG, PINEDU, PRUVIR, VALACU), and PINEDU's sample size is too small
as.factor(BAND_Trees_LasConchas_Merge$Species.Symbol)
BAND_Trees_LasConchas_INDV_PRE <- BAND_Trees_LasConchas_Merge %>%
  filter(LasConchas_Data == 1) %>%
  filter(Species.Symbol == "PINPON"| Species.Symbol == "JUNMON" | Species.Symbol == "PSEMEN" | Species.Symbol == "ABICON" | Species.Symbol == "PINFLE"|
           Species.Symbol == "POPTRE"| Species.Symbol == "PICPUN"|Species.Symbol == "PICENG")


#POST 
BAND_Trees_LasConchas_INDV_POST <- BAND_Trees_LasConchas_Merge %>%
  filter(LasConchas_Data == 2) %>%
  filter(Treatment_Unit_FH != "14")%>%
  filter(Species.Symbol == "PINPON"| Species.Symbol == "JUNMON" | Species.Symbol == "PSEMEN" | Species.Symbol == "ABICON" | Species.Symbol == "PINFLE"|
           Species.Symbol == "POPTRE"| Species.Symbol == "PICPUN"|Species.Symbol == "PICENG") %>%
  select(MacroPlot.Name, Date, SampleEvent_DefaultMonitoringStatus, MacroPlotSize, Treatment_Unit_FH, Species.Symbol, Index, QTR, SubFrac, TagNo, Status, DBH, Ht, CrwnRto, CrwnCl, LiCrBHt, CharHt, ScorchHt, CrScPct, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, LasConchas_Data)

#PA 14 POST
BAND_Trees_LasConchas_INDV_POST_PA14 <- BAND_Trees_LasConchas_Merge %>%
  filter(LasConchas_Data == 2) %>%
  filter(Treatment_Unit_FH == "14")%>%
  filter(Species.Symbol == "PINPON"| Species.Symbol == "JUNMON" | Species.Symbol == "PSEMEN" | Species.Symbol == "ABICON" | Species.Symbol == "PINFLE"|
           Species.Symbol == "POPTRE"| Species.Symbol == "PICPUN"|Species.Symbol == "PICENG") %>%
  select(MacroPlot.Name, Date, SampleEvent_DefaultMonitoringStatus, MacroPlotSize, Treatment_Unit_FH, LasConchas_Data)

#---

#create a new data frame that shows all of the trees that were tagged after the fire
##pole size trees recieve a tag if they exceed 2.5cm DBH. If the plot hadn't been read in awhile, seedlings may have grown into pole size trees and recived a new tag
##larger trees that could not have resonably grown into seedlings may have been retagged and the previous tag number was not changed
BAND_Trees_LasConchas_INDV_Mort_Anti_POSTPRE <- anti_join(BAND_Trees_LasConchas_INDV_POST, BAND_Trees_LasConchas_INDV_PRE, by= c("MacroPlot.Name", "TagNo"))

#create a new data frame that shows all of the trees
BAND_Trees_LasConchas_INDV_Mort_Full_withoutPA14 <- full_join(BAND_Trees_LasConchas_INDV_PRE, BAND_Trees_LasConchas_INDV_POST, by= c("MacroPlot.Name", "TagNo"))

#---

#add PA 14 plot information
BAND_Trees_LasConchas_INDV_Mort_Full <- merge(BAND_Trees_LasConchas_INDV_Mort_Full_withoutPA14, BAND_Trees_LasConchas_INDV_POST_PA14, by= "MacroPlot.Name", all.x=TRUE)

#---

#get rid of the trees that came up after Las Conchas, and create a new column for Mortality
as.factor(BAND_Trees_LasConchas_INDV_Mort_Full$Status.x)
as.factor(BAND_Trees_LasConchas_INDV_Mort_Full$Status.y)
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "L" & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "L"]<- FALSE #the tree survived
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "L" & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "D"]<- TRUE #the tree did not survive
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "L" & is.na(BAND_Trees_LasConchas_INDV_Mort_Full$Status.y)]<- TRUE #the tree did not survive
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "D" & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "D"]<- "CBA" #can not be assessed
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "D" & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "L"]<- "CHECK_TAG" #these need to be checked!
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Status.x == "D" & is.na(BAND_Trees_LasConchas_INDV_Mort_Full$Status.y)]<- "CBA" #can not be assessed
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[is.na(BAND_Trees_LasConchas_INDV_Mort_Full$Status.x) & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "L"]<- "CHECK_DBH" #these need to be checked if the tree has a large DBH
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[is.na(BAND_Trees_LasConchas_INDV_Mort_Full$Status.x) & BAND_Trees_LasConchas_INDV_Mort_Full$Status.y == "D"]<- "CHECK_DBH" #these need to be checked if the tree has a large DBH
BAND_Trees_LasConchas_INDV_Mort_Full$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_Full$Treatment_Unit_FH.x == "14"]<- FALSE #none of the trees in PA 14 survived and they were all retagged

#---

#create a new dataframe with the trees that can be analyzed
BAND_Trees_LasConchas_INDV_Mort <- BAND_Trees_LasConchas_INDV_Mort_Full %>%
  filter(BAND_LasConchas_Mortality == "TRUE" | BAND_LasConchas_Mortality == "FALSE")

#---

#rename some of the variables
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Date.x"] <- "PRE_SampleDate"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Date.y"] <- "POST_SampleDate"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "DBH.x"] <- "PRE_DBH"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "DBH.y"] <- "POST_DBH"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Ht.x"] <- "PRE_Height"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Ht.y"] <- "POST_Height"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "CrwnCl.x"] <- "PRE_CrownClass"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "CrwnCl.y"] <- "POST_CrownClass"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "LiCrBHt.x"] <- "PRE_LiCrBHt"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "LiCrBHt.y"] <- "POST_LiCrBHt"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Species.Symbol.x"] <- "PRE_Species"
names(BAND_Trees_LasConchas_INDV_Mort)[names(BAND_Trees_LasConchas_INDV_Mort) == "Species.Symbol.y"] <- "POST_Species"

#---

#remove trees that don't have the information we want to analyze
BAND_Trees_LasConchas_INDV_Mort <- BAND_Trees_LasConchas_INDV_Mort %>%
  filter(!is.na(PRE_Species) & !is.na(PRE_DBH) & !is.na(PRE_LiCrBHt) & !is.na(PRE_Height))

#---

#create graphs that compare PRE and POST DBH and Height
#graph that compares DBH and height with crown classes in different colors
as.numeric(BAND_Trees_LasConchas_INDV_Mort$DBH.x)
as.numeric(BAND_Trees_LasConchas_INDV_Mort$Ht.x)

#create new variables for analysis
BAND_Trees_LasConchas_INDV_Mort <- BAND_Trees_LasConchas_INDV_Mort %>%
  mutate(BAND_LasConchas_DBHDiff = POST_DBH - PRE_DBH) %>%
  mutate(BAND_LasConchas_HeightDiff = POST_Height - PRE_Height) %>%
  mutate(HeighttoDBHRatio = PRE_Height/PRE_DBH) %>%
  mutate(SampleDate_DiffDay = difftime(POST_SampleDate, PRE_SampleDate, units= "days")) %>%
  mutate(PRE_SampleDate_Year = year(PRE_SampleDate)) %>%
  mutate(POST_SampleDate_Year = year(POST_SampleDate)) %>%
  mutate(SampleDate_DiffYear = POST_SampleDate_Year - PRE_SampleDate_Year)

#boxplot-  change in DBH vs years between sample dates
as.character(BAND_Trees_LasConchas_INDV_Mort$SampleDate_DiffYear)
BAND_Trees_LasConchas_INDV_Mort$SampleDate_DiffYear[is.na(BAND_Trees_LasConchas_INDV_Mort$POST_SampleDate_Year)] <- "NA"
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data = BAND_Trees_LasConchas_INDV_Mort, aes(x=SampleDate_DiffYear, y=BAND_LasConchas_DBHDiff)) + geom_boxplot() +
labs(x = "Time between Las Conchas Samples (years)", y= "Difference between Pre and Post- Las Conchas DBH (cm)")

#boxplot-  change in Height vs years between sample dates
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=SampleDate_DiffYear, y=BAND_LasConchas_HeightDiff)) + geom_boxplot() +
  labs(x = "Time between Las Conchas Samples (years)", y= "Difference between Pre and Post- Las Conchas Tree Height (m)", color= "PRE- Las Conchas Crown Class")

#scatter plot-  change in Height vs years between sample dates
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=SampleDate_DiffDay, y=BAND_LasConchas_HeightDiff, color=PRE_CrownClass)) + geom_point() +
  scale_color_discrete(breaks=c("D", "C", "I", "SC", "RS", "LBS", "CS", "BAD", "X", "NA")) +
  labs(x = "Time between Las Conchas Samples (days)", y= "Difference between Pre and Post- Las Conchas Tree Height (m)", color= "PRE- Las Conchas Crown Class")

#scatter plot-  DBH vs. tree height
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=PRE_DBH, y=PRE_Height, color=PRE_CrownClass)) + geom_point() +
  scale_color_discrete(breaks=c("D", "C", "I", "SC", "RS", "LBS", "CS", "BAD", "X", "NA")) +
  labs(x = "Pre-Las Conchas Tree DBH", y= "PRE-Las Conchas Tree Height", color= "PRE- Las Conchas Crown Class")

#scatter plot-  DBH vs. tree height with regression line
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=PRE_DBH, y=PRE_Height)) + geom_point() +
  labs(x = "Pre-Las Conchas Tree DBH", y= "PRE-Las Conchas Tree Height") +
  geom_smooth(method=lm)

#scatter plot- live crown base height vs. tree height
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=PRE_Height, y=PRE_LiCrBHt, color=PRE_Species)) + geom_point() +
  labs(x = "Pre-Las Conchas Tree Height", y= "PRE-Las Conchas Live Crown Base Height", color= "Species")

#scatter plot- live crown base height vs. tree height with regression line
ggplot(data=BAND_Trees_LasConchas_INDV_Mort, aes(x=PRE_Height, y=PRE_LiCrBHt)) + geom_point() +
  labs(x = "Pre-Las Conchas Tree Height", y= "PRE-Las Conchas Live Crown Base Height") +
  geom_smooth(method=lm)

#---

#create a new data frame without outliers
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort

#---

#PRE_CrownClass should not have dead tree crown class codes- this brings into question if the status (L or D) was correct
as.factor(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_CrownClass)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers%>%
  filter(PRE_CrownClass != "RS" & PRE_CrownClass != "LBS" & PRE_CrownClass != "CS" & PRE_CrownClass != "BAD" & PRE_CrownClass != "S") %>%
  mutate(PRE_CrownClass = replace(PRE_CrownClass, PRE_CrownClass== "", "X")) 

#---

#check for outliers/mistakes in Species - see if PRE and POST values are the same; check FALSE values against the paper data
as.factor(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species)
as.factor(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$POST_Species)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_SameSpecies[BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species == BAND_Trees_LasConchas_INDV_Mort_NoOutliers$POST_Species] <- TRUE
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_SameSpecies[BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species != BAND_Trees_LasConchas_INDV_Mort_NoOutliers$POST_Species] <- FALSE

#since the number of samples we have for PICENG and PICPUN are low (PICENG = 24 and PICPUN = 16), we can combine them into PICEA9
summary(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species[BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species == "PICPUN" | BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species == "PICENG"] <- "PICEA9"
summary(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$POST_Species)
#---

#check for outliers/mistakes in DBH
as.numeric(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff)
summary(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff)
stat.desc(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff)
describe(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff)
hist(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff, xlab= "Difference in DBH (cm)", main="Difference between PRE and POST DBH")
quantile(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_DBHDiff, probs = c(0, 0.005, 0.01, 0.98, 0.99, 0.995), na.rm = TRUE)

#create a new dataframe and subset for values where the difference in DBH is <9.5 & <-10.800 (all of the data is within the 99th percentile)
## this allows for tree growth in between sample events and allows for some sampling error
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(BAND_LasConchas_DBHDiff < 9.5 & BAND_LasConchas_DBHDiff > (-10.800))

#---

#check for outliers/mistakes in Height
summary(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff)
stat.desc(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff)
describe(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff)
hist(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff, xlab= "Difference in Tree Height (m)", main="Difference between Post and Pre-Las Conchas Tree Height")

#remove values where the difference in tree height is greater than the greatest measured height of the tree
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers%>%
  filter(BAND_LasConchas_HeightDiff < PRE_Height)

#remove trees with a height less than 1.37m (breast height)- trees that do not have a DBH are seedlings
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(PRE_Height > 1.37)

#get an idea of how much conifer (not POPTREs) that survived the fire have grown after 5-10years
BAND_LasConchas_HeightDiff_NoMort5moreYR <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(BAND_LasConchas_Mortality == FALSE & SampleDate_DiffYear > 5 & PRE_CrownClass != "BAD")
describe(BAND_LasConchas_HeightDiff_NoMort5moreYR$BAND_LasConchas_HeightDiff)
#the highest value is 8, but we'll use 16 just incase there were aspens
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(BAND_LasConchas_HeightDiff < 16)

#remove any extreme values that are left
quantile(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff, probs = c(0, 0.001, 0.002, 0.50, 0.99, 0.995, 1.0), na.rm = TRUE)
describe(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_HeightDiff)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(BAND_LasConchas_HeightDiff > (-23))

#---

#check for outliers/mistakes in live crown base height
summary(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_LiCrBHt)
stat.desc(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_LiCrBHt)
describe(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_LiCrBHt)

#CBH should not exceed tree height
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  filter(PRE_LiCrBHt < PRE_Height)

#---

#calculate crown ratio
BAND_Trees_LasConchas_INDV_Mort_NoOutliers <- BAND_Trees_LasConchas_INDV_Mort_NoOutliers %>%
  mutate(PRE_CrownRatio = (PRE_LiCrBHt - PRE_Height/PRE_Height))

#---

#box plot - Pre_Las Conchas DBH by Species
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=PRE_Species, y=PRE_DBH, color=PRE_Species)) + geom_boxplot() +
  scale_colour_discrete(name="Pre- Las Conchas Species", labels=c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii")) +
  ggtitle("Pre-Las Conchas DBH by Species") + labs(y= "Pre- Las Conchas Diameter at Breast Height (cm)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#box plot - Pre- Las Conchas CBH by Species
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=PRE_Species, y=PRE_LiCrBHt, color=PRE_Species)) + geom_boxplot() +
  scale_colour_discrete(name="Pre- Las Conchas Species", labels=c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii")) +
  ggtitle("Pre-Las Conchas CBH by Species") + labs(y= "Pre- Las Conchas Live Crown Base Height (m)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#box plot - Post_Las Conchas DBH by Species
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=POST_Species, y=POST_DBH, color=POST_Species)) + geom_boxplot() +
  scale_colour_discrete(name="Post- Las Conchas Species", labels=c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii")) +
  ggtitle("Post-Las Conchas DBH by Species") + labs(y= "Post- Las Conchas Diameter at Breast Height (cm)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#box plot - Post- Las Conchas CBH by Species
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=POST_Species, y=POST_LiCrBHt, color=POST_Species)) + geom_boxplot() +
  scale_colour_discrete(name="Post- Las Conchas Species", labels=c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii")) +
  ggtitle("Post-Las Conchas CBH by Species") + labs(y= "Post- Las Conchas Live Crown Base Height (m)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


#box plot - Mortality by DBH
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=BAND_LasConchas_Mortality, y=PRE_DBH, color=BAND_LasConchas_Mortality,)) + geom_boxplot(col=c("blue", "red")) +
  labs(x = "Las Conchas Tree Mortality (False= Survival, TRUE= Mortality)", y= "Pre-Las Conchas Tree DBH (cm)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5)

#bar graph - Mortality by Species
ggplot(data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=PRE_Species, fill=BAND_LasConchas_Mortality)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red"), 
                    name="",
                    breaks=c(FALSE, TRUE),
                    labels=c("Mortality", "Survival")) +
  labs(x = "Tree Species", y = "Count", cex.main=0.5, cex.lab=0.5, cex.sub=0.5)

#scatter plots - DBH, Tree Height, Live Crown Base Height, Crown Class, and Species
ggplot(data=BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=PRE_DBH, y=PRE_Height, color=PRE_CrownClass)) + geom_point(size=0.9) +
  scale_colour_discrete(name="Pre- Las Conchas Crown Class", breaks=c("D", "C", "I", "SC", "O", "X"), labels=c("Dominant", "Co-dominant", "Intermediate", "Subcanopy", "Open Growth", "Not Assessed")) +
  ggtitle("Pre-Las Conchas DBH and Tree Height") + labs(x = "Post- Las Conchas Diameter at Breast Height (cm)", y= "Tree Height (m)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5)

ggplot(data=BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=POST_DBH, y=POST_Height, color=POST_CrownClass)) + geom_point(size=0.9) +
  scale_colour_discrete(name="Post- Las Conchas Crown Class", breaks=c("D", "C", "I", "SC", "O", "X"), labels=c("Dominant", "Co-dominant", "Intermediate", "Subcanopy", "Open Growth", "Not Assessed")) +
  ggtitle("Post-Las Conchas DBH and Tree Height") + labs(x = "Post- Las Conchas Diameter at Breast Height (cm)", y= "Post- Las Conchas Tree Height (m)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5)

ggplot(data=BAND_Trees_LasConchas_INDV_Mort_NoOutliers, aes(x=PRE_Height, y=PRE_LiCrBHt, color=PRE_Species)) + geom_point(size=0.9) +
  scale_colour_discrete(name="Post- Las Conchas Species", labels=c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii")) +
  ggtitle("Pre-Las Conchas CBH and Tree Height") + labs(x = "Pre- Las Conchas Tree Height (m)", y= "Pre- Las Conchas Live Crown Base Height (m)", cex.main=0.5, cex.lab=0.5, cex.sub=0.5)

#---

#change Mortality to one if the tree died (TRUE), and 0 if it survived (FALSE)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality == TRUE]<- 0 #the tree died
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality[BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality == FALSE]<- 1 #the tree survived
nrow(BAND_Trees_LasConchas_INDV_Mort_NoOutliers)

#---

#Logistic Regression Model
xtabs(~BAND_LasConchas_Mortality + PRE_Species, data = as.matrix(BAND_Trees_LasConchas_INDV_Mort_NoOutliers))
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality <- as.numeric(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$BAND_LasConchas_Mortality)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_CrownRatio <- as.numeric(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_CrownRatio)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_DBH <- as.numeric(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_DBH)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_LiCrBHt <- as.numeric(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_LiCrBHt)
BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species <- as.factor(BAND_Trees_LasConchas_INDV_Mort_NoOutliers$PRE_Species)

BAND_Trees_LasConchas_INDV_GLModel <- glm(BAND_LasConchas_Mortality ~ PRE_Species + PRE_DBH + PRE_LiCrBHt, data = BAND_Trees_LasConchas_INDV_Mort_NoOutliers, family = "binomial")
#draft- if PRE_CrownRatio is added, it comes out as NA, maybe because the info is redundant?

summary(BAND_Trees_LasConchas_INDV_GLModel)
confint(BAND_Trees_LasConchas_INDV_GLModel)
confint.default(BAND_Trees_LasConchas_INDV_GLModel)

##look at the overall effect of Species
wald.test(b = coef(BAND_Trees_LasConchas_INDV_GLModel), Sigma = vcov(BAND_Trees_LasConchas_INDV_GLModel), Terms = 2:6)

##difference between vegetation communities - PINPON vs mixed-conifer species (PICEA9, PINFLE, PSEMEN)
l <- cbind(0, 1, 1, -1, 0, 1, 0, 0)
wald.test(b = coef(BAND_Trees_LasConchas_INDV_GLModel), Sigma = vcov(BAND_Trees_LasConchas_INDV_GLModel), L = l)
#draft- is this correct? Do I assign 1 to all of the conifer variables?


#compare Species, DBH and Predicted Probability of Mortality
#exponentiate the coefficients and interpret them as odds-ratios
exp(coef(BAND_Trees_LasConchas_INDV_GLModel))
exp(cbind(OR = coef(BAND_Trees_LasConchas_INDV_GLModel), confint(BAND_Trees_LasConchas_INDV_GLModel)))

BAND_Trees_LasConchas_INDV_GLMPredict1 <- with(BAND_Trees_LasConchas_INDV_Mort_NoOutliers, data.frame(PRE_DBH = mean(PRE_DBH), 
                                              PRE_LiCrBHt = mean(PRE_LiCrBHt), PRE_Species = factor(PRE_Species, levels = c("ABICON", "PICEA9", "PINFLE", "PINPON", "POPTRE", "PSEMEN"))))

BAND_Trees_LasConchas_INDV_GLMPredict1$rankP <- predict(BAND_Trees_LasConchas_INDV_GLModel, newdata = BAND_Trees_LasConchas_INDV_GLMPredict1, type = "response")
BAND_Trees_LasConchas_INDV_GLMPredict1 <- BAND_Trees_LasConchas_INDV_GLMPredict1[!duplicated(BAND_Trees_LasConchas_INDV_GLMPredict1[c("PRE_DBH", "PRE_LiCrBHt", "PRE_Species", "rankP")]), ]

#line graph- compare Species, DBH and Predicted Probability of Mortality
BAND_Trees_LasConchas_INDV_GLMPredict2 <- with(BAND_Trees_LasConchas_INDV_Mort_NoOutliers, data.frame(PRE_DBH = rep(seq(from = 2.5, to = 99.2, length.out = 100),
                                              6), PRE_LiCrBHt = mean(PRE_LiCrBHt), PRE_Species = factor(rep(c("ABICON", "PICEA9", "PINFLE", "PINPON", "POPTRE", "PSEMEN"), each = 100))))

BAND_Trees_LasConchas_INDV_GLMPredict3 <- cbind(BAND_Trees_LasConchas_INDV_GLMPredict2, predict(BAND_Trees_LasConchas_INDV_GLModel, newdata = BAND_Trees_LasConchas_INDV_GLMPredict2,
             type = "link", se = TRUE))

BAND_Trees_LasConchas_INDV_GLMPredict3 <- within(BAND_Trees_LasConchas_INDV_GLMPredict3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(BAND_Trees_LasConchas_INDV_GLMPredict3)

ggplot(BAND_Trees_LasConchas_INDV_GLMPredict3, aes(x = PRE_DBH, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
  ymax = UL, fill = PRE_Species), alpha = 0.2) + geom_line(aes(color = PRE_Species), size = 1) +
  ggtitle("Pre-Las Conchas DBH and Predicted Probability of Tree Survival") + labs(x= "Diameter at Breast Height (cm)", y="Probability of Survival") + guides(size = FALSE, color=FALSE) +
  scale_fill_discrete(name = "Tree Species - 95% CI", labels = c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii"))



#compare Species, CBH and Predicted Probability of Mortality
#exponentiate the coefficients and interpret them as odds-ratios
BAND_Trees_LasConchas_INDV_GLMPredict4 <- with(BAND_Trees_LasConchas_INDV_Mort_NoOutliers, data.frame(PRE_LiCrBHt = mean(PRE_LiCrBHt), 
        PRE_DBH = mean(PRE_DBH), PRE_Species = factor(PRE_Species, levels = c("ABICON", "PICEA9", "PINFLE", "PINPON", "POPTRE", "PSEMEN"))))

BAND_Trees_LasConchas_INDV_GLMPredict4$rankP <- predict(BAND_Trees_LasConchas_INDV_GLModel, newdata = BAND_Trees_LasConchas_INDV_GLMPredict4, type = "response")

BAND_Trees_LasConchas_INDV_GLMPredict4 <- BAND_Trees_LasConchas_INDV_GLMPredict4[!duplicated(BAND_Trees_LasConchas_INDV_GLMPredict4[c("PRE_DBH", "PRE_LiCrBHt", "PRE_Species", "rankP")]), ]

#line graph- compare Species, CBH and Predicted Probability of Mortality
BAND_Trees_LasConchas_INDV_GLMPredict5 <- with(BAND_Trees_LasConchas_INDV_Mort_NoOutliers, data.frame(PRE_LiCrBHt = rep(seq(from = 0, to = 20, length.out = 100),
           6), PRE_DBH = mean(PRE_DBH), PRE_Species = factor(rep(c("ABICON", "PICEA9", "PINFLE", "PINPON", "POPTRE", "PSEMEN"), each = 100))))

BAND_Trees_LasConchas_INDV_GLMPredict6 <- cbind(BAND_Trees_LasConchas_INDV_GLMPredict5, predict(BAND_Trees_LasConchas_INDV_GLModel, 
           newdata = BAND_Trees_LasConchas_INDV_GLMPredict5, type = "link", se = TRUE))

BAND_Trees_LasConchas_INDV_GLMPredict6 <- within(BAND_Trees_LasConchas_INDV_GLMPredict6, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(BAND_Trees_LasConchas_INDV_GLMPredict6)

ggplot(BAND_Trees_LasConchas_INDV_GLMPredict6, aes(x = PRE_LiCrBHt, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
  ymax = UL, fill = PRE_Species), alpha = 0.2) + geom_line(aes(color = PRE_Species), size = 1) +
  ggtitle("Pre-Las Conchas CBH and Predicted Probability of Tree Survival") + labs(x= "Live Crown Base Height (m)", y="Probability of Survival") + guides(size = FALSE, color=FALSE) +
  scale_fill_discrete(name = "Tree Species - 95% CI", labels = c("Abies concolor", "Picea sp.", "Pinus flexilis", "Pinus ponderosa", "Populus tremuloides", "Pseudotsuga menziesii"))


#draft- how is the probability of mortality increasing with increasing CBH?

#check the model
with(BAND_Trees_LasConchas_INDV_GLModel, null.deviance - deviance)
with(BAND_Trees_LasConchas_INDV_GLModel, df.null - df.residual)
with(BAND_Trees_LasConchas_INDV_GLModel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(BAND_Trees_LasConchas_INDV_GLModel)

#---

#Quadratic iscriminant analysis



