############################################################################################
############################################################################################
## Compile 2021 Pop Survey Data
############################################################################################
############################################################################################


rm(list=ls())
require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)


#########################
## Hand-held reader data ##
#########################

### First three sites, each is unique
#... Scarlett: used hand-held for PIT numbers, excel for fish too small to tag. Excel fish numbers are wrong (I deleted them)
#.... but the text fish numbers are correct

#... Boronda: used the computer for all fish until the computer died, then switched to hand-held for all (even non-tagged fish)
#.... recorded non-tagged fish with a duplicate tag (which needs to be deleted from the data)
#.... Excel fish numbers are correct, text fish numbers are wrong

#... Garland: used hand-held reader only; fish numbers in text file are correct

#... Need to save all a single file for each site for CH



###############
## Scarlett
###############

file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Scarlett_101521.txt"
  tblScar<-read.table(file, header=FALSE, sep="|")

  colnames(tblScar)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")

  tblScar$SiteName = "Scarlett"
 

head(tblScar)

#Clean up formatting
tblScar$FishNumber<-substring(tblScar$FishNumber, 2)
  tblScar$FishNumber<-as.numeric(tblScar$FishNumber)
  tblScar$Time<-NULL
  tblScar$Date<-dmy(tblScar$Date)
  tblScar$Recapture <- tblScar$Recapture=="Yes"
  tblScar$DNAsample <- tblScar$DNAsample=="Yes"

#Tags to 15 digits
  tblScar$pre<-substr(tblScar$PITnumber, start=1, stop=2)
  tblScar$PITnumber<-ifelse(tblScar$pre=="R0",substring(tblScar$PITnumber, 7),substring(tblScar$PITnumber, 9)) 
  tblScar$pre<-NULL 

#Add columns to match fish data
  tblScar$Pass<-NA
  tblScar$Species <- "O. mykiss"
  tblScar$ScaleSample <- NA


### Read in the excel file:
  file2 = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Scarlett_CarmelTagging_2021.xlsx"
  tbl2Scar = read.xlsx2(file2, sheetIndex = 1, startRow = 1, 
             colClasses = c("character","Date", "numeric", "numeric", "numeric", 
                            "numeric","character","numeric", "character",
                             "character", "character", "character","character"),
             stringsAsFactors=FALSE)
  tbl2Scar <- tbl2Scar[!(is.na(tbl2Scar$Date)) , ] #remove empty junk from excel
  
  tbl2Scar$PITnumber<-NA
  
  tbl2Scar$Recapture <- tbl2Scar$Recapture=="Yes"
  tbl2Scar$DNAsample <- tbl2Scar$DNAsample=="Yes"

  #Add columns to match fish data
  tbl2Scar$Pass<-NA
  tbl2Scar$Species <- "O. mykiss"
    sort(names(tblScar))

#... Join them up:
  ScarDat <- rbind(tblScar, tbl2Scar)

#... Write out for CH:
  write.xlsx2(ScarDat, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Scarlett.xlsx")
  

  ###############
  ## Boronda
  ###############
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Boronda_101821.txt"
  tblBor<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblBor)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblBor$SiteName = "Boronda"
  
  
  head(tblBor)
  
  #Clean up formatting
  tblBor$FishNumber<-substring(tblBor$FishNumber, 2)
  tblBor$FishNumber<-as.numeric(tblBor$FishNumber)
    tblBor$FishNumber <- NA ## Fish numbers incorrect
  tblBor$Time<-NULL
  tblBor$Date<-dmy(tblBor$Date)
  tblBor$Recapture <- tblBor$Recapture=="Yes"
  tblBor$DNAsample <- tblBor$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblBor$pre<-substr(tblBor$PITnumber, start=1, stop=2)
  tblBor$PITnumber<-ifelse(tblBor$pre=="R0",substring(tblBor$PITnumber, 7),substring(tblBor$PITnumber, 9)) 
  tblBor$pre<-NULL 
  
  #Add columns to match fish data
  tblBor$Pass<-NA
  tblBor$Species <- "O. mykiss"
  tblBor$ScaleSample <- F
  
  
  ### Read in the excel file:
  file2 = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Boronda_CarmelTagging_2021.xlsx"
  tbl2Bor = read.xlsx2(file2, sheetIndex = 1, startRow = 1, 
                        colClasses = c("character","Date", "numeric", "numeric", "numeric", 
                                       "numeric","character","numeric", "character",
                                       "character", "character", "character","character"),
                        stringsAsFactors=FALSE)
  tbl2Bor <- tbl2Bor[!(is.na(tbl2Bor$Date)) , ] #remove empty junk from excel
  
  tbl2Bor$Recapture <- tbl2Bor$Recapture=="Yes"
  tbl2Bor$DNAsample <- tbl2Bor$DNAsample=="Yes"
  
  tbl2Bor$PITnumber = as.character(sub(" ", "", tbl2Bor[,"PITnumber"])) # Remove space from PITnum
  
  #Add columns to match fish data
  tbl2Bor$Species <- "O. mykiss"
  sort(names(tbl2Bor))
  
  #... Join them up:
  BorDat <- rbind(tblBor, tbl2Bor)
  
  BorDat <- BorDat %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                              TagSize = ifelse(is.na(PITnumber), NA, TagSize))
    
  
  #... Write out for CH:
  write.xlsx2(BorDat, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Boronda.xlsx")
  
  
  ###############
  ## Garland
  ###############
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Garland_101921.txt"
  tblGar<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblGar)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblGar$SiteName = "Garland"
  
  #Clean up formatting
  tblGar$FishNumber<-substring(tblGar$FishNumber, 2)
  tblGar$FishNumber<-as.numeric(tblGar$FishNumber)
    tblGar$Time<-NULL
  tblGar$Date<-dmy(tblGar$Date)
  tblGar$Recapture <- tblGar$Recapture=="Yes"
  tblGar$DNAsample <- tblGar$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblGar$pre<-substr(tblGar$PITnumber, start=1, stop=2)
  tblGar$PITnumber<-ifelse(tblGar$pre=="R0",substring(tblGar$PITnumber, 7),substring(tblGar$PITnumber, 9)) 
  tblGar$pre<-NULL 
  
  #Add columns to match fish data
  tblGar$Pass<-NA
  tblGar$Species <- "O. mykiss"
  tblGar$ScaleSample <- F
  
  tblGar <- tblGar %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                              TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
   #... Write out for CH:
  write.xlsx2(tblGar, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Garland.xlsx")
  
  
  #################################
  ## Sleepy Hollow ###
  #################################
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/SleepyHollow_10_22_21.txt"
  tblSH<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblSH)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblSH$SiteName = "SleepyHollow"
  
   
  head(tblSH)
  
  #Clean up formatting
  tblSH$FishNumber<-substring(tblSH$FishNumber, 2)
  tblSH$FishNumber<-as.numeric(tblSH$FishNumber)
  tblSH$Time<-NULL
  tblSH$Date<-dmy(tblSH$Date)
  tblSH$Recapture <- tblSH$Recapture=="Yes"
  tblSH$DNAsample <- tblSH$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblSH$pre<-substr(tblSH$PITnumber, start=1, stop=2)
  tblSH$PITnumber<-ifelse(tblSH$pre=="R0",substring(tblSH$PITnumber, 7),substring(tblSH$PITnumber, 9)) 
  tblSH$pre<-NULL 
  
  tblSH <- tblSH %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                            TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
  
  #Add columns to match fish data
  tblSH$Pass<-NA
  tblSH$Species <- "O. mykiss"
  tblSH$ScaleSample <- NA
  
  write.xlsx2(tblSH, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/SleepyHollow.xlsx")
  

  #################################
  ## Stonepine ###
  #################################
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/Stonepine_10_21_21.txt"
  tblSP<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblSP)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblSP$SiteName = "Stonepine"
  
  head(tblSP)
  
  #Clean up formatting
  tblSP$FishNumber<-substring(tblSP$FishNumber, 2)
  tblSP$FishNumber<-as.numeric(tblSP$FishNumber)
  tblSP$Time<-NULL
  tblSP$Date<-dmy(tblSP$Date)
  tblSP$Recapture <- tblSP$Recapture=="Yes"
  tblSP$DNAsample <- tblSP$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblSP$pre<-substr(tblSP$PITnumber, start=1, stop=2)
  tblSP$PITnumber<-ifelse(tblSP$pre=="R0",substring(tblSP$PITnumber, 7),substring(tblSP$PITnumber, 9)) 
  tblSP$pre<-NULL 
  
  tblSP <- tblSP %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                            TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
  #Add columns to match fiSP data
  tblSP$Pass<-NA
  tblSP$Species <- "O. mykiss"
  tblSP$ScaleSample <- NA
  
  write.xlsx2(tblSP, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Stonepine.xlsx")
  
  
  
  #################################
  ## UIZ ###
  #################################
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/UIZ_11_03_21.txt"
  tblUIZ<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblUIZ)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblUIZ$SiteName = "UIZ"
  
  head(tblUIZ)
  
  #Clean up formatting
  tblUIZ$FishNumber<-substring(tblUIZ$FishNumber, 2)
  tblUIZ$FishNumber<-as.numeric(tblUIZ$FishNumber)
  tblUIZ$Time<-NULL
  tblUIZ$Date<-dmy(tblUIZ$Date)
  tblUIZ$Recapture <- tblUIZ$Recapture=="Yes"
  tblUIZ$DNAsample <- tblUIZ$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblUIZ$pre<-substr(tblUIZ$PITnumber, start=1, stop=2)
  tblUIZ$PITnumber<-ifelse(tblUIZ$pre=="R0",substring(tblUIZ$PITnumber, 7),substring(tblUIZ$PITnumber, 9)) 
  tblUIZ$pre<-NULL 
  
  tblUIZ <- tblUIZ %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                            TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
  #Add columns to match fiUIZ data
  tblUIZ$Pass<-NA
  tblUIZ$Species <- "O. mykiss"
  tblUIZ$ScaleSample <- NA
  
  write.xlsx2(tblUIZ, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/UIZ.xlsx")
  
  
  #################################
  ## Cachagua ###
  #################################
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/cachagua_10_27_21.txt"
  tblCach<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblCach)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblCach$SiteName = "Cachagua"
  
  head(tblCach)
  
  #Clean up formatting
  tblCach$FishNumber<-substring(tblCach$FishNumber, 2)
  tblCach$FishNumber<-as.numeric(tblCach$FishNumber)
  tblCach$Time<-NULL
  tblCach$Date<-dmy(tblCach$Date)
  tblCach$Recapture <- tblCach$Recapture=="Yes"
  tblCach$DNAsample <- tblCach$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblCach$pre<-substr(tblCach$PITnumber, start=1, stop=2)
  tblCach$PITnumber<-ifelse(tblCach$pre=="R0",substring(tblCach$PITnumber, 7),substring(tblCach$PITnumber, 9)) 
  tblCach$pre<-NULL 
  
  tblCach <- tblCach %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                              TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
  #Add columns to match fiCach data
  tblCach$Pass<-NA
  tblCach$Species <- "O. mykiss"
  tblCach$ScaleSample <- NA
  
  write.xlsx2(tblCach, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Cachagua.xlsx")
  
  
  #################################
  ## Los Compadres ###
  #################################
  
  file = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Raw Data/loscompadres_10_29_21.txt"
  tblLC<-read.table(file, header=FALSE, sep="|")
  
  colnames(tblLC)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","Recapture","TagSize","DNAsample","FieldNotes","Date","Time")
  
  tblLC$SiteName = "Los Compadres"
  
  head(tblLC)
  
  #Clean up formatting
  tblLC$FishNumber<-substring(tblLC$FishNumber, 2)
  tblLC$FishNumber<-as.numeric(tblLC$FishNumber)
  tblLC$Time<-NULL
  tblLC$Date<-dmy(tblLC$Date)
  tblLC$Recapture <- tblLC$Recapture=="Yes"
  tblLC$DNAsample <- tblLC$DNAsample=="Yes"
  
  #Tags to 15 digits
  tblLC$pre<-substr(tblLC$PITnumber, start=1, stop=2)
  tblLC$PITnumber<-ifelse(tblLC$pre=="R0",substring(tblLC$PITnumber, 7),substring(tblLC$PITnumber, 9)) 
  tblLC$pre<-NULL 
  
  tblLC <- tblLC %>% mutate(PITnumber = ifelse(PITnumber=="982126054206704", NA, PITnumber), 
                                TagSize = ifelse(is.na(PITnumber), NA, TagSize))
  
  #Add columns to match fiLC data
  tblLC$Pass<-NA
  tblLC$Species <- "O. mykiss"
  tblLC$ScaleSample <- NA
  
  write.xlsx2(tblLC, file="C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/LosCompadres.xlsx")
  
  
  
################################################################
#... Add the rest of the database data: 
################################################################
  ## Database
  load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
  
  PSdat <- rbind(ScarDat, BorDat, tblGar, tblSH, tblSP, tblUIZ, tblCach, tblLC)
  
  #t <- FishData %>% group_by(SiteID, SiteName) %>% summarise(n())
  
  PSdat <- PSdat %>% mutate(SiteID = ifelse(SiteName=="Scarlett", 52, 
                              ifelse(SiteName=="Boronda", 80, 
                               ifelse(SiteName=="Garland", 72, 
                                ifelse(SiteName=="SleepyHollow", 114, 
                                 ifelse(SiteName=="Stonepine", 103, 
                                   ifelse(SiteName=="UIZ", 128, 
                                      ifelse(SiteName=="Cachagua", 164, 
                                         ifelse(SiteName=="Los Compadres", 134, NA)))))))))
  
    PSdat$OccCode = "DEP"
      PSdat$CaptureYear <- 2021
      PSdat$DNAID <- NA
      
      earlySites <- c(52, 80, 72)
      PSdat <- PSdat %>% mutate(DataEntryNotes = ifelse(SiteID %in% earlySites, "Used both computer and hand-held to record data, combined excel and text files for this final data entry, H. Ohms Nov 2021", 
                                                        "Compiled from reader .txt files by H. Ohms, Nov 2021"))
    
      PSdat$FishGroup <- NA
      max(FishData$FishID)
      
      PSdat[,"FishID"] <- seq(67018, 68797, by=1) 
      PSdat$Pass <- NULL
      PSdat$Sex <- NA
      PSdat$SiteTo <- NA
      PSdat$Year <- 2021
      PSdat$LifeForm <- NA
      PSdat <- PSdat %>% mutate(OccID = paste("DEP", Date, SiteID, sep = "-"))
    
      
      sort(names(PSdat))
      sort(names(FishData))
      
      FishData <- rbind(FishData, PSdat)
      
      # rm(BorDat, FishData2, PSdat, ScarDat, t, tbl2Bor, tbl2Scar, tblBor, tblCach, 
      #    tblGar, tblLC, tblScar, tblSH, tblSP, tblUIZ)

      save(Captive, Dam, Depletion, FishData, Flow,
           Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID, 
           Redd, Station, Temperature, Translocation, XS, file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss_20211119.RData")


      load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")


####################################################################    
