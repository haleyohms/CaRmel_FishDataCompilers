rm(list=ls())

require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
require(ggplot2)


# Main fish data file
AFD<-read_csv("C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv", col_names = T,
              col_types = cols(SiteID = "c", Date = col_date(), Pass = "d", FishNum = "d",
                               FL_mm = "d", Wt_g = "d", PITnum = "c", Recap = col_logical(),
                               TagSize = "i", DNAsamp = col_logical(), Notes = "c", SiteTo = "c",
                               Scales = "l", Species = "c", Sex = "c"))

## DONT DO THIS! BUT NEED TO CHECK FOR TAGS WITH SPACES!!
#AFD$PITnum2 = as.character(sub(" ", "", AFD[,"PITnum"])) # Remove space from PITnum

#AFD <- AFD[!(is.na(AFD$Date)) , ] #remove empty junk from excel

head(AFD)

############################################################################################
############################################################################################
## Compile Adult Data Collected at Los Padres Adult Trap
############################################################################################
############################################################################################
dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/NMFS_Data/Adults_LosPadres/2020 Adult Tagging/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "character", "numeric", "character", "numeric", 
                                  "character","character", "character", "character", "character"),
                   stringsAsFactors=FALSE)
  if(i == 1){
    adf = tbl
  } else{
    adf = rbind(adf, tbl)
  }
}

colnames(adf) <- c("SiteID", "Date", "Species", "FishNum", "Sex", "FL_mm", "PITnum", "TagorNot", 
                  "DNAsamp", "Scales", "Recap","Notes")

adf$DNAsamp <- adf$DNAsamp=="Y" | adf$DNAsamp=="T" ### convert character fields to logical fields
adf$Scales <- adf$Scales=="Y" | adf$Scales=="T"
adf$Recap <- adf$Recap=="Y" | adf$Recap=="T"
adf$Pass<-NA
adf$Wt_g<-NA
adf$TagSize<-23
adf$SiteTo<-NA
adf$TagorNot<-NULL
adf<-adf[!adf$SiteID=="" , ]
unique(adf$Species)
  adf$Species[adf$Species=="O. mykiss"] <- "Om"
adf$FL_mm <- adf$FL_mm*10 #convert length from cm to mm
unique(adf$PITnum)
  adf$PITnum[adf$PITnum=="n/a"] <- NA  
  adf$PITnum[adf$PITnum==""] <- NA
  adf$PITnum = as.character(sub(" ", "", adf[,"PITnum"])) # Remove space from PITnum
  
AFD <- rbind(adf, AFD)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")


#######################################################################################3

############################################################################################
############################################################################################
## Compile Rotary Screw Trap Fish Data Collected at Above Los Padres Reservoir
############################################################################################
############################################################################################

dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/NMFS_Data/Rotary Screw Trap/2020 RST Tagging/RST Data/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "character", "numeric", "numeric", "numeric", 
                                  "character","numeric", "character", "character", "character", "character","character"),
                   stringsAsFactors=FALSE)
  if(i == 1){
    rstdf = tbl
  } else{
    rstdf = rbind(rstdf, tbl)
  }
}

colnames(rstdf) <- c("SiteID", "Date", "Species", "FishNum", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", 
                  "DNAsamp", "Scales", "Recap","Notes")

rstdf<-rstdf[!rstdf$SiteID=="" , ] #remove blank columns; thanks excel... not
rstdf<-rstdf[!rstdf$SiteID==" " , ]

rstdf$DNAsamp <- rstdf$DNAsamp=="Y" | rstdf$DNAsamp=="T"
rstdf$Scales <- rstdf$Scales=="Y" | rstdf$Scales=="T"
rstdf$Recap <- rstdf$Recap=="Y" | rstdf$Recap=="T"
rstdf$TagorNot<-NULL
rstdf$Sex<-NA
rstdf$Pass<-NA
rstdf$SiteTo<-NA
# unique(rstdf$Notes)
#  unique(rstdf$Species)
  rstdf$Species[rstdf$Species=="O. mykiss"] <- "Om"
  # rstdf$Species[rstdf$Species=="O. Mykiss"] <- "Om"
  # rstdf$Species[rstdf$Species=="S. trutta"] <- "St"
  # rstdf$Species[rstdf$Species=="S. Trutta"] <- "St"
  rstdf$TagSize <- as.integer(rstdf$TagSize)
  
rstdf$TagSize[which(!rstdf$PITnum=="" & is.na(rstdf$TagSize))] <- 12
rstdf$PITnum = as.character(sub(" ", "", rstdf[,"PITnum"])) # Remove space from PITnum


#... combine the rst and AFD data    
AFD <- rbind(AFD, rstdf)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")

#write_csv(rstdf,"C:/Users/HaleyOhms/Documents/Carmel/Data Deliveries/2019RST.csv")


############################################################################################
############################################################################################
## Compile 2020 MPWMD Sleepy Rearing Rescues
############################################################################################
############################################################################################

#########################
## CORY"S DATA ##
#########################

#rm(list=ls())
require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
# require(ggplot2)

dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FishRescues/2020 Rescues/Sleepy Reared Rescues/"

files = list.files(dir, '*.txt', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.txt', '',bnames)

for(i in 1:length(files)){
  tbl<-read.table(files[i], header=FALSE, sep="|")
  
  pieces = unlist(strsplit(bnames[i], '_'))
  tbl$site = pieces[1]
  tbl$date = pieces[2]
  
  if(i == 1){
    dfr = tbl
  } else{
    dfr = rbind(dfr, tbl)
  }
}

colnames(dfr)<-c("FishNum","FL_mm", "Wt_g","PITnum","Recap","TagSize","DNAsamp","Notes","Date","Time","SiteID","Date2")
head(dfr)

#Clean up formatting
dfr$FishNum<-substring(dfr$FishNum, 2)
dfr$FishNum<-as.numeric(dfr$FishNum)
dfr$Time<-NULL
dfr$Date2<-as.character(dfr$Date2)
dfr$Date<-as.Date(dfr$Date2, "%y%m%d" )
dfr$Date2<-NULL
unique(dfr$Notes)

dfr$Recap <- dfr$Recap=="Yes"


#Tags to 15 digits
dfr$pre<-substr(dfr$PITnum, start=1, stop=2)
dfr$PITnum<-ifelse(dfr$pre=="R0",substring(dfr$PITnum, 7),substring(dfr$PITnum, 9)) 
dfr$pre<-NULL 

dfr$DNAsamp <- dfr$DNAsamp=="Yes"


#Add columns to match fish data
dfr$Pass<-NA
dfr$Scales<-FALSE
dfr$SiteTo<-"TBD"
#dfr$SiteGRTS<-"TBD"

#Notes = SiteID in this case
dfr$SiteID <- dfr$Notes
dfr$Notes <- NA

dfr$Species <- "Om"
dfr$Sex <- NA


#########################
## UCSC CREW DATA ##
#########################
#dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FishRescues/2020 Rescues/Sleepy Reared Rescues/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)


for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric",
                                  "character","numeric", "character", "character", "character", "character","character"),
                   stringsAsFactors=FALSE)
  tbl <- tbl[!(is.na(tbl$Date)) , ] #remove empty junk from excel
  
  if(i == 1){
    rstdf = tbl
  } else{
    rstdf = rbind(rstdf, tbl)
  }
}

colnames(rstdf) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", 
                     "DNAsamp", "Scales", "Recap", "Species", "Notes")

rstdf$DNAsamp <- rstdf$DNAsamp=="Y" | rstdf$DNAsamp=="T"
rstdf$Scales <- rstdf$Scales=="Y" | rstdf$Scales=="T"
rstdf$Recap <- rstdf$Recap=="Y" | rstdf$Recap=="T"
rstdf$Sex<-NA
rstdf$Pass<-NA
rstdf$SiteTo<-"TBD"
# unique(rstdf$Notes)
#  unique(rstdf$Species)
rstdf$Species[rstdf$Species=="Omy"] <- "Om"
rstdf$Species[rstdf$Species=="OMY"] <- "Om"
rstdf$TagSize <- as.integer(rstdf$TagSize)

rstdf$PITnum = as.character(sub(" ", "", rstdf[,"PITnum"])) # Remove space from PITnum

#... combine Cory's and NOAA data
SHDat <- rbind(rstdf, dfr)
  SHDat$SiteID = as.character(sub("_", "", SHDat[,"SiteID"])) # Standardize SiteIDs
  SHDat$SiteID = as.character(sub("C0", "C", SHDat[,"SiteID"])) 
  SHDat$SiteID = as.character(sub(" ", "", SHDat[,"SiteID"])) 

  SHDat$PITnum[SHDat$PITnum==""] <- NA
  
#... combine the dfr and AFD data    
AFD <- rbind(AFD, SHDat)
  AFD <- distinct(AFD, Date, Species, FishNum, FL_mm, Wt_g, PITnum, Recap, .keep_all=T)

  write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")


# Data export for Cory: 
write_csv(SHDat,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/SHRF_TaggingData_2020.csv")

  
  

   
  
    
  
   
   
  