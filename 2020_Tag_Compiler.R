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






  
  
  
  

   
  
    
  
   
   
  