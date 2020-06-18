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


############################################################################################
############################################################################################
## Compile Fall 2019 Pop Surveys
############################################################################################
############################################################################################

#... made a mistake. need to pull added data and re-add it :\
#CHANGE THIS CODE BEFORE USING - WILL DELETE OTHER DATA
# AFD <- AFD %>% filter(!Date > "2019-08-30")
# write_csv(AFD, "C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")
        
dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/NMFS_Data/TaggingHabitatScouting/2019/POP Surveys 2019/PIT Tagging"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric",
                                  "character","numeric", "character", "character", "character", "character", "character"),
                   stringsAsFactors=FALSE)
  if(i == 1){
    fallpop = tbl
  } else{
    fallpop = rbind(fallpop, tbl)
  }
}

colnames(fallpop) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", 
                    "DNAsamp", "Scales", "Recap","Notes")


fallpop<-fallpop[!fallpop$SiteID=="" , ] #remove blank columns; thanks excel... not
fallpop<-fallpop[!fallpop$SiteID==" " , ]
fallpop$DNAsamp <- fallpop$DNAsamp=="Y" | fallpop$DNAsamp=="T"
fallpop$Scales <- fallpop$Scales=="Y" | fallpop$Scales=="T"
fallpop$Recap <- fallpop$Recap=="Y" | fallpop$Recap=="T"
fallpop$Sex<-NA
fallpop$Species<-"Om"
fallpop$SiteTo<-NA
fallpop$TagorNot<-NULL
fallpop$TagSize <- as.integer(fallpop$TagSize)
fallpop$PITnum = as.character(sub(" ", "", fallpop[,"PITnum"])) # Remove space from PITnum

#... error on site 6 date
fallpop$Date[(fallpop$Date=="2018-09-05" & fallpop$SiteID=="6")] <- "2019-09-05"
fallpop$Date[(is.na(fallpop$Date) & fallpop$SiteID=="Boronda")] <- "2019-10-02"

#... Clean up morts
fallpop$Notes[fallpop$Notes=="MORT, NO EYES"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT"] <- "Mort"
fallpop$Notes[fallpop$Notes=="Mort. Side channel"] <- "Mort"
fallpop$Notes[fallpop$Notes=="Mort. Main channel"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT; tag removed"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT, no DNA sample"] <- "Mort"
fallpop$Notes[fallpop$Notes=="Drew blood-- MORT, body discarded, tag retrieved"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT, body discarded, tag retrieved"] <- "Mort"
fallpop$Notes[fallpop$Notes=="No tag, health concerns; MORT"] <- "Mort"
fallpop$Notes[fallpop$Notes=="Mort, no tag or scales"] <- "Mort"
fallpop$Notes[fallpop$Notes=="mort"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT / E-FISHING SCAR"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT, EFISHING SCAR"] <- "Mort"
fallpop$Notes[fallpop$Notes=="MORT, E-FISHING SCAR"] <- "Mort"

#... Clean up tag numbers
fallpop$PITnum[fallpop$PITnum=="NaN"] <- NA
fallpop$PITnum[fallpop$PITnum==""] <- NA

## Deal with duplicates
Tdat <- filter(fallpop, Recap==F, !is.na(PITnum)) #Non-recaps
dupTags <- Tdat[which(duplicated(Tdat$PITnum)==T) , ]
idx <- duplicated(Tdat$PITnum) | duplicated(Tdat$PITnum, fromLast = TRUE) 
AlldupTags <- Tdat[idx, ] 

#write.csv(AlldupTags, "C:/Users/HaleyOhms/Documents/Carmel/Data Deliveries/DupTags.csv")

fallpop$PITnum[fallpop$PITnum=="na"] <- NA

  fallpop$Notes[fallpop$PITnum=="900226001046800"] <- "Tag #900226001046800, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900226001046800"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900226001046839"] <- "Tag #900226001046839, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900226001046839"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633625"] <- "Tag #900228000633625, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633625"] <- NA
  
fallpop$Recap[fallpop$PITnum=="900226001046024" & fallpop$SiteID=="Scarlett" ] <- T
  
fallpop$Recap[fallpop$PITnum=="900226001046040" & fallpop$SiteID=="Scarlett" ] <- T

fallpop$Recap[fallpop$PITnum=="900226001046716" & fallpop$SiteID=="Scarlett" ] <- T

  fallpop$Notes[fallpop$PITnum=="900228000633208"] <- "Tag #900228000633208, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633208"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633600"] <- "Tag #900228000633600, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633600"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633610"] <- "Tag #900228000633610, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633610"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633634"] <- "Tag #900228000633634, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633634"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633636"] <- "Tag #900228000633636, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633636"] <- NA

  fallpop$Notes[fallpop$PITnum=="900228000633629"] <- "Tag #900228000633629, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633629"] <- NA

  fallpop$Notes[fallpop$PITnum=="900228000633605"] <- "Tag #900228000633605, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633605"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633613"] <- "Tag #900228000633613, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633613"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633622"] <- "Tag #900228000633622, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633622"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633638"] <- "Tag #900228000633638, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633638"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633640"] <- "Tag #900228000633640, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633640"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633650"] <- "Tag #900228000633650, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633650"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633652"] <- "Tag #900228000633652, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633652"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633654"] <- "Tag #900228000633654, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633654"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633658"] <- "Tag #900228000633658, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633658"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633660"] <- "Tag #900228000633660, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633660"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633663"] <- "Tag #900228000633663, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633663"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633670"] <- "Tag #900228000633670, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633670"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633678"] <- "Tag #900228000633678, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633678"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633679"] <- "Tag #900228000633679, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633679"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633684"] <- "Tag #900228000633684, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633684"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900228000633685"] <- "Tag #900228000633685, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000633685"] <- NA
  
  fallpop$Recap[fallpop$PITnum=="900228000633696" & fallpop$SiteID=="Stone Cabin" ] <- T
  
  fallpop$Notes[fallpop$PITnum=="900228000689180"] <- "Tag #900228000689180, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000689180"] <- NA
  
  fallpop$Notes[fallpop$PITnum=="900226001046111"] <- "Tag #900228000689180, duplicate, removed"
  fallpop$PITnum[fallpop$PITnum=="900228000689180"] <- NA
  
  AFD$Notes[AFD$PITnum=="900226001046111" & AFD$SiteID=="121"] <- "Mort, Tag #900226001046111, duplicate, removed"
  AFD$PITnum[AFD$PITnum=="900226001046111" & AFD$SiteID=="121"] <- NA
  
  #... Tag size errors 
  fallpop$TagSize[fallpop$SiteID=="UP CDFW 2" & fallpop$FishNum=="43"] <- "12"
  fallpop$TagSize[fallpop$SiteID=="Boronda" & fallpop$FishNum=="150" ] <- "23"
  fallpop$TagSize[fallpop$SiteID=="Finch" & fallpop$FishNum=="43" ] <- "12"
  fallpop$TagSize[fallpop$SiteID=="Hastings" & fallpop$FishNum=="210" ] <- "12"
  fallpop$TagSize[fallpop$SiteID=="Hastings" & fallpop$FishNum=="258" ] <- "12"
  fallpop$TagSize[fallpop$SiteID=="Red Rock" & fallpop$FishNum=="43" ] <- "23"
  fallpop$TagSize[fallpop$SiteID=="Red Rock" & fallpop$FishNum=="69" ] <- "23"
  fallpop$TagSize[fallpop$SiteID=="CDFW 3" & fallpop$FishNum=="9" ] <- "12"
  fallpop$TagSize[fallpop$SiteID=="CDFW 3" & fallpop$FishNum=="10" ] <- "12"

  
#   
#   #... DATA FRAME OF DUPLICATE TAGS
# AlldupTags <- AlldupTags[order(AlldupTags$PITnum, AlldupTags$Date),]
# #... dataframe saved as .csv
# write_csv(AlldupTags, "C:/Users/HaleyOhms/Documents/Carmel/Database/DuplicateTagData.csv", append = T)

#... combine the rst and AFD data    
AFD <- rbind(AFD, fallpop)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

AFD$PITnum[AFD$PITnum=="na"] <- NA

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")


#... change brown trout species
bts <- AFD[which(AFD$Notes=="BROWN TROUT"),]
fallpop$TagSize[fallpop$SiteID=="CDFW 3" & fallpop$FishNum=="10" ] <- "12"
AFD$Species[AFD$Notes=="BROWN TROUT"] <- "St"



#... write out fall pop data for D. Boughton
#write_csv(fallpop,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/FallPop2019.csv")


############################################################################################
############################################################################################
## Compile Cachagua Rescues with CRSA spring 2019
############################################################################################
############################################################################################
dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/NMFS_Data/TaggingHabitatScouting/2019/CRSA Rescues"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric",
                                  "character","numeric", "character", "character", "character", "character","character"),
                   stringsAsFactors=FALSE)
  if(i == 1){
    crsa = tbl
  } else{
    crsa = rbind(crsa, tbl)
  }
}

colnames(crsa) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", 
                     "DNAsamp", "Scales", "Recap","Notes")


crsa<-crsa[!crsa$SiteID=="" , ] #remove blank columns; thanks excel... not
crsa<-crsa[!crsa$SiteID==" " , ]
crsa$DNAsamp <- crsa$DNAsamp=="Y" | crsa$DNAsamp=="T"
crsa$Scales <- crsa$Scales=="Y" | crsa$Scales=="T"
crsa$Recap <- crsa$Recap=="Y" | crsa$Recap=="T"
crsa$Sex<-NA
crsa$Species<-"Om"
crsa$SiteTo<-NA
crsa$TagorNot<-NULL
crsa$TagSize <- as.integer(crsa$TagSize)
crsa$PITnum = as.character(sub(" ", "", crsa[,"PITnum"])) # Remove space from PITnum

#... combine the rst and AFD data    
AFD <- rbind(AFD, crsa)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")

#... write out data for D. Boughton
write_csv(crsa,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/CRSA_Rescues_2019.csv")


############################################################################################
############################################################################################
## Compile 2019 MPWMD Fish Rescue Data
############################################################################################
############################################################################################

rm(list=ls())
require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
# require(ggplot2)

dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FishRescues/2019 Rescue Raw Files/"

files = list.files(dir, '*.txt', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.txt', '',bnames)
bnames

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
# unique(dfr$Recap)
# dfr[which(dfr$Recap==TRUE),]
str(dfall)

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

#Move morts to 'site to' and deal with other notes
unique(dfr$Notes)
dfr$SiteTo[which(dfr$Notes=="MORT")]<-"Mort"

dfr$SiteID[dfr$SiteID=="Cachagua creek"] <- "cachagua creek"

dfr$Species <- "Om"
dfr$Sex <- NA

#... combine the dfr and AFD data    
AFD <- rbind(AFD, dfr)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")

#... write out data for D. Boughton
write_csv(dfr,"C:/Users/HaleyOhms/Documents/Carmel/Data Deliveries/MPWMD_Rescues_2019.csv")




############################################################################################
############################################################################################
## Compile Carmel River Lagoon Sampling Data 7/2019 
############################################################################################
############################################################################################
dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/NMFS_Data/TaggingHabitatScouting/2019/Lagoon"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric",
                                  "character","numeric", "character", "character", "character", "character","character"),
                   stringsAsFactors=FALSE)
  if(i == 1){
    crldf = tbl
  } else{
    crldf = rbind(crldf, tbl)
  }
}

colnames(crldf) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", 
                     "DNAsamp", "Scales", "Recap","Notes")


crldf<-crldf[!crldf$SiteID=="" , ] #remove blank columns; thanks excel... not
crldf<-crldf[!crldf$SiteID==" " , ]
crldf$DNAsamp <- crldf$DNAsamp=="Y" | crldf$DNAsamp=="T"
crldf$Scales <- crldf$Scales=="Y" | crldf$Scales=="T"
crldf$Recap <- crldf$Recap=="Y" | crldf$Recap=="T"
crldf$Sex<-NA
crldf$Species<-"Om"
crldf$SiteTo<-NA
crldf$TagorNot<-NULL
crldf$TagSize <- as.integer(crldf$TagSize)
crldf$PITnum = as.character(sub(" ", "", crldf[,"PITnum"])) # Remove space from PITnum

crldf[crldf$SiteID=="CRL" & crldf$FishNum=="71",]

#... combine the rst and AFD data    
AFD <- rbind(AFD, crldf)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/AllFishData.csv")

# #... write out data for D. Boughton
# write_csv(crldf,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/Lagoon_Tagging_2019.csv")

############################################################################################
############################################################################################
## Compile Adult Data Collected at Los Padres Adult Trap
############################################################################################
############################################################################################
dir = "C:/Users/HaleyOhms/Documents/Carmel/2019AdultData/"

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

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")


#######################################################################################3

############################################################################################
############################################################################################
## Compile Rotary Screw Trap Fish Data Collected at Above Los Padres Reservoir
############################################################################################
############################################################################################

dir = "C:/Users/HaleyOhms/Documents/Carmel/2019RSTData/"

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
#unique(rstdf$Notes)
  rstdf$SiteTo[rstdf$Notes=="Efficiency test - tagged fish released 400m upstream of RST"] <- "400m US RST"
  rstdf$SiteTo[rstdf$Notes=="Efficiency test - tagged fish released 400m upstream"] <- "400m US RST"
  rstdf$SiteTo[rstdf$Notes=="Efficiency test, released 400 m US of RST"] <- "400m US RST"
    rstdf$Notes[rstdf$Notes=="Mort."] <- "MORT"
    rstdf$Notes[rstdf$Notes=="Efficiency test - tagged fish released 400m upstream"] <- 
      "Efficiency test - tagged fish released 400m upstream of RST"  
    rstdf$Notes[rstdf$Notes=="Efficiency test, released 400 m US of RST"] <- 
      "Efficiency test - tagged fish released 400m upstream of RST"  
unique(rstdf$Species)
  rstdf$Species[rstdf$Species=="O. mykiss"] <- "Om"
  rstdf$Species[rstdf$Species=="O. Mykiss"] <- "Om"
  rstdf$Species[rstdf$Species=="S. trutta"] <- "St"
  rstdf$Species[rstdf$Species=="S. Trutta"] <- "St"
  rstdf$TagSize <- as.integer(rstdf$TagSize)
  
rstdf$TagSize[which(!rstdf$PITnum=="" & is.na(rstdf$TagSize))] <- 12

#... combine the rst and AFD data    
AFD <- rbind(AFD, rstdf)
AFD <- distinct(AFD, SiteID, Date, FishNum, FL_mm, Wt_g, PITnum, TagSize, DNAsamp, Recap, .keep_all=T)

write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")

#write_csv(rstdf,"C:/Users/HaleyOhms/Documents/Carmel/Data Deliveries/2019RST.csv")




############################################################################################
############################################################################################
## Check for and fix duplicate tags
############################################################################################
############################################################################################
# ... change "" tags to NA
 AFD$PITnum[AFD$PITnum==""] <- NA
 AFD$PITnum[AFD$PITnum=="NANA"] <- NA
 AFD$PITnum[AFD$PITnum=="NS"] <- NA
 AFD$PITnum[AFD$PITnum=="NaN"] <- NA
 AFD$PITnum[AFD$PITnum=="na"] <- NA
 

Tdat <- filter(AFD, Recap==F, !is.na(PITnum)) #Non-recaps
dupTags <- Tdat[which(duplicated(Tdat$PITnum)==T) , ]
idx <- duplicated(Tdat$PITnum) | duplicated(Tdat$PITnum, fromLast = TRUE) 
AlldupTags <- Tdat[idx, ] 

#... DATA FRAME OF DUPLICATE TAGS
AlldupTags <- AlldupTags[order(AlldupTags$PITnum, AlldupTags$Date),]
#... dataframe saved as .csv
write_csv(AlldupTags, "C:/Users/HaleyOhms/Documents/Carmel/Database/DuplicateTagData.csv", append = T)


#... change some of the duplicates to recaps (completed: 1/22/2020)



#... change some of the duplicates to recaps (completed: 5/29/19)
AFD$Recap[which(AFD$SiteID=="Garland" & AFD$PITnum=="900226000324768")] <- T
AFD$Recap[which(AFD$SiteID=="CarmelRiver" & AFD$PITnum=="900226000594074")] <- T
AFD$Recap[which(AFD$SiteID=="Redrock" & AFD$PITnum=="900226000594936")] <- T
AFD<-AFD[!(AFD$SiteID=="CarmelRiver" & AFD$Date=="2018-08-10" & AFD$FishNum==68) , ]
AFD$Recap[which(AFD$Date=="2018-08-02" & AFD$PITnum=="900228000631381")] <- T

#... change the duplicate numbers to NA in ADF (completed: 5/29/19)
DupTs <- unique(AlldupTags$PITnum)  #find the duplicate numbers
idx <- AFD$PITnum %in% DupTs  #match the duplicates back to the main dataframe (AFD)
AFD$PITnum[which(idx==T)] <- NA  #change those to NA
write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")

############################################################################################
############################################################################################
## Fix site names
############################################################################################
############################################################################################
unique(AFD$SiteID)
AFD$SiteID[AFD$SiteID=="CDFW Wild Trout 1"] <- "CDFW1"
AFD$SiteID[AFD$SiteID=="CDFW Wild Trout 2"] <- "CDFW2"
AFD$SiteID[AFD$SiteID=="SmoltTrap"] <- "2014RescueSmoltTrap"
AFD$SiteID[AFD$SiteID=="Sleepy Hollow"] <- "Sleepyhollow"
write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")

#... remove "n/a" from adult data sex
AFD$Sex[AFD$Sex=="n/a"] <- NA
write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")




  
  
  
  

   
  
    
  
   
   
  