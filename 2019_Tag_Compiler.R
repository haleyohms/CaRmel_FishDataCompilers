rm(list=ls())

require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
require(ggplot2)


# Main fish data file
AFD<-read_csv("C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv", col_names = T,
              col_types = cols(SiteID = "c", Date = col_date(), Pass = "d", FishNum = "d",
                               FL_mm = "d", Wt_g = "d", PITnum = "c", Recap = col_logical(),
                               TagSize = "i", DNAsamp = col_logical(), Notes = "c", SiteTo = "c",
                               Scales = "l", Species = "c", Sex = "c"))


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



############################################################################################
############################################################################################
## Check for and fix duplicate tags
############################################################################################
############################################################################################
 #... change "" tags to NA
  #AFD$PITnum[AFD$PITnum==""] <- NA
  #AFD$PITnum[AFD$PITnum=="NANA"] <- NA  
  #AFD$PITnum[AFD$PITnum=="NS"] <- NA 


Tdat <- filter(AFD, Recap==F, !is.na(PITnum)) #Non-recaps
dupTags <- Tdat[which(duplicated(Tdat$PITnum)==T) , ]
idx <- duplicated(Tdat$PITnum) | duplicated(Tdat$PITnum, fromLast = TRUE) 
AlldupTags <- Tdat[idx, ] 

#... DATA FRAME OF DUPLICATE TAGS
AlldupTags <- AlldupTags[order(AlldupTags$PITnum, AlldupTags$Date),]
#... dataframe saved as .csv
write_csv(AlldupTags, "C:/Users/HaleyOhms/Documents/Carmel/Database/DuplicateTagData.csv", append = T)

#... change some of the duplicates to recaps (completed: 5/29/29)
AFD$Recap[which(AFD$SiteID=="Garland" & AFD$PITnum=="900226000324768")] <- T
AFD$Recap[which(AFD$SiteID=="CarmelRiver" & AFD$PITnum=="900226000594074")] <- T
AFD$Recap[which(AFD$SiteID=="Redrock" & AFD$PITnum=="900226000594936")] <- T
AFD<-AFD[!(AFD$SiteID=="CarmelRiver" & AFD$Date=="2018-08-10" & AFD$FishNum==68) , ]
AFD$Recap[which(AFD$Date=="2018-08-02" & AFD$PITnum=="900228000631381")] <- T

#... change the duplicate numbers to NA in ADF (completed: 5/29/29)
DupTs <- unique(AlldupTags$PITnum)  #find the duplicate numbers
idx <- AFD$PITnum %in% DupTs  #match the duplicates back to the main dataframe (AFD)
AFD$PITnum[which(idx==T)] <- NA  #change those to NA
write_csv(AFD,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")


