
rm(list=ls())

require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
require(ggplot2)

############################################################################################
############################################################################################
## Compile 2018 Tag Data from Pop Surveys
############################################################################################
############################################################################################

dir = "C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2018/Tagging Data 2018/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
 tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                             colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric", 
                                            "character","character", "character", "character", "character", "character", "character"),
                             stringsAsFactors=FALSE)
  if(i == 1){
    df = tbl
    } else{
    df = rbind(df, tbl)
  }
}

# dfmain<-df
# df<-dfmain

colnames(df) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", "DNAsamp", "Scales", "Recap", "Notes")

# Fields from compiled data:
#flds <- c("SiteType", "SiteID","MPWMD_Name","Stream","Date","Pass","FishNum","FL_mm","Wt_g","PITnum","Recap","TagSize","DNAsamp","Notes","SiteTo")   

df[,"TagorNot"]<-NULL  #Remove Tag(Y/N) column
df<-df[!(is.na(df$SiteID) | df$SiteID==""), ] #Remove data without a site

df$SiteTo<-NA

# df$SiteGRTS<-df$SiteID
# 
# #Convert MPWMD names to GRTS Site IDs
# df$SiteGRTS[which(df$SiteID=="Boronda WD")]<-"81"
# df$SiteGRTS[which(df$SiteID=="CDFW 1")]<-"182"
# df$SiteGRTS[which(df$SiteID=="CDFW 2")]<-"186"
# df$SiteGRTS[which(df$SiteID=="Cachagua")]<-"164"
# df$SiteGRTS[which(df$SiteID=="DEDAMPIERRE WD")]<-"90"
# df$SiteGRTS[which(df$SiteID=="Garland")]<-"73"
# df$SiteGRTS[which(df$SiteID=="LosCompadres")]<-"134"
# df$SiteGRTS[which(df$SiteID=="Red Rock")]<-"41"
# df$SiteGRTS[which(df$SiteID=="Scarlett")]<-"53"
# df$SiteGRTS[which(df$SiteID=="Water District Sleepy Hollow")]<-"114"
# df$SiteGRTS[which(df$SiteID=="StonePine")]<-"104"
# df$SiteGRTS[which(df$SiteID=="Upper Ind Zone")] <- "128"

# Make uniform designations of MPWMD sites
df$SiteID[df$SiteID=="Boronda WD"] <- "Boronda"
df$SiteID[df$SiteID=="CDFW 1"] <- "CDFW1"
df$SiteID[df$SiteID=="CDFW 2"] <- "CDFW2"
df$SiteID[df$SiteID=="Cachagua"] <- "Cachagua"
df$SiteID[df$SiteID=="DEDAMPIERRE WD"] <- "Dedampierre"
df$SiteID[df$SiteID=="Garland"] <- "Garland"
df$SiteID[df$SiteID=="LosCompadres"] <- "Compadres"
df$SiteID[df$SiteID=="Red Rock"] <- "Redrock"
df$SiteID[df$SiteID=="Scarlett"] <- "Scarlett"
df$SiteID[df$SiteID=="StonePine"] <- "Stonepine"
df$SiteID[df$SiteID=="Upper Ind Zone"] <- "SCRupper"
df$SiteID[df$SiteID=="Water District Sleepy Hollow"] <- "SleepyHollow"


# convert tag size field
df$TagSize[ df$TagSize==""]   <- NA
df$TagSize[ df$TagSize=="N"]   <- NA
df$TagSize[ df$TagSize=="NA"]   <- NA
df$TagSize[ df$TagSize=="24"] <- "23"
df$TagSize[ df$TagSize=="t"]  <- "12"
df$TagSize[ df$TagSize=="T"]  <- "23"

#Create binary response for DNA, scales, recap 
df$DNAsamp <- df$DNAsamp=="Y"
df$Scales <- df$Scales=="Y"
df$Recap <- df$Recap=="Y"

#Move morts to 'site to' and deal with other notes
unique(df$Notes)
df$SiteTo[which(df$Notes=="MORT")]<-"Mort"
df$SiteTo[which(df$Notes=="Mort")]<-"Mort"
df$Notes[which(df$Notes=="MISSING TWO PIT NUMBERS AT THE END")]<-"Missing two PIT numbers at end 9002260003252"
df$PITnum[which(df$PITnum=="9002260003252")]<-NA
df$Pass[which(df$Notes=="Pass 1 Fish")]<-1

#############################################
# # check for nonstandard PITnum lengths

# idx <- nchar(df$PITnum) != 15 & nchar(df$PITnum) != 2 & !is.na(df$PITnum)
# which(idx=="TRUE")
# tagques<-df[which(idx=="TRUE"),]
# unique(tagques$SiteID)

# tagques$pre<-substr(tagques$PITnum, start=1, stop=3)
# tagques[which(tagques$pre=="138"),]
# tagques$lgpre<-tagques$pre<-substr(tagques$PITnum, start=1, stop=6)
# unique(tagques$lgpre)
# 
# df$char <- nchar(df$PITnum, "chars") 
# full<-df[which(df$char==15),]
# full$pre<-substr(full$PITnum, start=1, stop=3)
# full$lgpre<-substr(full$PITnum, start=4, stop=9)
# unique(full$pre)
# t1<-full[which(full$lgpre=="138619"),]
# unique(t1$pre)
# summary(t1)
# which(t1$TagSize=="23")
# which(t1$TagSize=="12")
# full[which(full$pre=="000"),]


df$PITnum[which(df$PITnum=="")]<-NA
df$PITnum[which(df$PITnum==NA)]<-NA

df$pre<-substr(df$PITnum, start=1, stop=6)
unique(df$pre)
df$PITnum<-ifelse(df$pre=="228000",paste("900",df$PITnum, sep=""), 
                  ifelse(df$pre=="226000", paste("900",df$PITnum, sep=""),
                         ifelse(df$pre=="126054", paste("982",df$PITnum, sep=""),
                                ifelse(df$pre=="000365", paste("982",df$PITnum, sep=""),
                                       ifelse(df$pre=="000362", paste("982",df$PITnum, sep=""),
                                              ifelse(df$pre=="138619", paste("000000",df$PITnum, sep=""),
                                           df$PITnum))))))

df$pre<-NULL



#save(df, file="C:\\Users\\ohmsh\\Google Drive\\Carmel\\Temp analysis\\MPWMD data\\AllTemps.R")
write.csv(df,"C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2018/Tagging Data 2018/2018FishData.csv")



############################################################################################
############################################################################################
## Compile 2018 Fish Rescue Data
############################################################################################
############################################################################################

# rm(list=ls())
# require(dplyr)
# require(tidyverse)
# require(lubridate)
# require(xlsx)
# require(ggplot2)

dir = "C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FishRescues/2018 Rescue Data/"

files = list.files(dir, '*.txt', recursive = F, full.names = TRUE) 
bnames = basename(files)
bnames = sub('.txt', '',bnames)
bnames
i=1
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


head(dfr)
head(df)

#write.csv(dfr,"C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FishRescues/2018FishRescueData.csv")



################################################################
## Compile 2018 Opportunistic Tag Data
################################################################

dir = "C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2018/Opportunistic Tagging 2018/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
#bnames = basename(files)
#bnames = sub('.xlsx', '',bnames)

for(i in 1:length(files)){
  tbl = read.xlsx2(files[i], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric", 
                                  "character","character", "character", "character", "character", "character", "character"),
                   stringsAsFactors=FALSE)
  
  colnames(tbl) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", "DNAsamp", "Scales", "Recap", "Notes")
  
  if(i == 1){
    dfo = tbl
  } else{
    dfo = rbind(dfo, tbl)
  }
}


dfo[,"TagorNot"]<-NULL  #Remove Tag(Y/N) column
dfo<-dfo[!(is.na(dfo$SiteID) | dfo$SiteID==""), ] #Remove data without a site

dfo$SiteTo<-NA
#dfo$SiteGRTS<-"TBD"

# convert tag size field
dfo$TagSize[ dfo$TagSize==""]   <- NA
dfo$TagSize[ dfo$TagSize=="N"]   <- NA
dfo$TagSize[ dfo$TagSize=="N/A"]   <- NA
dfo$TagSize[ dfo$TagSize=="24"] <- "23"
dfo$TagSize[ dfo$TagSize=="t"]  <- "12"
dfo$TagSize[ dfo$TagSize=="T"]  <- "23"

#Create binary response for DNA, scales, recap 
dfo$DNAsamp <- dfo$DNAsamp=="Y"
dfo$Scales <- dfo$Scales=="Y"
dfo$Recap <- dfo$Recap=="Y"

# # fix nonstandard PITnum lengths
dfo$PITnum[which(dfo$PITnum=="")]<-NA

dfo$pre<-substr(dfo$PITnum, start=1, stop=6)
unique(dfo$pre)
dfo$PITnum<-ifelse(dfo$pre=="228000",paste("900",dfo$PITnum, sep=""), 
                  ifelse(dfo$pre=="226000", paste("900",dfo$PITnum, sep=""),
                         ifelse(dfo$pre=="126053", paste("982",dfo$PITnum, sep=""),
                             dfo$PITnum)))
dfo$pre<-NULL



################################################################
## Compile 2018 CRSA Rescue Data
################################################################
dir = "C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2018/Fish Rescue 2018/"

files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 

  dfnr = read.xlsx2(files[1], sheetIndex = 1, startRow = 1, 
                   colClasses = c("character","Date", "numeric", "numeric", "numeric", "numeric", 
                                  "character","character", "character", "character", "character", "character", "character"),
                   stringsAsFactors=FALSE)
  
  colnames(dfnr) <- c("SiteID", "Date", "FishNum", "Pass", "FL_mm", "Wt_g", "PITnum", "TagSize", "TagorNot", "DNAsamp", "Scales", "Recap", "Notes")

    
  dfnr[,"TagorNot"]<-NULL  #Remove Tag(Y/N) column

  dfnr$SiteTo<-NA
  #dfnr$SiteGRTS<-"TBD"
  dfnr$TagSize[ dfnr$TagSize==""] <- NA

  #Create binary response for DNA, scales, recap 
  dfnr$DNAsamp <- dfnr$DNAsamp=="Y"
  dfnr$Scales <- dfnr$Scales=="Y"
  dfnr$Recap <- dfnr$Recap=="Y"
  
  # # fix nonstandard PITnum lengths
  dfnr$PITnum[dfnr$PITnum==""]<- NA
  
  #Move morts to 'site to' and deal with other notes
  unique(dfnr$Notes)
  dfnr$SiteTo[which(dfnr$Notes=="Mort, no tag or scales")]<-"Mort"
  dfnr$SiteTo[which(dfnr$Notes=="Mort")]<-"Mort"
  
  head(dfnr)


  ################################################################
  ## Compile 2018 YOY survey data
  ################################################################
  dir = "C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2018/YOY 2018/"
  
  files = list.files(dir, '*.xlsx', recursive = F, full.names = TRUE) 
  
  dfyoy = read.xlsx2(files[1], sheetIndex = 1, startRow = 1, 
                    colClasses = c("Date", "character","character", "numeric", 
                                   "character","character", "numeric", "numeric", "character"),
                    stringsAsFactors=FALSE)
  
  colnames(dfyoy) <- c("Date", "SiteID", "pers","FishNum", "DNAsamp", "Scales", "FL_mm", "Wt_g", "Notes")
  
  dfyoy$pers<-NULL
  dfyoy$SiteTo<-NA
  #dfyoy$SiteGRTS<-"TBD"
  dfyoy$TagSize <- NA
  dfyoy$Recap <- NA
  dfyoy$PITnum<-NA
  dfyoy$TagSize<-NA
  dfyoy$Pass<-NA

  #Create binary response for DNA, scales, recap 
  dfyoy$DNAsamp <- dfyoy$DNAsamp=="Y"
  dfyoy$Scales <- dfyoy$Scales=="Y"
  
  # # fix nonstandard PITnum lengths
  dfyoy$PITnum[dfyoy$PITnum==""]<- NA
  
  #Move morts to 'site to' and deal with other notes
  unique(dfyoy$Notes)
  dfyoy$SiteTo[which(dfyoy$Notes=="mort")]<-"Mort"

  head(dfyoy)
  head(df)
  


####################################
#######################################
#########################################


## Combine fall tagging data with spring rescues
dfall<-rbind(df,dfr,dfo,dfnr,dfyoy)
  
  head(dfall)
  
  #dfall$TagSize<-as.factor(dfall$TagSize)
  dfall$Species<-"Om"
  
#... read in pre-2018 data  
  oldDat<-read_csv("C:/Users/HaleyOhms/Documents/Carmel/DATA/Database/pre2018FishData.csv", col_names=T, 
           col_types = cols(SiteID = "c", MPWMD_Name = "c", Stream = "c", Date = col_date(format = ""),
           Pass = col_double(), FishNum = "d", FL_mm = col_double(), Wt_g = col_double(), PITnum = "c",
           Recap = col_logical(), TagSize = "c", DNAsamp = col_logical(), Notes = "c", SiteTo = "c"))
#... clean up old to match new data format
  unique(oldDat$TagSize)
  oldDat$TagSize[oldDat$TagSize=="NA"]   <- NA
  oldDat$TagSize[oldDat$TagSize=="23mm"] <- "23"
  oldDat$TagSize[oldDat$TagSize=="12mm"]  <- "12"
  oldDat$MPWMD_Name[oldDat$SiteID=="DrybackZone"]  <- "DrybackZone"
  oldDat$MPWMD_Name[oldDat$SiteID=="SmoltTrap"]  <- "SmoltTrap"
  oldDat$MPWMD_Name[oldDat$SiteID=="SWFSC-Tank"]  <- "SWFSCtank"
  oldDat$MPWMD_Name[oldDat$SiteID=="NMFS Lab"]  <- "NMFSlab"
  oldDat$SiteID<-oldDat$MPWMD_Name
  oldDat$MPWMD_Name<-NULL
  oldDat$Stream<-NULL
  oldDat$Species<-"Om"
  oldDat$Scales<-NA
  names(dfall)
  names(oldDat)
  
#... bind old and new fish data together
  dfall<-rbind(oldDat, dfall)
  dfall$Sex<-NA

  write_csv(dfall,"C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv")
  
#... check out the csv file  
  # dfall<-read_csv("C:/Users/HaleyOhms/Documents/Carmel/Database/AllFishData.csv", col_names = T,
  #               col_types = cols(SiteID = "c", Date = col_date(), Pass = "d", FishNum = "d",
  #                                FL_mm = "d", Wt_g = "d", PITnum = "c", Recap = col_logical(),
  #                                TagSize = "i", DNAsamp = col_logical(), Notes = "c", SiteTo = "c",
  #                                Scales = "l", Species = "c", Sex = "c"))

