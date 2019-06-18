# Compiles the various tagging spreadsheets into one flat file
# Files are for all tagging prior to Fall 2017 season
# D. Boughton, 18 Oct 2017 original
# H. Ohms added 2 March 2018


rm(list=ls())

library(xlsx)
library(stringr)
library(foreign)

###### Read in data and standardize the field names

flds <- c("SiteType", "SiteID","MPWMD_Name","Stream","Date","Pass","FishNum","FL_mm","Wt_g","PITnum","Recap","TagSize","DNAsamp","Notes","SiteTo")   

################################################################
# NMFS fall efishing, tagging 2017

fishdata_2017<- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2017/FishData_2017.xlsx", sheetIndex = 1, startRow = 1, 
                           colClasses = c("character","character","Date", "numeric", "numeric", "numeric", "numeric", 
                                          "character","character", "integer", "integer", "character", "character", "character"),
                           stringsAsFactors=FALSE)

str(fishdata_2017)

names(fishdata_2017)
fishdata_2017<-fishdata_2017[,-(15:26)]
fishdata_2017$SiteTo<-NA
names(fishdata_2017)[[which(names(fishdata_2017)=="SiteFrom")]] <- "SiteID"
fishdata_2017$MPWMD_Name<-fishdata_2017$SiteID
fishdata_2017$PITnum <- as.character(paste(fishdata_2017$PIT_prefix, fishdata_2017$PIT_last6, sep=""))
fishdata_2017<-fishdata_2017[ , -which(names(fishdata_2017) %in% c("PIT_prefix", "PIT_last6"))]
fishdata_2017<-fishdata_2017[ , -which(names(fishdata_2017) %in% c("Mort"))]
fishdata_2017$Stream <- NA

names(fishdata_2017)

fishdata_2017 <- fishdata_2017[,flds]
names(fishdata_2017)
head(fishdata_2017)

################################################################
# NMFS GRTS sample 2016. 
nmfs2016 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2016/CarmelFall2016_WatershedTagging_11012016.xlsx", sheetIndex=1, startRow=4,
                       colClasses=c("character", "Date", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character"),
                       stringsAsFactors=FALSE)

names(nmfs2016)
names(nmfs2016)[[which(names(nmfs2016)=="Site.ID"    )]] <- "SiteID"
nmfs2016$SiteTo <- NA
names(nmfs2016)[[which(names(nmfs2016)=="FL..mm."    )]] <- "FL_mm"
names(nmfs2016)[[which(names(nmfs2016)=="Wt..g."     )]] <- "Wt_g"
names(nmfs2016)[[which(names(nmfs2016)=="Tag.Size..12.23."   )]] <- "TagSize"
names(nmfs2016)[[which(names(nmfs2016)=="Recap..Y.N."   )]] <- "Recap"
names(nmfs2016)[[which(names(nmfs2016)=="DNA"       )]] <- "DNAsamp"
nmfs2016$SiteType<-"NOAA_FallPop"
nmfs2016$MPWMD_Name<-nmfs2016$SiteID
nmfs2016$Stream<-NA
nmfs2016 <- nmfs2016[,flds]


#######################################################
# Sleepy Hollow Fish 2016. 
# According to scanned datasheets, these fish were tagged on Aug 24 and 25, 2016, but the date was not on the spreadsheet and is added here
shsrf2016 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2016/SHRF_TAGGING_2016/2016length_wtdata_aug_created9.12.xlsx",
                        sheetIndex=1, startRow=1,
                        colClasses=c("Date","character", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "character"),
                        stringsAsFactors=FALSE)

shsrf2016$SiteType <- "Captive Rear"
shsrf2016$Fish.from <- paste0("SHSRF-", shsrf2016$Fish.from)
names(shsrf2016)[[which(names(shsrf2016)=="Fish.from"  )]] <- "SiteID"
shsrf2016$MPWMD_Name <- shsrf2016$SiteID
shsrf2016$Stream <- NA
shsrf2016$Pass <- NA
names(shsrf2016)[[which(names(shsrf2016)=="Fish.."  )]] <- "FishNum"
names(shsrf2016)[[which(names(shsrf2016)=="FL..mm." )]] <- "FL_mm"
names(shsrf2016)[[which(names(shsrf2016)=="Wt..g."  )]] <- "Wt_g"
names(shsrf2016)[[which(names(shsrf2016)=="PIT.."   )]] <- "PITnum"
names(shsrf2016)[[which(names(shsrf2016)=="Recap..Y.N." )]] <- "Recap"
names(shsrf2016)[[which(names(shsrf2016)=="Tagsize..12.23.")]] <- "TagSize"
names(shsrf2016)[[which(names(shsrf2016)=="Fin.clip..A.C.")]] <- "DNAsamp"
shsrf2016$Notes <- str_trim(paste("Tagged when moved from", shsrf2016$Fish.from, "to", paste0(shsrf2016$Fish.moved.to, "."), shsrf2016$Notes))
shsrf2016$SiteTo <- paste0("SHSRF-", shsrf2016$Fish.moved.to)

shsrf2016<-shsrf2016[,flds]


###################################################
# 2014 Tagging at Santa Cruz Lab
# These are in addition to the ones from the Fall Pop survey spreadsheet. 
# This group was moved to Santa Cruz due to drought conditions at SHSRF
scl2014 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2014/Captured_Omy_CarmelRiver_2014.xlsx", sheetName="NMFS_tagging", startRow=3,
                        colClasses=c("character", "numeric", "numeric", "numeric", "numeric", "character", "character", "Date", "character", "character"),
                        stringsAsFactors=FALSE)

scl2014$SiteType <-"Captive Rear"
scl2014$SiteID <- "NMFS Lab"
scl2014$MPWMD_Name <- NA
scl2014$Stream <- NA
scl2014$Pass <- NA
names(scl2014)[[which(names(scl2014)=="Fish.."  )]] <- "FishNum"
names(scl2014)[[which(names(scl2014)=="FL..mm." )]] <- "FL_mm"
names(scl2014)[[which(names(scl2014)=="Wt...g." )]] <- "Wt_g"
names(scl2014)[[which(names(scl2014)=="Tag.Number" )]] <- "PITnum"
scl2014$Recap <- "N"
names(scl2014)[[which(names(scl2014)=="Tag.size")]] <- "TagSize"
names(scl2014)[[which(names(scl2014)=="Fin.clip")]] <- "DNAsamp"
scl2014$SiteTo <- scl2014$SiteFrom <- paste0("SWFSC-Tank-", scl2014$"Tank..")

# correct the tag numbers with proper prefixes
idx <- substr(scl2014$PITnum, 1, 4)=="1786" | substr(scl2014$PITnum, 1, 4)=="1792"
scl2014$PITnum[idx] <- paste0("000000", scl2014$PITnum[idx])
idx <- substr(scl2014$PITnum, 1, 4)=="2260" 
scl2014$PITnum[idx] <- paste0("900", scl2014$PITnum[idx])

scl2014 <- scl2014[,flds]


################################################################
# Releases Spring 2014. 
# These are previously tagged fish that were released to various spots in Carmel River in Feb 2014
release2014 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2014/Captured_Omy_CarmelRiver_2014.xlsx", sheetName="NMFS_Release", startRow=3,
                          colClasses=c("character", "numeric", "Date", "character"),
                          stringsAsFactors=FALSE)

release2014$SiteType <- "Selected"
release2014$SiteID <- "SWFSC-Tank"
release2014$MPWMD_Name <- NA
release2014$Stream <- NA
names(release2014)[[which(names(release2014)=="Date_Released"  )]] <- "Date"
release2014$Pass <- NA
release2014$FishNum <- NA
release2014$FL_mm <- NA
release2014$Wt_g  <- NA
names(release2014)[[which(names(release2014)=="PIT.Tag.Number" )]] <- "PITnum"
release2014$Recap <- "Y"
release2014$TagSize <- NA
release2014$DNAsamp <- "N"
names(release2014)[[which(names(release2014)=="Reader" )]] <- "Notes"
release2014$SiteTo   <- paste0("Release-Point-", release2014$Release_Site)

# correct the tag numbers with proper prefixes
idx <- substr(release2014$PITnum, 1, 4)=="1786" | substr(release2014$PITnum, 1, 4)=="1792"
release2014$PITnum[idx] <- paste0("000000", release2014$PITnum[idx])

idx <- substr(release2014$PITnum, 1, 6)=="226000" 
release2014$PITnum[idx] <- paste0("900", release2014$PITnum[idx])

release2014<-release2014[,flds]

################################################################
# Rescues Spring 2014. 
# These are previously tagged fish that were rescued in the dry section of riverbed,
# or trapped in the smolt trap, in Spring of 2014
rescue2014 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2014/Captured_Omy_CarmelRiver_2014.xlsx", sheetName="MPWMD_2014Catch", startRow=3,
                         colClasses=c("character", "character", "Date", "numeric", "numeric", "character", "character", "character", "character"),
                         stringsAsFactors=FALSE)

rescue2014$SiteType <- "Selected"
rescue2014$SiteID <- "DrybackZone"
rescue2014$SiteID[rescue2014$From=="T"] <- "SmoltTrap"    # will want to replace this with subframe ID

rescue2014$MPWMD_Name <- NA
rescue2014$Stream <- NA
names(rescue2014)[[which(names(rescue2014)=="Catch_DATE"  )]] <- "Date"
rescue2014$Pass <- NA
rescue2014$FishNum <- NA
rescue2014$FL_mm <- NA
rescue2014$Wt_g <- NA
names(rescue2014)[[which(names(rescue2014)=="Tag.Number" )]] <- "PITnum"
rescue2014$Recap <- "Y"
rescue2014$TagSize <- NA
rescue2014$DNAsamp <- "N"

names(rescue2014)[[which(names(rescue2014)=="Catch_Notes" )]] <- "Notes"
names(rescue2014)[[which(names(rescue2014)=="From...Rescued...R.....Trap..T....Or.Mort.from.dryback..Mort." )]] <- "From"
rescue2014$Notes <- paste(rescue2014$Notes, "Rescued by MPWMD due to dryback after flood pulse")

rescue2014$SiteTo   <- "Marine"
rescue2014$SiteTo[rescue2014$"Released..FW...Marine.."=="FW"] <- "Freshwater"    # will want to replace this with subframe ID
rescue2014$SiteTo[rescue2014$From=="Mort"]                    <- "Mortality"    

# correct the tag numbers with proper prefixes
idx <- substr(rescue2014$PITnum, 1, 4)=="1786" | substr(rescue2014$PITnum, 1, 4)=="1792"
rescue2014$PITnum[idx] <- paste0("000000", rescue2014$PITnum[idx])

idx <- substr(rescue2014$PITnum, 1, 6)=="226000" 
rescue2014$PITnum[idx] <- paste0("900", rescue2014$PITnum[idx])

rescue2014<-rescue2014[,flds]

################################################################
#Sleep Hollow rearing facility, PIT tag training
shrf2013<- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/NMFS_Data/TaggingHabitatScouting/2013/SHRF_Tagging_2013.xlsx",
                      sheetIndex=1, startRow=1,
                      colClasses=c("Date", "character", "numeric", "numeric", "numeric", "character", "character", "character", "character"),
                      stringsAsFactors=FALSE)

shrf2013$SiteType<-"Captive Rear"
names(shrf2013)[[which(names(shrf2013)=="Site")]] <- "MPWMD_Name"
shrf2013$SiteID<-shrf2013$MPWMD_Name
shrf2013$Stream <- as.character(NA)
shrf2013$Pass <- NA
names(shrf2013)[[which(names(shrf2013)=="Fish..")]] <- "FishNum"
names(shrf2013)[[which(names(shrf2013)=="FL..mm.")]] <- "FL_mm"
names(shrf2013)[[which(names(shrf2013)=="Wt...g.")]] <- "Wt_g"
names(shrf2013)[[which(names(shrf2013)=="PIT.." )]] <- "PITnum"
shrf2013$Recap <- as.character(NA)
shrf2013$TagSize <- as.character(NA)
shrf2013$DNAsamp <- as.character(NA)
shrf2013$SiteTo <- as.character(NA)
shrf2013$Notes <- paste(shrf2013$Notes, "PIT-tag training", sep = " ")

shrf2013<-shrf2013[,flds]

################################################################
# MPWMD Fall Pop Survey 2017
####Included in NMFS 2017 data
##################################


################################################################
# MPWMD Fall Pop Survey 2016.
mpwmdsurv2016 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FallPopSurvey/2016/MPWMD_FallPop2016_10262017.xlsx", sheetIndex=1, 
                            colClasses=c("Date", "character", "character", "numeric", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "character"),
                            stringsAsFactors=FALSE)

mpwmdsurv2016$SiteType <- "MPWMD_FallPop"
names(mpwmdsurv2016)[[which(names(mpwmdsurv2016)=="SiteID")]] <- "MPWMD_Name"
mpwmdsurv2016$SiteID <- NA
mpwmdsurv2016$Stream <- NA
names(mpwmdsurv2016)[[which(names(mpwmdsurv2016)=="FishNumber")]] <- "FishNum"
names(mpwmdsurv2016)[[which(names(mpwmdsurv2016)=="FL"        )]] <- "FL_mm"
names(mpwmdsurv2016)[[which(names(mpwmdsurv2016)=="Wt"        )]] <- "Wt_g"
mpwmdsurv2016$PITnum<- paste(mpwmdsurv2016$PITprefix, mpwmdsurv2016$PITsuffix,sep="")
mpwmdsurv2016$Recap<-NA
names(mpwmdsurv2016)[[which(names(mpwmdsurv2016)=="DNA"       )]] <- "DNAsamp"
mpwmdsurv2016$SiteTo<-NA

mpwmdsurv2016<-mpwmdsurv2016[,flds]

################################################################
# MPWMD Fall Pop Survey 2015. Omits (for now) second sheet with Brown Trout data
mpwmdsurv2015 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FallPopSurvey/2015/MPWMD_FallPop2015_11182017.xlsx", sheetName="SH_Data", 
                            colClasses=c("Date", "character", "numeric", "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character"),
                            stringsAsFactors=FALSE)

mpwmdsurv2015$SiteType <- "MPWMD_FallPop"
mpwmdsurv2015$SiteID <- NA
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Site"  )]] <- "MPWMD_Name"
mpwmdsurv2015$Stream <- NA
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Pass.."  )]] <- "Pass"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Fish.."  )]] <- "FishNum"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="FL..mm." )]] <- "FL_mm"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Wt...g." )]] <- "Wt_g"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="PIT.."   )]] <- "PITnum"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Tag.size")]] <- "TagSize"
names(mpwmdsurv2015)[[which(names(mpwmdsurv2015)=="Fin.clip")]] <- "DNAsamp"
mpwmdsurv2015$SiteTo<-NA

# correct the tag numbers with proper prefixes
idx <- substr(mpwmdsurv2015$PITnum, 1, 4)=="1786" | substr(mpwmdsurv2015$PITnum, 1, 4)=="1792"
mpwmdsurv2015$PITnum[idx] <- paste0("000000", mpwmdsurv2015$PITnum[idx])

mpwmdsurv2015 <- mpwmdsurv2015[,flds]


################################################################
# MPWMD Fall Pop Survey 2014. No tagging occurred at these events
mpwmdsurv2014 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FallPopSurvey/2014/MPWMD_FallPop2014_10222017.xlsx", 
                            sheetIndex=1, startRow=1,endRow=308,
                            colClasses=c("Date", "character", "numeric", "numeric", "numeric", "numeric", "character", "character"),
                            stringsAsFactors=FALSE)

mpwmdsurv2014$SiteType <- "MPWMD_FallPop"
mpwmdsurv2014$SiteID<-as.character(NA)
mpwmdsurv2014$Stream<-as.character(NA)
names(mpwmdsurv2014)[[which(names(mpwmdsurv2014)=="Site")]] <- "MPWMD_Name"
mpwmdsurv2014$SiteTo <- as.character(NA)
mpwmdsurv2014$PITnum <- as.character(NA)
mpwmdsurv2014$DNAsamp <- as.character(NA)
mpwmdsurv2014$TagSize <- as.character(NA)
mpwmdsurv2014$Recap <- as.character(NA)
mpwmdsurv2014$SiteTo <-as.character(NA)

mpwmdsurv2014 <- mpwmdsurv2014[,flds]


################################################################
# MPWMD Fall Pop Survey 2013 with NMFS tagging practice

mpwmdsurv2013 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FallPopSurvey/2013/Carmel_efishing_2013_10262017.xlsx", sheetIndex=1, startRow=3,
                            colClasses=c("Date", "character", "numeric", "numeric", "numeric", "numeric", "character", "character"),
                            stringsAsFactors=FALSE)
mpwmdsurv2013$SiteType <- "MPWMD_FallPop"
mpwmdsurv2013$SiteID<-as.character(NA)
mpwmdsurv2013$Stream<-
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Site")]] <- "MPWMD_Name"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Pass.."  )]] <- "Pass"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Fish.."  )]] <- "FishNum"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="FL..mm." )]] <- "FL_mm"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Wt...g." )]] <- "Wt_g"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="PIT.."   )]] <- "PITnum"
mpwmdsurv2013$Recap<-
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Tag.size")]] <- "TagSize"
names(mpwmdsurv2013)[[which(names(mpwmdsurv2013)=="Fin.clip")]] <- "DNAsamp"
mpwmdsurv2013$SiteTo <- as.character(NA)

mpwmdsurv2013 <- mpwmdsurv2013[,flds]

#######################################################
# ADD IN MPWMD data 2013 and earlier
mpwmd_pre2013 <- read.xlsx2("C:/Users/HaleyOhms/Documents/Carmel/Tag Data/MPWMD_Data/MPWMD_FallPopSurvey/Pre-2013/Final_Data\\Fall_Survey_Data.xlsx", sheetIndex=1, startRow=1,
                            colClasses=c("Date", "character", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                         "numeric", "character"),
                            stringsAsFactors=FALSE)

mpwmd_pre2013$SiteType <- "MPWMD_FallPop"
mpwmd_pre2013$FishNum <- NA
names(mpwmd_pre2013)[[which(names(mpwmd_pre2013)=="Length" )]] <- "FL_mm"
names(mpwmd_pre2013)[[which(names(mpwmd_pre2013)=="Weight" )]] <- "Wt_g"
mpwmd_pre2013$PITnum<-NA
mpwmd_pre2013$Recap<-NA
mpwmd_pre2013$TagSize<-NA
mpwmd_pre2013$DNAsamp<-NA
mpwmd_pre2013$Notes<-NA
mpwmd_pre2013$SiteTo<-NA

mpwmd_pre2013 <- mpwmd_pre2013[,flds]


#################################
#################################
###### Combine the data frames
#################################
#################################

dat <- rbind(mpwmd_pre2013, mpwmdsurv2015, mpwmdsurv2016, rescue2014, release2014, scl2014, nmfs2016, 
             fishdata_2017, mpwmdsurv2014, shsrf2016, shrf2013, mpwmdsurv2013)

#################################
#################################

#################################
#################################
###### Clean up shop
#################################
#################################
  
# Remove weird rows with all NAs
   #which(is.na(dat$Date))
dat<-dat[complete.cases(dat[ , "Date"]),]


# Standardize the field contents 

### convert character fields to logical fields
unique(dat$Recap)
dat$Recap <- dat$Recap=="Y" | dat$Recap=="T"
unique(dat$Recap)

unique(dat$DNAsamp)  # Note: empty field corresponds to no fin clip (ie false)
dat$DNAsamp <- dat$DNAsamp=="C" | dat$DNAsamp=="T" | dat$DNAsamp=="Y"
unique(dat$DNAsamp)

# convert tag size field
unique(dat$TagSize)
dat$TagSize[ is.na(dat$TagSize) ]   <- "NA"
dat$TagSize[ dat$TagSize==""]   <- "NA"
dat$TagSize[ dat$TagSize=="12"] <- "12mm"
dat$TagSize[ dat$TagSize=="23"] <- "23mm"
dat$TagSize[ dat$TagSize=="t"]  <- "12mm"
dat$TagSize[ dat$TagSize=="T"]  <- "23mm"
unique(dat$TagSize)


# make uniform designations of MPWMD sites
dat$MPWMD_Name[dat$MPWMD_Name=="GarlandPark"] <- "Garland"
dat$MPWMD_Name[dat$MPWMD_Name=="Sleepy Hollow RF"] <- "SHSRF"
dat$MPWMD_Name[dat$MPWMD_Name=="StonePine"] <- "Stonepine"
dat$MPWMD_Name[dat$MPWMD_Name=="Upper Inundation Zone"] <- "SCRupper"
dat$MPWMD_Name[dat$MPWMD_Name=="Upper IND Zone"] <- "SCRupper"
dat$MPWMD_Name[dat$MPWMD_Name=="UIND"] <- "SCRupper"
dat$MPWMD_Name[dat$MPWMD_Name=="UIZ"] <- "SCRupper"
dat$MPWMD_Name[dat$MPWMD_Name=="LosCompadres"] <- "Compadres"
dat$MPWMD_Name[dat$MPWMD_Name=="Los Compadres"] <- "Compadres"
dat$MPWMD_Name[dat$MPWMD_Name=="SleepyHollow"] <- "Sleepyhollow"
dat$MPWMD_Name[dat$MPWMD_Name=="Sleepy Hollow"] <- "Sleepyhollow"
dat$MPWMD_Name[dat$MPWMD_Name=="Scarlet"] <- "Scarlett"
dat$MPWMD_Name[dat$MPWMD_Name=="UpperGarland"] <- "Garland"
dat$MPWMD_Name[dat$MPWMD_Name=="DeDampierre"] <- "Dedampierre"
dat$MPWMD_Name[dat$MPWMD_Name=="Pinecreek2l"] <- "Pinecreek"
dat$MPWMD_Name[dat$MPWMD_Name=="Pinecreek2"] <- "Pinecreek"
dat$MPWMD_Name[dat$MPWMD_Name=="Stonepine2"] <- "Stonepine"
dat$MPWMD_Name[dat$MPWMD_Name=="Redrock2"] <- "Redrock"
dat$MPWMD_Name[dat$MPWMD_Name=="Red Rock"] <- "Redrock"
dat$MPWMD_Name[dat$MPWMD_Name=="Garland2"] <- "Garland"
dat$MPWMD_Name[dat$MPWMD_Name=="Garland3"] <- "Garland"
dat$MPWMD_Name[dat$MPWMD_Name=="Stonepine3"] <- "Stonepine"
dat$MPWMD_Name[dat$MPWMD_Name=="Redrock3"] <- "Redrock"
dat$MPWMD_Name[dat$MPWMD_Name=="Scarlett2"] <- "Scarlett"
dat$MPWMD_Name[dat$MPWMD_Name=="Valleygreen2"] <- "Valleygreen"
dat$MPWMD_Name[dat$MPWMD_Name=="Valley Greens"] <- "Valleygreen"

dat$SiteID[which(dat$SiteID=="Sleepy Hollow RF")]<-"SHRF"
dat$SiteID[which(dat$SiteID=="CDFW Wild Trout 1")]<-"182"
dat$SiteID[which(dat$SiteID=="CDFW Wild Trout 2")]<-"186"
dat$SiteID[which(dat$SiteID=="CDFW Wild Trout 3")]<-"194"
dat$SiteID[which(dat$MPWMD_Name=="SCRlower")]<-"124"

unique(dat$MPWMD_Name)

# Update Site IDs for 2013 to 2017 only
# (correct IDs are in pre2013 data)
# noid<-dat[which(is.na(dat$SiteID)) , ]
# min(noid$Date)
# max(noid$Date)

early[which(early$MPWMD_Name=="Boronda") , ]

require(lubridate)



early <- dat[year(dat$Date)>=2013 , ]
early$SiteID[which(early$MPWMD_Name=="Cachagua")]<-"164"
early$SiteID[which(early$MPWMD_Name=="Compadres")]<-"134"
early$SiteID[which(early$MPWMD_Name=="Stonepine")]<-"104"
early$SiteID[which(early$MPWMD_Name=="Sleepyhollow")]<-"114"
early$SiteID[which(early$MPWMD_Name=="Garland")]<-"73"
early$SiteID[which(early$MPWMD_Name=="Boronda")]<-"81"
early$SiteID[which(early$MPWMD_Name=="Dedampierre")]<-"90"
early$SiteID[which(early$MPWMD_Name=="Scarlett")]<-"53"
early$SiteID[which(early$MPWMD_Name=="SCRupper")]<-"128"
early$SiteID[which(early$MPWMD_Name=="Redrock")]<-"41"
early$SiteID[which(early$MPWMD_Name=="Valleygreen")]<-"22"
early$SiteID[which(early$MPWMD_Name=="SHRF")]<-"SHRF"

dat <- dat[!year(dat$Date)>=2013 , ]
dat <- rbind(dat,early)

# unique(year(dat$Date))
# unique(dat$SiteID)
# unique(dat$MPWMD_Name)
# dat[which(dat$MPWMD_Name=="SCRlower") , ]


early <- dat[year(dat$Date)=="1994" , ]
early$SiteID[which(early$MPWMD_Name=="Cachagua")]<-"164"
early$SiteID[which(early$MPWMD_Name=="Compadres")]<-"134"
early$SiteID[which(early$MPWMD_Name=="Stonepine")]<-"105"
early$SiteID[which(early$MPWMD_Name=="Sleepyhollow")]<-"114"
early$SiteID[which(early$MPWMD_Name=="Garland")]<-"66"
early$SiteID[which(early$MPWMD_Name=="Boronda")]<-"80"
early$SiteID[which(early$MPWMD_Name=="Dedampierre")]<-"90"
early$SiteID[which(early$MPWMD_Name=="Scarlett")]<-"52"
early$SiteID[which(early$MPWMD_Name=="SCRupper")]<-"128"
early$SiteID[which(early$MPWMD_Name=="Redrock")]<-"41"
early$SiteID[which(early$MPWMD_Name=="Valleygreen")]<-"22"
early$SiteID[which(early$MPWMD_Name=="SHRF")]<-"SHRF"

dat <- dat[!year(dat$Date)=="1994" , ]
dat <- rbind(dat,early)


# unique identifier for DNA samples
dat$Date<-as.character(dat$Date)
dat$FishNum<-as.numeric(dat$FishNum)

unique(dat$DNAsamp)
str(dat$DNAsamp)


for (i in 1:nrow(dat)) {if(dat$DNAsamp[i]=="FALSE" | is.na(dat$DNAsamp[i])){dat$DNA_ID[i]=NA}
  else {dat$DNA_ID[i]<-sprintf("Carmel_%s-%04.0f_Site%s",
                            dat$Date[i],
                            dat$FishNum[i],
                            dat$SiteID[i])}   }

head(dat)


# # Sort dataframe by date and fish number
# ord <- order(dat$DNA_ID)
# dat <- dat[ord,]

###############
###############
# check for nonstandard PITnum lengths
dat$PITnum[dat$PITnum=="NotScanned"] <- "NS"
dat$PITnum[dat$PITnum==""] <- "NA"

idx <- nchar(dat$PITnum) != 15 & nchar(dat$PITnum) != 2 & !is.na(dat$PITnum)
which(idx=="TRUE")

dat$PITnum[which(dat$PITnum=="141131969")]<-"000000141131969"
dat$PITnum[which(dat$PITnum=="141765649")]<-"000000141765649"

##################
# Check for duplicate PITnum
### (check code below)
dat$Recap[which(dat$PITnum=="900226000593520" & dat$Date =="2016-09-26")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593533" & dat$Date =="2016-09-19")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593611" & dat$Date =="2016-09-19")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593615" & dat$Date =="2016-09-16")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593682" & dat$Date =="2016-09-19")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593768" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593782" & dat$Date =="2016-10-11")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593803" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000594362" & dat$Date =="2016-10-07")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000594431" & dat$Date =="2016-10-20")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000594500" & dat$Date =="2017-11-01")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000594660" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000594884" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900228000593519" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900228000593522" & dat$Date =="2017-11-02")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714243" & dat$Date =="2016-10-07")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714283" & dat$Date =="2017-09-21")]<-"TRUE"
dat$Recap[which(dat$PITnum=="900226000593063" & dat$Date =="2017-10-19")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714944" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714771" & dat$Date =="2017-09-28")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714970" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362714979" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362715002" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362715023" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362715027" & dat$Date =="2017-10-25")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362715060" & dat$Date =="2016-09-26")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000362724490" & dat$Date =="2016-09-09")]<-"TRUE"
dat$Recap[which(dat$PITnum=="982000365198574" & dat$Date =="2017-10-10")]<-"TRUE"

dat$PITnum[which(dat$PITnum=="900226000593101" & dat$FL_mm ==73)]<-"900226000593010"
dat$PITnum[which(dat$PITnum=="900226000593002" & dat$FishNum ==91)]<-"900226000593022"
dat$PITnum[which(dat$PITnum=="900226000593022" & dat$FishNum ==91)]<-87
dat$PITnum[which(dat$PITnum=="900226000593123" & dat$FL_mm ==99)]<-"900226000593126"


# unique(dat$Recap)
# dat$Recap[which(is.na(dat$Recap))]<-"FALSE"
# 
# origcaps <- subset(dat, !(Recap=="TRUE"))
# origcaps <- subset(origcaps, !is.na(origcaps$PITnum))
# origcaps <- subset(origcaps, !(PITnum=="NA"))
# origcaps <- subset(origcaps, !(PITnum=="NS"))
# unique(origcaps$Recap)
# 
# require(dplyr)
# 
# repeats <-
#   origcaps %>%
#   group_by(PITnum) %>%
#   filter(n()>1)
# 
# repeats <- data.frame(repeats)
# 
# in_ord <- order(repeats$PITnum)
# repeats <- repeats[in_ord,]

#write.csv(repeats,"C:\\Users\\haley.ohms\\Desktop\\Carmel project\\PITrepeats.csv")



################################
## Deal with Notes

unique(dat$Notes)

dat$SiteTo[which(dat$Notes=="Mortality. Tag recovered for reuse")]<-"Mort"
dat$PITnum[which(dat$Notes=="Mortality. Tag recovered for reuse")]<-NA
dat$DNAsamp[which(dat$Notes=="Missing tissue sample")]<-"FALSE"
dat$SiteTo[which(dat$Notes=="Nicked intestine")]<-"Mort"
dat$Wt_g[which(dat$Notes=="Unclear Wt.")]<-"NA"
dat$DNAsamp[which(dat$Notes=="Missing FL and Wt data. No Tissue Sample but it has an envelope (empty).")]<-"NA"
dat$Notes[which(dat$Notes=="  ")]<-""
dat$FL_mm[which(dat$Notes=="FL unclear.")]<-"NA"
dat$PITnum[which(dat$Notes== "Not tagged")]<-"NA"
dat$DNAsamp[which(dat$Notes== "No DNA. FL < 60 mm")]<-"NA"
dat$SiteTo[which(dat$Notes=="Mort")]<-"Mort"
dat$SiteTo[which(dat$Notes=="Not taggable. Dying")]<-"Mort"
dat$SiteTo[which(dat$Notes=="side channel. Mort")]<-"Mort"
dat$SiteTo[which(dat$Notes=="main channel. Mort")]<-"Mort"
dat$SiteTo[which(dat$Notes=="MORT; tag removed")]<-"Mort"
dat$SiteTo[which(dat$Notes=="No tag, health concerns; MORT")]<-"Mort"
dat$SiteTo[which(dat$Notes=="MORT")]<-"Mort"
dat$SiteTo[which(dat$Notes=="Drew blood-- MORT, body discarded, tag retrieved")]<-"Mort"
dat$SiteTo[which(dat$Notes=="MORT, body discarded, tag retrieved")]<-"Mort"
dat$SiteTo[which(dat$Notes=="MORT, no DNA sample")]<-"Mort"
dat$SiteTo[which(dat$Notes=="Tagged when moved from QT 5 to RC9. Bled a little, Mort")]<-"Mort"
dat$SiteTo[which(dat$Notes=="Tagged when moved from QT 5 to RC9. Mort")]<-"Mort" 
dat$Wt_g[which(dat$Notes=="weighed with tag. Tag = 0.6 grams")]<-(13.8-0.6)
dat<-dat[-(which(dat$Notes=="Brown trout. Euthanized")),]
dat<-dat[-(which(dat$Notes=="BROWN TROUT")),]
dat$Wt_g[which(dat$Notes=="Fish fell into bucket while weighing")]<-"NA"
dat$Notes[which(dat$Notes== "BN")]<-"Captured in block net"
dat$SiteType[which(dat$SiteFrom== "CDFW Wild Trout 2")]<-"CDFW Wild Trout"
dat$SiteType[which(dat$SiteFrom== "CDFW Wild Trout 1")]<-"CDFW Wild Trout"
dat$PITnum[which(dat$Notes=="Fish shed tag (Found in bottom of tank).")]<-NA
dat$PITnum[which(dat$Notes== "Mortality. Tag recovered for reuse PIT-tag training")]<-"NA"
dat$Notes[which(dat$Notes=="BN")]<-"Captured in block net"
dat$DNAsamp[which(dat$Notes=="DNA unable to be used; tag repeat")]<-FALSE



##################
##################

dat$SiteType<-NULL
dat$DNA_ID<-NULL

# export a csv
require(tidyverse)
write_csv(dat, "C:/Users/HaleyOhms/Documents/Carmel/Database/pre2018FishData.csv", col_names = T)





##################
## NOT ADDRESSED
#dat[which(dat$Notes== "Recap from today, fish #21, no clip"),]
#dat[which(dat$Notes== "Fish #2 from today. No fin clip"),]


#write.csv(dat, "C:\\Users\\haley.ohms\\Desktop\\Carmel project\\Local data\\FishDataLocal.csv")

##################
##################
## 2017 QA/QC
### check lengths/weights
# ## Add year to dataframe (to assign lengths and widths)
# dat$Date<-ymd(dat$Date)
# dat$Year<-year(dat$Date) 
# 
# dat$Year = NA
# for(i in 1:nrow(dat)){
#   sep = unlist(strsplit(as.character(dat$Date[i]), "-"))
#   dat$Year[i] = sep[1]
# }




# # 
# dat17<-subset(dat, Year=="2017")
# str(dat17)
# unique(dat17$SiteID)
# 
# dat17$FL_mm<-as.numeric(dat17$FL_mm)
# dat17$Wt_g<-as.numeric(dat17$Wt_g)
# # 
# # p<-plot(dat17$FL_mm, dat17$Wt_g)
# # identify(dat17$FL_mm, dat17$Wt_g, plot = TRUE)
# 
# cal17 <- subset(dat, Year=="2017" & SiteID=="CDFW Wild Trout 1")
#   
# unique(cal17$PITnum)  

# repeat PIT numbers?
## NONE




##################
##################

# ## Compare our data to Cory's sheet
# ## to identify discrepencies
# rm(list=ls())
# require(dplyr)
# 
# dat<-read.csv("G:\\development\\Carmel\\Database\\FishData.csv", header=TRUE)
# 
# corydat <- read.csv("G:\\development\\Carmel\\OmyData\\MPWMD_Data\\MPWMD_FallPopSurvey\\Juvenile Steelhead_Date_Location_Length_Weight_Tag.csv",
#                     header=TRUE)
# 
# 
# ## Add year to dataframe (to assign lengths and widths)
# dat$Year = NA
# for(i in 1:nrow(dat)){
#   sep = unlist(strsplit(as.character(dat$Date[i]), "-"))
#   dat$Year[i] = sep[1]
# }
# 
# str(corydat)
# corydat$Length..mm. <- as.numeric(corydat$Length..mm.)
# corydat$Date <- as.character(corydat$Date)
# corydat$Year = NA
# for(i in 1:nrow(corydat)){
#   sep = unlist(strsplit(as.character(corydat$Date[i]), "/"))
#   corydat$Year[i] = sep[3]
# }


# oursites<-
#   dat %>%
#   group_by(Year) %>%
#   summarise(n_sites=n_distinct(SiteID))
# 
# corysites <-
#   corydat %>%
#     group_by(Year) %>%
#     summarise(n_sites=n_distinct(Location))
# unique(corydat$Location)
# 
# cbind(oursites, corysites)

# str(dat)
# 
# oursites2<-
#   dat %>%
#   group_by(Year, MPWMD_Name) %>%
#   summarise(meanFL=mean(FL_mm, na.rm=TRUE), 
#             nfish = n())
# 
# ?mean
# 
# corysites2<-
#   corydat %>%
#   group_by(Year, Location) %>%
#   summarise(meanFL=mean(Length..mm., na.rm=TRUE), 
#             nfish = n())
# 
# corysites2 <- data.frame(corysites2)
# str(corysites2)
# data.frame(oursites2)
# 
# write.csv(oursites2, "C:\\Users\\haley.ohms\\Desktop\\Carmel project\\oursites.csv")
# write.csv(corysites2, "C:\\Users\\haley.ohms\\Desktop\\Carmel project\\corysites.csv")








