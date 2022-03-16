############################################################################################
############################################################################################
## Compile 2022 SHRF Release Data
############################################################################################
############################################################################################


rm(list=ls())
require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)


  dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_SHSRF/2021"
  
  files = list.files(dir, '*.txt', recursive = F, full.names = TRUE) 
  bnames = basename(files)
  bnames = sub('.txt', '',bnames)
  
  for(i in 1:length(files)){
    #i=1
    tbl<-read.table(files[i], header=FALSE, sep="|")
    
    pieces = unlist(strsplit(bnames[i], '_'))
      
    # Assign date based on file name
      tbl$Date = ymd(pieces[2])
      
    # Assign RC value Here
      tbl$V5 <- as.character(tbl$V5)
      RC <- tbl[1,5]
      tbl$V8 <- RC
 
    if(i == 1){
      dfr = tbl
    } else{
      dfr = rbind(dfr, tbl)
    }
  }

  dfr$V6 <- dfr$V7 <- NULL
  
  colnames(dfr)<-c("FishNumber","FL_mm", "Wt_g","PITnumber","FieldNotes","Date", "SiteName")
    
  #Clean up formatting
  dfr$FishNumber<-substring(dfr$FishNumber, 3)
  dfr$FishNumber<-as.numeric(dfr$FishNumber)

  #Tags to 15 digits
  dfr$pre<-substr(dfr$PITnumber, start=1, stop=2)
  dfr$PITnumber<-ifelse(dfr$pre=="R0",substring(dfr$PITnumber, 7),substring(dfr$PITnumber, 9)) 
  dfr$pre<-NULL 

  # Dummy tag numbers:
  #   R00000000000141765649
  #   A0000000982126054206704
  #   A0000000985121003064790
  
  # Remove dummy tags
  dfr <- dfr %>% mutate(PITnumber = ifelse(PITnumber=="000000141765649", NA, 
                                           ifelse(PITnumber=="982126054206704", NA, 
                                                  ifelse(PITnumber=="985121003064790", NA, PITnumber))))
  
  # Mark recaps
  dfr <- dfr %>% mutate(Recapture = ifelse(FieldNotes=="RECAP", TRUE, 
                                           ifelse(FieldNotes=="M RECAP", TRUE, FALSE)))
  
  # Sex
  dfr <- dfr %>% mutate(Sex = ifelse(FieldNotes=="RC1 M", "M", 
                                           ifelse(FieldNotes=="M RECAP", "M", 
                                                  ifelse(FieldNotes=="M", "M",  
                                                         ifelse(FieldNotes=="F", "F", NA)))))

  # Figure out tag sizes based on fish lengths
  dfr$pre<-substr(dfr$PITnumber, start=1, stop=7)
    dfr %>% ggplot() + geom_boxplot(aes(pre, FL_mm))
  
  # Tag Size
    dfr <- dfr %>% mutate(TagSize = ifelse(pre=="9002260", "12mm", 
                                       ifelse(pre=="9820910", "12mm", 
                                              ifelse(pre=="9002280", "23mm",  
                                                     ifelse(pre=="9821260", "23mm", NA)))))
  dfr$pre <- NULL

#Add columns to match fish data
  dfr$Species <- "O. mykiss"
  dfr$ScaleSample <- NA
  dfr$DataEntryNotes <- "Data recorded on handheld readers, files compiled by H. Ohms on 3-16-22"
  dfr$DNAID <- NA
  dfr$DNAsample <- NA
  dfr$FishGroup <- NA
  dfr$FishID <- NA
  dfr <- dfr %>% mutate(LifeForm = ifelse(is.na(Sex), NA, "RT"))
  dfr$OccCode <- "CAP"
  dfr$CaptureYear="2021"
  
  dfr$SiteName <- paste("SHSRF_", dfr$SiteName, sep = "")
  
  dfr <- dfr %>% mutate(SiteName = ifelse(SiteName=="SHSRF_RC1 M", "SHSRF_RC1", SiteName))
  
  dfr <- dfr %>% mutate(SiteID = ifelse(SiteName=="SHSRF_RC1", 579,
                          ifelse(SiteName=="SHSRF_RC3", 581, 
                            ifelse(SiteName=="SHSRF_RC4", 582, 
                              ifelse(SiteName=="SHSRF_RC5", 583, 
                                ifelse(SiteName=="SHSRF_RC8", 586, 
                                  ifelse(SiteName=="SHSRF_RC9", 587, 
                                    ifelse(SiteName=="SHSRF_RC10", 588, 
                                      ifelse(SiteName=="SHSRF_RC11", 589, 
                                        ifelse(SiteName=="SHSRF_RC12", 590, 
                                          ifelse(SiteName=="SHSRF_RC13", 591, 
                                            ifelse(SiteName=="SHSRF_RC14", 592, NA))))))))))))
  
  dfr$OccID <- paste(dfr$OccCode, dfr$SiteID, dfr$Date, sep="-")
  dfr$Year <- "2022"
  
  # Site To: 
  
  
  sort(names(FishData))
  sort(names(dfr))

  FishData2 <- rbind(FishData, dfr)
  
  
  
  
  
  
  
  
 
      save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
           Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID, 
           Redd, Station, Temperature, Translocation, XS, file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss_20220316.RData")


      load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")


####################################################################  
####################################################################  
####################################################################  
      
####################################################################  
    ## Weir data from 2021
####################################################################      
      
    require(readxl)
    weirDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/PIT_Tag_Metadata.xlsx", sheet=1)
    
      colnames(weirDat) <- c("Date", "Time", "FL_mm", "FL_in", "Prec", "Sex", "Recapture", 
                             "Tag", "Pre", "Post", "ScaleSample", "DNAsample", "FieldNotes")
      weirDat$Time <- weirDat$FL_in <- weirDat$Tag <- NULL
      
      weirDat$FL_mm <- as.integer(weirDat$FL_mm)
        weirDat <- weirDat %>% filter(!is.na(FL_mm)) #Remove fish with no length measure; count will be complete in Weir table
     
      weirDat <- weirDat %>% mutate(Recapture = ifelse(Recapture=="Y", TRUE, FALSE)) 
      
      weirDat$ScaleSample <- weirDat$DNAsample <- FALSE
    
      weirDat$PITnumber <- paste(weirDat$Pre, weirDat$Post, sep="")
        weirDat <- weirDat %>% mutate(PITnumber = ifelse(PITnumber=="NANA", NA, PITnumber))
        weirDat$Pre <- weirDat$Post <- NULL
      
      # Add measuring method to notes:
        weirDat <- weirDat %>% 
          mutate(FieldNotes = ifelse(Prec=="E", paste(FieldNotes, "FL estimated", sep=", "), FieldNotes))
        weirDat$Prec <- NULL
      
     # Add in the remaining database stuff
        weirDat$Species <- "O. mykiss"
        weirDat$DataEntryNotes <- "Data recorded by hand, files compiled by H. Ohms on 3-16-22"
        weirDat$DNAID <- NA
        weirDat$FishGroup <- NA
        weirDat$FishID <- NA
        weirDat$LifeForm<- "SH" 
        weirDat$OccCode <- "WIR"
        weirDat$CaptureYear="2021"
        weirDat$SiteName <- "Weir"
        weirDat$SiteID <- 1
        weirDat$FishNumber <- NA
        weirDat$SiteTo<- NA
        weirDat$TagSize <- "23mm"
        weirDat$Wt_g <- NA
        
        weirDat$OccID <- paste(weirDat$OccCode, weirDat$SiteID, weirDat$Date, sep="-")
        weirDat$Year <- "2021" 
        
        sort(names(weirDat))
        sort(names(FishData))
        

  load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")