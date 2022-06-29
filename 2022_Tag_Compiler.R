
rm(list=ls())
require(dplyr)
require(tidyverse)
require(lubridate)
require(xlsx)
require(readxl)


####################################################################  
    ## Add 2021 weir collected fish to FishData table
####################################################################   
     
    load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")


    require(readxl)
    weirDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2021/PIT_Tag_Metadata.xlsx", sheet=1, 
                          col_types = c("date", "skip", "numeric", "skip", 
                                        "text", "text", "text", 
                                        "text", "text", "text", 
                                        "text", "text", "text"))

      colnames(weirDat) <- c("Date", "FL_mm", "Prec", "Sex", "Recapture",
                             "Tag", "Pre", "Post", "ScaleSample", "DNAsample", "FieldNotes")
      weirDat$Tag <- NULL

      weirDat$FL_mm <- as.integer(weirDat$FL_mm)
        weirDat <- weirDat %>% filter(!is.na(FL_mm)) #Remove fish with no length measure; count will be complete in Weir table

      weirDat <- weirDat %>% mutate(Recapture = ifelse(Recapture=="Y", TRUE, FALSE))

      weirDat$ScaleSample <- weirDat$DNAsample <- FALSE

      weirDat <- weirDat %>% mutate(Post = ifelse(Post=="365198736", "000365198736", Post))
      
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
        weirDat$SiteID <- 001
        weirDat$FishNumber <- NA
        weirDat$SiteTo<- NA
        weirDat$TagSize <- "23mm"
        weirDat$Wt_g <- NA

        weirDat$OccID <- paste(weirDat$OccCode, weirDat$SiteID, weirDat$Date, sep="-")
        weirDat$Year <- "2021"

        sort(names(weirDat))
        sort(names(FishData))

        FishData <- rbind(FishData, weirDat)


        save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
             Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID,
             Redd, Station, Temperature, Translocation, Weir, XS,
             file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")

  ####################################################################  
   ## Create Weir operation table for 2021 data and future data
  ####################################################################      
        
    load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
        
    wmDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2021/Weir_Metadata.xlsx", sheet=1)

    names(wmDat)        
    colnames(wmDat) <- c("Date", "Fishing", "Captured", "Tagged", "Recaptures", 
                         "StackSurvey", "StackCount", "StackNotes", "StageHeight", 
                         "WaterTemp_C", "Flow_cfs", "FieldNotes")        

    # Add: Tagger, OccID, DataEntryNotes 
    wmDat$Tagger <- NA
    wmDat <- wmDat %>% mutate(OccID = paste("WIR", Date, "001", sep = "-"))
    wmDat$DataEntryNotes <- "Entered by H. Ohms on 3-18-2022 from MPWMD excel data file"  
    wmDat$SiteID <- "001"
    
    #Convert water temp to Celsius
    require(weathermetrics)
    wmDat$WaterTemp_C <- fahrenheit.to.celsius(wmDat$WaterTemp_C, round=2) 
    
    # Add check time
    weirDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2021/PIT_Tag_Metadata.xlsx", sheet=1)
    
    colnames(weirDat) <- c("Date", "Time", "FL_mm", "FL_in", "Prec", "Sex", "Recapture", 
                           "Tag", "Pre", "Post", "ScaleSample", "DNAsample", "FieldNotes")
    
    ## NOTE: I manually changed the times after 3/13 back to PST (they were recorded as PDT)
      weirDat$Time <- format(weirDat$Time, format="%H:%M:%S")
    
    weirDatSub <- weirDat %>% group_by(Date) %>% slice_tail() %>% select(Date, Time) 

    wmDat <- left_join(wmDat, weirDatSub)
  
    Weir <- wmDat
    
    
    # Save weir table in with database
    save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
         Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID, 
         Redd, Station, Temperature, Translocation, Weir, XS, 
         file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
    
   
    
    ####################################################################  
    ####################################################################  
    ####################################################################  
       
        
    ############################################################################################
    ############################################################################################
    ## Compile 2022 SHRF Release Data
    ############################################################################################
    ############################################################################################



    dir = "C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_SHSRF/2022/Tagging Files"

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

    ###############################
    ## ADD IN SITE TO DATA:
    ###############################

    require(readxl)
    toDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_SHSRF/2022/SHSRF 2021 Season Release locations.xlsx", sheet=1)

    head(toDat)
    toDat <- toDat %>% select(Date, `RC Bay`, SiteTo) %>%
      mutate(SiteName = paste("SHSRF_RC", `RC Bay`, sep="")) %>%
      select(Date, SiteName, SiteTo)

    dfr <- left_join(dfr, toDat)

    load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
    sort(names(FishData))
    sort(names(dfr))

    FishData <- rbind(FishData, dfr)


    save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
         Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID,
         Redd, Station, Temperature, Translocation, XS, file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")

    


    ####################################################################
    ####################################################################
    ####################################################################

    ####################################################################
    ## 2021 Fall Pop survey occasion data; put in Depletion table
    ####################################################################

    load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")


    require(readxl)
    depDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_FallPopSurvey/2021/Occasion Data/Occasion Data/Compiled_Occasion_2021.xlsx", sheet=1)

    depDat$AirTemp_C <- depDat$Conductivity <- NA
    depDat$CDFWsite <- FALSE
    depDat$CoordSource <- "Previous"

    max(Depletion$DepID) #395 was max
    depDat <- depDat %>% mutate(DepID = 396:403)

    depDat$EfisherSettings <- NA
    depDat$FishSampType <- "Efish"
    depDat$MPWMDsite <- TRUE
    depDat$NumEfishers <- 2

    depDat <- depDat %>% mutate(OccID = paste("DEP", SiteID, Date, sep="-"))

    depDat$Personnel <- "CH, HO, DA, RG, EL"
    depDat$Sampleable <- TRUE
    depDat$SiteSampType <- "Index"
    depDat$Stream <- "Carmel"
    depDat$Wet <- TRUE
    depDat$Year <- 2021

    Depletion <- rbind(Depletion, depDat)

    save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
         Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID,
         Redd, Station, Temperature, Translocation, XS, file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")

    
    ####################################################################  
    ## Write out FishData table for Cory
    ####################################################################   
    
    load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
      write_csv(FishData,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/CarmelFishData.xlsx")
      
    # Remove weird missing tag entries: 
      FishData <- FishData %>% mutate(PITnumber = ifelse(PITnumber == "", NA, 
                                                         ifelse(PITnumber =="NoTag          ", NA, PITnumber)))

    
      TagData <- FishData %>% filter(!is.na(PITnumber))
        write_csv(TagData,"C:/Users/HaleyOhms/Documents/Carmel/DATA/Data Deliveries/CarmelTagData.xlsx")

 
        
   ####################################################################  
    ## Add 2022 weir collected fish to FishData table
   ####################################################################   
        
        load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
        
        
        require(readxl)
        weirDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2022/PIT_Tag_Metadata.xlsx", sheet=1, 
                              col_types = c("date", "skip", "numeric", "skip", 
                                            "text", "text", "text", 
                                            "text", "text", "text", 
                                            "text", "text", "text", "text"))
        
        colnames(weirDat) <- c("Date", "FL_mm", "Prec", "Sex", "Recapture",
                               "Tag", "Pre", "Post", "ScaleSample", "DNAsample", "FieldNotes", "Tagger")
        weirDat$Tag <- NULL
        
        weirDat$FL_mm <- as.integer(weirDat$FL_mm)
        weirDat <- weirDat %>% filter(!is.na(FL_mm)) #Remove fish with no length measure; count will be complete in Weir table
        
        weirDat <- weirDat %>% mutate(Recapture = ifelse(Recapture=="Y", TRUE, FALSE))
          weirDat <- weirDat %>% mutate(DNAsample = ifelse(DNAsample=="Y", TRUE, FALSE))
            weirDat <- weirDat %>% mutate(ScaleSample = ifelse(ScaleSample=="Y", TRUE, FALSE))
          
  
        weirDat <- weirDat %>% mutate(Post = ifelse(Post=="365198736", "000365198736", Post))
        
        weirDat$PITnumber <- paste(weirDat$Pre, weirDat$Post, sep="")
        weirDat <- weirDat %>% mutate(PITnumber = ifelse(PITnumber=="NANA", NA, PITnumber))
        weirDat$Pre <- weirDat$Post <- NULL
        
        # Add measuring method to notes:
        weirDat <- weirDat %>%
          mutate(FieldNotes = ifelse(Prec=="E", paste(FieldNotes, "FL estimated", sep=", "), FieldNotes))
        weirDat$Prec <- NULL
        
        # Add tagger to notes:
        weirDat <- weirDat %>%
          mutate(FieldNotes = ifelse(!is.na(Tagger), paste(FieldNotes, "; Tagger: ", Tagger, sep=""), FieldNotes))
        weirDat$Tagger <- NULL
        
        
        
        # Add in the remaining database stuff
        weirDat$Species <- "O. mykiss"
        weirDat$DataEntryNotes <- "Data recorded by hand, files compiled by H. Ohms on 3-16-22"
        weirDat$DNAID <- NA
        weirDat$FishGroup <- NA
        weirDat$FishID <- NA
        weirDat$LifeForm<- "SH"
        weirDat$OccCode <- "WIR"
        weirDat$CaptureYear="2022"
        weirDat$SiteName <- "Weir"
        weirDat$SiteID <- 001
        weirDat$FishNumber <- NA
        weirDat$SiteTo<- NA
        weirDat$TagSize <- "23mm"
        weirDat$Wt_g <- NA
        
        weirDat$OccID <- paste(weirDat$OccCode, weirDat$SiteID, weirDat$Date, sep="-")
        weirDat$Year <- "2022"
        
        sort(names(weirDat))
        sort(names(FishData))
        
        FishData <- FishData %>% filter(!(OccCode=="WIR" & Year=="2022"))
        
        FishData <- rbind(FishData, weirDat)
        
        
        save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
             Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID,
             Redd, Station, Temperature, Translocation, Weir, XS,
             file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
        
        ####################################################################  
        ## Add Weir operation data for 2022 
        ####################################################################      
        
        load(file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
        
        wmDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2022/2022_Weir_Metadata.xlsx", sheet=1)
        
        names(wmDat)        
        colnames(wmDat) <- c("Date", "Fishing", "Captured", "Tagged", "Recaptures", 
                             "StackSurvey", "StackCount", "StackNotes", "StageHeight", 
                             "WaterTemp_C", "Flow_cfs", "FieldNotes", "Backwatered")        
        
        # Add: Tagger, OccID, DataEntryNotes 
        wmDat$Tagger <- NA
        wmDat <- wmDat %>% mutate(OccID = paste("WIR", Date, "001", sep = "-"))
        wmDat$DataEntryNotes <- "Entered by H. Ohms on 3-18-2022 from MPWMD excel data file"  
        wmDat$SiteID <- "001"
        
        #Convert water temp to Celsius
        wmDat$WaterTemp_C <- as.numeric(wmDat$WaterTemp_C)
        require(weathermetrics)
        wmDat$WaterTemp_C <- fahrenheit.to.celsius(wmDat$WaterTemp_C, round=2) 
        
        # Add check time
        weirDat <- read_excel("C:/Users/HaleyOhms/Documents/Carmel/DATA/MPWMD_Data/MPWMD_WeirData/2022/PIT_Tag_Metadata.xlsx")
        
        colnames(weirDat) <- c("Date", "Time", "FL_mm", "FL_in", "Prec", "Sex", "Recapture", 
                               "Tag", "Pre", "Post", "ScaleSample", "DNAsample", "FieldNotes", "Tagger")
        
        weirDat$Time <- format(weirDat$Time, format="%H:%M:%S")
        
        weirDatSub <- weirDat %>% group_by(Date) %>% slice_tail() %>% select(Date, Time) 
        
        wmDat <- left_join(wmDat, weirDatSub)
        
        wmDat$Tagger <- NULL
          Weir$Tagger <- NULL
        
     
        # Add backwatered to notes:
        wmDat <- wmDat %>%
          mutate(Backwatered = ifelse(Backwatered=="NA", NA, Backwatered))
        
        wmDat <- wmDat %>%
          mutate(FieldNotes = ifelse(!is.na(Backwatered), paste(FieldNotes, "; Backwatered: ", Backwatered, sep=""), FieldNotes))
        wmDat$Backwatered <- NULL
        

        Weir <- rbind(wmDat, Weir)
        
        
        # Save weir table in with database
        save(Captive, Dam, Depletion, FishData, Flow, FlowMeasures,
             Frame, ReaderData, ReaderMetaDataBiomark, ReaderMetaDataORFID, 
             Redd, Station, Temperature, Translocation, Weir, XS, 
             file="C:/Users/HaleyOhms/Documents/GitHub/Carmel_Database/carmel_river_mykiss.RData")
        
        
        
        ####################################################################  
        ####################################################################  
        ####################################################################  
        
        
           