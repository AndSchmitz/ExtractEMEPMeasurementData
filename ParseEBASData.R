#init-----
rm(list=ls())
graphics.off()
options(warnPartialMatchDollar = T)

library(plyr)
library(tidyverse)

# WorkDir <- "D:\\Users\\Schmitz12\\Desktop\\EMEP data preparation"
WorkDir <- "D:\\Users\\Schmitz12\\Desktop\\NotSyncToY\\Projekte\\KRB Unsicherheiten Paper\\EMEP data preparation"

#Prepare I/O
InDir <- file.path(WorkDir,"Input")
OutDir <- file.path(WorkDir,"Output")
dir.create(OutDir,showWarnings = F)

#Read explanation of EMEP data quality flag values
EMEPFlagList <- read.table(file = file.path(InDir,"EMEP_data_flags.csv"),sep=";",header = T,skip = 2)

#Read EMEP data-----
InputFolders <- list.dirs(path = file.path(InDir),full.names = T)
InputFolders <- InputFolders[!(InputFolders == file.path(InDir))]
InputFolders <- basename(InputFolders)

#Define which metata variables to extract from files
MetatadataKeyWords <- data.frame(
  KeyWord = c("Station name","Station code","Station latitude","Station longitude","Station altitude","Component","Matrix","Unit","Instrument type"),
  KeyWordShort = c("StationName","code_plot","lat","lon","altitude","component","matrix","unit","InstrumentType"),
  stringsAsFactors = F
)

#_Loop over folders------
DataList <- list()
Metadata <- data.frame()
FileCounter <- 1

for ( CurrentFolderBasename in InputFolders ) {
  print(paste("Processing folder",CurrentFolderBasename,"..."))
  
  #Include only ".nas" files in file list
  CurrentFolder <- file.path(InDir,CurrentFolderBasename)
  InFiles <- list.files(CurrentFolder,pattern = ".nas")
  nInFiles <- length(InFiles)
  
  CurrentMetadata <- matrix(NA,nrow=nInFiles,ncol = nrow(MetatadataKeyWords))
  colnames(CurrentMetadata) <- MetatadataKeyWords$KeyWordShort
  CurrentMetadata <- as.data.frame(CurrentMetadata)
  CurrentMetadata$StartDate <- NA
  CurrentMetadata$EndDate <- NA
  CurrentMetadata$FileID <- NA
  
  #_Loop over files------
  for ( iFile in 1:nInFiles ) {
    CurrentInFile <- InFiles[iFile]
    cat(paste0(round(iFile/nInFiles*100,2),"% done. Parsing file ",CurrentInFile,"..."), " \r")
    flush.console()

    CurrentMetadata$FileName[iFile] <- CurrentInFile
    FullPath <- file.path(CurrentFolder,CurrentInFile)
        
    #__Extract metadata-----
    CurrentMetadata$FileID[iFile] <- FileCounter

    #Try to extract each metatadata variable specified in MetatadataKeyWords
    TextContent <- readLines(FullPath)
    for ( iMD in 1:nrow(MetatadataKeyWords) ) {
      CurrentMetadataVariable <- MetatadataKeyWords$KeyWord[iMD]
      CurrentMetadataVariableShort <- MetatadataKeyWords$KeyWordShort[iMD]
      if ( CurrentMetadataVariable == "FileName" ) next
      
      #Parse  
      Txt <- grep(pattern = paste0(CurrentMetadataVariable,":"), x = TextContent,value = T)
      if ( length(Txt) != 1 ) next
      Txt <- gsub(x=Txt, pattern = paste0(CurrentMetadataVariable,":"),replacement = "")
      Txt <- gsub(x=Txt, pattern = " ",replacement = "")
      #Save
      CurrentMetadata[iFile,CurrentMetadataVariableShort] <- Txt
      
    } #end of loop over metadata-extraction

    
    #Identify date of start of observations
    StartDate <- grep(pattern = "Startdate", x = TextContent,value = T) %>%
      gsub(pattern = "Startdate:\\s*",replacement = "")
    if ( is.na(StartDate) ) stop("is.na(StartDate)")
    if ( nchar(StartDate) != 14 ) {
      stop("Unexpected format for StartDate")
    }
    CurrentMetadata$StartDate[iFile] <- StartDate
    
    

    #FIXME
    # if ( str_sub(string = StartDate, start = 11, end = 14) != "0000" ) {
    #   stop("Startdate is not start at mightnight.")
    # }
    # StartDate <- StartDate %>%
    #   strptime(format = "%Y%m%d%H%M%S") %>%
    #   as.Date() #FIXME keep as DAtetime!
    # if ( is.na(StartDate) ) stop("is.na(StartDate)")
    # CurrentMetadata$StartDate[iFile] <- as.character(StartDate)
    
    
    #Identify date of end of observations
    #Take last row of data table (NumDaysEnd) and add StartDate
    NumDaysEnd <- as.numeric(unlist(strsplit(x=TextContent[length(TextContent)],split = " "))[[2]])
    CurrentMetadata$EndDate[iFile] <- as.character(StartDate + NumDaysEnd)
  
      
    #__Extract data-----
    
    #Identify line where data starts
    LineDataStart <- grep(pattern = "starttime", x = TextContent)
    if ( length(LineDataStart) != 1 ) stop("length(LineDataStart)")
    #Read data
    CurrentData <- read.table(
      file=FullPath,
      header = T,
      skip = LineDataStart-1
    )
    CurrentColnames <- colnames(CurrentData)
    nColnames <- length(CurrentColnames)
    
    #___Identify EMEP data format-----
    DataFormat <- NA
    idx_flag_columns <- which(grepl(x=CurrentColnames,pattern="flag"))
    nFlagCols <- length(idx_flag_columns)
    #Case 1: No quality flags
    if ( length(idx_flag_columns) == 0 ) {
      DataFormat <- "NoQFlags"
    }
    #Case 2: One quality flag for all data columns
    if ( length(idx_flag_columns) == 1 ) {
      DataFormat <- "OneQFlagForAll"
    }
    #Case 3: One quality flag for each data column
    if ( length(idx_flag_columns) > 1 ) {
      #Sanity check
      if ( nColnames != (2 + 2*nFlagCols) ) {
        stop(paste("No way to process data structure for input file",FullPath,"implemented (1)."))
      }
      DataFormat <- "OneQFlagForEach"
    }
    #Else
    if ( is.na(DataFormat) ) {
      stop(paste("No way to process data structure for input file",FullPath,"implemented (2)."))
    }
    
    #___Read data according to format------
    
    #____Case 1: No quality flags-----
    if ( DataFormat == "NoQFlags" ) {
      #No matter how many data columns- gather them to long format
      # stop("1")
      #FIXME
      
      CurrentData <- CurrentData %>%
        mutate(
          date_start = StartDate + starttime,
          date_end = StartDate + endtime
        ) %>%
        select(-starttime,-endtime) %>%
        #Bring from wide to long format
        gather(key=substance,value=value,-date_start,-date_end)         
    }
    
    #____Case 2: One quality flag for all data columns-----
    if ( DataFormat == "OneQFlagForAll" ) {
      
      # stop("2")
      #FIXME
      
      colnames(CurrentData)[idx_flag_columns] <- "Flag"    
      #Sometimes flag values are too low by factor 1000
      if ( any(CurrentData$Flag < 1) ) {
        CurrentData$Flag <- round(CurrentData$Flag*1000,0)
      }
      CurrentData <- CurrentData %>%
        merge(EMEPFlagList,by="Flag",all.x = T)      
      CurrentData <- CurrentData %>%
        filter(Category == "V") %>%
        select(-Flag,-Category,-Description) %>%
        mutate(
          date_start = StartDate + starttime,
          date_end = StartDate + endtime
        ) %>%
        select(-starttime,-endtime) %>%
        #Bring from wide to long format
        gather(key=substance,value=value,-date_start,-date_end)
    }
 
    #____Case 3: One quality flag for each data column--------
    if ( DataFormat == "OneQFlagForEach" ) {
      
      # stop("3")
      #FIXME
      
      tmp1 <- data.frame()
      #Iteratively select combinations of columns starttime,endtime and a value column + flag column combination
      for ( iFlagCol in 1:nFlagCols ) {
        tmp2 <- CurrentData[,c(1,2,idx_flag_columns[iFlagCol]-1,idx_flag_columns[iFlagCol])]
        colnames(tmp2)[4] <- "Flag"       
        tmp2 <- tmp2 %>%
          #Convert dates
          mutate(
            date_start = StartDate + starttime,
            date_end = StartDate + endtime
          ) %>%
          select(-starttime,-endtime) %>%
          #Bring from wide to long format
          gather(key=substance,value=value,-date_start,-date_end,-Flag)              
        tmp1 <- rbind.fill(tmp1,tmp2)            
      } #end of loop over flag columns
      #Sometimes flag values are too low by factor 1000
      if ( any(CurrentData$Flag < 1) ) {
        CurrentData$Flag <- round(CurrentData$Flag*1000,0)
      }      
      #Filter by flag category - all datasets with a flag category "V" = "valid" are ok      
      CurrentData <- tmp1 %>%
        merge(EMEPFlagList,by="Flag",all.x = T)
      CurrentData <- CurrentData %>%
        filter(Category == "V") %>%
        select(substance,value,date_start,date_end)      
    }
    
    #___Append CurrentData to overall data list-----
    #In some cases quality flags indicated invalid measurements for all datasets
    #->Empty data after filtering
    if ( nrow(CurrentData) == 0 ) next
    CurrentData$FileID <- FileCounter
    DataList[[FileCounter]] <- CurrentData
    FileCounter <- FileCounter + 1
  
  } #end of loop over files
  Metadata <- rbind.fill(Metadata,CurrentMetadata)
} #end of loop over folders


#Merge and clean data-----
print("Combining input data to one large data frame...")
Data <- do.call(bind_rows, DataList)


#Some sanity checks
if ( any(is.na(Data$date_start)) ) stop("any(is.na(Data$date_start))")
if ( any(is.na(Data$date_end)) ) stop("any(is.na(Data$date_end))")
if ( any(is.na(Data$substance)) ) stop("any(is.na(Data$substance))")
#In some files data has multiple columns with the same column name (parallel measurements)
#R adds .1 ..2 or similar to produce unique column names.
#After converting form wide to long format, these modifications can be deleted.
#The regular expression used below
#converts all of the following combinations to "Na"
#txt <- c("Na", "Na.", "Na..","Na..1","Na.2","Na..10")
#and converts "NO3."  to NO3 (not a parallel measurement!)
#First delete all digits (one or multiple occurences) following a "."
Data$substance <- gsub(x=Data$substance,pattern = "\\.[0-9]+",replacement = "\\.")
#Then delete one or multiple occurences of "."
Data$substance <- gsub(x=Data$substance,pattern = "\\.+",replacement = "")
#Although quality flags have been used to exclude invalid data, some values are still
#99.999.., 999.9999, 9.99, 9-99999, 9999.9 or similar, indicating missing or invalid data
#The following regular expression matches the pattern of "some number of 9s, dot, some number of 9s".
Data$value[grepl(x=Data$value,pattern=("^9+\\.9+$"))] <- NA
Data <- Data %>%
  drop_na()


#Check coords------
CoordCheck <- Metadata %>%
  select(StationName,lat,lon) %>%
  distinct() %>%
  group_by(StationName) %>%
  summarise(
    nDifferentCoords = n()
  ) %>%
  filter(nDifferentCoords>1)
if ( nrow(CoordCheck) > 0 ) warning("Some plots have different coords in different files.")

#Save to file------
print("Saving metadata to csv...")
write.table(x=Metadata,file = file.path(OutDir,"Parsed_EMEP_Metadata.csv"),sep=";",row.names = F)
print("Saving data to csv...")
write.table(x=Data,file = file.path(OutDir,"Parsed_EMEP_Data.csv"),sep=";",row.names = F)

print(paste(
  "Finished processing",FileCounter,"files from",length(InputFolders),
  "folders, containing",nrow(Data),"measured values from",
  length(unique(Metadata$code_plot)),"stations. Output data contains ",
  nrow(Data),"measurement values."
))
