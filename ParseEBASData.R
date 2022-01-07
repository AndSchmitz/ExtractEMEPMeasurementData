#init-----
rm(list=ls())
graphics.off()
options(warnPartialMatchDollar = T)

library(data.table) #fast I/O
library(tidyverse) #data handling
library(lubridate) #to handle dates

#Define working directory here
WorkDir <- "/path/to/MyWorkDir"
#EMEP data files (.nas files) must be located in one or multiple
#sub-folders in a folder named "Input" in the working directory.
#e.g.:
#.../MyWorkDir/Input/EBASData/EBAS_file_1.nas
#all folders in the "/WorkDir/Input" directory are scanned for ".nas" files,
#i.e. using multiple subfolders is also possible:
#.../MyWorkDir/Input/EBASData/NH4_folder/EBAS_NH4_file_1.nas
#.../MyWorkDir/Input/EBASData/NH4_folder/EBAS_NH4_file_n.nas
#.../MyWorkDir/Input/EBASData/NO3_folder/EBAS_NO3_file_1.nas
#.../MyWorkDir/Input/EBASData/NO3_folder/EBAS_NO3_file_n.nas
#The file "EMEP_data_flags.csv" must be located in the WorkDir/
#Results are placed in a folder names "Output" in .../MyWorkDir/Input
#(or change code for "InDir" and "OutDir" below for a different folder structure)


#Define quality flags to drop
#Rows with quality flags indicating non-valid measurements (i.e. quality flag not in category
#"V") are deleted, with four exceptions relating to low precipitation measurements. See readme.md
#for more information. Here, additional quality flags can be defined which will cause data to
#be discarded if it is flagged with this values.
QualityFlagsToDropData <- c(
  #Explanations according to https://projects.nilu.no//ccc/flags/
  781, 	#V 	Value below detection limit, data element contains detection limit
  780, 	#V 	Value below detection or quantification limit, data element contains estimated or measured value. Use of flag 147 is encouraged.
  771, 	#V 	Value above range, data element contains upper range limit
  770 	#V 	Value above range, data element contains estimated value
  #147 	#V 	Below theoretical detection limit or formal Q/A limit, but a value has been measured and reported and is considered valid
)


# --- No changes required below this line ---



#Prepare I/O
InDir <- file.path(WorkDir,"Input")
OutDir <- file.path(WorkDir,"Output")
dir.create(OutDir,showWarnings = F)
#Folder for skipped files
SkippepFilesDir <- file.path(OutDir,"SkippedFiles")
dir.create(SkippepFilesDir,showWarnings = F)

#Read explanation of EMEP data quality flag values
EMEPFlagList <- read.table(
  file = file.path(InDir,"EMEP_data_flags.csv"),
  sep=";",
  header = T,
  skip = 2,
  colClasses = "character"
)


#Helping function to interpret quality flags------
CheckValuesAreValid <- function(ASingleFlagRow) {
  
  #https://projects.nilu.no//ccc/flags/
  
  #Must be a combination of 3-character-codes:
  StringLength <- nchar(ASingleFlagRow)
  if ( (StringLength %% 3) != 0 ) {
    stop("Number of characters in quality flag must be 3,6,9,...")
  }
  
  #Cut into chunks of 3 characters
  nChunks <- StringLength/3
  VectorOfFlags <- str_sub(
    string = ASingleFlagRow,
    start = seq(from = 1,to = nChunks*3, by = 3 ),
    end = seq(from = 3,to = nChunks*3, by = 3 )
  )
  VectorOfFlags <- unique(VectorOfFlags)
  
  #Drop if any flag is in manually defined (above) list of QualityFlagsToDropData
  if ( any(VectorOfFlags %in% QualityFlagsToDropData) ) {
    return("NotAllValid")
  }
  
  #Compare to EMEP flag list
  if ( !all(VectorOfFlags %in% EMEPFlagList$Flag) ) {
    stop(paste("One of the flag values not in EMEP flag list:",ASingleFlagRow))
  }
  Interpretation <- EMEPFlagList %>%
    filter(
      Flag %in% VectorOfFlags
    ) %>%
    distinct()
  
  #There are four categories of quality flags:
  #V (valid measurement), I (invalid measurement), M (missing measurement) or H (hidden and invalid measurements)
  #https://projects.nilu.no//ccc/flags/
  #Multiple flags can be assigned to each value.
  #If any of the flags is not of category "V", then discard the dataset.
  #Exceptions: The following cases refer to measurements of precipitation and specifically
  #to cases where low or no precipitation occurred, such that chemical analyses
  #was not possible. These cases are no "data gaps" (not: sampling not performed). Thus, these
  #cases are accepted but the value is set to NA and a comment is added.
  # 784	I	Low precipitation, concentration estimated
  # 783	M	Low precipitation, concentration unknown
  # 782	V	Low precipitation, concentration estimated
  # 890	M	Concentration in precipitation undefined, no precipitation
  LowPrecipFlags <- c(784,783, 782, 890)
  if ( any( VectorOfFlags %in% LowPrecipFlags ) ) {
    return("LowPrecipVolume")
  } else if ( !all(Interpretation$Category == "V") ) {
    return("NotAllValid")
  } else {
    return("AllValid")
  }
  
}


#Read EMEP data-----
InputFolders <- list.dirs(path = file.path(InDir),full.names = T, recursive = T)
InputFolders <- InputFolders[!(InputFolders == file.path(InDir))]
# InputFolders <- basename(InputFolders)

#Define which metata variables to extract from files
MetatadataKeyWords <- data.frame(
  KeyWord = c("Station name","Station code","Station latitude","Station longitude","Station altitude","Component","Matrix","Unit","Instrument type"),
  KeyWordShort = c("StationName","code_plot","lat","lon","altitude","component","matrix","unit","InstrumentType"),
  stringsAsFactors = F
)

#_Loop over folders------
DataList <- list()
Metadata <- data.frame()
FileCounter <- 0
SkippedFilesCounter <- 0

for ( CurrentFolder in InputFolders ) {
  print(paste("Processing folder",CurrentFolder,"..."))
  
  #Include only ".nas" files in file list
  InFiles <- list.files(CurrentFolder,pattern = ".nas")
  nInFiles <- length(InFiles)
  if ( nInFiles == 0 ) {
    next
  }
  
  CurrentMetadata <- matrix(NA,nrow=nInFiles,ncol = nrow(MetatadataKeyWords))
  colnames(CurrentMetadata) <- MetatadataKeyWords$KeyWordShort
  CurrentMetadata <- as.data.frame(CurrentMetadata)
  CurrentMetadata$FileID <- NA
  CurrentMetadata$TimeStampFirstMeasurement <- as.POSIXct(NA)
  CurrentMetadata$TimeStampLastMeasurement <- as.POSIXct(NA)
  CurrentMetadata$FileName <- NA
  
  #_Loop over files------
  for ( iFile in 1:nInFiles ) {
    FileCounter <- FileCounter + 1
    CurrentInFile <- InFiles[iFile]
    cat(paste0(round(iFile/nInFiles*100,2),"% of current folder processed. Parsing file ",CurrentInFile,"..."), " \r")
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
    
    #If "matrix" could not be determined, skip file and
    #copy it to a folder called "SkippedFiles". This can happen
    #if data columns refer to different matrices, i.e. PM2.5 and PM10.
    #Currently, the script cannot handle these cases. I.e. the "global metadata"
    #section of each .nas file is taken into account but the local metadata
    #section is not correctly translated.
    CurrentMatrix <- CurrentMetadata$matrix[iFile]
    if ( is.na(CurrentMatrix) | (CurrentMatrix == "") ) {
      file.copy(
        from = FullPath,
        to = file.path(SkippepFilesDir,CurrentInFile)
      )
      CurrentMetadata$matrix[iFile] <- "matrix_NA_file_skipped"
      SkippedFilesCounter <- SkippedFilesCounter + 1
      next
    }
    
    
    #Identify timestamp of start of observations
    ReferenceTimeStamp <- grep(pattern = "Startdate", x = TextContent,value = T) %>%
      gsub(pattern = "Startdate:\\s*",replacement = "")
    if ( is.na(ReferenceTimeStamp) ) stop("is.na(ReferenceTimeStamp)")
    if ( nchar(ReferenceTimeStamp) != 14 ) {
      stop("Unexpected format for ReferenceTimeStamp")
    }
    ReferenceTimeStamp <- as.POSIXct(x=ReferenceTimeStamp,format="%Y%m%d%H%M%S")
    if ( is.na(ReferenceTimeStamp) ) stop("is.na(ReferenceTimeStamp)")
    
    
    #__Extract data-----
    
    #Identify line where data starts
    LineDataStart <- grep(pattern = "starttime", x = TextContent)
    if ( length(LineDataStart) != 1 ) stop("length(LineDataStart)")
    #Read data
    CurrentData <- read.table(
      file=FullPath,
      header = T,
      skip = LineDataStart-1,
      colClasses = "character"
    )
    CurrentColnames <- colnames(CurrentData)
    nColnames <- length(CurrentColnames)
    
    if ( !all(c("starttime","endtime") %in% CurrentColnames) ) {
      stop("!(c(starttime,endtime) %in% CurrentColnames)")
    }
    if ( nrow(CurrentData) == 0 ) {
      stop("Empty file")
    }
    
    
    #___Treat date and time-----
    #Start and end time:
    #Time is always provided in "days" "time has to be real (we use "days since" reference date)."
    #Presentation "EBAS Data format" Technical workshop on data quality and data reporting to
    #EBAS October 26 - 28th 2016, Paul Eckhardt, ATMOS, NILU
    #ddays(): https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/duration
    tmp <- CurrentData %>%
      mutate(
        TimeStampStart = ReferenceTimeStamp + ddays(as.numeric(starttime)),
        TimeStampEnd = ReferenceTimeStamp + ddays(as.numeric(endtime))
      ) %>%
      select(-starttime,-endtime)
    if ( any(is.na(tmp[,c("TimeStampStart","TimeStampEnd")])) ) {
      stop("Error with start/end timestamps")
    }
    
    
    #___Bring data in long format------
    
    #"A flag variable always follows the data variable(s) it applies to. When a flag variable
    #applies to more then one data variables, the data variables must be in sequence,
    #directly followed by the applicable flag variable. (With other words: A flag variable
    #applies to all data variables between the previous flag variable and the current
    #one.)"
    #Presentation "EBAS Data format" Technical workshop on data quality and data reporting to
    #EBAS October 26 - 28th 2016, Paul Eckhardt, ATMOS, NILU
    
    #Now timestamp columns are the last two columns
    nColTreat <- ncol(tmp) - 2
    idxFlagCols <- which(grepl(x=colnames(tmp),pattern="flag"))
    #There must be a flag column
    if ( length(idxFlagCols) == 0 ) { 
      #However, this is not always true
      #Add a Flag column at the end, indicating all values are OK (optimistic approach)
      #Ensure timestamp columns are still the last two columns
      tmp2 <- (tmp[,1:nColTreat])
      if ( nColTreat == 1 ) {
        tmp2 <- data.frame(tmp[,1:nColTreat],stringsAsFactors = F)
        colnames(tmp2) <- colnames(tmp)[1]
      }
      tmp2 <- bind_cols(tmp2,flag = rep("000",times = nrow(tmp)))
      tmp <- bind_cols(tmp2,tmp[,c(nColTreat+1,nColTreat+2)])
      nColTreat <- ncol(tmp) - 2
      idxFlagCols <- which(grepl(x=colnames(tmp),pattern="flag"))
    }
    #The last column to treat must be a flag column
    if ( !(nColTreat %in% idxFlagCols) ) stop(
      stop("The last column to treat must be a flag column")  
    )
    #The first column must not be a flag column 
    if ( (1 %in% idxFlagCols) ) stop(
      stop("The first column must not be a flag column ")  
    )
    #Two flag columns must not come directly adjacent
    if (  ( length(idxFlagCols) > 1 ) & (any(diff(idxFlagCols) == 1)) ) {
      stop("Two flag columns must not come directly adjacent")
    }
    #Extract each data column together with start and end timestamps and flag column.
    #Append this to a dataframe containing the current file content in long format
    CurrentDataLong <- data.frame()
    for ( iCol in 1:nColTreat ) {
      if ( iCol %in% idxFlagCols ) {
        next
      }
      #This is a data column. Identify corresponding flag column
      CorrespondingFlagCol <- min(idxFlagCols[idxFlagCols > iCol])
      #Take the four relevant columns and append to long format data
      Extracted <- tmp[,c(nColTreat+1,nColTreat+2,iCol,CorrespondingFlagCol)]
      colnames(Extracted)[3] <- "value"
      colnames(Extracted)[4] <- "Flag"
      Extracted$substance <- colnames(tmp)[iCol]
      CurrentDataLong <- bind_rows(CurrentDataLong,Extracted)
    }
    
    
    #___Treat quality flags------
    if ( ncol(CurrentDataLong) != 5 ) {
      stop("ncol(CurrentDataLong) != 5")
    }
    #Sanity-check quality flag data format
    #https://projects.nilu.no//ccc/flags/
    #Must start with "0." according to data format description. But not the case for all.
    #Convert if necessary
    if ( all(str_sub(string = CurrentDataLong$Flag, start = 1, end = 2) == "0.") ) {
      CurrentDataLong$Flag <- gsub(x = CurrentDataLong$Flag, pattern = "^0.",replacement = "")
    }
    #See function CheckValuesAreValid() for information which datasets are discarded / accepted.
    CurrentDataLong$FlagStatus <- sapply(X = CurrentDataLong$Flag, FUN = CheckValuesAreValid)
    CurrentDataLong <- CurrentDataLong %>%
      #Drop rows where flags indicate problems (but keep flags indicating low precip volume)
      filter(
        FlagStatus != "NotAllValid"
      ) %>%
      #Treat rows where flags indicate low precip volume
      mutate(
        comment = case_when(
          FlagStatus == "LowPrecipVolume" ~ "LowPrecipVolume",
          T ~ NA_character_
        ),
        value = case_when(
          FlagStatus == "LowPrecipVolume" ~ NA_character_,
          T ~ value
        )
      ) 
    CurrentDataLong <- CurrentDataLong %>%
      select(-FlagStatus,-Flag)
    
    #Skip if no valid data left
    if ( nrow(CurrentDataLong) == 0 ) {
      next
    }
    
    
    #___Append CurrentData to overall data list-----
    CurrentDataLong$FileID <- FileCounter
    DataList[[length(DataList)+1]] <- CurrentDataLong
    
    
    #___Store temporal range of observations in metadata----
    if ( any(is.na(min(CurrentDataLong$TimeStampStart))) ) stop("if ( any(is.na(min(CurrentDataLong$TimeStampStart))) )")
    if ( any(is.na(max(CurrentDataLong$TimeStampStart))) ) stop("if ( any(is.na(max(CurrentDataLong$TimeStampStart))) )")
    CurrentMetadata$TimeStampFirstMeasurement[iFile] <- min(CurrentDataLong$TimeStampStart)
    CurrentMetadata$TimeStampLastMeasurement[iFile] <- max(CurrentDataLong$TimeStampStart)
    
  } #__end of loop over files------
  
  #_Append metadata for current folder to list-----
  Metadata <- bind_rows(Metadata,CurrentMetadata)
  if ( any(duplicated(Metadata$FileID)) ) {
    stop("FileID must be unique in Metadata dataframe.")
  }
} #end of loop over folders


#Merge and clean data-----
print("---")
print("Loop over files and folders finished.")
print("---")
print("Combining input data to one large data frame...")
Data <- do.call(bind_rows, DataList)


#Some sanity checks
if ( any(is.na(Data$TimeStampStart)) ) stop("any(is.na(Data$TimeStampStart))")
if ( any(is.na(Data$TimeStampEnd)) ) stop("any(is.na(Data$TimeStampEnd))")
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
idxDatasets999 <- which(grepl(x=Data$value,pattern=("^9+\\.9+$")))
if ( length(idxDatasets999) > 0 ) {
  Datasets999 <- Data[idxDatasets999,]
  nDatasets999 <- length(idxDatasets999)
  print(paste("Despite filtering for quality flags, there are",nDatasets999,"records where the value is 9.999, 9.9, 999.99, etc. This is",round(nDatasets999/nrow(Data)*100,2),"% of the data."))
  print("The exact values are (only one of occurrence of each exact value):")
  print(unique(Datasets999$value))
  print("These datasets are deleted. Even if this affects some valid data, most of the deleted records probably represent missing values.")
  Data <- Data[-idxDatasets999,]
}


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
fwrite(
  x = Metadata,
  file = file.path(OutDir,"Parsed_EMEP_Metadata.csv"),
  sep=";"
)
print("Saving data to csv...")
fwrite(
  x = Data,
  file = file.path(OutDir,"Parsed_EMEP_Data.csv"),
  sep=";"
)

print(paste(
  "Skipped",SkippedFilesCounter,"file(s) because it contains data from different matrices (e.g. PM2.5 and PM10).",
  "Processing these files is not yet implemented. All skipped files have been copied to output folder \"SkippedFiles\"."
))

print(paste(
  "Finished processing",FileCounter,"files from",length(InputFolders),
  "folders, containing",nrow(Data),"measured values from",
  length(unique(Metadata$code_plot)),"stations."
))
