#ExcelV3 sheet development code

library(RODBC)
options(java.parameters = "-Xmx1024m")
library(XLConnect)

rm(list=ls())
gc()

#sourceDir <- "C:/Dev/R/DiscardsLoader/Source"
sourceDir <- "//galwayFS03/fishdata/Discards/Discards Excel For Upload/Source"

source(paste(sourceDir,"/MetaDataValidationFunctions.r",sep=""))
source(paste(sourceDir,"/DBObjectValidationFunctions.r",sep=""))
source(paste(sourceDir,"/DBfunctions.r",sep=""))
source(paste(sourceDir,"/ExcelFunctions.r",sep=""))

#cruise.code <- "FAT/DMR/14/4" #uploaded to tst & live DB 07/11/2014
#cruise.code <- "FAT/CTB/14/10" #uploaded to tst & live DB 07/11/2014
#cruise.code <- "FAT/CHD/14/18" #uploaded to tst & live DB 07/11/2014
#cruise.code <- "FAT/KBG/14/9" #uploaded to tst & live DB 07/11/2014
#cruise.code <- "FAT/ROS/15/1" #uploaded to tst 23/07/2015, uploaded to live DB 30/07/2015
#cruise.code <- "FAT/ROS/15/3" #uploaded to tst 27/07/2015, uploaded to live DB 30/07/2015
#cruise.code <- "FAT/CTB/15/2" #uploaded to tst 30/07/2015, uploaded to live DB 30/07/2015
#cruise.code <- "FAT/CTB/15/3" #uploaded to tst 23/07/2015, uploaded to live DB 30/07/2015
#cruise.code <- "FAT/KBG/15/13" #uploaded to tst 23/07/2015, uploaded to live DB 30/07/2015
#cruise.code <- "FAT/CHD/15/8" #uploaded to tst 29/02/2016, uploaded to live DB 29/02/2016
#cruise.code <- "FAT/CHD/14/18"

#19th August 2016
#cruise.code <- "FAT/ROS/16/3"  #uploaded to tst 19/08/2016, uploaded to live DB 19/08/2016
#cruise.code <- "FAT/ROS/16/2"   #uploaded to tst 28/11/2016, uploaded to live DB 28/11/2016
cruise.code <- "FAT/ROS/16/7"   #uploaded to tst 28/11/2016

cruise.tab <- "Cruise"
hauls.tab <- "Hauls"
bulk.tab <- "Landings"
sampleheader.tab <- "Sample Headers"
mosample.tab <- "MO Samples"
agedsample.tab <- "Aged Samples"
ext <- ".xlsm"

#construct path to data files
file.dir <- paste0(".//Data//",gsub("/","_",cruise.code),"//")
file.name <- paste0(file.dir,gsub("/","_",cruise.code),ext)  

#check file exists
if (file.exists(file.name)) {
  
  #load the workbook
  wb <- loadWorkbook(file.name, create=FALSE)
  
  #Cruise data
  cruise.dat <- getCruiseFromExcelV3(cruise.code,cruise.tab,ext,wb)
  #Hauls data
  hauls.dat <- getHaulsFromExcelV3(cruise.code,hauls.tab,ext,wb)
  #Landings Data
  bulk.dat <- getBulkFromExcelV3(cruise.code,bulk.tab,ext,wb)
  #Sample Headers
  sampleheader.dat <- getSampleHeaderFromExcelV3(cruise.code,sampleheader.tab,ext,wb)
  #Measured only samples
  MOsample.dat <- getMOSampleFromExcelV3(cruise.code,mosample.tab,ext,wb)
  #aged samples
  agedsample.dat <- getAgedSampleFromExcelV3(cruise.code,agedsample.tab,ext,wb)
  
  #SQLServer = 'VMFSSTST02'
  SQLServer = 'VMFSSSQL01'
  DBName = 'FEAS_DemDiscards'

  #read in meta data
  metaData <- getMetaData(SQLServer,DBName)
  
  validCruise <- validCruise(cruise.dat,
                             metaData$personnel.MD,
                             metaData$vessels.MD,
                             metaData$ports.MD,
                             metaData$type.MD)
  
  if (sum(!is.na(validCruise$msg))>0) {
    cat("Invalid Cruise Record\n")
    print(validCruise[which(!is.na(validCruise$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validCruise), " Cruise records validated\n"))  
  }
  
  validHaul <- as.data.frame(do.call(rbind,
                                     apply(hauls.dat,1,validHaul,
                                           ices = metaData$ices.MD,
                                           gear = metaData$gear.MD,
                                           winddir = metaData$winddir.MD,
                                           windfor = metaData$windfor.MD,
                                           seastate = metaData$seastate.MD,
                                           swelldir = metaData$swelldir.MD,
                                           swell = metaData$swell.MD,
                                           groundtype = metaData$groundtype.MD,
                                           ground = metaData$fishground.MD,
                                           success = metaData$successCode.MD,
                                           cruise = validCruise)))
  
  if (sum(!is.na(validHaul$msg))>0) {
    cat("Invalid Haul Record(s)\n")
    print(validHaul[which(!is.na(validHaul$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validHaul), " Haul records validated\n"))
  }
  
  validLandings <- as.data.frame(do.call(rbind,
                                         apply(bulk.dat,1,validLanding,
                                               species=metaData$species.MD,
                                               state=metaData$condition.MD,
                                               factor=metaData$presfactors.MD,
                                               type=metaData$fishtype.MD,
                                               grades=metaData$grade.MD,
                                               hauls=validHaul,cruise=validCruise)))
  
  if (sum(!is.na(validLandings$msg))>0) {
    cat("Invalid Landings Record(s)\n")
    print(validLandings[which(!is.na(validLandings$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validLandings), " Landings records validated\n"))
  }

  validSampleHeaders <- as.data.frame(do.call(rbind,
                                              apply(sampleheader.dat,1,validSampleHeader,
                                                    condition=metaData$condition.MD,
                                                    type=metaData$fishtype.MD,
                                                    grades=metaData$grade.MD,
                                                    datatype=metaData$datatype.MD,
                                                    landings=validLandings,
                                                    hauls=validHaul,
                                                    cruise=validCruise)))
  
  if (sum(!is.na(validSampleHeaders$msg))>0) {
    cat("Invalid Sample Header Record(s)\n")
    print(validSampleHeaders[which(!is.na(validSampleHeaders$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validSampleHeaders), " Sample header records validated\n"))
  }
  
  validMOSamples <- as.data.frame(do.call(rbind,
                                        apply(MOsample.dat,1,validMOSample,
                                              species=metaData$species.MD,
                                              sampleheaders=validSampleHeaders,
                                              hauls=validHaul,
                                              cruise=validCruise)))
  
  if (sum(!is.na(validMOSamples$msg))>0) {
    cat("Invalid Measured Only Sample Records\n")
    print(validMOSamples[which(!is.na(validMOSamples$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validMOSamples), " Measured Only Sample records validated\n"))
  }

  validAgedSamples <- as.data.frame(do.call(rbind,
                                          apply(agedsample.dat,1,validAgedSample,
                                                species=metaData$species.MD,
                                                sampleheaders=validSampleHeaders,
                                                hauls=validHaul,
                                                cruise=validCruise)))

  if (sum(!is.na(validAgedSamples$msg))>0) {
    cat("Invalid Aged Sample Records\n")
    print(validAgedSamples[which(!is.na(validAgedSamples$msg)),])
    stop()
  } else {
    cat(paste0(nrow(validAgedSamples), " Aged Sample records validated\n"))
  }
  
} else {
  stop(paste0("Cannot find file",file.name))
}


#report of landings records with mismatched condition factors
validLandings[!(validLandings$Factor==validLandings$SuppliedFactor) | is.na(validLandings$SuppliedFactor),]


cat("Uploading cruise data...\n")
cruise_id <- uploadCruise(validCruise,SQLServer,DBName)

cat("Uploading haul data...\n")
haul_id <- as.data.frame(do.call(rbind,
                                 apply(validHaul,1,uploadHaul,Server=SQLServer,DB=DBName)))

cat("Uploading landings data...\n")
landing_id <- as.data.frame(do.call(rbind,
                                    apply(validLandings,1,uploadLanding,Server=SQLServer,DB=DBName)))

cat("Uploading sample header data...\n")
sh_id <- as.data.frame(do.call(rbind,
                               apply(validSampleHeaders,1,uploadSampleHeader,Server=SQLServer,DB=DBName)))



#TO DO - insert a check here that haulcode and samplenumber combination is unique

#merge the new ids into the validSampleHeader 
validSampleHeaders_id<-merge(validSampleHeaders,sh_id,by.x=c("HaulCode","SampleNumber"),by.y=c("Haul code","Sample Number"))

#don't want columns 'Cruise code','msg' in sample header df
drops <- c("Cruise code","msg","Quantity","Units","Condition","Type","NFD","Grade_id","GradeDescription","DataType","SampleID")
validSampleHeaders_id <- validSampleHeaders_id[,!(names(validSampleHeaders_id) %in% drops)]

#don't want column 'msg' in sample df
drops <- c("msg")
validMOSamples <- validMOSamples[,!(names(validMOSamples) %in% drops)]
validAgedSamples <- validAgedSamples[,!(names(validAgedSamples) %in% drops)]
  
validMOSamples_id<-merge(validMOSamples,
                        validSampleHeaders_id,
                        by.x=c("CruiseCode","HaulCode","SampleNumber"),
                        by.y=c("CruiseCode","HaulCode","SampleNumber"))

validAgedSamples_id<-merge(validAgedSamples,
                         validSampleHeaders_id,
                         by.x=c("CruiseCode","HaulCode","SampleNumber"),
                         by.y=c("CruiseCode","HaulCode","SampleNumber"))

cat("Uploading measured only sample data...\n")

#upload measured only samples
MOsamp_id <- apply(validMOSamples_id,1,uploadSample,Server=SQLServer,DB=DBName)

if (nrow(validAgedSamples_id)>0) {
  cat("Uploading",nrow(validAgedSamples_id),"aged sample data...\n")
  #upload Aged Samples
  Agedsamp_id <- apply(validAgedSamples_id,1,uploadSample,Server=SQLServer,DB=DBName)
} else {
  cat("Zero valid aged samples to upload\n")
}
