uploadTrip <- function(cruise.code = "missing",cruise.tab,haul.tab,
                        bulk.tab,sh.tab,sample.tab,ext,SQLServer,DBName){

#prompt for cruise code, if not already supplied
if (missing(cruise.code)){
  cruise.code <- winDialogString("Cruise Code","")
}

if(nchar(cruise.code)==0) stop("Exiting, no cruise code supplied")

#count records in database for the trip - should all be zero to proceed
record.count <- cruiseDetails(cruise.code,SQLServer=SQLServer,DB=DBName)

if(sum(record.count[1,])>0) {
  cat(record.count$Cruise,"Cruise record\n")
  cat(record.count$Haul,"Haul record(s)\n")
  cat(record.count$Bulk,"Bulk record(s)\n")
  cat(record.count$SampleHeader,"Sample header record(s)\n")
  cat(record.count$Sample,"Sample record(s)\n")
  stop("Data already exists in the database for this cruise")
}

#construct path to data files
file.dir <- paste0(".//Data//",gsub("/","_",cruise.code),"//")

cat("file.dir",file.dir,"\n")

#Cruise data
cruise.dat <- getCruiseFromExcel(file.dir,cruise.code,cruise.tab,ext)
cat(nrow(cruise.dat),"Cruise records read in\n")
#return(cruise.dat)
#Haul data
haul.dat <- getHaulFromExcel(file.dir,cruise.code,haul.tab,ext)
cat(nrow(haul.dat),"Haul records read in\n")
#Landings data
bulk.dat <- getBulkFromExcel(file.dir,cruise.code,bulk.tab,ext)
cat(nrow(bulk.dat),"Landings records read in\n")
#Sample Headers
sh.dat <- getSampleHeaderFromExcel(file.dir,cruise.code,sh.tab,ext)
cat(nrow(sh.dat),"Sample header records read in\n")
#Samples
samp.dat <- getSampleFromExcel(file.dir,cruise.code,sample.tab,ext)
cat(nrow(samp.dat),"Sample records read in\n")

#read in meta data
metaData <- getMetaData(SQLServer,DBName)

#VALIDATION SECTION
#validate the cruise record
validCruise <- validCruise(cruise.dat,
                           metaData$personnel.MD,
                           metaData$vessels.MD,
                           metaData$ports.MD,
                           metaData$type.MD)

#return(validCruise)

if (sum(!is.na(validCruise$msg))>0) {
  cat("Invalid Cruise Record\n")
  print(validCruise[which(!is.na(validCruise$msg)),])
  stop()
} else {
  cat(paste0(nrow(validCruise), " Cruise records validated\n"))  
}

validHaul <- as.data.frame(do.call(rbind,
                                   apply(haul.dat,1,validHaul,
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
                                            apply(sh.dat,1,validSampleHeader,
                                                  condition=metaData$condition.MD,
                                                  type=metaData$fishtype.MD,
                                                  grades=metaData$grade.MD,
                                                  datatype=metaData$datatype.MD,
                                                  landings=validLandings,
                                                  hauls=validHaul,
                                                  cruise=validCruise)))

#return(validSampleHeaders)

if (sum(!is.na(validSampleHeaders$msg))>0) {
  cat("Invalid Sample Header Record(s)\n")
  print(validSampleHeaders[which(!is.na(validSampleHeaders$msg)),])
  stop()
} else {
  cat(paste0(nrow(validSampleHeaders), " Sample header records validated\n"))
}

validSamples <- as.data.frame(do.call(rbind,
                                      apply(samp.dat,1,validSample,
                                            species=metaData$species.MD,
                                            sampleheaders=validSampleHeaders,
                                            hauls=validHaul,
                                            cruise=validCruise)))

if (sum(!is.na(validSamples$msg))>0) {
  cat("Invalid Sample Records\n")
  print(validSamples[which(!is.na(validSamples$msg)),])
  stop()
} else {
  cat(paste0(nrow(validSamples), " Sample records validated\n"))
}

cat("Uploading data...\n")

#DATA UPLOAD SECTION

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
 
#assign appropriate sample header ids into sample records

#merge the new ids into the validSampleHeader 
validSampleHeaders_id<-merge(validSampleHeaders,sh_id,by.x=c("HaulCode","SampleNumber"),by.y=c("Haul code","Sample Number"))
 
#don't want columns 'Cruise code','msg' in sample header df
drops <- c("Cruise code","msg","Quantity","Units","Condition","Type","NFD","Grade_id","GradeDescription","DataType","SampleID")
validSampleHeaders_id <- validSampleHeaders_id[,!(names(validSampleHeaders_id) %in% drops)]

#don't want column 'msg' in sample df
drops <- c("msg")
validSamples <- validSamples[,!(names(validSamples) %in% drops)]

validSamples_id<-merge(validSamples,
                       validSampleHeaders_id,
                       by.x=c("CruiseCode","HaulCode","SampleNumber"),
                       by.y=c("CruiseCode","HaulCode","SampleNumber"))


cat("Uploading sample data...\n")
 
#upload Sample
samp_id <- apply(validSamples_id,1,uploadSample,Server=SQLServer,DB=DBName)
 
#count records uploaded
uploaded.count <- cruiseDetails(cruise.code,SQLServer=SQLServer,DB=DBName)
 
cat(paste0(uploaded.count$Cruise," cruise records of ",nrow(validCruise)," uploaded\n"))
cat(paste0(uploaded.count$Haul," haul records of ",nrow(validHaul)," uploaded\n"))
cat(paste0(uploaded.count$Bulk," landings records of ",nrow(validLandings)," uploaded\n"))
cat(paste0(uploaded.count$SampleHeader," sample header records of ",nrow(validSampleHeaders)," uploaded\n"))
cat(paste0(uploaded.count$Sample," sample records of ",nrow(validSamples)," uploaded\n"))

#return success
return(1)

}
