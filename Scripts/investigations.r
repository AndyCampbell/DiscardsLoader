#investigations

#before reading excel files, a registry setting needs to be updated:
#MyComputer -> HKEY_LOCAL_MACHINE -> SOFTWARE -> Microsoft -> Jet -> 4.0 -> Engines -> Excel-> TypeGuessRows set to 0

rm(list=ls())
library(RODBC)

source('MetaDataValidationFunctions.r')
source('DBObjectValidationFunctions.r')
source('DBfunctions.r')
source('ExcelFunctions.R')

SQLServer = 'VMFSSDEV01'
DBName = 'FEAS_DemDiscards'

#prompt for cruise code
cruise.code <- winDialogString("Cruise Code","")

#count records in database for the trip - should all be zero to proceed
record.count <- cruiseDetails(cruise.code,SQLServer=SQLServer,DB=DBName)

#if(sum(record.count[1,])>0) {
#  stop("Data already exists in the database for this cruise")
#}

#construct path to data files
file.dir <- paste0(".//Data//",gsub("/","",cruise.code),"//")

#Cruise data
cruise.dat <- getCruiseFromExcel(file.dir,cruise.code)





#Haul Data
file.name <- ".//Data//Test Data//TSTTST1215//TSTTST1215h.xls"
sheet.name <- "L_output_haul"

excel.connect <- odbcConnectExcel(file.name)
haul.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","-"),stringsAsFactors=FALSE)
odbcClose(excel.connect)

#Bulk (landings) Data
file.name <- ".//Data//Test Data//TSTTST1215//TSTTST1215b.xls"
sheet.name <- "L_output_bulk"

excel.connect <- odbcConnectExcel(file.name)
bulk.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
odbcClose(excel.connect)

#Sample Header Data
file.name <- ".//Data//Test Data//TSTTST1215//TSTTST1215sh.xls"
sheet.name <- "L_output_sample-header"

excel.connect <- odbcConnectExcel(file.name)
sh.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
odbcClose(excel.connect)

#remove spaces from field names
names(sh.dat)<-gsub(" ","",names(sh.dat))

#Sample Data
file.name <- ".//Data//Test Data//TSTTST1215//TSTTST1215s.xls"
sheet.name <- "L_output_sample"

excel.connect <- odbcConnectExcel(file.name)
samples.dat <- sqlFetch(excel.connect,sheet.name,na.string=c("","_"),stringsAsFactors=FALSE)
odbcClose(excel.connect)

#remove spaces from field names
names(samples.dat) <- gsub(" ","",names(samples.dat))

samples.dat[1:40,]



#meta data

SQL.connect <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV01; Database=FEAS_DemDiscards")
personnel.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSPersonnel",stringsAsFactors=FALSE)
personnel.MD$eff_from <- as.Date(personnel.MD$eff_from)
personnel.MD$eff_to <- as.Date(personnel.MD$eff_to)

ports.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSPort",stringsAsFactors=FALSE)

vessels.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSVessel",stringsAsFactors=FALSE)
vessels.MD$eff_from <- as.Date(vessels.MD$eff_from)
vessels.MD$eff_to <- as.Date(vessels.MD$eff_to)

ices.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSICES",stringsAsFactors=FALSE)

gear.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSGear",stringsAsFactors=FALSE)

type.MD <- sqlQuery(SQL.connect,"Select * from dbo.CRUISETYPE",stringsAsFactors=FALSE)

winddir.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSWindDir",stringsAsFactors=FALSE)

windfor.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSWindForce",stringsAsFactors=FALSE)

seastate.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSeaState",stringsAsFactors=FALSE)

swelldir.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSwellDir",stringsAsFactors=FALSE)

swell.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSwell",stringsAsFactors=FALSE)

groundtype.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSGroundType",stringsAsFactors=FALSE)

fishground.MD <- sqlQuery(SQL.connect,"Select * from dbo.FISHING_GROUND", stringsAsFactors=FALSE)
names(fishground.MD) <- gsub(" ","_",names(fishground.MD))

species.MD <- sqlQuery(SQL.connect,"Select * from dbo.SpeciesLink",stringsAsFactors=FALSE)

condition.MD <- sqlQuery(SQL.connect,"Select * from dbo.[Fish Condition]",stringsAsFactors=FALSE)
names(condition.MD) <- gsub(" ","_",names(condition.MD))

fishtype.MD <- sqlQuery(SQL.connect,"Select * from dbo.[Fish Type]", stringsAsFactors=FALSE)
names(fishtype.MD) <- gsub(" ","",names(fishtype.MD))

presfactors.MD <- sqlQuery(SQL.connect,"Select * from dbo.PresentationFactorLookups", stringsAsFactors=FALSE)

grade.MD <- sqlQuery(SQL.connect,"Select * from dbo.GradeLUT",stringsAsFactors=FALSE)

datatype.MD <- sqlQuery(SQL.connect,"Select * from dbo.DATATYPE",stringsAsFactors=FALSE)
names(datatype.MD) <- gsub(" ","",names(datatype.MD))

odbcClose(SQL.connect)


validCruise <- validCruise(cruise.dat,personnel.MD,vessels.MD,ports.MD,type.MD)
validCruise

validHaul <- as.data.frame(do.call(rbind,
                                   apply(haul.dat,1,validHaul,ices=ices.MD,gear=gear.MD,
                                         winddir=winddir.MD,windfor=windfor.MD,seastate=seastate.MD,
                                         swelldir=swelldir.MD,swell=swell.MD,groundtype=groundtype.MD,
                                         ground=fishground.MD,cruise=validCruise)))

#validHaul$msg

validLandings <- as.data.frame(do.call(rbind,
                                       apply(bulk.dat,1,validLanding,species=species.MD,
                                             state=condition.MD,factor=presfactors.MD,type=fishtype.MD,
                                             grades=grade.MD,hauls=validHaul,cruise=validCruise)))

#validLandings$msg

validSampleHeaders <- as.data.frame(do.call(rbind,
                                            apply(sh.dat,1,validSampleHeader,condition=condition.MD,
                                                  type=fishtype.MD,grades=grade.MD,datatype=datatype.MD,
                                                  landings=validLandings,hauls=validHaul,cruise=validCruise)))
#validSampleHeaders

validSamples <- as.data.frame(do.call(rbind,
                                      apply(samples.dat,1,validSample,species=species.MD,
                                            sampleheaders=validSampleHeaders)))


cruise_id <- uploadCruise(validCruise,SQLServer,DBName)
#cruise_id

haul_id <- as.data.frame(do.call(rbind,
                                 apply(validHaul,1,uploadHaul,Server=SQLServer,DB=DBName)))
#haul_id

landing_id <- as.data.frame(do.call(rbind,
                                    apply(validLandings,1,uploadLanding,Server=SQLServer,DB=DBName)))
#landing_id

sh_id <- as.data.frame(do.call(rbind,
                               apply(validSampleHeaders,1,uploadSampleHeader,Server=SQLServer,DB=DBName)))
#sh_id

#assign appropriate sample header ids into sample records
#merge the new ids into the validSampleHeader 
validSampleHeaders_id<-merge(validSampleHeaders,sh_id,by.x=c("HaulCode","SampleNumber"),by.y=c("Haul code","Sample Number"))

#match the sample header id to the sample
validSamples_id<-merge(validSamples,
                       validSampleHeaders_id,
                       by.x=c("SampleHeaderID"),
                       by.y=c("SampleID"))

#append the sample header if to the original sample data frame (don't want all the extra columns returned by the merge)
validSamples$sample_header_id <- as.vector(unlist(validSamples_id['sample_header_id']))

#upload Sample
#samp_id <- as.data.frame(do.call(rbind,
#                                 apply(validSamples,1,uploadSample,Server=SQLServer,DB=DBName)))
samp_id <- apply(validSamples,1,uploadSample,Server=SQLServer,DB=DBName)

#samp_id

#count records uploaded
uploaded.count <- cruiseDetails('TST/TST/12/15',SQLServer=SQLServer,DB=DBName)

cat(paste0(uploaded.count$Cruise," cruise records of ",nrow(validCruise)," uploaded\n"))
cat(paste0(uploaded.count$Haul," haul records of ",nrow(validHaul)," uploaded\n"))
cat(paste0(uploaded.count$Bulk," landings records of ",nrow(validLandings)," uploaded\n"))
cat(paste0(uploaded.count$SampleHeader," sample header records of ",nrow(validSampleHeaders)," uploaded\n"))
cat(paste0(uploaded.count$Sample," sample records of ",nrow(validSamples)," uploaded\n"))


