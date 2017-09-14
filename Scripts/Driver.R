#Discard Upload Driver script
#Andrew Campbell
#FEAS, Marine Institute

#05/03/2014 - initial development

#updated November 2014 to incorporate changes to upload spreadsheet
#moved to H:\Dev\R\DiscardsLoader


#uploads trips to discard DB from xlsx spreadsheet

#clean up
rm(list=ls())
gc()

#ODBC library for excel/database connectivity
library(RODBC)

#source dir, should be same as wd
#sourceDir <- "H:/TFS_Projects/Database Dev/Discard Dev/R Upload"
sourceDir <- "C:/Dev/R/DiscardsLoader/Source"

#test DB
SQLServer = 'VMFSSTST01'
#live DB
#SQLServer = 'VMFSSSQL01'
DBName = 'FEAS_DemDiscards'

source(paste(sourceDir,"/MetaDataValidationFunctions.r",sep=""))
source(paste(sourceDir,"/DBObjectValidationFunctions.r",sep=""))
source(paste(sourceDir,"/DBfunctions.r",sep=""))
source(paste(sourceDir,"/ExcelFunctions.r",sep=""))
source(paste(sourceDir,"/UploadDiscardTrip.r",sep=""))

#ret<-uploadTrip("FAT/ROS/13/9",cruise.tab="Cruise",haul.tab="Hauls",
#                 bulk.tab="Landings",sh.tab="Sample Headers",
#                 sample.tab="Samples",ext=".xlsx",SQLServer,DBName)

#ret<-uploadTrip("FAT/ROS/13/1",cruise.tab="Cruise",haul.tab="Hauls",
#                 bulk.tab="Landings",sh.tab="Sample Headers",
#                 sample.tab="Samples",ext=".xlsx",SQLServer,DBName)

#ret<-uploadTrip("FAT/CTB/13/4",cruise.tab="Cruise",haul.tab="Hauls",
#                 bulk.tab="Landings",sh.tab="Sample Headers",
#                 sample.tab="Samples",ext=".xlsx",SQLServer,DBName)

#ret<-uploadTrip("FAT/CHD/13/18",cruise.tab="Cruise",haul.tab="Hauls",
#                 bulk.tab="Landings",sh.tab="Sample Headers",
#                 sample.tab="Samples",ext=".xlsx",SQLServer,DBName)

