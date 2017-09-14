#Excel functions for Discard uploads

getCruiseFromExcel <- function(path,cruise,cruise.tab,ext){
  
  file.name <- paste0(path,gsub("/","_",cruise),ext)  
  sheet.name <- cruise.tab

  
  cat("file.name",file.name,"\n")
  
  #check file exists
  if (file.exists(file.name)) {
    
    cat("file exists\n")
    
    excel.connect <- odbcConnectExcel2007(file.name)
    cruise.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","-"),stringsAsFactors=FALSE)
    #only need the first row
    cruise.dat <- cruise.dat[1,]
    
    #TO DO - should really check that there is only a single row of data
    #cast dates to Date type
    cruise.dat[,'Departure Date'] <- as.Date(strptime(cruise.dat[,'Departure Date'],format="%d/%m/%Y"))
    cruise.dat[,'Completion Date'] <- as.Date(strptime(cruise.dat[,'Completion Date'],format="%d/%m/%Y"))
    cruise.dat[,'Date Received'] <- as.Date(strptime(cruise.dat[,'Date Received'],format="%d/%m/%Y"))
    cruise.dat[,'Date Reported'] <- as.Date(strptime(cruise.dat[,'Date Reported'],format="%d/%m/%Y"))
    
    #parse out boatname and CFR
    #boatname is string to left of opening left parenthesis
    #TO DO - error handling here
    tBoatName<-strsplit(cruise.dat[,'Boat'],"\\(")[[1]][1]
    tCFR<-strsplit(cruise.dat[,'Boat'],"\\(")[[1]][2]
    cruise.dat[,'BoatName'] <- gsub("^\\s+|\\s+$","",tBoatName)
    cruise.dat[,'CFR'] <- substring(gsub("^\\s+|\\s+$", "", tCFR),1,nchar(gsub("^\\s+|\\s+$", "", tCFR))-1)
    
    odbcClose(excel.connect)
    
    ret <- cruise.dat
    
  } else {
    
    stop(paste0("Cannot find file",file.name))
    
  }
  
  ret
  
}

getCruiseFromExcelV3 <- function(cruise.code,cruise.tab,ext,wb){
  
#   file.name <- paste0(file.dir,gsub("/","_",cruise.code),ext)  
#   sheet.name <- cruise.tab
  
  #check file exists
#   if (file.exists(file.name)) {
#     
#     #load the workbook
#     wb <- loadWorkbook(file.name, create=FALSE)
      
    #cruise code
    exCruiseCode <- readWorksheet(wb,cruise.tab,startRow=3,startCol=2,endRow=3,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #cruise type
    exCruiseType <- readWorksheet(wb,cruise.tab,startRow=5,startCol=2,endRow=5,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #departure date
    exDepDate <- readWorksheet(wb,cruise.tab,startRow=6,startCol=2,endRow=6,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #completion date
    exCompDate <- readWorksheet(wb,cruise.tab,startRow=7,startCol=2,endRow=7,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #boat
    exBoat <- readWorksheet(wb,cruise.tab,startRow=8,startCol=2,endRow=8,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #departure port
    exDepPort <- readWorksheet(wb,cruise.tab,startRow=9,startCol=2,endRow=9,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #arrival port
    exArrPort <- readWorksheet(wb,cruise.tab,startRow=10,startCol=2,endRow=10,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #personnel
    exPersonnel <- readWorksheet(wb,cruise.tab,startRow=11,startCol=2,endRow=11,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #inputby
    exInputBy <- readWorksheet(wb,cruise.tab,startRow=5,startCol=5,endRow=5,endCol=5,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #discards collected - value stored in hidden cell (I6)
    exDisCollected <- readWorksheet(wb,cruise.tab,startRow=6,startCol=9,endRow=6,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #reason
    exReason <- readWorksheet(wb,cruise.tab,startRow=7,startCol=5,endRow=7,endCol=5,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Cod
    exCod <- readWorksheet(wb,cruise.tab,startRow=7,startCol=9,endRow=7,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Whiting
    exWhiting <- readWorksheet(wb,cruise.tab,startRow=8,startCol=9,endRow=8,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Hake
    exHake <- readWorksheet(wb,cruise.tab,startRow=9,startCol=9,endRow=9,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Megrim
    exMegrim <- readWorksheet(wb,cruise.tab,startRow=10,startCol=9,endRow=10,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #BSole
    exBSole <- readWorksheet(wb,cruise.tab,startRow=11,startCol=9,endRow=11,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #LPisc
    exLPisc <- readWorksheet(wb,cruise.tab,startRow=12,startCol=9,endRow=12,endCol=9,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #LBud
    exLBud <- readWorksheet(wb,cruise.tab,startRow=7,startCol=11,endRow=7,endCol=11,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Plaice
    exPlaice <- readWorksheet(wb,cruise.tab,startRow=8,startCol=11,endRow=8,endCol=11,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Haddock
    exHaddock <- readWorksheet(wb,cruise.tab,startRow=9,startCol=11,endRow=9,endCol=11,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #Saithe
    exSaithe <- readWorksheet(wb,cruise.tab,startRow=10,startCol=11,endRow=10,endCol=11,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #LSole
    exLSole <- readWorksheet(wb,cruise.tab,startRow=11,startCol=11,endRow=11,endCol=11,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    #comments
    exComments <- readWorksheet(wb,cruise.tab,startRow=14,startCol=2,endRow=14,endCol=2,header=FALSE,autofitCol=FALSE,autofitRow=FALSE)
    
  #if a date is only 5 characters long append a leading zero
  depdate <- exDepDate
  compdate <- exCompDate
  if (!is.na(exDepDate) & nchar(exDepDate)==5) {depdate <- paste("0",exDepDate,sep="")}
  if (!is.na(exCompDate) & nchar(exCompDate)==5) {compdate <- paste("0",exCompDate,sep="")}
  
  #parse out boat name and CFR
  exBoatName <- gsub("^\\s+|\\s+$","",strsplit(exBoat[1,1],"\\(")[[1]][1])
  exCFR <- substring(gsub("^\\s+|\\s+$", "", strsplit(exBoat[1,1],"\\(")[[1]][2]),1,nchar(gsub("^\\s+|\\s+$", "", strsplit(exBoat[1,1],"\\(")[[1]][2]))-1)
    
    exCruiseDat <- list("Cruise Code" = exCruiseCode[1,1],"Cruise Type" = exCruiseType[1,1], "Boat Name" = exBoatName,
                        "CFR" = exCFR, "Dep Port" = exDepPort[1,1], "Arr Port" = exArrPort[1,1], 
                        "Dep Date" = as.Date(strptime(depdate,format="%d%m%y")),
                        "Comp Date" = as.Date(strptime(compdate,format="%d%m%y")),
                        "Personnel" = exPersonnel[1,1], "Comment" = exComments[1,1],
                        "Input By" = exInputBy[1,1], "Discards Collected" = exDisCollected[1,1],
                        "Reason" = if (is.null(exReason[1,1])){""} else {exReason[1,1]}, "Cod" = exCod[1,1], "Whiting" = exWhiting[1,1],
                        "Hake" = exHake[1,1], "Megrim" = exMegrim[1,1], "B Sole" = exBSole[1,1],
                        "L Pisc" = exLPisc[1,1], "L Bud" = exLBud[1,1], "Plaice" = exPlaice[1,1],
                        "Haddock" = exHaddock[1,1], "Saithe" = exSaithe[1,1], "L Sole" = exLSole[1,1])
    
#     ret <- exCruiseDat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
  
}

getHaulsFromExcelV3 <- function(cruise.code,hauls.tab,ext,wb){

#  file.name <- paste0(file.dir,gsub("/","_",cruise.code),ext)  
  #sheet.name <- hauls.tab
  
  #check file exists
#  if (file.exists(file.name)) {
    
#    #load the workbook
#    wb <- loadWorkbook(file.name, create=FALSE)

    #haul details
    exHaulsDat <- readWorksheet(wb,hauls.tab,startRow=300,startCol=2,endRow=400,endCol=29,
                                header=FALSE,autofitCol=FALSE)
    
    #if a date is only 5 characters long append a leading zero
    shootdate <- as.character(exHaulsDat[,13])
    shootdate[!is.na(shootdate) & nchar(shootdate)==5] <- paste("0",shootdate[!is.na(shootdate) & nchar(shootdate)==5],sep="")
    hauldate <- as.character(exHaulsDat[,20])
    hauldate[!is.na(hauldate) & nchar(hauldate)==5] <- paste("0",hauldate[!is.na(hauldate) & nchar(hauldate)==5],sep="")
    
    #success code
    sc <- rep(NA,length(exHaulsDat[,4]))
    if (sum(!is.na(exHaulsDat[,4]))>0){
      sc[!is.na(exHaulsDat[,4])] <- unlist(lapply(strsplit(as.character(exHaulsDat[,4][!is.na(exHaulsDat[,4])]),":"),"[[",1))
    }
    
    #sea state
    ss <- rep(NA,length(exHaulsDat[,24]))
    if (sum(!is.na(exHaulsDat[,24]))>0){
      ss[!is.na(exHaulsDat[,24])] <- unlist(lapply(strsplit(as.character(exHaulsDat[,24][!is.na(exHaulsDat[,24])]),":"),"[[",1))
    }

    exHaulsDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exHaulsDat)),
                       "Haul" = exHaulsDat[,1],
                       "GearCode"=unlist(lapply(strsplit(exHaulsDat[,2]," : "),"[[",1)),
                       "MeshSize" = exHaulsDat[,3],
                       "SuccessCode" = sc,
                       "ICESDiv" = exHaulsDat[,5],
                       "FishingGround"=unlist(lapply(strsplit(exHaulsDat[,6]," : "),"[[",1)),
                       "TimeShot" = exHaulsDat[,7],
                       #"TimeShot" = substr(exHaulsDat[,7],start=12,stop=19),
                       "DepthShot" = exHaulsDat[,8],
                       "LatShot" = exHaulsDat[,9],
                       "LatShotSign" = exHaulsDat[,10],
                       "LonShot" = exHaulsDat[,11],
                       "LonShotSign" = exHaulsDat[,12],
                       "ShootDate" = as.Date(strptime(shootdate,format="%d%m%y")),
                       "TimeHauled" = exHaulsDat[,14],
                       #"TimeHauled" = substr(exHaulsDat[,14],start=12,stop=19),
                       "DepthHauled" = exHaulsDat[,15],
                       "LatHauled" = exHaulsDat[,16],
                       "LatHauledSign" = exHaulsDat[,17],
                       "LonHauled" = exHaulsDat[,18],
                       "LonHauledSign" = exHaulsDat[,19],
                       "HauledDate" = as.Date(strptime(hauldate,format="%d%m%y")),
                       "Catch" = exHaulsDat[,21],
                       "WindDir" = exHaulsDat[,22],
                       "WindForce" = exHaulsDat[,23],
                       "SeaState" = ss,
                       "SwellDir" = exHaulsDat[,25],
                       "SeaSwell" = exHaulsDat[,26],
                       "GroundType"=unlist(lapply(strsplit(exHaulsDat[,27]," : "),"[[",1)),
                       "Memo" = exHaulsDat[,28],
                       #"EA" = rep(NA,nrow(exHaulsDat)),
                       stringsAsFactors=FALSE
                       )
    
#    ret <- exHaulsDat
    
#  } else {
    
#    stop(paste0("Cannot find file",file.name))
    
#  }
  
#  ret
  
}

getHaulFromExcel <- function(path,cruise,tab.name,ext){
  
  #column names for haul data
  col.names<-c("Cruise Code","Haul","Station Number","Gear Code","Mesh Size",
               "Success Code","ICES DIV","Fishing Ground Code","Time Shot","Depth Shot",
               "Lat Shot Deg","Lat Shot Mins","Lat Shot Sign","Long Shot Deg",
               "Long Shot Mins","Long Shot Sign","Shoot Date","Time Hauled","Depth Hauled",
               "Lat Hauled Deg","Lat Hauled Mins","Lat Hauled Sign","Long Hauled Deg",
               "Long Hauled Mins","Long Hauled Sign","Haul Date","Haul Year","Quarter",
               "Wind Direction","Wind Force","Sea State","Swell Direction","Sea Swell",
               "Ground Type Code","Catch","CatchUnits","EA","Memo")
  
  file.name <- paste0(path,gsub("/","_",cruise),ext)  
  sheet.name <- tab.name
  
  #check file exists
  if (file.exists(file.name)) {
    
    excel.connect <- odbcConnectExcel2007(file.name)
    haul.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","-"),stringsAsFactors=FALSE)
    odbcClose(excel.connect)
    
    ret <- haul.dat
    
    #strip blank rows
    ret <- ret[apply(!is.na(ret),1,sum)>0,]
    #and any columns not in vector of recognised col names
    ret <- ret[,!is.na(match(colnames(ret),col.names))]
    
  } else {
    
    stop(paste0("Cannot find file",file.name))
    
  }
  
  ret
  
}

getBulkFromExcelV3 <- function(cruise.code,bulk.tab,ext,wb){

#   file.name <- paste0(file.dir,gsub("/","_",cruise.code),ext)  
#   sheet.name <- bulk.tab
  
#   #check file exists
#   if (file.exists(file.name)) {
    
    #load the workbook
    #wb <- loadWorkbook(file.name, create=FALSE)
    
    #bulk landings details
    exBulkDat <- readWorksheet(wb,bulk.tab,startRow=100,startCol=2,endRow=1099,endCol=8,
                               header=FALSE,autofitCol=FALSE)
    
    #select only those records with a haul code that is not empty
    
    exBulkDat <- exBulkDat[!is.na(exBulkDat[,1]),]
    
    exBulkDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exBulkDat)),
                            "Haul" = exBulkDat[,1],
                            "BSpecies" = exBulkDat[,2],
                            "Grade" = exBulkDat[,3],
                            "Condition" = exBulkDat[,4],
                            "BQuantity" = exBulkDat[,5],
                            "Factor" = exBulkDat[,6],
                            "Total" = exBulkDat[,7],
                             stringsAsFactors=FALSE)
    
#     ret <- exBulkDat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
  
  
}

getBulkFromExcel <- function(path,cruise,tab.name,ext){
  
  col.names <- c("Cruise Code","Haul","B-Species","B-Quantity","State","Factor","Fish Type","Grade")
  
  file.name <- paste0(path,gsub("/","_",cruise),ext)  
  sheet.name <- tab.name

  #check file exists
  if (file.exists(file.name)) {
    
    excel.connect <- odbcConnectExcel2007(file.name)
    bulk.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
    odbcClose(excel.connect)
    
    ret <- bulk.dat
    
    #strip blank rows
    ret <- ret[apply(!is.na(ret),1,sum)>0,]
    #and any columns not in vector of recognised col names
    ret <- ret[,!is.na(match(colnames(ret),col.names))]
    
    
  } else {
    
    stop(paste0("Cannot find file",file.name))
    
  }
  
  ret
  
}

getSampleHeaderFromExcelV3 <- function(cruise.code,sampleheader.tab,ext,wb){
  
  #sample header details
  #exSampleHeaderDat <- readWorksheet(wb,sampleheader.tab,startRow=100,startCol=2,endRow=1098,endCol=7,
  #                                   header=FALSE,autofitCol=FALSE)
  #2 new fields - grade and data type
  exSampleHeaderDat <- readWorksheet(wb,sampleheader.tab,startRow=100,startCol=2,endRow=1098,endCol=9,
                                     header=FALSE,autofitCol=FALSE)
  
  #select only those records with a haul code that is not empty
  
  exSampleHeaderDat <- exSampleHeaderDat[!is.na(exSampleHeaderDat[,1]),]
  
#   exSampleHeaderDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exSampleHeaderDat)),
#                           "Haul" = exSampleHeaderDat[,1],
#                           "SampleNumber" = exSampleHeaderDat[,2],
#                           "Quantity" = exSampleHeaderDat[,3],
#                           "Presentation" = exSampleHeaderDat[,4],
#                           "Type" = exSampleHeaderDat[,5],
#                           "NFD" = exSampleHeaderDat[,6],
#                           stringsAsFactors=FALSE)

  exSampleHeaderDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exSampleHeaderDat)),
                                  "Haul" = exSampleHeaderDat[,1],
                                  "SampleNumber" = exSampleHeaderDat[,2],
                                  "Quantity" = exSampleHeaderDat[,3],
                                  "Presentation" = exSampleHeaderDat[,4],
                                  "Type" = exSampleHeaderDat[,5],
                                  "NFD" = exSampleHeaderDat[,6],
                                  "Grade" = exSampleHeaderDat[,7],
                                  "DataType" = exSampleHeaderDat[,8],
                                  stringsAsFactors=FALSE)
  
}

getSampleHeaderFromExcel <- function(path,cruise,tab.name,ext){
  
  col.names <- c("Cruise Code","Haul Code","Sample Number","Quantity",
                 "Units","Condition","Type","NFD","Grade")
  
  file.name <- paste0(path,gsub("/","_",cruise),ext)  
  sheet.name <- tab.name
  
  #check file exists
  if (file.exists(file.name)) {
    
    excel.connect <- odbcConnectExcel2007(file.name)
    sh.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
    odbcClose(excel.connect)
    
    ret <- sh.dat
    #strip blank rows
    ret <- ret[apply(!is.na(ret),1,sum)>0,]
    #and any columns not in vector of recognised col names
    ret <- ret[,!is.na(match(colnames(ret),col.names))]
    
  } else {
    
    stop(paste0("Cannot find file",file.name))
    
  }
  
  ret
  
}

getMOSampleFromExcelV3 <- function(cruise.code,tabname="MO Samples",ext,wb) {

  #MO sample details
  exMOSampleDat <- readWorksheet(wb,tabname,startRow=100,startCol=2,endRow=5000,endCol=7,
                                 header=FALSE,autofitCol=FALSE)
  
  #select only those records with a haul code that is not empty
  
  exMOSampleDat <- exMOSampleDat[!is.na(exMOSampleDat[,1]),]
  
  exMOSampleDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exMOSampleDat)),
                              "Haul" = exMOSampleDat[,1],
                              "SampleNumber" = exMOSampleDat[,2],
                              "Species" = exMOSampleDat[,3],
                              "Length" = exMOSampleDat[,4],
                              "Number" = exMOSampleDat[,5],
                              "Sex" = exMOSampleDat[,6],
                              stringsAsFactors=FALSE)
  
}

getAgedSampleFromExcelV3 <- function(cruise.code,tabname="Aged Samples",ext,wb) {
  
  #aged sample details
  exAgedSampleDat <- readWorksheet(wb,tabname,startRow=13,startCol=2,endRow=1011,
                                   endCol=10,header=FALSE,autofitCol=FALSE)
  
  #select only those records with a haul code that is not empty
  
  exAgedSampleDat <- exAgedSampleDat[!is.na(exAgedSampleDat[,1]),]
  
  exAgedSampleDat <- data.frame("CruiseCode" = rep(cruise.code,nrow(exAgedSampleDat)),
                                "Haul" = exAgedSampleDat[,1],
                                "SampleNumber" = exAgedSampleDat[,2],
                                "Species" = exAgedSampleDat[,3],
                                "IndexNo" = exAgedSampleDat[,4],
                                "Length" = exAgedSampleDat[,5],
                                "Weight" = exAgedSampleDat[,6],
                                "Age" = exAgedSampleDat[,7],
                                "Sex" = exAgedSampleDat[,8],
                                "Maturity" = exAgedSampleDat[,9],
                                stringsAsFactors=FALSE)
  
}



getSampleFromExcel <- function(path,cruise,tab.name,ext){
  
  col.names <- c("Cruise Code","Haul Code","Sample Number","Species","Length","Number",
                 "Weight","Age","Sex","Index Number","Maturity")
  
  file.name <- paste0(path,gsub("/","_",cruise),ext)  
  sheet.name <- tab.name
  
  #check file exists
  if (file.exists(file.name)) {
    
    excel.connect <- odbcConnectExcel2007(file.name)
    samples.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
    odbcClose(excel.connect)
    
    ret <- samples.dat
    #strip blank rows
    ret <- ret[apply(!is.na(ret),1,sum)>0,]
    #and any columns not in vector of recognised col names
    ret <- ret[,!is.na(match(colnames(ret),col.names))]
    
  } else {
    
    stop(paste0("Cannot find file",file.name))
    
  }
  
  ret
  
}

# getCruiseFromExcel <- function(path,cruise){
#   
#   file.name <- paste0(path,gsub("/","",cruise),"c.xls")
#   sheet.name <- "L_output_cruise"
# 
#   #check file exists
#   if (file.exists(file.name)) {
#   
#     excel.connect <- odbcConnectExcel(file.name)
#     cruise.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","-"),stringsAsFactors=FALSE)
#     cruise.dat[,'Departure Date'] <- as.Date(cruise.dat[,'Departure Date'])
#     cruise.dat[,'Completion Date'] <- as.Date(cruise.dat[,'Completion Date'])
#     cruise.dat[,'Date received'] <- as.Date(cruise.dat[,'Date received'])
#     cruise.dat[,'Date reported'] <- as.Date(cruise.dat[,'Date reported'])
#     odbcClose(excel.connect)
# 
#     ret <- cruise.dat
#   
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#   
#   }
#   
#   ret
#   
# }
# 
# getHaulFromExcel <- function(path,cruise){
#   
#   file.name <- paste0(path,gsub("/","",cruise),"h.xls")
#   sheet.name <- "L_output_haul"
#   
#   #check file exists
#   if (file.exists(file.name)) {
#     
#     excel.connect <- odbcConnectExcel(file.name)
#     haul.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","-"),stringsAsFactors=FALSE)
#     odbcClose(excel.connect)
#     
#     ret <- haul.dat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
#   
# }
# 
# getBulkFromExcel <- function(path,cruise){
#   
#   file.name <- paste0(path,gsub("/","",cruise),"b.xls")
#   sheet.name <- "L_output_bulk"
#   
#   #check file exists
#   if (file.exists(file.name)) {
#     
#     excel.connect <- odbcConnectExcel(file.name)
#     bulk.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
#     odbcClose(excel.connect)
#     
#     ret <- bulk.dat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
#   
# }
# 
# getSampleHeaderFromExcel <- function(path,cruise){
#   
#   file.name <- paste0(path,gsub("/","",cruise),"sh.xls")
#   sheet.name <- "L_output_sample-header"
#   
#   #check file exists
#   if (file.exists(file.name)) {
#     
#     excel.connect <- odbcConnectExcel(file.name)
#     sh.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
#     odbcClose(excel.connect)
# 
#     #remove spaces from field names
#     names(sh.dat)<-gsub(" ","",names(sh.dat))
#     
#     ret <- sh.dat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
#   
# }
# 
# getSampleFromExcel <- function(path,cruise){
#   
#   file.name <- paste0(path,gsub("/","",cruise),"s.xls")
#   sheet.name <- "L_output_sample"
#   
#   #check file exists
#   if (file.exists(file.name)) {
#     
#     excel.connect <- odbcConnectExcel(file.name)
#     samples.dat <- sqlFetch(excel.connect,sheet.name,na.strings=c("","_"),stringsAsFactors=FALSE)
#     odbcClose(excel.connect)
#     
#     #remove spaces from field names
#     names(samples.dat) <- gsub(" ","",names(samples.dat))
#     
#     ret <- samples.dat
#     
#   } else {
#     
#     stop(paste0("Cannot find file",file.name))
#     
#   }
#   
#   ret
#   
# }
