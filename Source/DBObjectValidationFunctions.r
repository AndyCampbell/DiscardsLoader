validCruise <- function(cruise,personnel,vessels,ports,types){
  
  #cruise validation
  #1 is cruise code unique?
  #2 is vessel valid?
  #3 is dep port valid?
  #4 is arr port valid?
  #5 is dep date a valid date and in the past?
  #6 is comp data a valid date and in the past and after dep date?
  #7 is date received a valid date in the past
  #8 is input by valid?
  #9 is validated by valid?
  #10 is reported by valid?
  #11 is the reason provided for no discards?
  #12 who was the crew member?
  
  #returns a new data frame with the validated and formatted data plus necessary ids
  #if cruise is invalid the original data is returned
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"Type"=NA,"FSSVessel_id"=NA,"BoatName"=NA,"VesselLOA"=NA,
                    "VesselkW"=NA,"Segment"=NA,"DepPort_id"=NA,"DepPort"=NA,"ArrPort_id"=NA,"ArrPort"=NA,
                    "DepDate"=NA,"CompDate"=NA,"Memo"=NA,"DateReceived"=NA,"InputCompleted"=NA,"InputBy"=NA,
                    "InputBy_id"=NA,"ValidatedBy"=NA,"ValidatedBy_id"=NA,"ReportRequired"=NA,"ReportedBy"=NA,
                    "ReportedBy_id"=NA,"DateReported"=NA,"BoxBrought"=NA,"Cod"=NA,"Hake"=NA,"B_Sole"=NA,"L_Bud"=NA,
                    "Had"=NA,"Whg"=NA,"Meg"=NA,"L_Pisc"=NA,"Plaice"=NA,"Saithe"=NA,"L_Sole"=NA,"Reason"=NA,
                    "Metier"=NA,"CruiseCodeOrig"=NA,"Crew"=NA,"Crew_id"=NA)
  
  #ret$CruiseCode <- cruise[,'Cruise Code']
  ret$CruiseCode <- cruise[['Cruise Code']]
  
  #validCruiseType <- validCruiseType(cruise[,'Cruise Type'],types)
  validCruiseType <- validCruiseType(cruise[['Cruise Type']],types)
  ret$Type <- validCruiseType$type

  #parse out boat name and CFR from 
  
  #cat("cruise[,'Boat']=",cruise[,'Boat'],"\n")
  #cat("cruise[,'BoatName']=",cruise[,'BoatName'],"\n")
  #cat("cruise[,'CFR']=",cruise[,'CFR'],"\n")
  
  #validVessel <- validVessel(cruise[,'Boat'],NULL,vessels,cruise[,'Departure Date'])
  #validVessel <- validVessel(cruise[,'BoatName'],cruise[,'CFR'],vessels,cruise[,'Departure Date'])
  validVessel <- validVessel(cruise[['Boat Name']],cruise[['CFR']],vessels,cruise[['Dep Date']])
  
  #TO DO - catch for invalid vessel
  
  ret$FSSVessel_id <- validVessel$id
  ret$BoatName <- validVessel$name
  ret$VesselLOA <- validVessel$LOA
  ret$VesselkW <- validVessel$kW
  ret$Segment <- validVessel$segment
  
  #validDepPort <- validPort(cruise[,'Departure Port'],ports)
  validDepPort <- validPort(cruise[['Dep Port']],ports)
  ret$DepPort_id <- validDepPort$id
  ret$DepPort <- validDepPort$name
  
  #validArrPort <- validPort(cruise[,'Arrival Port'],ports)
  validArrPort <- validPort(cruise[['Arr Port']],ports)
  ret$ArrPort_id <- validArrPort$id
  ret$ArrPort <- validArrPort$name
  
  #ret$DepDate <- cruise[,'Departure Date']
  ret$DepDate <- cruise[['Dep Date']]
  
  #if (cruise[,'Completion Date']>cruise[,'Departure Date']) {
  if (cruise[['Comp Date']]>cruise[['Dep Date']]) {
      #ret$CompDate <- cruise[,'Completion Date']  
      ret$CompDate <- cruise[['Comp Date']]  
  } else {
    #error
    ret$CompDate <- NA
  }
  
  #Memo field
  #cannot be longer than 4000 chars
  #if (nchar(cruise[,'Memo'])<=4000) {
  if (!is.na(cruise[['Comment']])) {
    if (nchar(cruise[['Comment']])<=4000) {
      #ret$Memo <- cruise[,'Memo']  
      ret$Memo <- cruise[['Comment']]  
    } else {
      #error
      ret$Memo <- 'Error'
    }
  }
  
  #Date Received
  #if (is.na(cruise[,'Date Received'])){
    ret$DateReceived <- NA
  #} else {
  #  ret$DateReceived <- cruise[,'Date Received']  
  #}
  
  #Input Completed
  #ret$InputCompleted <- cruise[,'Input Completed']
  
  #Input By
  #validInputBy <- validPersonnel(cruise[,'Data Inputted By'],cruise[,'Departure Date'],personnel)
  validInputBy <- validPersonnel(cruise[['Input By']],cruise[['Dep Date']],personnel)
  ret$InputBy <- validInputBy$name
  ret$InputBy_id <- validInputBy$id
  
  #Validated By
  #validValidatedBy <- validPersonnel(cruise[,'Data Validated By'],cruise[,'Departure Date'],personnel)
  #ret$ValidatedBy <- validValidatedBy$name
  #ret$ValidatedBy_id <- validValidatedBy$id
  
  #Reported By
  #validReportedBy <- validPersonnel(cruise[,'Reported By'],cruise[,'Departure Date'],personnel)
  #ret$ReportedBy <- validReportedBy$name
  #ret$ReportedBy_id <- validReportedBy$id
  
  #CrewID
  #validCrew <- validPersonnel(cruise[,'Crew'],cruise[,'Departure Date'],personnel)
  validCrew <- validPersonnel(cruise[['Personnel']],cruise[['Dep Date']],personnel)
  ret$Crew <- validCrew$name
  ret$Crew_id <- validCrew$id
  
  #Report Required
  #ret$ReportRequired <- cruise[,'Report Required']
  
  #Date Reported
  #if (is.na(cruise[,'Date Reported'])){
  #  ret$DateReported <- NA
  #} else {
  #  ret$DateReported <- cruise[,'Date Reported']  
  #}
  
  #Box Brought to Lab
  #if (cruise[,'BoxBroughtToLabForAgeing'] == 1) {
  if (cruise[['Discards Collected']] == TRUE) {
    ret$BoxBrought <- 'Yes'
  #} else if (cruise[,'BoxBroughtToLabForAgeing'] == 0) {
  } else if (cruise[['Discards Collected']] == FALSE) {
    ret$BoxBrought <- 'No'
  } 
  
  #Cod
  #ret$Cod <- cruise[,'Cod']
  if (cruise[['Cod']] == TRUE) {ret$Cod <- 1} else {ret$Cod <- 0}
  #Hake
  #ret$Hake <- cruise[,'Hake']
  if (cruise[['Hake']] == TRUE) {ret$Hake <- 1} else {ret$Hake <- 0}
  #B_Sole
  #ret$B_Sole <- cruise[,'B_Sole']
  if (cruise[['B Sole']] == TRUE) {ret$B_Sole <- 1} else {ret$B_Sole <- 0}
  #L_Bud
  #ret$L_Bud <- cruise[,'L_Bud']
  if (cruise[['L Bud']] == TRUE) {ret$L_Bud <- 1} else {ret$L_Bud <- 0}
  #Had
  #ret$Had <- cruise[,'Had']
  if (cruise[['Haddock']] == TRUE) {ret$Had <- 1} else {ret$Had <- 0}
  #Whg
  #ret$Whg <- cruise[,'Whg']
  if (cruise[['Whiting']] == TRUE) {ret$Whg <- 1} else {ret$Whg <- 0}
  #Meg
  #ret$Meg <- cruise[,'Meg']
  if (cruise[['Megrim']] == TRUE) {ret$Meg <- 1} else {ret$Meg <- 0}
  #L_Pisc
  #ret$L_Pisc <- cruise[,'L_Pisc']
  if (cruise[['L Pisc']] == TRUE) {ret$L_Pisc <- 1} else {ret$L_Pisc <- 0}
  #Plaice
  #ret$Plaice <- cruise[,'Plaice']
  if (cruise[['Plaice']] == TRUE) {ret$Plaice <- 1} else {ret$Plaice <- 0}
  #Saithe
  #ret$Saithe <- cruise[,'Saithe']
  if (cruise[['Saithe']] == TRUE) {ret$saithe <- 1} else {ret$Saithe <- 0}
  #L_Sole
  #ret$L_Sole <- cruise[,'L_Sole']
  if (cruise[['L Sole']] == TRUE) {ret$L_Sole <- 1} else {ret$L_Sole <- 0}
  
  #Reason field
  #cannot be longer than 255 chars
  #if (nchar(cruise[,'ReasonsDiscardsNotBroughtBackToLab'])<=255) {
  if (!is.na(cruise[['Reason']])) {
    if (nchar(cruise[['Reason']])<=255) {
      ret$Reason <- cruise[['Reason']]  
      #ret$Reason <- cruise[,'ReasonsDiscardsNotBroughtBackToLab']  
    } else {
      #error
      ret$Reason <- 'Error'
    }
  }
  
  ret
  
}

validHaul <- function(haul,ices,gear,winddir,windfor,seastate,swelldir,swell,
                      groundtype,ground,success,cruise){
  
  require(stringr)
  
  #haul - the hauls data to be checked
  #ices - ICES area reference list
  #cruise - the validated cruise record
  
  #shoot date and time must be during cruise
  #haul date and time must be during cruise
  #haul date and time must be later than shoot date and time
  #gear code must be on list
  #quarter must be one of 1,2,3,4
  #wind force must be on reference list
  #wind direction must be on reference list
  #sea state must be on reference list
  #swell direction must be on reference list
  #sea swell must be on reference list
  #ground type code must be on reference list
  #fishing ground code must be on reference list
  #positional information is required for all hauls except for 0,299 & 300
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"Haul"=NA,"ICES_id"=NA,"ICESDiv"=NA,"Gear_id"=NA,
                    "GearCode"=NA,"Mesh"=NA,"Success"=NA,"TimeShot"=NA,"DepthShot"=NA,"LatShotDeg"=NA,"LatShotMin"=NA,
                    "LatShotSign"=NA,"LonShotDeg"=NA,"LonShotMin"=NA,"LonShotSign"=NA,"DateShot"=NA,"TimeHaul"=NA,
                    "DepthHaul"=NA,"LatHaulDeg"=NA,"LatHaulMin"=NA,"LatHaulSign"=NA,"LonHaulDeg"=NA,"LonHaulMin"=NA,
                    "LonHaulSign"=NA,"Catch"=NA,"CatchUnits"=NA,"EA"=NA,"DateHaul"=NA,"YearHaul"=NA,"Quarter"=NA,
                    "WindDir_id"=NA,"WindDir"=NA,"WindFor_id"=NA,"WindFor"=NA,"SeaSta_id"=NA,"SeaSta"=NA,
                    "SwellDir_id"=NA,"SwellDir"=NA,"SeaSwe_id"=NA,"SeaSwe"=NA,"GndTyp_id"=NA,"GndTyp"=NA,"FshGrn_id"=NA,
                    "FshGrn"=NA,"GearCodeStd"=NA,"Metier"=NA,"Memo"=NA)
  
  if (haul['CruiseCode'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = haul['CruiseCode']
  }
  
  #haul - mandatory, must be numeric
  if (is.na(haul['Haul'])) {
    ret$msg = "Missing Haul number"
  } else if (is.na(as.numeric(haul['Haul']))) {
    ret$msg = "Invalid Haul number"
    ret$Haul = haul['Haul']
  } else {
    ret$Haul = gsub(" ","",haul['Haul'])
  }
  
  #ICES division
  i<-validICES(haul['ICESDiv'],ices)
  if (!is.na(i$err)) {
    ret$msg = "Invalid ICES Division"
    ret$ICESDiv = haul['ICESDiv']
  } else {
    ret$ICES_id = i$id
    ret$ICESDiv = i$name
  }
  
  #Gear code
  i<-validGear(haul['GearCode'],gear)
  if (!is.na(i$err)) {
    ret$msg = "Invalid Gear Code"
    ret$GearCode = haul['GearCode']
  } else {
    ret$Gear_id = i$id
    ret$GearCode = i$name
  }

  #Mesh size, must be numeric
  i<-validMeshSize(haul['MeshSize'])
  if (!is.na(i$err)){
    #paste specific error message
    ret$msg = paste("Invalid Mesh Size",i$err,sep=":")
    ret$Mesh = haul['MeshSize']
  } else {
    ret$Mesh = i$mesh
  }

  #Success code
  i<-validSuccessCode(haul['SuccessCode'],success)
  if (!is.na(i$err)) {
    ret$msg = "Invalid Success Code"
    ret$Success = haul['SuccessCode']
  } else {
    ret$Success = i$code
  }
  
  #Shoot time
  ret$TimeShot = haul['TimeShot']
  if (!is.na(haul['TimeShot']) & nchar(haul['TimeShot'])>0) {
    ret$TimeShot = stringr::str_pad(stringr::str_trim(haul['TimeShot']),4,side="left",pad="0")
    #insert separator
    ret$TimeShot = paste0(stringr::str_sub(ret$TimeShot,1,2),':',stringr::str_sub(ret$TimeShot,3,4))
  }
  
  #Shoot Depth
  ret$DepthShot = gsub(" ","",haul['DepthShot'])
  
  #Shoot Latitude Degrees
  #ret$LatShotDeg = gsub(" ","",haul['LatShotDeg'])
  ret$LatShotDeg = strsplit(haul['LatShot'],split=" ")[[1]][1]
  
  #Shoot Latitude Minutes
  #ret$LatShotMin = haul['LatShotMins']
  ret$LatShotMin = strsplit(haul['LatShot'],split=" ")[[1]][2]
  
  #Shoot Latitude Sign (N/S)
  i <- validLatSign(haul['LatShotSign'])
  if (!is.na(i$err)){
    ret$msg = "Invalid Shoot Latitude Sign"
    ret$LatShotSign = haul['LatShotSign']
  } else {
    ret$LatShotSign = i$sign
  }
    
  #Shoot Longitude Degrees
  #ret$LonShotDeg = gsub(" ","",haul['Long Shot Deg'])
  ret$LonShotDeg = strsplit(haul['LonShot'],split=" ")[[1]][1]
   
  #Shoot Longitude Minutes
  #ret$LonShotMin = haul['Long Shot Mins']
  ret$LonShotMin = strsplit(haul['LonShot'],split=" ")[[1]][2]
  
  #Shoot Longitude Sign (W/E)
  i <- validLonSign(haul['LonShotSign'])
  if (!is.na(i$err)){
    ret$msg = "Invalid Shoot Longitude Sign"
    ret$LonShotSign = haul['LonShotSign']
  } else {
    ret$LonShotSign = i$sign
  }
  
  #Shoot Date
  ret$DateShot = haul['ShootDate']
  
  #Haul Time
  ret$TimeHaul = haul['TimeHauled']
  if (!is.na(haul['TimeHauled']) & nchar(haul['TimeHauled'])>0) {
    ret$TimeHaul = stringr::str_pad(stringr::str_trim(haul['TimeHauled']),4,side="left",pad="0")
    #insert separator
    ret$TimeHaul = paste0(stringr::str_sub(ret$TimeHaul,1,2),':',stringr::str_sub(ret$TimeHaul,3,4))
  }
  
  #Haul Depth
  ret$DepthHaul = gsub(" ","",haul['DepthHauled'])
  
  #Haul Latitude Degrees
  #ret$LatHaulDeg = gsub(" ","",haul['Lat Hauled Deg'])
  ret$LatHaulDeg = strsplit(haul['LatHauled'],split=" ")[[1]][1]
  
  #Haul Latitude Minutes
  #ret$LatHaulMin = haul['Lat Hauled Mins']
  ret$LatHaulMin = strsplit(haul['LatHauled'],split=" ")[[1]][2]
  
  #Haul Latitude Sign (N/S)
  i <- validLatSign(haul['LatHauledSign'])
  if (!is.na(i$err)){
    ret$msg = "Invalid Haul Latitude Sign"
    ret$LatHaulSign = haul['LatHauledSign']
  } else {
    ret$LatHaulSign = i$sign
  }
  
  #Haul Longitude Degrees
  #ret$LonHaulDeg = gsub(" ","",haul['Long Hauled Deg'])
  ret$LonHaulDeg = strsplit(haul['LonHauled'],split=" ")[[1]][1]
  
  #Haul Longitude Minutes
  #ret$LonHaulMin = haul['Long Hauled Mins']
  ret$LonHaulMin = strsplit(haul['LonHauled'],split=" ")[[1]][2]
  
  #Haul Longitude Sign (W/E)
  i <- validLonSign(haul['LonHauledSign'])
  if (!is.na(i$err)){
    ret$msg = "Invalid Haul Longitude Sign"
    ret$LonHaulSign = haul['LonHauledSign']
  } else {
    ret$LonHaulSign = i$sign
  }
  
  #Catch
  ret$Catch = gsub(" ","",haul['Catch'])
  
  #Catch Units
  #default to kg
  ret$CatchUnits = 1
#   ret$CatchUnits = haul['CatchUnits']
   
  #EA
  #ret$EA = NULL
  # ret$EA = haul['EA']
   
   #Haul Date
   ret$DateHaul = haul['HauledDate']
   
  #Haul Year
  #ret$YearHaul = haul['Haul Year']
  ret$YearHaul = substring(ret$DateHaul,1,4)
  
  #Haul Quarter
  #ret$Quarter = gsub(" ","",haul['Quarter'])
  month <- as.integer(substring(ret$DateHaul,6,7))
  ret$Quarter <- ceiling(4*month/12)

  #Wind Direction
  i<-validWindDir(haul['WindDir'],winddir)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$WindDir = haul['WindDir']
  } else {
    ret$WindDir_id = i$id
    ret$WindDir = i$name
  }
  
  #Wind Force
  i<-validWindFor(haul['WindForce'],windfor)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$WindFor = haul['WindForce']
  } else {
    ret$WindFor_id = i$id
    ret$WindFor = i$name
  }
  
  #Sea State
  i<-validSeaState(haul['SeaState'],seastate)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$SeaSta = haul['SeaState']
  } else {
    ret$SeaSta_id = i$id
    ret$SeaSta = i$name
  }
  
  #Swell Direction
  i<-validSwellDir(haul['SwellDir'],swelldir)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$SwellDir = haul['SwellDir']
  } else {
    ret$SwellDir_id = i$id
    ret$SwellDir = i$name
  }
  
  #Swell
  i<-validSwell(haul['SeaSwell'],swell)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$SeaSwe = haul['SeaSwell']
  } else {
    ret$SeaSwe_id = i$id
    ret$SeaSwe = i$name
  }
  
  #Ground Type
  i<-validGroundType(haul['GroundType'],groundtype)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$GndTyp = haul['GroundType']
  } else {
    ret$GndTyp_id = i$id
    ret$GndTyp = i$name
  }
  
  #Fishing Ground
  i<-validGround(haul['FishingGround'],ground)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$FshGrn = haul['FishingGround']
  } else {
    ret$FshGrn_id = i$id
    ret$FshGrn = i$name
  }

  #cannot be longer than 4000 chars
  if (is.na(haul['Memo'])) {
    ret$Memo <- ''
  } else if (nchar(haul['Memo'])<=4000) {
    ret$Memo = haul['Memo']   
  } else {
    #error
    ret$Memo <- 'Error'
  }

  #TO DO
  #ret$GearCodeStd
  #ret$Metier
  
  
  ret
  
}

validLanding <- function(bulk,species,state,factor,type,grades,hauls,cruise){
  
  #cruise code must be the same as in the validated cruise record
  #haul number must exist in the validated haul records
  #species must be on the reference list
  #state must be on the reference list
  #factor should match the value in factor for the validated species and state varibles
  #type should be on the reference list (should be Landings?)
  #grade ID must be on the reference list
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"Haul"=NA,"SpeciesCode"=NA,"Species"=NA,"Quantity"=NA,
                    "State"=NA,"Factor"=NA,"Type"=NA,"Grade_id"=NA,"GradeDescription"=NA,
                    "SuppliedFactor"=NA)
  
  if (bulk['CruiseCode'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = bulk['CruiseCode']
  }
  
  if (as.numeric(bulk['Haul']) %in% as.numeric(gsub(" ","",hauls[,'Haul']))) {
    ret$Haul <- bulk['Haul']
  } else {
    ret$msg = "Invalid Haul Code"
    ret$Haul = bulk['Haul']
  }
  
  #Species
  i<-validSpecies(bulk['BSpecies'],species)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Species = bulk['BSpecies']
  } else {
    ret$SpeciesCode = i$code
    ret$Species = i$name
  }
  
  #Quantity
  ret$Quantity = gsub(" ","",bulk['BQuantity'])
  
  #State/Condition
  i<-validState(bulk['Condition'],state)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$State = bulk['Condition']
  } else {
    ret$State = i$name
  }
  
  #Factor
  i<-validFactor(species = bulk['BSpecies'], condition = bulk['Condition'],
                 factor = bulk['Factor'], hauldate = hauls[as.integer(hauls$Haul)==as.integer(bulk['Haul']),]$DateHaul,
                 factors = factor)
  
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Factor = bulk['Factor']
    ret$SuppliedFactor <- bulk['Factor']
  } else {
    ret$SuppliedFactor <- bulk['Factor']
    ret$Factor <- as.numeric(i$factor)
  }
  
  #Type
  #i<-validType(bulk['Fish Type'],type)
  #if (!is.na(i$err)) {
  #  ret$msg = i$err
  #  ret$Type = bulk['Fish Type']
  #} else {
    ret$Type = "Landings"
  #}
  
  #Grade
  i<-validGrade(bulk['Grade'],grades)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$GradeDescription = bulk['Grade']
  } else {
    ret$Grade_id = i$id
    ret$GradeDescription = i$desc
  }
    
  ret
  
}  

validSampleHeader <- function(sh,condition,type,grades,datatype,landings,hauls,cruise){
  
  #cruise code must be the same as in the validated cruise record
  #haul number must exist in the validated haul records
  #condition must be on the reference list
  #type should be on the reference list (should be Landings?)
  #grade description must be on the reference list
  #data type must be on the reference list
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"HaulCode"=NA,"SampleID"=NA,"SampleNumber"=NA,
                    "Quantity"=NA,"Units"=NA,"Condition"=NA,"Type"=NA,"NFD"=NA,"Grade_id"=NA,
                    "GradeDescription"=NA,"DataType"=NA)
  
  if (sh['CruiseCode'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = sh['CruiseCode']
  }
  
  if (as.numeric(sh['Haul']) %in% as.numeric(gsub(" ","",hauls[,'Haul']))) {
    ret$HaulCode <- as.numeric(gsub(" ","",sh['Haul']))
  } else {
    ret$msg = "Invalid Haul Code"
    ret$HaulCode = as.numeric(gsub(" ","",sh['Haul']))
  }
  
  #SampleID
  #ret$SampleID <- as.numeric(gsub(" ","",sh['SampleID']))
  
  #SampleNumber
  ret$SampleNumber <- as.numeric(gsub(" ","",sh['SampleNumber']))
  
  #Quantity
  ret$Quantity <- sh['Quantity']
  
  #Units (default to kg)
  ret$Units <- 1
  
  #Condition (Presentation)
  i<-validState(sh['Presentation'],condition)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Condition = sh['Presentation']
  } else {
    ret$Condition = i$name
  }
  
  #Type
  i<-validType(sh['Type'],type)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Type = sh['Type']
  } else {
    ret$Type = i$name
  }
  
  #NFD
  ret$NFD <- sh['NFD']
  
   #Grade
   i<-validGrade(sh['Grade'],grades)
   if (!is.na(i$err)) {
     ret$msg = i$err
     ret$GradeDescription = sh['Grade']
   } else {
     ret$Grade_id = i$id
     ret$GradeDescription = i$desc
   }
   
   #Data Type
   i<-validDataType(sh['DataType'],datatype)
   if (!is.na(i$err)) {
     ret$msg = i$err
     ret$DataType = sh['DataType']
   } else {
     ret$DataType = i$id
   }
     
  ret
  
}


validMOSample <- function(sample,species,sampleheaders,hauls,cruise){
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"HaulCode"=NA,"SampleNumber"=NA,"SpeciesCode"=NA,
                    "Species"=NA,"Length"=NA,"Number"=NA,"Sex"=NA)
  
  #cruise code must match that of the already validated cruise
  if (sample['CruiseCode'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = sample['CruiseCode']
  }
  
  #haul number must exist in the validated haul records
  if (as.numeric(sample['Haul']) %in% as.numeric(gsub(" ","",hauls[,'Haul']))) {
    ret$HaulCode <- as.numeric(gsub(" ","",sample['Haul']))
  } else {
    ret$msg = "Invalid Haul Code"
    ret$HaulCode = as.numeric(gsub(" ","",sample['Haul']))
  }
  
  #SampleNumber
  ret$SampleNumber <- as.numeric(gsub(" ","",sample['SampleNumber']))
  
  #species must be on meta data list
  i<-validSpecies(sample['Species'],species)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Species = sample['Species']
  } else {
    ret$SpeciesCode = i$code
    ret$Species = i$name
  }
  
  #Length
  ret$Length = as.numeric(gsub(" ","",sample['Length']))
  
  #Number
  ret$Number = as.numeric(gsub(" ","",sample['Number']))
  
  #Sex
  ret$Sex = gsub(" ","",sample['Sex'])
  
  ret
  
}

validAgedSample <- function(sample,species,sampleheaders,hauls,cruise){
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"HaulCode"=NA,"SampleNumber"=NA,"SpeciesCode"=NA,
                    "Species"=NA,"Length"=NA,"Number"=NA,"Weight"=NA,"Age"=NA,"Sex"=NA,
                    "IndexNumber"=NA,"Maturity"=NA)
  
  #cruise code must match that of the already validated cruise
  if (sample['CruiseCode'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = sample['CruiseCode']
  }
  
  #haul number must exist in the validated haul records
  if (as.numeric(sample['Haul']) %in% as.numeric(gsub(" ","",hauls[,'Haul']))) {
    ret$HaulCode <- as.numeric(gsub(" ","",sample['Haul']))
  } else {
    ret$msg = "Invalid Haul Code"
    ret$HaulCode = as.numeric(gsub(" ","",sample['Haul']))
  }
  
   #SampleNumber
   ret$SampleNumber <- as.numeric(gsub(" ","",sample['SampleNumber']))

  #species must be on meta data list
  i<-validSpecies(sample['Species'],species)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Species = sample['Species']
  } else {
    ret$SpeciesCode = i$code
    ret$Species = i$name
  }
   
   #Length
   ret$Length = as.numeric(gsub(" ","",sample['Length']))
   
   #Weight
   ret$Weight = as.numeric(gsub(" ","",sample['Weight']))
   
   #Age
   ret$Age = as.numeric(gsub(" ","",sample['Age']))
   
   #Sex
   ret$Sex = gsub(" ","",sample['Sex'])
   
   #Index Number
   ret$IndexNumber <- as.numeric(gsub(" ","",sample['IndexNo']))
   
   #Maturity
   ret$Maturity <- sample['Maturity']
  
  ret
  
}



validSample <- function(sample,species,sampleheaders,hauls,cruise){
  
  ret <- data.frame("msg"=NA,"CruiseCode"=NA,"HaulCode"=NA,"SampleNumber"=NA,"SpeciesCode"=NA,
                    "Species"=NA,"Length"=NA,"Number"=NA,"Weight"=NA,"Age"=NA,"Sex"=NA,
                    "IndexNumber"=NA,"Maturity"=NA)

  #cruise code must match that of the already validated cruise
  if (sample['Cruise Code'] == cruise[,'CruiseCode']) {
    ret$CruiseCode = cruise[,'CruiseCode']
  } else {
    ret$msg = "Invalid Cruise Code"
    ret$CruiseCode = sample['Cruise Code']
  }
  
  #haul number must exist in the validated haul records
  if (as.numeric(sample['Haul Code']) %in% as.numeric(gsub(" ","",hauls[,'Haul']))) {
    ret$HaulCode <- as.numeric(gsub(" ","",sample['Haul Code']))
  } else {
    ret$msg = "Invalid Haul Code"
    ret$HaulCode = as.numeric(gsub(" ","",sample['Haul Code']))
  }

  #SampleNumber
  ret$SampleNumber <- as.numeric(gsub(" ","",sample['Sample Number']))
  
  #species must be on meta data list
  i<-validSpecies(sample['Species'],species)
  if (!is.na(i$err)) {
    ret$msg = i$err
    ret$Species = sample['Species']
  } else {
    ret$SpeciesCode = i$code
    ret$Species = i$name
  }
  
  #Length
  ret$Length = as.numeric(gsub(" ","",sample['Length']))

  #Number
  ret$Number = as.numeric(gsub(" ","",sample['Number']))

  #Weight
  ret$Weight = as.numeric(gsub(" ","",sample['Weight']))
  
  #Age
  ret$Age = as.numeric(gsub(" ","",sample['Age']))

  #Sex
  ret$Sex = gsub(" ","",sample['Sex'])
  
  #Index Number
  ret$IndexNumber <- as.numeric(gsub(" ","",sample['Index Number']))
  
  #Maturity
  ret$Maturity <- sample['Maturity']
  
  ret
  
}

