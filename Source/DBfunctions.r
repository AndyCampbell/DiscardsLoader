#functions to upload data to SQL Server Discards DB

SQLsnippet<-function(vals,val,fields,name,quoted=FALSE,first=FALSE){
  if (!is.na(val)) {
    if (quoted) {
      
      #check for apostrophes (usually surnames)
      val<-gsub("'","''",val)
      
      if (first) {
        #quoted and first value
        ret <- c(paste0(fields,name),paste0(vals,"'",val,"'"))
      } else {
        #quoted and not first
        ret <- c(paste0(fields,",",name),paste0(vals,",'",val,"'"))
      }
    } else {
      if (first) {
        #not quoted and first
        ret <- c(paste0(fields,name),paste0(vals,val))
      } else {
        #not quoted and not first
        ret <- c(paste0(fields,",",name),paste0(vals,",",val))
      }
    }
  } else {
    ret <- c(fields,vals)
  }
  ret
}

uploadSample <- function(sample,Server,DB){

  #build SQL string
  fields <- "INSERT INTO [dbo].[SAMPLE] ("
  values <- "VALUES ("
  
  fields <- SQLsnippet(values,sample['SpeciesCode'],fields,"[Species_code]",quoted=TRUE,first=TRUE)[1]
  values <- SQLsnippet(values,sample['SpeciesCode'],fields,"[Species_code]",quoted=TRUE,first=TRUE)[2]
  
  fields <- SQLsnippet(values,sample['Species'],fields,"[Species]",quoted=TRUE)[1]
  values <- SQLsnippet(values,sample['Species'],fields,"[Species]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,sample['Length'],fields,"[Length]")[1]
  values <- SQLsnippet(values,sample['Length'],fields,"[Length]")[2]

  fields <- SQLsnippet(values,sample['Number'],fields,"[Number]")[1]
  values <- SQLsnippet(values,sample['Number'],fields,"[Number]")[2]
  
  fields <- SQLsnippet(values,sample['Weight'],fields,"[Weight]")[1]
  values <- SQLsnippet(values,sample['Weight'],fields,"[Weight]")[2]
  
  fields <- SQLsnippet(values,sample['Age'],fields,"[Age]")[1]
  values <- SQLsnippet(values,sample['Age'],fields,"[Age]")[2]
  
  fields <- SQLsnippet(values,sample['Sex'],fields,"[Sex]",quoted=TRUE)[1]
  values <- SQLsnippet(values,sample['Sex'],fields,"[Sex]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,sample['sample_header_id'],fields,"[sample_header_id]")[1]
  values <- SQLsnippet(values,sample['sample_header_id'],fields,"[sample_header_id]")[2]
  
  fields <- SQLsnippet(values,sample['IndexNumber'],fields,"[Index Number]")[1]
  values <- SQLsnippet(values,sample['IndexNumber'],fields,"[Index Number]")[2]

  fields <- SQLsnippet(values,sample['Maturity'],fields,"[Maturity]")[1]
  values <- SQLsnippet(values,sample['Maturity'],fields,"[Maturity]")[2]
    
  SQL <- paste0(fields,") ",values,")")  
  
  #cat(SQL,"\n")
  
  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",Server,"; Database=",DB))
  
  #do the insert
  ret<-sqlQuery(SQL.connect,SQL)
  
  #get back the new sample_id
  #id<-sqlQuery(SQL.connect,paste0("SELECT sample_id",
  #                                " FROM [dbo].[SAMPLe] WHERE [sample_header_id] = ",
  #                                as.character(sample['sample_header_id'])))
  
  #close the connection
  odbcClose(SQL.connect)
  
  #return the results of the upload
  ret
  
}

uploadSampleHeader <- function(header,Server,DB){

  #build SQL string
  fields <- "INSERT INTO [dbo].[SAMPLE-HEADEr] ("
  values <- "VALUES ("
  
  fields <- SQLsnippet(values,header['CruiseCode'],fields,"[Cruise code]",quoted=TRUE,first=TRUE)[1]
  values <- SQLsnippet(values,header['CruiseCode'],fields,"[Cruise code]",quoted=TRUE,first=TRUE)[2]
  
  fields <- SQLsnippet(values,header['HaulCode'],fields,"[Haul code]")[1]
  values <- SQLsnippet(values,header['HaulCode'],fields,"[Haul code]")[2]

  fields <- SQLsnippet(values,header['SampleNumber'],fields,"[Sample Number]")[1]
  values <- SQLsnippet(values,header['SampleNumber'],fields,"[Sample Number]")[2]

  fields <- SQLsnippet(values,header['Quantity'],fields,"[Quantity]")[1]
  values <- SQLsnippet(values,header['Quantity'],fields,"[Quantity]")[2]

  fields <- SQLsnippet(values,header['Units'],fields,"[Units]")[1]
  values <- SQLsnippet(values,header['Units'],fields,"[Units]")[2]
  
  fields <- SQLsnippet(values,header['Condition'],fields,"[Condition]",quoted=TRUE)[1]
  values <- SQLsnippet(values,header['Condition'],fields,"[Condition]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,header['Type'],fields,"[Type]",quoted=TRUE)[1]
  values <- SQLsnippet(values,header['Type'],fields,"[Type]",quoted=TRUE)[2]

  if (!is.na(header['NFD'])) {
    if (!header['NFD']=="NA") {
      if (!header['NFD']=="") {
        fields <- SQLsnippet(values,header['NFD'],fields,"[NFD]")[1]
        values <- SQLsnippet(values,header['NFD'],fields,"[NFD]")[2]
      }
    }
  }
  
  fields <- SQLsnippet(values,header['Grade_id'],fields,"[GradeID]")[1]
  values <- SQLsnippet(values,header['Grade_id'],fields,"[GradeID]")[2]
  
  fields <- SQLsnippet(values,header['DataType'],fields,"[Data Type]")[1]
  values <- SQLsnippet(values,header['DataType'],fields,"[Data Type]")[2]
  
  SQL <- paste0(fields,") ",values,")")  
  
#  cat(SQL,"\n")
  
  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",Server,"; Database=",DB))
  
  #do the insert
  sqlQuery(SQL.connect,SQL)
  
  #get back the new sample_header_id
  id<-sqlQuery(SQL.connect,paste0("SELECT [Cruise code],[Haul code],[Sample Number],[sample_header_id]",
                                  " FROM [dbo].[SAMPLE-HEADEr] WHERE [Cruise Code] = '",
                                  as.character(header['CruiseCode']),
                                  "' AND [Haul code] = ",
                                  as.character(header['HaulCode']),
                                  " AND [Sample Number] = ",
                                  as.character(header['SampleNumber'])))

  id$SQL <- SQL
  
  #close the connection
  odbcClose(SQL.connect)
  
  #return the results of the upload
  #return(list("id"=id,"sql"=SQL))
  id

}


uploadLanding <- function(landing,Server,DB){

  #build SQL string
  fields <- "INSERT INTO [dbo].[BULk] ("
  values <- "VALUES ("

  fields <- SQLsnippet(values,landing['CruiseCode'],fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[1]
  values <- SQLsnippet(values,landing['CruiseCode'],fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[2]
  
  fields <- SQLsnippet(values,landing['Haul'],fields,"[Haul]")[1]
  values <- SQLsnippet(values,landing['Haul'],fields,"[Haul]")[2]

  fields <- SQLsnippet(values,landing['SpeciesCode'],fields,"[B-Species_code]",quoted=TRUE)[1]
  values <- SQLsnippet(values,landing['SpeciesCode'],fields,"[B-Species_code]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,landing['Species'],fields,"[B-Species]",quoted=TRUE)[1]
  values <- SQLsnippet(values,landing['Species'],fields,"[B-Species]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,landing['Quantity'],fields,"[B-Quantity]")[1]
  values <- SQLsnippet(values,landing['Quantity'],fields,"[B-Quantity]")[2]
  
  fields <- SQLsnippet(values,landing['State'],fields,"[State]",quoted=TRUE)[1]
  values <- SQLsnippet(values,landing['State'],fields,"[State]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,landing['Factor'],fields,"[Factor]")[1]
  values <- SQLsnippet(values,landing['Factor'],fields,"[Factor]")[2]

  fields <- SQLsnippet(values,landing['Type'],fields,"[Fish Type]",quoted=TRUE)[1]
  values <- SQLsnippet(values,landing['Type'],fields,"[Fish Type]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,landing['Grade_id'],fields,"[GradeID]")[1]
  values <- SQLsnippet(values,landing['Grade_id'],fields,"[GradeID]")[2]
  
  SQL <- paste0(fields,") ",values,")")  
  
  #cat(SQL,"\n")
  
  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",Server,"; Database=",DB))
  
  #do the insert
  ret<-sqlQuery(SQL.connect,SQL)
  
  #get back the new bulk_id
  id<-sqlQuery(SQL.connect,paste0("SELECT bulk_id FROM [dbo].[BULk] WHERE [Cruise Code] = '",
                                  as.character(landing['CruiseCode']),
                                  "' AND [HAUl] = ",
                                  as.character(landing['Haul']),
                                  " AND [B-Species_code] = '",
                                  as.character(landing['SpeciesCode']),
                                  "'"))
  
  #close the connection
  odbcClose(SQL.connect)
  
  #return the results
  id
    
  
}

uploadHaul <- function(haul,Server,DB){

  #build SQL string
  fields <- "INSERT INTO [dbo].[HAUl] ("
  values <- "VALUES ("

  fields <- SQLsnippet(values,haul['CruiseCode'],fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[1]
  values <- SQLsnippet(values,haul['CruiseCode'],fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[2]
  
  fields <- SQLsnippet(values,haul['Haul'],fields,"[Haul]")[1]
  values <- SQLsnippet(values,haul['Haul'],fields,"[Haul]")[2]

  fields <- SQLsnippet(values,haul['ICES_id'],fields,"[ICES_id]")[1]
  values <- SQLsnippet(values,haul['ICES_id'],fields,"[ICES_id]")[2]

  fields <- SQLsnippet(values,haul['ICESDiv'],fields,"[ICES DIV]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['ICESDiv'],fields,"[ICES DIV]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['Gear_id'],fields,"[Gear_id]")[1]
  values <- SQLsnippet(values,haul['Gear_id'],fields,"[Gear_id]")[2]

  fields <- SQLsnippet(values,haul['GearCode'],fields,"[Gear Code]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['GearCode'],fields,"[Gear Code]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,haul['Mesh'],fields,"[Mesh size]")[1]
  values <- SQLsnippet(values,haul['Mesh'],fields,"[Mesh size]")[2]

  fields <- SQLsnippet(values,haul['Success'],fields,"[Success code]")[1]
  values <- SQLsnippet(values,haul['Success'],fields,"[Success code]")[2]

  fields <- SQLsnippet(values,haul['TimeShot'],fields,"[Time shot]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['TimeShot'],fields,"[Time shot]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['DepthShot'],fields,"[Depth shot]")[1]
  values <- SQLsnippet(values,haul['DepthShot'],fields,"[Depth shot]")[2]

  fields <- SQLsnippet(values,haul['LatShotDeg'],fields,"[Lat shot deg]")[1]
  values <- SQLsnippet(values,haul['LatShotDeg'],fields,"[Lat shot deg]")[2]

  fields <- SQLsnippet(values,haul['LatShotMin'],fields,"[Lat shot mins]")[1]
  values <- SQLsnippet(values,haul['LatShotMin'],fields,"[Lat shot mins]")[2]

  fields <- SQLsnippet(values,haul['LatShotSign'],fields,"[Lat shot sign]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['LatShotSign'],fields,"[Lat shot sign]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['LonShotDeg'],fields,"[Long shot deg]")[1]
  values <- SQLsnippet(values,haul['LonShotDeg'],fields,"[Long shot deg]")[2]
  
  fields <- SQLsnippet(values,haul['LonShotMin'],fields,"[Long shot mins]")[1]
  values <- SQLsnippet(values,haul['LonShotMin'],fields,"[Long shot mins]")[2]
  
  fields <- SQLsnippet(values,haul['LonShotSign'],fields,"[Long shot sign]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['LonShotSign'],fields,"[Long shot sign]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['DateShot'],fields,"[Shoot Date]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['DateShot'],fields,"[Shoot Date]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,haul['TimeHaul'],fields,"[Time hauled]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['TimeHaul'],fields,"[Time hauled]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['DepthHaul'],fields,"[Depth Hauled]")[1]
  values <- SQLsnippet(values,haul['DepthHaul'],fields,"[Depth Hauled]")[2]

  fields <- SQLsnippet(values,haul['LatHaulDeg'],fields,"[Lat hauled deg]")[1]
  values <- SQLsnippet(values,haul['LatHaulDeg'],fields,"[Lat hauled deg]")[2]
  
  fields <- SQLsnippet(values,haul['LatHaulMin'],fields,"[Lat hauled mins]")[1]
  values <- SQLsnippet(values,haul['LatHaulMin'],fields,"[Lat hauled mins]")[2]
  
  fields <- SQLsnippet(values,haul['LatHaulSign'],fields,"[Lat hauled sign]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['LatHaulSign'],fields,"[Lat hauled sign]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['LonHaulDeg'],fields,"[Long hauled deg]")[1]
  values <- SQLsnippet(values,haul['LonHaulDeg'],fields,"[Long hauled deg]")[2]
  
  fields <- SQLsnippet(values,haul['LonHaulMin'],fields,"[Long hauled mins]")[1]
  values <- SQLsnippet(values,haul['LonHaulMin'],fields,"[Long hauled mins]")[2]
  
  fields <- SQLsnippet(values,haul['LonHaulSign'],fields,"[Long hauled sign]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['LonHaulSign'],fields,"[Long hauled sign]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['Catch'],fields,"[Catch]")[1]
  values <- SQLsnippet(values,haul['Catch'],fields,"[Catch]")[2]

  fields <- SQLsnippet(values,haul['CatchUnits'],fields,"[CatchUnits]")[1]
  values <- SQLsnippet(values,haul['CatchUnits'],fields,"[CatchUnits]")[2]

  #fields <- SQLsnippet(values,haul['EA'],fields,"[EA]")[1]
  #values <- SQLsnippet(values,haul['EA'],fields,"[EA]")[2]

  fields <- SQLsnippet(values,haul['DateHaul'],fields,"[Haul Date]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['DateHaul'],fields,"[Haul Date]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,haul['YearHaul'],fields,"[Haul year]")[1]
  values <- SQLsnippet(values,haul['YearHaul'],fields,"[Haul year]")[2]

  fields <- SQLsnippet(values,haul['Quarter'],fields,"[Quarter]")[1]
  values <- SQLsnippet(values,haul['Quarter'],fields,"[Quarter]")[2]

  fields <- SQLsnippet(values,haul['WindDir_id'],fields,"[WindDir_id]")[1]
  values <- SQLsnippet(values,haul['WindDir_id'],fields,"[WindDir_id]")[2]

  fields <- SQLsnippet(values,haul['WindDir'],fields,"[Wind direction]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['WindDir'],fields,"[Wind direction]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['WindFor_id'],fields,"[WindForce_id]")[1]
  values <- SQLsnippet(values,haul['WindFor_id'],fields,"[WindForce_id]")[2]

  fields <- SQLsnippet(values,haul['WindFor'],fields,"[Wind force]")[1]
  values <- SQLsnippet(values,haul['WindFor'],fields,"[Wind force]")[2]
  
  fields <- SQLsnippet(values,haul['SeaSta_id'],fields,"[SeaState_id]")[1]
  values <- SQLsnippet(values,haul['SeaSta_id'],fields,"[SeaState_id]")[2]

  fields <- SQLsnippet(values,haul['SeaSta'],fields,"[Sea state]")[1]
  values <- SQLsnippet(values,haul['SeaSta'],fields,"[Sea state]")[2]

  fields <- SQLsnippet(values,haul['SwellDir_id'],fields,"[SwellDir_id]")[1]
  values <- SQLsnippet(values,haul['SwellDir_id'],fields,"[SwellDir_id]")[2]

  fields <- SQLsnippet(values,haul['SwellDir'],fields,"[Swell direction]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['SwellDir'],fields,"[Swell direction]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['SeaSwe_id'],fields,"[SeaSwell_id]")[1]
  values <- SQLsnippet(values,haul['SeaSwe_id'],fields,"[SeaSwell_id]")[2]

  fields <- SQLsnippet(values,haul['SeaSwe'],fields,"[Sea swell]")[1]
  values <- SQLsnippet(values,haul['SeaSwe'],fields,"[Sea swell]")[2]
  
  fields <- SQLsnippet(values,haul['GndTyp_id'],fields,"[GroundType_id]")[1]
  values <- SQLsnippet(values,haul['GndTyp_id'],fields,"[GroundType_id]")[2]

  fields <- SQLsnippet(values,haul['GndTyp'],fields,"[Ground type code]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['GndTyp'],fields,"[Ground type code]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['FshGrn_id'],fields,"[FishingGround_id]")[1]
  values <- SQLsnippet(values,haul['FshGrn_id'],fields,"[FishingGround_id]")[2]

  fields <- SQLsnippet(values,haul['FshGrn'],fields,"[Fishing ground code]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['FshGrn'],fields,"[Fishing ground code]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['GearCodeStd'],fields,"[Gear Code Standard]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['GearCodeStd'],fields,"[Gear Code Standard]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['Metier'],fields,"[Metier]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['Metier'],fields,"[Metier]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,haul['Memo'],fields,"[Memo]",quoted=TRUE)[1]
  values <- SQLsnippet(values,haul['Memo'],fields,"[Memo]",quoted=TRUE)[2]
    
  SQL <- paste0(fields,") ",values,")")  
  
  #cat(SQL,"\n")
  
  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",Server,"; Database=",DB))
  
  #do the insert
  sqlQuery(SQL.connect,SQL)
  
  #get back the new haul_id
  id<-sqlQuery(SQL.connect,paste0("SELECT haul_id FROM [dbo].[HAUl] WHERE [Cruise Code] = '",
                                  as.character(haul['CruiseCode']),
                                  "' AND [HAUl] = ",
                                  as.character(haul['Haul'])))
  #close the connection
  odbcClose(SQL.connect)
  
  #return the id
  return(list("id"=id,"sql"=SQL))
  
  }

uploadCruise <- function(cruise,Server,DB){
    
  #build SQL string
  fields <- "INSERT INTO [dbo].[CRUISe] ("
  values <- "VALUES ("
  
  fields <- SQLsnippet(values,cruise$CruiseCode,fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[1]
  values <- SQLsnippet(values,cruise$CruiseCode,fields,"[Cruise Code]",quoted=TRUE,first=TRUE)[2]
  
  fields <- SQLsnippet(values,cruise$Type,fields,"[Type]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$Type,fields,"[Type]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$FSSVessel_id,fields,"[fss_vessel_id]")[1]
  values <- SQLsnippet(values,cruise$FSSVessel_id,fields,"[fss_vessel_id]")[2]
  
  fields <- SQLsnippet(values,cruise$BoatName,fields,"[Boat Name]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$BoatName,fields,"[Boat Name]",quoted=TRUE)[2]
  
  fields <- SQLsnippet(values,cruise$VesselLOA,fields,"[Vessel LOA]")[1]
  values <- SQLsnippet(values,cruise$VesselLOA,fields,"[Vessel LOA]")[2]
  
  fields <- SQLsnippet(values,cruise$VesselkW,fields,"[Vessel Kw]")[1]
  values <- SQLsnippet(values,cruise$VesselkW,fields,"[Vessel Kw]")[2]

  fields <- SQLsnippet(values,cruise$Segment,fields,"[Segment]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$Segment,fields,"[Segment]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$DepPort_id,fields,"[DepPort_id]")[1]
  values <- SQLsnippet(values,cruise$DepPort_id,fields,"[DepPort_id]")[2]

  fields <- SQLsnippet(values,cruise$DepPort,fields,"[Port of Departure]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$DepPort,fields,"[Port of Departure]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$ArrPort_id,fields,"[ArrPort_id]")[1]
  values <- SQLsnippet(values,cruise$ArrPort_id,fields,"[ArrPort_id]")[2]

  fields <- SQLsnippet(values,cruise$ArrPort,fields,"[Arrival Port]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$ArrPort,fields,"[Arrival Port]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$DepDate,fields,"[Departure Date]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$DepDate,fields,"[Departure Date]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$CompDate,fields,"[Completion Date]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$CompDate,fields,"[Completion Date]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$Memo,fields,"[Memo]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$Memo,fields,"[Memo]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$DateReceived,fields,"[Date received]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$DateReceived,fields,"[Date received]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$InputCompleted,fields,"[Input completed]")[1]
  values <- SQLsnippet(values,cruise$InputCompleted,fields,"[Input completed]")[2]

  fields <- SQLsnippet(values,cruise$InputBy_id,fields,"[InputBy_id]")[1]
  values <- SQLsnippet(values,cruise$InputBy_id,fields,"[InputBy_id]")[2]

  fields <- SQLsnippet(values,cruise$InputBy,fields,"[Data inputted by]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$InputBy,fields,"[Data inputted by]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$ValidatedBy_id,fields,"[ValidatedBy_id]")[1]
  values <- SQLsnippet(values,cruise$ValidatedBy_id,fields,"[ValidatedBy_id]")[2]

  fields <- SQLsnippet(values,cruise$ValidatedBy,fields,"[Data validated by]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$ValidatedBy,fields,"[Data validated by]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$ReportedBy_id,fields,"[ReportedBy_id]")[1]
  values <- SQLsnippet(values,cruise$ReportedBy_id,fields,"[ReportedBy_id]")[2]
 
  fields <- SQLsnippet(values,cruise$ReportRequired,fields,"[Report required]")[1]
  values <- SQLsnippet(values,cruise$ReportRequired,fields,"[Report required]")[2]
 
  fields <- SQLsnippet(values,cruise$DateReported,fields,"[Date reported]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$DateReported,fields,"[Date reported]",quoted=TRUE)[2]
 
  fields <- SQLsnippet(values,cruise$ReportedBy,fields,"[Reported by]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$ReportedBy,fields,"[Reported by]",quoted=TRUE)[2]
   
  fields <- SQLsnippet(values,cruise$BoxBrought,fields,"[BoxBroughtToLabForAgeing]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$BoxBrought,fields,"[BoxBroughtToLabForAgeing]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$Cod,fields,"[Cod]")[1]
  values <- SQLsnippet(values,cruise$Cod,fields,"[Cod]")[2]

  fields <- SQLsnippet(values,cruise$Hake,fields,"[Hake]")[1]
  values <- SQLsnippet(values,cruise$Hake,fields,"[Hake]")[2]

  fields <- SQLsnippet(values,cruise$B_Sole,fields,"[B_Sole]")[1]
  values <- SQLsnippet(values,cruise$B_Sole,fields,"[B_Sole]")[2]

  fields <- SQLsnippet(values,cruise$L_Bud,fields,"[L_Bud]")[1]
  values <- SQLsnippet(values,cruise$L_Bud,fields,"[L_Bud]")[2]

  fields <- SQLsnippet(values,cruise$Had,fields,"[Had]")[1]
  values <- SQLsnippet(values,cruise$Had,fields,"[Had]")[2]
  
  fields <- SQLsnippet(values,cruise$Whg,fields,"[Whg]")[1]
  values <- SQLsnippet(values,cruise$Whg,fields,"[Whg]")[2]

  fields <- SQLsnippet(values,cruise$Meg,fields,"[Meg]")[1]
  values <- SQLsnippet(values,cruise$Meg,fields,"[Meg]")[2]
  
  fields <- SQLsnippet(values,cruise$L_Pisc,fields,"[L_Pisc]")[1]
  values <- SQLsnippet(values,cruise$L_Pisc,fields,"[L_Pisc]")[2]

  fields <- SQLsnippet(values,cruise$Plaice,fields,"[Plaice]")[1]
  values <- SQLsnippet(values,cruise$Plaice,fields,"[Plaice]")[2]

  fields <- SQLsnippet(values,cruise$Saithe,fields,"[Saithe]")[1]
  values <- SQLsnippet(values,cruise$Saithe,fields,"[Saithe]")[2]
  
  fields <- SQLsnippet(values,cruise$L_Sole,fields,"[L_Sole]")[1]
  values <- SQLsnippet(values,cruise$L_Sole,fields,"[L_Sole]")[2]

  fields <- SQLsnippet(values,cruise$Reason,fields,"[ReasonsDiscardsNotBroughtBackToLab]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$Reason,fields,"[ReasonsDiscardsNotBroughtBackToLab]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$Metier,fields,"[Metier]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$Metier,fields,"[Metier]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$CruiseCodeOrig,fields,"[Cruise Code Original]",quoted=TRUE)[1]
  values <- SQLsnippet(values,cruise$CruiseCodeOrig,fields,"[Cruise Code Original]",quoted=TRUE)[2]

  fields <- SQLsnippet(values,cruise$Crew_id,fields,"[Crew_id]")[1]
  values <- SQLsnippet(values,cruise$Crew_id,fields,"[Crew_id]")[2]
  
  SQL <- paste0(fields,") ",values,")")  
  
  #cat(SQL,"\n")

  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",Server,"; Database=",DB))
  
  #do the insert
  ret<-sqlQuery(SQL.connect,SQL)

  #get back the new cruise_id
  id<-sqlQuery(SQL.connect,paste0("SELECT cruise_id FROM [dbo].[CRUISe] WHERE [Cruise Code] = '",
                                  cruise$CruiseCode,"'"))
  
  #close the connection
  odbcClose(SQL.connect)
  
  #return the result
  id
  
}

cruiseDetails <- function(cruise,SQLServer,DB){

  #return the number of records in the DB for the specified cruise
  #looks in tables CRUISE, HAUL, BULK, SAMPLE-HEADER and SAMPLE

  ret <- data.frame("Cruise"=NA,"Haul"=NA,"Bulk"=NA,"SampleHeader"=NA,"Sample"=NA)
  
  #make a connection
  SQL.connect <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",SQLServer,"; Database=",DB))
  
  SQL.Cruise <- paste0("SELECT COUNT(1) FROM [dbo].[CRUISe] WHERE [Cruise Code] = '",
                       as.character(cruise),
                       "'")
    
  SQL.Haul <- paste0("SELECT COUNT(1) FROM [dbo].[HAUl] WHERE [Cruise Code] = '",
                    as.character(cruise),
                    "'")
  
  SQL.Bulk <- paste0("SELECT COUNT(1) FROM [dbo].[BULk] WHERE [Cruise Code] = '",
                    as.character(cruise),
                    "'")
  
  SQL.SampleHeader <- paste0("SELECT COUNT(1) FROM [dbo].[SAMPLE-HEADEr] WHERE [Cruise code] = '",
                            as.character(cruise),
                            "'")
  
  SQL.Sample <- paste0("SELECT COUNT(1) FROM [dbo].[SAMPLe] WHERE sample_header_id IN ",
                       "(SELECT DISTINCT sample_header_id FROM [dbo].[SAMPLE-HEADEr] WHERE ",
                       "[Cruise code] = '",
                       as.character(cruise),
                       "')")
  
  #do the queries
  ret['Cruise'] <- sqlQuery(SQL.connect,SQL.Cruise)
  ret['Haul'] <- sqlQuery(SQL.connect,SQL.Haul)
  ret['Bulk'] <- sqlQuery(SQL.connect,SQL.Bulk)
  ret['SampleHeader'] <- sqlQuery(SQL.connect,SQL.SampleHeader)
  ret['Sample'] <- sqlQuery(SQL.connect,SQL.Sample)
  
  #close the connection
  odbcClose(SQL.connect)
  
  #return the results
  ret
  
}