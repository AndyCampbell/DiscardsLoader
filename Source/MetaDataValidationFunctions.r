#metadata fetch and validation functions

#valid Vessel 
validVessel <- function(name,CFR,vessels,date){
  
  #checks if the vessel is valid based on the name and CFR provided
  #issues a warning if the vessel LOA, KW differs from that provided - TO DO
  #must also be active on the date provided
  #use LOA to derive segment
  
  #cat("validVessel\n")
  #cat(date,"\n")
  
  if (is.na(name) | is.na(date)) {
    
    ret <- data.frame(name=NA,id=NA,LOA=NA,kW=NA,segment=NA)
    
  } else {
    
    v <- vessels[toupper(vessels$vessel_name)==toupper(name) 
                 & vessels$eff_from < date
                 & vessels$eff_to > date,]

    if (!nrow(v) == 1) {
      ret <- data.frame(name=NA,id=NA,LOA=NA,kW=NA,segment=NA,err='zero or multiple port records')  
    } else {
      
      if (v$LOA < 10) {
        seg <- '<10m'
      } else if (v$LOA < 12) {
        seg <- '10<12m'
      } else if (v$LOA < 24) {
        seg <- '12<24m' 
      } else if (v$LOA < 40) {
        seg <- '24<40m'
      } else if (v$LOA >= 40) {
        seg <- '>=40m'
      } else {
        seg <- ''
      }
      
      ret <- data.frame(name=v$vessel_name,
                        id=v$vessel_id,
                        LOA = v$LOA,
                        kW = v$kw_power,
                        segment = seg,
                        err=NA)
    }
    
  }
  
  ret
  
}

#valid Lat Sign
validLatSign <- function(sign){

  #TO DO - if a latitude is specified the sign mus be 'N' or 'S'
  
  #must be 'N' or 'S' or ''
  if ((sign=='N') | (sign=='S') | (sign=='') | is.na(sign)) {
    ret <- data.frame(sign=sign,err=NA)
  } else {
    ret <- data.frame(sign=sign,err="Sign must be N or S or blank")
  }
  
  ret
  
}

#valid Lon Sign
validLonSign <- function(sign){

  #TO DO - if a longitude is specified the sign mus be 'N' or 'S'
  
  #must be 'E' or 'W'
  if ((sign=='E') | (sign=='W') | (sign=='') | is.na(sign)) {
    ret <- data.frame(sign=sign,err=NA)
  } else {
    ret <- data.frame(sign=sign,err="Sign must be E or W or blank")
  }
  
  ret
  
}

#valid Mesh size
validMeshSize <- function(mesh){

  if (is.na(mesh)) {
    ret <- data.frame(mesh=NA,err=NA)
  } else {  
    #must be numeric
    if (!(is.na(suppressWarnings(as.numeric(mesh))))){
      ret <- data.frame(mesh=mesh,err=NA)
    } else {
      ret <- data.frame(mesh=mesh,err='Mesh Size must be numeric')
    }
  }
  
  ret
  
}

#valid ICES division
validICES <- function(name,ices){

  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    #i <- ices[ices$ices_code==name,]
    i <- ices[toupper(ices$ices_code)==toupper(name),]
    
    if (!nrow(i) == 1) {
      ret <- data.frame(name=NA,id=NA,err='zero or multiple ices division records')  
    } else {
      ret <- data.frame(name=i$ices_code,id=i$ices_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid gear code
validGear <- function(name,gears){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    g <- subset(gears,gear_abbrev==name)
    
    if (!nrow(g) == 1) {
      ret <- data.frame(name=NA,id=NA,err='zero or multiple ices division records')  
    } else {
      ret <- data.frame(name=g$gear_abbrev,id=g$gear_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid success code
validSuccessCode <- function(success_code,success_codes){
  
  if (is.na(success_code)){
    ret <- data.frame(code=NA,desc=NA,err=NA)    
  } else {
    
    sc <- subset(success_codes,code==as.numeric(success_code))
    
    if (!nrow(sc) == 1) {
      ret <- data.frame(code=NA,desc=NA,err='zero or multiple success codes')  
    } else {
      ret <- data.frame(code=sc$code,desc=sc$desc,err=NA)
    }
    
  }
  
  ret
  
}


#valid wind direction code
validWindDir <- function(name,wind){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(wind,toupper(wind_dir)==toupper(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Wind Direction')  
    } else {
      ret <- data.frame(name=w$wind_dir,id=w$dir_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid state
validState <- function(name,state){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(state,toupper(Fish_Condition)==toupper(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Fish Condition')  
    } else {
      ret <- data.frame(name=w$Fish_Condition,id=w$fishcondition_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid type
validType <- function(name,type){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(type,FishType==name)
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Fish Type')  
    } else {
      ret <- data.frame(name=w$FishType,id=w$fishtype_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid grade ID
validGradeID <- function(id,grade){
  
  if (is.na(id)){
    ret <- data.frame(id=NA,err=NA)    
  } else {
    
    id <- gsub(" ","",id)
    
    w <- subset(grade,GradeID==id)
    
    if (!nrow(w) == 1) {
      ret <- data.frame(id=NA,err='Invalid Grade ID')  
    } else {
      ret <- data.frame(id=w$GradeID,err=NA)
    }
    
  }
  
  ret
  
}


#valid grade
validGrade <- function(grade,grades){
  
  if (is.na(grade)){
    ret <- data.frame(id=NA,desc=NA,err=NA)    
  } else {
    
    grade <- gsub(" ","",grade)
    
    w <- subset(grades,toupper(gsub(" ","",GradeDescription))==toupper(grade))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(id=NA,desc=NA,err='Invalid Grade')  
    } else {
      ret <- data.frame(id=w$GradeID,desc=w$GradeDescription,err=NA)
    }
    
  }
  
  ret
  
}



#valid Data Type ID
validDataTypeID <- function(id,type){
  
  if (is.na(id)){
    ret <- data.frame(id=NA,err=NA)    
  } else {
    
    w <- subset(type,datatype_id==id)
    
    if (!nrow(w) == 1) {
      ret <- data.frame(id=NA,err='Invalid Data Type ID')  
    } else {
      ret <- data.frame(id=w$datatype_id,err=NA)
    }
    
  }
  
  ret
  
}


#valid Data Type
validDataType <- function(datatype,type){
  
  if (is.na(datatype)){
    ret <- data.frame(id=NA,err=NA)    
  } else {
    
    w <- subset(type,toupper(DataDescription)==toupper(datatype))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(id=NA,err='Invalid Data Type ID')  
    } else {
      ret <- data.frame(id=w$datatype_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid factor
validFactor <- function(species,condition,factor,hauldate,factors){
  
  #species - the dpecies to be checked - mandatory, error if not supplied
  #condition - the condition to be checked - mandatory, error if not supplied
  #factor - the assigned factor - optional, warning if not supplied
  #hauldate - the date the sample was recorded (haul date)
  #factors - data frame of factors from meta data - mandatory
  
  if (is.na(species)){
    
    ret <- data.frame(factor=NA,err='No species supplied for factor check')

  } else if (is.na(condition)) {

    ret <- data.frame(factor=NA,err='No condition supplied for factor check')

  } else if (is.na(hauldate)) {
  
    ret <- data.frame(factor=NA,err='No hauldate supplied for factor check')
    
  } else {
        
    w <- subset(factors,gsub(" ","",toupper(Species)) == gsub(" ","",toupper(species)) 
                & gsub(" ","",toupper(Condition)) == gsub(" ","",toupper(condition))
                & FromDate < hauldate
                & (ToDate > hauldate | is.na(ToDate)))
        
    if (!nrow(w) == 1) {
      ret <- data.frame(factor=NA,err=paste('Invalid Presentation Factor',nrow(w),'matching records in metadata',species,condition,hauldate))  
    } else {
      ret <- data.frame(factor=as.numeric(w$Factor),err=NA)
    }
    
  }
  
  ret
  
}

#valid swell direction code
validSwellDir <- function(name,swell){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(swell,swell_dir==toupper(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Swell Direction')  
    } else {
      ret <- data.frame(name=w$swell_dir,id=w$dir_id,err=NA)
    }
    
  }
  
  ret
  
}


#valid wind force code
validWindFor <- function(name,wind){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(wind,beaufort_scale==as.numeric(name))
  
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Wind Force')  
    } else {
      ret <- data.frame(name=w$beaufort_scale,id=w$force_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid sea state code
validSeaState <- function(name,seastate){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(seastate,state_code==as.numeric(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Sea State')  
    } else {
      ret <- data.frame(name=w$state_code,id=w$seastate_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid swell code
validSwell <- function(name,swell){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(swell,swell_code==as.numeric(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Sea Swell')  
    } else {
      ret <- data.frame(name=w$swell_code,id=w$swell_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid ground type code
validGroundType <- function(name,groundtype){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(groundtype,groundtype_code==name)
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Ground Type Code')  
    } else {
      ret <- data.frame(name=w$groundtype_code,id=w$groundtype_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid fishing ground
validGround <- function(name,ground){
  
  if (is.na(name)){
    ret <- data.frame(name=NA,id=NA,err=NA)    
  } else {
    
    w <- subset(ground,Fishing_ground_code==name)
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,err='Invalid Fishing Ground Code')  
    } else {
      ret <- data.frame(name=w$Fishing_ground_code,id=w$fishingground_id,err=NA)
    }
    
  }
  
  ret
  
}

#valid port
validPort <- function(name,ports){
  
  #checks if port name is valid
  #if so, returns ID. If not, ID will be NA

  if (is.na(name)) {

    ret <- data.frame(name=NA,id=NA,err=NA)
    
  } else {
    
    p <- ports[ports$portname == name,]

    if (!nrow(p) == 1) {
      ret <- data.frame(name=NA,id=NA,err='zero or multiple port records')  
    } else {
      ret <- data.frame(name=p$portname,id=p$port_id,err=NA)
    }
    
  }
  
  ret
  
}


#valid species
validSpecies <- function(name,species){
    
  if (is.na(name)) {  
    
    ret <- data.frame(name=NA,code=NA,id=NA,err=NA)
    
  } else {
    
    #trim any outer spaces
    name=gsub("(^ +)|( +$)", "", name)
    
    w <- subset(species,toupper(DiscardsSpeciesDisplay)==toupper(name))
    
    if (!nrow(w) == 1) {
      ret <- data.frame(name=NA,id=NA,code=NA,err='Invalid Species Name')  
    } else {
      ret <- data.frame(name=w$DiscardsSpeciesDisplay,
                        id=w$DiscardsSpeciesID,
                        code=w$Species_Code,
                        err=NA)
    }
  }
  
  ret
  
}


#valid personnel
validPersonnel <- function(name,date,personnel){ 

  #checks if a person of this name was active on the date specified
  #if so, returns the name and ID. If not, returns original object
    
  if (is.na(name) | is.na(date)) {
    
    ret <- data.frame(name=NA,id=NA,err=NA)

  } else {

      p <- personnel[personnel$full_name == name 
                   & personnel$eff_from < date
                   & personnel$eff_to > date,]

      if (!nrow(p) == 1) {
        ret <- data.frame(name=NA,id=NA,err='zero or multiple personnel records')  
      } else {
        ret <- data.frame(name=p$full_name,id=p$personnel_id,err=NA)
      }
  
  }
  
  ret
  
}

validCruiseType <- function(type,types){
  
  if (is.na(type)) {
    
    ret <- data.frame(type=NA,err=NA)

    } else {
    t <- types[types[,'Cruise Type'] == type,]
    
    if (!nrow(t) == 1) {
      ret <- data.frame(type=NA,id=NA,err='zero or multiple cruise type')  
    } else {
      ret <- data.frame(type=t[,'Cruise Type'],id=t$cruisetype_id,err=NA)
    }
    
  }
  
  ret
  
}

getMetaData <- function(SQLServer,DBName){
  
  #read in the meta data from the identitied DB
  
  #connect to SQL Server DB
  connect.string<-paste0("Driver=SQL Server; Server=",SQLServer,"; Database=",DBName)
  SQL.connect <- odbcDriverConnect(connect.string)
  
  #personnel data
  personnel.MD <- sqlQuery(SQL.connect,paste0("SELECT [personnel_id],[first_name],[last_name],[full_name],",
                                              "[grade],[institution],[eff_from],[eff_to],[stockman_id],[discards_fname],[discards_lname] FROM ",
                                              "dbo.FSSPersonnel"),stringsAsFactors=FALSE)
  
  personnel.MD$eff_from <- as.Date(personnel.MD$eff_from)
  personnel.MD$eff_to <- as.Date(personnel.MD$eff_to)
  
  #ports data
  ports.MD <- sqlQuery(SQL.connect,paste0("SELECT [port_id],[portname],[port_ctrycode],",
                                          "[stockman_portcode],[discard_portname] FROM dbo.FSSPort"),stringsAsFactors=FALSE)
  
  #vessels data
  vessels.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSVessel",stringsAsFactors=FALSE)
  vessels.MD$eff_from <- as.Date(vessels.MD$eff_from)
  vessels.MD$eff_to <- as.Date(vessels.MD$eff_to)
  
  #ICES area data
  ices.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSICES",stringsAsFactors=FALSE)
  
  #Gear data
  gear.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSGear",stringsAsFactors=FALSE)
  
  #Cruise Type data
  type.MD <- sqlQuery(SQL.connect,"Select * from dbo.CRUISETYPE",stringsAsFactors=FALSE)
  
  #Wind direction
  winddir.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSWindDir",stringsAsFactors=FALSE)
  
  #Wind force
  windfor.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSWindForce",stringsAsFactors=FALSE)
  
  #Sea State
  seastate.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSeaState",stringsAsFactors=FALSE)
  
  #Swell Direction
  swelldir.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSwellDir",stringsAsFactors=FALSE)
  
  #Swell
  swell.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSSwell",stringsAsFactors=FALSE)
  
  #Ground Type
  groundtype.MD <- sqlQuery(SQL.connect,"Select * from dbo.FSSGroundType",stringsAsFactors=FALSE)
  
  #Fishing Ground
  fishground.MD <- sqlQuery(SQL.connect,"Select * from dbo.FISHING_GROUND", stringsAsFactors=FALSE)
  names(fishground.MD) <- gsub(" ","_",names(fishground.MD))
  
  #Species
  species.MD <- sqlQuery(SQL.connect,"Select * from dbo.SpeciesLink",stringsAsFactors=FALSE)
  
  #Fish Condition
  condition.MD <- sqlQuery(SQL.connect,"Select * from dbo.[Fish Condition]",stringsAsFactors=FALSE)
  names(condition.MD) <- gsub(" ","_",names(condition.MD))
  
  #Fish Type 
  fishtype.MD <- sqlQuery(SQL.connect,"Select * from dbo.[Fish Type]", stringsAsFactors=FALSE)
  names(fishtype.MD) <- gsub(" ","",names(fishtype.MD))
  
  #Presentation Factors
  presfactors.MD <- sqlQuery(SQL.connect,"Select * from dbo.PresentationFactorLookups", stringsAsFactors=FALSE)
  
  #Grade
  grade.MD <- sqlQuery(SQL.connect,"Select * from dbo.GradeLUT",stringsAsFactors=FALSE)
  
  #Sample data type
  datatype.MD <- sqlQuery(SQL.connect,"Select * from dbo.DATATYPE",stringsAsFactors=FALSE)
  names(datatype.MD) <- gsub(" ","",names(datatype.MD))
  
  #close the connection
  odbcClose(SQL.connect)

  #non DB meta data
  
  #success codes
  successCode.MD <- data.frame(code=c(1,2,3,4,5),
                               desc=c("Successful - Fish measured",
                                      "Foul Haul - Fish measured or Negative Discards",
                                      "Successful - No Fish measured",
                                      "Foul Haul - No Fish measured",
                                      "Group 1 Species"))
  
  return(list("personnel.MD" = personnel.MD,
              "ports.MD" = ports.MD,
              "vessels.MD" = vessels.MD,
              "ices.MD" = ices.MD,
              "gear.MD" = gear.MD,
              "type.MD" = type.MD,
              "winddir.MD" = winddir.MD,
              "windfor.MD" = windfor.MD,
              "seastate.MD" = seastate.MD,
              "swelldir.MD" = swelldir.MD,
              "swell.MD" = swell.MD,
              "groundtype.MD" = groundtype.MD,
              "fishground.MD" = fishground.MD,
              "species.MD" = species.MD,
              "condition.MD" = condition.MD,
              "fishtype.MD" = fishtype.MD,
              "presfactors.MD" = presfactors.MD,
              "grade.MD" = grade.MD,
              "datatype.MD" = datatype.MD,
              "successCode.MD" = successCode.MD))
  
}




