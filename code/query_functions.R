#############################################################
#
#   Program: query_functions.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for logic checks
#            missing values, and out of range.
#
#   INPUT: 
#   OUTPUT: standard R functions
#
#   Notes: File should be sourced by the query checks.
#
#   Created: 31 October 2012
#   Revisions: 
#     
#############################################################

## WRITE FUNCTION TO CHECK FOR VARIABLES THAT SHOULD NOT BE THERE 
extravar <- function(expectednames,table){
  datatable <- get(table)
  vars <- datatable[!(names(datatable) %in% expectednames)]
  if(ncol(vars)>0){
    query <- data.frame("",tablename,names(vars),
    										"Table Structure",
    										"Unexpected Variable","",
    										stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
    assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
  }
  check <- c(get("allcheck"),paste("Table Structure","Unexpected Variable",tablename,"",sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR VARIABLES THAT SHOULD BE THERE
missvar <- function(expectednames,table){
  datatable <- get(table)
  vars <- expectednames[!(expectednames %in% names(datatable))]
  if(length(vars)>0){
    query <- data.frame("",tablename,vars,
    										"Table Structure",
    										"Missing Variable","",
    										stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
    assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
  }
  check <- c(get("allcheck"),paste("Table Structure","Missing Variable",tablename,"",sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR MISSING PATIENT VALUES
missingvalue <- function(var,table){
datatable <- get(table)
if(exists(var,datatable)){
    if(!exists(paste(var,"_a",sep=""),datatable)){
	if(any(is.na(get(var,datatable)))){
	  query <- data.frame(datatable$patient[is.na(get(var,datatable))],
											  tablename,
											  var,
											  "Missing Value",
											  "Missing Value","",
											  stringsAsFactors=FALSE)
	  names(query) <- names(emptyquery)
	  assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
	}
      }
    ## THIS SECOND LOOP IS PERTINENT FOR DATE VARIABLES WITH FLAG THAT THE DATE IS UNKNOWN *_D_A="U"
    if(exists(paste(var,"_a",sep=""),datatable)){
	if(any(!(!is.na(get(paste(var,"_a",sep=""),datatable)) & get(paste(var,"_a",sep=""),datatable)=="U") & is.na(get(var,datatable)))){
	  miserr <- !(!is.na(get(paste(var,"_a",sep=""),datatable)) & get(paste(var,"_a",sep=""),datatable)=="U") & is.na(get(var,datatable))
	  query <- data.frame(datatable$patient[miserr],
											  tablename,
											  var,
											  "Missing Value",
											  "Missing Value","",
											  stringsAsFactors=FALSE)
	  names(query) <- names(emptyquery)
	  assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
	}
      }
  }
  check <- c(get("allcheck"),paste("Missing Value","Missing Value",tablename,var,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR DATES OCCURRING IN FUTURE
futuredate <- function(var,table){
  datatable <- get(table)
  if(exists(var,datatable)){
    futerr <- !is.na(get(var,datatable)) & get(var,datatable)>databaseclose
    ## IN THE CASE THAT A FLAG HAS BEEN PROVIDED FOR DATE VARIABLES, THEN CHECK FOR APPROPRIATE WINDOW AND DO NOT QUERY UNKNOWN DATES
    if(exists(paste(var,"_a",sep=""),datatable)){
      flag <- get(paste(var,"_a",sep=""),datatable)
      futerr[!is.na(flag) & flag=="Y" & databaseclose - as.numeric(get(var,datatable)) > -365] <- FALSE
      futerr[!is.na(flag) & flag=="M" & databaseclose - as.numeric(get(var,datatable)) > -183] <- FALSE
      futerr[!is.na(flag) & flag=="D" & databaseclose - as.numeric(get(var,datatable)) > -16] <- FALSE
      futerr[!is.na(flag) & flag=="U"] <- FALSE
      futerr[!is.na(flag) & flag=="<"] <- FALSE
    }
    query <- emptyquery
    if(any(futerr)){query<-data.frame(datatable$patient[futerr],
    																	tablename,var,"Logic",
    																	"Date in the Future",
    																	paste("databaseclose=",format(as.Date(databaseclose,origin="1970-01-01"),format="%Y-%m-%d"),"&",var,"=",get(var,datatable)[futerr],sep=""),
    																	stringsAsFactors=FALSE)
    }
    names(query) <- names(emptyquery)
    if(nrow(query)>0){assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1}
  }
  check <- c(get("allcheck"),paste("Logic","Date in the Future",tablename,var,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR UNEXPECTED CODES
badcodes <- function(var,codelist,table){
  datatable <- get(table)
  if(exists(var,datatable)){
    coderr <- !is.na(get(var,datatable)) & !(get(var,datatable) %in% codelist)
    if(any(coderr)){
      query<-data.frame(datatable$patient[coderr],
      									tablename,var,"Value Error",
      									"Unexpected Code",
      									paste(var,"=",get(var,datatable)[coderr],sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
  check <- c(get("allcheck"),paste("Value Error","Unexpected Code",tablename,var,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR DUPLICATES OF UNIQUEID
queryduplicates <- function(uniqueid,table,date=date,subsettext=""){
  datatable <- get(table)
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(uniqueid,datatable)){
  	if(!missing(date)){id <- paste(get(uniqueid,datatable),"&",get(date,datatable))}
  	if(missing(date)){id <- get(uniqueid,datatable)}
    if(any(duplicated(id))){
      duperr <- duplicated(id)
      if(missing(date)){
      	query <- data.frame(datatable$patient[duperr],
      										tablename,
      										"patient",
      										"Logic",
      										"Duplicate Record",
      										paste("patient =",datatable$patient[duperr]),
      										stringsAsFactors=FALSE)
      }
      	 if(!missing(date)){
      	 	query <- data.frame(datatable$patient[duperr],
      	 											tablename,
      	 											ifelse(!is.na(subvar),paste("patient&",date,subvar,sep=""),
      	 														 paste("patient&",date,sep="")),
      	 											"Logic",
      	 											"Duplicate Record",
      	 											paste("patient=",datatable$patient[duperr],"&",
      	 														date,"=",get(date,datatable)[duperr],subsettext,sep=""),
      	 											stringsAsFactors=FALSE)
      	 }      	 
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
    }
  }
  thevars <- ifelse(!missing(date),paste("patient &", date),"patient")
  thevars <- ifelse(!is.na(subvar),paste(thevars,subvar),thevars)
  check <- c(get("allcheck"),paste("Logic","Duplicate Record",tablename,thevars,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION FOR OUT OF ORDER DATES, DATE2 IS SUPPOSED TO OCCUR ON OR AFTER DATE1
outoforder <- function(date1,date2,table,table2=table2){
	if(!missing(table2)){tablename <- paste(tablename,"&",table2)}
  datatable <- get(table)
  if(exists(date1,datatable) & exists(date2,datatable)){
     logicerr <- !is.na(get(date1,datatable)) & !is.na(get(date2,datatable)) & get(date1,datatable) > get(date2,datatable)
     if(exists(paste(date1,"_a",sep=""),datatable)){
	flag1 <- get(paste(date1,"_a",sep=""),datatable)
	logicerr[!is.na(flag1) & flag1 %in% c("U",">")] <- FALSE
     }
     if(exists(paste(date2,"_a",sep=""),datatable)){
	flag2 <- get(paste(date2,"_a",sep=""),datatable)
	logicerr[!is.na(flag2) & flag2 %in% c("U","<")] <- FALSE
     }
     if(any(logicerr)){
      query <- data.frame(datatable$patient[logicerr],
     										 tablename,paste(date1,"&",date2,sep=""),
     										 "Logic",
     										 "Out of Order",
     										 paste(date1,"=",get(date1,datatable)[logicerr],"&",date2,"=",get(date2,datatable)[logicerr],sep=""),
     										 stringsAsFactors=FALSE)
     names(query) <- names(emptyquery)
     assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
     }
  }
  check <- c(get("allcheck"),paste("Logic","Out of Order",tablename,paste(date1,"&",date2,sep=""),sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## CONVERT DATE VARIABLES AND PERFORM LOGIC CHECK AGAINST DATE OF BIRTH
convertdate <- function(date,table){
  datatable <- get(table)
  if(exists(date,datatable)){
    var <- as.Date(get(date,datatable),"%Y-%m-%d")
  }
  if(!exists(date,datatable)){
    var <- NULL
  }
  return(var)
}
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
upperrangecheck <- function(var,value,table,subsettext="",ptlevel=TRUE){
  datatable <- get(table)
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(var,datatable)){
    coderr <- !is.na(get(var,datatable)) & get(var,datatable) > value
    if(any(coderr)){
      query<-data.frame(ifelse(ptlevel,datatable$patient[coderr],datatable$center[coderr]),
      									tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
      									"Out of Range",
      									paste(var,"=",get(var,datatable)[coderr],subsettext,sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
  thevars <- ifelse(!is.na(subvar),paste(var,subvar,sep=" "),var)
  check <- c(get("allcheck"),paste("Logic","Out of Range",tablename,thevars,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
lowerrangecheck <- function(var,value,table,subsettext="",ptlevel=TRUE){
  datatable <- get(table)
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(var,datatable)){
    coderr <- !is.na(get(var,datatable)) & get(var,datatable) < value
    if(any(coderr)){
      query<-data.frame(ifelse(ptlevel,datatable$patient[coderr],datatable$center[coderr]),
      									tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
      									"Out of Range",
      									paste(var,"=",get(var,datatable)[coderr],subsettext,sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
  thevars <- ifelse(!is.na(subvar),paste(var,subvar,sep=" "),var)
  check <- c(get("allcheck"),paste("Logic","Out of Range",tablename,thevars,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notnumeric <- function(var,table){
  datatable <- get(table)
  if(exists(var,datatable)){
    numerr <- grepl("[:alpha:]",get(var,datatable))
    if(any(numerr)){
      query<-data.frame(datatable$patient[numerr],
      									tablename,var,"Value Error",
      									"Unexpected Type",
      									paste(var,"=",get(var,datatable)[numerr],"&type=numeric",sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
  check <- c(get("allcheck"),paste("Value Error","Unexpected Type",tablename,var,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
## FORCE NUMBER will take a factor/character to a number without issuing the warnings
forcenumber <- function(var){
    options(warn = -1)
    x <- as.numeric(gsub("[^0-9.]","",as.character(var)))
    options(warn = 1)
    return(x)
}

## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notdate <- function(var,table){
  datatable <- get(table)
  if(exists(var,datatable)){
    datavar <- get(var,datatable)
    numerr <- !is.na(datavar) & !(grepl("[0-9]",substr(datavar,1,4)) & grepl("-",substr(datavar,5,5)) & grepl("[0-9]",substr(datavar,6,7)) & grepl("-",substr(datavar,8,8)) & grepl("[0-9]",substr(datavar,9,10)))
    if(any(numerr)){
      query<-data.frame(datatable$patient[numerr],
      									tablename,var,"Value Error",
      									"Unexpected Type",
      									paste(var,"=",datavar[numerr],"&type=date",sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
  check <- c(get("allcheck"),paste("Value Error","Unexpected Type",tablename,var,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}

## WRITE FUNCTION TO CHECK FOR RECORDS THAT EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
badrecord <- function(uniqueid,subset,superset,table,subsettext=""){
  subset <- get(subset)
  superset <- get(superset)
  subvar <- unlist(strsplit(subsettext,"="))[1]
  subvar <- gsub("!","",subvar)
  if(exists(uniqueid,subset) & exists(uniqueid,superset)){
    if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
      recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
      	query <- data.frame(get("patient",subset)[recerr],
      										tablename,
      										ifelse(!is.na(subvar),paste(uniqueid,subvar,sep=""),uniqueid),
      										"Logic",
      										"Unexpected Record",
      										paste(paste(uniqueid,"=",get(uniqueid,subset)[recerr]),subsettext,sep=""),
      										stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
    }
  }
  thevars <- ifelse(!is.na(subvar),paste(uniqueid,subvar),uniqueid)
  check <- c(get("allcheck"),paste("Logic","Unexpected Record",tablename,thevars,sep=" and "))
  assign("allcheck",check,envir=globalenv())
}
