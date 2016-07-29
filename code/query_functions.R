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
extravar <- function(expectednames,table=parent.frame()){
  vars <- table[!(names(table) %in% expectednames)]
  if(ncol(vars)>0){
    query <- data.frame("",tablename,names(vars),
    										"Table Structure",
    										"Unexpected Variable","",
    										stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
    assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
  }
}
## WRITE FUNCTION TO CHECK FOR VARIABLES THAT SHOULD BE THERE
missvar <- function(expectednames,table=parent.frame()){
  vars <- expectednames[!(expectednames %in% names(table))]
  if(length(vars)>0){
    query <- data.frame("",tablename,vars,
    										"Table Structure",
    										"Missing Variable","",
    										stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
    assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
  }
}
## WRITE FUNCTION TO CHECK FOR MISSING PATIENT VALUES
missingvalue <- function(var,table=parent.frame(),id=patient){
  var <- deparse(substitute(var))
  id <- deparse(substitute(id))
if(exists(var,table)){
    if(!exists(paste(var,"_a",sep=""),table)){
	if(any(is.na(get(var,table)))){
	  query <- data.frame(get(id,table)[is.na(get(var,table))],
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
    if(exists(paste(var,"_a",sep=""),table)){
	if(any(!(!is.na(get(paste(var,"_a",sep=""),table)) & get(paste(var,"_a",sep=""),table)=="U") & is.na(get(var,table)))){
	  miserr <- !(!is.na(get(paste(var,"_a",sep=""),table)) & get(paste(var,"_a",sep=""),table)=="U") & is.na(get(var,table))
	  query <- data.frame(get(id,table)[miserr],
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
}
## WRITE FUNCTION TO CHECK FOR DATES OCCURRING IN FUTURE
futuredate <- function(var,table=parent.frame(),id=patient){
  var <- deparse(substitute(var))
  if(exists(var,table)){
    futerr <- !is.na(get(var,table)) & get(var,table)>databaseclose
    ## IN THE CASE THAT A FLAG HAS BEEN PROVIDED FOR DATE VARIABLES, THEN CHECK FOR APPROPRIATE WINDOW AND DO NOT QUERY UNKNOWN DATES
    if(exists(paste(var,"_a",sep=""),table)){
      flag <- get(paste(var,"_a",sep=""),table)
      futerr[!is.na(flag) & flag=="Y" & databaseclose - as.numeric(get(var,table)) > -365] <- FALSE
      futerr[!is.na(flag) & flag=="M" & databaseclose - as.numeric(get(var,table)) > -183] <- FALSE
      futerr[!is.na(flag) & flag=="D" & databaseclose - as.numeric(get(var,table)) > -16] <- FALSE
      futerr[!is.na(flag) & flag=="U"] <- FALSE
      futerr[!is.na(flag) & flag=="<"] <- FALSE
    }
    query <- emptyquery
    if(any(futerr)){query<-data.frame(get(deparse(substitute(id)),table)[futerr],
    																	tablename,var,"Logic",
    																	"Date in the Future",
    																	paste("databaseclose=",format(as.Date(databaseclose,origin="1970-01-01"),format="%Y-%m-%d"),"&",var,"=",get(var,table)[futerr],sep=""),
    																	stringsAsFactors=FALSE)
    }
    names(query) <- names(emptyquery)
    if(nrow(query)>0){assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1}
  }
}
## WRITE FUNCTION TO CHECK FOR UNEXPECTED CODES
## LDR 2016-07-15 - this is the original badcodes function
##badcodes <- function(var,codelist,table=parent.frame(),id=patient){
##  var <- deparse(substitute(var))
##  if(exists(var,table)){
##    coderr <- !is.na(get(var,table)) & !(get(var,table) %in% codelist)
##    if(any(coderr)){
##      query<-data.frame(get(deparse(substitute(id)),table)[coderr],
##      									tablename,var,"Value Error",
##      									"Unexpected Code",
##      									paste(var,"=",get(var,table)[coderr],sep=""),
##      									stringsAsFactors=FALSE)
##      names(query) <- names(emptyquery)
##      assign(paste("query",index,sep=""),query,envir=globalenv())
##      index <<- index + 1
##    }
##  }
##}

## WRITE FUNCTION TO CHECK FOR UNEXPECTED CODES
## LDR 2016-07-15 - the badcodes function below has updates to allow for text for
##                  Auxiliary critera specified on subsetting the input table and
##                  also to allow for the error and query values to be overridden
badcodes <- function(var,codelist,table=parent.frame(),id=patient,auxcriteria=NA,error="Value Error",query="Unexpected Code"){
  var <- deparse(substitute(var))
  if(exists(var,table)){
    coderr <- !is.na(get(var,table)) & !(get(var,table) %in% codelist)
    print(coderr)
    if(any(coderr)){
      print(get(deparse(substitute(id)),table)[coderr])
      print(tablename)
      print(var)
      print(error)
      print(query)
      print(paste(var,"=",get(var,table)[coderr],auxcriteria[coderr],sep=""))
      print(auxcriteria)
      print(length(auxcriteria))
      print(class(auxcriteria))
     if (class(auxcriteria)!="logical") {
          query<-data.frame(get(deparse(substitute(id)),table)[coderr],
                            tablename,var,error,
                            query,
                            paste(var,"=",get(var,table)[coderr],auxcriteria[coderr],sep=""),
                            stringsAsFactors=FALSE)
      }
      else {
          query<-data.frame(get(deparse(substitute(id)),table)[coderr],
                            tablename,var,error,
                            query,
                            paste(var,"=",get(var,table)[coderr],sep=""),
                            stringsAsFactors=FALSE)
      }

        
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
}

## WRITE FUNCTION TO CHECK FOR DUPLICATES OF UNIQUEID
queryduplicates <- function(uniqueid,table=parent.frame(),date=date,subsettext="",id=patient){
  uniqueid <- deparse(substitute(uniqueid))
  id <- deparse(substitute(id))
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(uniqueid,table)){
  	if(!missing(date)){
          date <- deparse(substitute(date))
          pid <- paste(get(uniqueid,table),"&",get(date,table))
        }
  	if(missing(date)){pid <- get(uniqueid,table)}
    if(any(duplicated(pid))){
      duperr <- duplicated(pid)
      if(missing(date)){
      	query <- data.frame(get(id,table)[duperr],
      										tablename,
      										id,
      										"Logic",
      										"Duplicate Record",
      										paste(id," =",get(id,table)[duperr]),
      										stringsAsFactors=FALSE)
      }
      	 if(!missing(date)){
      	 	query <- data.frame(get(id,table)[duperr],
      	 											tablename,
      	 											ifelse(!is.na(subvar),paste(id,"&",date,subvar,sep=""),
      	 														 paste(id,"&",date,sep="")),
      	 											"Logic",
      	 											"Duplicate Record",
      	 											paste(id,"=",get(id,table)[duperr],"&",
      	 														date,"=",get(date,table)[duperr],subsettext,sep=""),
      	 											stringsAsFactors=FALSE)
      	 }      	 
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
    }
  }
}
## WRITE FUNCTION FOR OUT OF ORDER DATES, DATE2 IS SUPPOSED TO OCCUR ON OR AFTER DATE1
outoforder <- function(date1,date2,table=parent.frame(),table2=table2,id=patient){
  date1 <- deparse(substitute(date1))
  date2 <- deparse(substitute(date2))
	if(!missing(table2)){tablename <- paste(tablename,"&",table2)}
  if(exists(date1,table) & exists(date2,table)){
     logicerr <- !is.na(get(date1,table)) & !is.na(get(date2,table)) & get(date1,table) > get(date2,table)
     if(exists(paste(date1,"_a",sep=""),table)){
	flag1 <- get(paste(date1,"_a",sep=""),table)
	logicerr[!is.na(flag1) & flag1 %in% c("U",">")] <- FALSE
     }
     if(exists(paste(date2,"_a",sep=""),table)){
	flag2 <- get(paste(date2,"_a",sep=""),table)
	logicerr[!is.na(flag2) & flag2 %in% c("U","<")] <- FALSE
     }
     if(any(logicerr)){
      query <- data.frame(get(deparse(substitute(id)),table)[logicerr],
     										 tablename,paste(date1,"&",date2,sep=""),
     										 "Logic",
     										 "Out of Order",
     										 paste(date1,"=",get(date1,table)[logicerr],"&",date2,"=",get(date2,table)[logicerr],sep=""),
     										 stringsAsFactors=FALSE)
     names(query) <- names(emptyquery)
     assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
     }
  }
}
## CONVERT DATE VARIABLES AND PERFORM LOGIC CHECK AGAINST DATE OF BIRTH
convertdate <- function(date,table=parent.frame()){
  if(exists(deparse(substitute(date)),table)){
    var <- as.Date(get(deparse(substitute(date)),table),"%Y-%m-%d")
  }
  if(!exists(deparse(substitute(date)),table)){
    var <- NULL
  }
  return(var)
}
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
upperrangecheck <- function(var,value,table=parent.frame(),subsettext="",id=patient){
  var <- deparse(substitute(var))
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(var,table)){
    coderr <- !is.na(get(var,table)) & get(var,table) > value
    if(any(coderr)){
      query<-data.frame(get(deparse(substitute(id)),table)[coderr],
      									tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
      									"Out of Range",
      									paste(var,"=",get(var,table)[coderr],subsettext,sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
}
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
lowerrangecheck <- function(var,value,table=parent.frame(),subsettext="",id=patient){
  var <- deparse(substitute(var))
  subvar <- unlist(strsplit(subsettext,"="))[1]
  if(exists(var,table)){
    coderr <- !is.na(get(var,table)) & get(var,table) < value
    if(any(coderr)){
      query<-data.frame(get(deparse(substitute(id)),table)[coderr],
      						tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
      						"Out of Range",
      						paste(var,"=",get(var,table)[coderr],subsettext,sep=""),
      						stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
}
## Original
## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
#notnumeric <- function(var,table=parent.frame(),id=patient){
#  var <- deparse(substitute(var))
#  if(exists(var,table)){
#    numerr <- grepl("[:alpha:]",get(var,table))
#    if(any(numerr)){
#      query<-data.frame(get(deparse(substitute(id)),table)[numerr],
#      						tablename,var,"Value Error",
#      						"Unexpected Type",
#      						paste(var,"=",get(var,table)[numerr],"&type=numeric",sep=""),
#      						stringsAsFactors=FALSE)
#      names(query) <- names(emptyquery)
#      assign(paste("query",index,sep=""),query,envir=globalenv())
#      index <<- index + 1
#    }
#  }
#}
## LDR 2016-07-29 - syntax error ([:alpha:] in grepl s/b [[:alpha:]]) correct in version below
## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notnumeric <- function(var,table=parent.frame(),id=patient){
  print(table$var)
  print(table)
  print(table$id)
  var <- deparse(substitute(var))
  print(var)
  print(exists(var,table))
  if(exists(var,table)){
    print(get(var,table))
    numerr <- grepl("[[:alpha:]]",get(var,table))
    print(numerr)
    if(any(numerr)){
      query<-data.frame(get(deparse(substitute(id)),table)[numerr],
                        tablename,var,"Value Error",
                        "Unexpected Type",
                        paste(var,"=",get(var,table)[numerr],"&type=numeric",sep=""),
                        stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
}
## FORCE NUMBER will take a factor/character to a number without issuing the warnings
forcenumber <- function(var){
    options(warn = -1)
    neg <- ifelse(substr(var,1,1)=="-","-","")
    x <- as.numeric(paste0(neg,gsub("[^0-9.]","",as.character(var))))
    options(warn = 1)
    return(x)
}

## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notdate <- function(var,table=parent.frame(),id=patient){
  var <- deparse(substitute(var))
  if(exists(var,table)){
    datavar <- get(var,table)
    numerr <- !is.na(datavar) & !(grepl("[0-9]",substr(datavar,1,4)) & grepl("-",substr(datavar,5,5)) & grepl("[0-9]",substr(datavar,6,7)) & grepl("-",substr(datavar,8,8)) & grepl("[0-9]",substr(datavar,9,10)))
## ADDED TO FLAG BAD DATES AS VALUE ERRORS - 3/29/2013
    ## bad dates will be forced to mssing by as.Date(), eg 2010-07-32
    dateform <- as.Date(datavar,"%Y-%m-%d")
    ## if missing the formatted date and not the original date, then we know something was wrong with one component (avoids programming this check seperately)
    numerr[!is.na(datavar) & is.na(dateform)] <- TRUE
    if(any(numerr)){
      query<-data.frame(get(deparse(substitute(id)),table)[numerr],
      									tablename,var,"Value Error",
      									"Unexpected Type",
      									paste(var,"=",datavar[numerr],"&type=date",sep=""),
      									stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv())
      index <<- index + 1
    }
  }
}

## ???? LDR missrecord and badrecord functions appear to be identical - need to review prior downloads from github ????
## WRITE FUNCTION TO CHECK FOR RECORDS THAT EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
badrecord <- function(uniqueid,subset=parent.frame(),superset=parent.frame(),subsettext="",id=patient){
  uniqueid <- deparse(substitute(uniqueid))
  subvar <- unlist(strsplit(subsettext,"="))[1]
  subvar <- gsub("!","",subvar)
  if(exists(uniqueid,subset) & exists(uniqueid,superset)){
    if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
      recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
      	query <- data.frame(get(deparse(substitute(id)),subset)[recerr],
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
}
## ???? LDR missrecord and badrecord functions appear to be identical - need to review prior downloads from github ????
## Original
## WRITE FUNCTION TO CHECK FOR RECORDS THAT SHOULD EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
#missrecord <- function(uniqueid,subset=parent.frame(),superset=parent.frame(),subsettext="",id=patient){
#  uniqueid <- deparse(substitute(uniqueid))
#  subvar <- unlist(strsplit(subsettext,"="))[1]
#  subvar <- gsub("!","",subvar)
#  if(exists(uniqueid,subset) & exists(uniqueid,superset)){
#    if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
#      recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
#      	query <- data.frame(get(deparse(substitute(id)),subset)[recerr],
#      										tablename,
#      										ifelse(!is.na(subvar),paste(uniqueid,subvar,sep=""),uniqueid),
#      										"Logic",
#      										"Missing Record",
#      										paste(paste(uniqueid,"=",get(uniqueid,subset)[recerr]),subsettext,sep=""),
#      										stringsAsFactors=FALSE)
#      names(query) <- names(emptyquery)
#      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#    }
#  }
#}
## LDR Modifed
## WRITE FUNCTION TO CHECK FOR RECORDS THAT SHOULD EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
missrecord <- function(uniqueid,subset=parent.frame(),superset=parent.frame(),subsettext="",id=patient,subsettext_txtonly="No"){
  uniqueid <- deparse(substitute(uniqueid))
  if (subsettext_txtonly=="No") {
      subvar <- unlist(strsplit(subsettext,"="))[1]
      subvar <- gsub("!","",subvar)
  }
  else {
    subvar <- ""
  }
  if(exists(uniqueid,subset) & exists(uniqueid,superset)){
    if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
      recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
      query <- data.frame(get(deparse(substitute(id)),subset)[recerr],
                          tablename,
                          ifelse(!is.na(subvar),paste(uniqueid,subvar,sep=""),uniqueid),
                          "Logic",
                          "Missing Record",
                          paste(paste(uniqueid,"=",get(uniqueid,subset)[recerr]),subsettext,sep=""),
                          stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
    }
  }
}
