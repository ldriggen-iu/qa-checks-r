#############################################################
#
#   Program: utility_functions.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for deriving
#            variables routinely needed for medical research.
#
#   INPUT: 
#   OUTPUT: standard R functions
#
#   Notes: 
#
#   Created: 11 February 2013
#   Revisions: 
#     
#############################################################

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

# baseline - window defined, closest/latest/earliest
getbaseline <- function(baselinedate,visitdate,ids,value=value,before=30,after=30,type="closest",data=parent.frame(),returndate=FALSE,dateformat=dateformat,subset=subset){
   ## get appropriate variables from data frame if provided
    if(!missing(data)){
        bdate <- get(deparse(substitute(baselinedate)),data)
        vdate <- get(deparse(substitute(visitdate)),data)
        ids <- get(deparse(substitute(ids)),data)
        if(!missing(value)) values <- get(deparse(substitute(value)),data)
    }
    if(!missing(dateformat)){
        vdate <- as.Date(vdate,dateformat)
	bdate <- as.Date(bdate,dateformat)
    }
  ## check variable lengths
   if(!missing(value) & length(values)!=length(ids))  stop("values is not the appropriate length")
   if(length(vdate)!=length(ids))  stop("vdate is not the appropriate length")
   if(length(bdate)!=length(ids))  stop("bdate is not the appropriate length")
   if(!missing(subset)){
       if(length(subset) != length(ids)) stop("subset is not the appropriate length")
       if(!is.logical(subset)) stop("subset should be a logical argument")
       vdate <- vdate[subset]
       bdate <- bdate[subset]
       ids <- ids[subset]
       if(!missing(value)) values <- values[subset]
   }
  ## check for duplicates by visitdate
    dups <- unsplit(lapply(split(vdate, ids), FUN=anyDuplicated), ids)
    dupids <- sort(unique(ids[dups>0]))
    if(length(dupids)>0){
         warning(paste("visitdate is duplicated for",length(dupids),"id's; returning list of duplicate id."))
         return(duplicateids = dupids)
    }
  ## now extract the date and value (optional) associated with the window (before/after) and type (closest/earliest/latest)
    originalid <- ids
    diff <- vdate - bdate
    window <- diff >= before*(-1) & diff <= after
    ids <- ids[window]; vdate <- vdate[window]
    diff <- diff[window]
    if(!missing(value)) values <- values[window]
    if(type=="closest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(min(abs(x)))), ids)
    if(type=="earliest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(min(x))), ids)
    if(type=="latest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(max(x))), ids)
    if(!missing(value)) {
        if(returndate){
            baselinevalues <- data.frame(ids,vdate,values)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),paste(deparse(substitute(value)),"_d",sep=""),deparse(substitute(value)))
        }
        if(!returndate){
            baselinevalues <- data.frame(ids,values)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),deparse(substitute(value)))
        }
    }
    if(missing(value)) {
        baselinevalues <- data.frame(ids,vdate)[keep1==diff,]
        names(baselinevalues) <- c(deparse(substitute(id)),deparse(substitute(visitdate)))
    }
    return(baselinevalues)
}



# LTFU functions



# first date
# last date


getselectdate <- function(date,id,type="first",data=data,dateformat=dateformat){
    ## get appropriate variables from data frame if provided
    if(!missing(data)){
        dates <- get(deparse(substitute(date)),data)
        ids <- get(deparse(substitute(id)),data)
    }
    if(!missing(dateformat)){
        dates <- as.Date(dates,dateformat)
    }
    if(type=="first") keep1 <- unsplit(lapply(split(dates, ids), FUN=function(x) min(x)), ids)
    if(type=="last") keep1 <- unsplit(lapply(split(dates, ids), FUN=function(x) max(x)), ids)
    selectdate <- unique(data.frame(ids,keep1))
    names(selectdate) <- c(deparse(substitute(id)),deparse(substitute(date)))
    return(selectdate)
}



getnadirvalue <- function(value,id,data=data,date=date,dateformat=dateformat){
    ## get appropriate variables from data frame if provided
    if(!missing(data)){
        ids <- get(deparse(substitute(id)),data)
        values <- get(deparse(substitute(value)),data)
        if(!missing(date)) dates <- get(deparse(substitute(date)),data)
    }
    if(!missing(dateformat) & !missing(date)){
        dates <- as.Date(dates,dateformat)
    }
    keep1 <- unsplit(lapply(split(values, ids), FUN=function(x) min(x)), ids)
    if(missing(date)){
        nadirvalue <- unique(data.frame(ids,keep1))
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)))
    }
    if(!missing(date)){
        nadirvalue <- data.frame(ids,keep1,dates)[keep1==values,]
	nadirvalue <- nadirvalue[!duplicated(nadirvalue$ids),]
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)),deparse(substitute(date)))
    }
    return(nadirvalue)
}








# getfirstdate <- function(...,data=data){
#     ## get appropriate variables from data frame if provided
#     if(!missing(data)){
#         bdate <- get(deparse(substitute(dates)),data)
#         vdate <- get(deparse(substitute(visitdate)),data)
#         ids <- get(deparse(substitute(id)),data)
#         if(!missing(value)) values <- get(deparse(substitute(value)),data)
#     }
#     x  <- sapply(match.call()[-1],deparse)
#     y <- do.call("cbind",lapply(x,function(x) assign(x,get(x,basic))))
#     return(y)
# }
# y <- getfirstdate(enrol_d,birth_d)



