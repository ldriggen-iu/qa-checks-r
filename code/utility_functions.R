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
#' convertdate
#'
#' summary
#'
#' details
#'
#' @param date NEEDDOC
#' @param table NEEDDOC
#' @export

convertdate <- function(date,table=parent.frame()){
  if(exists(deparse(substitute(date)),table)){
    var <- as.Date(get(deparse(substitute(date)),table),"%Y-%m-%d")
  }
  if(!exists(deparse(substitute(date)),table)){
    var <- NULL
  }
  return(var)
}

# baseline - window defined, closest/latest/earliest
#' getbaseline
#'
#' summary
#'
#' details
#'
#' @param baselinedate NEEDDOC
#' @param visitdate NEEDDOC
#' @param ids NEEDDOC
#' @param value NEEDDOC
#' @param before NEEDDOC
#' @param after NEEDDOC
#' @param type NEEDDOC
#' @param data NEEDDOC
#' @param returndate NEEDDOC
#' @param dateformat NEEDDOC
#' @param subset NEEDDOC
#' @export

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
    if(type=="closest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(x[which.min(abs(x))])), ids)
    if(type=="earliest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(min(x))), ids)
    if(type=="latest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(max(x))), ids)
    if(!missing(value)) {
        if(returndate){
            baselinevalues <- data.frame(ids,vdate,values,stringsAsFactors = FALSE)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),paste(deparse(substitute(value)),"_cmp_d",sep=""),paste0(deparse(substitute(value)),"_cmp"))
        }
        if(!returndate){
            baselinevalues <- data.frame(ids,values,stringsAsFactors = FALSE)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),paste0(deparse(substitute(value)),"_cmp"))
        }
    }
    if(missing(value)) {
        baselinevalues <- data.frame(ids,vdate,stringsAsFactors = FALSE)[keep1==diff,]
        names(baselinevalues) <- c(deparse(substitute(id)),paste0(deparse(substitute(visitdate)),"_cmp_d"))
    }
    return(baselinevalues)
}



# LTFU functions



# first date
# last date

#' getselectdate
#'
#' summary
#'
#' details
#'
#' @param date NEEDDOC
#' @param id NEEDDOC
#' @param type NEEDDOC
#' @param data NEEDDOC
#' @param dateformat NEEDDOC
#' @export

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
    selectdate <- unique(data.frame(ids,keep1,stringsAsFactors = FALSE))
    names(selectdate) <- c(deparse(substitute(id)),paste(deparse(substitute(date)),type,"cmp",sep="_"))
    return(selectdate)
}


## THIS FUNCTION ALLOWS THE USER TO GET THE VALUE OF VAR WHICH APPEARS FIRST, LAST, or MOST OFTEN.
most <- function(x){ux <- unique(x); ux[which.max(tabulate(match(x,ux)))]}
getselectvar <- function(date,id,var,type="first",data=data,dateformat=dateformat){
    ## get appropriate variables from data frame if provided
    if(!missing(data)){
        dates <- get(deparse(substitute(date)),data)
        ids <- get(deparse(substitute(id)),data)
        vars <- get(deparse(substitute(var)),data)
    }
    if(!missing(dateformat)){
        dates <- as.Date(dates,dateformat)
    }
    if(type=="first"){
        date1 <- unsplit(lapply(split(dates, ids), FUN=function(x) min(x)), ids)
	keep1 <- data.frame(ids,vars,stringsAsFactors = FALSE)[date1==dates,]
    }
    if(type=="most"){
        keep1 <- unsplit(lapply(split(vars, ids), FUN=function(x) most(x)), ids)
        keep1 <- unique(data.frame(ids,keep1,stringsAsFactors = FALSE))
    }
    if(type=="last"){
        date1 <- unsplit(lapply(split(dates, ids), FUN=function(x) max(x)), ids)
        keep1 <- data.frame(ids,vars,stringsAsFactors = FALSE)[date1==dates,]
    }
    names(keep1) <- c(deparse(substitute(id)),paste(deparse(substitute(var)),type,"cmp",sep="_"))
    return(keep1)
}

#' getnadirvalue
#'
#' summary
#'
#' details
#'
#' @param value NEEDDOC
#' @param id NEEDDOC
#' @param data NEEDDOC
#' @param date NEEDDOC
#' @param dateformat NEEDDOC
#' @export

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
        nadirvalue <- unique(data.frame(ids,keep1,stringsAsFactors = FALSE))
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)))
    }
    if(!missing(date)){
        nadirvalue <- data.frame(ids,keep1,dates,stringsAsFactors = FALSE)[keep1==values,]
	nadirvalue <- nadirvalue[!duplicated(nadirvalue$ids),]
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)),deparse(substitute(date)))
    }
    return(nadirvalue)
}



## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))



