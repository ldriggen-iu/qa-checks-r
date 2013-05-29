#############################################################
#
#   Program: summarize_exceptions.R
#   Project: IeDEA
# 
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS data exceptions and summarize
#
#   INPUT: "tbl_query_yyyymmdd.csv"
#   OUTPUT: "summaryX_yyyymmdd.csv"
#   PACKAGES: Brewer
#
#   Notes: As long as the working directory structure 
#          matches README.md, such that the data tables,
#          R-code, and resources may be sourced, 
#          then this code should run smoothly, generating
#          a listing of data queries in /output.
#
#   Created: 25 January 2013
#   Revisions: 20 February 2013
#     
#############################################################
rm(list=ls()) # clear namespace

## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY
#setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")

## SUMMARIZE COUNT DATA
source("code/summarize_counts.R")
source("code/utility_functions.R")

## READ IN QUERY FILE -- CHOOSE MOST RECENT IF MULTIPLE
wd <- getwd()
  if(!file.exists("output")) stop("ERROR: there is no output directory") 
getexceptions <- list.files("output")
  if(length(getexceptions)<1) stop("ERROR: there are no exception files in the output directory")
  getexceptions <- getexceptions[grep("tbl_query_",getexceptions)]
  useexceptions <- getexceptions[which.max(gsub(".csv","",gsub("tbl_query_","",getexceptions)))]
allquery <- read.csv(paste("output/",useexceptions,sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))

## IDENTIFY PRIMARY TABLE QUERIED FOR MERGING WITH RECORDS COUNT DATA BELOW
allquery$tbl <- gsub(" ","",(sub(" &.*","",allquery$Table)))

## READ IN TABLE COUNTS (RECORDS AND PATIENTS)
recordcounts <- read.csv(paste("output/","counts_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
allquery <- merge(allquery,recordcounts)

## DETERMINE UNIQUE QUERY LISTINGS
allquery$err2tbl <- with(allquery, paste(Error,Query,Table,sep="---"))
allquery$err2var <- with(allquery, paste(Error,Query,Table,Variable,sep="---"))
allquery$err <- ifelse(allquery$Error=="Table Structure",paste(allquery$err2tbl,"---",sep=""),allquery$err2var)

## BUILD A FORMAT FOR PERCENTAGES
myper0 <- function(x){
    y <- NULL
    if(x <= 0.5 & x!=0){y <- "<1"}
    else{y <- format(round(x),nsmall=0)}
    return(y)
}

## FUNCTION TO COUNT THE NUMBER OF ERRORS, ALLOWS FOR ALLQUERY TO BE SUBSET 
## WHEN SUBSET=TRUE, THIS SELECTS EVERYTHING (NO SUBSETTING)
geterrcounts <- function(vartype,subset=TRUE){
  data <- allquery[allquery$err %in% vartype & subset,]
  x1 <- nrow(data)
  if(x1==0){
    x2 <- x3 <- x4 <- " "
  }
  else{
    if(unique(data$Error)=="Table Structure"){
      x2 <- x3 <- x4 <- " "
    }
    if(unique(data$Error)!="Table Structure"){
      x2 <- myper0(100*x1/as.numeric(data$records[1]))
      x3 <- length(unique(data$PID))
      x4 <- myper0(100*x3/as.numeric(data$patients[1]))
    }
  }
  line <- paste(vartype,x1,x2,x3,x4,sep="---")
  return(line)
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


## GET QUERY COUNTS FOR ALL ERROR TYPES, THEN COMBINE INTO A TABLE
## WHEN SUBSET=TRUE, THIS SELECTS EVERYTHING (NO SUBSETTING)
geterrtable <- function(vartypes,subset=TRUE,total=FALSE){
  summarytable <- data.frame(do.call("rbind",strsplit(sapply(vartypes,geterrcounts,subset=subset),"---")),stringsAsFactors=FALSE)
  names(summarytable) <- c("Error","Query","Table","Variable","Record Count","Pct Total Records","Patient Count","Pct Total Patients")
  row.names(summarytable) <- NULL
  summarytable <- summarytable[order(summarytable$Error,summarytable$Query,summarytable$Table,summarytable$Variable),]
  if(total){
    qyct <- sum(allquery$err %in% vartypes & subset)
    ptct <- length(unique(allquery$PID[allquery$err %in% vartypes & subset]))
    summarytable[nrow(summarytable)+1,]<- c("Total","","","",qyct,"",ptct,"")
  }
  return(summarytable)
}  

## GET SUMMARY OF ALL QUERIES
## WHEN SUBSET=TRUE, THIS SELECTS EVERYTHING (NO SUBSETTING)
overalltable <- geterrtable(unique(allquery$err),total=TRUE)

## WRITE QUERY SUMMARY
write.csv(overalltable,paste("output/query_summary_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)


############ EVERYTHING CODED ABOVE IS NOT DEPENDENT ON ANY R PACKAGE OR ORIGINAL DATASETS ############
## READ IN TABLE BASIC TO UTILIZE ENROLLMENT DATE FOR FIGURES
basic <- read.csv("input/tblBAS.csv",header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(basic) <- tolower(names(basic))
## READ IN TABLE VISIT TO UTILIZE CENTER FOR SUBSETTING
visit <- read.csv("input/tblVIS.csv",header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(visit) <- tolower(names(visit))
## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("enrol_d",basic)){basic$enrol_d <- convertdate(enrol_d,basic)}
if(exists("vis_d",visit)){visit$vis_d <- convertdate(vis_d,visit)}
## MERGE FOR ENROLLMENT DATE
visit <- merge(with(visit,data.frame(patient,center,vis_d,stringsAsFactors = FALSE)),with(basic,data.frame(patient,enrol_d,stringsAsFactors = FALSE)),by="patient",all.x=TRUE)
# in case there are duplicates by visit date 
dups <- unsplit(lapply(split(visit$vis_d, visit$patient), FUN=anyDuplicated), visit$patient)
visit <- visit[dups == 0 & !is.na(visit$vis_d),]
## IDENTIFY CENTER of ENROLLMENT BY DATE OF VISIT CLOSEST TO DATE OF ENROLLMENT
enroll_center <- getbaseline(enrol_d,vis_d,patient,value=center,data=visit,returndate=FALSE,before=365*5,after=365*5) # default is closest date with window -30,30
allquery <- merge(merge(allquery,basic,by.x="PID",by.y="patient",all.x=TRUE),enroll_center,by.x="PID",by.y="id",all.x=TRUE)

## SUMMARIZE QUERIES IN HTML
library(brew)
brew(file='code/summarize.brew',output='output/summary_report.html')
