#############################################################
#
#   Program: tbl_checks.R
#   Project: IeDEA
# 
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblXXX.csv", "tblXXX_checks.R"
#   OUTPUT: "tbl_query_yyyymmdd.csv"
#
#   Notes: As long as the working directory structure 
#          matches README.md, such that the data tables,
#          R-code, and resources may be sourced, 
#          then this code should run smoothly, generating
#          a listing of data queries in /output.
#
#   Created: 9 November 2012
#   Revisions: 
#     
#############################################################
rm(list=ls()) # clear namespace


## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY AND SET THE APPROPRIATE DATABASE CLOSE DATE
#setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")

## IN ORDER TO ASSESS DATES OCCURRING IN THE FUTURE, WE NEED A DATABASE CLOSE DATE (YYYY-MM-DD)
databaseclose <- "2012-10-31"

## READ QUERY_FUNCTIONS.R
source("code/query_functions.R")
## INDEX NUMBER FOR QUERY FILES
index <- 1
## EMPTY MATRIX FOR ALL QUERIES and ALL CHECKS
emptyquery <- data.frame(PID=character(),Table=character(),Variable=character(),Error=character(),Query=character(),Info=character())
allcheck <- NULL
## CONVERT DATABASE CLOSE TO DATE FORMAT, IF MISSING/INCORRECT, THEN USE SYSTEM DATE (TODAY)
databaseclose <- as.Date(databaseclose,"%Y-%m-%d")
databaseclose <- ifelse(is.na(databaseclose),Sys.Date(),databaseclose)

## IDENTIFY WHICH TABLES TO EXPECT FROM DES
expectedtables <- c("center","basic","ltfu","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblBAS","tblLTFU","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS")
## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files("input"))
readtables <- expectedtables[match(existingtables,expecteddestables)]
## READ IN ALL EXISTING TABLES
for(i in 1:length(readtables)){
  readcsv <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE)
  names(readcsv) <- tolower(names(readcsv))
  assign(readtables[i],readcsv)
}

################### QUERY CHECK PROGRAMS BEGIN HERE #################

# if(exists("center")) source("code/tblCENTER_checks.R")
if(exists("basic")) source("code/tblBAS_checks.R")
if(exists("cd4")) source("code/tblLAB_CD4_checks.R")
if(exists("rna")) source("code/tblLAB_RNA_checks.R")
if(exists("art")) source("code/tblART_checks.R")
if(exists("dis")) source("code/tblDIS_checks.R")
if(exists("visit")) source("code/tblVIS_checks.R")

################### QUERY CHECK PROGRAMS END HERE ###################

## COMBINE ALL QUERY FILES
allquery <- do.call(rbind,lapply(paste("query",1:(index-1),sep=""),get))

## WRITE QUERY FILES
## REORDER QUERY FILE ACCORDING TO SPECS 
allquery <- allquery[,c(4:5,2:3,1,6)]
## WRITE QUERY FILES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}
write.csv(allquery,paste("output/tbl_query_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)
#allcheck <- as.data.frame(allcheck); names(allcheck) <- "CHECKS PERFORMED"
#write.csv(allcheck,paste("output/tbl_checks_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)
