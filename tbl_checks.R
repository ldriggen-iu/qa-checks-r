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
#setwd("C:/Documents and Settings/blevinml/My Documents/Projects/IeDEAS/qa-checks-r")

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

################### QUERY CHECK PROGRAMS BEGIN HERE #################

source("code/tblCENTER_checks.R")
source("code/tblBAS_checks.R")
source("code/tblLAB_CD4_checks.R")
source("code/tblLAB_RNA_checks.R")
source("code/tblART_checks.R")
source("code/tblDIS_checks.R")
source("code/tblVIS_checks.R")

################### QUERY CHECK PROGRAMS END HERE ###################

## COMBINE ALL QUERY FILES
allquery <- do.call(rbind,lapply(paste("query",1:(index-1),sep=""),get))

## WRITE QUERY FILES
## REORDER QUERY FILE ACCORDING TO SPECS 
allquery <- allquery[,c(4:5,2:3,1,6)]
## WRITE QUERY FILES
write.csv(allquery,paste("output/tbl_query_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)
#allcheck <- as.data.frame(allcheck); names(allcheck) <- "CHECKS PERFORMED"
#write.csv(allcheck,paste("output/tbl_checks_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)
