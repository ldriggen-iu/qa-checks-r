#############################################################
#
# Program: tbl_checks.R
# Project: IeDEA
#
# Biostatistician/Programmer: Meridith Blevins, MS
# Purpose: Read in IeDEAS standard and write
# data queries
#
# INPUT: "tblXXX.csv", "tblXXX_checks.R"
# OUTPUT: "tbl_query_yyyymmdd.csv"
#
# Notes: As long as the working directory structure
# matches README.md, such that the data tables,
# R-code, and resources may be sourced,
# then this code should run smoothly, generating
# a listing of data queries in /output.
#
# Created: 9 November 2012
# Revisions:
#
#############################################################
rm(list=ls()) # clear namespace


## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY AND SET THE APPROPRIATE DATABASE CLOSE DATE
#setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")

## IN ORDER TO ASSESS DATES OCCURRING IN THE FUTURE, WE NEED A DATABASE CLOSE DATE (YYYY-MM-DD)
databaseclose <- "2013-12-01"

## READ QUERY_FUNCTIONS.R
source("code/query_functions2.R")
## INDEX NUMBER FOR QUERY FILES
index <- 1
## EMPTY MATRIX FOR ALL QUERIES and ALL CHECKS
emptyquery <- data.frame(PID=character(),Table=character(),Variable=character(),Error=character(),Query=character(),Info=character())
allcheck <- NULL
## CONVERT DATABASE CLOSE TO DATE FORMAT, IF MISSING/INCORRECT, THEN USE SYSTEM DATE (TODAY)
databaseclose <- as.Date(databaseclose,"%Y-%m-%d")
databaseclose <- ifelse(is.na(databaseclose),Sys.Date(),databaseclose)

## IDENTIFY WHICH TABLES TO EXPECT FROM DES
expectedtables <- c("center","program","basic","ltfu","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblPROGRAM","tblBAS","tblLTFU","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS")
## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
# choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
# existingtables <- choosefirst(list.files("input"))
# readtables <- expectedtables[match(existingtables,expecteddestables)]
## READ IN ALL EXISTING TABLES
fn <- file.path('input', sprintf("%s.csv", expecteddestables))
for(i in which(file.access(fn, mode=4) == 0)) {
  readcsv <- read.csv(fn[i], header=TRUE, stringsAsFactors = FALSE, na.strings=c(NA,""))
  names(readcsv) <- tolower(names(readcsv))
  assign(expectedtables[i], readcsv)
}

art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
dis_id_codebook <- read.csv("resource/dis_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
dis_wd_codebook <- read.csv("resource/dis_wd_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")

qa.fn <- file.path('rules', sprintf("%s.csv", expectedtables))
errs <- vector('list', length(qa.fn))
for(i in which(file.access(qa.fn, mode=4) == 0)) {
  if(exists(expectedtables[i])) {
    readcsv <- read.csv(qa.fn[i], header=TRUE, stringsAsFactors = FALSE)
    errs[[i]] <- check(readcsv)
  }
}
all.errs <- do.call(rbind, errs)[,c(4,5,2,3,1,6)]
write.csv(all.errs, 'output/myerrs.csv', row.names=FALSE)
