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
# Revisions: 24 February 2016 Larry Riggen added code for 
#            tblDELIVERY_CHILD, ???? complete list when all new 
#            tables are defined ????
#            added code to process North America (NA) region codes
#            as text instead of missing values.
#            
#
#############################################################
rm(list=ls()) # clear namespace


## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY AND SET THE APPROPRIATE DATABASE CLOSE DATE
setwd("I:/Projects/IeDEA/Grants/Yiannoutsos/BD2K/IeDEA_DES/GitRepository")

## IN ORDER TO ASSESS DATES OCCURRING IN THE FUTURE, WE NEED A DATABASE CLOSE DATE (YYYY-MM-DD)
databaseclose <- "2013-12-01"

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
expectedtables <- c("center","program","basic","ltfu","cd4","rna","bp","viro","res","reslvl2","reslvl3","art","med",
                    "dis","distb","visit","deliverychild","deliverymum","lab","canc","cep","newborn","newbornabnorm","pregout","preg")
expecteddestables <- c("tblCENTER","tblPROGRAM","tblBAS","tblLTFU","tblLAB_CD4","tblLAB_RNA","tblLAB_BP","tblLAB_VIRO","tblLAB_RES","tblLAB_RES_LVL_2","tblLAB_RES_LVL_3","tblART","tblMED","tblDIS","tblDIS_TB","tblVIS",
                       "tblDELIVERY_CHILD","tblDELIVERY_MUM","tblLAB","tblCANC","tblCEP","tblNEWBORN","tblNEWBORN_ABNORM","tblPREG_OUT","tblPREG")
## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files("input"))
readtables <- expectedtables[match(existingtables,expecteddestables)]
## READ IN ALL EXISTING TABLES
## ???? LDR original code
#for(i in 1:length(readtables)){
#  if(!is.na(readtables[i])){
#     readcsv <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
#     names(readcsv) <- tolower(names(readcsv))
#     assign(readtables[i],readcsv)
#   }
#}
## ???? LDR code to handle "NA" North America region code as text instead of missing values
for(i in 1:length(readtables)){
  if(!is.na(readtables[i])){
    if (readtables[i] == "center" || readtables[i]== "program")  {
      # read the table in with na.strings=c("") to pick up the region as text
      read1 <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(""))
      names(read1) <- tolower(names(read1))
      read2 <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
      names(read2) <- tolower(names(read2))
      center_region<-subset(read1,select=c("region"))
      center_rest<-subset(read1,select= - region)
      readcsv<-cbind(center_region,center_rest)
      assign(readtables[i],readcsv)     
    }
    else {
      readcsv <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
      names(readcsv) <- tolower(names(readcsv))
      assign(readtables[i],readcsv)

    }
  }
}


################### QUERY CHECK PROGRAMS BEGIN HERE #################

if(exists("center")) source("code/tblCENTER_checks.R")
if(exists("program")) source("code/tblPROGRAM_checks.R")
if(exists("basic")) source("code/tblBAS_checks.R")
if(exists("ltfu")) source("code/tblLTFU_checks.R")
if(exists("cd4")) source("code/tblLAB_CD4_checks.R")
if(exists("rna")) source("code/tblLAB_RNA_checks.R")
if(exists("art")) source("code/tblART_checks.R")
if(exists("dis")) source("code/tblDIS_checks.R")
if(exists("visit")) source("code/tblVIS_checks.R")
if(exists("deliverychild")) source("code/tblDELIVERY_CHILD_checks.R")
if(exists("deliverymum")) source("code/tblDELIVERY_MUM_checks.R")
if(exists("lab")) source("code/tblLAB_checks.R")
if(exists("canc")) source("code/tblCANC_checks.R")
if(exists("cep")) source("code/tblCEP_checks.R")
if(exists("distb")) source("code/tblDIS_TB_checks.R")
if(exists("bp")) source("code/tblLAB_BP_checks.R")
if(exists("viro")) source("code/tblLAB_VIRO_checks.R")
if(exists("res")) source("code/tblLAB_RES_checks.R")
if(exists("reslvl2")) source("code/tblLAB_RES_LVL_2_checks.R")
if(exists("reslvl3")) source("code/tblLAB_RES_LVL_3_checks.R")
if(exists("med")) source("code/tblMED_checks.R")
if(exists("newbornabnorm")) source("code/tblNEWBORN_ABNORM_checks.R")
if(exists("newborn")) source("code/tblNEWBORN_checks.R")
if(exists("pregout")) source("code/tblPREG_OUT_checks.R")
if(exists("preg")) source("code/tblPREG_checks.R")
################### QUERY CHECK PROGRAMS END HERE ###################

## COMBINE ALL QUERY FILES
allquery <- do.call(rbind,lapply(paste("query",1:(index-1),sep=""),get))

## WRITE QUERY FILES
## REORDER QUERY FILE ACCORDING TO SPECS
allquery <- allquery[,c(4:5,2:3,1,6)]
## WRITE QUERY FILES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}
write.csv(allquery,paste("output/tbl_query_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)
