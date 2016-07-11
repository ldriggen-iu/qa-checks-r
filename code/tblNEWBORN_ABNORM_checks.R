#############################################################
#
#   Program: tblNEWBORN_ABNORM _checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblNEWBORN_ABNORM .csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblNEWBORN_ABNORM .csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 11 March 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblNEWBORN_ABNORM "
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("child_id","abnorm1","abnorm2","abnorm3","abnorm4","abnorm5","abnorm_s")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,newbornabnorm)
missvar(expectednames,newbornabnorm)



## ??? Should any field present be flagged for missing
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CHECK FOR UNEXPECTED CODING
abnorm_codebook <- read.csv("resource/abnorm_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(abnorm1,abnorm_codebook$code,newbornabnorm,id=child_id)
badcodes(abnorm2,abnorm_codebook$code,newbornabnorm,id=child_id)
badcodes(abnorm3,abnorm_codebook$code,newbornabnorm,id=child_id)
badcodes(abnorm4,abnorm_codebook$code,newbornabnorm,id=child_id)
badcodes(abnorm5,abnorm_codebook$code,newbornabnorm,id=child_id)

## CHECK FOR CHILD_IDs NOT IN NEWBORN


################### QUERY CHECKING ENDS HERE ###################
