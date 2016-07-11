#############################################################
#
#   Program: tblNEWBORN_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblNEWBORN.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblNEWBORN.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 23 June 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblNEWBORN"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("child_id","entry_pmtct_y","prenat_mtct_y","infant_mtct_y","infant_mtct_dur","postpar_mtct_y","breastfd_y","breastfd_dur","abnorm_y")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,newborn)
missvar(expectednames,newborn)



## ??? Should any field present be flagged for missing
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CHECK FOR UNEXPECTED CODING

badcodes(entry_pmtct_y,c(0,1,9),newborn,id=child_id)
badcodes(prenat_mtct_y,c(0,10,11,12,13,14,99),newborn,id=child_id)
badcodes(infant_mtct_y,c(0,10,11,12,13,14,99),newborn,id=child_id)
badcodes(infant_mtct_dur,c(0,1,88,99),newborn,id=child_id)
badcodes(postpar_mtct_y,c(0,1,9),newborn,id=child_id)
badcodes(breastfd_y,c(0,1,9),newborn,id=child_id)
#???? need to check conditionally the following:
#     - if infant_mtct_y is 0 or 99 and infant_mtct_dur not = 88
#     - if infant_mtct_y is 10, 11, 12, 14, 14 and infant_mtct_dur is not 0,1,99
#     - if breastfd_y is 1 then check breastfd_dur (breastfd_dur weeks - acceptable range ????)
#     - if breastfd_y is 0 or 9 then breastfd_dur should not be populated
#

badcodes(abnorm_y,c(0,1,9),newborn,id=child_id)
# Check for existence of child_id when abnorm_y=1 ???? this needs work - not clear what the issues is in the query table
if(exists("newbornabnorm")){missrecord(child_id,newborn[newborn$abnorm_y == '1' & !is.na(newborn$abnorm_y),],newbornabnorm," has abnorm_y=1 and no row in tblNEWBORN_ABNORM",id=child_id)}

################### QUERY CHECKING ENDS HERE ###################
