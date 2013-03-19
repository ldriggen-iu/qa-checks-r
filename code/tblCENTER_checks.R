#############################################################
#
#   Program: tblCENTER_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblCENTER.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblBAS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 14 December 2012
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblCENTER"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("center","country","geocode_lat","geocode_lon","rural",
                   "level","close_d")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(expectednames,"center")
missvar(expectednames,"center")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("close_d","center",id="center")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("close_d",center)){center$close_d <- convertdate("close_d","center")}

## CHECK FOR DUPLICATE PATIENT IDs
queryduplicates("center","center",id="center")

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric("geocode_lat","center",id="center")
notnumeric("geocode_lon","center",id="center")

## RANGE CHECKS
upperrangecheck("geocode_lat",90,"center",id="center")
lowerrangecheck("geocode_lat",-90,"center",id="center")
upperrangecheck("geocode_lon",180,"center",id="center")
lowerrangecheck("geocode_lon",-180,"center",id="center")
                
## CHECK FOR MISSING DATA
missingvalue("center","center",id="center")
missingvalue("country","center",id="center")
missingvalue("geocode_lat","center",id="center")
missingvalue("geocode_lon","center",id="center")
missingvalue("rural","center",id="center")
missingvalue("level","center",id="center")
missingvalue("close_d","center",id="center")

## CHECK FOR UNEXPECTED CODING
badcodes("rural",c(1,2,3,4,9),"center",id="center")
badcodes("level",c(1:3),"center",id="center")

################### QUERY CHECKING ENDS HERE ###################
