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
#   Revisions: Larry Riggen July 2016 updated for
#                  add_center and drop_center
#              reworked longitude and latitude checks to
#              allow lower/upper ranged checks to be done
#              against numeric variables
#
#
#
#
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblCENTER"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("region","center","name","program","adultped","rural","level",
                   "geocode_lat","geocode_lon","open_d","close_d","drop_center")
acceptablenames <- c(expectednames,"city","district","province","country","add_center","notes")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,center)
missvar(expectednames,center)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(close_d,center,id=center)
                
## CHECK FOR MISSING DATA
missingvalue(region,center,id=center)
missingvalue(center,center,id=center)
missingvalue(name,center,id=center)
missingvalue(program,center,id=center)
missingvalue(adultped,center,id=center)
missingvalue(rural,center,id=center)
missingvalue(level,center,id=center)
# missingvalue(country,center,id=center)
missingvalue(geocode_lat,center,id=center)
missingvalue(geocode_lon,center,id=center)
missingvalue(close_d,center,id=center)

## identify dates that are invalid (missing is OK)
if(exists("open_d",center)){notdate(open_d,center,name)}
notdate(close_d,center,name)
if(exists("add_center",center)){notdate(add_center,center,name)}
notdate(drop_center,center,name)


## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("open_d",center)){center$open_d<-convertdate(open_d,center)}
center$close_d<-convertdate(close_d,center)
if(exists("add_center",center)){center$add_center<-convertdate(add_center,center)}
center$drop_center<-convertdate(drop_center,center)

if(exists("open_d",center)){
  outoforder(open_d,close_d,center,id=center)
}



## Need Center within region check due to numeric centers in multiple regions
## CHECK FOR DUPLICATE CENTER IDs
center$region_center<-paste(center$region,"-",center$center)
queryduplicates(region_center,center,id=center)

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(geocode_lat,center,id=center)
notnumeric(geocode_lon,center,id=center)

## RANGE CHECKS
center$geocode_lat<-forcenumber(center$geocode_lat)
upperrangecheck(geocode_lat,90,center[!is.na(center$geocode_lat),],id=center)
lowerrangecheck(geocode_lat,-90,center[!is.na(center$geocode_lat),],id=center)
center$geocode_lon<-forcenumber(center$geocode_lon)
upperrangecheck(geocode_lon,180,center[!is.na(center$geocode_lon),],id=center)
lowerrangecheck(geocode_lon,-180,center[!is.na(center$geocode_lon),],id=center)

## CHECK FOR UNEXPECTED CODING
badcodes(rural,c(1,2,3,4,9),center,id=center)
badcodes(level,c(1:3),center,id=center)
badcodes(adultped,c("PED","ADULT","BOTH"),center,id=center)

## QUERY PROGRAMS WITH NO RECORD IN tblPROGRAM
if(exists("program")){badrecord(program,center,program,id=program)}

################### QUERY CHECKING ENDS HERE ###################
