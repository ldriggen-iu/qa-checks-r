#############################################################
#
#   Program: tblBAS_checks.R
#   Project: IeDEAS -- CCASANET
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblBAS.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblBAS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 22 October 2012
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblBAS"
## READ TABLE
basic <- read.csv(paste("input/",tablename,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE)
names(basic) <- tolower(names(basic))
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","center","country","birth_d","enrol_d","gender",
                   "height_bas","weight_bas","mode","recart_y","who_stage","cdc_stage","haart_d")
acceptablenames <- c(expectednames,"birth_d_a","enrol_d_a","haart_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"basic")
missvar(expectednames,"basic")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("birth_d","basic")
notdate("enrol_d","basic")
notdate("haart_d","basic")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("birth_d",basic)){basic$birth_d <- convertdate("birth_d","basic")}
if(exists("enrol_d",basic)){basic$enrol_d <- convertdate("enrol_d","basic")}
if(exists("haart_d",basic)){basic$haart_d <- convertdate("haart_d","basic")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder("birth_d","enrol_d","basic")
outoforder("birth_d","haart_d","basic")

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("birth_d","basic")
futuredate("enrol_d","basic")
futuredate("haart_d","basic")

## CHECK FOR DUPLICATE PATIENT IDs
queryduplicates("patient","basic")

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric("height_bas","basic")
notnumeric("weight_bas","basic")

## CHECK FOR MISSING DATA
missingvalue("birth_d","basic")
missingvalue("enrol_d","basic")
missingvalue("haart_d","basic")
missingvalue("patient","basic")
missingvalue("center","basic")
missingvalue("country","basic")
missingvalue("gender","basic")
missingvalue("height_bas","basic")
missingvalue("weight_bas","basic")
missingvalue("mode","basic")
missingvalue("recart_y","basic")
missingvalue("who_stage","basic")
missingvalue("cdc_stage","basic")

## CHECK FOR UNEXPECTED CODING
badcodes("gender",c(1,2,9),"basic")
badcodes("mode",c(1:8,90,99),"basic")
badcodes("recart_y",c(0,1,9),"basic")
badcodes("who_stage",c(1:4,9),"basic")
badcodes("cdc_stage",c("A","A1","A2","A3","B","B1","B2","B3","C","C1","C2","C3"),"basic")
badcodes("birth_d_a",c("<",">","D","M","Y","U"),"basic")
badcodes("enrol_d_a",c("<",">","D","M","Y","U"),"basic")
badcodes("haart_d_a",c("<",">","D","M","Y","U"),"basic")

################### QUERY CHECKING ENDS HERE ###################
