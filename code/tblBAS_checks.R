#############################################################
#
#   Program: tblBAS_checks.R
#   Project: IeDEA
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
#   Revisions: June 2016 Larry Riggen
#              Added the fields PROPH_Y, RECART_Y, RECART_D(_A),
#              AIDS_Y, AIDS_D(_A) and removed HAART_D_(A) for the BD2K supplement
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblBAS"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","birth_d","enrol_d","gender",
		   "mode","naive_y","proph_y","recart_y","recart_d","aids_y","aids_d")
acceptablenames <- c(expectednames,"program","birth_d_a","enrol_d_a","recart_d_a","aids_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,basic)
missvar(expectednames,basic)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(birth_d,basic)
notdate(enrol_d,basic)
notdate(recart_d,basic)
notdate(aids_d,basic)

## CHECK FOR MISSING DATA
missingvalue(birth_d,basic)
missingvalue(enrol_d,basic)
missingvalue(patient,basic)
missingvalue(country,basic)
missingvalue(gender,basic)
missingvalue(mode,basic)
missingvalue(naive_y,basic)
missingvalue(proph_y,basic)
missingvalue(recart_y,basic)
missingvalue(aids_y,basic)

# ???? not sure this check is correct with the change from HAART to RECART ???
# check missing haart_d only among those confirmed not naive 
#notnaive <- basic[basic$naive_y==0,]
#missingvalue(haart_d,notnaive)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("birth_d",basic)){basic$birth_d <- convertdate(birth_d,basic)}
if(exists("enrol_d",basic)){basic$enrol_d <- convertdate(enrol_d,basic)}
if(exists("recart_d",basic)){basic$recart_d <- convertdate(recart_d,basic)}
if(exists("aids_d",basic)){basic$aids_d <- convertdate(aids_d,basic)}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(birth_d,enrol_d,basic)
outoforder(birth_d,recart_d,basic)
outoforder(birth_d,aids_d,basic)


## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(birth_d,basic)
futuredate(enrol_d,basic)
futuredate(recart_d,basic)
futuredate(aids_d,basic)

## CHECK FOR DUPLICATE PATIENT IDs
queryduplicates(patient,basic)

## CHECK FOR UNEXPECTED CODING
badcodes(gender,c(1,2,9),basic)
badcodes(mode,c(1:8,90,99),basic)
badcodes(naive_y,c(0,1,9),basic)
badcodes(proph_y,c(0,1,9),basic)
badcodes(recart_y,c(0,1,9),basic)
badcodes(aids_y,c(0,1,9),basic)
badcodes(birth_d_a,c("<",">","D","M","Y","U"),basic)
badcodes(enrol_d_a,c("<",">","D","M","Y","U"),basic)
badcodes(recart_d_a,c("<",">","D","M","Y","U"),basic)
badcodes(aids_d_a,c("<",">","D","M","Y","U"),basic)

## QUERY PATIENTS WITH NO RECORD IN tblPROGRAM
if(exists("program") & exists("program",basic)){badrecord(program,basic,program)}

## ???? Stepany comments for tblBAS follow: 
##
## 1. (RECART variables) In historic data, we may not be able to tell when drugs were given for PMTCT vs. treatment.
## For each of these, we will need to make sure to have data quality rules that check against the ART reason for start/reason for stop variables.
## 2. (AIDS variables) We will need data checks that compare this to the WHO stage 3 or 4 diseases listed in tblDIS. 
##    BSM: Do some sites document AIDS diagnosis without recording/collecting WHO stage?




################### QUERY CHECKING ENDS HERE ###################
