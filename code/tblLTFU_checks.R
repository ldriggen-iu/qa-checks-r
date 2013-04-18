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
#   INPUT: "tblLTFU.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblBAS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 29 March 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLTFU"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","lastvis_d","death_d","lastinfo_d","transfer_d")
acceptablenames <- c(expectednames,"lastvis_d_a","death_d_a","lastinfo_d_a","transfer_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,ltfu)
missvar(expectednames,ltfu)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(lastvis_d,ltfu)
notdate(death_d,ltfu)
notdate(lastinfo_d,ltfu)
notdate(transfer_d,ltfu)

## CHECK FOR MISSING DATA
missingvalue(lastvis_d,ltfu)
# missingvalue(death_d,ltfu)
missingvalue(lastinfo_d,ltfu)
# missingvalue(transfer_d,ltfu)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("lastvis_d",ltfu)){ltfu$lastvis_d <- convertdate(lastvis_d,ltfu)}
if(exists("death_d",ltfu)){ltfu$death_d <- convertdate(death_d,ltfu)}
if(exists("lastinfo_d",ltfu)){ltfu$lastinfo_d <- convertdate(lastinfo_d,ltfu)}
if(exists("transfer_d",ltfu)){ltfu$transfer_d <- convertdate(transfer_d,ltfu)}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basltfu <- merge(ltfu,with(basic,data.frame(patient,birth_d,enrol_d)),all.x=TRUE)
	basltfu$birth_d <- convertdate(birth_d,basltfu)
	outoforder(birth_d,lastvis_d,basltfu,table2="tblBAS")
	outoforder(birth_d,death_d,basltfu,table2="tblBAS")
	outoforder(birth_d,lastinfo_d,basltfu,table2="tblBAS")
	outoforder(birth_d,transfer_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,lastvis_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,death_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,lastinfo_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,transfer_d,basltfu,table2="tblBAS")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(lastvis_d,death_d,ltfu)
outoforder(lastinfo_d,death_d,ltfu)
outoforder(transfer_d,death_d,ltfu)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(lastvis_d,ltfu)
futuredate(death_d,ltfu)
futuredate(lastinfo_d,ltfu)
futuredate(transfer_d,ltfu)

## CHECK FOR DUPLICATE PATIENT IDs
queryduplicates(patient,ltfu)

## CHECK FOR UNEXPECTED CODING
badcodes(lastvis_d_a,c("<",">","D","M","Y","U"),ltfu)
badcodes(death_d_a,c("<",">","D","M","Y","U"),ltfu)
badcodes(lastinfo_d_a,c("<",">","D","M","Y","U"),ltfu)
badcodes(transfer_d_a,c("<",">","D","M","Y","U"),ltfu)

################### QUERY CHECKING ENDS HERE ###################
