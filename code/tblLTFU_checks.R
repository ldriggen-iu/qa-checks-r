#############################################################
#
#   Program: tblLTFU_checks.R
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
#          correctly pointing to the location of tblLTFU.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 29 March 2013
#   Revisions: Larry Riggen 22 June 2016
#              add MOTHERDEATH_Y, MOTHERDEATH_D(_A), FATHERDEATH_Y,
#              and FATHERDEATH_D(_A) for BD2K
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLTFU"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","drop_y","death_y","death_d","l_alive_d","transfer_d",
                   "motherdeath_y","motherdeath_d","fatherdeath_y","fatherdeath_d")
acceptablenames <- c(expectednames,"death_d_a","l_alive_d_a","transfer_d_a","motherdeath_d_a","fatherdeath_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,ltfu)
missvar(expectednames,ltfu)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE (when the date should be populated)

notdate(death_d,ltfu[ltfu$death_y == 1,])
notdate(l_alive_d,ltfu)
notdate(transfer_d,ltfu[ltfu$transfer_d != " ",])
notdate(motherdeath_d,ltfu[ltfu$motherdeath_y == 1,])
notdate(fatherdeath_d,ltfu[ltfu$fatherdeath_y == 1,])

## CHECK FOR MISSING DATA
# missingvalue(death_d,ltfu)
missingvalue(l_alive_d,ltfu)
# missingvalue(transfer_d,ltfu)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
ltfu$death_d <- convertdate(death_d,ltfu)
ltfu$l_alive_d <- convertdate(l_alive_d,ltfu)
ltfu$transfer_d <- convertdate(transfer_d,ltfu)
ltfu$motherdeath_d <- convertdate(motherdeath_d,ltfu)
ltfu$fatherdeath_d <- convertdate(fatherdeath_d,ltfu)

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basltfu <- merge(ltfu,with(basic,data.frame(patient,birth_d,enrol_d)),all.x=TRUE)
	basltfu$birth_d <- convertdate(birth_d,basltfu)
	basltfu$enrol_d <- convertdate(enrol_d,basltfu)
	outoforder(birth_d,death_d,basltfu,table2="tblBAS")
	outoforder(birth_d,l_alive_d,basltfu,table2="tblBAS")
	outoforder(birth_d,transfer_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,death_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,l_alive_d,basltfu,table2="tblBAS")
	outoforder(enrol_d,transfer_d,basltfu,table2="tblBAS")
	outoforder(birth_d,motherdeath_d,basltfu,table2="tblBAS")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(l_alive_d,death_d,ltfu)
outoforder(transfer_d,death_d,ltfu)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(death_d,ltfu)
futuredate(l_alive_d,ltfu)
futuredate(transfer_d,ltfu)
futuredate(motherdeath_d,ltfu)
futuredate(fatherdeath_d,ltfu)

## CHECK FOR DUPLICATE PATIENT IDs
queryduplicates(patient,ltfu)

## CHECK FOR UNEXPECTED CODING
## ???? LDR - are checks for cases where the accuracy (_a) variables are populated, but no date is provided needed??
badcodes(drop_y,c(0,1,9),ltfu)
badcodes(death_y,c(0,1,9),ltfu)
badcodes(motherdeath_y,c(0,1,9),ltfu)
badcodes(fatherdeath_y,c(0,1,9),ltfu)
badcodes(death_d_a,c("<",">","D","M","Y","U"),ltfu)
badcodes(l_alive_d_a,c("<",">","D","M","Y","U"),ltfu)
badcodes(transfer_d_a,c("<",">","D","M","Y","U"),ltfu)

################### QUERY CHECKING ENDS HERE ###################
