#############################################################
#
#   Program: tblCD4_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblCD4.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblCD4.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 9 November 2012
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_CD4"
## READ TABLE
cd4 <- read.csv(paste("input/",tablename,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(cd4) <- tolower(names(cd4))
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","cd4_d","cd4_v","cd4_u")
acceptablenames <- c(expectednames,"cd4_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"cd4")
missvar(expectednames,"cd4")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("cd4_d","cd4")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("cd4_d",cd4)){cd4$cd4_d <- convertdate("cd4_d","cd4")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if("tblBAS.csv" %in% list.files(path="input")){
	basic <- read.csv("input/tblBAS.csv",header=TRUE,stringsAsFactors = FALSE)
	names(basic) <- tolower(names(basic))
	cd4 <- merge(cd4,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	cd4$birth_d <- convertdate("birth_d","cd4")
	outoforder("birth_d","cd4_d","cd4",table2="tblBAS")
}
if("tblLTFU.csv" %in% list.files(path="input")){
	ltfu <- read.csv("input/tblLTFU.csv",header=TRUE,stringsAsFactors = FALSE)
	names(ltfu) <- tolower(names(ltfu))
  cd4 <- merge(cd4,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	cd4$death_d <- convertdate("death_d","cd4")
	outoforder("cd4_d","death_d","cd4",table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("cd4_d","cd4")

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks)
notnumeric("cd4_v","cd4")

## CHECK FOR MISSING DATA
missingvalue("cd4_d","cd4")
missingvalue("cd4_v","cd4")
missingvalue("cd4_u","cd4")

## CONVERT TO NUMERIC OR FORCE MISSING FOR NON-NUMERIC
cd4$cd4_v <- forcenumber(cd4$cd4_v)

## CHECK FOR DUPLICATE PATIENT IDs + RANGE CHECKS
cd4_sub <- cd4[cd4$cd4_u==1,]
queryduplicates("patient","cd4_sub",date="cd4_d",subsettext="&cd4_u=1")
upperrangecheck("cd4_v",3000,"cd4_sub",subsettext="&cd4_u=1")
lowerrangecheck("cd4_v",0,"cd4_sub",subsettext="&cd4_u=1")
cd4_sub <- cd4[cd4$cd4_u==2,]
queryduplicates("patient","cd4_sub",date="cd4_d",subsettext="&cd4_u=2")
upperrangecheck("cd4_v",100,"cd4_sub",subsettext="&cd4_u=2")
lowerrangecheck("cd4_v",0,"cd4_sub",subsettext="&cd4_u=2")

## CHECK FOR UNEXPECTED CODING
badcodes("cd4_u",c(1,2),"cd4")
badcodes("cd4_d_a",c("<",">","D","M","Y","U"),"cd4")

## QUERY PATIENTS WITH NO RECORD IN tblBAS
badrecord("patient","cd4","basic")


################### QUERY CHECKING ENDS HERE ###################
