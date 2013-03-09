#############################################################
#
#   Program: tblVIS_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblVIS.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblVIS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 16 January 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblVIS"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","vis_d","weigh","heigh","cdc_stage","who_stage")
acceptablenames <- c(expectednames,"vis_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"visit")
missvar(expectednames,"visit")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("vis_d","visit")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("vis_d",visit)){visit$vis_d <- convertdate("vis_d","visit")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basvisit <- merge(visit,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basvisit$birth_d <- convertdate("birth_d","visit")
	outoforder("birth_d","vis_d","basvisit",table2="tblBAS")
}
if(exists("ltfu")){
        ltfuvisit <- merge(visit,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfuvisit$death_d <- convertdate("death_d","visit")
	outoforder("vis_d","death_d","ltfuvisit",table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("vis_d","visit")

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks)
notnumeric("heigh","visit")
notnumeric("weigh","visit")

## CHECK FOR MISSING DATA
missingvalue("vis_d","visit")
# it's okay for others to be missing 

## CONVERT TO NUMERIC OR FORCE MISSING FOR NON-NUMERIC
if(exists("heigh",visit)){visit$heigh <- forcenumber(visit$heigh)}
if(exists("weigh",visit)){visit$weigh <- forcenumber(visit$weigh)}
if(exists("who_stage",visit)){visit$who_stage <- forcenumber(visit$who_stage)}

## FORCE MISSING VALUES AS NA FOR RANGE CHECKS
if(exists("weigh",visit)){visit$weigh[visit$weigh==999] <- NA}
if(exists("heigh",visit)){visit$heigh[visit$heigh==999] <- NA}

## CHECK FOR DUPLICATE PATIENT IDs + RANGE CHECKS
queryduplicates("patient","visit",date="vis_d")
upperrangecheck("weigh",120,"visit")
lowerrangecheck("weigh",0,"visit") # consider specifying lower limit for adult population
upperrangecheck("heigh",220,"visit")
lowerrangecheck("heigh",0,"visit") # consider specifying lower limit for adult population

## CHECK FOR UNEXPECTED CODING
badcodes("who_stage",c(1:4,9),"visit")
badcodes("cdc_stage",c("A","A1","A2","A3","B","B1","B2","B3","C","C1","C2","C3","N"),"visit")
badcodes("vis_d_a",c("<",">","D","M","Y","U"),"visit")

## QUERY PATIENTS WITH NO RECORD IN tblBAS
badrecord("patient","visit","basic")

################### QUERY CHECKING ENDS HERE ###################


## QUERY CHECKS TO CODE ##
#tblVIS	WithinTable	VW002	Height decreasing over time 		YES
#tblVIS	CrossTable	VC002	No weights within 3 mths of starting FPV/DRV


