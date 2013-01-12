#############################################################
#
#   Program: tblRNA_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblRNA.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblRNA.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 9 November 2012
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_RNA"
## READ TABLE
rna <- read.csv(paste("input/",tablename,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(rna) <- tolower(names(rna))
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","rna_d","rna_v","rna_l","rna_t")
acceptablenames <- c(expectednames,"rna_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"rna")
missvar(expectednames,"rna")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("rna_d","rna")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("rna_d",rna)){rna$rna_d <- convertdate("rna_d","rna")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if("input/tblBAS.csv" %in% list.files(path="input")){
	basic <- read.csv("tblBAS.csv",header=TRUE,stringsAsFactors = FALSE)
	rna <- merge(rna,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	rna$birth_d <- convertdate("birth_d","rna")
	outoforder("birth_d","rna_d","rna",table2="tblBAS")
}
if("input/tblLTFU.csv" %in% list.files(path="input")){
	ltfu <- read.csv("tblLTFU.csv",header=TRUE,stringsAsFactors = FALSE)
  rna <- merge(rna,with(ltfu,data.frame(patient,drop_d,death_d)),all.x=TRUE)
	rna$drop_d <- convertdate("drop_d","rna")
	rna$death_d <- convertdate("death_d","rna")
	outoforder("rna_d","drop_d","rna",table2="tblLTFU")
	outoforder("rna_d","death_d","rna",table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("rna_d","rna")

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks)
notnumeric("rna_v","rna")
notnumeric("rna_l","rna")

## CHECK FOR MISSING DATA
missingvalue("rna_d","rna")
missingvalue("rna_v","rna")
# it's okay for "rna_l" and "rna_t" to be missing 

## CONVERT TO NUMERIC OR FORCE MISSING FOR NON-NUMERIC
rna$rna_v <- forcenumber(rna$rna_v)

## CHECK FOR DUPLICATE PATIENT IDs + RANGE CHECKS
queryduplicates("patient","rna",date="rna_d")
upperrangecheck("rna_v",10000000,"rna")
lowerrangecheck("rna_v",-1,"rna")
outoforder("rna_l","rna_v","basic")


## CHECK FOR UNEXPECTED CODING
badcodes("rna_t",c("5","10","15","19","20","21","29","31","32","33","39","40","41","50","51","55","56","65","66","90","99"),"rna")
badcodes("rna_d_a",c("<",">","D","M","Y","U"),"rna")

## QUERY PATIENTS WITH NO RECORD IN tblBAS
badrecord("patient","rna","basic")

################### QUERY CHECKING ENDS HERE ###################





