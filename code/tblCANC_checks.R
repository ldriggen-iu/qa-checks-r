#############################################################
#
#   Program: tblCANC_checks.R
#   Project: IeDEA
# 
#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblCANC.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblCANC.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 26 February 2016
#   Revisions: ???? table was drastically revised IeDEA_DES_Proposed_Additions_2016_Nov_14_V14.docx
#              Need to check with Bev and Stephany on how to implement "Location coding System: ICD10, ICD9, other systems,
#              e.g. NA-ACCORD-short list (suggest using NA-ACCORD-short list: NA-ACCORD_Clinical_DxICD9_Mapping Update Sept 2014.xls)"
#              and "Histology coding system: ICD-O-3, other systems, e.g. NA-ACCORD-short list, None (suggest using NA-ACCORD-short list:
#              NA-ACCORD_Cancer_Registry_Dx_Mapping Update Sept 2014.xls)" code verification
#     
#############################################################
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblCANC"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
##???? Assuming all columns required for now
#expectednames <- c("patient","canc_d","loc_code","loc_code_sys","hist_code","hist_code_sys")
#acceptablenames <- c(expectednames,"canc_d_a")
expectednames <- c("patient","canc_d","canc_d_a","canc_type","canc_type_oth","canc_cert","canc_extent","canc_tx")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,canc)
missvar(expectednames,canc)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(canc_d,canc,id=patient)

## ??? should canc_d_a be a required field and checked here???
## ??? logic for canc_type_oth populated when canc_type=OTH
## CHECK FOR MISSING DATA
missingvalue(canc_d,canc)
#missingvalue(canc_d_a,canc)


## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
#if(exists("canc_d",canc)){canc$canc_d <- convertdate(canc_d,canc)}
#???? assuming the date is required
canc$canc_d <- convertdate(canc_d,canc)


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	bascanc <- merge(canc,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	bascanc$birth_d <- convertdate(birth_d,bascanc)
	outoforder(birth_d,canc_d,bascanc,table2="tblBAS")
}
if(exists("ltfu")){
  ltfucanc <- merge(canc,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfucanc$death_d <- convertdate(death_d,ltfucanc)
	outoforder(canc_d,death_d,ltfucanc,table2="tblLTFU")
}


## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(canc_d,canc)

##???? Currently checking for duplicates for canc_type/canc_d by patient
##???? Are futher checks needed?
## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(canc$canc_loc)[!is.na(unique(canc$canc_loc))]){
  canc_sub <- canc[canc$canc_type %in% i,]
  queryduplicates(patient,canc_loc,date=canc_d,subsettext=paste("&canc_loc=",i,sep=""))
}


## CHECK FOR UNEXPECTED CODING
canc_type_codebook <- read.csv("resource/canc_type_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(canc_d_a,c("<",">","D","M","Y","U"),canc)
badcodes(canc_type,canc_type_codebook$code,canc)
badcodes(canc_cert,c(1,2,9),canc)
badcodes(canc_extent,c(1,2,9),canc)
badcodes(canc_tx,c(1,2,3,4,5,9),canc)


################### QUERY CHECKING ENDS HERE ###################
