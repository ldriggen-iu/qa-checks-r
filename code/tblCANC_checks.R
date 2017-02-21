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
## Modified for Harmonist - Look for a file containing the expected names.
## If the file of expected names doesn't exist, use the defaults for the table
if (file.exists("./input/Specification_of_required_and_optional_columns.tsv")) {
  
  column_specs<-read.table("./input/Specification_of_required_and_optional_columns.tsv",header = TRUE, sep="\t", stringsAsFactors=FALSE)
  # get the specs for tblART
  expectednames<-unlist(strsplit(gsub('\"','',column_specs[column_specs$tbl==tablename,]$required_columns),','))
  acceptablenames<-c(expectednames,unlist(strsplit(gsub('\"','',column_specs[column_specs$tbl==tablename,]$optional_columns),',')))
  
}
if (!(file.exists("./input/Specification_of_required_and_optional_columns.tsv"))) { 
  expectednames <- c("patient","canc_d","loc_code","loc_code_sys","hist_code","hist_code_sys")
  acceptablenames <- c(expectednames,"canc_d_a")
}

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,canc)
missvar(expectednames,canc)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
if(exists("canc_d",canc)) {(notdate(canc_d,canc,id=patient))}

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("canc_d",canc)){canc$canc_d <- convertdate(canc_d,canc)}

## CHECK FOR MISSING DATES
if(exists("canc_d",canc)) {missingvalue(canc_d,canc)}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic") && exists("birth_d",basic) && exists("canc_d",canc)){
	bascanc <- merge(canc,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	bascanc$birth_d <- convertdate(birth_d,bascanc)
	outoforder(birth_d,canc_d,bascanc,table2="tblBAS")
}
if(exists("ltfu") && exists("death_d",ltfu) && exists("canc_d",canc)){
  ltfucanc <- merge(canc,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfucanc$death_d <- convertdate(death_d,ltfucanc)
	outoforder(canc_d,death_d,ltfucanc,table2="tblLTFU")
}
if(exists("ltfu") && exists("l_alive_d",ltfu) && exists("canc_d",canc)){
  ltfucanc <- merge(canc,with(ltfu,data.frame(patient,l_alive_d)),all.x=TRUE)
  ltfucanc$l_alive_d <- convertdate(l_alive_d,ltfucanc)
  outoforder(canc_d,l_alive_d,ltfucanc,table2="tblLTFU")
}
## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
if(exists("canc_d",canc)){futuredate(canc_d,canc)}

## CHECK FOR Invalid location codes (only for NA-ACCORD-short list at this time)
if(exists("loc_code_sys",canc) && exists ("loc_code",canc) && canc$loc_code_sys=="NA-ACCORD-short list") {
  badcodes(loc_code,c(20,39,9,33,8,1,12,62,64,65,51),canc)
}


## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(canc$loc_code)[!is.na(unique(canc$loc_code))]){
  canc_sub <- canc[canc$loc_code %in% i,]
  queryduplicates(patient,canc_sub,date=canc_d,subsettext=paste("&loc_code=",i,sep=""))
}

## ???? need some help from Bev and Stephany on how to code histology. 
## CHECK FOR UNEXPECTED CODING
#canc_type_codebook <- read.csv("resource/canc_type_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
#badcodes(canc_d_a,c("<",">","D","M","Y","U"),canc)
#badcodes(canc_type,canc_type_codebook$code,canc)
#badcodes(canc_cert,c(1,2,9),canc)
#badcodes(canc_extent,c(1,2,9),canc)
#badcodes(canc_tx,c(1,2,3,4,5,9),canc)

# Verify patient exists in tblBAS
if (exists("basic")) {missrecord(patient,canc,basic)}

################### QUERY CHECKING ENDS HERE ###################
