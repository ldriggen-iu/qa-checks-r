#############################################################
#
#   Program: tblART_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblART.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblART.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 9 October 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblART"
## READ TABLE
art <- read.csv(paste("input/",tablename,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
art <- art[!is.na(art$patient),]
names(art) <- tolower(names(art))
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","art_id","art_sd","art_ed","art_rs")
acceptablenames <- c(expectednames,"art_sd_a","art_ed_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"art")
missvar(expectednames,"art")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("art_sd","art")
notdate("art_ed","art")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("art_sd",art)){art$art_sd <- convertdate("art_sd","art")}
if(exists("art_ed",art)){art$art_ed <- convertdate("art_ed","art")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if("tblBAS.csv" %in% list.files(path="input")){
	basic <- read.csv("input/tblBAS.csv",header=TRUE,stringsAsFactors = FALSE)
	art <- merge(art,with(basic,data.frame(patient,birth_d,recart_y)),all.x=TRUE)
	art$birth_d <- convertdate("birth_d","art")
	outoforder("birth_d","art_sd","art",table2="tblBAS")
	outoforder("birth_d","art_ed","art",table2="tblBAS")
}
if("tblLTFU.csv" %in% list.files(path="input")){
	ltfu <- read.csv("input/tblLTFU.csv",header=TRUE,stringsAsFactors = FALSE)
  art <- merge(art,with(ltfu,data.frame(patient,drop_d,death_d)),all.x=TRUE)
	art$drop_d <- convertdate("drop_d","art")
	art$death_d <- convertdate("death_d","art")
	outoforder("art_sd","drop_d","art",table2="tblLTFU")
	outoforder("art_sd","death_d","art",table2="tblLTFU")
	outoforder("art_ed","drop_d","art",table2="tblLTFU")
	outoforder("art_ed","death_d","art",table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder("art_sd","art_ed","art")

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("art_sd","art")
futuredate("art_ed","art")

## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
  art_sub <- art[art$id %in% i,]
  queryduplicates("patient","art_sub",date="art_sd",subsettext=paste("&art_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric("art_rs","art")

## CHECK FOR MISSING DATA
missingvalue("art_id","art")
missingvalue("art_sd","art")

## CHECK FOR UNEXPECTED CODING
art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes("art_id",art_id_codebook$code,"art")
badcodes("art_rs",art_rs_codebook$code,"art")
badcodes("art_sd_a",c("<",">","D","M","Y","U"),"art")
badcodes("art_ed_a",c("<",">","D","M","Y","U"),"art")

## CHECK FOR UNEXPECTED RECORDS
if(exists("basic")){
  art_unique <- art[!duplicated(art$patient),]
  basic_sub <- basic[basic$recart_y==1,]
  badrecord("patient","art_unique","basic_sub",subsettext="&recart_y!=1")
}

# ## NEED TO PROGRAM:
# Overlapping periods of same drug
# Double reporting - records reported for both combination drugs and their components
# Periods of overlap of contra-indicated drugs
# Restart of same drug without a stop 

################### QUERY CHECKING ENDS HERE ###################
