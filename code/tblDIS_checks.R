#############################################################
#
#   Program: tblDIS_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblDIS.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblDIS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 9 October 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblDIS"
## READ TABLE
dis <- read.csv(paste("input/",tablename,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
dis <- dis[!is.na(dis$patient),]
names(dis) <- tolower(names(dis))
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","dis_id","dis_d","dis_ed","dis_wd")
acceptablenames <- c(expectednames,"dis_d_a","dis_ed_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,"dis")
missvar(expectednames,"dis")

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate("dis_d","dis")
notdate("dis_ed","dis")

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("dis_d",dis)){dis$dis_d <- convertdate("dis_d","dis")}
if(exists("dis_ed",dis)){dis$dis_ed <- convertdate("dis_ed","dis")}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if("tblBAS.csv" %in% list.files(path="input")){
	basic <- read.csv("input/tblBAS.csv",header=TRUE,stringsAsFactors = FALSE)
	dis <- merge(dis,with(basic,data.frame(patient,birth_d,recdis_y)),all.x=TRUE)
	dis$birth_d <- convertdate("birth_d","dis")
	outoforder("birth_d","dis_d","dis",table2="tblBAS")
        outoforder("birth_d","dis_ed","dis",table2="tblBAS")
}
if("tblLTFU.csv" %in% list.files(path="input")){
	ltfu <- read.csv("input/tblLTFU.csv",header=TRUE,stringsAsFactors = FALSE)
  dis <- merge(dis,with(ltfu,data.frame(patient,drop_d,death_d)),all.x=TRUE)
	dis$drop_d <- convertdate("drop_d","dis")
	dis$death_d <- convertdate("death_d","dis")
	outoforder("dis_d","drop_d","dis",table2="tblLTFU")
	outoforder("dis_d","death_d","dis",table2="tblLTFU")
	outoforder("dis_ed","drop_d","dis",table2="tblLTFU")
	outoforder("dis_ed","death_d","dis",table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder("dis_d","dis_ed","dis")

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate("dis_d","dis")
futuredate("dis_ed","dis")

## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(dis$dis_id)[!is.na(unique(dis$dis_id))]){
  dis_sub <- dis[dis$id %in% i,]
  queryduplicates("patient","dis_sub",date="dis_d",subsettext=paste("&dis_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric("dis_wd","dis")

## CHECK FOR MISSING DATA
missingvalue("dis_id","dis")
missingvalue("dis_d","dis")

## CHECK FOR UNEXPECTED CODING
dis_id_codebook <- read.csv("resource/dis_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
dis_wd_codebook <- read.csv("resource/dis_wd_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes("dis_id",dis_id_codebook$code,"dis")
badcodes("dis_wd",dis_wd_codebook$code,"dis")
badcodes("dis_d_a",c("<",">","D","M","Y","U"),"dis")
badcodes("dis_ed_a",c("<",">","D","M","Y","U"),"dis")

## QUERY PATIENTS WITH NO RECORD IN tblBAS
badrecord("patient","dis","basic")

# ## NEED TO PROGRAM ADDITIONAL CHECKS:
# tblDIS	WithinTable	DW006	Same event recorded twice - 2 records, same DIS_ID, DIS_D within 6 months		YES
# tblDIS	WithinTable	DW007	DIS_ED present but before DIS_D		YES
# tblDIS	CrossTable	DC002	AIDS-defining records, yet AIDS=0 in tblBAS		YES
# tblDIS	CrossTable	DC003	First AIDS-defining DIS_D not equal to AIDS_D in tblBAS		Y

################### QUERY CHECKING ENDS HERE ###################