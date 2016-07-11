#############################################################
#
#   Program: tblLAB_BP_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_BP.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB_BP.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 01 March 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_BP"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","bp_d","bp_sys","bp_dia","bp_u")
acceptablenames <- c(expectednames,"bp_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,bp)
missvar(expectednames,bp)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(bp_d,bp,id=patient)

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("bp_d",lab)){bp$bp_d <- convertdate(bp_d,bp)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basbp <- merge(bp,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basbp$birth_d <- convertdate(birth_d,basbp)
	outoforder(birth_d,bp_d,basbp,table2="tblBAS")
}
if(exists("ltfu")){
  ltfubp <- merge(bp,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfubp$death_d <- convertdate(death_d,ltfubp)
	outoforder(bp_d,death_d,ltfubp,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
## ??? this is blowing up for some reason ???futuredate(bp_d,bp,id=patient)

#??? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(bp_u,bp)

##??? range checks on BP by units???

## CHECK FOR UNEXPECTED CODING
bp_u_codebook <- read.csv("resource/bp_u_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(bp_d_a,c("<",">","D","M","Y","U"),bp)
badcodes(bp_u,bp_u_codebook$code,bp)


# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
