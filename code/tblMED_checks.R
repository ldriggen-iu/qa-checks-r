#############################################################
#
#   Program: tblLAB_MED_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_MED.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblMED.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 01 March 2016
#   Revisions: 
#     
#############################################################
#???? for reasons for discontinuation codes, used HICDEP reasons...
#???? should DES reasons or other reasons be used???
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblMED"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","med_id","med_sd","med_ed","med_rs","med_rs2","med_rs3","med_rs4")
acceptablenames <- c(expectednames,"med_sd_a","med_ed_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,med)
missvar(expectednames,med)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(med_sd,med,id=patient)
notdate(med_ed,med,id=patient)

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
med$med_sd <- convertdate(med_sd,med)
med$med_ed <- convertdate(med_ed,med)

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basmed <- merge(med,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basmed$birth_d <- convertdate(birth_d,basmed)
	outoforder(birth_d,med_sd,basmed,table2="tblBAS")
	outoforder(birth_d,med_ed,basmed,table2="tblBAS")
}
if(exists("ltfu")){
  ltfumed <- merge(med,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfumed$death_d <- convertdate(death_d,ltfumed)
	outoforder(med_sd,death_d,ltfumed,table2="tblLTFU")
	outoforder(med_ed,death_d,ltfumed,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(med_sd,med_ed,med)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(med_sd,med,id=patient)
futuredate(med_ed,med,id=patient)

#??? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(bp_u,bp)

##??? med_id really supposed to be numeric ???

## CHECK FOR UNEXPECTED CODING
med_id_codebook <- read.csv("resource/med_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
med_id_codebook <- read.csv("resource/med_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(med_sd_a,c("<",">","D","M","Y","U"),med)
badcodes(med_ed_a,c("<",">","D","M","Y","U"),med)
badcodes(med_id,med_id_codebook$code,med)
# ???? reasons for stopping???

# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
