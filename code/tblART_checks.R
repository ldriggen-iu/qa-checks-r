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
#   Revisions: Larry Riggen 22 June 2016
#              Added variables ART_FORM, ART_COMB, and ARTSTART_RS for
#              BD2K
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblART"
## READ TABLE
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","art_id","art_sd","art_ed","art_rs","art_rs2","art_rs3","art_rs4","art_form","art_comb","artstart_rs")
acceptablenames <- c(expectednames,"art_sd_a","art_ed_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,art)
missvar(expectednames,art)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(art_sd,art)
notdate(art_ed,art)

## CHECK FOR MISSING DATA
missingvalue(art_id,art)
missingvalue(art_sd,art)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("art_sd",art)){art$art_sd <- convertdate(art_sd,art)}
if(exists("art_ed",art)){art$art_ed <- convertdate(art_ed,art)}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basart <- merge(art,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basart$birth_d <- convertdate(birth_d,basart)
	outoforder(birth_d,art_sd,basart,table2="tblBAS")
	outoforder(birth_d,art_ed,basart,table2="tblBAS")
}
if(exists("ltfu")){
        ltfuart <- merge(art,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfuart$death_d <- convertdate(death_d,ltfuart)
	outoforder(art_sd,death_d,ltfuart,table2="tblLTFU")
	outoforder(art_ed,death_d,ltfuart,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(art_sd,art)
futuredate(art_ed,art)

## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
  art_sub <- art[art$id %in% i,]
  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(art_rs,art)
notnumeric(art_rs1,art)

## CHECK FOR UNEXPECTED CODING
art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(art_id,art_id_codebook$code,art,id=patient)
## ???? need to figure out how to check that populated rs->rs_2->rs_3->rs_4 and only check those that are populated.
## ???? the code below forces art_rs to always be populated (even if 99 - unknown) and art_rs2, art_rs3, art_rs4 to
## ???? be a valid code if they contain an entry
badcodes(art_rs,art_rs_codebook$code,art,id=patient)
badcodes(art_rs2,art_rs_codebook$code,art[art$art_rs2 !=' ',],id=patient)
badcodes(art_rs3,art_rs_codebook$code,art[art$art_rs3 !=' ',],id=patient)
badcodes(art_rs4,art_rs_codebook$code,art[art$art_rs4 !=' ',],id=patient)
badcodes(art_form,c("1","2","3","4","5","6","9"),art)
badcodes(art_comb,c("0","1","9"),art)
badcodes(art_sd_a,c("<",">","D","M","Y","U"),art)
badcodes(art_ed_a,c("<",">","D","M","Y","U"),art)
## ???? Stephany had comments concerning changing the reason for start specification
art_startrs_codebook <- read.csv("resource/art_startrs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(artstart_rs,art_startrs_codebook$code,art)
# ## NEED TO PROGRAM:
# Overlapping periods of same drug
# Double reporting - records reported for both combination drugs and their components
# Periods of overlap of contra-indicated drugs
# Restart of same drug without a stop 

################### QUERY CHECKING ENDS HERE ###################
