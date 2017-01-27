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
## Modified for Harmonist - Look for a file containing the expected names.
## If the file of expected names doesn't exist, use the defaults for the table
if (file.exists("./input/Specification_of_required_and_optional_columns.tsv")) {

   column_specs<-read.table("./input/Specification_of_required_and_optional_columns.tsv",header = TRUE, sep="\t", stringsAsFactors=FALSE)
   # get the specs for tblART
   expectednames<-unlist(strsplit(gsub('\"','',column_specs[column_specs$tbl==tablename,]$required_columns),','))
   acceptablenames<-c(expectednames,unlist(strsplit(gsub('\"','',column_specs[column_specs$tbl==tablename,]$optional_columns),',')))

}
if (!(file.exists("./input/Specification_of_required_and_optional_columns.tsv"))) { 
  expectednames <- c("patient","art_id","art_sd","art_ed","art_rs","art_rs2","art_rs3","art_rs4","art_form","art_comb","artstart_rs")
  acceptablenames <- c(expectednames,"art_sd_a","art_ed_a")  
}




################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,art)
missvar(expectednames,art)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
if(exists("art_sd",art)){notdate(art_sd,art)}
if(exists("art_ed",art))notdate(art_ed,art)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("art_sd",art)){art$art_sd <- convertdate(art_sd,art)}
if(exists("art_ed",art)){art$art_ed <- convertdate(art_ed,art)}

## CHECK FOR MISSING DATA
if(exists("art_id",art)){missingvalue(art_id,art)}
if(exists("art_sd",art)){missingvalue(art_sd,art)}



## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic") && exists("birth_d",basic)){
	basart <- merge(art,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basart$birth_d <- convertdate(birth_d,basart)
	outoforder(birth_d,art_sd,basart,table2="tblBAS")
	outoforder(birth_d,art_ed,basart,table2="tblBAS")
}
if(exists("ltfu") && exists("death_d",ltfu) ){
  ltfuart <- merge(art,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfuart$death_d <- convertdate(death_d,ltfuart)
	outoforder(art_sd,death_d,ltfuart,table2="tblLTFU")
	outoforder(art_ed,death_d,ltfuart,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("art_sd",art) && exists("art_ed",art)){outoforder(art_sd,art_ed,art)}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
if(exists("art_sd",art)){futuredate(art_sd,art)}
if(exists("art_ed",art)){futuredate(art_ed,art)}

## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
  art_sub <- art[art$art_id %in% i,]
  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
if(exists("art_rs",art)){notnumeric(art_rs,art)}
if(exists("art_rs1",art)){notnumeric(art_rs1,art)}
if(exists("art_rs2",art)){notnumeric(art_rs2,art)}
if(exists("art_rs3",art)){notnumeric(art_rs3,art)}
if(exists("art_rs4",art)){notnumeric(art_rs4,art)}

## CHECK FOR UNEXPECTED CODING
art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
if(exists("art_id",art)){badcodes(art_id,art_id_codebook$code,art,id=patient)}
# ???? Is it OK for art_rs to be blank and art_rs2 to be populated - similarly for the rs2, rs3, rs4
if(exists("art_rs",art)){badcodes(art_rs,art_rs_codebook$code,art[art$art_rs != ' ',],id=patient)}
if(exists("art_rs1",art)){badcodes(art_rs1,art_rs_codebook$code,art[art$art_rs1 != ' ',],id=patient)}
if(exists("art_rs2",art)){badcodes(art_rs2,art_rs_codebook$code,art[art$art_rs2 != ' ',],id=patient)}
if(exists("art_rs3",art)){badcodes(art_rs3,art_rs_codebook$code,art[art$art_rs3 != ' ',],id=patient)}
if(exists("art_rs4",art)){badcodes(art_rs4,art_rs_codebook$code,art[art$art_rs4 != ' ',],id=patient)}
# ART Formulations:
#   1 = Tablet/capsule
#   2 = Syrup/suspension
#   3 = Combination of 1 and 2
#   4 = Powder
#   5 = Subcutaneous
#   6 = Intravenous
#   7 = Intramuscular
#   9 = Unknown
if(exists("art_form",art)){badcodes(art_form,c("1","2","3","4","5","6","7","9"),art)}
# ART Combinations
#   0 = Individual drug
#   1 = Part of a fixed-dose combination
#   9 = Unknown
if(exists("art_comb",art)){badcodes(art_comb,c("0","1","9"),art)}
if(exists("art_sd_a",art)){badcodes(art_sd_a,c("<",">","D","M","Y","U"),art)}
if(exists("art_ed_a",art)){badcodes(art_ed_a,c("<",">","D","M","Y","U"),art)}
## Stephany had comments concerning changing the reason for start specification
## In 2016-11-16 BD2K meeting it was decided to reduce the reasons for start to just 6 codes
art_startrs_codebook <- read.csv("resource/art_startrs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
if(exists("artstart_rs",art)){badcodes(artstart_rs,art_startrs_codebook$code,art)}


if (exists("basic")) {missrecord(patient,art,basic)}
    
# ## NEED TO PROGRAM:
# Overlapping periods of same drug
# Double reporting - records reported for both combination drugs and their components
# Periods of overlap of contra-indicated drugs
# Restart of same drug without a stop 

################### QUERY CHECKING ENDS HERE ###################
