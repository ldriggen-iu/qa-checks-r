#   Program: tblART_MUM_checks.R
#   Project: IeDEA/BD2K
# 
#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblART_MUM.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblART_MUM.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created:   November 2016 (added to new tables by BD2K team)
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblART_MUM"


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
  expectednames <- c("child_id","art_id","art_sd","art_ed","art_rs","art_rs2","art_rs3","art_rs4","art_form","art_comb","artstart_rs")
  acceptablenames <- c(expectednames,"art_sd_a","art_ed_a")  
}

expectednames <- c("child_id","art_id","art_sd","art_ed","art_rs","art_rs2","art_rs3","art_rs4","art_form","art_comb","artstart_rs")
acceptablenames <- c(expectednames,"art_sd_a","art_ed_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,artmum)
missvar(expectednames,artmum)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
if(exists("art_sd",artmum)){notdate(art_sd,artmum,id=child_id)}
if(exists("art_ed",artmum))notdate(art_ed,artmum,id=child_id)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("art_sd",artmum)){artmum$art_sd <- convertdate(art_sd,artmum)}
if(exists("art_ed",artmum)){artmum$art_ed <- convertdate(art_ed,artmum)}

## CHECK FOR MISSING DATA
if(exists("art_id",artmum)){missingvalue(art_id,artmum,id=child_id)}
if(exists("art_sd",artmum)){missingvalue(art_sd,artmum,id=child_id)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic") && exists("birth_d",basic)){
	basartmum <- merge(artmum,with(basic,data.frame(patient,birth_d)),by.x="child_id",by.y="patient",all.x=TRUE)
	basartmum$birth_d <- convertdate(birth_d,basartmum)
	outoforder(birth_d,art_sd,basartmum,table2="tblBAS",id=child_id)
	outoforder(birth_d,art_ed,basartmum,table2="tblBAS",id=child_id)
}
if(exists("ltfu") && exists("death_d",ltfu)){
  ltfuartmum <- merge(artmum,with(ltfu,data.frame(patient,death_d)),by.x="child_id",by.y="patient",all.x=TRUE)
	ltfuartmum$death_d <- convertdate(death_d,ltfuartmum)
	outoforder(art_sd,death_d,ltfuartmum,table2="tblLTFU",id=child_id)
	outoforder(art_ed,death_d,ltfuartmum,table2="tblLTFU",id=child_id)
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("art_sd",artmum) && exists("art_sd",artmum)) {outoforder(art_sd,art_ed,artmum,id=child_id)}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
if(exists("art_sd",artmum)) {futuredate(art_sd,artmum,id=child_id)}
if(exists("art_sd",artmum)) {futuredate(art_ed,artmum,id=child_id)}

## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(artmum$art_id)[!is.na(unique(artmum$art_id))]){
  artmum_sub <- artmum[artmum$art_id %in% i,]
  queryduplicates(child_id,artmum_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
if(exists("art_rs",artmum)){notnumeric(art_rs,artmum,id=child_id)}
if(exists("art_rs1",artmum)){notnumeric(art_rs1,artmum,id=child_id)}
if(exists("art_rs2",artmum)){notnumeric(art_rs2,artmum,id=child_id)}
if(exists("art_rs3",artmum)){notnumeric(art_rs3,artmum,id=child_id)}
if(exists("art_rs4",artmum)){notnumeric(art_rs4,artmum,id=child_id)}

## CHECK FOR UNEXPECTED CODING
art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
if(exists("art_id",artmum)){badcodes(art_id,art_id_codebook$code,artmum,id=child_id)}
# ???? Is it OK for art_rs to be blank and art_rs2 to be populated - similarly for the rs2, rs3, rs4
if(exists("art_rs",artmum)){badcodes(art_rs,art_rs_codebook$code,artmum[artmum$art_rs != ' ',],id=child_id)}
if(exists("art_rs",artmum)){badcodes(art_rs1,art_rs_codebook$code,artmum[artmum$art_rs1 != ' ',],id=child_id)}
if(exists("art_rs",artmum)){badcodes(art_rs2,art_rs_codebook$code,artmum[artmum$art_rs2 != ' ',],id=child_id)}
if(exists("art_rs",artmum)){badcodes(art_rs3,art_rs_codebook$code,artmum[artmum$art_rs3 != ' ',],id=child_id)}
if(exists("art_rs",artmum)){badcodes(art_rs4,art_rs_codebook$code,artmum[artmum$art_rs4 != ' ',],id=child_id)}
# ART Formulations:
#   1 = Tablet/capsule
#   2 = Syrup/suspension
#   3 = Combination of 1 and 2
#   4 = Powder
#   5 = Subcutaneous
#   6 = Intravenous
#   7 = Intramuscular
#   9 = Unknown
if(exists("art_form",artmum)){badcodes(art_form,c("1","2","3","4","5","6","7","9"),artmum,id=child_id)}
# ART Combinations
#   0 = Individual drug
#   1 = Part of a fixed-dose combination
#   9 = Unknown
if(exists("art_comb",artmum)){badcodes(art_comb,c("0","1","9"),artmum,id=child_id)}
if(exists("art_sd_a",artmum)){badcodes(art_sd_a,c("<",">","D","M","Y","U"),artmum,id=child_id)}
if(exists("art_ed_a",artmum)){badcodes(art_ed_a,c("<",">","D","M","Y","U"),artmum,id=child_id)}
## Stephany had comments concerning changing the reason for start specification
## In 2016-11-16 BD2K meeting it was decided to reduce the reasons for start to just 6 codes
art_startrs_codebook <- read.csv("resource/art_startrs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
if(exists("artstart_rs",artmum)){badcodes(artstart_rs,art_startrs_codebook$code,artmum,id=child_id)}

## ???? Need to code for child_id (modified to match mothers id if necessary) in tblMUM and not in tblBAS ????

# ## NEED TO PROGRAM:
# Overlapping periods of same drug
# Double reporting - records reported for both combination drugs and their components
# Periods of overlap of contra-indicated drugs
# Restart of same drug without a stop 

################### QUERY CHECKING ENDS HERE ###################
