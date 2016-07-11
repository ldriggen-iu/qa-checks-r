#############################################################
#
#   Program: tblLAB_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 24 February 2016
#   Revisions: Larry Riggen 23 June 2016
#              added new variables lab_fa,lab_st,drug_res,tb_drug
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","lab_id","lab_d","lab_v","lab_u","lab_fa","lab_st","drug_res","tb_drug")
acceptablenames <- c(expectednames,"lab_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,lab)
missvar(expectednames,lab)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(lab_d,lab,id=patient)

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("lab_d",lab)){lab$lab_d <- convertdate(lab_d,lab)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  baslab <- merge(lab,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	baslab$birth_d <- convertdate(birth_d,baslab)
	outoforder(birth_d,lab_d,baslab,table2="tblBAS")
}
if(exists("ltfu")){
  ltfulab <- merge(lab,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfulab$death_d <- convertdate(death_d,ltfulab)
	outoforder(lab_d,death_d,ltfulab,table2="tblLTFU")
	outoforder(lab_d,death_d,ltfulab,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(lab_d,lab)

#??? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(lab_v,lab)
notnumeric(lab_u,lab)

## CHECK FOR UNEXPECTED CODING
lab_id_codebook <- read.csv("resource/lab_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
lab_u_codebook <- read.csv("resource/lab_u_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(lab_d_a,c("<",">","D","M","Y","U"),lab)
badcodes(lab_id,lab_id_codebook$code,lab)
## ??? need to think about how to accommodate numeric values as well as the coded dipstick results
## ??? just allow any numeric value - can this be qualified by test by using the known dipstick tests ???
## ??? are only the test that have "Dipstick" in their name dipstick tests???
badcodes(lab_v,c(-1,0,1,2,3,4,9,99),lab)
badcodes(lab_u,lab_u_codebook$code,lab)
badcodes(lab_fa,c(0,1,9),lab)
badcodes(lab_st,c("WB","P","S","U24","U"),lab)
badcodes(drug_res,c(0,1,88),lab)
badcodes(tb_drug,c("INH_L","INH_H","INH_U","PZA","RIF","ETN","ETB","STREP","QUI","88"),lab)

# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
