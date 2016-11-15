#############################################################
#
#   Program: tblPREG_OUT_checks.R
#   Project: IeDEA-BD2K (Based on the IeDEA-DES Project - PI Firas Wehbe (MD,PhD), Statistian/Programmer Meridith Blevins  (MS))

#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblPREG_OUT.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblPREG_OUT.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 11 March 2016
#   Revisions: 
#     
#############################################################
#???? XXX_childnotincarehivstatus variable needs a definition
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblPREG_OUT"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
## ???? assuming all fields are required
#expectednames <- c("mother_id","preg_seq","child_id","outcom","outcom_d","b_gagew",
#                   "XXX_childnotincarehivstatus")
#acceptablenames <- c(expectednames,"outcom_d_a")
expectednames <- c("mother_id","preg_seq","child_id","outcom","outcom_d","outcom_d_a","b_gagew",
                   "XXX_childnotincarehivstatus")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,pregout)
missvar(expectednames,pregout)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(outcom_d,pregout,id=mother_id)

## ??? Should any field present be flagged for missing
## ??? General question on missingvalue vs badcodes check
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
pregout$outcom_d <- convertdate(outcom_d,pregout)


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basictmp <- basic
  basictmp$mother_id <- basictmp$patient
	baspregout <- merge(pregout,with(basictmp,data.frame(mother_id,birth_d)),all.x=TRUE)
	baspregout$birth_d <- convertdate(birth_d,baspregout)
	outoforder(birth_d,outcome_d,baspregout,table2="tblBAS",id=mother_id)
}
if(exists("ltfu")){
  ltfutmp <- ltfu
  ltfutmp$mother_id <- ltfutmp$patient
  ltfupregout <- merge(pregout,with(ltfutmp,data.frame(mother_id,death_d)),all.x=TRUE)
	ltfupregout$death_d <- convertdate(death_d,ltfupregout)
	outoforder(outcom_d,death_d,ltfupregout,table2="tblLTFU",id=mother_id)
}


## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(outcome_d,pregout)



## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(preg_seq,pregout,id=mother_id)
notnumeric(outcom,pregout,id=mother_id)
notnumeric(b_gagew,pregout,id=mother_id)


## CHECK FOR UNEXPECTED CODING
upperrangecheck(preg_seq,20,pregout,subsettext="",id=mother_id)
lowerrangecheck(preg_seq,1,pregout,subsettext="",id=mother_id)
badcodes(outcom,c(4,10,11,20,21,22),pregout,id=mother_id)
badcodes(outcom_d_a,c("<",">","D","M","Y","U"),pregout,id=mother_id)
upperrangecheck(b_gagew,50,pregout,subsettext="",id=mother_id)
lowerrangecheck(b_gagew,0,pregout,subsettext="",id=mother_id)

## CHECK FOR MOTHER_ID/PREG_SEQ IN tblPREG
if(exists("preg")){
  pregtmp <- preg
  pregtmp$mother_id_preg_seq <- paste("mother_id:",pregtmp$mother_id,"/ preg_seq: ",pregtmp$preg_seq)
  pregout$mother_id_preg_seq <- paste("mother_id:",pregout$mother_id,"/ preg_seq: ",pregout$preg_seq)
  badrecord(mother_id_preg_seq,pregout,pregtmp,id=mother_id)
}

## CHECK FOR MOTHER_ID/PREG_SEQ IN tblPREG
if(exists("preg")){
  pregtmp <- preg
  pregtmp$mother_id_preg_seq <- paste("mother_id:",pregtmp$mother_id,"/ preg_seq: ",pregtmp$preg_seq)
  pregout$mother_id_preg_seq <- paste("mother_id:",pregout$mother_id,"/ preg_seq: ",pregout$preg_seq)
  badrecord(mother_id_preg_seq,pregout,pregtmp,id=mother_id)
}

################### QUERY CHECKING ENDS HERE ###################
