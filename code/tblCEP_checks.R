#############################################################
#
#   Program: tblCEP_checks.R
#   Project: IeDEA-BD2K (Based on the IeDEA-DES Project - PI Firas Wehbe (MD,PhD), Statistian/Programmer Meridith Blevins  (MS))

#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblCEP.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblCEP.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 26 February 2016
#   Revisions: 
#     
#############################################################
#??? USAB duplicates in cep_id_codebook ????
#??? 2015-03-31 LDR I believe all the HICDEP QC checks have been applied to this table.
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblCEP"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
##???? Assuming all columns required
#expectednames <- c("patient","event_id","cep_d","cep_id","cep_spec","cep_v")
#acceptablenames <- c(expectednames,"cep_d_a")
expectednames <- c("patient","event_id","cep_d","cep_d_a","cep_id","cep_spec","cep_v")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,cep)
missvar(expectednames,cep)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(cep_d,cep,id=patient)

## CHECK FOR MISSING DATA
##???? need to research the use of missingvalue function vs coding/notdate
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
## ???? assuming date is required
#if(exists("cep_d",cep)){cep$cep_d <- convertdate(cep_d,cep)}
cep$cep_d <- convertdate(cep_d,cep)

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
   bascep <- merge(cep,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
   bascep$birth_d <- convertdate(birth_d,bascep)
	 outoforder(birth_d,cep_d,bascep,table2="tblBAS")
}
if(exists("ltfu")){
   ltfucep <- merge(cep,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
   ltfucep$death_d <- convertdate(death_d,ltfucep)
   outoforder(cep_d,death_d,ltfucep,table2="tblLTFU")
}


## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(cep_d,cep)


## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(cep$cep_id)[!is.na(unique(cep$cep_id))]){
  cep_sub <- cep[cep$cep_id %in% i,]
  queryduplicates(patient,cep_sub,date=cep_d,subsettext=paste("&cep_id=",i,sep=""))
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(cep_v,cep)

## CHECK FOR UNEXPECTED CODING
cep_id_spec_codebook <- read.csv("resource/cep_id_spec_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(cep_d_a,c("<",">","D","M","Y","U"),cep)
## ???? Do the two lines of code immediately below need to be repeated for blanks ???  
cep$cep_id_cep_spec[is.na(cep$cep_spec) == FALSE]<- paste(cep$cep_id,"-",cep$cep_spec,sep="")[is.na(cep$cep_spec) == FALSE]
cep$cep_id_cep_spec[is.na(cep$cep_spec) == TRUE]<- paste(cep$cep_id,"-","",sep="")[is.na(cep$cep_spec) == TRUE]
badcodes(cep_id_cep_spec,cep_id_spec_codebook$code,cep[is.na(cep$cep_spec)==FALSE,])




################### QUERY CHECKING ENDS HERE ###################
