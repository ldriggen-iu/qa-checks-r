#############################################################
#
#   Program: tblPREG_checks.R
#   Project: IeDEA-BD2K (Based on the IeDEA-DES Project - PI Firas Wehbe (MD,PhD), Statistian/Programmer Meridith Blevins  (MS))

#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblPREG.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblPREG.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 10 March 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblPREG"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
##???? force all columns to be present until determine which are optional
#expectednames <- c("mother_id","preg_seq","mens_d","est_concept_d","anc_d","preg_test_d",
#                  "ultr_1","ultr_a_1","ultr_2","ultr_a_2","ultr_3","ultr_a_3")
#acceptablenames <- c(expectednames,"mens_d_a","est_concept_d_a","anc_d_a","preg_test_d_a")
expectednames <- c("mother_id","preg_seq","mens_d","mens_d_a","est_concept_d","est_concept_d_a",
                   "anc_d","anc_d_a","preg_test_d","preg_test_d_a","num_fetus",
                   "ultr_1","ultr_a_1","ultr_2","ultr_a_2","ultr_3","ultr_a_3")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,preg)
missvar(expectednames,preg)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(mens_d,preg,id=mother_id)
notdate(est_concept_d,preg,id=mother_id)
notdate(anc_d,preg,id=mother_id)
notdate(preg_test_d,preg,id=mother_id)

## ??? Should any field present be flagged for missing
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)

preg$mens_d <- convertdate(mens_d,preg)
preg$est_concept_d <- convertdate(est_concept_d,preg)
preg$anc_d <- convertdate(anc_d,preg)
preg$preg_test_d <- convertdate(preg_test_d,preg)


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	baspreg <- merge(preg,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	baspreg$birth_d <- convertdate(birth_d,baspreg)
	outoforder(birth_d,mens_d,baspreg,table2="tblBAS")
	outoforder(birth_d,est_concept_d,baspreg,table2="tblBAS")
	outoforder(birth_d,anc_d,baspreg,table2="tblBAS")
	outoforder(birth_d,preg_test_d,baspreg,table2="tblBAS")
}
if(exists("ltfu")){
  ltfupreg <- merge(preg,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfupreg$death_d <- convertdate(death_d,ltfupreg)
	outoforder(mens_d,death_d,ltfupreg,table2="tblLTFU")
	outoforder(est_concept_d,death_d,ltfupreg,table2="tblLTFU")
	outoforder(anc_d,death_d,ltfupreg,table2="tblLTFU")
	outoforder(preg_test_d,death_d,ltfupreg,table2="tblLTFU")
}

## CHECK FOR INTERNAL TABLE DATES OCCURRING IN THE WRONG ORDER
outoforder(mens_d,est_concept_d,preg,table2="tblPREG",id=mother_id)
outoforder(est_concept_d,preg_test_d,preg,table2="tblPREG",id=mother_id)
outoforder(preg_test_d,anc_d,preg,table2="tblPREG",id=mother_id)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(mens_d,preg,id=mother_id)
futuredate(est_concept_d,preg,id=mother_id)
futuredate(anc_d,preg,id=mother_id)
futuredate(preg_test_d,preg,id=mother_id)


## CHECK FOR DUPLICATE PATIENT IDs 
for(i in unique(preg$preg_seq)[!is.na(unique(preg$preg_seq))]){
  preg_sub <- preg[preg$preg_seq %in% i,]
  queryduplicates(preg_seq,preg_sub,date=est_concept_d,subsettext=paste("&preg_seq=",i,sep=""),id=mother_id)
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(num_fetus,preg,id=mother_id)
notnumeric(ultr_1,preg,id=mother_id)
notnumeric(ultr_2,preg,id=mother_id)
notnumeric(ultr_3,preg,id=mother_id)

## CHECK FOR UNEXPECTED CODING

badcodes(mens_d_a,c("<",">","D","M","Y","U"),preg,id=mother_id)
badcodes(est_concept_d_a,c("<",">","D","M","Y","U"),preg,id=mother_id)
badcodes(anc_d_a,c("<",">","D","M","Y","U"),preg,id=mother_id)
badcodes(preg_test_d_a,c("<",">","D","M","Y","U"),preg,id=mother_id)
badcodes(ultr_1,c(0,1,2,9),preg,id=mother_id)
badcodes(ultr_2,c(0,1,2,9),preg,id=mother_id)
badcodes(ultr_3,c(0,1,2,9),preg,id=mother_id)

## CHECK FOR ULTRASOUNDS UNPOPULATED IN EARLIER TRIMESTERS,
## BUT POPULATED in LATER TRIMESTERS
##???? need to write a function to do this

################### QUERY CHECKING ENDS HERE ###################
