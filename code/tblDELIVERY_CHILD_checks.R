#############################################################
#
#   Program: tblDELIVERY_CHILD_checks.R
#   Project: IeDEA/BD2K
# 
#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblDELIVERY_CHILD.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblDELIVERY_CHILD.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 24 February 2016
#   Revisions: 
#     
#############################################################
#???? may need to add B_SEQ and PREG_SEQ back into this tblDELIVERY_CHILD
#???? some HICDEP checks will need to be included (with modification)

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblDELIVERY_CHILD"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
#expectednames <- c("mother_id","child_enrol","child_id","deliv_d","deliv_m","breech_y")
#acceptablenames <- c(expectednames,"deliv_d_a")
#???? for now force all variables to be expected
expectednames <- c("mother_id","child_enrol","child_id","deliv_d","deliv_d_a","deliv_m","breech_y")
acceptablenames <- c(expectednames)


################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,deliverychild)
missvar(expectednames,deliverychild)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(deliv_d,deliverychild,id=mother_id)

## ???? For now require all fields be populated - missingvalue only checks numerics
## ???? Only missing values for the numeric variables are performed 
## ???? not sure the missing checks are needed since non-valid value check seem 
## ???? like they also catch "missingness" 
## CHECK FOR MISSING DATA
#missingvalue(mother_id,deliverychild,id=mother_id)
missingvalue(child_enrol,deliverychild,id=mother_id)
#missingvalue(child_id,deliverychild,id=mother_id)
#missingvalue(deliv_d,deliverychild,id=mother_id)
#missingvalue(deliv_d_a,deliverychild,id=mother_id)
missingvalue(deliv_m,deliverychild,id=mother_id)
missingvalue(breech_y,deliverychild,id=mother_id)


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basdeliverychild <- merge(deliverychild,with(basic,data.frame(patient,birth_d)),by.x="mother_id",by.y="patient",all.x=TRUE)
  basdeliverychild$birth_d <- convertdate(birth_d,basdeliverychild )
  outoforder(birth_d,deliv_d,basdeliverychild ,table2="tblBAS",id=mother_id)
}

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
#if(exists("deliv_d",deliverychild)){deliverychild$deliv_d <- convertdate(deliv_d,deliverychild)}
#???? assumption delivery date is required
deliverychild$deliv_d <- convertdate(deliv_d,deliverychild)

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basdeliverychild <- merge(deliverychild,with(basic,data.frame(patient,birth_d)),by.x="mother_id",by.y="patient",all.x=TRUE)
  basdeliverychild$birth_d <- convertdate(birth_d,basdeliverychild )
	outoforder(birth_d,deliv_d,basdeliverychild ,table2="tblBAS",id=mother_id)
}
if(exists("ltfu")){
  ltfudeliverychild <- merge(deliverychild,with(ltfu,data.frame(patient,death_d)),by.x="mother_id",by.y="patient",all.x=TRUE)
  ltfudeliverychild$death_d <- convertdate(death_d,ltfudeliverychild )
  outoforder(deliv_d,death_d,ltfudeliverychild ,table2="tblLTFU",id=mother_id)
}


## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(deliv_d,deliverychild,id=mother_id)


## CHECK FOR DUPLICATE mother_id/child_id on a delivery date 
## ??? need to add B_SEQ back into table to be able to handle
## ??? cases where there are multiple births
##
for(i in unique(deliverychild$child_id)[!is.na(unique(deliverychild$child_id))]){
  deliverychild_sub <- deliverychild[deliverychild$child_id %in% i,]
  queryduplicates(child_id,deliverychild_sub,date=deliv_d,subsettext=paste("&child_id=",i,sep=""),id=mother_id)
}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(child_enrol,deliverychild)
notnumeric(deliv_m,deliverychild)
notnumeric(breech_y,deliverychild)

## CHECK FOR UNEXPECTED CODING
#art_id_codebook <- read.csv("resource/art_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
#art_rs_codebook <- read.csv("resource/art_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
#badcodes(art_id,art_id_codebook$code,art)
#badcodes(art_rs,art_rs_codebook$code,art)
badcodes(child_enrol,c(0,1,9),deliverychild,id=mother_id)
badcodes(deliv_d_a,c("<",">","D","M","Y","U"),deliverychild,id=mother_id)
badcodes(deliv_m,c(1,2,3,4,5,10,11,12),deliverychild,id=mother_id)
badcodes(breech_y,c(0,1,9),deliverychild,id=mother_id)


# Check for mothers that aren't in tblPREG
if(exists("preg")){badrecord(mother_id,deliverychild,preg,id=mother_id)}

# Check for children that aren't in tblPREG_OUT
if(exists("pregout")){badrecord(child_id,deliverychild,pregout,id=child_id)}

# ## NEED TO PROGRAM:
## ???? other checks for delivery child

################### QUERY CHECKING ENDS HERE ###################
