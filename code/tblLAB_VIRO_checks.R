#############################################################
#
#   Program: tblLAB_VIRO_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_VIRO.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB_VIRO.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 01 March 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_VIRO"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","vs_id","vs_d","vs_r","vs_v","vs_u","vs_st")
acceptablenames <- c(expectednames,"vs_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,viro)
missvar(expectednames,viro)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
if (exists("vs_d",viro)) {notdate(vs_d,viro,id=patient)}

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if (exists("vs_d",viro)) {viro$vs_d <- convertdate(vs_d,viro)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic") && exists("birth_d",basic) && exists("vs_d",viro)){
  basviro <- merge(viro,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basviro$birth_d <- convertdate(birth_d,basviro)
	outoforder(birth_d,vs_d,basviro,table2="tblBAS")
}
if(exists("ltfu") && exists("death_d",ltfu) && exists("vs_d",viro)) {
  ltfuviro <- merge(viro,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfuviro$death_d <- convertdate(death_d,ltfuviro)
	outoforder(vs_d,death_d,ltfuviro,table2="tblLTFU")
}
if(exists("ltfu") && exists("death_d",ltfu) && exists("l_alive_d",viro)) {
  ltfuviro <- merge(viro,with(ltfu,data.frame(patient,l_alive_d)),all.x=TRUE)
  ltfuviro$l_alive_d <- convertdate(l_alive_d,ltfuviro)
  outoforder(vs_d,l_alive_d,ltfuviro,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
if (exists("vs_d",viro)) {futuredate(vs_d,viro,id=patient)}

#??? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
if (exists(viro$vs_r)) {notnumeric(vs_r,viro)}
if (exists(viro$vs_u)) {notnumeric(vs_u,viro)}

##??? units on vs_u???
##??? range checks on vs_v - just for HVC-RNA and HBV-DNA???

## CHECK FOR UNEXPECTED CODING
## ???? LDR - need to check with Bev and Stephany on final set of codes to be included in vs_id_codebook.
vs_id_codebook <- read.csv("resource/vs_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
if (exists("vs_d_a",viro)) {badcodes(vs_d_a,c("<",">","D","M","Y","U"),viro)}
if (exists("vs_id",viro)) {badcodes(vs_id,vs_id_codebook$code,viro)}
if (exists("vs_st",viro)) {badcodes(vs_st,c("WB","P","S","U24","U","CSF","9"),viro)}

# Verify patient exists in tblBAS
if (exists("basic")) {missrecord(patient,canc,basic)}
# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
