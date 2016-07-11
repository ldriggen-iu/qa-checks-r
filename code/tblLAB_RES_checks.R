#############################################################
#
#   Program: tblLAB_RES_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_RES.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB_RES.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 24 February 2016
#   Revisions: 
#     
#############################################################
#???? seq_dt - need a related accuracy variable, need to build a not dt check, or split into date time
#????          how to compare to sample_d (always s/b after)
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_RES"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","test_id","sample_d","seq_dt","lab",
                   "library","refseq","kit","software","testtype",
                   "virustype","subtype")
acceptablenames <- c(expectednames,"sample_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,res)
missvar(expectednames,res)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(sample_d,res,id=patient)

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("sample_d",res)){res$sample_d <- convertdate(sample_d,res)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basres <- merge(res,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basres$birth_d <- convertdate(birth_d,basres)
	outoforder(birth_d,sample_d,basres,table2="tblBAS")
}
if(exists("ltfu")){
  ltfures <- merge(res,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfures$death_d <- convertdate(death_d,ltfures)
	outoforder(sample_d,death_d,ltfures,table2="tblLTFU")
	outoforder(sample_d,death_d,ltfulres,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(sample_d,res)

##???? need to add sometype of checks for:
##     seq_dt, lab, library, refseq, kit, software


#???? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(testtype,res)
notnumeric(virustype,res)

## CHECK FOR UNEXPECTED CODING

badcodes(sample_d_a,c("<",">","D","M","Y","U"),res)
badcodes(testtype,c(1,2,9),res)
badcodes(virustype,c(1,2),res)

##????  Need check for subtype 


# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
