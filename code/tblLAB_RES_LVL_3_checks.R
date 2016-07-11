#############################################################
#
#   Program: tblLAB_RES_LVL_3_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_RES_LVL_3.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB_RES_LVL_2.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 24 February 2016
#   Revisions: 
#     
#############################################################
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_RES_LVL_3"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("test_id","atc_code","res_scor_id")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,reslvl3)
missvar(expectednames,reslvl3)




#???? need to add duplicate checks for test_id (maybe test_id/atc_code)????
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

##???? need cross table check for test_id

## CHECK FOR UNEXPECTED CODING
badcodes(res_scor_id,c("S","L","I","H"),reslvl3,id=test_id)
med_id_codebook <- read.csv("resource/med_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(atc_code,med_id_codebook$code,reslvl3,id=test_id)


# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
