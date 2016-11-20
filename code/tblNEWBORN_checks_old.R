#############################################################
#
#   Program: tblNEWBORN_checks.R
#   Project: IeDEA
# 
#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblNEWBORN.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblNEWBORN.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 23 June 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblNEWBORN"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("child_id","entry_pmtct_y","prenat_mtct_y","infant_mtct_y","infant_mtct_dur","postpar_mtct_y","breastfd_y","breastfd_dur","abnorm_y")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,newborn)
missvar(expectednames,newborn)



## ??? Should any field present be flagged for missing
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CHECK FOR UNEXPECTED CODING

badcodes(entry_pmtct_y,c(0,1,9),newborn,id=child_id)
badcodes(prenat_mtct_y,c(0,10,11,12,13,14,99),newborn,id=child_id)
badcodes(infant_mtct_y,c(0,10,11,12,13,14,99),newborn,id=child_id)
badcodes(infant_mtct_dur,c(0,1,88,99),newborn,id=child_id)
badcodes(postpar_mtct_y,c(0,1,9),newborn,id=child_id)
badcodes(breastfd_y,c(0,1,9),newborn,id=child_id)
badcodes(abnorm_y,c(0,1,9),newborn,id=child_id)
#???? need to check conditionally the following:
#     - if infant_mtct_y is 0 or 99 and infant_mtct_dur not = 88
subsetofinterest<-(newborn$infant_mtct_y == '0' | newborn$infant_mtct_y == '99') & !is.na(newborn$infant_mtct_y)
badcodes(infant_mtct_dur,c(88),newborn[subsetofinterest,],id=child_id,
         auxcriteria = paste(": infant_mtct_dur value not consistent with infant_mtct_y=",newborn[subsetofinterest,]$infant_mtct_y),error="logic",query="Inconsistent values")
#     - if infant_mtct_y is 10, 11, 12, 13, 14 and infant_mtct_dur is not 0,1,99
subsetofinterest<-!(newborn$infant_mtct_y %in% c('0','99')) & !is.na(newborn$infant_mtct_y)
badcodes(infant_mtct_dur,c(0,1,99),newborn[subsetofinterest,],id=child_id,
         auxcriteria = paste(": infant_mtct_dur value not consistent with infant_mtct_y=",newborn[subsetofinterest,]$infant_mtct_y),error="logic",query="Inconsistent values")
#     - if breastfd_y is 1 then check that breastfd_dur is numeric
subsetofinterest<-newborn$breastfd_y == '1' & !is.na(newborn$breastfd_y)
if (any(subsetofinterest)) {
  notnumeric(breastfd_dur,newborn[subsetofinterest,],id=child_id)
}
# if breastfd_y is 1 then check that and breastfd_dur is numeric, check that breastfd_dur is in the allowable range (???? SWAGGED this - needs to be verified)
subsetofinterest<-(newborn$breastfd_y == '1' & !is.na(newborn$breastfd_y)) & !(grepl("[[:alpha:]]",newborn$breastfd_dur))
if (any(subsetofinterest)) {
   lowerrangecheck(breastfd_dur,0,newborn[subsetofinterest,],id=child_id)
   upperrangecheck(breastfd_dur,48,newborn[subsetofinterest,],id=child_id)  
}

# Get rows where breastfd_dur is populated and numeric
subsetofinterest<-(is.finite(newborn$breastfd_dur))
# if any of the rows have a breastfd_dur with numeric value
# flag an error if breastfd_y does not have a value of 1 (Yes)
if (any(subsetofinterest)) {
  badcodes(breastfd_y,c(1),newborn[subsetofinterest,],id=child_id,
           auxcriteria = paste(": breastfd_y value not consistent with breastfd_dur=",newborn[subsetofinterest,]$breastfd_dur),error="logic",query="Inconsistent values")
}


# Check for existence of the child_id in table newbornabnorm  when abnorm_y=1 is specified for the child_id in table newborn
if (exists("newbornabnorm")) {
  if (nrow(newborn[newborn$abnorm_y=="1" &  !is.na(newborn$abnorm_y),])>0) {
    missrecord(child_id,subset=newborn[newborn$abnorm_y=="1" &  !is.na(newborn$abnorm_y),],superset=newbornabnorm,
               subsettext=" child_id with abnorm_y='1' not found in tblNEWBORN_ABNORM",id=child_id,subsettext_txtonly="Yes")
  }
}


################### QUERY CHECKING ENDS HERE ###################
