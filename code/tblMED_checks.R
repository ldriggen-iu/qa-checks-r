#############################################################
#
#   Program: tblLAB_MED_checks.R
#   Project: IeDEA
# 
#   PIs: Constantin Yiannoutsos, PhD; Stephany Duda, PhD; Beverly Music, MS
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_MED.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblMED.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 01 March 2016
#   Revisions: 
#     
#############################################################
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblMED"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","med_id","med_sd","med_ed","med_rs","med_rs2","med_rs3","med_rs4","med_fr")
acceptablenames <- c(expectednames,"med_sd_a","med_ed_a","medstart_rs","med_do","dot_y")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,med)
missvar(expectednames,med)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(med_sd,med,id=patient)
notdate(med_ed,med,id=patient)

##??? require all variables to be present (even _A's)
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
med$med_sd <- convertdate(med_sd,med)
med$med_ed <- convertdate(med_ed,med)

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
  basmed <- merge(med,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basmed$birth_d <- convertdate(birth_d,basmed)
	outoforder(birth_d,med_sd,basmed,table2="tblBAS")
	outoforder(birth_d,med_ed,basmed,table2="tblBAS")
}
if(exists("ltfu")){
  ltfumed <- merge(med,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfumed$death_d <- convertdate(death_d,ltfumed)
	outoforder(med_sd,death_d,ltfumed,table2="tblLTFU")
	outoforder(med_ed,death_d,ltfumed,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(med_sd,med_ed,med)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(med_sd,med,id=patient)
futuredate(med_ed,med,id=patient)

#??? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}


## CHECK FOR UNEXPECTED CODING
if(exists("med_sd_a",med)){badcodes(med_sd_a,c("<",">","D","M","Y","U"),med)}
if(exists("med_ed_a",med)){badcodes(med_ed_a,c("<",">","D","M","Y","U"),med)}

med_id_codebook <- read.csv("resource/med_id_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(med_id,med_id_codebook$code,med)

med_rs_codebook <- read.csv("resource/med_rs_codebook.csv",header=TRUE,stringsAsFactors = FALSE,na.strings="")
badcodes(med_rs,med_rs_codebook$code,med)
badcodes(med_rs2,med_rs_codebook$code,med)
badcodes(med_rs3,med_rs_codebook$code,med)
badcodes(med_rs4,med_rs_codebook$code,med)

#medstart_rs - Reason for starting medication (optional)
#   1 = Treatment(incl. for presumptive dx)
#   2 = Prophylaxis (primary or secondary)
if(exists("medstart_rs",med)){badcodes(medstart_rs,c("1","2"),med)}

# ???? LDR need to talk with Bev and Stephany on how to handle the dosing frequency
# ???? for now just checking if it is -1 and less 24
notnumeric(med_fr,med)
med$med_fr<-forcenumber(med$med_fr)

lowerrangecheck(med_fr,-1,med,subsettext="",id=patient)
upperrangecheck(med_fr,24,med,subsettext="",id=patient)


# ???? LDR need to talk with Bev and Stephany on how to handle the dose
# ???? for now just checking if it is > 0 and is not populated when
# ???? dose_fr is -1 

if(exists("med_do",med)){
  #when dose frequency is populated and dose exists, dose should be populated as well ???? LDR check
  notnumeric(med_do,med[med$med_fr != -1 & is.na(med$med_fr)==FALSE,])
  #when dose frequency is -1, dose should not be populated
  badcodes(med_do,c(" "),med[med$med_fr == -1 | is.na(med$med_fr)==TRUE,]) 
  med$med_fr<-forcenumber(med$med_fr)
  lowerrangecheck(med_do,0,med[med$med_fr != -1 & is.na(med$med_fr)==FALSE,],subsettext="",id=patient)
}

if(exists("dot_y",med)){
  notnumeric(dot_y,med)
  badcodes(dot_y,c("1","2","9"),med)
}


# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
