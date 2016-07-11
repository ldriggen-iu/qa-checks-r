#############################################################
#
#   Program: tblDIS_TB_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblDIS_TB.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblDIS_TB.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 26 February 2016
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblDIS_TB"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","tbtx_sd","tbtx_sd_a","tbtx_ed","tbtx_ed_a","tbsite","extra_site","inh_res_y","rif_res_y","intermit","tb_outcome")
acceptablenames <- c(expectednames,"tbdx_d","tbdx_d_a","ipt_y","dot_y")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,distb)
missvar(expectednames,distb)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(tbtx_sd,distb,id=patient)
notdate(tbtx_ed,distb,id=patient)
if(exists("tbdx_d",distb)){notdate(tbdx_d,distb,id=patient)}

## ??? Should any field present be flage for missning
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)

distb$tbtx_sd <- convertdate(tbtx_sd,distb)
distb$tbtx_ed <- convertdate(tbtx_ed,distb)
if(exists("tbdx_d",distb)){distb$tbdx_d <- convertdate(tbdx_d,distb)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basdistb <- merge(distb,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basdistb$birth_d <- convertdate(birth_d,basdistb)
	outoforder(birth_d,tbtx_sd,basdistb,table2="tblBAS")
	outoforder(birth_d,tbtx_ed,basdistb,table2="tblBAS")
	if(exists("tbdx_d",distb)){outoforder(birth_d,tbtx_d,basdistb,table2="tblBAS")}
}
if(exists("ltfu")){
        ltfudistb <- merge(distb,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfudistb$death_d <- convertdate(death_d,ltfuart)
	outoforder(tbtx_sd,death_d,ltfudistb,table2="tblLTFU")
	outoforder(tbtx_ed,death_d,ltfudistb,table2="tblLTFU")
	if(exists("tbdx_d",distb)){outoforder(tbdx_d,death_d,ltfudistb,table2="tblBAS")}
}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
outoforder(tbtx_sd,tbtx_ed,distb)
if(exists("tbdx_d",distb)){outoforder(tbdx_d,tbtx_sd,distb)}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
#futuredate(art_sd,art)
#futuredate(art_ed,art)

## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
#notnumeric(art_rs,art)

## CHECK FOR UNEXPECTED CODING

badcodes(tbtx_sd_a,c("<",">","D","M","Y","U"),distb)
badcodes(tbtx_ed_a,c("<",">","D","M","Y","U"),distb)
badcodes(tbsite,c(0,1,2,9),distb)
badcodes(extra_site,c(0,1,2,3,4,5,6,7,8,9,10,11),distb)
badcodes(inh_res_y,c(0,1,9),distb)
badcodes(rif_res_y,c(0,1,9),distb)
badcodes(intermit,c(1,2,3,4,5,6,7,-1,9),distb) #???? need to ask Bev on this one
badcodes(tb_outcome,c(1,2,3,4,5,6),distb)
badcodes(extra_site,c(0,1,2,3,4,5,6,7,8,9,10,11),distb)

if(exists("tbdx_d_a",distb)){badcodes(tbtx_d_a,c("<",">","D","M","Y","U"),distb)}
if(exists("ipt_y",distb)){badcodes(ipt_y,c(0,1,9),distb)}
if(exists("dot_y",distb)){badcodes(dot_y,c(0,1,9),distb)}


################### QUERY CHECKING ENDS HERE ###################
