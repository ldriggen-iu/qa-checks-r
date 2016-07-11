#############################################################
#
#   Program: tblDELIVERY_CHILD_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblDELIVERY_MUM.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblDELIVERY_MUM.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 26 February 2016
#   Revisions: 
#     
#############################################################
#???? Date needed in table for matching
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblDELIVERY_MUM"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("mother_id","preg_seq","rom_dur","rom_dur_a","deliv_location","planned_home","deliv_location","deliv_assist","tear_y")
acceptablenames <- c(expectednames,"deliv_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,deliverymum)
missvar(expectednames,deliverymum)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
# need a date in this table ??? notdate(deliv_d,deliverychild,id=mother_id)

##??? all vars required to be non-missing???
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
#need a date in this table ??? if(exists("deliv_d",deliverychild)){deliverychild$deliv_d <- convertdate(deliv_d,deliverychild)}


## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#if(exists("basic")){
#	basart <- merge(art,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
#	basart$birth_d <- convertdate(birth_d,basart)
#	outoforder(birth_d,art_sd,basart,table2="tblBAS")
#	outoforder(birth_d,art_ed,basart,table2="tblBAS")
#}
#if(exists("ltfu")){
#        ltfuart <- merge(art,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
#	ltfuart$death_d <- convertdate(death_d,ltfuart)
#	outoforder(art_sd,death_d,ltfuart,table2="tblLTFU")
#	outoforder(art_ed,death_d,ltfuart,table2="tblLTFU")
#}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
#outoforder(art_sd,art_ed,art)

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
#futuredate(art_sd,art)
#futuredate(art_ed,art)

## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks, if applicable)
notnumeric(preg_seq,deliverymum)
notnumeric(rom_dur,deliverymum)
notnumeric(deliv_location,deliverymum)
notnumeric(planned_home,deliverymum)
notnumeric(deliv_assist,deliverymum)
notnumeric(tear_y,deliverymum)

## CHECK FOR UNEXPECTED CODING
upperrangecheck(preg_seq,25,deliverymum,subsettext="",id=mother_id)
lowerrangecheck(preg_seq,1,deliverymum,subsettext="",id=mother_id)
upperrangecheck(rom_dur,40,deliverymum,subsettext="",id=mother_id)
lowerrangecheck(rom_dur,0,deliverymum,subsettext="",id=mother_id)
badcodes(rom_dur_a,c("<",">","="),deliverymum,id=mother_id)
badcodes(deliv_location,c(1,2,3,9),deliverymum,id=mother_id)
badcodes(planned_home,c(0,1,9),deliverymum,id=mother_id)
badcodes(deliv_assist,c(1,2,3,4,9),deliverymum,id=mother_id)
badcodes(tear_y,c(0,1,9),deliverymum,id=mother_id)



# ## NEED TO PROGRAM:


################### QUERY CHECKING ENDS HERE ###################
