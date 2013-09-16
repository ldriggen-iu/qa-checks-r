#############################################################
#
#   Program: tblVIS_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblVIS.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblVIS.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 16 January 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblVIS"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("patient","center","vis_d","weigh","heigh","cdc_stage","who_stage")
acceptablenames <- c(expectednames,"vis_d_a")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,visit)
missvar(expectednames,visit)

## PRIOR TO CONVERTING DATES, CHECK THAT THE TYPE IS APPROPRIATE 
notdate(vis_d,visit)

## CHECK FOR MISSING DATA
missingvalue(center,visit)
missingvalue(vis_d,visit)
# it's okay for others to be missing 

## CONVERT DATES USING EXPECTED FORMAT (will force NA if format is incorrect)
if(exists("vis_d",visit)){visit$vis_d <- convertdate(vis_d,visit)}

## CHECK FOR DATES OCCURRING IN THE WRONG ORDER
if(exists("basic")){
	basvisit <- merge(visit,with(basic,data.frame(patient,birth_d)),all.x=TRUE)
	basvisit$birth_d <- convertdate(birth_d,visit)
	outoforder(birth_d,vis_d,basvisit,table2="tblBAS")
}
if(exists("ltfu")){
        ltfuvisit <- merge(visit,with(ltfu,data.frame(patient,death_d)),all.x=TRUE)
	ltfuvisit$death_d <- convertdate(death_d,visit)
	outoforder(vis_d,death_d,ltfuvisit,table2="tblLTFU")
}

## CHECK FOR DATES OCCURRING TOO FAR IN THE FUTURE
futuredate(vis_d,visit)

## CHECK FOR INCORRECT VARIABLE TYPE (prior to range checks)
notnumeric(heigh,visit)
notnumeric(weigh,visit)

## CONVERT TO NUMERIC OR FORCE MISSING FOR NON-NUMERIC
if(exists("heigh",visit)){visit$heigh <- forcenumber(visit$heigh)}
if(exists("weigh",visit)){visit$weigh <- forcenumber(visit$weigh)}
if(exists("who_stage",visit)){visit$who_stage <- forcenumber(visit$who_stage)}

## FORCE MISSING VALUES AS NA FOR RANGE CHECKS
if(exists("weigh",visit)){visit$weigh[visit$weigh==999] <- NA}
if(exists("heigh",visit)){visit$heigh[visit$heigh==999] <- NA}

## CHECK FOR DUPLICATE PATIENT IDs + RANGE CHECKS
queryduplicates(patient,visit,date=vis_d)
upperrangecheck(weigh,120,visit)
lowerrangecheck(weigh,0,visit) # consider specifying lower limit for adult population
upperrangecheck(heigh,220,visit)
lowerrangecheck(heigh,0,visit) # consider specifying lower limit for adult population

## CHECK FOR UNEXPECTED CODING
badcodes(who_stage,c(1:4,9),visit)
badcodes(cdc_stage,c("A","A1","A2","A3","B","B1","B2","B3","C","C1","C2","C3","9"),visit)
badcodes(vis_d_a,c("<",">","D","M","Y","U"),visit)

## QUERY PATIENTS WITH NO RECORD IN tblBAS (bad ID)
badrecord(patient,visit,basic)
## QUERY PATIENTS WITH MISSING RECORD IN tblVIS (has ID, but no VISIT data)
missrecord(patient,basic,visit)

## QUERY PATIENTS WITH NO RECORD IN tblCENTER
if(exists("center")){badrecord(center,visit,center,id=patient)}


## QUERY ANY HEIGHT DECREASES FROM ONE VISIT TO THE NEXT
if(exists("heigh",visit)){
    qheigh <- visit[with(visit,order(patient,vis_d)),]
    qheigh <- qheigh[!is.na(qheigh$heigh),]
    qheigh$heightdelta <- with(qheigh,unsplit(lapply(split(heigh, patient), FUN=function(x) c(NA, diff(x))), patient))
    recerr <- which(!is.na(qheigh$heightdelta) & qheigh$heightdelta < 0)
    if(length(recerr)>0){
	query <- data.frame(qheigh$patient[recerr],
		tablename,
		"heigh",
		"Logic",
		"Out of Range",
		paste(paste0("vis_d=",qheigh$vis_d[recerr-1]),paste0("heigh=",qheigh$heigh[recerr-1]),
		      paste0("vis_d=",qheigh$vis_d[recerr]),  paste0("heigh=",qheigh$heigh[recerr]),sep="&"),
		stringsAsFactors=FALSE)
	names(query) <- names(emptyquery)
	assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
    }
}
################### QUERY CHECKING ENDS HERE ###################


## QUERY CHECKS TO CODE ##
# pediatric ranges
