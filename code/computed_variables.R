#############################################################
#
#   Program: computed_variables.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for computing
#            variables routinely needed for medical research.
#
#   INPUT: 
#   OUTPUT: VARIABLE(_TYPE)_CMP attached to tblXXX_CMP.csv
#
#   Notes: 
#
#   Created: 2 July 2013
#   Revisions: 
#     
#############################################################

# set working directory
# setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")

## READ UTILITY_FUNCTIONS.R
source("code/utility_functions.R")

## IDENTIFY WHICH TABLES TO EXPECT FROM DES
expectedtables <- c("center","program","basic","ltfu","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblPROGRAM","tblBAS","tblLTFU","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS")
## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files("input"))
readtables <- expectedtables[match(existingtables,expecteddestables)]
## READ IN ALL EXISTING TABLES
for(i in 1:length(readtables)){
  if(!is.na(readtables[i])){
     readcsv <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
     names(readcsv) <- tolower(names(readcsv))
     assign(readtables[i],readcsv)
   }
}

## DUPLICATE VISIT DATES ARE A PROBLEM, SO REMOVE THEM IF THEY EXIST
## NOTE: These will be listed as data exceptions in the output of tbl_checks.R
dups <- unsplit(lapply(split(visit$vis_d, visit$patient), FUN=duplicated), visit$patient)
visit <- visit[!dups,]

## DETERMINE WHICH CENTER THE PATIENT VISITED FIRST, LAST, AND MOST OFTEN 
center_first <- getselectvar(date=vis_d,id=patient,var=center,type="first",data=visit,dateformat="%Y-%m-%d")
center_last <- getselectvar(date=vis_d,id=patient,var=center,type="last",data=visit,dateformat="%Y-%m-%d")
center_most <- getselectvar(date=vis_d,id=patient,var=center,type="most",data=visit,dateformat="%Y-%m-%d")

## DETERMINE THE FIRST DATE OF ART RECEIPT FROM TBLART
art_first <- getselectdate(art_sd,patient,data=art,dateformat="%Y-%m-%d") # default type="first"

## MERGE COMPUTED VARIABLES WITH tblBAS
## NOTE: THIS IS NOT CBIND() IN CASE A PATIENT IN BASIC DOES NOT HAVE A RECORD IN VISIT, AND IN CASE THEY ARE NOT IDENTICALLY SORTED
basic_cmp <- merge(merge(merge(merge(basic,center_first,all.x=TRUE),center_last,all.x=TRUE),center_most,all.x=TRUE),art_first,all.x=TRUE)

## WRITE TABLE FILES THAT HAVE COMPUTED VARIABLES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}
write.csv(basic_cmp,"output/tblBAS_CMP.csv",row.names=FALSE)
