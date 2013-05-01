#############################################################
#
#   Program: summarize_counts.R
#   Project: IeDEA
# 
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Get record and patient counts for each data tables
#            for inclusion in summary reports
#
#   INPUT: "tbl_XXX.csv"
#   OUTPUT: "counts_yyyymmdd.csv"
#
#   Notes: As long as the working directory structure 
#          matches README.md, such that the data tables,
#          R-code, and resources may be sourced, 
#          then this code should run smoothly, generating
#          a listing of data queries in /output.
#
#   Created: 25 January 2013
#   Revisions: 
#     
#############################################################
## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY AND SET THE APPROPRIATE DATABASE CLOSE DATE
#setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")
#setwd("C:/Documents and Settings/blevinml/My Documents/Projects/IeDEAS/qa-checks-r")

## IDENTIFY WHICH TABLES TO EXPECT FROM DES
## STILL NEED TO INCORPORATE tblLTFU and tblCENTER
expectedtables <- c("center","basic","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblBAS","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS") 

## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))

## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files("input"))
existingtables <- existingtables[match(expecteddestables,existingtables)]
existingtables <- existingtables[!is.na(existingtables)]
readtables <- expectedtables[match(existingtables,expecteddestables)]


## READ IN ALL EXISTING TABLES
for(i in 1:length(readtables)){
  if(!is.na(readtables[i])){
    readcsv <- read.csv(paste("input/",existingtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
    names(readcsv) <- tolower(names(readcsv))
    assign(readtables[i],readcsv)
  }
}

getrecordcounts <- function(table,unique_id="patient",subset=basic$patient){
  x1 <- nrow(get(table)[get(unique_id,get(table)) %in% subset,])
  x2 <- length(unique(get(unique_id,get(table))[get(unique_id,get(table)) %in% subset]))
  return(c(table,x1,x2))
  }




recordcounts <- rbind(getrecordcounts(table="center",unique_id="center",subset=center$center),t(sapply(readtables[-1],getrecordcounts)))
recordcounts <- data.frame(existingtables,recordcounts[,2:3])
names(recordcounts) <- c("tbl","records","patients")

## WRITE COUNT FILE -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}
write.csv(recordcounts,paste("output/counts_",format(Sys.Date(),"%Y%m%d"),".csv",sep=""),row.names=FALSE)


