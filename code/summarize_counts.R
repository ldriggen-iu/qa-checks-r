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
#   Created: 25 January 2012
#   Revisions: 
#     
#############################################################
## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY AND SET THE APPROPRIATE DATABASE CLOSE DATE
#setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")
#setwd("C:/Documents and Settings/blevinml/My Documents/Projects/IeDEAS/qa-checks-r")

## IDENTIFY WHICH TABLES TO SUMMARIZE
expectedtables <- c("center","basic","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblBAS","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS")
actualtables <- expectedtables[sapply(expectedtables,exists)]

getrecordcounts <- function(table,unique_id="patient"){
  x1 <- nrow(get(table))
  x2 <- length(unique(get(unique_id,get(table))))
  return(c(table,x1,x2))
  }
  
recordcounts <- t(sapply(actualtables,getrecordcounts))
recordcounts <- cbind(recordcounts[,1],expecteddestables[match(recordcounts[,1],expectedtables)],recordcounts[,2:3])
recordcounts <- data.frame(rbind(recordcounts,c("databaseclose","close_d",format(as.Date(databaseclose,origin="1970-01-01"),"%Y-%m-%d"),"")))
names(recordcounts) <- c("table","tbl","records","patients")
