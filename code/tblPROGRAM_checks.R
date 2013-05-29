#############################################################
#
#   Program: tblPROGRAM_checks.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblPROGRAM.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblPROGRAM.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 29 May 2013
#   Revisions: 
#     
#############################################################

## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblPROGRAM"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
expectednames <- c("program","region")

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(expectednames,program)
missvar(expectednames,program)
                
## CHECK FOR MISSING DATA
missingvalue(program,program,id=program)
missingvalue(region,program,id=program)

## CHECK FOR DUPLICATE PROGRAM IDs
queryduplicates(program,program,id=program)

################### QUERY CHECKING ENDS HERE ###################
