#############################################################
#
#   Program: load_data.R
#  
#   Project: Leverage IeDEA-DES for interactive graphics
#
#   Biostatisticians: Meridith Blevins, MS**
#                     Bryan E Shepherd, PhD
#   ** contact programmer: meridith.blevins at vanderbilt.edu
#
#   Purpose: Reproducible research.
#
#   Notes: 
#          
#   Created: 1 October 2013
#	  
#   Revisions: 
#   
#
#############################################################

## IDENTIFIED WHICH TABLES TO READ IN FROM DES IN MASTER PANEL_GRAPHICX.R PROGRAM

## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files("input"))
if(!all(readtables %in% existingtables)) stop(paste0("Please place the expected tables in /input folder named as specified: (",paste(readtables,collapse=", "),")"))
## READ IN ALL EXISTING TABLES
for(i in 1:length(readtables)){
  if(!is.na(readtables[i])){
     readcsv <- read.csv(paste("input/",readtables[i],".csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
     names(readcsv) <- tolower(names(readcsv))
     assign(readtables[i],readcsv)
   }
}
if(exists("art")) art$patient <- paste(art$patient,art$site,sep="-")
if(exists("basic")) basic$patient <- paste(basic$patient,basic$site,sep="-")
if(exists("follow")) follow$patient <- paste(follow$patient,follow$site,sep="-")
if(exists("lab_cd4")) lab_cd4$patient <- paste(lab_cd4$patient,lab_cd4$site,sep="-")
if(exists("lab_rna")) lab_rna$patient <- paste(lab_rna$patient,lab_rna$site,sep="-")
if(exists("visit")) visit$patient <- paste(visit$patient,visit$site,sep="-")
