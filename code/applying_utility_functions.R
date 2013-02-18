#############################################################
#
#   Program: applying_utility_functions.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for deriving
#            variables routinely needed for medical research.
#
#   INPUT: 
#   OUTPUT: standard R functions
#
#   Notes: 
#
#   Created: 11 February 2013
#   Revisions: 
#     
#############################################################

# set working directory
setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")

# read in pertinent tables
visit <- read.csv(paste("input/tblVIS.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
basic <- read.csv(paste("input/tblBAS.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
cd4 <- read.csv(paste("input/tblLAB_CD4.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))

# merge visit with basic, so that we can calculate 'baseline' values
visit <- merge(basic,visit)
names(visit) <- tolower(names(visit))
# there are duplicates in the training visit data...
dups <- unsplit(lapply(split(visit$vis_d, visit$patient), FUN=anyDuplicated), visit$patient)
visit <- visit[dups == 0,]





## EXAMPLES
base_who_stage <- getbaseline(enrol_d,vis_d,patient,value=who_stage,data=visit,returndate=TRUE,dateformat="%Y-%m-%d") # default is closest date with window -30,30

firstvisit <- getselectdate(vis_d,patient,data=visit,dateformat="%Y-%m-%d") # default is first
lastvisit <- getselectdate(vis_d,patient,type="last",data=visit,dateformat="%Y-%m-%d")

cd4value <- cd4[cd4$cd4_u==1,]
nadircd4 <- getnadirvalue(cd4_v,patient,data=cd4value,date=cd4_d,dateformat="%Y-%m-%d") # default is first

