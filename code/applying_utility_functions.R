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
names(visit) <- tolower(names(visit))
basic <- read.csv(paste("input/tblBAS.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(basic) <- tolower(names(basic))
cd4 <- read.csv(paste("input/tblLAB_CD4.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(cd4) <- tolower(names(cd4))
art <- read.csv(paste("input/tblART.csv",sep=""),header=TRUE,stringsAsFactors = FALSE,na.strings=c("NA",""))
names(art) <- tolower(names(art))

# merge visit with basic, so that we can calculate 'baseline' values
visit <- merge(basic,visit)
# there are duplicates in the training visit data...
dups <- unsplit(lapply(split(visit$vis_d, visit$patient), FUN=anyDuplicated), visit$patient)
visit <- visit[dups == 0,]


source("code/utility_functions.R")


## EXAMPLES
base_who_stage <- getbaseline(enrol_d,vis_d,patient,value=who_stage,data=visit,returndate=TRUE,dateformat="%Y-%m-%d") # default is closest date with window -30,30

firstvisit <- getselectdate(vis_d,patient,data=visit,dateformat="%Y-%m-%d") # default is first
lastvisit <- getselectdate(vis_d,patient,type="last",data=visit,dateformat="%Y-%m-%d")

cd4value <- cd4[cd4$cd4_u==1,]
nadircd4 <- getnadirvalue(cd4_v,patient,data=cd4value,date=cd4_d,dateformat="%Y-%m-%d") 

firsthaart <- getselectdate(art_sd,patient,data=art,dateformat="%Y-%m-%d") # default is first

    # merge CD4 with basic (to get enrol_d) and with firsthaart (for date of art initiation)
    cd4 <- merge(merge(basic,cd4,all.y=TRUE),firsthaart,all.x=TRUE)
    # there are duplicates in the cd4 data...
    dups <- unsplit(lapply(split(cd4$cd4_d, paste(cd4$patient,cd4$cd4_u)), FUN=anyDuplicated), paste(cd4$patient,cd4$cd4_u))
    cd4 <- cd4[dups == 0,]

## GET CD4 COUNT ONLY BEFORE ART using "subset" feature
base_cd4_no_haart <- getbaseline(enrol_d,cd4_d,patient,value=cd4_v,data=cd4,returndate=TRUE,dateformat="%Y-%m-%d",subset=(cd4$cd4_u==1 & convertdate(cd4_d,cd4) <= convertdate(art_sd,cd4))) # default is closest date with window -30,30


# tblDIS	CrossTable	DC002	AIDS-defining records, yet AIDS=0 in tblBAS		YES
# tblDIS	CrossTable	DC003	First AIDS-defining DIS_D not equal to AIDS_D in tblBAS		Y

