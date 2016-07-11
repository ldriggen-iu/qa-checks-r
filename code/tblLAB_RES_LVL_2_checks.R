#############################################################
#
#   Program: tblLAB_RES_LVL_2_checks.R
#   Project: IeDEA
# 
#   PI: Stephany Duda, PhD
#   Programmer: Larry Riggen, MS
#   Purpose: Read in IeDEAS standard and write  
#            data queries
#
#   INPUT: "tblLAB_RES_LVL_2.csv"
#   OUTPUT: 
#
#   Notes: As long as the working directory in "setwd" is
#          correctly pointing to the location of tblLAB_RES_LVL_2.csv,
#          then this code should run smoothly, generating
#          a listing of data queries.
#
#   Created: 24 February 2016
#   Revisions: 
#     
#############################################################
#???? need to ask about aa_pos and aa_pos_sub (will use a-z for now for aa_pos_sub)
## NAME OF TABLE FOR WRITING QUERIES
tablename <- "tblLAB_RES_LVL_2"
## NAMES EXPECTED FROM HICDEP+/IeDEAS DES
##????? test_id links to tblLAB_RES ???
expectednames <- c("test_id","gene","aa_pos","aa_pos_sub",
                   "aa_found_1","aa_found_2","aa_found_3","aa_found_4")
acceptablenames <- c(expectednames)

################### QUERY CHECKING BEGINS HERE ###################

## CHECK FOR EXTRA OR MISSING VARIABLES
extravar(acceptablenames,reslvl2)
missvar(expectednames,reslvl2)


##???? require all variables to be present (even _A's) 
##???? appears test_id, gene, aa_pos, aa_pos_sub, and aa_found_1 are required.
## CHECK FOR MISSING DATA
#missingvalue(art_id,deliverychild)
#missingvalue(art_sd,deliverychild)





#???? need to add duplicate checks
## CHECK FOR DUPLICATE PATIENT IDs 
#for(i in unique(art$art_id)[!is.na(unique(art$art_id))]){
#  art_sub <- art[art$id %in% i,]
#  queryduplicates(patient,art_sub,date=art_sd,subsettext=paste("&art_id=",i,sep=""))
#}

##???? need cross table check for test_id
##???? need a check for AA_FOUND_x but nothing in AA_FOUND_(x-1)
## CHECK FOR UNEXPECTED CODING
## need other for gene ????
badcodes(gene,c("PRO","RT","GP41","GP120"),reslvl2,id=test_id)
## need range for aa_pos ???? ???? also notnumeric doesn't seem to be working here ????
notnumeric(aa_pos,table=reslvl2,id=test_id)
## ???? not sure aa_pos_sub is correctly checked
badcodes(aa_pos_sub,c("a","b","c","d","e","f","g","h","i",
                      "j","k","l","m","n","o","p","q","r",
                      "s","t","u","v","w","x","y","z"),reslvl2,id=test_id)



# ## NEED TO PROGRAM:
## ???? other checks ????

################### QUERY CHECKING ENDS HERE ###################
