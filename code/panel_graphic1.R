#############################################################
#
#   Program: panel_graphic1.R
#  
#   Project: Leverage DES for interactive graphics
#
#   Biostatisticians: Meridith Blevins, MS**
#                     Bryan E Shepherd, PhD
#   ** contact programmer: meridith.blevins at vanderbilt.edu
#
#   Purpose: Reproducible research.
#
#   Notes: 
#          
#   Published: 6 March 2013
#	  
#   Revisions: 
#   
#
#############################################################
# setwd("~/Projects/IeDEAS/qa-checks-r")
# source("code/panel_graphic1.R")
rm(list=ls()) # SHOULD BE FIRST LINE IN ALL R PROGRAMS - CLEARS NAMESPACE
library(scales)
library(RColorBrewer)
library(rms)


set.seed(2)

## READ IN UTILITY FUNCTIONS
source("code/utility_functions.R")

specs <- read.csv("input/panel1_specs.csv",header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))

for(i in 1:nrow(specs)){
  if(!is.na(specs$specification[i])){
     assign(specs$name[i],specs$specification[i])
  }
  if(is.na(specs$specification[i])){
     if(specs$name[i] %in% c('id','longtablename','longvar','longvardate','eventtablename','event','enddate',
                             'grouptablename','group','starttablename','startdate')){
        stop(paste("Mandatory variable has not been specified:",specs$name[i]))
     }
  }
}  
## EVALUATE ASSIGNED VARIABLES
if(exists("longsubset")){longsubset <- eval(parse(text=longsubset))}
if(exists("maxtime")){maxtime <- eval(parse(text=maxtime))}
if(exists("long2eventwindow")){long2eventwindow <- eval(parse(text=long2eventwindow))}
if(exists("longvarlim")){longvarlim <- eval(parse(text=longvarlim))}
if(exists("problim")){problim <- eval(parse(text=problim))}
if(exists("longlabels")){longlabels <- eval(parse(text=longlabels))}

## READ IN DATA FILES
readtables <- unique(c(longtablename,eventtablename,grouptablename,starttablename))
source("code/load_data.R")

## SET DEFAULTS 1
if(!exists("longvartrans")) longvartrans <- "I"
if(!exists("long2eventwindow")) long2eventwindow <- 12*30
if(!exists("maxtime")) maxtime <- 730
if(!exists("longsubset")) longsubset <- function(longvar) !is.na(longvar) 
if(!exists("starttype")) starttype <- "first"
if(longvartrans=="I") longvarbacktrans <- match.fun(I)
if(longvartrans=="sqrt") longvarbacktrans <- function (x) x^2
if(longvartrans=="log") longvarbacktrans <- match.fun(exp)

## PULL IN RELEVANT DATA AND FORMATS
## LONGITUDINAL
longtable <- get(longtablename)
longtable$longvardate <- get(longvardate,longtable)
longtable$longvardate <- convertdate(longvardate,longtable)
longtable$longvar <- get(longvar,longtable)
longtable$id <- get(id,longtable)
longtable <- longtable[sapply(longtable$longvar,longsubset),]
longtable$tlongvar <- sapply(longtable$longvar,longvartrans)
longtable <- with(longtable,data.frame(id,longvar,tlongvar,longvardate))
if(!exists("longvarlim")) longvarlim <- c(sort(longtable$longvar)[round(c(0.005,0.995)*nrow(longtable))])
## EVENT OCCURRENCE AND TIME
eventtable <- get(eventtablename)
eventtable$enddate <- get(enddate,eventtable)
eventtable$enddate <- convertdate(enddate,eventtable)
eventtable$event <- get(event,eventtable)
eventtable$id <- get(id,eventtable)
eventtable <- with(eventtable,data.frame(id,event,enddate))
if(any(duplicated(eventtable$id))) {stop("ERROR: There are duplicate IDs in the event table.")}
## GROUPING VARIABLE
grouptable <- get(grouptablename)
grouptable$group <- get(group,grouptable)
grouptable$id <- get(id,grouptable)
if(!exists("groupsubset")) groupsubset <- unique(grouptable$group) 
grouptable <- with(grouptable[grouptable$group %in% groupsubset,],data.frame(id,group))
if(any(duplicated(grouptable$id))) {stop("ERROR: There are duplicate IDs in the group table.")}
## START TIME
starttable <- get(starttablename)
starttable$startdate <- get(startdate,starttable)
starttable$id <- get(id,starttable)
onestart <- getselectdate(startdate,id,type=starttype,data=starttable,dateformat="%Y-%m-%d")

## DROP MISSING VALUES OR MISSING DATES FROM LONGITUDINAL VARIABLE
longtable <- longtable[!is.na(longtable$longvardate) & !is.na(longtable$longvar),]
longtable <- longtable[!duplicated(paste(longtable$id,longtable$longvardate)),]

## DROP IDs THAT ARE NOT COMMON BETWEEN TABLES
keepid <- unique(intersect(starttable$id,intersect(eventtable$id,intersect(longtable$id,grouptable$id))))
starttable <- starttable[starttable$id %in% keepid,]
eventtable <- eventtable[eventtable$id %in% keepid,]
longtable <- longtable[longtable$id %in% keepid,]
grouptable <- grouptable[grouptable$id %in% keepid,]

## MERGE EVENT/START DATA WITH LONGITUDINAL
startgroupevent <- merge(merge(grouptable,onestart,all.x=TRUE),eventtable)
startgroupevent$start2end <- as.numeric(convertdate(enddate,startgroupevent) - get(names(onestart)[2],startgroupevent))
startgroupevent <- startgroupevent[!is.na(startgroupevent$group),]
longevent <- merge(longtable,startgroupevent)
longevent$start2long <- longevent$longvardate - get(names(onestart)[2],longevent)
longevent <- longevent[!is.na(longevent$start2long) & longevent$start2long>=0,]  # keep only post-START values
longevent <- longevent[!is.na(longevent$group),]

longevent <- droplevels(longevent)
startgroupevent <- droplevels(startgroupevent)
n <- nrow(startgroupevent)
n1 <- table(startgroupevent$group)
tgroup <- names(n1)
ngroup <- length(tgroup)

S<-Surv(startgroupevent$start2end,startgroupevent$event)
problim2 <- 1-summary(survfit(S~1),times=c(maxtime))$upper
for(i in tgroup){
  assign(paste0("plot_lowess_event",i),lowess(x=longevent$start2long[longevent$group==i],y=longevent$tlongvar[longevent$group==i]))
  assign(paste0("S.",i),Surv(startgroupevent$start2end[startgroupevent$group==i],startgroupevent$event[startgroupevent$group==i]))
  problim2 <- max(problim2,1-summary(survfit(get(paste0("S.",i))~1),times=c(maxtime))$upper)
}
if(!exists("problim")){problim <- c(0,problim2*1.4)}
if(ngroup>9){stop("ERROR: Maximum number of groupings is 9.")}
grouppalette <- brewer.pal(min(9,max(3,ngroup)), "Set1")
grouppastel <- brewer.pal(min(9,max(3,ngroup)), "Pastel1")
formatch <- data.frame(grouppastel[1:ngroup],tgroup)
formatch2 <- data.frame(grouppalette[1:ngroup],tgroup)
## ACTUAL DATA, AS OPPOSED TO VALUES CARRIED FORWARD
groupcolvec1 <- formatch[match(longevent$group,formatch2[,2]),1]
groupcolvec1 <- droplevels(groupcolvec1)



######  PUT LAST VALUE AS EVENTY
longevent <- longevent[order(longevent$start2long),]
longevent <- longevent[order(longevent$id),]
longatevent <- getbaseline(baselinedate=enddate,visitdate=longvardate,id,value=longvar,before=long2eventwindow,data=longevent,returndate=TRUE) # default is closest date with window -180,30
longevent2 <- merge(startgroupevent,longatevent,all.x=TRUE)
eventy <- sapply(longevent2$longvar_cmp[longevent2$event==1],longvartrans)
deathx <- longevent2$start2end[longevent2$event==1]
groupcolvec2 <- formatch2[match(longevent2$group,formatch2[,2]),1]
groupcolvec2 <- droplevels(groupcolvec2)
eventcol <- groupcolvec2[longevent2$event==1]

if(!exists("longlabels")) longlabels <- longvarbacktrans(pretty(sapply(longvarlim,longvartrans),n=5))

## WRITE PICTURE FILES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}
if(!file.exists("output/scroll_images")){dir.create(file.path(wd,"output/scroll_images"))}
for(i in 1:maxtime) 
{
    png(paste0("output/scroll_images/panel_graphic1_",sprintf("%04d",i),".png"),res=100,width=500,height=800) 
    par(mar=c(3.5,3,1.2,.7),mgp=c(2.2,1,0))
    ## PANEL 2
    par(fig=c(0,0.9,0.5,1))
    plot(1:10,1:10,ylim=sapply(longvarlim,longvartrans),xlim=c(0,min(i+5,maxtime)),col=0,axes=FALSE,
        main=paste("Days 0 to ",i,"",sep=""),xlab="Days from Start",ylab="Longitudinal Value")
    points(x=longevent$start2long,y=longevent$tlongvar,pch=20,cex=0.2,col=alpha(groupcolvec1,0.35))
    points(x=deathx,y=eventy,pch=4,col=alpha(eventcol,.6))
    for(j in 1:ngroup) lines(get(paste0("plot_lowess_event",tgroup[j])), col = grouppalette[j], lwd=2)
    axis(1)
    axis(2,at=sapply(longlabels,longvartrans),label=round(longlabels))
    ## PANEL 2b
    par(fig=c(0.75,1,0.5,1), new=TRUE)
    longevent$current_d <- get(names(onestart)[2],longevent) + i
    longlocf <- getbaseline(baselinedate=current_d,visitdate=longvardate,id,value=longvar,before=6*30,after=0,data=longevent,returndate=TRUE)
    longlocf <- merge(startgroupevent,longlocf,ALL.y=TRUE)
    longlocf$tlongvar <- sapply(longlocf$longvar_cmp,longvartrans)
    maxcount1 <- 100
    z1 <- density(longlocf$tlongvar)
    plot(maxcount1*z1$y/max(z1$y),z1$x, axes=FALSE,type='l',xlab="",ylab="",col=alpha(grey(.3),.45),lwd=3,ylim=sapply(longvarlim,longvartrans)) 
    
    for(j in 1:ngroup){
       maxcount2 <- maxcount1*sum(longlocf$group==tgroup[j])/nrow(longlocf)
       z2 <- density(longlocf$tlongvar[longlocf$group==tgroup[j]])
       lines(maxcount2*z2$y/max(z2$y),z2$x,col=alpha(grouppalette[j],.65),lwd=3,ylim=sapply(longvarlim,longvartrans)) 
    }
    ## PANEL 1
    par(fig=c(0,1,0,0.5), new=TRUE)
    plot(1:10,1:10,ylim=problim,xlim=c(0,min(i+5,maxtime)),col=0,axes=FALSE,
        main="",xlab="Days from Start",ylab="Probability of Event")
    lines(survfit(S~1),lty=1,fun="event",mark.time=FALSE,xmax=maxtime,col=alpha(grey(.3),.45),lwd=3)
    for(j in 1:ngroup) lines(survfit(get(paste0("S.",tgroup[j]))~1),lty=1,fun="event",mark.time=FALSE,xmax=maxtime,col=grouppalette[j],lwd=2)
    legend("topleft",lty=1,lwd=c(rep(2,ngroup),3),paste0(c(rep("Group ",ngroup),"Combined"),c(tgroup,"")," (n=",c(n1,n),")"),col=c(grouppalette[1:ngroup],alpha(grey(.3),.45)),text.col = c(grouppalette[1:ngroup],grey(.3)),bty='n')
    axis(1)
    axis(2)
    dev.off()
}
panelkey <- "panel_graphic1_"

library(brew)
brew('code/scroll.html', output='output/viewer.html')
