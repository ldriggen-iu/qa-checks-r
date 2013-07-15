#############################################################
#
#   Program: mapping.R
#   Project: IeDEA
# 
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEA standard and create maps
#
#   INPUT: "tblCENTER.csv"
#   OUTPUT: "tbl_query_yyyymmdd.csv"
#
#   Notes: As long as the working directory structure 
#          matches README.md, such that the tblCENTER,
#          R-code, and resources may be sourced, 
#          then this code should run smoothly, generating
#          a listing of data queries in /output.
#
#   Dependency: latticeExtra and cshapes (which loads addtl
#               dependencies)
#   Created: 15 May 2013
#   Revisions: 15 July 2013  -- format for GitHub
#     
#############################################################
rm(list=ls()) # clear namespace
library(latticeExtra)
library(cshapes)

## DEFINE COLOR PALETTE
# library(RColorBrewer)
# colvec <- brewer.pal(7, "Set1")
colvec <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628")

## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY
# setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")


## READ IN tblCENTER 
center <- read.csv("input/tblCENTER.csv",header=TRUE,na.strings=c(""))
names(center) <- tolower(names(center))

## UNCOMMENT LINES BELOW IF YOU NEED TO USE COUNTRY NAME TEXT TO DERIVE ISO-3-CHARACTER CODE
## library(countrycode)
## center$countrycode <- countrycode(center$country,"country.name","iso3c")
## write.csv(center,"input/tblCENTER.csv",row.names=FALSE,na=c(""))

## LATER MADE USE OF CSHAPES PACKAGE BECAUSE IT IS MOST CURRENT (2012)
cmap <- cshp(date=as.Date("2012-6-30"))

## SET OUR REGION DATA TOGETHER WITH THE SHP FILE
## THIS WOULD BE THE SECTION OF CODE TO INCLUDE COUNTRY 
## SPECIFIC INFORMATION IF AVAILABLE (e.g, HIV prevalence)
uniquecountry <- center[c("region","countrycode")]
o <- match(cmap$ISO1AL3,uniquecountry$countrycode)
uniquecountry <- uniquecountry[o,]
row.names(uniquecountry) <- uniquecountry$FEATUREID
countriesSPDF <- cmap
countriesSPDF@data <- cbind(uniquecountry,cmap@data)
colnum <- length(levels(center$region))

## SET SEED SO JITTER IS CONSISTENT 
set.seed(1)
centerpoints <- center[!is.na(center$geocode_lon),]
## I AM ADDING A .5 DEGREE JITTER BECAUSE SOME SITES ARE ON TOP OF EACH OTHER
centerpoints$geocode_latj <- jitter(centerpoints$geocode_lat,amount=0.5)
centerpoints$geocode_lonj <- jitter(centerpoints$geocode_lon,amount=0.5)
## SET THE LONG, LAT AS COORDINATES FOR PLOTTING
coordinates(centerpoints) = ~geocode_lonj+geocode_latj

## WRITE MAP FILES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
wd <- getwd(); if(!file.exists("output")){dir.create(file.path(wd,"output"))}

## MAP 1
png("output/map1.png",res=125,width=1200,height=600, bg="transparent") 
region_codes <- c("NA","CN","SA","EA","WA","CA","AP")
region_labels <- c("North America","CCASAnet","Southern Africa","East Africa","West Africa","Central Africa","Asia-Pacific") 
region_labels1 <- region_labels[match(levels(countriesSPDF$region),region_codes)]
spplot_sites = list("sp.points", centerpoints, pch=19, col="black",alpha=0.75)
p1 <- spplot(countriesSPDF,"region", col.regions=colvec[1:colnum], main="",sp.layout=list(spplot_sites),colorkey=FALSE)
p1 <- p1 + layer(panel.key(region_labels1, corner = c(.02,0.15), padding = 2,rectangles = TRUE, space="left",size=2, height=c(1,1), points = FALSE, lines = FALSE, packets = 1,cex=0.8))
print(update(p1, par.settings =  custom.theme(symbol = colvec[1:colnum], fill = colvec[1:colnum], lwd=1)))
dev.off()

## MAP 2
png("output/map2.png",res=125,width=1200,height=600, bg="transparent") 
spplot_adultped1 = list("sp.points", centerpoints[centerpoints$adultped=="ADULT" & !is.na(centerpoints$adultped),], pch=19, col=colvec[1], cex=0.7)
spplot_adultped2 = list("sp.points", centerpoints[centerpoints$adultped=="PED" & !is.na(centerpoints$adultped),], pch=19, col=colvec[2], cex=0.7)
spplot_adultped3 = list("sp.points", centerpoints[centerpoints$adultped=="BOTH" & !is.na(centerpoints$adultped),], pch=19, col=colvec[3], cex=0.7)
p2 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_adultped1,spplot_adultped2,spplot_adultped3),colorkey=FALSE)
p2 <- p2 + layer(panel.key(c("Adult only","Pediatrics only","Both"), corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p2, par.settings =  custom.theme(symbol = colvec[1:3], pch=19)))
dev.off()

## MAP 3
png("output/map3.png",res=125,width=1200,height=600, bg="transparent") 
spplot_rural1 = list("sp.points", centerpoints[centerpoints$rural==1 & !is.na(centerpoints$rural),], pch=19, col=colvec[1])
spplot_rural2 = list("sp.points", centerpoints[centerpoints$rural==2 & !is.na(centerpoints$rural),], pch=19, col=colvec[2])
spplot_rural3 = list("sp.points", centerpoints[centerpoints$rural==3 & !is.na(centerpoints$rural),], pch=19, col=colvec[3])
spplot_rural4 = list("sp.points", centerpoints[centerpoints$rural==4 & !is.na(centerpoints$rural),], pch=19, col=colvec[4])
rural_labels <- c("Urban","Mostly urban","Mostly rural","Rural")
p3 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_rural1,spplot_rural2,spplot_rural3,spplot_rural4),colorkey=FALSE)
p3 <- p3 + layer(panel.key(rural_labels, corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p3, par.settings =  custom.theme(symbol = colvec[1:4], pch=19)))
dev.off()

## MAP 4
png("output/map4.png",res=125,width=1200,height=600, bg="transparent") 
spplot_level1 = list("sp.points", centerpoints[centerpoints$level==1 & !is.na(centerpoints$level),], pch=19, col=colvec[1])
spplot_level2 = list("sp.points", centerpoints[centerpoints$level==2 & !is.na(centerpoints$level),], pch=19, col=colvec[2])
spplot_level3 = list("sp.points", centerpoints[centerpoints$level==3 & !is.na(centerpoints$level),], pch=19, col=colvec[3])
level_labels <- c("Health center","District hospital","Regional, provincial or \n university hospital")
p4 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_level1,spplot_level2,spplot_level3),colorkey=FALSE)
p4 <- p4 + layer(panel.key(level_labels, corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p4, par.settings =  custom.theme(symbol = colvec[1:3], pch=19)))
dev.off()


## COLLATE MAPS IN HTML FILE
library(brew)
brew(file='code/mapping.brew',output='output/mapping_report.html')

# system.time(source("code/mapping.R"))
#    user  system elapsed 
# 18.281   0.068  18.562 

