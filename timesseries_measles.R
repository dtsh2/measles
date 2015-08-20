rm(list=ls())

library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(rworldmap)
library(adehabitat)
library(rgdal)
library(raster)
library(maps)
library(maptools)
library(sp)
#library(SDMTools)
library(ggplot2)
require(rgeos)
library(ggmap)
library(mapdata)
gpclibPermit()
library(mapproj)
library(plyr)
library(grid)
library(gridExtra)

# Data from http://thematicmapping.org/downloads/world_borders.php.
# Direct link: http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip
# Unpack and put the files in a dir 'data'

gpclibPermit()
library(utils)
library(rgdal)
getwd()
setwd("~/data")
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
world.map <- readOGR(dsn="C:/Users/David Hayman/Documents/data", layer="TM_WORLD_BORDERS_SIMPL-0.3")

setwd("C:/Users/David Hayman/Dropbox/measles/data")

incidence<-read.csv("incidence_series.csv",header=T)
head(incidence)

population<-read.csv("population.csv", header=T)
head(population)

#######

world.ggmap <- fortify(world.map, region = "NAME")
save(world.ggmap,file="world.ggmap.Rda")
load("world.ggmap.Rda")
n <- length(unique(world.ggmap$id))

id = unique(world.ggmap$id)
id<-as.data.frame(id)

colnames(incidence)[3]<-"id"
levels(incidence$id)[match(c("Bahamas (the)",
                             "Bolivia (Plurinational State of)",
                             "Central African Republic (the)",
                             "Comoros (the)",
                             "Congo (the)",
                             "Côte d'Ivoire",
                             "Czech Republic (the)",
                             "Democratic People's Republic of Korea (the)",
                             "Democratic Republic of the Congo (the)",
                             "Dominican Republic (the)",
                             "Gambia (the)",
                             "Lao People's Democratic Republic (the)",
                             "Libya",
                             "Marshall Islands (the)",
                             "Micronesia (Federated States of)",
                             "Netherlands (the)",
                             "Niger (the)",
                             "Philippines (the)",
                             'Republic of Korea (the)',
                             "Republic of Moldova (the)",
                             "Russian Federation (the)",
                             "Sudan (the)",
                             "Syrian Arab Republic (the)",
                             "United Arab Emirates (the)",
                             "United Kingdom of Great Britain and Northern Ireland (the)",
                             "United States of America (the)",
                             "Venezuela (Bolivarian Republic of)"),levels(incidence$id))] <-
  c("Bahamas",
    "Bolivia",
    "Central African Republic",
    "Comoros",
    "Congo",
    "Cote d'Ivoire",
    "Czech Republic",
    "Korea, Democratic People's Republic of",
    "Democratic Republic of the Congo",
    "Dominican Republic",
    "Gambia",
    "Lao People's Democratic Republic",
    "Libyan Arab Jamahiriya",
    "Marshall Islands",
    "Micronesia, Federated States of",
    "Netherlands",
    "Niger",
    "Philippines",
    'Korea, Republic of Korea',
    "Republic of Moldova",
    "Russia",
    "Sudan",
    "Syrian Arab Republic",
    "United Arab Emirates",
    "United Kingdom",
    "United States",
    "Venezuela")

####
colnames(population)[7]<-"id"

levels(population$id)[match(c(
  "Bolivia (Plurinational State of)",
  "C?te d'Ivoire",
  "Democratic People's Republic of Korea",
  "Libya",
  "Micronesia (Federated States of)",
  "Republic of Korea",
  "Russian Federation",
  "South Sudan",
  "United States of America",
  "Venezuela (Bolivarian Republic of)"),levels(population$id))] <-
  c("Bolivia",
    "Cote d'Ivoire",
    "Korea, Democratic People's Republic of",
    "Libyan Arab Jamahiriya",
    "Micronesia, Federated States of",
    'Korea, Republic of Korea',
    "Russia",
    "Sudan",
    "United States",
    "Venezuela")

####


names(population)
head(population)
summary(population$Indicator=="Population (in thousands) total")
newpop<-population[which(population$Indicator%in%c("Population (in thousands) total")),]
head(newpop)

testpop<-aggregate( cbind(Numeric.Value) ~ Year , data = newpop , sum )
head(testpop)

testinc<-aggregate( cbind(X2012,X2011,X2010,X2009,X2008,X2007,X2006,X2005,X2004,X2003,X2002,
X2001,X2000,X1999,X1998,X1997,X1996,X1995,X1994,X1993,X1992,X1991,X1990) ~ id , data = incidence , sum )

head(testinc)
testinct<-colSums(testinc[,2:24])
plot(testinct)
change<-cbind(testpop,rev(testinct))
change$change<-with(change, change[,3]/(change[,2]*1000))

plot.new()
ttimes<-read.csv("times.csv",header=T)
lmres<-lm(res~time,data=ttimes)
summary(lmres)
plot(ttimes$res,xaxt="n",type="l",xlab="Date",ylab="")
axis(1, at=1:71, labels=ttimes$mydate) 
abline(lmres,col="red",
       lty=2)

acf(ttimes$res,lag.max = length(ttimes$res),plot=T,main="")


write.csv(change,"change.csv",row.names=FALSE)
change<-read.csv("change.csv",header=T)

plot(change[18:25,1],change[18:25,4],type="l",xlab="Year",ylab="",ylim=c(0,max(change[,4])),
     bty="n")
ylim <- range(c(0,max(change[,4])))
xlim <- range(change[,1])
par(fig = c(.6, 1, 0.6, 1), mar=c(0,0,0,0), new=TRUE)
plot(change[,1],change[,4],type="l",xlab="Year",ylab="",
     ylim=c(0,max(change[,4])),bg="grey",add=T,bty="n")
dev.off()
## set wd to get data
nzers<-read.csv("monthtravel.csv",header=T)
head(nzers)
names(nzers)
dim(nzers)

plot(as.numeric(nzers[247,2:79]),type="l",ylim=c(min(as.numeric(nzers[247,2:79])),max(ttimes$res)),
     )
points(ttimes$res,add=T,type="l",col="red")

par(mar = c(7, 4, 4, 2) + 0.1)
plot(as.numeric(nzers[247,8:79]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(as.numeric(nzers[247,2:79])),max(ttimes$res)))
points(ttimes$res,type="l",col="red")
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(ttimes$mydate, sep = " ")
axis(side=1, at=1:71, labels=labels,las=2)
legend("topleft",c("New Zealanders","Non-New Zealanders"),
       bty="n",col=c("black","red"),lty=1)
# check same length
length(nzers[247,8:78])
length(ttimes$res)

nz<-nzers[247,8:78]
im<-ttimes$res
newd<-rbind(nz,im)
rownames(newd)<-c("nz","im")
colnames(newd)<-ttimes$mydate
sum<-colSums(newd)
newd<-rbind(sum,newd)
rownames(newd)<-c("total","nz","im")

pdf(paste("nzers.pdf"), width=7, height=7)
par(mai=c(2,1,0.5,0.5))
plot(as.numeric(newd[1,]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(newd),max(newd)))
points(as.numeric(newd[2,]),type="l",col="red")
points(as.numeric(newd[3,]),type="l",col="black",lty=2)
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(colnames(newd), sep = " ")
axis(side=1, at=seq(from=1,to=71,by=2), labels=labels[seq(from=1,to=71,by=2)],las=2)
abline(v=seq(from=6,to=71,by=12),lty=3,col="grey")
legend("topleft",bg="white",c("Total","New Zealanders","Non-New Zealanders"),
       col=c("black","red","black"),lty=c(1,1,2),box.col=rgb(0,0,0,alpha=0.5) )
mtext("Month and year",side=1,line=5)
dev.off()

plot(as.numeric(newd[1,]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(newd),max(newd)))
points(as.numeric(newd[2,]),type="l",col="red")
points(as.numeric(newd[3,]),type="l",col="black",lty=2)
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(colnames(newd), sep = " ")
axis(side=1, at=seq(from=1,to=71,by=2), labels=labels[seq(from=1,to=71,by=2)],las=2)
abline(v=seq(from=6,to=71,by=12),lty=3,col="grey")
legend("topleft",bg="white",c("Total","New Zealanders","Non-New Zealanders"),
       col=c("black","red","black"),lty=c(1,1,2),box.col=rgb(0,0,0,alpha=0.5) )
# check same length

