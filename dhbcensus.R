rm(list=ls())
# setwd("~/Massey 2014/DHayman_20140627")
# read data
library(reshape)
library(reshape2)
library(plyr)
library(pscl)
require(reshape2)  # this is the library that lets one flatten out data
require(ggplot2)   # plotting library
require(lattice)
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
#qplot(Age, Naïve.Population,data=popimmune,size=I(2))
popimmune$Immunity
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
         rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))
plot(pop,xlab="Age",ylab="Population")
points(naive,pch=16)
legend("topright",c("Population","Naïve"),pch=c(1,16),bty="n")

dhbpop<-read.csv("dhbcensus.csv",header=T)

## match cases per age

df.expanded <- dhbpop[rep(row.names(dhbpop),5), 1:21]
df<-df.expanded[with(df.expanded, order(Age)), ]
df<-df[,2:21]/5
popdhb<-colSums(df)
naivedhb<-df*(1-impop[1:90])
head(naivedhb)
#matplot(naivedhb,type="l")
colnames(naivedhb)<-c("Northland","Waitemata","Auckland","Counties Manukau",
                      "Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki",
                      "Hawke's Bay","Whanganui","Midcentral","Hutt","Capital and Coast",
                      "Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury",
                      "Southern")
for (i in 1:20){
  pdf(paste("dhb", i, ".pdf", sep = ""))
  barplot(naivedhb[,i],ylim=c(0,8000),main=colnames(naivedhb)[i],xlab="Age",ylab="Numbers",
       #type="l",
       cex=1.1,cex.lab=1.1,cex.main=1.1,cex.axis=1.1)
  legend("topright",legend=c(c("Total naive", round(sum(naivedhb[,i]))),
                             c("Total population", signif(popdhb[i],5)),
                             c("Percent naive",round(sum(naivedhb[,i])/popdhb[i],3)*100)),
                             bty="n")
  dev.off()
}  

library(maptools)

# read in data from NIR

# read in shapefile for AU's
dhb <- readShapeSpatial("nz-district-health-boards-2012.shp")
#plot(dhb,xlim=c(170,180))

head(naivedhb)
# library(plyr)
data<-round(colSums(naivedhb[,1:20]))
data<-as.data.frame(data)
dhbname<-rownames(data)
data<-cbind(data,dhbname); rownames(data)=NULL
data

desired_order <- c("Lakes","Hawke's Bay","Auckland","Canterbury","Waitemata",
"Nelson Marlborough","West Coast","Waikato","Taranaki","Hutt",
"South Canterbury","Whanganui","Capital and Coast","Tairawhiti","Midcentral",
"Wairarapa","Northland","Counties Manukau","Southern","Bay of Plenty")
# Re-order the levels
data$dhbname <- factor( as.character(data$dhbname), levels=desired_order )
# Re-order the data.frame
data <- data[order(data$dhbname),]

library(RColorBrewer)
library(classInt)

fixedBreaks <- c(0,10000,15000,20000,25000,30000,35000,40000,45000,Inf)

breaks <- classIntervals(data$data, n=9, style="fixed", fixedBreaks=fixedBreaks)
pal <- brewer.pal("YlOrRd", n=9)

cols <- findColours(breaks, pal)
plot(dhb, col=cols)

## plot to show Ministry the data issues...
legend("topleft",c("0-10000","10000-15000","15000-20000",
                   "20000-25000","25000-30000","30000-35000",
                   "35000-40000","40000-45000",">45000")
       ,fill=pal,bty="n")

