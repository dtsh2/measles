rm(list=ls())
library(adehabitat)
library(classInt)
library(ggplot2)   # plotting library
library(grid)
library(gridExtra)
library(ggmap)
library(Hmisc)
library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(maptools)
library(mapproj)
library(plyr)
library(pscl)
library(RColorBrewer)
library(rworldmap)
library(reshape)
library(reshape2)
library(rgeos)
library(rgdal)
library(raster)
library(sp)
library(utils)
library(xtable)

## get data
# set wd
# setwd("~/Massey 2014/DHayman_20140627")
# read data
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)


time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)

pdf(paste("case_age_dist.pdf"), width=7, height=5)
par(mfrow=c(1,1))
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
hist(time$AgeInYears,col="grey",xlab="Age in years",main="",breaks=90,include.lowest=TRUE,right=F,ylab="Frequency",cex.lab=1)
hist(test$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,add=T)
legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n",cex=1)
dev.off()
## plot cases / age class
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)

popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))

## match cases per age
AgeInYears<-0:100
naive<-cbind(AgeInYears,naive)
colnames(naive)<-c("AgeInYears","Naive")
naive<-merge(naive,caseyr,by="AgeInYears",all=T)
colnames(naive)<-c("AgeInYears","Naive","Cases")

dose1=as.factor(round(vac$Dose1Mths/12,0));
dose1<-summary(dose1)
dose1<-as.data.frame(dose1)
dose1$AgeInYears<-rownames(dose1)
dose2=as.factor(round(na.omit(vac$Dose2Mths/12,0)));
dose2<-summary(dose2)
dose2<-as.data.frame(dose2)
dose2$AgeInYears<-rownames(dose2)

## need to merge vaccination data with data
## to get years the cases were from
datav<-merge(vac,data,by="CaseCode",all=T)
##
datav$Dose1Mths[which(is.na(datav$Dose1Mths))] <- 9999
datav$Dose2Mths[which(is.na(datav$Dose2Mths))] <- 9999
#########################
datav$RptYear<-as.factor(datav$RptYear)
datav$SurvWeek<-as.factor(datav$SurvWeek)
datav$NZDep01<-as.factor(datav$NZDep01)
datav$NZDep06<-as.factor(datav$NZDep06)
datav$NZDep13<-as.factor(datav$NZDep13)
datav$Dose1Mths<-as.factor(datav$Dose1Mths)
datav$Dose2Mths<-as.factor(datav$Dose2Mths)

#data$AgeInYears<-as.factor(data$AgeInYears)
datav$EthnicityPrioritised<-as.factor(datav$EthnicityPrioritised)

timev<-aggregate( cbind( DiseaseName) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear+ Dose1Mths +Dose2Mths, 
data = datav , FUN=sum,na.rm=F,na.action=na.pass)

testv<-subset(timev, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
testv$Dose1Mths<-revalue(testv$Dose1Mths, c("9999"=NA));
testv$Dose2Mths<-revalue(testv$Dose2Mths, c("9999"=NA));
testv$Dose1Mths<-as.numeric(testv$Dose1Mths)
testv$Dose2Mths<-as.numeric(testv$Dose2Mths)
testv$D2vac<-ifelse(testv$Dose1Mths > -1 & testv$Dose2Mths >= testv$Dose1Mths,testv$Dose2Mths,NA)
testv$D1vac<-ifelse(testv$Dose1Mths > -1 & is.na(testv$D2vac) == T,testv$Dose1Mths,NA)
testv$Unvac<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,testv$AgeInYears,NA)

testv$VC<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,0,
ifelse(testv$Dose1Mths >= 0 & is.na(testv$Dose2Mths) == T,1,2))
AgeVac<-table(testv$VC,testv$AgeInYears)

row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)
AgeInYears<-(as.numeric(rownames(AgeVac)))
AgeVac<-cbind(AgeVac,AgeInYears)       
AgeInYears<-(as.numeric(rownames(pop)))
pop<-cbind(pop,AgeInYears)
colnames(pop)<-c("Population","AgeInYears")
AgeV<-merge(pop,AgeVac,by="AgeInYears",all=T)

## regression analyses - measles
# setwd("~/Massey 2014/DHayman_20140627")
# read data
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)

time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)
caseyr
# test$AgeInYears<-findInterval(test$AgeInYears,c(3,6,18,25))
test$AgeInYears<-findInterval(test$AgeInYears,c(2,5,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
#tage<-revalue(test$AgeInYears, c("0"="0-2", "1"="3-5","2"="6-17","3"="18-24","4"="25+"));
tage<-revalue(test$AgeInYears, c("0"="0-1", "1"="2-4","2"="5-17","3"="18-24","4"="25+"));
test$AgeInYears<-tage
#summary(test$EthnicityPrioritised)
teth<-revalue(test$EthnicityPrioritised, c("European or Other"="European", "Middle Eastern/Latin American/African"="MLA",
"Pacific Peoples"="Pacific","Response cannot be classified"="None","Unknown"="None"));

test$EthnicityPrioritised<-teth
head(test)
test <- within(test, NZDep06[NZDep06 == "0"]<-NA)
test <- within(test, NZDep13[NZDep13 == "0"]<-NA)

# want to set NZ Deprivation to the appropriate year

testt <- within(test, NZDep<- ifelse (test$RptYear == "2007",test$NZDep06,
ifelse (test$RptYear == "2008",test$NZDep06,
ifelse (test$RptYear == "2009",test$NZDep06,
ifelse (test$RptYear == "2010",test$NZDep06,
ifelse (test$RptYear == "2011",test$NZDep06,
ifelse (test$RptYear == "2012",test$NZDep06,
ifelse (test$RptYear == "2013",test$NZDep13,NZDep13))))))))
head(testt)
tail(testt)
str(testt)
testt$NZDep<-as.factor(testt$NZDep-1)

testtable<-aggregate( cbind( DiseaseName) #+ as.numeric(SurvWeek))
~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
head(testtable)

#tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
#"5"="1-5","6"="6-10","7"="6-10","8"="6-10",
#"9"="6-10","10"="6-10"));

tnzd<-revalue(testtable$NZDep, c("1"="1-3","2"="1-3","3"="1-3","4"="4-7",
                                 "5"="4-7","6"="4-7","7"="4-7","8"="8-10",
                                 "9"="8-10","10"="8-10"));


testtable$NZDep<-tnzd
# head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
data = testtable , FUN=sum)

ttyr<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity, 
data = testtable , FUN=sum)
ttyr <- ttyr[ttyr$Ethnicity=="None",]

latex(ttyr, file="",table.env=FALSE,rowname=NULL)

### denominator data
# setwd("~/Massey_2014/measles/data")
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)

time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)
caseyr
## for MoH desired ages
test$AgeInYears<-findInterval(test$AgeInYears,c(2,5,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="0-1", "1"="2-4","2"="5-17","3"="18-24","4"="25+"));
test$AgeInYears<-tage
#summary(test$EthnicityPrioritised)
teth<-revalue(test$EthnicityPrioritised, c("European or Other"="European", "Middle Eastern/Latin American/African"="MLA",
"Pacific Peoples"="Pacific","Response cannot be classified"="None","Unknown"="None"));

test$EthnicityPrioritised<-teth
head(test)
test <- within(test, NZDep06[NZDep06 == "0"]<-NA)
test <- within(test, NZDep13[NZDep13 == "0"]<-NA)

# want to set NZ Deprivation to the appropriate year

testt <- within(test, NZDep<- ifelse (test$RptYear == "2007",test$NZDep06,
ifelse (test$RptYear == "2008",test$NZDep06,
ifelse (test$RptYear == "2009",test$NZDep06,
ifelse (test$RptYear == "2010",test$NZDep06,
ifelse (test$RptYear == "2011",test$NZDep06,
ifelse (test$RptYear == "2012",test$NZDep06,
ifelse (test$RptYear == "2013",test$NZDep13,NZDep13))))))))
head(testt)
tail(testt)
str(testt)
testt$NZDep<-as.factor(testt$NZDep-1)

testtable<-aggregate( cbind( DiseaseName) #+ as.numeric(SurvWeek))
~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
head(testtable)

#tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
#"5"="1-5","6"="6-10","7"="6-10","8"="6-10",
#"9"="6-10","10"="6-10"));

tnzd<-revalue(testtable$NZDep, c("1"="1-3","2"="1-3","3"="1-3","4"="4-7",
                                 "5"="4-7","6"="4-7","7"="4-7","8"="8-10",
                                 "9"="8-10","10"="8-10"));

testtable$NZDep<-tnzd
# head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
data = testtable , FUN=sum)

ttyr<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity, 
data = testtable , FUN=sum)
denom<-read.csv("NZDep2006Denominators.csv",header=T)
head(denom)
denom<-denom[,-c(9)]
summary(denom)
dim(denom)
denom<-denom[,1:18]
row.has.na <- apply(denom, 1, function(x){any(is.na(x))})
sum(row.has.na)
denomt <- denom[!row.has.na,]
head(denomt)
denomt<-denomt[denomt$Sex_code==99,]
denomt$Eth_Level<-as.factor(denomt$Eth_Level)
denomt<-denomt[denomt$Eth_Level %in% c("24","21","66",'22','77'),]
dm<-cbind(denomt[,c(3,6,9:18)])
head(dm)
dm<-dm[dm$Age_Label %in% c("A: 0- 5 yrs", "A: 6-17 yrs","A:18-24 yrs","A:25-64 yrs","A:65+ yrs"),]
dage<-revalue(dm$Age_Label, c("A: 0- 5 yrs"="<6", "A: 6-17 yrs"="6-17","A:18-24 yrs"="18-24","A:25-64 yrs"="25+","A:65+ yrs"="25+"));
dm$Age_Label<-dage
colnames(dm)<-c('Age','Ethnicity',"Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
head(dm)
datadm<-dm[dm$Age=="<6",]
dataA<-(rbind(datadm,datadm)/2)
dataA<-round(dataA[,3:12])
dataA<-cbind(c(rep("0-2",5),rep("3-5",5)),rep(datadm$Ethnicity[1:5],2),dataA)
colnames(dataA)<-c('Age','Ethnicity',"Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
ddm<-dm[!dm$Age=="<6",]
dm<-rbind(dataA,ddm)
dm$Ethnicity <- factor(dm$Ethnicity)
deth<-revalue(dm$Ethnicity, c("Asian (Prioritised)"="Asian","European (NZ European and Other European)"="European",
"Maori (Prioritised)"="Maori","MELAA"="MLA","Pacific People (Prioritised)"="Pacific"));
dm$Ethnicity<-deth
head(dm)

## here's the additional fudge the MoH asked for.
## add 5 year olds to next class
dm[11:15,3:12]<-dm[11:15,3:12]+round(dm[6:10,3:12]/3,0)
dm[6:10,3:12]<-round(dm[6:10,3:12]*2/3,0)
## add 2 year olds to next class
dm[6:10,3:12]<-dm[6:10,3:12]+round(dm[1:5,3:12]/3,0)
dm[1:5,3:12]<-round(dm[1:5,3:12]*2/3,0)
## 
# rename
dm$Age<-revalue(dm$Age, c("0-2"="0-1", "3-5"="2-4","6-17"="5-17"));

popn<-melt(dm,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
colnames(popn)<-c("Age","Ethnicity","NZDep","Popn")
popn
#pnzd<-revalue(popn$NZDep, c("Dep1"="1-5","Dep2"="1-5","Dep3"="1-5","Dep4"="1-5",
#"Dep5"="1-5","Dep6"="6-10","Dep7"="6-10","Dep8"="6-10",
#"Dep9"="6-10","Dep10"="6-10"));

pnzd<-revalue(popn$NZDep, c("Dep1"="1-3","Dep2"="1-3","Dep3"="1-3","Dep4"="4-7",
                            "Dep5"="4-7","Dep6"="4-7","Dep7"="4-7","Dep8"="8-10",
                            "Dep9"="8-10","Dep10"="8-10"));


popn$NZDep<-pnzd
head(popn)
tp<-aggregate( cbind(Popn) ~ NZDep+Age+Ethnicity, 
data = popn , FUN=sum)

tp$merge <- paste(tp$NZDep, tp$Age, tp$Ethnicity)
ttyr$merge <- paste(ttyr$NZDep, ttyr$Age, ttyr$Ethnicity)
ttyr <- ttyr[ttyr$Ethnicity!="None",]

tp$cases <- 0
cases <- matrix(0, length(tp$merge),1)
rownames(cases) <- tp$merge
cases[ttyr$merge,] <- ttyr$Cases
tp$cases <- cases
PerCapita<-(tp$cases/tp$Popn)
PerCap<-cbind(tp[,c(1:4,6)],round(PerCapita*10000,4))
colnames(PerCap)<-c("NZDep","Age","Ethnicity","Population","Cases","Per capita")

tp$perCap<-with(tp,tp$cases/tp$Popn*10000)


pdf(paste("case_age_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, cases, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
 # theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("case_eth_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
#  theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("case_age_eth.pdf"), width=7, height=5)
ggplot(tp, aes(Age, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
#  theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("percap_age_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
 # theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_age.pdf"), width=7, height=5)
ggplot(tp, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

tpsub<-tp[!(tp$Ethnicity=="MLA"),]
tpsub$ordert <- factor(tpsub$merge, as.character(tpsub$merge))

tpsub1<-tpsub[ order(tpsub$Age), ]
tpsub1$ordert <- factor(tpsub1$merge, as.character(tpsub1$merge))

pdf(paste("percap_nzdep_age.pdf"), width=7, height=5)
ggplot(tpsub, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_nzdep.pdf"), width=7, height=5)
ggplot(tpsub, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_age.pdf"), width=7, height=5)
ggplot(tpsub, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()
##
pdf(paste("case_merge.pdf"), width=10, height=5)
ggplot(tp, aes(x=merge, y=cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  # theme(text=element_text(size=45))+
  ylab("cases")+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 0))
dev.off()
#
pdf(paste("case_ordert.pdf"), width=10, height=5)
ggplot(tpsub, aes(x=ordert, y=cases, fill=Ethnicity)) +
  geom_bar(stat="identity", position="dodge")+
  # theme(text=element_text(size=45))+
  ylab("cases")+xlab("")+theme(axis.text.x = element_text(angle = 90, vjust=0,hjust = 0))
dev.off()

pdf(paste("case_ordert2.pdf"), width=10, height=5)
ggplot(tpsub1, aes(x=ordert,y= cases, fill=Ethnicity)) +
  geom_bar(stat="identity", position="dodge")+
  # theme(text=element_text(size=45))+
  ylab("cases")+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 0))
dev.off()
##
pdf(paste("percap_merge.pdf"), width=10, height=5)
ggplot(tp, aes(x=merge, y=perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  # theme(text=element_text(size=45))+
  ylab("Per capita per 10000")+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 0))
dev.off()
#
pdf(paste("percap_ordert.pdf"), width=10, height=5)
ggplot(tpsub, aes(x=ordert, y=perCap, fill=Ethnicity)) +
  geom_bar(stat="identity", position="dodge")+
  # theme(text=element_text(size=45))+
  ylab("Per capita per 10000")+xlab("")+theme(axis.text.x = element_text(angle = 90, vjust=0,hjust = 0))
dev.off()


# This chunk uses "results=tex"

latex(PerCap, file="", table.env=FALSE,rowname=NULL)
write.csv(PerCap,"percap.csv")
## plot this figure?
#hist(tp$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
tp$Ethnicity<- relevel(tp$Ethnicity, "European")
model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tp,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F") # F test, not Chisq, because dispersion estimated by moments

model2<-update(model1,~.-Age:NZDep)
summary(model2)
anova(model2,test="F")

## use tpsub
#hist(tpsub$cases,xlab="Cases",main='Histogram of cases per category',col="darkgrey",breaks=20)

model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tpsub,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F")

#model2<-update(model1,~.-Ethnicity:NZDep)
#summary(model2)
#anova(model2,test="F")
model2<-update(model1,~.-Age:NZDep)
summary(model2)
anova(model2,test="F")

model3<-update(model2,~.-Ethnicity:NZDep)
summary(model3)
anova(model3,test="F")
anovap<-anova(model3,test="F")
write.csv(anovap,"anovap.csv")

pdf(paste("Cases_regmodel.pdf"), width=7, height=5)
par(cex.axis=1)
par(mar=c(5,6,4,2)+0.1)
hist(tpsub$cases,xlab="Number of cases",main='',col='grey',breaks=20,ylab="Number of categories",cex.lab=1)
dev.off()
res<-predict(model3)
#res<-predict(model2)

pdf(paste("Cases_regmodel_prediction.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
plot(exp(res),tpsub$cases,xlab="cases",ylab='predictions',main="",pch=16,col="darkgrey",cex.lab=1,cex=1)
dev.off()

cor(exp(res),tpsub$cases)
cor.test(exp(res),tpsub$cases)

pdf(paste("Cases_regmodel_resid.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
#hist(model3$residuals,main="",xlab="residuals",col="grey",cex.lab=1,ylab="")
hist(model2$residuals,main="",xlab="residuals",col="grey",cex.lab=1,ylab="frequency")
dev.off()

resan<-xtable(anova(model3,test="F"))
#resan<-xtable(anova(model2,test="F"))
print(resan,floating = FALSE)

#xtb<-xtable(summary(model3))
xtb<-xtable(summary(model2))
print(xtb,floating = FALSE)

AgeVac<-table(testv$VC,testv$AgeInYears)
row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)

pdf(paste("vacc_status.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
barplot(t(AgeVac),xlab="Age",col=c("darkgrey","red","orange"),main="",ylab="Frequency",cex.lab=1)
legend("topright",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"),cex=1,bty="n")
dev.off()

pdf(paste("vacc_age.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
hist(vac$Dose1Mths,breaks=100,col=rgb(0,0,1,1/4),xlab="Age in months",main="",cex.lab=1,ylab="Frequency")
abline(v=15,col="blue")
hist(vac$Dose2Mths,breaks=50,add=T,col=rgb(1,0,0,1/4))
abline(v=48,col="red")
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),pch=15,bty="n",cex=1)
legend("top",c("15 months","48 months"),col=c("blue","red"),lty=1,bty="n",cex=1)
dev.off()

pdf(paste("vacc_age_close.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
hist(vac$Dose1Mths,breaks=100,col=rgb(0,0,1,1/4),xlab="Age in months",main="",cex.lab=1,ylab="Frequency",xlim=c(0,100))
abline(v=15,col="blue")
hist(vac$Dose2Mths,breaks=50,add=T,col=rgb(1,0,0,1/4),xlim=c(0,100))
abline(v=48,col="red")
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),pch=15,bty="n",cex=1)
legend("right",c("15 months","48 months"),col=c("blue","red"),lty=1,bty="n",cex=1)
dev.off()


DOB<-as.numeric(levels(testv$RptYear))[testv$RptYear]-testv$AgeInYears
#hist(DOB,breaks=100,col="grey",main="2007- 2014 measles cases",xlab="Date of Birth")

DOBVac<-table(testv$VC,DOB)
row.names(DOBVac)<-c("Unvaccinated","Dose1","Dose2")
DOBVac<-t(DOBVac)

pdf(paste("dob_vacc_status.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
barplot(t(DOBVac),xlab="Age",col=c("darkgrey","red","orange"),main="",ylab="Frequency",cex.lab=1)
legend("topleft",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"),cex=1,bty="n")
dev.off()

pdf(paste("dob_vacc_dose.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
par(mfrow=c(2,1))
barplot(DOBVac[,2],col=rgb(0,0,1,1/4),xlab="Year of Birth",main="",cex.lab=1,ylab="Frequency")
legend("topleft",c("Dose 1"),col=rgb(0,0,1,1/4),pch=15,bty="n",cex=1)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
barplot(DOBVac[,3],col=rgb(1,0,0,1/4),xlab="Year of Birth",main="",cex.lab=1,ylab="Frequency")
legend("topleft",c("Dose 2"),col=rgb(1,0,0,1/4),pch=15,bty="n",cex=1)
dev.off()

pdf(paste("dob_vacc_unvac.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
par(mfrow=c(1,1))
barplot(DOBVac[,1],col="grey",xlab="Year of Birth",main="",cex.lab=1,ylab="Frequency")
legend("topleft",c("Unvaccinated"),col="grey",pch=15,bty="n",cex=1)
dev.off()

popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))

pdf(paste("naive_allPop.pdf"), width=7, height=5)
par(cex.axis=1)
par(mar=c(5,6,4,2)+0.1)
plot(pop,xlab="Age",ylab="Population",cex.lab=1)
points(naive,pch=16)
legend("topright",c("Population","Naive"),pch=c(1,16),bty="n",cex=1)
dev.off()

pdf(paste("naive_allPop_yob.pdf"), width=7, height=5)
par(mfrow=c(2,1))
par(cex.axis=1)
par(mar=c(4,5,1,2)+0.1)
plot(rev(pop),xlab="Year of birth",ylab="Population",cex.lab=1,xaxt="n")
points(rev(naive),pch=16)
legend("topleft",c("Population","Naive"),pch=c(1,16),bty="n",cex=1)
plotting_age <- seq(0,100,by=10)
axis(side=1, at=plotting_age, labels=2013-rev(plotting_age))
plot(x=100-rev(caseyr[,1]),y=rev(caseyr[,2]),pch=16,col=2,cex.lab=1,xaxt="n",xlim=c(0,100),
     ylab="Cases",xlab="Year of birth")
legend("topleft",c("Cases"),pch=c(16),col='red',bty="n",cex=1)
plotting_age <- seq(0,100,by=10)
axis(side=1, at=plotting_age, labels=2013-rev(plotting_age))
dev.off()


popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))
## match cases per age
AgeInYears<-0:100
naive<-cbind(AgeInYears,naive)
colnames(naive)<-c("AgeInYears","Naive")
naive<-merge(naive,caseyr,by="AgeInYears",all=T)
#naive[is.na(naive)] <- 0
colnames(naive)<-c("AgeInYears","Naive","Cases")
##
###
pdf(paste("numvac.pdf"), width=7, height=5)
par(mfrow=c(1,1))
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
plot(naive$Naive ,col="black",bg="black",pch=1,ylab="Number to vaccinate",xlab="Age in Years",
     main="",cex.lab=1,cex=1)
points(round(0.28*naive$Naive),col="grey",bg="red",pch=21,cex=1)
points(x=3:18,y=naive$Naive[3:18],col="grey",bg="orange",pch=23,cex=1) # 
legend("topright",c("Naive","28% naive, all ages","28% naive, all 2-17 yr olds"),
       bty="n",pch=c(1,21,23),col=c("black","grey","grey"),pt.bg=c("black","red","orange"),cex=1)
dev.off()

# proportions of the naive population the wuld be covered if vaccinated by age groups now
propvCohort<-c(
round((sum(naive$Naive[3:6])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:12])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:17])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:19])/sum(naive$Naive)),2),
round((sum(naive$Naive[12:17])/sum(naive$Naive)),2),
round((sum(naive$Naive[12:19])/sum(naive$Naive)),2),
round((sum(naive$Naive[1:30])/sum(naive$Naive)),2),
round((sum(naive$Naive[1:45])/sum(naive$Naive)),2),
round((sum(naive$Naive[8:100])/sum(naive$Naive)),2))
names(propvCohort)<-c("2-5","5-11","5-16","5-18","11-16","11-18","0-29","0-44",">8")
write.table(propvCohort,"vaccine_proportions_Cohort.txt",sep=",",row.names=T,col.names=F)

propsv = NULL
for (i in 1:20 ) {
  propsv[i] = round((sum(naive$Naive[i:13])/sum(naive$Naive)),2)
}
propsvAge = NULL
for (i in 1:20 ) {
  propsvAge[i]=paste(c(min(i:20)-1, max(i:20)-1),collapse="-")
}

table120<-as.data.frame(propsv,propsvAge)
table120<-cbind(Age=rownames(table120),table120)
rownames(table120)<-1:20
colnames(table120)<-c("Age","Proportion")

propsvrev = NULL
for (i in 20:1 ) {
  propsvrev[i] = round((sum(naive$Naive[i:1])/sum(naive$Naive)),2)
}
propsvrevAge = NULL
for (i in 20:1 ) {
  propsvrevAge[i]=paste(c(min(i:1)-1, max(i:1)-1),collapse="-")
}

table201<-as.data.frame(propsvrev,propsvrevAge)
table201<-cbind(Age=rownames(table201),table201)
rownames(table201)<-1:20
colnames(table201)<-c("Age","Proportion")

propsv11 = NULL
for (i in 1:11 ) {
  propsv11[i] = round((sum(naive$Naive[i:11])/sum(naive$Naive)),2)
}
propsv11Age = NULL
for (i in 1:11 ) {
  propsv11Age[i]=paste(c(min(i:11)-1, max(i:11)-1),collapse="-")
}

table11<-as.data.frame(propsv11,propsv11Age)
table11<-cbind(Age=rownames(table11),table11)
rownames(table11)<-1:11
colnames(table11)<-c("Age","Proportion")

# write.table(table11,"vaccine_proportions_11yrOlds.txt",sep=",",row.names=F)
# write.table(table120,"vaccine_proportions_1_20yrOlds.txt",sep=",",row.names=F)
# write.table(table201,"vaccine_proportions_20_1yrOlds.txt",sep=",",row.names=F)

###
# read in data from NIR
data_d <- read.table("NIR_MMR_data_20060101-20140731/NIR_MMR_data_20060101-20140731.txt", header=T, sep="|")

# read in domicile <-> AU map
data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

# merge NIR with domicile/AU so we have AU for each NIR
data_merge <- merge(data_d, data_dom, by.x = "DOMICILE_CODE", by.y="dom", all=T)

# TODO: figure out how many kids are immunised in each AU
# find number of kids by BIRTH_CALENDAR_YEAR, AU, VACCINE_DOSE

num_kids <- table(data_merge$BIRTH_CALENDAR_YEAR, data_merge$area.unit, data_merge$VACCINE_DOSE)
# drop the 0's from vaccine_dose
dose1 <- t(num_kids[,,2])
dose2 <- t(num_kids[,,3])
dose1 <- as.data.frame(dose1[,-ncol(dose1)])
dose2 <- as.data.frame(dose2[,-ncol(dose2)])
dose1$cau <- rownames(dose1)
dose2$cau <- rownames(dose2)


# read in shapefile for AU's
au <- readShapeSpatial("AU2013_GV_Full")

au_data <- slot(au, "data")

# read in denominator data for AU's
data_denom<-read.csv("2013-mb-dataset-Total-New-Zealand-individual-part-1_AU.csv",header=T)
denom<-data_denom[,1]
for (i in 1:5)
  denom <- cbind(denom, data_denom[,3]/5)
for (i in 1:5)
  denom <- cbind(denom, data_denom[,4]/5)
colnames(denom)[2:11] <- 2013:(2013-9)
denom <- denom[,c(1,9:2)]

dose2_merge <- merge(dose2, denom, by.x="cau", by.y="denom", all=T)
diff <- dose2_merge[,2:8+7] - dose2_merge[,2:8]
#image(as.matrix(diff > 0))

dose2_num <- data.frame(cau=dose2_merge$cau, vacc = rowSums(dose2_merge[,2:5]), total = rowSums(dose2_merge[,10:13]))
diff <- dose2_num$total - dose2_num$vacc

dose1_merge <- merge(dose1, denom, by.x="cau", by.y="denom", all=T)
diff <- dose1_merge[,2:8] - dose1_merge[,2:8+7]
#image(as.matrix(diff > 0))

au_new <- merge(au_data, dose2_num, by.x="AU2013", by.y="cau", all.x=T)
au_new$prop <- au_new$vacc / au_new$total

fixedBreaks <- c(0,0.5,0.7, 0.9, 1.1, 1.3, 1.5, Inf)
au_new$prop[!is.finite(au_new$prop)] <- NA
breaks <- classIntervals(au_new$prop, n=7, style="fixed", fixedBreaks=fixedBreaks)
pal <- brewer.pal("Spectral", n=7)

cols <- findColours(breaks, pal)
# plot(au, col=cols)

## plot to show Ministry the data issues...
fixedBreaks <- c(0,0.6,0.8,1, 1.2,1.4,Inf)
au_new$prop[!is.finite(au_new$prop)] <- NA
breaks <- classIntervals(au_new$prop, n=6, style="fixed", fixedBreaks=fixedBreaks)
pal <- brewer.pal("RdGy", n=6)
pal<-c("red","orange","grey","blue","green","yellow")
cols <- findColours(breaks, pal)

###
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
         rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))

totdpdf<-read.csv("mapdata_tot.csv",header=T)
# output top countries
colnames(totdpdf)[1]<-c("country")
toptot <- totdpdf[order(-totdpdf$immigration),] 
immigration<-toptot[1:30,c(1,4)]
topincidence <- totdpdf[order(-totdpdf$incidence),] 
incidence<-topincidence[1:30,c(1,2)]
incidence[,2]<-round(incidence[,2],0)
topvaccine <- totdpdf[order(totdpdf$cover),] 
vaccinecover<-topvaccine[1:30,c(1,3)]
toprisk <- totdpdf[order(-totdpdf$risk),] 
risk<-(toprisk[1:30,c(1,5)])
risk[,2]<-round(risk[,2],0)
toprisk2<-(toprisk[1:10,c(1,2,3,4)])
toprisk2[,2]<-round(toprisk2[,2],0)

nzdpdf<-read.csv("mapdata_nzers.csv",header=T)
# output top countries
colnames(nzdpdf)[1]<-c("country")
topnz <- nzdpdf[order(-nzdpdf$immigration),] 
nz<-topnz[1:30,c(1,4)]
topnzrisk <- nzdpdf[order(-nzdpdf$risk),] 
nzrisk<-(topnzrisk[1:30,c(1,5)])
nzrisk[,2]<-round(nzrisk[,2],0)

imdpdf<-read.csv("mapdata_immigration.csv",header=T)
# output top countries
colnames(imdpdf)[1]<-c("country")
topim <- imdpdf[order(-imdpdf$immigration),] 
im<-topim[1:30,c(1,4)]
topimrisk <- imdpdf[order(-imdpdf$risk),] 
imrisk<-(topimrisk[1:30,c(1,5)])
imrisk[,2]<-round(imrisk[,2],0)
# haven't had time to sort out which packages I need

#getwd()
#setwd("~/data")
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
load("world.ggmap.Rda")
n <- length(unique(world.ggmap$id))
id = unique(world.ggmap$id)
id<-as.data.frame(id)
dpdf<-read.csv("mapdata_tot.csv",header=T)
# output top countries

np1<-ggplot(dpdf, aes(map_id = id))  +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np2<-ggplot(dpdf, aes(map_id = id))  +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np3<-ggplot(dpdf, aes(map_id = id))  +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

#grid.arrange(np1, np2, np3, ncol=3, main="")

np4<-ggplot(dpdf, aes(map_id = id)) +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#np4
# output top countries
dpdf<-read.csv("mapdata_immigration.csv",header=T)
imnp1<-ggplot(dpdf, aes(map_id = id))  +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

imnp4<-ggplot(dpdf, aes(map_id = id)) +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#np4
dpdf<-read.csv("mapdata_nzers.csv",header=T)
nznp1<-ggplot(dpdf, aes(map_id = id))  +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

nznp4<-ggplot(dpdf, aes(map_id = id)) +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  theme(text = element_text(size=20))+
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#np4

change<-read.csv("change.csv",header=T)
costtable1<-read.csv("costtable1.csv",header=T)
costtable2<-read.csv("costtable2v2.csv",header=T)
costtable3<-read.csv("costtable3.csv",header=T)

hosp<-read.csv("hospital.csv",header=T)

p<-ggplot(data=hosp, aes(x=Days, y=Number)) + geom_bar(stat="identity")+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=16),axis.text.x  = element_text(size=14),axis.text.y  = element_text(size=14))

###
# pdf(paste("ageinyears.pdf"), width=7, height=5)
# par(mfrow=c(1,1))
# par(mar=c(5,6,4,2)+0.1)
# par(cex.axis=1)
# hist(time$AgeInYears,col="grey",xlab="Age in years",main="",breaks=90,include.lowest=TRUE,right=F,ylab="Frequency",cex.lab=1)
# hist(time_yr$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,add=T)
# legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n",cex=1)
# dev.off()

###
pdf(paste("naive_au.pdf"), width=7, height=5)
par(mfrow=c(1,1))
plot(au, col=cols)
legend("topright",c("0-60%","61-80%","81-100%","101-120%","121-140%",">141%","NA")
       ,fill=c("red","orange","grey","blue","green","yellow","white"),
       bty="n",inset=0.1,title="Percent vaccinated")

par(fig = c(0.05, 0.4, 0.5,0.9), mar=c(5,5,2,2), new=TRUE)
hist(au_new$prop[au_new$prop < 2], breaks=50,col="grey",
     xlab="Proportion vaccinated",main="")
abline(v=1,col="red",lwd=3)
dev.off()
###
pdf(paste("naive_au_prop.pdf"), width=7, height=5)
par(mfrow=c(1,1))
par(cex.axis=1)
hist(au_new$prop, breaks=100,col="grey",xlab="Proportion vaccination",main="")
abline(v=1,col="red")
dev.off()
###
pdf(paste("incidence12.pdf"), width=10, height=5)
print(np2)
dev.off()
###
pdf(paste("cover12.pdf"), width=10, height=5)
print(np3)
dev.off()
###
pdf(paste("immigration12.pdf"), width=10, height=5)
print(imnp1)
dev.off()
###
pdf(paste("nztravel12.pdf"), width=10, height=5)
print(nznp1)
dev.off()
###
pdf(paste("travel12.pdf"), width=10, height=5)
print(np1)
dev.off()
###
pdf(paste("risk12.pdf"), width=10, height=5)
print(np4)
dev.off()
###
pdf(paste("nzrisk12.pdf"), width=10, height=5)
print(nznp4)
dev.off()
###
pdf(paste("imrisk12.pdf"), width=10, height=5)
print(imnp4)
dev.off()
###

pdf(paste("p.pdf"), width=7, height=5)
print(p)
dev.off()

## simulate measles outbreaks/epidemics using GT and R0 from data.
# http://www.sciencedirect.com/science/article/pii/S0022519311003146
mu    <- 12
sigma <- 3.5
sigma_logn <- sqrt(log(1 + (sigma/mu)^2))
mu_logn    <- log(mu) - log(1 + (sigma/mu)^2) / 2

# then exp(rnorm(n, mu_logn, sigma_logn)) simulates from lognormal with the given mean and sd.

# the R0 library 
require(R0)
#source("estR0.R")

# generation time 
genTime <- generation.time(type="lognormal", val=c(12, 3.5))

jm.epid <- function (epid.nb, GT, R0, epid.length, family, negbin.size = NULL, 
                     peak.value = 300000, popn = 300000) 
{
  if (class(GT) != "R0.GT") {
    stop("GT object must be of class R0.GT.")
  }
  if (family == "negbin" & is.null(negbin.size)) {
    negbin.size <- R0/4
  }
  GT <- GT$GT
  epidemics <- matrix(data = 0, nrow = epid.length, ncol = epid.nb)
  for (n in 1:epid.nb) {
    sim.epid = c(1, rep(0, epid.length - 1))
    for (t in 1:epid.length) {
      if (family == "poisson") {
        susc <- max(0,popn - sum(sim.epid, na.rm=T))
        #    peak.value[t]<-susc
        new <- rbinom(sim.epid[t], susc, min(1, R0/susc))
        if (sum(new) > susc) # arggrggg
        {
          #      cat("doing the new thing", susc, sim.epid[t], sum(sim.epid[1:t]),"\n")
          new <- rep(0, sim.epid[t])
          for (i in 1:sim.epid[t]) {
            #         cat("taking a random number, susc=", susc, "\n")
            new[i] <- rbinom(1, susc, min(1, R0/susc))
            susc <- susc - new[i]
            if (susc == 0)
            {
              #             cat("got 'em all", sum(new), "\n")
              break;
            }
          }
        }
        #        cat("generating", new[1], "individuals from first, susc=", susc, "\n")
      }
      else if (family == "negbin") {
        new <- rnbinom(sim.epid[t], size = negbin.size, 
                       mu = R0)
      }
      if (is.na(sum(new)))
        break
      
      newd <- rmultinom(1, sum(new), GT)[, 1]
      sim.epid[t:(t + length(GT) - 1)] <- sim.epid[t:(t + 
                                                        length(GT) - 1)] + newd
      if (sim.epid[t + 1] > peak.value & t < (epid.length - 
                                                1)) {
        sim.epid[(t + 2):epid.length] <- 0
        break
      }
    }
    sim.epid <- sim.epid[!is.na(sim.epid)]
    epidemics[, n] <- sim.epid
  }
  return(epidemics)
}


## if R 0 == 2
set.seed(1) # all infected
res<-jm.epid(epid.nb=1000,GT=genTime,R0=0.99,epid.length=365*5,popn=331385
             ,family="poisson",peak.value=331385)
sizes<-colSums(res)
#sizes
legMd<-as.factor(paste("median =",c(median(sizes))))
legMn<-as.factor(paste("mean =",c(round(mean(sizes)))))

###
pdf(paste("sim.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
hist(sizes,xlim=c(0,200),col="grey",breaks=1000,xlab="Outbreak size",main="",cex.lab=1)
#summary(sizes)
abline(v=median(sizes),col="orange",lty=3,lwd=8)
abline(v=mean(sizes),col="red",lty=3,lwd=8)
#length(which(sizes>5))/1000
legend('topright',c(levels(legMd),levels(legMn)),lty=rep(3,2),col=c("orange","red"),bty="n",lwd=8,cex=1)
dev.off()

###
pdf(paste("sim1.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
hist(sizes,breaks=1000,xlab="Outbreak size",main="",cex.lab=1,ylim=c(0,20))
abline(v=median(sizes),col="orange",lty=3,lwd=8)
abline(v=mean(sizes),col="red",lty=3,lwd=8)
legend('topright',c(levels(legMd),levels(legMn)),lty=rep(3,2),col=c("orange","red"),bty="n",lwd=8,cex=1)
dev.off()
