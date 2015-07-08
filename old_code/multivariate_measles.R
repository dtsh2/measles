rm(list=ls())
## regression analyses - measles
## get data
# set wd
 setwd("~/Massey 2014/DHayman_20140627")
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
library(reshape)
library(reshape2)
library(plyr)
library(pscl)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)

## plot cases / age class

par(mfrow=c(1,1))
hist(time$AgeInYears,col="grey",xlab="Age in years",main="Age of cases",breaks=90,include.lowest=TRUE,right=F)
hist(test$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,add=T)
legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n")

caseyr<-aggregate( DiseaseName ~ AgeInYears, 
                   data = test , FUN=sum)

caseyr
head(vac)
hist(vac$Dose1Mths,breaks=100,col=rgb(0,0,1,1/4),xlab="Age in months",main="Age of vaccination of vaccinated cases")
abline(v=12,col="blue")
hist(vac$Dose2Mths,breaks=50,add=T,col=rgb(1,0,0,1/4))
abline(v=60,col="red")
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),pch=15,bty="n")
legend(x=400,y=200,c("12 months","60 months"),col=c("blue","red"),lty=1,bty="n")

hist(time$AgeInYears,col=rgb(1,1,1,1/4),xlab="Age in Years",main="Proportions of cases vaccinated",breaks=90,include.lowest=TRUE,right=F)
#hist(test$AgeInYears,col=rgb(0,1,1,1/4),breaks=90,include.lowest=TRUE,right=F,add=T)
hist(vac$Dose1Mths/12,breaks=50,col=rgb(0,1,1,1/4),add=T)
hist(vac$Dose2Mths/12,breaks=10,add=T,col=rgb(0,0,1,1/4))
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,1,1,1/4),rgb(0,0,1,1/4)),pch=15,bty="n")

require(reshape2)  # this is the library that lets one flatten out data
require(ggplot2)   # plotting library
# bucket<-list(dose1=vac$Dose1Mths/12,dose2=vac$Dose2Mths/12) # this puts all values in one list
# the melt command flattens the 'bucket' list into value/vectorname pairs
# the 2 columns are called 'value' and 'L1' by default
# 'fill' will color bars differently depending on L1 group
# ggplot(melt(bucket), aes(value, fill = L1)) + 
#  #call geom_histogram with position="dodge" to offset the bars and manual binwidth of 2
#  geom_histogram(position = "stack", binwidth=1) ## not really what I want
  
##
setwd("~/Massey_2014/measles/data")
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
#popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
#plot(y=popimmune$Naïve.Population, x=popimmune$Age,data=popimmune,ylim=c(0,max(popimmune$Naïve.Population)))
require(ggplot2)
qplot(Age, Naïve.Population,data=popimmune,size=I(2))
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
## match cases per age
AgeInYears<-0:100
naive<-cbind(AgeInYears,naive)
colnames(naive)<-c("AgeInYears","Naive")
naive<-merge(naive,caseyr,by="AgeInYears",all=T)
#naive[is.na(naive)] <- 0
colnames(naive)<-c("AgeInYears","Naive","Cases")
#points(naive$Cases,pch=16,col="red")

par(mar=c(5,4,4,5)+.1)
hist(test$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,ylim=c(0,500),
     main="Cases and Naïve Population",xlab="Age")
#points(naive[1:64,2],pch=16,col="blue")
# remove 0 and 1 year old old
cor(naive$Naive[3:64],naive$Cases[3:64])
plot(naive$Naive[3:64],naive$Cases[3:64])

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
head(testv)

par(mfrow=c(1,1))
hist(timev$AgeInYears,col="grey",xlab="Age in years",main="Age of cases",breaks=90,include.lowest=TRUE,right=F)
hist(testv$AgeInYears,col="black",breaks=50,include.lowest=TRUE,right=F,add=T)
legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n")

library(plyr)
testv$Dose1Mths<-revalue(testv$Dose1Mths, c("9999"=NA));
testv$Dose2Mths<-revalue(testv$Dose2Mths, c("9999"=NA));
plot(na.omit(testv$Dose1Mths))
plot(na.omit(testv$Dose2Mths))
testv$Dose1Mths<-as.numeric(testv$Dose1Mths)
testv$Dose2Mths<-as.numeric(testv$Dose2Mths)
testv$D2vac<-ifelse(testv$Dose1Mths > -1 & testv$Dose2Mths >= testv$Dose1Mths,testv$Dose2Mths,NA)
testv$D1vac<-ifelse(testv$Dose1Mths > -1 & is.na(testv$D2vac) == T,testv$Dose1Mths,NA)
testv$Unvac<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,testv$AgeInYears,NA)

bucket<-list(dose1=testv$D1vac/12,dose2=testv$D2vac/12,unvaccinated=testv$Unvac) # this puts all values in one list
# the melt command flattens the 'bucket' list into value/vectorname pairs
# the 2 columns are called 'value' and 'L1' by default
# 'fill' will color bars differently depending on L1 group
ggplot(melt(bucket), aes(value, fill = L1)) + xlab("Age in Years") +
  #call geom_histogram with position="dodge" to offset the bars and manual binwidth of 2
  geom_histogram(position = "stack", binwidth=1)+ guides(fill=guide_legend(title=NULL))+ theme(legend.position=c(.75, .75))

testv$VC<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,0,
                 ifelse(testv$Dose1Mths >= 0 & is.na(testv$Dose2Mths) == T,1,2))
AgeVac<-table(testv$VC,testv$AgeInYears)
barplot(AgeVac,xlab="Age",col=c("darkgrey","red","orange"),main="Age and vaccination status of cases")
legend("topright",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"))
row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)
AgeInYears<-(as.numeric(rownames(AgeVac)))
AgeVac<-cbind(AgeVac,AgeInYears)       
AgeInYears<-(as.numeric(rownames(pop)))
pop<-cbind(pop,AgeInYears)
colnames(pop)<-c("Population","AgeInYears")
AgeV<-merge(pop,AgeVac,by="AgeInYears",all=T)
head(AgeV)
#AgeVac<-t(AgeVac)
## setwd -
write.table(AgeV,"AgeVaccinationCases.csv",row.names=F,sep=",")
#########################

## continue with stats...

test$AgeInYears<-findInterval(test$AgeInYears,c(3,6,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="0-2", "1"="3-5","2"="6-17","3"="18-24","4"="25+"));
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

testtable<-aggregate( cbind( DiseaseName + as.numeric(SurvWeek)) ~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
                 data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
head(testtable)

tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
                                 "5"="1-5","6"="6-10","7"="6-10","8"="6-10",
                                 "9"="6-10","10"="6-10"));

testtable$NZDep<-tnzd
head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
                 data = testtable , FUN=sum)

### denominator data
 setwd("~/Massey_2014/measles/data")
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

popn<-melt(dm,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
colnames(popn)<-c("Age","Ethnicity","NZDep","Popn")
popn
pnzd<-revalue(popn$NZDep, c("Dep1"="1-5","Dep2"="1-5","Dep3"="1-5","Dep4"="1-5",
                                 "Dep5"="1-5","Dep6"="6-10","Dep7"="6-10","Dep8"="6-10",
                                 "Dep9"="6-10","Dep10"="6-10"));

popn$NZDep<-pnzd
head(popn)
tp<-aggregate( cbind(Popn) ~ NZDep+Age+Ethnicity, 
               data = popn , FUN=sum)

# JM's working code
#popn$NZDep <- as.numeric(popn$NZDep)
#popn$merge <- paste(popn$NZDep, popn$Age, popn$Ethnicity)
#testtable$merge <- paste(testtable$NZDep, testtable$Age, testtable$Ethnicity)
#testtable <- testtable[testtable$Ethnicity!="None",]

#popn$cases <- 0
#cases <- matrix(0, length(popn$merge),1)
#rownames(cases) <- popn$merge
#cases[testtable$merge,] <- testtable$Cases
#popn$cases <- cases

## end JM working code

tp$merge <- paste(tp$NZDep, tp$Age, tp$Ethnicity)
testtable$merge <- paste(testtable$NZDep, testtable$Age, testtable$Ethnicity)
testtable <- testtable[testtable$Ethnicity!="None",]

tp$cases <- 0
cases <- matrix(0, length(tp$merge),1)
rownames(cases) <- tp$merge
cases[testtable$merge,] <- testtable$Cases
tp$cases <- cases

#
#model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=popn,family=poisson)
#summary(model)
#
##
#
#modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|1,data=popn)
#summary(modelz)
#
#modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|Ethnicity+as.factor(NZDep)+offset(log(Popn)),data=popn)
#
#res<-predict(modelz)
#plot(res,popn$cases)
#cor(res,popn$cases)
#cor.test(res,popn$cases)
#
## reduce NZDep #s

hist(tp$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
tp$Ethnicity<- relevel(tp$Ethnicity, "European")
model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tp,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F") # F test, not Chisq, because dispersion estimated by moments

model2<-update(model1,~.-Age:NZDep)
summary(model2)
anova(model2,test="F")

par(mfrow=c(2,2))
hist(tp$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
plot(tp$cases,main="Cases per category",ylab="Count",pch=16,col="darkgrey")
res<-predict(model2)
plot(exp(res),tp$cases,xlab="results",ylab='predictions',main="Fit",pch=16,col="darkgrey")
cor(exp(res),tp$cases)
cor.test(exp(res),tp$cases)
hist(model2$residuals,main="Histogram of residuals",xlab="residuals",col="grey")
par(mfrow=c(1,1))
plot(tp$cases/tp$Popn,main="Cases per capita",ylab="Rate",pch=16,col=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(6,10)),
     xlab="Category")
legend("topleft",c("Asian","European","Maori","MELAA","Pacific"),
       col=c(1:4,6),pch=16,bty="n")

## drop MLA
tpsub<-tp[!(tp$Ethnicity=="MLA"),]

hist(tpsub$cases,xlab="Cases",main='Histogram of cases per category',col="darkgrey",breaks=20)

model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tpsub,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F")

model2<-update(model1,~.-Ethnicity:NZDep)
summary(model2)
anova(model2,test="F")

model3<-update(model2,~.-Age:NZDep)
summary(model3)
anova(model3,test="F")

##

par(mfrow=c(2,2))
hist(tpsub$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
plot(tpsub$cases,main="Cases per category",ylab="Count",pch=16,col="darkgrey")
res<-predict(model3)
plot(exp(res),tpsub$cases,xlab="predictions",ylab='cases',main="Fit",pch=16,col="darkgrey")
cor(exp(res),tpsub$cases)
cor.test(exp(res),tpsub$cases)
#abline(lm(exp(res)~tpsub$cases))
hist(model3$residuals,main="Histogram of residuals",xlab="residuals",col="grey")

##

# NB use 1-(1/R0) to estimate % additional vaccination from R0 values
setwd("~/GitHub/measles/estimatingR0")
aveR0<-read.csv("averageR0.csv",header=T)
head(aveR0)
# read out outbreak folder
outbreak_folder <- "outbreaks"

# read in and reorder by date
outbreak_files <- list.files(path=outbreak_folder, pattern="*.csv")

max_week <- rep("", length(outbreak_files))
for (i in 1:length(outbreak_files))
{
  incidence <- read.csv(file.path(outbreak_folder, outbreak_files[i]), stringsAsFactors=F)
  max_week[i] <- max(incidence$week)
}
max_week<-as.numeric(max_week)
weight<-max_week/sum(max_week)

library(plyr)
df<-summarise(aveR0,#c("outbreak13.csv","outbreak33.csv","outbreak37.csv","outbreak38.csv","outbreak99.csv"),
              #summarise,
              #avg.ob13<-
                mean(aveR0$outbreak13.csv),
              #avg.ob33<-
                mean(aveR0$outbreak33.csv),
              #avg.ob37<-
                mean(aveR0$outbreak37.csv),
              #avg.ob38<-
                mean(aveR0$outbreak38.csv),
              #avg.ob99<-
                mean(aveR0$outbreak99.csv),
              #sd.ob13<-
                sd(aveR0$outbreak13.csv),
              #sd.ob33<-
                sd(aveR0$outbreak33.csv),
              #sd.ob37<-
                sd(aveR0$outbreak37.csv),
              #sd.ob38<-
                sd(aveR0$outbreak38.csv),
              #sd.ob99<-
                sd(aveR0$outbreak99.csv))
## weighted estimates
r0estAve<-sum(df[,1:5]*max_week)/(sum(max_week))
r0estSd<-sum(df[,6:10]*max_week)/(sum(max_week))
## 95% CI
r095<-c(r0estAve-1.96*r0estSd,r0estAve+1.96*r0estSd)
## for 2014 only
r09514<-c(df[,5]-1.96*df[,10],df[,5]+1.96*df[,10])

## Pvac<-1-(1/R0)
## R'0' estimates == 0.92 - 1.19 (all, weighted)
## R'0' estimates == 1.82 - 2.13 (2013/2014)
head(pop)
head(naive)
# proportion naive
colSums(naive)

numberhigh<-round(sum(0.53*naive$Naive)) # 
sum(naive$Naive[0:21]) # 
sum(naive$Naive[3:30]) #
sum(naive$Naive[1:5]) # impossible

numberlow<-round(sum(0.16*naive$Naive)) # 
sum(naive$Naive[5:13]) # 
sum(naive$Naive[1:5]) # 

plot(round(0.53*naive$Naive) ,col="grey",bg="red",pch=21,ylab="Number to vaccinate",xlab="Age in Years",
     main=expression('Numbers to vaccinate'))
mtext(expression("Based on upper estimates of R"[0]*" from 2014 (53%) or 2007-14 (16%) outbreaks"))
points(naive$Naive[0:21],col="grey",bg="orange",pch=22) # 
points(naive$Naive[3:30],col="grey",bg="yellow",pch=23) 
points(jitter(round(0.16*naive$Naive)),col="grey",bg="lightblue",pch=21) 
points(jitter(naive$Naive[5:13]),col="grey",pch=22,bg="skyblue") # 
points(naive$Naive[1:5],col="grey",pch=23,bg="navyblue") #
legend("topright",c("53% naive, all ages","53% naive, all 0-21 yr olds","53% naive, all 3-30 yr olds",
                    "16% naive, all ages","16% naive, all 5-13 yr olds","16% naive, all 1-5 yr olds"),
       bty="n",pch=c(rep(21:23,2)),col=rep("grey",6),pt.bg=c("red","orange","yellow","lightblue","skyblue","navyblue"),cex=1)
