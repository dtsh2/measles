rm(list=ls())

# read data
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
pop<-read.csv("data/popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
         rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
naive<-round(pop-(pop*impop))

# check by plotting
plot(pop,xlab="Age",ylab="Population")
points(naive,pch=16)
legend("topright",c("Population","Naïve"),pch=c(1,16),bty="n")

# match cases per age
dhbpop<-read.csv("data/dhbcensus.csv",header=T)

df.expanded <- dhbpop[rep(row.names(dhbpop),5), 1:21]
df<-df.expanded[with(df.expanded, order(Age)), ]
df<-df[,2:21]/5
popdhb<-colSums(df)
naivedhb<-df*(1-impop[1:90])
head(naivedhb)
colnames(naivedhb)<-c("Northland","Waitemata","Auckland","Counties Manukau",
                      "Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki",
                      "Hawke's Bay","Whanganui","Midcentral","Hutt","Capital and Coast",
                      "Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury",
                      "Southern")

# and plot
for (i in 1:20){
  pdf(paste("dhb", i, ".pdf", sep = ""))
  barplot(naivedhb[,i],ylim=c(0,5000),main=colnames(naivedhb)[i],xlab="Age",ylab="Numbers",
       #type="l",
       cex=1.1,cex.lab=1.1,cex.main=1.1,cex.axis=1.1)
  legend("topright",legend=c(c("Total naive", round(sum(naivedhb[,i]))),
                             c("Total population", signif(popdhb[i],5)),
                             c("Percent naive",round(sum(naivedhb[,i])/popdhb[i],3)*100)),
                             bty="n")
  dev.off()
}  
