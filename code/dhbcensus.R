rm(list=ls())

# output path for figures
dhb_fig_path <- figures

# read data
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
pop<-read.csv("data/popnsize.csv",header=T)
colnames(pop)<-0:100
# expand popimmune out to years 0..100
NaiveByYear <- data.frame(Age=0:100, Population=t(pop), Immunity=NA)
for (i in 1:nrow(popimmune)) {
  start <- popimmune$AgeStart[i]+1
  end   <- ifelse(is.finite(popimmune$AgeEnd[i]), popimmune$AgeEnd[i]+1, nrow(NaiveByYear))
  cat("start=", start, "end=", end, "\n")
  NaiveByYear$Immunity[start:end] <- popimmune$Immunity[i]
}
NaiveByYear$Naive <- round(NaiveByYear$Population * (1-NaiveByYear$Immunity))

# check by plotting
plot(Population ~ Age, data=NaiveByYear)
points(Naive ~ Age,pch=16, data=NaiveByYear)
legend("topright",c("Population","Naïve"),pch=c(1,16),bty="n")

# match cases per age
dhbpop<-read.csv("data/dhbcensus.csv",header=T)
df.expanded <- dhbpop[rep(row.names(dhbpop),each=5),] # expand out by repeating each age group (row) 5 times
df <- df.expanded[,-1]/5
rownames(df) <- 1:nrow(df)-1
popdhb<-colSums(df)
naivedhb<-df*(1-NaiveByYear$Immunity[1:nrow(df)])

colnames(naivedhb)<-c("Northland","Waitemata","Auckland","Counties Manukau",
                      "Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki",
                      "Hawke's Bay","Whanganui","Midcentral","Hutt","Capital and Coast",
                      "Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury",
                      "Southern")

# and plot
for (i in 1:20){
  pdf(file.path(dhb_fig_path, paste("dhb", i, ".pdf", sep = "")))
  barplot(naivedhb[,i],ylim=c(0,5000),main=colnames(naivedhb)[i],xlab="Age",ylab="Numbers",
       #type="l",
       cex=1.1,cex.lab=1.1,cex.main=1.1,cex.axis=1.1)
  legend("topright",legend=c(c("Total naïve", round(sum(naivedhb[,i]))),
                             c("Total population", signif(popdhb[i],5)),
                             c("Percent naïve",round(sum(naivedhb[,i])/popdhb[i],3)*100)),
                             bty="n")
  dev.off()
}  

# micks code now to derive attack size etc.
R0 = 12.8;
x0max = 0.3;
Nx0 = 200;
P = (1:Nx0)*x0max/(Nx0+1);
x0 = P/(1-exp(-R0*P));
Rv = R0*x0;

# order alphabetically
dhb_order <- order(colnames(naivedhb))

Naive <- round(colSums(naivedhb)[dhb_order])
Pop   <- popdhb[dhb_order]

RvDHB = R0*Naive/Pop;

library(akima)
PDHB = aspline(x=Rv,y=P,xout=RvDHB);

# The number of cases per DHB
FSDHB = round(PDHB$y*Naive);

# The number of vaccinations short
VDHB = round(Naive-Pop/R0);

Z<-data.frame(Population=Pop,
              Naïve=Naive,
              Outbreak=FSDHB,
              Vaccination=VDHB, stringsAsFactors = FALSE)
# add TOTAL row...
Z <- rbind(Z, colSums(Z))
Z$DHB <- c(names(naivedhb)[dhb_order], "TOTAL")
Z$PC = round(Z$Vaccination / Z$Naïve, 2)
Z <- Z[,c(5,1:4,6)]
write.csv(Z,"tables/dhb_vacc.csv",row.names=F)
