rm(list=ls())

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
write.csv(NaiveByYear, "tables/NaiveByYear.csv", row.names=FALSE)
