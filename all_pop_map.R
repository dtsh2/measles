library(RColorBrewer)
library(classInt)

data_denom<-read.csv("2013-mb-dataset-Total-New-Zealand-individual-part-1_AU.csv",header=T)

popimmune<-read.csv("PopnImmunityAll.csv",header=T)
popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])

data_denom$AGE_10_14_im <- 4/5 * data_denom$AGE_10_14 * popimmune[7,3] + 1/5  * data_denom$AGE_10_14 * popimmune[8,3]
data_denom$AGE_15_19_im <- 4/5 * data_denom$AGE_15_19 * popimmune[8,3] + 1/5  * data_denom$AGE_15_19 * popimmune[9,3]
data_denom$AGE_20_24_im <- 4/5 * data_denom$AGE_20_24 * popimmune[9,3] + 1/5  * data_denom$AGE_20_24 * popimmune[10,3]
data_denom$AGE_25_29_im <- 4/5 * data_denom$AGE_25_29 * popimmune[10,3] + 1/5  * data_denom$AGE_25_29 * popimmune[11,3]
data_denom$AGE_30_34_im <-  data_denom$AGE_30_34  * popimmune[11,3]
data_denom$AGE_35_39_im <-  data_denom$AGE_35_39  * popimmune[11,3]
data_denom$AGE_40_44_im <-  data_denom$AGE_40_44  * popimmune[11,3]
data_denom$AGE_45_49_im <-  data_denom$AGE_45_49  * popimmune[11,3]
data_denom$AGE_50_54_im <-  3/5 * data_denom$AGE_50_54 * popimmune[11,3] + 2/5  * data_denom$AGE_10_14 * popimmune[12,3]
data_denom$AGE_55_59_im <-  data_denom$AGE_55_59  * popimmune[12,3]
data_denom$AGE_60_64_im <-  data_denom$AGE_60_64 * popimmune[12,3]
data_denom$AGE_65_OVER_im <- data_denom$AGE_65_OVER * popimmune[12,3]

# data_denom$AGE_10_14_pc <- data_denom$AGE_10_14_im / data_denom$AGE_10_14
# data_denom$AGE_15_19_pc <- data_denom$AGE_15_19_im / data_denom$AGE_15_19
# data_denom$AGE_20_24_pc <- data_denom$AGE_20_24_im / data_denom$AGE_20_24
# data_denom$AGE_25_29_pc <- data_denom$AGE_25_29_im / data_denom$AGE_25_29
# data_denom$AGE_30_34_pc <- data_denom$AGE_30_34_im / data_denom$AGE_30_34
# data_denom$AGE_35_39_pc <- data_denom$AGE_35_39_im / data_denom$AGE_35_39
# data_denom$AGE_40_44_pc <- data_denom$AGE_40_44_im / data_denom$AGE_40_44
# data_denom$AGE_45_49_pc <- data_denom$AGE_45_49_im / data_denom$AGE_45_49
# data_denom$AGE_50_54_pc <- data_denom$AGE_50_54_im / data_denom$AGE_50_54
# data_denom$AGE_55_59_pc <- data_denom$AGE_55_59_im / data_denom$AGE_55_59
# data_denom$AGE_60_64_pc <- data_denom$AGE_60_64_im / data_denom$AGE_60_64
# data_denom$AGE_65_OVER_pc <- data_denom$AGE_65_OVER_im / data_denom$AGE_65_OVER

data_denom$AGE_TOTAL_im <- 
  
data_denom$AGE_10_14_im+
data_denom$AGE_15_19_im+
data_denom$AGE_20_24_im+
data_denom$AGE_25_29_im+
data_denom$AGE_30_34_im+
data_denom$AGE_35_39_im+
data_denom$AGE_40_44_im+
data_denom$AGE_45_49_im+
data_denom$AGE_50_54_im+
data_denom$AGE_55_59_im+
data_denom$AGE_60_64_im+
data_denom$AGE_65_OVER_im

data_denom$AGE_TOTAL_dh <- 
  
  data_denom$AGE_10_14+
  data_denom$AGE_15_19+
  data_denom$AGE_20_24+
  data_denom$AGE_25_29+
  data_denom$AGE_30_34+
  data_denom$AGE_35_39+
  data_denom$AGE_40_44+
  data_denom$AGE_45_49+
  data_denom$AGE_50_54+
  data_denom$AGE_55_59+
  data_denom$AGE_60_64+
  data_denom$AGE_65_OVER


data_denom$pc <- data_denom$AGE_TOTAL_im / data_denom$AGE_TOTAL_dh

###############

library(maptools)
library(lubridate)
library(dplyr)

# read in shapefile for AU's
au <- readShapeSpatial("AU2013_GV_Full")

# read in domicile <-> AU map
data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

testgp <- data_denom %>% mutate(area.unit=AU_CODE)
testgp <- testgp %>% left_join(data_dom)

testgp <- testgp %>% mutate(AU2013 = area.unit)

testgp <- testgp %>% mutate(dom = DOMICILE_CODE)
# grouped_all_years <- grouped_all_years %>% left_join(data_denom)

data_au <- slot(au, "data") %>% mutate(AU2013 = as.numeric(as.character(AU2013)))
data_au <- data_au %>% left_join(testgp)

in_the_sea <- grepl("^Oceanic", data_au$AU2013_NAM)

## plot and save

fixedBreaks <- c(0,0.85,0.9,0.95,1,Inf)

################

# breaks <- cut(data_au$prop1, fixedBreaks)
breaks <- cut(data_au$pc, fixedBreaks)

####

pal<-c(heat.colors(3),"green","blue")
cols <- pal[breaks]

cols[in_the_sea] <- NA
par(mfrow=c(1,1))

#####

pdf(paste("census_immunity_age.pdf"), width=7, height=5)

plot(au, col=cols,lwd=1,lty=3,main="MMR1")
legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       bty="n",inset=0.1,title="Percent vaccinated")
dev.off()
#####

pdf(paste("census_immunity_age_hist.pdf"), width=7, height=5)
hist(data_au$pc, breaks=50,col="grey", xlab="Proportion vaccinated",main="")
abline(v=.95,col="red",lty=3,lwd=3)
abline(v=median(data_au$pc,na.rm=T),col="orange",lty=3,lwd=3)
text(x=c(0.835,.85),y=c(200,200),c("median = ",round(median(data_au$pc,na.rm=T),2)))
 dev.off()

###

## Christchurch
pdf(paste("census_immunity_age_ChCh.pdf"), width=7, height=5)

#####

plot(au, col=cols,lwd=1,lty=3,main="MMR1",xlim=c(1550461 , 1559320),ylim=c(5141317 , 5212140)) # Christchurch
legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       # bty="n",inset=0.0,
       title="Percent vaccinated",bg="white",box.col="white")
 dev.off()


## Auckland

pdf(paste("census_immunity_age_Auk.pdf"), width=7, height=5)

plot(au, col=cols,lwd=1,lty=3,main="MMR1",xlim=c( 1693320, 1803320),ylim=c(5892140, 5952140)) # auckland
legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       # bty="n",inset=0.0,
       title="Percent vaccinated",bg="white",box.col="white")
 dev.off()

## Wellington

 pdf(paste("census_immunity_age_Well.pdf"), width=7, height=5)

plot(au, col=cols,lwd=1,lty=3,main="MMR1",xlim=c( 1730320, 1784500),ylim=c(5421140, 5444400)) # wellington
legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       # bty="n",inset=0.0,
       title="Percent vaccinated",bg="white",box.col="white")
dev.off()

##############
res_age<- testgp[,c(2:17,19:33)]
colnames(res_age) <-c("Area_unit","AGE_0_4","AGE_5_9","AGE_10_14","AGE_15_19","AGE_20_24","AGE_25_29",
                      "AGE_30_34","AGE_35_39","AGE_40_44","AGE_45_49","AGE_50_54","AGE_55_59","AGE_60_64",
                      "AGE_65_OVER","AGE_TOTAL","AGE_10_14_im","AGE_15_19_im","AGE_20_24_im","AGE_25_29_im",
                      "AGE_30_34_im","AGE_35_39_im","AGE_40_44_im","AGE_45_49_im","AGE_50_54_im","AGE_55_59_im",
                      "AGE_60_64_im", "AGE_65_OVER_im","AGE_TOTAL_im","AGE_TOTAL_hayman","Percent_Immune")
write.csv(res_age,"area_sero_immunity.csv",row.names=F)
