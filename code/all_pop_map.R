library(RColorBrewer)
library(classInt)

data_denom<-read.csv("2013-mb-dataset-Total-New-Zealand-individual-part-1_AU.csv",header=T)

# expand the age groups to individual ages by assuming that all ages are equally likely
# NOTE: This analysis only uses those over the age of 10 and ignores all others

cols <- paste("AGE",seq(10,60,by=5),seq(10,60,by=5)+4, sep="_")
au_ages <- data_denom[,rep(cols, each=5)]/5
au_ages <- cbind(au_ages, data_denom[,"AGE_65_OVER"])
names(au_ages) <- 10:65

# multiply up by population immunity levels
# TODO: THIS IS REPEATED CODE - ELIMINATE IT!!!
popimmune<-read.csv("data/PopnImmunityAll.csv",header=T)
pop<-read.csv("data/popnsize.csv",header=T)
colnames(pop)<-0:100
# expand popimmune out to years 0..100
NaiveByYear <- data.frame(Age=0:100, Population=t(pop), Immunity=NA)
for (i in 1:nrow(popimmune)) {
  start <- popimmune$AgeStart[i]+1
  end   <- ifelse(is.finite(popimmune$AgeEnd[i]), popimmune$AgeEnd[i]+1, nrow(NaiveByYear))
  NaiveByYear$Immunity[start:end] <- popimmune$Immunity[i]
}

immunity <- (NaiveByYear %>% filter(Age >= 10 & Age <= 65) %>% select(Immunity))[,1]
au_imm <- sweep(au_ages, 2, immunity, "*")

data_denom$pc <- rowSums(au_imm) / rowSums(au_ages)

###############
## MAPS HERE
###############

map_dir <- "figures"

library(maptools)
library(lubridate)
library(dplyr)

# read in shapefile for AU's
au <- readShapeSpatial("AU2013_GV_Full")

data_au <- slot(au, "data") %>% mutate(AU_CODE = as.numeric(as.character(AU2013)))
data_au <- data_au %>% left_join(data_denom)

in_the_sea <- grepl("^Oceanic\\-", data_au$AU2013_NAM)
tidal      <- grepl("^Tidal\\-", data_au$AU2013_NAM)
inlet      <- grepl("^Inlet[s]*\\-", data_au$AU2013_NAM)
inland_water <- grepl("^Inland Water\\-", data_au$AU2013_NAM)
no_area    <- data_au$LAND_AREA_ == 0

## plot and save

fixedBreaks <- c(0,0.85,0.9,0.95,1,Inf)

################

# breaks <- cut(data_au$prop1, fixedBreaks)
breaks <- cut(data_au$pc, fixedBreaks)

####

pal<-c(heat.colors(3),"green","blue")
cols <- pal[breaks]

cols[in_the_sea | tidal | inlet | inland_water | no_area] <- NA

#####

pdf(file.path(map_dir, "census_immunity_age.pdf"), width=7, height=5)
plot(au, col=cols,lty=0,main="")
legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       bty="n",inset=0.1,title="Percent immune")
dev.off()
#####

pdf(file.path(map_dir, "census_immunity_age_hist.pdf"), width=7, height=5)
hist(data_au$pc, breaks=50,col="grey", xlab="Proportion vaccinated",main="")
abline(v=.95,col="red",lty=3,lwd=3)
abline(v=median(data_au$pc,na.rm=T),col="orange",lty=3,lwd=3)
text(x=c(0.835,.85),y=c(200,200),c("median = ",round(median(data_au$pc,na.rm=T),2)))
 dev.off()

#####
main_centers <- read.table(header=TRUE, text = "
      City xmin    xmax    ymin    ymax
      ChCh 1550461 1559320 5141317 5212140
      Auk  1693320 1803320 5892140 5952140
      Well 1730320 1784500 5421140 5444400")

for (center in 1:nrow(main_centers)) {
  map_file <- paste0("census_immunity_age_", main_centers$City[center], ".pdf")
  pdf(file.path(map_dir, map_file), width=7, height=5)
  
  xlim <- as.numeric(main_centers[center, c("xmin", "xmax")])
  ylim <- as.numeric(main_centers[center, c("ymin", "ymax")])
  plot(au, col=cols, lty=0, xlim=xlim, ylim=ylim)
  legend("topright",c("0-85%","86-90%","91-95%","96-100%",">100%","NA")
         ,fill=c(pal,"white"),
         # bty="n",inset=0.0,
         title="Percent immune",bg="white",box.col="white")
  dev.off()
}

##############
res_age<- testgp[,c(2:17,19:33)]
colnames(res_age) <-c("Area_unit","AGE_0_4","AGE_5_9","AGE_10_14","AGE_15_19","AGE_20_24","AGE_25_29",
                      "AGE_30_34","AGE_35_39","AGE_40_44","AGE_45_49","AGE_50_54","AGE_55_59","AGE_60_64",
                      "AGE_65_OVER","AGE_TOTAL","AGE_10_14_im","AGE_15_19_im","AGE_20_24_im","AGE_25_29_im",
                      "AGE_30_34_im","AGE_35_39_im","AGE_40_44_im","AGE_45_49_im","AGE_50_54_im","AGE_55_59_im",
                      "AGE_60_64_im", "AGE_65_OVER_im","AGE_TOTAL_im","AGE_TOTAL_hayman","Percent_Immune")
write.csv(res_age,"area_sero_immunity.csv",row.names=F)
