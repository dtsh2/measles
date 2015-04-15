library(maptools)
library(lubridate)
library(dplyr)

# read in shapefile for AU's
au <- readShapeSpatial("AU2013_GV_Full")

# read in domicile <-> AU map
data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

data_d <- read.table("NIR_CI_denominator_raw_data_20150106_encrypted.txt", header=T, sep="|")
data_d <- data_d %>% mutate(Year = year(dmy(DATE_OF_BIRTH)))
grouped_d <- data_d %>% group_by(DOMICILE_CODE, Year) %>% summarize(total = length(unique(UNIQUE_PATIENT_IDENTIFIER)))

data_n <- read.table("NIR_MMR_numerator_raw_data_20150106_encrypted.txt", header=T, sep="|")
data_n <- data_n %>% mutate(Year = year(dmy(DATE_OF_BIRTH)))
grouped_n1 <- data_n %>% filter(VACCINE_DOSE == 1) %>% group_by(DOMICILE_CODE, Year) %>% summarize(vacc1 = length(unique(UNIQUE_PATIENT_IDENTIFIER)))
grouped_n2 <- data_n %>% filter(VACCINE_DOSE == 2) %>% group_by(DOMICILE_CODE, Year) %>% summarize(vacc2 = length(unique(UNIQUE_PATIENT_IDENTIFIER)))

####
## from here for each new year
###

grouped <- grouped_d %>% left_join(grouped_n1) %>% left_join(grouped_n2)

## 2006 -- 2014
grouped <- grouped[grouped$Year == 2014,]

####
grouped_all_years <- grouped %>% group_by(DOMICILE_CODE) %>% summarize(total=sum(total), vacc1=sum(vacc1, na.rm=T), vacc2=sum(vacc2, na.rm=T))

any(grouped_all_years$vacc1 > grouped_all_years$total)

data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

grouped_all_years <- grouped_all_years %>% mutate(dom = DOMICILE_CODE)
grouped_all_years <- grouped_all_years %>% left_join(data_dom)

grouped_all_years <- grouped_all_years %>% mutate(AU2013 = area.unit, prop1 = vacc1/total, prop2= vacc2/total)
data_au <- slot(au, "data") %>% mutate(AU2013 = as.numeric(as.character(AU2013)))
data_au <- data_au %>% left_join(grouped_all_years)

in_the_sea <- grepl("^Oceanic", data_au$AU2013_NAM)

## plot and save

fixedBreaks <- c(0,0.4,0.5,0.6,0.7,0.8,0.9,0.95,1,Inf)

####

breaks <- cut(data_au$prop1, fixedBreaks)
# breaks <- cut(data_au$prop2, fixedBreaks)

####

pal<-c(heat.colors(7),"green","blue")
cols <- pal[breaks]

cols[in_the_sea] <- NA
par(mfrow=c(1,1))

#####

pdf(paste("nir_census_MMR1_NIR_2014.pdf"), width=7, height=5)
# pdf(paste("nir_census_MMR2_NIR_2014.pdf"), width=7, height=5)

#####

plot(au, col=cols,lwd=1,lty=3,main="MMR1")
legend("topright",c("0-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-95%","96-100%",">100%","NA")
       ,fill=c(pal,"white"),
       bty="n",inset=0.1,title="Percent vaccinated")
par(fig = c(0.05, 0.4, 0.5,0.9), mar=c(5,5,2,2), new=TRUE)

#####

hist(data_au$prop1, breaks=50,col="grey", xlab="Proportion vaccinated",main="")
# hist(data_au$prop2, breaks=50,col="grey", xlab="Proportion vaccinated",main="")

##### 

abline(v=.95,col="red",lty=3,lwd=3)

#####

abline(v=median(data_au$prop1,na.rm=T),col="orange",lty=3,lwd=3)
# abline(v=median(data_au$prop2,na.rm=T),col="orange",lty=3,lwd=3)

#####
dev.off()
par(mfrow=c(1,1))

####

sink(file="MMR1summary2014.txt") 
summary(data_au$prop1)
# sink(file="MMR2summary2014.txt") 
# summary(data_au$prop2)

sink(NULL)

####

results<-cbind(data_au$total,data_au$vacc1,
               data_au$vacc2,round(data_au$prop1,4)*100,round(data_au$prop2,4)*100)
nams<-as.character(data_au$AU2013_NAM)
results<-cbind.data.frame(nams,results)
colnames(results)<-c("Area","Population","MMR1","MMR2","MMR1 %", "MMR2 %")

####
sink(file="results2014.txt") 

####
results
sink(NULL)
