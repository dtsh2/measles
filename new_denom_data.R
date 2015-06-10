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

#############################################################################
## from here for each new year
###

grouped <- grouped_d %>% left_join(grouped_n1) %>% left_join(grouped_n2)

data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

# output folder
map_dir <- "nir_maps"
dir.create(map_dir, showWarnings = FALSE)

years <- 2006:2014

for (year in years) {
  
  grouped_year <- grouped[grouped$Year == year,]

  ####
  grouped_year <- grouped_year %>% group_by(DOMICILE_CODE) %>% summarize(total=sum(total), vacc1=sum(vacc1, na.rm=T), vacc2=sum(vacc2, na.rm=T))

  any(grouped_year$vacc1 > grouped_year$total)

  grouped_year <- grouped_year %>% mutate(dom = DOMICILE_CODE)
  grouped_year <- grouped_year %>% left_join(data_dom)

  grouped_year <- grouped_year %>% mutate(AU2013 = area.unit, prop1 = vacc1/total, prop2= vacc2/total)

  data_au <- slot(au, "data") %>% mutate(AU2013 = as.numeric(as.character(AU2013)))
  data_au <- data_au %>% left_join(grouped_year)

  fixedBreaks <- c(0,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,1,Inf)

  for (vaccine in 1:2) {
    breaks <- cut(data_au[, paste0("prop",vaccine)], fixedBreaks)

    pal<-c(heat.colors(8),"green","blue")
    cols <- pal[breaks]
  
    # make the AUs in the sea transparent
    in_the_sea <- grepl("^Oceanic", data_au$AU2013_NAM)
    cols[in_the_sea] <- NA
  
    # do the plot
    map_file <- paste0("nir_census_MMR", vaccine, "_NIR_", year, ".pdf")
    pdf(file.path(map_dir, map_file), width=9, height=6)
    text.cex <- 0.8
    par(mai=rep(0.1,4))
    xlim <- c(0, 2120000)
    ylim <- c(4974000, 5920000)
  
    plot(au, col=cols,lty=0, xlim=xlim, ylim=ylim)
    legend(2120000,5274000,c("0-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-95%","96-98%", "99-100%",">100%","NA")
         ,fill=c(pal,"white"),
         bty="n",inset=0.1,title="Percent vaccinated", cex=text.cex, xjust=1)
  
    par(fig = c(0, 0.65, 0.35, 0.95), mar=c(5,5,2,2), new=TRUE)
    hist(data_au$prop2, breaks=50,col="grey", border=NA, xlab="Proportion vaccinated",main="", cex.axis=text.cex)
    abline(v=.95,col="red",lty=3,lwd=3)
    abline(v=median(data_au$prop2,na.rm=T),col="orange",lty=3,lwd=3)
    dev.off()

    main_centers <- read.table(header=TRUE, text = "
      City xmin    xmax    ymin    ymax
      ChCh 1550461 1559320 5141317 5212140
      Auk  1693320 1803320 5892140 5952140
      Well 1730320 1784500 5421140 5444400")
    
    for (center in 1:nrow(main_centers)) {
      map_file <- paste0("nir_census_MMR", vaccine, "_NIR_", main_centers$City[center], "_", year, ".pdf")
      pdf(file.path(map_dir, map_file), width=7, height=6)

      xlim <- as.numeric(main_centers[center, c("xmin", "xmax")])
      ylim <- as.numeric(main_centers[center, c("ymin", "ymax")])
      plot(au, col=cols, lty=0, xlim=xlim, ylim=ylim)
      legend("left",c("0-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-95%","96-98%", "99-100%",">100%","NA")
       ,fill=c(pal,"white"),
      title="Percent vaccinated",bg="white",box.col="white")
      dev.off()
    }
  }
}


######################################################################
pdf(paste("nir_census_MMR1_NIR_2014_cor.pdf"), width=7, height=7)
plot(data_au$prop1,data_au$prop2,xlab="MMR1",ylab="MMR2")
txt<-c(round(cor(data_au$prop1,data_au$prop2,use="complete.obs"),2))
text(x=c(0,0.05),y=c(1.1,1.1),c(expression("R = "),txt))
dev.off()

########################################################################
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
sink(file="results2006.txt") 
options(max.print=999999)
####
results
sink(NULL)
