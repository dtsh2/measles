
# compute generation time.  We're wanting a lognormal distribution with mean 12.0 and sd 3.5 from
# http://www.sciencedirect.com/science/article/pii/S0022519311003146
mu    <- 12
sigma <- 3.5
sigma_logn <- sqrt(log(1 + (sigma/mu)^2))
mu_logn    <- log(mu) - log(1 + (sigma/mu)^2) / 2

# then exp(rnorm(n, mu_logn, sigma_logn)) simulates from lognormal with the given mean and sd.

# the R0 library can estimate our distribution from incidence data
require(R0)
source("estR0.R")

# generation time in weeks
genTime <- generation.time(type="lognormal", val=c(12, 3.5)/7)

# read out outbreak folder
outbreak_folder <- "all_ob"

# read in and reorder by date
outbreak_files <- list.files(path=outbreak_folder, pattern="*.csv")

max_dates <- rep("", length(outbreak_files))
for (i in 1:length(outbreak_files))
{
  incidence <- read.csv(file.path(outbreak_folder, outbreak_files[i]), stringsAsFactors=F)
  max_dates[i] <- max(incidence$date)
}

outbreak_files <- outbreak_files[order(max_dates)]
cols <- rev(rainbow(length(outbreak_files))) # reversed as the output files are in reverse order so they plot in order
# function for modifying colours to make them transparent
alpha <- function(col, a)
{
  if (nchar(col) == 9)
    rgb(t(col2rgb(substr(col,1,7))/255), alpha=a)
  else
    rgb(t(col2rgb(col)/255), alpha=a)
}

# do the separate analyses
average_R0 <- vector("list", length(outbreak_files))
length_R0  <- vector("list", length(outbreak_files))
size_R0    <- vector("list", length(outbreak_files))
start_R0   <- vector("list", length(outbreak_files))
end_R0     <- vector("list", length(outbreak_files))
for (i in 1:length(outbreak_files))
{
  ob_file <- outbreak_files[i]
  incidence <- read.csv(file.path(outbreak_folder, ob_file))

  # convert our incidence data to something we can use
  counts <- incidence$incidence
  names(counts) <- incidence$date

  # if we have too long gaps in the data we'll need to strip it out
  counts_rle <- rle(counts)
  gaps <- which(counts_rle$values == 0 & counts_rle$length > 3)

  if (length(gaps)) {
    keep   <- sum(counts_rle$lengths[1:(gaps[1]-1)])
    counts <- counts[1:keep]
  }

  # if we have too few counts, skip
  if (length(counts) > 2) {


#  counts <- counts[1:51]
#  estR0<-estimate.R(counts, genTime, t=1:51, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=1300000, nsim=100)
#  estR0<-estimate.R(counts, genTime, t=1:51, end=51, methods=c("EG"), pop.size=1300000, nsim=100)
#  estR0<-estimate.R(counts, genTime, t=1:51, end=51, methods=c("ML"), pop.size=1300000, nsim=100)
#  estR0<-estimate.R(counts, genTime, t=1:51, end=51, methods=c("TD"), pop.size=1300000, nsim=100)
#  estR0<-estimate.R(counts, genTime, t=1:51, end=51, methods=c("SB"), pop.size=1300000, nsim=100)

  # add dates....
  estR0<-jm_estR0(counts, genTime, t=1:length(counts), end=length(counts), methods=c("TD"), nsim=1000)

  months <- as.Date(as.vector(t(outer(2009:2015,1:12,function(x,y) { sprintf("%04d-%02d-01", x, y) }))))
  month_lab_short <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  month_lab_long  <- months(as.Date(sprintf("2005-%02d-01", 1:12)), T)
  month_lab <- rep(1:12,6)
  years  <- as.Date(sprintf("%04d-01-01", 2009:2015))

  date_range <- as.Date(names(counts))
  # plot...
  pdf(file.path(outbreak_folder, sprintf("R0_%s.pdf", strsplit(ob_file, split=".csv")[[1]])), width=8, height=4)
  plot(NULL, xlim=range(date_range), ylim=range(estR0$conf.int), ylab="R0", xaxt="n", xlab="")
  polygon(c(date_range,rev(date_range)), c(estR0$conf.int[,1], rev(estR0$conf.int[,2])), col=alpha(cols[i], 0.5), border=NA)
  lines(date_range, estR0$R, lwd=2, col=cols[i])
  abline(h=1)
  axis(1, at=months, labels=rep("",length(months)))
  axis(1, at=years, labels=rep("",length(years)), tcl=-2.5, lwd.ticks=1.5)

  # determine whether we should show months (the +10 and +20 is to center the month label at 15, middle of month)
  incl_month <- months+10 >= min(date_range) & months+20 < max(date_range)
  incl_year <- years >= min(date_range) & years < max(date_range)

  if (sum(incl_month)) {
    if (sum(incl_month) < 10) {
      mtext(month_lab_long[month_lab[incl_month]], side=1, at = months[incl_month] + 15, line=0.25)
    } else {
      mtext(month_lab_short[month_lab[incl_month]], side=1, at = months[incl_month] + 15, line=0.25)
    }
  }
  if (sum(incl_year))
  {
    mtext(format.Date(years[incl_year], "%Y"), side=1, at = years[incl_year], line=1.5, adj=-0.25)
    mtext(as.numeric(format.Date(years[incl_year], "%Y"))-1, side=1, at = years[incl_year], line=1.5, adj=1.25)
  }

  dev.off()

  average_R0[[i]] <- estR0$R0
  } else {
  average_R0[[i]] <- NA
  }
  length_R0[[i]]  <- length(counts)
  size_R0[[i]]    <- sum(counts)
  start_R0[[i]]   <- min(date_range)
  end_R0[[i]]     <- max(date_range)
}

# plot R0 averages

my_vioplot <- function(dat, bw, border, col, at)
{
  if (!any(is.na(dat))) {
    e <- density(dat) #, bw)
    m <- 0.3/max(e$y)
    incl <- e$y > max(e$y)/1000
    polygon(c(at-e$y[incl]*m,rev(at+e$y[incl]*m)), c(e$x[incl], rev(e$x[incl])), col=col, border=border)
  }
}

pdf("averageR0_all_ob.pdf", width=8, height=6)
range_R0 <- range(sapply(average_R0, function(x) { if(!any(is.na(x))) { range(x, na.rm=T) } else { NA } }), na.rm=T)
plot(NULL, xlim=c(0.5,length(average_R0)+0.5), ylim=range_R0 + diff(range_R0)*0.05*c(-1,1), ylab="Average Rv", xlab="", xaxt="n", yaxs="i")
for (i in 1:length(average_R0))
  my_vioplot(average_R0[[i]], bw=0.015, border=cols[i], col=cols[i], at=i)
abline(h=1, col="black")


labels <- sapply(start_R0, function(x) { format(x, "%Y") })
labels[labels == "2013" | labels == "2014"] <- "2013-2014"

rle_lab <- rle(labels)

axis(side=1, at=c(0.5,cumsum(rle_lab$lengths)+0.5), labels=rep("", length(rle_lab$lengths)+1))
mtext(rle_lab$values, side=1, at=cumsum(rle_lab$lengths)-0.5*rle_lab$lengths + 0.5, line=1)
dev.off()


