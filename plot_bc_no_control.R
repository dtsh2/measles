result <- vector("list")
sample<-seq(1,2000,by=1)
for (i in 1:length(sample))
{
  result[[i]] <- benefit_cost_no(vacc_pred_no, vacc_cost = i) # I used print to see the results in the screen
}

#Print the first matrix on screen
#result[[1]][1,21]
# res<-vector()
# for (i in 1:length(sample))
# {
#   res[j]<-result[[i]][21,11]
# }
#   
# plot(res[[1]][21,11])

bc_res <- sapply(1:length(result), function(i) as.numeric(result[[i]][21,11]))
#bc_res
pdf(file.path(dhb_fig_path, paste("bc_no.pdf")))

plot(bc_res,xaxt='n',ylim=c(0,max(bc_res)), col='red',lwd=2,type='l',ylab="benefit/cost ratio",xlab="Cost")
#axis(1, at=1:length(sample), labels=signif(sample,2),tick=F)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))

segments(x0=1667, y0=1, x1 = 0, y1 = 1)
segments(x0=1667, y0=0, x1 = 1667, y1 = 1)

par(new=TRUE)
par(oma=c(1,2,4,1))
par(mfcol=c(2,2), mfg=c(1,2))
par(mar=c(2,2,2,2))

plot(bc_res,xaxt='n',ylim=c(0,max(bc_res)), xlim=c(0,200),col='red',lwd=2,type='l',ylab="",xlab="")
#axis(1, at=1:length(sample), labels=signif(sample,2),tick=F)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
segments(x0=1667, y0=1, x1 = 0, y1 = 1)

dev.off()

b<-as.data.frame(bc_res)
which.min(abs(b$bc_res - 1))
