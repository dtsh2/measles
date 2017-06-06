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
plot(bc_res,xaxt='n',ylim=c(0,max(bc_res)), type='l')
axis(1, at=1:length(sample), labels=signif(sample,2),tick=F)
abline(h=1, col='red',lty=3)
b<-as.data.frame(bc_res)
which.min(abs(b$bc_res - 1))
