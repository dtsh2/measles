R0 = 12.8;
x0max = 0.3;
Nx0 = 200;
P = (1:Nx0)*x0max/(Nx0+1);
x0 = P/(1-exp(-R0*P));
Rv = R0*x0;
Pop = c(436350,206000,482180,283700,469300,151700,138380,98196,162560,137000,151690,
        55620,297420,43650,109750,359310,41112,525550,32151,60120,4241739);
Naive = c(51673,20511,50995,32406,55085,15471,15078,10470,17196,12960,14793,5199,
          31389,4727,11378,39087,3901,57904,3172,6025,459420);
RvDHB = R0*Naive/Pop;

library(akima)
PDHB = aspline(x=Rv,y=P,xout=RvDHB);

# The number of cases per DHB
FSDHB = PDHB$y*Naive;
# The number of vaccinations short
VDHB = Naive-Pop/R0;

Z<-data.frame(nrows=21,ncols=5)
for (i in 1:21){
Z[i,1] = Pop[i];
Z[i,2] = Naive[i];
Z[i,3] = round(FSDHB[i]);
Z[i,4] = round(VDHB[i]);
Z[i,5] = round(round(VDHB[i])/Naive[i],2)}
colnames(Z)<-c("Population","Naive","Outbreak","Vaccination","PC")

write.csv(Z,"dhb_vacc.csv",row.names=F)
