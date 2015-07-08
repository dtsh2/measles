R0 = 12.8;
x0max = 0.3;
Nx0 = 200;
P = (1:Nx0)*x0max/(Nx0+1);
x0 = P/(1-exp(-R0*P));
Rv = R0*x0;
Pop = c(436350, # auck # 3
        206000, # bay of plenty # 7
        482180, # canterbury # 18
        283700, # capital and coast # 14
        469300, # Counties Manukau # 4
        151700, # hawkes bay # 10
        138380, # hutt # 13
        98196, # lakes # 6
        162560, # midcentral # 12
        137000, # nelson marlborough # 16
        151690, # northland # 1
        55620, # south canterbury # 19
        297420, # southern # 20
        43650, # Tairawhiti # 8
        109750, # taranaki # 9
        359310, # waikato # 5
        41112, # wairarapa # 15
        525550,  # Waitemata # 2
        32151, # west coast # 17
        60120, # whanganui # 11
        4353189); # population
Naive = c(74410, # 3
          35150, # 7
          82195, # 18
          48368, # 14
          80484, # 4
          25934, # 10
          23864, # 13
          16854, # 6
          27397, # 12
          23502, # 16
          25902, # 1
          9444, # 19
          50047, # 20
          7473, # 8
          18842, # 9
          61138, # 5
          7005, # 15
          90522, # 2
          5554, # 17
          10218, # 11
          742103); # yikes!
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

write.csv(Z,"tables/dhb_vacc.csv",row.names=F)
