rm(list=ls())
library(dplyr)

# output path for figures
dhb_fig_path <- "figures"

# read data
NaiveByYear <- read.csv("tables/NaiveByYear.csv")

# check by plotting
plot(Population ~ Age, data=NaiveByYear)
points(Naive ~ Age,pch=16, data=NaiveByYear)
legend("topright",c("Population","Naïve"),pch=c(1,16),bty="n")

# match cases per age
dhbpop<-read.csv("data/dhbcensus.csv",header=T)
df.expanded <- dhbpop[rep(row.names(dhbpop),each=5),] # expand out by repeating each age group (row) 5 times
df <- df.expanded[,-1]/5
rownames(df) <- 1:nrow(df)-1
popdhb<-colSums(df)
naivedhb<-df*(1-NaiveByYear$Immunity[1:nrow(df)])

colnames(naivedhb)<-c("Northland","Waitemata","Auckland","Counties Manukau",
                      "Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki",
                      "Hawke's Bay","Whanganui","MidCentral","Hutt Valley","Capital and Coast",
                      "Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury",
                      "Southern")

# and plot
for (i in 1:20){
  pdf(file.path(dhb_fig_path, paste("dhb", i, ".pdf", sep = "")))
  barplot(naivedhb[,i],ylim=c(0,5000),main=colnames(naivedhb)[i],xlab="Age",ylab="Numbers",
       #type="l",
       cex=1.1,cex.lab=1.1,cex.main=1.1,cex.axis=1.1)
  legend("topright",legend=c(c("Total naïve", round(sum(naivedhb[,i]))),
                             c("Total population", signif(popdhb[i],5)),
                             c("Percent naïve",round(sum(naivedhb[,i])/popdhb[i],3)*100)),
                             bty="n")
  dev.off()
}  

# micks code now to derive attack size etc.
R0 = 12.8;
x0max = 0.3;
Nx0 = 200;
P = (1:Nx0)*x0max/(Nx0+1);
x0 = P/(1-exp(-R0*P));
Rv = R0*x0;

# order alphabetically
dhb_order <- order(colnames(naivedhb))

Naive <- round(colSums(naivedhb)[dhb_order])
Pop   <- popdhb[dhb_order]

RvDHB = R0*Naive/Pop;

library(akima)
PDHB = aspline(x=Rv,y=P,xout=RvDHB);

# The number of cases per DHB
FSDHB = round(PDHB$y*Pop);

# The number of vaccinations short
VDHB = round(Naive-Pop/R0);

Z<-data.frame(Population=Pop,
              Naïve=Naive,
              Outbreak=FSDHB,
              Vaccination=VDHB, stringsAsFactors = FALSE)
# add TOTAL row...
Z <- rbind(Z, colSums(Z))
Z$DHB <- c(names(naivedhb)[dhb_order], "TOTAL")
Z$PC = round(Z$Vaccination / Z$Naïve, 2)
Z <- Z[,c(5,1:4,6)]
write.csv(Z,"tables/dhb_vacc.csv",row.names=F)

# Code to compute "more realistic" outbreak sizes
cases<-read.csv("DHayman_20140627.csv")
#dat<-table(cases$RptYear,cases$DiseaseName)
#length(dat)
#mean(dat[13:18])
#dat<-table(cases$RptYear,cases$DiseaseName)
tab_cases<-table(cases$RptYear,cases$DiseaseName,cases$DHB)
mean_cases<-round(colSums(tab_cases[13:18,,])/5.58,0)
mean_cases["TOTAL"] = sum(mean_cases)
mean_cases=data.frame(DHB=names(mean_cases), Attack=mean_cases, stringsAsFactors = FALSE)

# Generate vacc_predictions.csv
ob_sizes <- read.csv("data/outbreak_size_from_simulations.csv", check.names=FALSE, stringsAsFactors = FALSE)
ob_sizes[ob_sizes$DHB=="TOTAL",2:3] <- colSums(ob_sizes[ob_sizes$DHB != "TOTAL",2:3])
vacc_pred <- Z %>% mutate("Naïve post vaccination" = Naïve - Vaccination) %>%
                   left_join(ob_sizes) %>% left_join(mean_cases) 
# %>%
#                   rename_vars(Proportion=PC, Vacc=Vaccination, Size=Population)
# vacc_pred<-rename(vacc_pred, c(PC="Proportion", Vaccination = "Vacc", Population = "Size"))
vacc_pred <- vacc_pred[,c(1:3,10,5:8)]

write.csv(vacc_pred, "tables/vacc_predictions.csv", row.names=F)

# Shit for costtables*.csv. Ideally these constants would be read in from a file
benefit_cost <- function(vacc_pred, vacc_cost = 50) {
  lost_wages           <- 207155/247
  case_management      <- 330147/187
  gp_costs             <- 20
  lab_costs            <- 0
  contacts_quarantined <- 2.11
  quarantine_length    <- 7.3
  contact_wage         <- 210436/247/5
  prop_hospitalised    <- 0.17
  hosp_costs           <- 1877
  disc_rate            <- 0.03
  disc_years           <- 10
  disc_multiplier      <- 1*(1-1/(1+disc_rate)^disc_years)/(1-1/(1+disc_rate))
  
  bc <- vacc_pred %>% mutate(case_wage_loss = lost_wages * Attack,
                             manage_cost = (case_management + gp_costs + lab_costs) * Attack,
                             hosp_cost = prop_hospitalised * hosp_costs * Attack, 
                             contacts_wage_loss = contacts_quarantined * quarantine_length * contact_wage * Attack)
  bc <- bc %>% mutate(total_discounted_costs = (case_wage_loss+manage_cost+hosp_cost+contacts_wage_loss) * disc_multiplier,
                      vaccine_cost = vacc_cost * Vaccination,
                      future_ob_costs = (lost_wages + case_management + gp_costs + lab_costs + prop_hospitalised*hosp_costs +
                                           contacts_quarantined*quarantine_length*contact_wage) * `Median outbreak` * disc_multiplier)
#   bc <- bc %>% mutate(benefit_cost = total_discounted_costs / (vaccine_cost + future_ob_costs)) %>%
#     select(DHB, Vaccination, vaccine_cost, case_wage_loss, manage_cost, hosp_cost, contacts_wage_loss,
#            total_discounted_costs, outbreak_size_post_vacc = `Median outbreak`, future_ob_costs, benefit_cost)
#   
  bc <- bc %>% mutate(benefit_cost
                      = (ifelse (total_discounted_costs - future_ob_costs 
                                 < 0,0,(total_discounted_costs - future_ob_costs)/ vaccine_cost)),
                      net_present_value = (ifelse (total_discounted_costs - future_ob_costs 
                                                   < 0,0,(total_discounted_costs - future_ob_costs) -vaccine_cost))) %>%
  select(DHB, Vaccination, vaccine_cost, case_wage_loss, manage_cost, hosp_cost, contacts_wage_loss,
           total_discounted_costs, outbreak_size_post_vacc = `Median outbreak`, future_ob_costs, benefit_cost,
         net_present_value)
  
  
  #bc[-nrow(bc),]
}

write.csv(benefit_cost(vacc_pred, 20), "tables/cost_benefit_20.csv", row.names=FALSE)
write.csv(benefit_cost(vacc_pred, 50), "tables/cost_benefit_50.csv", row.names=FALSE)
write.csv(benefit_cost(vacc_pred, 74.53), "tables/cost_benefit_74.csv", row.names=FALSE)

res_sm<-matrix(nrow=length(seq(from=10, to=200, by=10)),ncol=2)

for (i in seq(from=10, to=200, by=10)){
  x<-i
  y<-benefit_cost(vacc_pred, i)[21,11]
  res_sm[i/10,1]<-x
  res_sm[i/10,2]<-y
}  

# plot the b/c ratio v costs per vacc
x<-res_sm[,1]; y<-res_sm[,2]
res_sm_lo <- loess(y~x)
#points(x,y)

pdf(file.path(dhb_fig_path, paste("bc.pdf")))
plot(y=predict(res_sm_lo),x=x, col='red', lwd=2,type="l",
     ylab="benefit/cost ratio",xlab="Cost",xaxt="n")#,ylim=c(0,max(y)))
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
 segments(x0=74.53, y0=1, x1 = 0, y1 = 1)
 segments(x0=74.53, y0=0, x1 = 74.53, y1 = 1)
dev.off()

####  VERSION #2 

vacc_pred_no <- vacc_pred
vacc_pred_no$Attack <- Z$Outbreak
## could make this a function were we add all the variables in, but too lazy for now..
# Shit for costtables*.csv. Ideally these constants would be read in from a file
benefit_cost_no <- function(vacc_pred_no, vacc_cost = 50) {
  lost_wages           <- 207155/247
  case_management      <- 0
  gp_costs             <- 20
  lab_costs            <- 0
  contacts_quarantined <- 0
  quarantine_length    <- 7.3
  contact_wage         <- 0
  prop_hospitalised    <- 0.17
  hosp_costs           <- 1877
  disc_rate            <- 0.03
  disc_years           <- 10
  disc_multiplier      <- 1*(1-1/(1+disc_rate)^disc_years)/(1-1/(1+disc_rate))
  
  bc <- vacc_pred_no %>% mutate(case_wage_loss = lost_wages * Attack,
                             manage_cost = (case_management + gp_costs + lab_costs) * Attack,
                             hosp_cost = prop_hospitalised * hosp_costs * Attack, 
                             contacts_wage_loss = contacts_quarantined * quarantine_length * contact_wage * Attack)
  bc <- bc %>% mutate(total_discounted_costs = (case_wage_loss+manage_cost+hosp_cost+contacts_wage_loss) * disc_multiplier,
                      vaccine_cost = vacc_cost * Vaccination,
                      future_ob_costs = (lost_wages + case_management + gp_costs + lab_costs + prop_hospitalised*hosp_costs +
                                           contacts_quarantined*quarantine_length*contact_wage) * `Median outbreak` * disc_multiplier)
  #   bc <- bc %>% mutate(benefit_cost = total_discounted_costs / (vaccine_cost + future_ob_costs)) %>%
  #     select(DHB, Vaccination, vaccine_cost, case_wage_loss, manage_cost, hosp_cost, contacts_wage_loss,
  #            total_discounted_costs, outbreak_size_post_vacc = `Median outbreak`, future_ob_costs, benefit_cost)
  #   
  bc <- bc %>% mutate(benefit_cost
                      = (ifelse (total_discounted_costs - future_ob_costs 
                                 < 0,0,(total_discounted_costs - future_ob_costs)/ vaccine_cost)),
                      net_present_value = (ifelse (total_discounted_costs - future_ob_costs 
                                                   < 0,0,(total_discounted_costs - future_ob_costs) -vaccine_cost))) %>%
    select(DHB, Vaccination, vaccine_cost, case_wage_loss, manage_cost, hosp_cost, contacts_wage_loss,
           total_discounted_costs, outbreak_size_post_vacc = `Median outbreak`, future_ob_costs, benefit_cost,
           net_present_value)
  
  
  #bc[-nrow(bc),]
}

write.csv(benefit_cost_no(vacc_pred_no, 20), "tables/cost_benefit_no_20.csv", row.names=FALSE)
write.csv(benefit_cost_no(vacc_pred_no, 50), "tables/cost_benefit_no_50.csv", row.names=FALSE)

