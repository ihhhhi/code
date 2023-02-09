
library(lme4)

# Read data
rate_dispoint = read.csv("rate_disp.csv")
#View(rate_dispoint)

rate_regret = read.csv("rate_regret.csv")
#View(rate_regret)

anticipate_regret = read.csv("anticipate_regret.csv")
#View(anticipate_regret)


sink('results_group.txt', split=TRUE,append = TRUE)

  rate_dispoint = read.csv(paste("rate_dispoint_",groups[i],".csv",sep=""))
  #View(rate_dispoint)
  rate_regret = read.csv(paste("rate_regret_",groups[i],".csv",sep=""))
  #View(rate_regret)
  anticipate_regret = read.csv(paste("anticipate_regret_",groups[i],".csv",sep=""))
  #View(anticipate_regret)
  rate_dispoint.model = lmer(rate1 ~ obt+chance_obt+group+group:obt+group:chance_obt+(1|subject),data=rate_dispoint)
  
  #pvals.fnc
  #languageR

  rate_disp.null = lmer(rate1 ~ obt+chance_obt+group:obt+group:chance_obt+(1|subject),data=rate_dispoint)
  results<-anova(rate_dispoint.null,rate_dispoint.model)
  print(results)
  rate_disp.nullint1 = lmer(rate1 ~ obt+chance_obt+group+group:chance_obt+(1|subject),data=rate_dispoint)
  results<-anova(rate_disp.nullint1,rate_disp.model)
  print(results)
  rate_disp.nullint2 = lmer(rate1 ~ obt+chance_obt+group+group:obt+(1|subject),data=rate_dispoint)
  results<-anova(rate_disp.nullint2,rate_disp.model)
  print(results)
summary(rate_disp.model)
  #regret
  rate_regret.model = lmer(rate2 ~ obt+agent_obt+group+group:obt+group:agent_obt+(1|subject),data=rate_regret)
  rate_regret.nullgroup = lmer(rate2 ~ obt+agent_obt+group:obt+group:agent_obt+(1|subject),data=rate_regret)
  results<-anova(rate_regret.nullgroup,rate_regret.model)
  print(results)
  rate_regret.nullint1 = lmer(rate2 ~ obt+agent_obt+group+group:agent_obt+(1|subject),data=rate_regret)
  results<-anova(rate_regret.nullint1,rate_regret.model)
  print(results)
  rate_regret.nullint2 = lmer(rate2 ~ obt+agent_obt+group+group:obt+(1|subject),data=rate_regret)
  results<-anova(rate_regret.nullint2,rate_regret.model)
  print(results)
summary(rate_regret.model)
  #DM model
  ##main effect of E,D,R
anticipate_regret_mainE.model = lmer(choice ~ E + D + R + (1|subject),data=anticipate_regret)
summary(anticipate_regret_mainE.model)
  ##interactions
  anticipate_regret.model = lmer(choice ~ E + D + R + group:E + group:D + group:R + (1|subject),data=anticipate_regret)
  anticipate_regret.nullE = lmer(choice ~ D + R + group:E + group:D+ group:R + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullE,anticipate_regret.model)
  print(results)
  anticipate_regret.nullD = lmer(choice ~ E + R + group:E + group:D + group:R + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullD,anticipate_regret.model)
  print(results)
  anticipate_regret.nullR = lmer(choice ~ E + D + group:E + group:D + group:R + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullR,anticipate_regret.model)
  print(results)
  anticipate_regret.nullEG = lmer(choice ~ E + D + R + group:D + group:R + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullEG,anticipate_regret.model)
  print(results)
  anticipate_regret.nullDG = lmer(choice ~ E + D + R + group:E + group:R + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullDG,anticipate_regret.model)
  print(results)
  anticipate_regret.nullRG = lmer(choice ~ E + D + R + group:E + group:D + (1|subject),data=anticipate_regret)
  results<-anova(anticipate_regret.nullRG,anticipate_regret.model)
  print(results)
  summary(anticipate_regret.model)
# Analyses within ISD 
# Read data
rate_dispoint = read.csv("rate_disp_SSI.csv")
#View(rate_dispoint)

rate_regret = read.csv("rate_regret_SSI.csv")
#View(rate_regret)

anticipate_regret = read.csv("anticipate_regret_SSI.csv")
#View(anticipate_regret)


sink('results_SSI.txt', split=TRUE,append = TRUE)

#disappointment
rate_disp.null = lmer(rate1 ~ obt+chance_obt+SSI19:obt+SSI19:chance_obt+(1|subject),data=rate_dispoint)
results<-anova(rate_dispoint.null,rate_dispoint.model)
#print("Rate_disp:SSI19");
print(results)

rate_disp.null = lmer(rate1 ~ SSI19+obt+chance_obt+SSI19:chance_obt+(1|subject),data=rate_dispoint)
results<-anova(rate_disp.null,rate_disp.model)
print(results)

rate_disp.null = lmer(rate1 ~ SSI19+obt+chance_obt+SSI19:obt+(1|subject),data=rate_dispoint)
results<-anova(rate_disp.null,rate_disp.model)
print(results)
summary(rate_disp.model)

#regret
rate_regret.model = lmer(rate2 ~ SSI19+obt+agent_obt+obt:SSI19+agent_obt:SSI19+(1|subject),data=rate_regret)

rate_regret.null = lmer(rate2 ~ obt+agent_obt+SSI19:obt+SSI19:agent_obt+(1|subject),data=rate_regret)
results<-anova(rate_regret.null,rate_regret.model)
print(results)

rate_regret.null = lmer(rate2 ~ SSI19+obt+agent_obt+SSI19:agent_obt+(1|subject),data=rate_regret)
results<-anova(rate_regret.null,rate_regret.model)
print(results)

rate_regret.null = lmer(rate2 ~ SSI19+obt+agent_obt+SSI19:obt+(1|subject),data=rate_regret)
results<-anova(rate_regret.null,rate_regret.model)
print(results)
summary(rate_regret.model)

  #anticipate SSI model: SSI effect
  anticipate_regret_SSI_worst.model=lmer(choice~ E + D + R + SSI_worst + SSI_worst:E + SSI_worst:D + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w)
  anticipate_regret_SSI_worst.nullSSIE=lmer(choice~ E + D + R + SSI_worst + SSI_worst:D + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w)
  anticipate_regret_SSI_worst.nullSSID=lmer(choice~ E + D + R + SSI_worst + SSI_worst:E + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w)
  anticipate_regret_SSI_worst.nullSSIR=lmer(choice~ E + D + R + SSI_worst + SSI_worst:D + SSI_worst:E + (1|subject), data=anticipate_regret_SSI_w)
  results<-anova(anticipate_regret_SSI_worst.nullSSIE,anticipate_regret_SSI_worst.model)
  print(results)
  
  results<-anova(anticipate_regret_SSI_worst.nullSSID,anticipate_regret_SSI_worst.model)
  print(results)
  
  results<-anova(anticipate_regret_SSI_worst.nullSSIR,anticipate_regret_SSI_worst.model)
  print(results)

 summary(anticipate_regret_SSI_worst.model)
  
#AIC criteria for the best chice model
anticipate_group_full_0209.nullgroupED<- lmer(choice ~ E + D + R + group:R + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209.nullgroupDR<- lmer(choice ~ E + D + R + group:E + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209.nullgroupER<- lmer(choice ~ E + D + R + group:D + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209.nullgroupR<- lmer(choice ~ E + D + R + group:E + group:D + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209.nullgroupE<- lmer(choice ~ E + D + R + group:D + group:R + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209.nullgroupD<- lmer(choice ~ E + D + R + group:E + group:R + (1 | subject), data=anticipate_regret_SSI_w)
anticipate_group_full_0209 <- lmer(choice ~ E + D + R + group:E + group:D + group:R + (1 | subject), data=anticipate_regret_SSI_w)

results<- anova(anticipate_group_full_0209,anticipate_group_full_0209.nullgroupR,anticipate_group_full_0209.nullgroupE,anticipate_group_full_0209.nullgroupD,anticipate_group_full_0209.nullgroupED,anticipate_group_full_0209.nullgroupDR,anticipate_group_full_0209.nullgroupER)}
print(results)

#check the best model estimates
summary(anticipate_group_full_0209.nullgroupE)


