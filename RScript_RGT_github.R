
library(lme4)

groups <- c('Group_s')

for (i in 1:length(groups)){
  #browser()
  sink('output.txt', split=TRUE,append = TRUE)
  print(groups[i])
  rate_dispoint = read.csv(paste("rate_dispoint_",groups[i],".csv",sep=""))
  #View(rate_dispoint)
  rate_regret = read.csv(paste("rate_regret_",groups[i],".csv",sep=""))
  #View(rate_regret)
  anticipate_regret = read.csv(paste("anticipate_regret_",groups[i],".csv",sep=""))
  #View(anticipate_regret)
  rate_dispoint.model = lmer(rate1 ~ obt+chance_obt+group+group:obt+group:chance_obt+(1|subject),data=rate_dispoint)
  #summary(rate_dispoint.model)
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
  #DM model
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
  
  #anticipate SSI model: SSI effect
  anticipate_regret_SSI_worst.model=lmer(choice~ E + D + R + SSI_worst + SSI_worst:E + SSI_worst:D + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w_group_n159)
  anticipate_regret_SSI_worst.nullSSIE=lmer(choice~ E + D + R + SSI_worst + SSI_worst:D + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w_group_n159)
  anticipate_regret_SSI_worst.nullSSID=lmer(choice~ E + D + R + SSI_worst + SSI_worst:E + SSI_worst:R + (1|subject), data=anticipate_regret_SSI_w_group_n159)
  anticipate_regret_SSI_worst.nullSSIR=lmer(choice~ E + D + R + SSI_worst + SSI_worst:D + SSI_worst:E + (1|subject), data=anticipate_regret_SSI_w_group_n159)
  results<-anova(anticipate_regret_SSI_worst.nullSSIE,anticipate_regret_SSI_worst.model)
  print(results)
  
  results<-anova(anticipate_regret_SSI_worst.nullSSID,anticipate_regret_SSI_worst.model)
  print(results)
  
  results<-anova(anticipate_regret_SSI_worst.nullSSIR,anticipate_regret_SSI_worst.model)
  print(results)

  sink()
  
  write('                                             ',file='output.txt',append=TRUE)
  write('#############################################',file='output.txt',append=TRUE)
}