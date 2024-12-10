
#### Sensitivity Analysis

### The numbers used in this section are arbitrary and just assume four hunting percentages, two above and two below the benchmark level. They are even spread meaning that 1 level below is the same magnitude lower as 1 level above. For example, the adult male hunting percentage benchmark is 10%. One level below is 5% and one level above is 15%. 


require(tidyverse)
require(RSQLite)
require(DBI)
require(popbio)
require(openxlsx)

# LOAD IN THE FUNCTIONS
source("CWD_SCENARIO_SCRIPT.R")

# CONNECT TO DATABASE
#dbConn<- dbConnect(SQLite(), "ElkCWD_Tables_SENS.sqlite")

# FOR THE LOOPS LATER TO ORGANIZE 
Juv = c(1:2)
Adult = c(3:13) 
Old = c(14:18)
ages<-list(Juv=Juv,Adult=Adult,Old=Old)


## ADULT MALE SENS

# SCENARIO 2
SCEN2_AD_MALE_SENS_0     <-calcHealthy(hunt.mort.ad.m=0,H=1,P=0)
SCEN2_AD_MALE_SENS_0.05  <-calcHealthy(hunt.mort.ad.m=0.05,H=1,P=0)
SCEN2_AD_MALE_SENS_0.1   <-calcHealthy(hunt.mort.ad.m=0.1,H=1,P=0)
SCEN2_AD_MALE_SENS_0.15  <-calcHealthy(hunt.mort.ad.m=0.15,H=1,P=0)
SCEN2_AD_MALE_SENS_0.2   <-calcHealthy(hunt.mort.ad.m=0.2,H=1,P=0)
SCEN2_AD_MALE_SENS       <-list(SENS_0=SCEN2_AD_MALE_SENS_0,
                                SENS_0.05=SCEN2_AD_MALE_SENS_0.05,
                                SENS_0.1=SCEN2_AD_MALE_SENS_0.1,
                                SENS_0.15=SCEN2_AD_MALE_SENS_0.15,
                                SENS_0.2=SCEN2_AD_MALE_SENS_0.2)

# SCENARIO 4
SCEN4_AD_MALE_SENS_0     <-calcInfection(hunt.mort.ad.m=0,H=1,P=0)
SCEN4_AD_MALE_SENS_0.05  <-calcInfection(hunt.mort.ad.m=0.05,H=1,P=0)
SCEN4_AD_MALE_SENS_0.1   <-calcInfection(hunt.mort.ad.m=0.1,H=1,P=0)
SCEN4_AD_MALE_SENS_0.15  <-calcInfection(hunt.mort.ad.m=0.15,H=1,P=0)
SCEN4_AD_MALE_SENS_0.2   <-calcInfection(hunt.mort.ad.m=0.2,H=1,P=0)
SCEN4_AD_MALE_SENS       <-list(SENS_0=SCEN4_AD_MALE_SENS_0,
                                SENS_0.05=SCEN4_AD_MALE_SENS_0.05,
                                SENS_0.1=SCEN4_AD_MALE_SENS_0.1,
                                SENS_0.15=SCEN4_AD_MALE_SENS_0.15,
                                SENS_0.2=SCEN4_AD_MALE_SENS_0.2)

# SCEANRIO 6
SCEN6_AD_MALE_SENS_0     <-calcHealthy(hunt.mort.ad.m=0,H=1,P=1)
SCEN6_AD_MALE_SENS_0.05  <-calcHealthy(hunt.mort.ad.m=0.05,H=1,P=1)
SCEN6_AD_MALE_SENS_0.1   <-calcHealthy(hunt.mort.ad.m=0.1,H=1,P=1)
SCEN6_AD_MALE_SENS_0.15  <-calcHealthy(hunt.mort.ad.m=0.15,H=1,P=1)
SCEN6_AD_MALE_SENS_0.2   <-calcHealthy(hunt.mort.ad.m=0.2,H=1,P=1)
SCEN6_AD_MALE_SENS       <-list(SENS_0=SCEN6_AD_MALE_SENS_0,
                                SENS_0.05=SCEN6_AD_MALE_SENS_0.05,
                                SENS_0.1=SCEN6_AD_MALE_SENS_0.1,
                                SENS_0.15=SCEN6_AD_MALE_SENS_0.15,
                                SENS_0.2=SCEN6_AD_MALE_SENS_0.2)

# SCNEARIO 8
SCEN8_AD_MALE_SENS_0     <-calcInfection(hunt.mort.ad.m=0,H=1,P=1)
SCEN8_AD_MALE_SENS_0.05  <-calcInfection(hunt.mort.ad.m=0.05,H=1,P=1)
SCEN8_AD_MALE_SENS_0.1   <-calcInfection(hunt.mort.ad.m=0.1,H=1,P=1)
SCEN8_AD_MALE_SENS_0.15  <-calcInfection(hunt.mort.ad.m=0.15,H=1,P=1)
SCEN8_AD_MALE_SENS_0.2   <-calcInfection(hunt.mort.ad.m=0.2,H=1,P=1)
SCEN8_AD_MALE_SENS       <-list(SENS_0=SCEN8_AD_MALE_SENS_0,
                                SENS_0.05=SCEN8_AD_MALE_SENS_0.05,
                                SENS_0.1=SCEN8_AD_MALE_SENS_0.1,
                                SENS_0.15=SCEN8_AD_MALE_SENS_0.15,
                                SENS_0.2=SCEN8_AD_MALE_SENS_0.2)


# COMBINING ALL SCENARIOS INTO ONE LIST
AD_MALE_SENS<-list(SCEN2_SENS=SCEN2_AD_MALE_SENS,SCEN4_SENS=SCEN4_AD_MALE_SENS,SCEN6_SENS=SCEN6_AD_MALE_SENS,
                   SCEN8_SENS=SCEN8_AD_MALE_SENS)
length(AD_MALE_SENS[[1]][[1]])
length(AD_MALE_SENS[[1]][[1]])

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
AD_MALE_table<-data.frame()
for(k in 1:length(AD_MALE_SENS)){
  for (j in 1:length(AD_MALE_SENS[[k]])){
    for (w in 1:length(AD_MALE_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        AD_MALE_table<- rbind(AD_MALE_table, data.frame(Scenario = names(AD_MALE_SENS)[k],
                                                        Hunt_perc = names(AD_MALE_SENS[[k]])[j],
                                                        Category = names(AD_MALE_SENS[[k]][[j]])[w],
                                                        Age = names(ages)[i],
                                                        Month = 1:ncol(AD_MALE_SENS[[k]][[j]][[w]]),
                                                        Count = sapply(1:ncol(AD_MALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(AD_MALE_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}

AD_MALE_table<- AD_MALE_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                  Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                  Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                  Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                  Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                  Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                  Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                  Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                  Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                  Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

AD_MALE_table<-AD_MALE_table %>% pivot_wider(names_from = Category, values_from = Count) %>% mutate(Totalpreyi = TotalSprey+TotalIprey,
 Totalpreyh = FpreyA+MpreyA)

AD_MALE_table<-AD_MALE_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")

AD_MALE_table1<- AD_MALE_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(AD_MALE_table1))){
  Total_pop = AD_MALE_table1$Juv+AD_MALE_table1$Adult+AD_MALE_table1$Old
}
AD_MALE_table1<- add_column(AD_MALE_table1,Total_pop,.after = "Old")
AD_MALE_table1<-AD_MALE_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") 


# POPULATON DIFFERENCE TABLE
POP_DIFF_AM <- AD_MALE_table1 %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
POP_DIFF_AM <- POP_DIFF_AM %>% 
  mutate(SN0 = POP_DIFF_AM$S_0-POP_DIFF_AM$S_0.1,
         SN05 =POP_DIFF_AM$S_0.05-POP_DIFF_AM$S_0.1,
         SN1 = POP_DIFF_AM$S_0.1-POP_DIFF_AM$S_0.1,
         SN15 = POP_DIFF_AM$S_0.15-POP_DIFF_AM$S_0.1,
         SN20 = POP_DIFF_AM$S_0.2-POP_DIFF_AM$S_0.1) %>% 
  pivot_longer(., cols = starts_with("SN"), names_to = "Sensitivity_Level", values_to = "POP_DIFF")

# PREVELANCE TABLE
PREV_DIFF_AM <- AD_MALE_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_DIFF_AM <- PREV_DIFF_AM %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","char","Hunt_perc", "Age","Month","Year"),names_to = "Category", values_to= "Count")


# ELASTICITY TABLE to population 
elasticity_AM <-AD_MALE_table1 %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
elasticity_AM <-elasticity_AM %>% 
  mutate(Elast0   = ((elasticity_AM$S_0-elasticity_AM$S_0.1)/elasticity_AM$S_0.1)/((0-0.1)/0.1),
         Elast05 = ((elasticity_AM$S_0.05-elasticity_AM$S_0.1)/elasticity_AM$S_0.1)/((0.05-0.1)/0.1),
         Elast1   = rep(0,nrow(elasticity_AM)),
         Elast15 = ((elasticity_AM$S_0.15-elasticity_AM$S_0.1)/elasticity_AM$S_0.1)/((.15-0.1)/0.1),
         Elast20   = ((elasticity_AM$S_0.2-elasticity_AM$S_0.1)/elasticity_AM$S_0.1)/((0.2-0.1)/0.1)) %>% 
  pivot_longer(., cols = starts_with("E"), names_to = "Elasticity_Sensitivity_Level", values_to = "ELASTICITY")

# CUMMULATIVE ELAST CWD

sens_name<-c("SCEN4_SENS","SCEN8_SENS")
cummul_cwd_AM_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_AM <- AD_MALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AM <- cummul_AM %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_cwd_AM <-cummul_AM %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
  cummul_cwd_AM_table<-rbind(cummul_cwd_AM_table, data.frame(cummul_cwd_AM %>% select(Scenario,S_0,S_0.05,S_0.1,S_0.15,S_0.2)))
}

cummul_cwd_AM_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_cwd_AM_elast_1  <-cummul_cwd_AM_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
  mutate(Cummul_cwd0   = ((S_0-S_0.1)/S_0.1)/((0-0.1)/0.1),
         Cummul_cwd05 = ((S_0.05-S_0.1)/S_0.1)/((0.05-0.1)/0.1),
         Cummul_cwd1   = rep(0,nrow(cummul_cwd_AM)),
         Cummul_cwd15 = ((S_0.15-S_0.1)/S_0.1)/((.15-0.1)/0.1),
         Cummul_cwd2   = ((S_0.2-S_0.1)/S_0.1)/((0.2-0.1)/0.1)) 
cummul_cwd_AM_elast<-rbind(cummul_cwd_AM_elast, data.frame(Scenario=sens_name[i],cummul_cwd_AM_elast_1))

}
cummul_cwd_AM_elast <- cummul_cwd_AM_elast %>% 
  pivot_longer(., cols = c("Cummul_cwd0","Cummul_cwd05","Cummul_cwd1","Cummul_cwd15","Cummul_cwd2"), names_to = "Cummulative_cwd", values_to = "Cummul_cwd_Elast")
stargazer(t(cummul_cwd_AM_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_cwd.tex")

# CUMMULATIVE ELAST HUNTING
sens_name<-c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS")
cummul_hunt_AM_table<-data.frame()
for (i in 1:length(sens_name)){
  
  cummul_AM <- AD_MALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AM <- cummul_AM %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  mod<-vector()
  mod[cummul_AM$Month%%12 == 7] <- 1 
  cummul_AM <-cummul_AM %>% add_column(.,mod,.after = "Month")
  cummul_AM <- filter(cummul_AM, mod > 0) 
  
  cummul_hunt_AM <-cummul_AM %>% filter(Month %in% c("235"), Category %in% c("cummul_hunt"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Hunt_perc, values_from = Count)
  
  cummul_hunt_AM_table<-rbind(cummul_hunt_AM_table, data.frame(cummul_hunt_AM %>% select(Scenario,S_0,S_0.05,S_0.1,S_0.15,S_0.2)))
}
cummul_hunt_AM_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_hunt_AM_elast_1  <-cummul_hunt_AM_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
  mutate(Cummul_hunt0   = ((S_0-S_0.1)/S_0.1)/((0-0.1)/0.1),
         Cummul_hunt05 = ((S_0.05-S_0.1)/S_0.1)/((0.05-0.1)/0.1),
         Cummul_hunt1   = rep(0,nrow=1),
         Cummul_hunt15 = ((S_0.15-S_0.1)/S_0.1)/((.15-0.1)/0.1),
         Cummul_hunt2   = ((S_0.2-S_0.1)/S_0.1)/((0.2-0.1)/0.1))
  cummul_hunt_AM_elast<-rbind(cummul_hunt_AM_elast, data.frame(Scenario=sens_name[i],cummul_hunt_AM_elast_1))

}
cummul_hunt_AM_elast <- cummul_hunt_AM_elast %>% 
  pivot_longer(., cols = c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2"), names_to = "Cummulative_hunt", values_to = "Cummul_hunt_Elast")
stargazer(t(cummul_hunt_AM_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_hunt.tex")

# CUMMULATIVE ELAST PRED

sens_name<-c("SCEN6_SENS","SCEN8_SENS")
cummul_pred_AM_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_AM <- AD_MALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AM <- cummul_AM %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_pred_AM <-cummul_AM %>% filter(Year %in% c("20"), Category %in% c("cummul_pred"), Age %in% c("Total_pop")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
  cummul_pred_AM_table<-rbind(cummul_pred_AM_table, data.frame(cummul_pred_AM %>% select(Scenario,S_0,S_0.05,S_0.1,S_0.15,S_0.2)))
}
cummul_pred_AM_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_pred_AM_elast_1  <-cummul_pred_AM_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
  mutate(Cummul_pred0   = ((S_0-S_0.1)/S_0.1)/((0-0.1)/0.1),
         Cummul_pred05 = ((S_0.05-S_0.1)/S_0.1)/((0.05-0.1)/0.1),
         Cummul_pred1   = rep(0,nrow(cummul_pred_AM)),
         Cummul_pred15 = ((S_0.15-S_0.1)/S_0.1)/((.15-0.1)/0.1),
         Cummul_pred2   = ((S_0.2-S_0.1)/S_0.1)/((0.2-0.1)/0.1))
cummul_pred_AM_elast<-rbind(cummul_pred_AM_elast, data.frame(Scenario=sens_name[i],cummul_pred_AM_elast_1))
}

cummul_pred_AM_elast <- cummul_pred_AM_elast %>% 
  pivot_longer(., cols = c("Cummul_pred0","Cummul_pred05","Cummul_pred1","Cummul_pred15","Cummul_pred2"), names_to = "Cummulative_pred", values_to = "Cummul_pred_Elast")


stargazer(t(cummul_pred_AM_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_pred.tex")

# ELASTICITY to prevelance
prev_AM <-PREV_DIFF_AM  %>% filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"), Category %in% c("PREV")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
prev_AM <-prev_AM %>% 
  mutate(Prev0   = ((prev_AM$S_0-prev_AM$S_0.1)/prev_AM$S_0.1)/((0-0.1)/0.1),
         Prev05 = ((prev_AM$S_0.05-prev_AM$S_0.1)/prev_AM$S_0.1)/((0.05-0.1)/0.1),
         Prev1   = rep(0,nrow(prev_AM)),
         Prev15 = ((prev_AM$S_0.15-prev_AM$S_0.1)/prev_AM$S_0.1)/((.15-0.1)/0.1),
         Prev20   = ((prev_AM$S_0.2-prev_AM$S_0.1)/prev_AM$S_0.1)/((0.2-0.1)/0.1)) %>% 
  pivot_longer(., cols = starts_with("P"), names_to = "Prevalence", values_to = "PREV_Elast")

# elasticity to cwd

cwd_AM <-AD_MALE_table1  %>% filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"), Category %in% c("TotalCWDdeath")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
cwd_AM <-cwd_AM %>% 
  mutate(e_cwd0   = ((cwd_AM$S_0-cwd_AM$S_0.1)/cwd_AM$S_0.1)/((0-0.1)/0.1),
         e_cwd05 = ((cwd_AM$S_0.05-cwd_AM$S_0.1)/cwd_AM$S_0.1)/((0.05-0.1)/0.1),
         e_cwd1   = rep(0,nrow(cwd_AM)),
         e_cwd15 = ((cwd_AM$S_0.15-cwd_AM$S_0.1)/cwd_AM$S_0.1)/((.15-0.1)/0.1),
         e_cwd20   = ((cwd_AM$S_0.2-cwd_AM$S_0.1)/cwd_AM$S_0.1)/((0.2-0.1)/0.1)) %>% 
  pivot_longer(., cols = starts_with("e_c"), names_to = "CWD", values_to = "CWD_Elast")

## ADULT FEMALE SENS

# SCENARIO 2
SCEN2_AD_FEMALE_SENS_0      <-calcHealthy(hunt.mort.ad.f=0,H=1,P=0)
SCEN2_AD_FEMALE_SENS_0.025  <-calcHealthy(hunt.mort.ad.f=0.025,H=1,P=0)
SCEN2_AD_FEMALE_SENS_0.05   <-calcHealthy(hunt.mort.ad.f=0.05,H=1,P=0)
SCEN2_AD_FEMALE_SENS_0.075  <-calcHealthy(hunt.mort.ad.f=0.075,H=1,P=0)
SCEN2_AD_FEMALE_SENS_0.1    <-calcHealthy(hunt.mort.ad.f=0.1,H=1,P=0)
SCEN2_AD_FEMALE_SENS       <-list(SENS_0     = SCEN2_AD_FEMALE_SENS_0,
                                  SENS_0.025 = SCEN2_AD_FEMALE_SENS_0.025,
                                  SENS_0.05  = SCEN2_AD_FEMALE_SENS_0.05,
                                  SENS_0.075 = SCEN2_AD_FEMALE_SENS_0.075,
                                  SENS_0.1   = SCEN2_AD_FEMALE_SENS_0.1)

# SCENARIO 4
SCEN4_AD_FEMALE_SENS_0      <-calcInfection(hunt.mort.ad.f=0,H=1,P=0)
SCEN4_AD_FEMALE_SENS_0.025  <-calcInfection(hunt.mort.ad.f=0.025,H=1,P=0)
SCEN4_AD_FEMALE_SENS_0.05   <-calcInfection(hunt.mort.ad.f=0.05,H=1,P=0)
SCEN4_AD_FEMALE_SENS_0.075  <-calcInfection(hunt.mort.ad.f=0.075,H=1,P=0)
SCEN4_AD_FEMALE_SENS_0.1    <-calcInfection(hunt.mort.ad.f=0.1,H=1,P=0)
SCEN4_AD_FEMALE_SENS       <-list(SENS_0     = SCEN4_AD_FEMALE_SENS_0,
                                  SENS_0.025 = SCEN4_AD_FEMALE_SENS_0.025,
                                  SENS_0.05  = SCEN4_AD_FEMALE_SENS_0.05,
                                  SENS_0.075 = SCEN4_AD_FEMALE_SENS_0.075,
                                  SENS_0.1   = SCEN4_AD_FEMALE_SENS_0.1)

# SCEANRIO 6
SCEN6_AD_FEMALE_SENS_0     <-calcHealthy(hunt.mort.ad.f=0,H=1,P=1)
SCEN6_AD_FEMALE_SENS_0.025  <-calcHealthy(hunt.mort.ad.f=0.025,H=1,P=1)
SCEN6_AD_FEMALE_SENS_0.05   <-calcHealthy(hunt.mort.ad.f=0.05,H=1,P=1)
SCEN6_AD_FEMALE_SENS_0.075  <-calcHealthy(hunt.mort.ad.f=0.075,H=1,P=1)
SCEN6_AD_FEMALE_SENS_0.1   <-calcHealthy(hunt.mort.ad.f=0.1,H=1,P=1)
SCEN6_AD_FEMALE_SENS       <-list(SENS_0     = SCEN6_AD_FEMALE_SENS_0,
                                  SENS_0.025 = SCEN6_AD_FEMALE_SENS_0.025,
                                  SENS_0.05  = SCEN6_AD_FEMALE_SENS_0.05,
                                  SENS_0.075 = SCEN6_AD_FEMALE_SENS_0.075,
                                  SENS_0.1   = SCEN6_AD_FEMALE_SENS_0.1)

# SCNEARIO 8
SCEN8_AD_FEMALE_SENS_0      <-calcInfection(hunt.mort.ad.f=0,H=1,P=1)
SCEN8_AD_FEMALE_SENS_0.025  <-calcInfection(hunt.mort.ad.f=0.025,H=1,P=1)
SCEN8_AD_FEMALE_SENS_0.05   <-calcInfection(hunt.mort.ad.f=0.05,H=1,P=1)
SCEN8_AD_FEMALE_SENS_0.075  <-calcInfection(hunt.mort.ad.f=0.075,H=1,P=1)
SCEN8_AD_FEMALE_SENS_0.1    <-calcInfection(hunt.mort.ad.f=0.1,H=1,P=1)
SCEN8_AD_FEMALE_SENS       <-list(SENS_0     = SCEN8_AD_FEMALE_SENS_0,
                                  SENS_0.025 = SCEN8_AD_FEMALE_SENS_0.025,
                                  SENS_0.05  = SCEN8_AD_FEMALE_SENS_0.05,
                                  SENS_0.075 = SCEN8_AD_FEMALE_SENS_0.075,
                                  SENS_0.1   = SCEN8_AD_FEMALE_SENS_0.1)

# COMBINING ALL SCENARIOS INTO ONE LIST
AD_FEMALE_SENS<-list(SCEN2_SENS=SCEN2_AD_FEMALE_SENS,SCEN4_SENS=SCEN4_AD_FEMALE_SENS,SCEN6_SENS=SCEN6_AD_FEMALE_SENS,
                     SCEN8_SENS=SCEN8_AD_FEMALE_SENS)

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
AD_FEMALE_table<-data.frame()
for(k in 1:length(AD_FEMALE_SENS)){
  for (j in 1:length(AD_FEMALE_SENS[[k]])){
    for (w in 1:length(AD_FEMALE_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        AD_FEMALE_table<- rbind(AD_FEMALE_table, data.frame(Scenario = names(AD_FEMALE_SENS)[k],
                                                            Hunt_perc = names(AD_FEMALE_SENS[[k]])[j],
                                                            Category = names(AD_FEMALE_SENS[[k]][[j]])[w],
                                                            Age = names(ages)[i],
                                                            Month = 1:ncol(AD_FEMALE_SENS[[k]][[j]][[w]]),
                                                            Count = sapply(1:ncol(AD_FEMALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(AD_FEMALE_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}

AD_FEMALE_table<- AD_FEMALE_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                    Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                    Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                    Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                    Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                    Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                    Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                    Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                    Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                    Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

AD_FEMALE_table<-AD_FEMALE_table %>% pivot_wider(names_from = Category, values_from = Count) %>% mutate(Totalpreyi = TotalSprey+TotalIprey,Totalpreyh = FpreyA+MpreyA)

AD_FEMALE_table<-AD_FEMALE_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")

AD_FEMALE_table1<- AD_FEMALE_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(AD_FEMALE_table1))){
  Total_pop = AD_FEMALE_table1$Juv+AD_FEMALE_table1$Adult+AD_FEMALE_table1$Old
}
AD_FEMALE_table1<- add_column(AD_FEMALE_table1,Total_pop,.after = "Adult")
AD_FEMALE_table1<-AD_FEMALE_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") 

# POPULATION DIFFERENCE TABLE
POP_DIFF_AF <- AD_FEMALE_table1 %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
POP_DIFF_AF <- POP_DIFF_AF %>% 
  mutate(SN0 = POP_DIFF_AF$S_0-POP_DIFF_AF$S_0.05,
         SN025 =POP_DIFF_AF$S_0.025-POP_DIFF_AF$S_0.05,
         SN050 = POP_DIFF_AF$S_0.05-POP_DIFF_AF$S_0.05,
         SN075 = POP_DIFF_AF$S_0.075-POP_DIFF_AF$S_0.05,
         SN10 = POP_DIFF_AF$S_0.1-POP_DIFF_AF$S_0.05) %>% 
  pivot_longer(., cols = starts_with("SN"), names_to = "Sensitivity_Level", values_to = "POP_DIFF")

# PREVELANCE TABLE
PREV_DIFF_AF <- AD_FEMALE_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_DIFF_AF <- PREV_DIFF_AF %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","char","Hunt_perc", "Age","Month","Year"),names_to = "Category", values_to= "Count")

# ELASTICITY TABLE
elasticity_AF <-AD_FEMALE_table1 %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
elasticity_AF <-elasticity_AF %>% 
  mutate(Elast0   = ((elasticity_AF$S_0-elasticity_AF$S_0.05)/elasticity_AF$S_0.05)/((0-0.05)/0.05),
         Elast025 = ((elasticity_AF$S_0.025-elasticity_AF$S_0.05)/elasticity_AF$S_0.05)/((0.025-0.05)/0.05),
         Elast05   = rep(0,nrow(elasticity_AF)),
         Elast075 = ((elasticity_AF$S_0.075-elasticity_AF$S_0.05)/elasticity_AF$S_0.05)/((0.075-0.05)/0.05),
         Elast10   = ((elasticity_AF$S_0.1-elasticity_AF$S_0.05)/elasticity_AF$S_0.05)/((0.1-0.05)/0.05)) %>% 
  pivot_longer(., cols = starts_with("E"), names_to = "Elasticity_Sensitivity_Level", values_to = "ELASTICITY")


# CUMMULATIVE ELAST TO HUNT
sens_name<-c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS")
cummul_hunt_AF_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_AF <- AD_FEMALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AF <- cummul_AF %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  mod<-vector()
  mod[cummul_AF$Month%%12 == 7] <- 1 
  cummul_AF <-cummul_AF %>% add_column(.,mod,.after = "Month")
  cummul_AF <- filter(cummul_AF, mod > 0) 
  
  cummul_hunt_AF <-cummul_AF %>% filter(Month %in% c("235"), Category %in% c("cummul_hunt"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Hunt_perc, values_from = Count)
  cummul_hunt_AF_table<-rbind(cummul_hunt_AF_table, data.frame(cummul_hunt_AF %>% select(Scenario,S_0,S_0.025,S_0.05,S_0.075,S_0.1)))
}

cummul_hunt_AF_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_hunt_AF_elast_1  <-cummul_hunt_AF_table %>% filter(Scenario %in% c(sens_name[i])) %>%
  mutate(Cummul_hunt0   = ((S_0-S_0.05)/S_0.05)/((0-0.05)/0.05),
         Cummul_hunt025 = ((S_0.025-S_0.05)/S_0.05)/((0.025-0.05)/0.05),
         Cummul_hunt05   = rep(0,nrow=1),
         Cummul_hunt075 = ((S_0.075-S_0.05)/S_0.05)/((.075-0.05)/0.05),
         Cummul_hunt1   = ((S_0.1-S_0.05)/S_0.05)/((0.1-0.05)/0.05))
cummul_hunt_AF_elast<-rbind(cummul_hunt_AF_elast, data.frame(Scenario=sens_name[i],cummul_hunt_AF_elast_1))
}

cummul_hunt_AF_elast  <-cummul_hunt_AF_elast  %>% 
  pivot_longer(., cols = c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1"), names_to = "Cummulative_hunt", values_to = "Cummul_hunt_Elast")
stargazer(t(cummul_hunt_AF_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_hunt.tex")

# CUMMULATIVE ELAST TO CWD
sens_name<-c("SCEN4_SENS","SCEN8_SENS")
cummul_cwd_AF_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_AF <- AD_FEMALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AF <- cummul_AF %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_cwd_AF <-cummul_AF %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Hunt_perc, values_from = Count)
  cummul_cwd_AF_table<-rbind(cummul_cwd_AF_table, data.frame(cummul_cwd_AF %>% select(Scenario,S_0,S_0.025,S_0.05,S_0.075,S_0.1)))
}

cummul_cwd_AF_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_cwd_AF_elast_1 <-cummul_cwd_AF_table %>% filter(Scenario %in% c(sens_name[i])) %>%
  mutate(Cummul_cwd0   = ((S_0-S_0.05)/S_0.05)/((0-0.05)/0.05),
         Cummul_cwd025 = ((S_0.025-S_0.05)/S_0.05)/((0.025-0.05)/0.05),
         Cummul_cwd05   = rep(0,nrow(cummul_cwd_AF)),
         Cummul_cwd075 = ((S_0.075-S_0.05)/S_0.05)/((.075-0.05)/0.05),
         Cummul_cwd1   = ((S_0.1-S_0.05)/S_0.05)/((0.1-0.05)/0.05))
cummul_cwd_AF_elast<-rbind(cummul_cwd_AF_elast, data.frame(Scenario=sens_name[i],cummul_cwd_AF_elast_1))
}
cummul_cwd_AF_elast<- cummul_cwd_AF_elast %>% 
  pivot_longer(., cols = c("Cummul_cwd0","Cummul_cwd025","Cummul_cwd05","Cummul_cwd075","Cummul_cwd1"), names_to = "Cummulative_cwd", values_to = "Cummul_cwd_Elast")

stargazer(t(cummul_cwd_AF_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_cwd.tex")

# CUMMULATIVE ELAST TO pred death
sens_name<-c("SCEN6_SENS","SCEN8_SENS")
cummul_pred_AF_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_AF <- AD_FEMALE_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_AF <- cummul_AF %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_pred_AF <-cummul_AF %>% filter(Year %in% c("20"), Category %in% c("cummul_pred"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Hunt_perc, values_from = Count)
  cummul_pred_AF_table<-rbind(cummul_pred_AF_table, data.frame(cummul_pred_AF %>% select(Scenario,S_0,S_0.025,S_0.05,S_0.075,S_0.1)))
}

cummul_pred_AF_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_pred_AF_elast_1 <-cummul_pred_AF_table %>% filter(Scenario %in% c(sens_name[i])) %>%
  mutate(Cummul_pred0   = ((S_0-S_0.05)/S_0.05)/((0-0.05)/0.05),
         Cummul_pred025 = ((S_0.025-S_0.05)/S_0.05)/((0.025-0.05)/0.05),
         Cummul_pred05   = rep(0,nrow(cummul_pred_AF)),
         Cummul_pred075 = ((S_0.075-S_0.05)/S_0.05)/((.075-0.05)/0.05),
         Cummul_pred1   = ((S_0.1-S_0.05)/S_0.05)/((0.1-0.05)/0.05))
cummul_pred_AF_elast<-rbind(cummul_pred_AF_elast, data.frame(Scenario=sens_name[i],cummul_pred_AF_elast_1))
}
cummul_pred_AF_elast<- cummul_pred_AF_elast %>% 
  pivot_longer(., cols = c("Cummul_pred0","Cummul_pred025","Cummul_pred05","Cummul_pred075","Cummul_pred1"), names_to = "Cummulative_pred", values_to = "Cummul_pred_Elast")

stargazer(t(cummul_pred_AF_elast),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_pred.tex")

# elasticity to cwd
cwd_AF <-AD_FEMALE_table1 %>% filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"), Category %in% c("TotalCWDdeath")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
cwd_AF <-cwd_AF %>% 
  mutate(e_cwd0   = ((cwd_AF$S_0-cwd_AF$S_0.05)/cwd_AF$S_0.05)/((0-0.05)/0.05),
         e_cwd025 = ((cwd_AF$S_0.025-cwd_AF$S_0.05)/cwd_AF$S_0.05)/((0.025-0.05)/0.05),
         e_cwd05   = rep(0,nrow(cwd_AF)),
         e_cwd075 = ((cwd_AF$S_0.075-cwd_AF$S_0.05)/cwd_AF$S_0.05)/((0.075-0.05)/0.05),
         e_cwd10   = ((cwd_AF$S_0.1-cwd_AF$S_0.05)/cwd_AF$S_0.05)/((0.1-0.05)/0.05)) %>% 
  pivot_longer(., cols = starts_with("e_c"), names_to = "CWD", values_to = "CWD_Elast")

# prevalence elasticity
prev_AF <-PREV_DIFF_AF %>% filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"), Category %in% c("PREV")) %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
prev_AF <-prev_AF %>% 
  mutate(Prev0   = ((prev_AF$S_0-prev_AF$S_0.05)/prev_AF$S_0.05)/((0-0.05)/0.05),
         Prev025 = ((prev_AF$S_0.025-prev_AF$S_0.05)/prev_AF$S_0.05)/((0.025-0.05)/0.05),
         Prev05   = rep(0,nrow(prev_AF)),
         Prev075 = ((prev_AF$S_0.075-prev_AF$S_0.05)/prev_AF$S_0.05)/((0.075-0.05)/0.05),
         Prev10   = ((prev_AF$S_0.1-prev_AF$S_0.05)/prev_AF$S_0.05)/((0.1-0.05)/0.05)) %>% 
  pivot_longer(., cols = starts_with("P"), names_to = "Prevalence", values_to = "PREV_Elast")

list_of_sens_tables<-list("POP_DIFF_AM"=POP_DIFF_AM,"POP_DIFF_AF"=POP_DIFF_AF,"PREV_DIFF_AM"=PREV_DIFF_AM,"PREV_DIFF_AF"=PREV_DIFF_AF,
                          "elasticity_AM"=elasticity_AM,"elasticity_AF"=elasticity_AF,"cwd_AM"=cwd_AM,"cwd_AF"=cwd_AF,"prev_AM"=prev_AM,
                          "prev_AF"=prev_AF,"cummul_pred_AM_elast"=cummul_pred_AM_elast,"cummul_cwd_AM_elast"=cummul_cwd_AM_elast,
                          "cummul_hunt_AM_elast"=cummul_hunt_AM_elast,"cummul_pred_AF_elast"=cummul_pred_AF_elast,
                          "cummul_cwd_AF_elast"=cummul_cwd_AF_elast,"cummul_hunt_AF_elast"=cummul_hunt_AF_elast)
write.xlsx(list_of_sens_tables,"elasticities_and_differences_M_F.xlsx")

## JUV MALE SENS

# SCENARIO 4
SCEN4_JUV_MALE_SENS_0      <-calcInfection(hunt.mort.ad.m=0,H=1,P=0)
SCEN4_JUV_MALE_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=0)
SCEN4_JUV_MALE_SENS_0.03   <-calcInfection(hunt.mort.ad.m=0.03,H=1,P=0)
SCEN4_JUV_MALE_SENS_0.045  <-calcInfection(hunt.mort.ad.m=0.045,H=1,P=0)
SCEN4_JUV_MALE_SENS_0.06   <-calcInfection(hunt.mort.ad.m=0.06,H=1,P=0)
SCEN4_JUV_MALE_SENS       <-list(SENS_0     = SCEN4_JUV_MALE_SENS_0,
                                 SENS_0.015 = SCEN4_JUV_MALE_SENS_0.015,
                                 SENS_0.03  = SCEN4_JUV_MALE_SENS_0.03,
                                 SENS_0.045 = SCEN4_JUV_MALE_SENS_0.045,
                                 SENS_0.06  = SCEN4_JUV_MALE_SENS_0.06)

# SCEANRIO 6
SCEN6_JUV_MALE_SENS_0      <-calcHealthy(hunt.mort.ad.m=0,H=1,P=1)
SCEN6_JUV_MALE_SENS_0.015  <-calcHealthy(hunt.mort.ad.m=0.015,H=1,P=1)
SCEN6_JUV_MALE_SENS_0.03   <-calcHealthy(hunt.mort.ad.m=0.03,H=1,P=1)
SCEN6_JUV_MALE_SENS_0.045  <-calcHealthy(hunt.mort.ad.m=0.045,H=1,P=1)
SCEN6_JUV_MALE_SENS_0.06   <-calcHealthy(hunt.mort.ad.m=0.06,H=1,P=1)
SCEN6_JUV_MALE_SENS       <-list(SENS_0     = SCEN6_JUV_MALE_SENS_0,
                                 SENS_0.015 = SCEN6_JUV_MALE_SENS_0.015,
                                 SENS_0.03  = SCEN6_JUV_MALE_SENS_0.03,
                                 SENS_0.045 = SCEN6_JUV_MALE_SENS_0.045,
                                 SENS_0.06   = SCEN6_JUV_MALE_SENS_0.06)

# SCNEARIO 8
SCEN8_JUV_MALE_SENS_0      <-calcInfection(hunt.mort.ad.m=0,H=1,P=1)
SCEN8_JUV_MALE_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=1)
SCEN8_JUV_MALE_SENS_0.03   <-calcInfection(hunt.mort.ad.m=0.03,H=1,P=1)
SCEN8_JUV_MALE_SENS_0.045  <-calcInfection(hunt.mort.ad.m=0.045,H=1,P=1)
SCEN8_JUV_MALE_SENS_0.06   <-calcInfection(hunt.mort.ad.m=0.06,H=1,P=1)
SCEN8_JUV_MALE_SENS       <-list(SENS_0     = SCEN8_JUV_MALE_SENS_0,
                                 SENS_0.015 = SCEN8_JUV_MALE_SENS_0.015,
                                 SENS_0.03  = SCEN8_JUV_MALE_SENS_0.03,
                                 SENS_0.045 = SCEN8_JUV_MALE_SENS_0.045,
                                 SENS_0.06   = SCEN8_JUV_MALE_SENS_0.06)

# COMBINING ALL SCENARIOS INTO ONE LIST
JUV_MALE_SENS<-list(SCEN4_SENS=SCEN4_JUV_MALE_SENS,SCEN6_SENS=SCEN6_JUV_MALE_SENS,
                    SCEN8_SENS=SCEN8_JUV_MALE_SENS)
length(JUV_MALE_SENS[[1]][[1]])
names(JUV_MALE_SENS[[1]][1])

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
JUV_MALE_table<-data.frame()
for(k in 1:length(JUV_MALE_SENS)){
  for (j in 1:length(JUV_MALE_SENS[[k]])){
    for (w in 1:length(JUV_MALE_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        JUV_MALE_table<- rbind(JUV_MALE_table, data.frame(Scenario = names(JUV_MALE_SENS)[k],
                                                          Hunt_perc = names(JUV_MALE_SENS[[k]])[j],
                                                          Category = names(JUV_MALE_SENS[[k]][[j]])[w],
                                                          Age = names(ages)[i],
                                                          Month = 1:ncol(JUV_MALE_SENS[[k]][[j]][[w]]),
                                                          Count = sapply(1:ncol(JUV_MALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(JUV_MALE_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}

JUV_MALE_table<- JUV_MALE_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                  Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                  Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                  Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                  Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                  Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                  Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                  Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                  Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                  Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

JUV_MALE_table<-JUV_MALE_table %>% pivot_wider(names_from = Category, values_from = Count) %>% mutate(Totalprey1 = TotalSprey+TotalIprey)

JUV_MALE_table<-JUV_MALE_table %>% pivot_longer(. , cols = "SFpreyA":"Totalprey1",names_to = "Category", values_to= "Count")

# POPULATION DIFFERENCE TABLE
POP_DIFF_JM <- JUV_MALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
POP_DIFF_JM <- POP_DIFF_JM %>% 
  mutate(SNL2 = POP_DIFF_JM$S_0-POP_DIFF_JM$S_0.03,
         SNL1 =POP_DIFF_JM$S_0.015-POP_DIFF_JM$S_0.03,
         SNB = POP_DIFF_JM$S_0.03-POP_DIFF_JM$S_0.03,
         SNH1 = POP_DIFF_JM$S_0.045-POP_DIFF_JM$S_0.03,
         SNH2 = POP_DIFF_JM$S_0.06-POP_DIFF_JM$S_0.03) %>% 
  pivot_longer(., cols = starts_with("SN"), names_to = "LEVEL_SENS", values_to = "POP_DIFF")

# ELASTICITY TABLE
elasticity_JM <-JUV_MALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
elasticity_JM <-elasticity_JM %>% 
  mutate(DiffL2   = ((elasticity_JM$S_0-elasticity_JM$S_0.03)/elasticity_JM$S_0.03)/((0-0.03)/0.03),
         DiffL1 = ((elasticity_JM$S_0.015-elasticity_JM$S_0.03)/elasticity_JM$S_0.03)/((0.015-0.03)/0.03),
         DiffB   = rep(0,nrow(elasticity_JM)),
         DiffH1 = ((elasticity_JM$S_0.045-elasticity_JM$S_0.03)/elasticity_JM$S_0.03)/((0.045-0.03)/0.03),
         DiffH2   = ((elasticity_JM$S_0.06-elasticity_JM$S_0.03)/elasticity_JM$S_0.03)/((0.06-0.03)/0.03)) %>% 
  pivot_longer(., cols = starts_with("D"), names_to = "LEVEL_DIFF", values_to = "ELASTICITY")

## JUV FEMALE SENS

# SCENARIO 4
SCEN4_JUV_FEMALE_SENS_0      <-calcInfection(hunt.mort.ad.m=0,H=1,P=0)
SCEN4_JUV_FEMALE_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=0)
SCEN4_JUV_FEMALE_SENS_0.03   <-calcInfection(hunt.mort.ad.m=0.03,H=1,P=0)
SCEN4_JUV_FEMALE_SENS_0.045  <-calcInfection(hunt.mort.ad.m=0.045,H=1,P=0)
SCEN4_JUV_FEMALE_SENS_0.06   <-calcInfection(hunt.mort.ad.m=0.06,H=1,P=0)
SCEN4_JUV_FEMALE_SENS        <-list(SENS_0     = SCEN4_JUV_FEMALE_SENS_0,
                                    SENS_0.015 = SCEN4_JUV_FEMALE_SENS_0.015,
                                    SENS_0.03  = SCEN4_JUV_FEMALE_SENS_0.03,
                                    SENS_0.045 = SCEN4_JUV_FEMALE_SENS_0.045,
                                    SENS_0.06   = SCEN4_JUV_FEMALE_SENS_0.06)

# SCEANRIO 6
SCEN6_JUV_FEMALE_SENS_0      <-calcHealthy(hunt.mort.ad.m=0,H=1,P=1)
SCEN6_JUV_FEMALE_SENS_0.015  <-calcHealthy(hunt.mort.ad.m=0.015,H=1,P=1)
SCEN6_JUV_FEMALE_SENS_0.03   <-calcHealthy(hunt.mort.ad.m=0.03,H=1,P=1)
SCEN6_JUV_FEMALE_SENS_0.045  <-calcHealthy(hunt.mort.ad.m=0.045,H=1,P=1)
SCEN6_JUV_FEMALE_SENS_0.06   <-calcHealthy(hunt.mort.ad.m=0.06,H=1,P=1)
SCEN6_JUV_FEMALE_SENS        <-list(SENS_0     = SCEN6_JUV_FEMALE_SENS_0,
                                    SENS_0.015 = SCEN6_JUV_FEMALE_SENS_0.015,
                                    SENS_0.03  = SCEN6_JUV_FEMALE_SENS_0.03,
                                    SENS_0.045 = SCEN6_JUV_FEMALE_SENS_0.045,
                                    SENS_0.06   = SCEN6_JUV_FEMALE_SENS_0.06)

# SCNEARIO 8
SCEN8_JUV_FEMALE_SENS_0      <-calcInfection(hunt.mort.ad.m=0,H=1,P=1)
SCEN8_JUV_FEMALE_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=1)
SCEN8_JUV_FEMALE_SENS_0.03   <-calcInfection(hunt.mort.ad.m=0.03,H=1,P=1)
SCEN8_JUV_FEMALE_SENS_0.045  <-calcInfection(hunt.mort.ad.m=0.045,H=1,P=1)
SCEN8_JUV_FEMALE_SENS_0.06   <-calcInfection(hunt.mort.ad.m=0.06,H=1,P=1)
SCEN8_JUV_FEMALE_SENS        <-list(SENS_0     = SCEN8_JUV_FEMALE_SENS_0,
                                    SENS_0.015 = SCEN8_JUV_FEMALE_SENS_0.015,
                                    SENS_0.03  = SCEN8_JUV_FEMALE_SENS_0.03,
                                    SENS_0.045 = SCEN8_JUV_FEMALE_SENS_0.045,
                                    SENS_0.06  = SCEN8_JUV_FEMALE_SENS_0.06)

# COMBINING ALL SCENARIOS INTO ONE LIST
JUV_FEMALE_SENS<-list(SCEN4_SENS=SCEN4_JUV_FEMALE_SENS,SCEN6_SENS=SCEN6_JUV_FEMALE_SENS,
                      SCEN8_SENS=SCEN8_JUV_FEMALE_SENS)

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
JUV_FEMALE_table<-data.frame()
for(k in 1:length(JUV_FEMALE_SENS)){
  for (j in 1:length(JUV_FEMALE_SENS[[k]])){
    for (w in 1:length(JUV_FEMALE_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        JUV_FEMALE_table<- rbind(JUV_FEMALE_table, data.frame(Scenario = names(JUV_FEMALE_SENS)[k],
                                                              Hunt_perc = names(JUV_FEMALE_SENS[[k]])[j],
                                                              Category = names(JUV_FEMALE_SENS[[k]][[j]])[w],
                                                              Age = names(ages)[i],
                                                              Month = 1:ncol(JUV_FEMALE_SENS[[k]][[j]][[w]]),
                                                              Count = sapply(1:ncol(JUV_FEMALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(JUV_FEMALE_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}

JUV_FEMALE_table<- JUV_FEMALE_table %>% separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                    Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                    Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                    Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                    Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                    Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                    Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                    Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                    Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                    Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

# POPULATION DIFFERENCE TABLE
POP_DIFF_JF <- JUV_FEMALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
POP_DIFF_JF <- POP_DIFF_JF %>%
  mutate(SNL2 = POP_DIFF_JF$S_0-POP_DIFF_JF$S_0.03,
         SNL1 =POP_DIFF_JF$S_0.015-POP_DIFF_JF$S_0.03,
         SNB = POP_DIFF_JF$S_0.03-POP_DIFF_JF$S_0.03,
         SNH1 = POP_DIFF_JF$S_0.045-POP_DIFF_JF$S_0.03,
         SNH2 = POP_DIFF_JF$S_0.06-POP_DIFF_JF$S_0.03) %>%
  pivot_longer(., cols = starts_with("SN"), names_to = "LEVEL_SENS", values_to = "POP_DIFF")

# ELASTICITY TABLE
elasticity_JF <-JUV_FEMALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
elasticity_JF <-elasticity_JF %>%
  mutate(DiffL2   = ((elasticity_JF$S_0-elasticity_JF$S_0.03)/elasticity_JF$S_0.03)/((0-0.03)/0.03),
         DiffL1 = ((elasticity_JF$S_0.015-elasticity_JF$S_0.03)/elasticity_JF$S_0.03)/((0.015-0.03)/0.03),
         DiffB   = rep(0,nrow(elasticity_JF)),
         DiffH1 = ((elasticity_JF$S_0.045-elasticity_JF$S_0.03)/elasticity_JF$S_0.03)/((0.045-0.03)/0.03),
         DiffH2   = ((elasticity_JF$S_0.06-elasticity_JF$S_0.03)/elasticity_JF$S_0.03)/((0.06-0.03)/0.03)) %>%
  pivot_longer(., cols = starts_with("D"), names_to = "LEVEL_DIFF", values_to = "ELASTICITY")

# ## FAWN SENS
# 
# # SCENARIO 4
# SCEN4_FAWN_SENS_0      <-calcInfection(hunt.mort.ad.m=0,H=1,P=0)
# SCEN4_FAWN_SENS_0.005  <-calcInfection(hunt.mort.ad.m=0.005,H=1,P=0)
# SCEN4_FAWN_SENS_0.01   <-calcInfection(hunt.mort.ad.m=0.01,H=1,P=0)
# SCEN4_FAWN_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=0)
# SCEN4_FAWN_SENS_0.02   <-calcInfection(hunt.mort.ad.m=0.02,H=1,P=0)
# SCEN4_FAWN_SENS        <-list(SENS_0     = SCEN4_FAWN_SENS_0,
#                               SENS_0.005 = SCEN4_FAWN_SENS_0.005,
#                               SENS_0.01  = SCEN4_FAWN_SENS_0.01,
#                               SENS_0.015 = SCEN4_FAWN_SENS_0.015,
#                               SENS_0.02  = SCEN4_FAWN_SENS_0.02)
# 
# # SCEANRIO 6
# SCEN6_FAWN_SENS_0     <-calcHealthy(hunt.mort.ad.m=0,H=1,P=1)
# SCEN6_FAWN_SENS_0.005  <-calcHealthy(hunt.mort.ad.m=0.005,H=1,P=1)
# SCEN6_FAWN_SENS_0.01   <-calcHealthy(hunt.mort.ad.m=0.01,H=1,P=1)
# SCEN6_FAWN_SENS_0.015  <-calcHealthy(hunt.mort.ad.m=0.015,H=1,P=1)
# SCEN6_FAWN_SENS_0.02   <-calcHealthy(hunt.mort.ad.m=0.02,H=1,P=1)
# SCEN6_FAWN_SENS        <-list(SENS_0     = SCEN6_FAWN_SENS_0,
#                               SENS_0.005 = SCEN6_FAWN_SENS_0.005,
#                               SENS_0.01  = SCEN6_FAWN_SENS_0.01,
#                               SENS_0.015 = SCEN6_FAWN_SENS_0.015,
#                               SENS_0.02  = SCEN6_FAWN_SENS_0.02)
# 
# # SCNEARIO 8
# SCEN8_FAWN_SENS_0     <-calcInfection(hunt.mort.ad.m=0,H=1,P=1)
# SCEN8_FAWN_SENS_0.005  <-calcInfection(hunt.mort.ad.m=0.005,H=1,P=1)
# SCEN8_FAWN_SENS_0.01   <-calcInfection(hunt.mort.ad.m=0.01,H=1,P=1)
# SCEN8_FAWN_SENS_0.015  <-calcInfection(hunt.mort.ad.m=0.015,H=1,P=1)
# SCEN8_FAWN_SENS_0.02   <-calcInfection(hunt.mort.ad.m=0.02,H=1,P=1)
# SCEN8_FAWN_SENS        <-list(SENS_0     = SCEN8_FAWN_SENS_0,
#                               SENS_0.005 = SCEN8_FAWN_SENS_0.005,
#                               SENS_0.01  = SCEN8_FAWN_SENS_0.01,
#                               SENS_0.015 = SCEN8_FAWN_SENS_0.015,
#                               SENS_0.02  = SCEN8_FAWN_SENS_0.02)
# 
# # COMBINING ALL SCENARIOS INTO ONE LIST
# FAWN_SENS<-list(SCEN4_SENS=SCEN4_FAWN_SENS,SCEN6_SENS=SCEN6_FAWN_SENS,
#                 SCEN8_SENS=SCEN8_FAWN_SENS)
# 
# # LOOP COMBINING ALL SCENARIOS IN LONG FORM
# FAWN_table<-data.frame()
# for(k in 1:length(FAWN_SENS)){
#   for (j in 1:length(FAWN_SENS[[k]])){
#     for (w in 1:length(FAWN_SENS[[k]][[j]])){
#       for (i in 1:length(ages)) {
#         vars<-unlist(mget(names(ages)[i]))
#         FAWN_table<- rbind(FAWN_table, 
#                            data.frame(Scenario = names(FAWN_SENS)[k],
#                                       Hunt_perc = names(FAWN_SENS[[k]])[j],
#                                       Category = names(FAWN_SENS[[k]][[j]])[w],
#                                       Age = names(ages)[i],
#                                       Month = 1:ncol(FAWN_SENS[[k]][[j]][[w]]),
#                                       Count = sapply(1:ncol(FAWN_SENS[[k]][[j]][[w]]), 
#                                       FUN = function(x) {sum(FAWN_SENS[[k]][[j]][[w]][vars,x])})))
#       }
#     }
#   }
# }
# 
# FAWN_table<- FAWN_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
#                           Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
#                           Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
#                           Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
#                           Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
#                           Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
#                           Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
#                           Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
#                           Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
#                           Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))
# 
# # POPULATION DIFFERENCE TABLE
# POP_DIFF_FAWN <- FAWN_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# POP_DIFF_FAWN <- POP_DIFF_FAWN %>% 
#   mutate(SNL2 = POP_DIFF_FAWN$S_0-POP_DIFF_FAWN$S_0.01,
#          SNL1 =POP_DIFF_FAWN$S_0.005-POP_DIFF_FAWN$S_0.01,
#          SNB = POP_DIFF_FAWN$S_0.02-POP_DIFF_FAWN$S_0.01,
#          SNH1 = POP_DIFF_FAWN$S_0.015-POP_DIFF_FAWN$S_0.01,
#          SNH2 = POP_DIFF_FAWN$S_0.02-POP_DIFF_FAWN$S_0.01) %>% 
#   pivot_longer(., cols = starts_with("SN"), names_to = "LEVEL_SENS", values_to = "POP_DIFF")
# 
# # ELASTICITY TABLE
# elasticity_FAWN <-FAWN_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# elasticity_FAWN <-elasticity_FAWN %>% 
#   mutate(DiffL2   = ((elasticity_FAWN$S_0-elasticity_FAWN$S_0.01)/elasticity_FAWN$S_0.01)/((0-0.05)/0.01),
#          DiffL1 = ((elasticity_FAWN$S_0.005-elasticity_FAWN$S_0.01)/elasticity_FAWN$S_0.01)/((0.005-0.01)/0.01),
#          DiffB   = rep(0,nrow(elasticity_FAWN)),
#          DiffH1 = ((elasticity_FAWN$S_0.015-elasticity_FAWN$S_0.01)/elasticity_FAWN$S_0.01)/((0.015-0.01)/0.01),
#          DiffH2   = ((elasticity_FAWN$S_0.02-elasticity_FAWN$S_0.01)/elasticity_FAWN$S_0.01)/((0.02-0.01)/0.01)) %>% 
#   pivot_longer(., cols = starts_with("D"), names_to = "LEVEL_DIFF", values_to = "ELASTICITY")
# 
# ### The numbers used in this section are historically accurate min, average, and max values for each sex and age, except fawns. Historically, fawns are not hunted and thus the hunting percentage for fawns in this section is set at 0.5% to account for accidental hunting deaths.
# 
# ## ADULT MALE SENS
# 
# # SCENARIO 4
# SCEN4_AD_MALE_SENS_0.0675 <-calcInfection(hunt.mort.ad.m=0.0675,H=1,P=0)
# SCEN4_AD_MALE_SENS_0.1    <-calcInfection(hunt.mort.ad.m=0.1,H=1,P=0)
# SCEN4_AD_MALE_SENS_0.17   <-calcInfection(hunt.mort.ad.m=0.17,H=1,P=0)
# SCEN4_AD_MALE_SENS        <-list(SENS_0.0675 = SCEN4_AD_MALE_SENS_0.0675,
#                                  SENS_0.1    = SCEN4_AD_MALE_SENS_0.1,
#                                  SENS_0.17   = SCEN4_AD_MALE_SENS_0.17)
# 
# # SCEANRIO 6
# SCEN6_AD_MALE_SENS_0.0675 <-calcHealthy(hunt.mort.ad.m=0.0675,H=1,P=1)
# SCEN6_AD_MALE_SENS_0.1    <-calcHealthy(hunt.mort.ad.m=0.1,H=1,P=1)
# SCEN6_AD_MALE_SENS_0.17   <-calcHealthy(hunt.mort.ad.m=0.17,H=1,P=1)
# SCEN6_AD_MALE_SENS        <-list(SENS_0.0675 = SCEN6_AD_MALE_SENS_0.0675,
#                                  SENS_0.1    = SCEN6_AD_MALE_SENS_0.1,
#                                  SENS_0.17   = SCEN6_AD_MALE_SENS_0.17)
# 
# # SCNEARIO 8
# SCEN8_AD_MALE_SENS_0.0675 <-calcInfection(hunt.mort.ad.m=0.0675,H=1,P=1)
# SCEN8_AD_MALE_SENS_0.1    <-calcInfection(hunt.mort.ad.m=0.1,H=1,P=1)
# SCEN8_AD_MALE_SENS_0.17   <-calcInfection(hunt.mort.ad.m=0.17,H=1,P=1)
# SCEN8_AD_MALE_SENS        <-list(SENS_0.0675 = SCEN8_AD_MALE_SENS_0.0675,
#                                  SENS_0.1    = SCEN8_AD_MALE_SENS_0.1,
#                                  SENS_0.17   = SCEN8_AD_MALE_SENS_0.17)
# 
# 
# # COMBINING ALL SCENARIOS INTO ONE LIST
# H_AD_MALE_SENS<-list(SCEN4_SENS=SCEN4_AD_MALE_SENS,SCEN6_SENS=SCEN6_AD_MALE_SENS,
#                      SCEN8_SENS=SCEN8_AD_MALE_SENS)
# 
# # LOOP COMBINING ALL SCENARIOS IN LONG FORM
# H_AD_MALE_table<-data.frame()
# for(k in 1:length(H_AD_MALE_SENS)){
#   for (j in 1:length(H_AD_MALE_SENS[[k]])){
#     for (w in 1:length(H_AD_MALE_SENS[[k]][[j]])){
#       for (i in 1:length(ages)) {
#         vars<-unlist(mget(names(ages)[i]))
#         H_AD_MALE_table<- rbind(H_AD_MALE_table, data.frame(Scenario = names(H_AD_MALE_SENS)[k],
#                                                             Hunt_perc = names(H_AD_MALE_SENS[[k]])[j],
#                                                             Category = names(H_AD_MALE_SENS[[k]][[j]])[w],
#                                                             Age = names(ages)[i],
#                                                             Month = 1:ncol(H_AD_MALE_SENS[[k]][[j]][[w]]),
#                                                             Count = sapply(1:ncol(H_AD_MALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(H_AD_MALE_SENS[[k]][[j]][[w]][vars,x])})))
#       }
#     }
#   }
# }
# 
# H_AD_MALE_table<- H_AD_MALE_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
#                                     Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
#                                     Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
#                                     Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
#                                     Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
#                                     Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
#                                     Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
#                                     Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
#                                     Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
#                                     Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))
# # POPULATION DIFFERENCE TABLE
# POP_DIFF_H_AM <- H_AD_MALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# POP_DIFF_H_AM <- POP_DIFF_H_AM %>% 
#   mutate(SNL = POP_DIFF_H_AM$S_0.0675-POP_DIFF_H_AM$S_0.1,
#          SNB = POP_DIFF_H_AM$S_0.1-POP_DIFF_H_AM$S_0.1,
#          SNH =POP_DIFF_H_AM$S_0.17-POP_DIFF_H_AM$S_0.1) %>% 
#   pivot_longer(., cols = starts_with("SN"), names_to = "LEVEL_SENS", values_to = "POP_DIFF")
# 
# # ELASTICITY TABLE
# elasticity_H_AM <-H_AD_MALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# elasticity_H_AM <-elasticity_H_AM %>% 
#   mutate(DiffL = ((elasticity_H_AM$S_0.0675-elasticity_H_AM$S_0.1)/elasticity_H_AM$S_0.1)/((0.0675-0.1)/0.1),
#          DiffB   = rep(0,nrow(elasticity_H_AM)),
#          DiffH   = ((elasticity_H_AM$S_0.17-elasticity_H_AM$S_0.1)/elasticity_H_AM$S_0.1)/((0.17-0.1)/0.1)) %>% 
#   pivot_longer(., cols = starts_with("D"), names_to = "LEVEL_DIFF", values_to = "ELASTICITY")
# 
# ## ADULT FEMALE SENS
# 
# # SCENARIO 4
# SCEN4_AD_FEMALE_SENS_0.057 <-calcInfection(hunt.mort.ad.f=0.057,H=1,P=0)
# SCEN4_AD_FEMALE_SENS_0.11  <-calcInfection(hunt.mort.ad.f=0.11,H=1,P=0)
# SCEN4_AD_FEMALE_SENS_0.224 <-calcInfection(hunt.mort.ad.f=0.224,H=1,P=0)
# SCEN4_AD_FEMALE_SENS       <-list(SENS_0.057  = SCEN4_AD_FEMALE_SENS_0.057,
#                                   SENS_0.11   = SCEN4_AD_FEMALE_SENS_0.11,
#                                   SENS_0.224  = SCEN4_AD_FEMALE_SENS_0.224)
# 
# # SCEANRIO 6
# SCEN6_AD_FEMALE_SENS_0.057     <-calcHealthy(hunt.mort.ad.f=0.057,H=1,P=1)
# SCEN6_AD_FEMALE_SENS_0.11  <-calcHealthy(hunt.mort.ad.f=0.11,H=1,P=1)
# SCEN6_AD_FEMALE_SENS_0.224   <-calcHealthy(hunt.mort.ad.f=0.224,H=1,P=1)
# SCEN6_AD_FEMALE_SENS       <-list(SENS_0.057  = SCEN6_AD_FEMALE_SENS_0.057,
#                                   SENS_0.11   = SCEN6_AD_FEMALE_SENS_0.11,
#                                   SENS_0.224  = SCEN6_AD_FEMALE_SENS_0.224)
# 
# # SCNEARIO 8
# SCEN8_AD_FEMALE_SENS_0.057 <-calcInfection(hunt.mort.ad.f=0.057,H=1,P=0)
# SCEN8_AD_FEMALE_SENS_0.11  <-calcInfection(hunt.mort.ad.f=0.11,H=1,P=0)
# SCEN8_AD_FEMALE_SENS_0.224 <-calcInfection(hunt.mort.ad.f=0.224,H=1,P=0)
# SCEN8_AD_FEMALE_SENS       <-list(SENS_0.057  = SCEN8_AD_FEMALE_SENS_0.057,
#                                   SENS_0.11   = SCEN8_AD_FEMALE_SENS_0.11,
#                                   SENS_0.224  = SCEN8_AD_FEMALE_SENS_0.224)
# 
# # COMBINING ALL SCENARIOS INTO ONE LIST
# H_AD_FEMALE_SENS<-list(SCEN4_SENS=SCEN4_AD_FEMALE_SENS,SCEN6_SENS=SCEN6_AD_FEMALE_SENS,
#                        SCEN8_SENS=SCEN8_AD_FEMALE_SENS)
# 
# 
# # LOOP COMBINING ALL SCENARIOS IN LONG FORM
# H_AD_FEMALE_table<-data.frame()
# for(k in 1:length(H_AD_FEMALE_SENS)){
#   for (j in 1:length(H_AD_FEMALE_SENS[[k]])){
#     for (w in 1:length(H_AD_FEMALE_SENS[[k]][[j]])){
#       for (i in 1:length(ages)) {
#         vars<-unlist(mget(names(ages)[i]))
#         H_AD_FEMALE_table<- rbind(H_AD_FEMALE_table, data.frame(Scenario = names(H_AD_FEMALE_SENS)[k],
#                                                                 Hunt_perc = names(H_AD_FEMALE_SENS[[k]])[j],
#                                                                 Category = names(H_AD_FEMALE_SENS[[k]][[j]])[w],
#                                                                 Age = names(ages)[i],
#                                                                 Month = 1:ncol(H_AD_FEMALE_SENS[[k]][[j]][[w]]),
#                                                                 Count = sapply(1:ncol(H_AD_FEMALE_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(H_AD_FEMALE_SENS[[k]][[j]][[w]][vars,x])})))
#       }
#     }
#   }
# }
# 
# H_AD_FEMALE_table<- H_AD_FEMALE_table %>%   separate(Hunt_perc, into = c("char","Hunt_perc"), sep = "N", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
#                                                                                                                                                     Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
#                                                                                                                                                     Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
#                                                                                                                                                     Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
#                                                                                                                                                     Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
#                                                                                                                                                     Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
#                                                                                                                                                     Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
#                                                                                                                                                     Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
#                                                                                                                                                     Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
#                                                                                                                                                     Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))
# 
# POP_DIFF_H_AF <- H_AD_FEMALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# POP_DIFF_H_AF <- POP_DIFF_H_AF %>% 
#   mutate(SNL = POP_DIFF_H_AF$S_0.057-POP_DIFF_H_AF$S_0.11,
#          SNB =POP_DIFF_H_AF$S_0.11-POP_DIFF_H_AF$S_0.11,
#          SNH = POP_DIFF_H_AF$S_0.224-POP_DIFF_H_AF$S_0.11) %>% 
#   pivot_longer(., cols = starts_with("SN"), names_to = "LEVEL_SENS", values_to = "POP_DIFF")
# 
# 
# elasticity_H_AF <-H_AD_FEMALE_table %>% pivot_wider(names_from = Hunt_perc, values_from = Count)
# elasticity_H_AF <-elasticity_H_AF %>% 
#   mutate(DiffL = ((elasticity_H_AF$S_0.057-elasticity_H_AF$S_0.11)/elasticity_H_AF$S_0.11)/((0.057-0.11)/0.11),
#          DiffB = rep(0,nrow(elasticity_H_AF)),
#          DiffH = ((elasticity_H_AF$S_0.224-elasticity_H_AF$S_0.11)/elasticity_H_AF$S_0.11)/((0.224-0.11)/0.11)) %>% 
#   pivot_longer(., cols = starts_with("D"), names_to = "LEVEL_DIFF", values_to = "ELASTICITY")

