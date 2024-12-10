### Start by loading in the packages and functions needed for pulling data. I connect to a database to store all of the data I am generating.

require(tidyverse)
require(RSQLite)
require(DBI)
require(popbio)
require(openxlsx)
require(stargazer)

# LOAD IN THE FUNCTIONS
source("CWD_FUNCTIONS.R")

Juv = c(1:2)
Adult = c(3:13) 
Old = c(14:18)
ages<-list(Juv=Juv,Adult=Adult,Old=Old)


## PREDATION KILL RATE SCENARIOS

# SCENARIO 5
PREDP_20  <-calcHealthy(k_max=0.2,H=0,P=1)
PREDP_50  <-calcHealthy(k_max=0.5,H=0,P=1)
PREDP_80  <-calcHealthy(k_max=0.8,H=0,P=1)
PREDP_100 <-calcHealthy(k_max=1,H=0,P=1)
PREDP_120 <-calcHealthy(k_max=1.2,H=0,P=1)
SCEN5_PRED       <-list(PREDP_20=PREDP_20,
                        PREDP_50=PREDP_50,
                        PREDP_80=PREDP_80,
                        PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120)
# SCENARIO 6
PREDP_20  <-calcHealthy(k_max=0.2,H=1,P=1)
PREDP_50  <-calcHealthy(k_max=0.5,H=1,P=1)
PREDP_80  <-calcHealthy(k_max=0.8,H=1,P=1)
PREDP_100 <-calcHealthy(k_max=1,H=1,P=1)
PREDP_120 <-calcHealthy(k_max=1.2,H=1,P=1)
SCEN6_PRED       <-list(PREDP_20=PREDP_20,
                        PREDP_50=PREDP_50,
                        PREDP_80=PREDP_80,
                        PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120)
# SCEANRIO 7
PREDP_20  <-calcInfection(k_max=0.2,H=0,P=1)
PREDP_50  <-calcInfection(k_max=0.5,H=0,P=1)
PREDP_80  <-calcInfection(k_max=0.8,H=0,P=1)
PREDP_100 <-calcInfection(k_max=1,H=0,P=1)
PREDP_120 <-calcInfection(k_max=1.2,H=0,P=1)
SCEN7_PRED       <-list(PREDP_20=PREDP_20,
                        PREDP_50=PREDP_50,
                        PREDP_80=PREDP_80,
                        PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120)

# SCEANRIO 8
PREDP_20  <-calcInfection(k_max=0.2,H=1,P=1)
PREDP_50  <-calcInfection(k_max=0.5,H=1,P=1)
PREDP_80  <-calcInfection(k_max=0.8,H=1,P=1)
PREDP_100 <-calcInfection(k_max=1,H=1,P=1)
PREDP_120 <-calcInfection(k_max=1.2,H=1,P=1)
SCEN8_PRED       <-list(PREDP_20=PREDP_20,
                        PREDP_50=PREDP_50,
                        PREDP_80=PREDP_80,
                        PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120)

# COMBINING ALL SCENARIOS INTO ONE LIST
PRED_SENS<-list(SCEN5_PRED=SCEN5_PRED,SCEN6_PRED=SCEN6_PRED,SCEN7_PRED=SCEN7_PRED,SCEN8_PRED=SCEN8_PRED)
length(PRED_SENS[[1]][[1]])
length(PRED_SENS[[1]][[1]])

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
PRED_table<-data.frame()
for(k in 1:length(PRED_SENS)){
  for (j in 1:length(PRED_SENS[[k]])){
    for (w in 1:length(PRED_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        PRED_table<- rbind(PRED_table, data.frame(Scenario = names(PRED_SENS)[k],
                                                        Kill_rate = names(PRED_SENS[[k]])[j],
                                                        Category = names(PRED_SENS[[k]][[j]])[w],
                                                        Age = names(ages)[i],
                                                        Month = 1:ncol(PRED_SENS[[k]][[j]][[w]]),
                                                        Count = sapply(1:ncol(PRED_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(PRED_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}
PRED_table<- PRED_table %>% separate(Kill_rate, into = c("char","Kill_rate"), sep = "D", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                      Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                      Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                      Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                      Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                      Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                      Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                      Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                      Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                      Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20",
                                                      Month %in% c(241:252) ~ "21", Month %in% c(253:264) ~ "22",
                                                      Month %in% c(265:276) ~ "23", Month %in% c(277:288) ~ "24",
                                                      Month %in% c(289:300) ~ "25", Month %in% c(301:312) ~ "26",
                                                      Month %in% c(313:324) ~ "27", Month %in% c(325:336) ~ "28",
                                                      Month %in% c(337:348) ~ "29", Month %in% c(349:360) ~ "30"))

PRED_table<-PRED_table %>% pivot_wider(names_from = Category, values_from = Count)%>%
  mutate(Totalpreyi = TotalSprey+TotalIprey,Totalpreyh = FpreyA+MpreyA)

PRED_table<-PRED_table %>% pivot_longer(. , cols = !c("Scenario","char","Kill_rate","Age","Month","Year"),names_to = "Category", values_to= "Count")

PRED_table1<- PRED_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(PRED_table1))){
  Total_pop = PRED_table1$Juv+PRED_table1$Adult+PRED_table1$Old
}
PRED_table1<- add_column(PRED_table1,Total_pop,.after = "Old")
PRED_table1<-PRED_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Kill_rate","Month","Year","Category"), names_to = "Age", values_to= "Count")

# POPULATON DIFFERENCE TABLE
POP_DIFF_PRED <- PRED_table1 %>% pivot_wider(names_from = Kill_rate, values_from = Count)
POP_DIFF_PRED <- POP_DIFF_PRED %>% 
  mutate(PD20 = POP_DIFF_PRED$P_20-POP_DIFF_PRED$P_100,
         PD50 = POP_DIFF_PRED$P_50-POP_DIFF_PRED$P_100,
         PD80 = POP_DIFF_PRED$P_80-POP_DIFF_PRED$P_100,
         PDP100 = POP_DIFF_PRED$P_100-POP_DIFF_PRED$P_100,
         PDP120 = POP_DIFF_PRED$P_120-POP_DIFF_PRED$P_100) %>% 
  pivot_longer(., cols = starts_with("PD"), names_to = "Predation_Level", values_to = "POP_DIFF")

# PREVELANCE TABLE
PREV_DIFF_PRED <- PRED_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_DIFF_PRED <- PREV_DIFF_PRED %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","char","Kill_rate", "Age","Month","Year"),names_to = "Category", values_to= "Count")

# CUMMULATIVE ELAST CWD

sens_name<-c("SCEN7_PRED","SCEN8_PRED")
cummul_cwd_PRED_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PRED <- PRED_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PRED <- cummul_PRED %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Kill_rate","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_cwd_PRED <-cummul_PRED %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Kill_rate, values_from = Count)
  cummul_cwd_PRED_table<-rbind(cummul_cwd_PRED_table, data.frame(cummul_cwd_PRED %>% select(Scenario,P_20,P_50,P_80,P_100,P_120)))
}
cummul_cwd_PRED_elast<-data.frame()
for (i in 1:length(sens_name)){
  cummul_cwd_PRED_elast_1  <-cummul_cwd_PRED_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
    mutate(Cummul_cwd20   = ((P_20-P_100)/P_100)/((0.2-1)/1),
           Cummul_cwd50 = ((P_50-P_100)/P_100)/((0.5-1)/1),
           Cummul_cwd80   = ((P_80-P_100)/P_100)/((0.8-1)/1),
           Cummul_cwd100 = rep(0,nrow(cummul_cwd_PRED)),
           Cummul_cwd120   = ((P_120-P_100)/P_100)/((1.2-1)/1))
  cummul_cwd_PRED_elast<-rbind(cummul_cwd_PRED_elast, data.frame(Scenario=sens_name[i],cummul_cwd_PRED_elast_1))
}
cummul_cwd_PRED_elast  <-cummul_cwd_PRED_elast  %>% 
  pivot_longer(., cols = c("Cummul_cwd20","Cummul_cwd50","Cummul_cwd80","Cummul_cwd100","Cummul_cwd120"), names_to = "Cummulative_cwd", values_to = "Cummul_cwd_Elast")


stargazer(t(cummul_cwd_PRED_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_cwd.tex")

# CUMMULATIVE ELAST HUNTING
sens_name<-c("SCEN6_PRED","SCEN8_PRED")
cummul_hunt_PRED_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PRED <- PRED_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PRED <- cummul_PRED %>% mutate(cummul_hunt = cumsum(TotalH),
                                    cummul_cwd = cumsum(TotalCWDdeath),
                                    cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Kill_rate","Age","Month","Year"),names_to = "Category", values_to= "Count")
  huntmod<-vector()
  huntmod[cummul_PRED$Month%%12 == 7] <- 1 
  cummul_PRED <-cummul_PRED %>% add_column(.,huntmod,.after = "Month")
  cummul_PRED <- filter(cummul_PRED, huntmod > 0) 
  
  cummul_hunt_PRED <-cummul_PRED %>% filter(Month%in% c("235"), Category %in% c("cummul_hunt"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Kill_rate, values_from = Count)
  cummul_hunt_PRED_table<-rbind(cummul_hunt_PRED_table, data.frame(cummul_hunt_PRED %>% select(Scenario,P_20,P_50,P_80,P_100,P_120)))
}

cummul_hunt_PRED_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_hunt_PRED_elast_1  <-cummul_hunt_PRED_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
  mutate(Cummul_hunt20   = ((P_20-P_100)/P_100)/((0.2-1)/1),
         Cummul_hunt50 = ((P_50-P_100)/P_100)/((0.5-1)/1),
         Cummul_hunt80   = ((P_80-P_100)/P_100)/((0.8-1)/1),
         Cummul_hunt100 = rep(0,nrow(cummul_hunt_PRED)),
         Cummul_hunt120   = ((P_120-P_100)/P_100)/((1.2-1)/1))
cummul_hunt_PRED_elast<-rbind(cummul_hunt_PRED_elast, data.frame(Scenario=sens_name[i],cummul_hunt_PRED_elast_1))
}
cummul_hunt_PRED_elast  <-cummul_hunt_PRED_elast %>% 
  pivot_longer(., cols = c("Cummul_hunt20","Cummul_hunt50","Cummul_hunt80","Cummul_hunt100","Cummul_hunt120"), names_to = "Cummulative_hunt", values_to = "Cummul_hunt_Elast")

# CUMMULATIVE ELAST PREDATION
sens_name<-c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED")
cummul_pred_PRED_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PRED <- PRED_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PRED <- cummul_PRED %>% mutate(cummul_hunt = cumsum(TotalH),
                                        cummul_cwd = cumsum(TotalCWDdeath),
                                        cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Kill_rate","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_pred_PRED <-cummul_PRED %>% filter(Year %in% c("20"), Category %in% c("cummul_pred"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Kill_rate, values_from = Count)
  cummul_pred_PRED_table<-rbind(cummul_pred_PRED_table, data.frame(cummul_pred_PRED %>% select(Scenario,P_20,P_50,P_80,P_100,P_120)))
}
cummul_pred_PRED_elast<-data.frame()
for (i in 1:length(sens_name)){
cummul_pred_PRED_elast_1  <-cummul_pred_PRED_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
  mutate(Cummul_pred20   = ((P_20-P_100)/P_100)/((0.2-1)/1),
         Cummul_pred50 = ((P_50-P_100)/P_100)/((0.5-1)/1),
         Cummul_pred80   = ((P_80-P_100)/P_100)/((0.8-1)/1),
         Cummul_pred100 = rep(0,nrow(cummul_pred_PRED)),
         Cummul_pred120   = ((P_120-P_100)/P_100)/((1.2-1)/1))
cummul_pred_PRED_elast<-rbind(cummul_pred_PRED_elast, data.frame(Scenario=sens_name[i],cummul_pred_PRED_elast_1))
}
cummul_pred_PRED_elast  <-cummul_pred_PRED_elast %>% 
  pivot_longer(., cols = c("Cummul_pred20","Cummul_pred50","Cummul_pred80","Cummul_pred100","Cummul_pred120"), names_to = "Cummulative_pred", values_to = "Cummul_pred_Elast")

# ELASTICITY TABLE to population 
elasticity_PRED <-PRED_table1 %>% pivot_wider(names_from = Kill_rate, values_from = Count)
elasticity_PRED <-elasticity_PRED %>% 
  mutate(Elast20   = ((elasticity_PRED$P_20-elasticity_PRED$P_100)/elasticity_PRED$P_100)/((0.2-1)/1),
         Elast50 = ((elasticity_PRED$P_50-elasticity_PRED$P_100)/elasticity_PRED$P_100)/((0.5-1)/1),
         Elast80   = ((elasticity_PRED$P_80-elasticity_PRED$P_100)/elasticity_PRED$P_100)/((0.8-1)/1),
         ElastP100 = rep(0,nrow(elasticity_PRED)),
         ElastP120   = ((elasticity_PRED$P_120-elasticity_PRED$P_100)/elasticity_PRED$P_100)/((1.2-1)/1)) %>% 
  pivot_longer(., cols = starts_with("E"), names_to = "Elasticity_Predation_Level", values_to = "Predation")

# ELASTICITY to prevalence
prev_PRED <-PREV_DIFF_PRED  %>% filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"), Category %in% c("PREV")) %>% pivot_wider(names_from = Kill_rate, values_from = Count)
prev_PRED <-prev_PRED %>% 
  mutate(Prev20   = ((prev_PRED$P_20-prev_PRED$P_100)/prev_PRED$P_100)/((0.2-1)/1),
         Prev50   = ((prev_PRED$P_50-prev_PRED$P_100)/prev_PRED$P_100)/((0.5-1)/1),
         Prev80   = ((prev_PRED$P_80-prev_PRED$P_100)/prev_PRED$P_100)/((0.8-1)/1),
         PrevP100  = rep(0,nrow(prev_PRED)),
         PrevP120  = ((prev_PRED$P_120-prev_PRED$P_100)/prev_PRED$P_100)/((1.2-1)/1)) %>% 
  pivot_longer(., cols = starts_with("Prev"), names_to = "Prevalence", values_to = "PREV_Elast")
prev_test<-prev_PRED %>% filter(Year %in% c("20"))
# CWD TABLE

cwd_PRED <-PRED_table1  %>% filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"), Category %in% c("TotalCWDdeath")) %>% pivot_wider(names_from = Kill_rate, values_from = Count)
cwd_PRED <-cwd_PRED %>% 
  mutate(e_cwd20   = ((cwd_PRED$P_20-cwd_PRED$P_100)/cwd_PRED$P_100)/((0.2-1)/1),
         e_cwd50   = ((cwd_PRED$P_50-cwd_PRED$P_100)/cwd_PRED$P_100)/((0.5-1)/1),
         e_cwd80   = ((cwd_PRED$P_80-cwd_PRED$P_100)/cwd_PRED$P_100)/((0.8-1)/1),
         e_cwdP100  = rep(0,nrow(cwd_PRED)),
         e_cwdP120  = ((cwd_PRED$P_120-cwd_PRED$P_100)/cwd_PRED$P_100)/((1.2-1)/1)) %>% 
  pivot_longer(., cols = starts_with("e_c"), names_to = "CWD", values_to = "CWD_Elast")

list_of_pred_tables<-list("POP_DIFF_PRED"=POP_DIFF_PRED,"PREV_DIFF_PRED"=PREV_DIFF_PRED,"elasticity_PRED"=elasticity_PRED,
                          "prev_PRED"=prev_PRED,"cwd_PRED"=cwd_PRED,"cummul_pred_PRED_elast"=cummul_pred_PRED_elast,
                          "cummul_cwd_PRED_elast"=cummul_cwd_PRED_elast,"cummul_hunt_PRED_elast"=cummul_hunt_PRED_elast)
write.xlsx(list_of_pred_tables,"elasticities_and_differences_pred.xlsx")

