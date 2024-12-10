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
PREDP_100  <-calcHealthy(max.predators = 100,H=0,P=1)
PREDP_120  <-calcHealthy(max.predators = 120,H=0,P=1)
PREDP_150  <-calcHealthy(max.predators = 150,H=0,P=1)
PREDP_180 <-calcHealthy(max.predators = 180,H=0,P=1)
PREDP_200 <-calcHealthy(max.predators = 200,H=0,P=1)
SCEN5_PREDMax       <-list(PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120,
                        PREDP_150=PREDP_150,
                        PREDP_180=PREDP_180,
                        PREDP_200=PREDP_200)
# SCENARIO 6
PREDP_100  <-calcHealthy(max.predators = 100,H=1,P=1)
PREDP_120  <-calcHealthy(max.predators = 120,H=1,P=1)
PREDP_150  <-calcHealthy(max.predators = 150,H=1,P=1)
PREDP_180 <-calcHealthy(max.predators = 180,H=1,P=1)
PREDP_200 <-calcHealthy(max.predators = 200,H=1,P=1)
SCEN6_PREDMax       <-list(PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120,
                        PREDP_150=PREDP_150,
                        PREDP_180=PREDP_180,
                        PREDP_200=PREDP_200)
# SCEANRIO 7
PREDP_100  <-calcInfection(max.predators = 100,H=0,P=1)
PREDP_120  <-calcInfection(max.predators = 120,H=0,P=1)
PREDP_150  <-calcInfection(max.predators = 150,H=0,P=1)
PREDP_180 <-calcInfection(max.predators = 180,H=0,P=1)
PREDP_200 <-calcInfection(max.predators = 200,H=0,P=1)
SCEN7_PREDMax       <-list(PREDP_100=PREDP_100,
                        PREDP_120=PREDP_120,
                        PREDP_150=PREDP_150,
                        PREDP_180=PREDP_180,
                        PREDP_200=PREDP_200)

# SCEANRIO 8
PREDP_100  <-calcInfection(max.predators = 100,H=1,P=1)
PREDP_120  <-calcInfection(max.predators = 120,H=1,P=1)
PREDP_150  <-calcInfection(max.predators = 150,H=1,P=1)
PREDP_180 <-calcInfection(max.predators = 180,H=1,P=1)
PREDP_200 <-calcInfection(max.predators = 200,H=1,P=1)
SCEN8_PREDMax       <-list(PREDP_100=PREDP_100,
                           PREDP_120=PREDP_120,
                           PREDP_150=PREDP_150,
                           PREDP_180=PREDP_180,
                           PREDP_200=PREDP_200)

# COMBINING ALL SCENARIOS INTO ONE LIST
PREDMax_SENS<-list(SCEN5_PREDMax=SCEN5_PREDMax,SCEN6_PREDMax=SCEN6_PREDMax,SCEN7_PREDMax=SCEN7_PREDMax,SCEN8_PREDMax=SCEN8_PREDMax)
length(PREDMax_SENS[[1]][[1]])
length(PREDMax_SENS[[1]][[1]])

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
PREDMax_table<-data.frame()
for(k in 1:length(PREDMax_SENS)){
  for (j in 1:length(PREDMax_SENS[[k]])){
    for (w in 1:length(PREDMax_SENS[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        PREDMax_table<- rbind(PREDMax_table, data.frame(Scenario = names(PREDMax_SENS)[k],
                                                  Max_Pred = names(PREDMax_SENS[[k]])[j],
                                                  Category = names(PREDMax_SENS[[k]][[j]])[w],
                                                  Age = names(ages)[i],
                                                  Month = 1:ncol(PREDMax_SENS[[k]][[j]][[w]]),
                                                  Count = sapply(1:ncol(PREDMax_SENS[[k]][[j]][[w]]), FUN = function(x) {sum(PREDMax_SENS[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}
PREDMax_table<- PREDMax_table %>% separate(Max_Pred, into = c("char","Max_Pred"), sep = "D", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                   Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                   Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                   Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                   Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                   Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                 Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                   Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                   Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                   Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

PREDMax_table<-PREDMax_table %>% pivot_wider(names_from = Category, values_from = Count)%>%
  mutate(Totalpreyi = TotalSprey+TotalIprey,Totalpreyh = FpreyA+MpreyA)

PREDMax_table<-PREDMax_table %>% pivot_longer(. , cols = !c("Scenario","char","Max_Pred","Age","Month","Year"),names_to = "Category", values_to= "Count")

PREDMax_table1<- PREDMax_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(PREDMax_table1))){
  Total_pop = PREDMax_table1$Juv+PREDMax_table1$Adult+PREDMax_table1$Old
}
PREDMax_table1<- add_column(PREDMax_table1,Total_pop,.after = "Old")
PREDMax_table1<-PREDMax_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Max_Pred","Month","Year","Category"),names_to = "Age", values_to= "Count") 

# POPULATON DIFFERENCE TABLE
POP_DIFF_PREDMax <- PREDMax_table1 %>% pivot_wider(names_from = Max_Pred, values_from = Count)
POP_DIFF_PREDMax <- POP_DIFF_PREDMax %>% 
  mutate(PD100 = POP_DIFF_PREDMax$P_100-POP_DIFF_PREDMax$P_150,
         PD120 = POP_DIFF_PREDMax$P_120-POP_DIFF_PREDMax$P_150,
         PD150 = POP_DIFF_PREDMax$P_150-POP_DIFF_PREDMax$P_150,
         PD180 = POP_DIFF_PREDMax$P_180-POP_DIFF_PREDMax$P_150,
         PD200 = POP_DIFF_PREDMax$P_200-POP_DIFF_PREDMax$P_150) %>% 
  pivot_longer(., cols = starts_with("PD"), names_to = "Predation_Level", values_to = "POP_DIFF")

# PREVELANCE TABLE
PREV_DIFF_PREDMax <- PREDMax_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_DIFF_PREDMax <- PREV_DIFF_PREDMax %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","char","Max_Pred", "Age","Month","Year"),names_to = "Category", values_to= "Count")

# CUMMULATIVE ELAST CWD

sens_name<-c("SCEN7_PREDMax","SCEN8_PREDMax")
cummul_cwd_PREDMax_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PREDMax <- PREDMax_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PREDMax <- cummul_PREDMax %>% mutate(cummul_hunt = cumsum(TotalH),
                                        cummul_cwd = cumsum(TotalCWDdeath),
                                        cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Max_Pred","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_cwd_PREDMax <-cummul_PREDMax %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Max_Pred, values_from = Count)
  cummul_cwd_PREDMax_table<-rbind(cummul_cwd_PREDMax_table, data.frame(cummul_cwd_PREDMax %>% select(Scenario,P_100,P_120,P_150,P_180,P_200)))
}
cummul_cwd_PREDMax_elast<-data.frame()
for (i in 1:length(sens_name)){
  cummul_cwd_PREDMax_elast_1  <-cummul_cwd_PREDMax_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
    mutate(Cummul_cwd100   = ((P_100-P_150)/P_150)/((1-1.5)/1.5),
           Cummul_cwd120 = ((P_120-P_150)/P_150)/((1.2-1.5)/1.5),
           Cummul_cwd150   = rep(0,nrow(cummul_cwd_PREDMax)),
           Cummul_cwd180 =  ((P_180-P_150)/P_150)/((1.8-1.5)/1.5),
           Cummul_cwd200   = ((P_200-P_150)/P_150)/((2-1.5)/1.5))
  cummul_cwd_PREDMax_elast<-rbind(cummul_cwd_PREDMax_elast, data.frame(Scenario=sens_name[i],cummul_cwd_PREDMax_elast_1))
}
cummul_cwd_PREDMax_elast  <-cummul_cwd_PREDMax_elast  %>% 
  pivot_longer(., cols = c("Cummul_cwd100","Cummul_cwd120","Cummul_cwd150","Cummul_cwd180","Cummul_cwd200"), names_to = "Cummulative_cwd", values_to = "Cummul_cwd_Elast")


stargazer(t(cummul_cwd_PREDMax_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "cumul_cwd.tex")

# CUMMULATIVE ELAST HUNTING
sens_name<-c("SCEN6_PREDMax","SCEN8_PREDMax")
cummul_hunt_PREDMax_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PREDMax <- PREDMax_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PREDMax <- cummul_PREDMax %>% mutate(cummul_hunt = cumsum(TotalH),
                                        cummul_cwd = cumsum(TotalCWDdeath),
                                        cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Max_Pred","Age","Month","Year"),names_to = "Category", values_to= "Count")
  huntmod<-vector()
  huntmod[cummul_PREDMax$Month%%12 == 7] <- 1 
  cummul_PREDMax <-cummul_PREDMax %>% add_column(.,huntmod,.after = "Month")
  cummul_PREDMax <- filter(cummul_PREDMax, huntmod>0) 
  
  cummul_hunt_PREDMax <-cummul_PREDMax %>% filter(Month%in% c("235"), Category %in% c("cummul_hunt"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Max_Pred, values_from = Count)
  cummul_hunt_PREDMax_table<-rbind(cummul_hunt_PREDMax_table, data.frame(cummul_hunt_PREDMax %>% select(Scenario,P_100,P_120,P_150,P_180,P_200)))
}

cummul_hunt_PREDMax_elast<-data.frame()
for (i in 1:length(sens_name)){
  cummul_hunt_PREDMax_elast_1  <-cummul_hunt_PREDMax_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
    mutate(Cummul_hunt100   = ((P_100-P_150)/P_150)/((1-1.5)/1.5),
           Cummul_hunt120 = ((P_120-P_150)/P_150)/((1.2-1.5)/1.5),
           Cummul_hunt150   = rep(0,nrow(cummul_hunt_PREDMax)),
           Cummul_hunt180 =  ((P_180-P_150)/P_150)/((1.8-1.5)/1.5),
           Cummul_hunt200   = ((P_200-P_150)/P_150)/((2-1.5)/1.5))
  cummul_hunt_PREDMax_elast<-rbind(cummul_hunt_PREDMax_elast, data.frame(Scenario=sens_name[i],cummul_hunt_PREDMax_elast_1))
}
cummul_hunt_PREDMax_elast  <-cummul_hunt_PREDMax_elast  %>% 
  pivot_longer(., cols = c("Cummul_hunt100","Cummul_hunt120","Cummul_hunt150","Cummul_hunt180","Cummul_hunt200"), names_to = "Cummulative_hunt", values_to = "Cummul_hunt_Elast")

# CUMMULATIVE ELAST PREDATION
sens_name<-c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax")
cummul_pred_PREDMax_table<-data.frame()
for (i in 1:length(sens_name)){
  cummul_PREDMax <- PREDMax_table1 %>% filter(Scenario %in% c(sens_name[i])) %>% pivot_wider(names_from = Category, values_from = Count)
  cummul_PREDMax <- cummul_PREDMax %>% mutate(cummul_hunt = cumsum(TotalH),
                                        cummul_cwd = cumsum(TotalCWDdeath),
                                        cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","char","Max_Pred","Age","Month","Year"),names_to = "Category", values_to= "Count")
  
  cummul_pred_PREDMax <-cummul_PREDMax %>% filter(Year %in% c("20"), Category %in% c("cummul_pred"), Age %in% c("Total_pop")) %>% 
    pivot_wider(names_from = Max_Pred, values_from = Count)
  cummul_pred_PREDMax_table<-rbind(cummul_pred_PREDMax_table, data.frame(cummul_pred_PREDMax %>% select(Scenario,P_100,P_120,P_150,P_180,P_200)))
}
cummul_pred_PREDMax_elast<-data.frame()
for (i in 1:length(sens_name)){
  cummul_pred_PREDMax_elast_1  <-cummul_pred_PREDMax_table %>% filter(Scenario %in% c(sens_name[i])) %>% 
    mutate(Cummul_pred100   = ((P_100-P_150)/P_150)/((1-1.5)/1.5),
           Cummul_pred120 = ((P_120-P_150)/P_150)/((1.2-1.5)/1.5),
           Cummul_pred150   = rep(0,nrow(cummul_pred_PREDMax)),
           Cummul_pred180 =  ((P_180-P_150)/P_150)/((1.8-1.5)/1.5),
           Cummul_pred200   = ((P_200-P_150)/P_150)/((2-1.5)/1.5))
  cummul_pred_PREDMax_elast<-rbind(cummul_pred_PREDMax_elast, data.frame(Scenario=sens_name[i],cummul_pred_PREDMax_elast_1))
}
cummul_pred_PREDMax_elast  <-cummul_pred_PREDMax_elast %>% 
  pivot_longer(., cols = c("Cummul_pred100","Cummul_pred120","Cummul_pred150","Cummul_pred180","Cummul_pred200"), names_to = "Cummulative_pred", values_to = "Cummul_pred_Elast")

# ELASTICITY TABLE to population 
elasticity_PREDMax <-PREDMax_table1 %>% pivot_wider(names_from = Max_Pred, values_from = Count)
elasticity_PREDMax <-elasticity_PREDMax %>% 
  mutate(Elast100   = ((elasticity_PREDMax$P_100-elasticity_PREDMax$P_150)/elasticity_PREDMax$P_150)/((1-1.5)/1.5),
         Elast120 = ((elasticity_PREDMax$P_120-elasticity_PREDMax$P_150)/elasticity_PREDMax$P_150)/((1.2-1.5)/1.5),
         Elast150   = rep(0,nrow(elasticity_PREDMax)),
         Elast180 = ((elasticity_PREDMax$P_180-elasticity_PREDMax$P_150)/elasticity_PREDMax$P_150)/((1.8-1.5)/1.5),
         Elast200   = ((elasticity_PREDMax$P_200-elasticity_PREDMax$P_150)/elasticity_PREDMax$P_150)/((2-1.5)/1.5)) %>% 
  pivot_longer(., cols = starts_with("E"), names_to = "Elasticity_Predation_Level", values_to = "Predation")

# ELASTICITY to prevalence
prev_PREDMax <-PREV_DIFF_PREDMax  %>% filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"), Category %in% c("PREV")) %>% pivot_wider(names_from = Max_Pred, values_from = Count)
prev_PREDMax <-prev_PREDMax %>% 
  mutate(Prev100   = ((prev_PREDMax$P_100-prev_PREDMax$P_150)/prev_PREDMax$P_150)/((1-1.5)/1.5),
         Prev120   = ((prev_PREDMax$P_120-prev_PREDMax$P_150)/prev_PREDMax$P_150)/((1.2-1.5)/1.5),
         Prev150   = rep(0,nrow(prev_PREDMax)),
         Prev180  = ((prev_PREDMax$P_180-prev_PREDMax$P_150)/prev_PREDMax$P_150)/((1.8-1.5)/1.5),
         Prev200  = ((prev_PREDMax$P_200-prev_PREDMax$P_150)/prev_PREDMax$P_150)/((2-1.5)/1.5)) %>% 
  pivot_longer(., cols = starts_with("Prev"), names_to = "Prevalence", values_to = "PREV_Elast")
prev_test<-prev_PREDMax %>% filter(Year %in% c("20"))
# CWD TABLE

cwd_PREDMax <-PREDMax_table1  %>% filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"), Category %in% c("TotalCWDdeath")) %>% pivot_wider(names_from = Max_Pred, values_from = Count)
cwd_PREDMax <-cwd_PREDMax %>% 
  mutate(e_cwd100   = ((cwd_PREDMax$P_100-cwd_PREDMax$P_150)/cwd_PREDMax$P_150)/((1-1.5)/1.5),
         e_cwd120   = ((cwd_PREDMax$P_120-cwd_PREDMax$P_150)/cwd_PREDMax$P_150)/((1.2-1.5)/1.5),
         e_cwd150   = rep(0,nrow(cwd_PREDMax)),
         e_cwd180  = ((cwd_PREDMax$P_180-cwd_PREDMax$P_150)/cwd_PREDMax$P_150)/((1.8-1.5)/1.5),
         e_cwd200  = ((cwd_PREDMax$P_200-cwd_PREDMax$P_150)/cwd_PREDMax$P_150)/((2-1.5)/1.5)) %>% 
  pivot_longer(., cols = starts_with("e_c"), names_to = "CWD", values_to = "CWD_Elast")

list_of_PREDMax_tables<-list("POP_DIFF_PREDMax"=POP_DIFF_PREDMax,"PREV_DIFF_PREDMax"=PREV_DIFF_PREDMax,"elasticity_PREDMax"=elasticity_PREDMax,
                          "prev_PREDMax"=prev_PREDMax,"cwd_PREDMax"=cwd_PREDMax,"cummul_pred_PREDMax_elast"=cummul_pred_PREDMax_elast,
                          "cummul_cwd_PREDMax_elast"=cummul_cwd_PREDMax_elast,"cummul_hunt_PREDMax_elast"=cummul_hunt_PREDMax_elast)
write.xlsx(list_of_PREDMax_tables,"elasticities_and_differences_PREDMax.xlsx")

