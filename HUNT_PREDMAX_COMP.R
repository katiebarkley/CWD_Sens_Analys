
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

# SCEANRIO 8
HUNT0_PREDP_100  <-calcInfection(hunt.mort.ad.m=0,max.predators = 100,H=1,P=1)
HUNT0_PREDP_120  <-calcInfection(hunt.mort.ad.m=0,max.predators = 120,H=1,P=1)
HUNT0_PREDP_150  <-calcInfection(hunt.mort.ad.m=0,max.predators = 150,H=1,P=1)
HUNT0_PREDP_180  <-calcInfection(hunt.mort.ad.m=0,max.predators = 180,H=1,P=1)
HUNT0_PREDP_200  <-calcInfection(hunt.mort.ad.m=0,max.predators = 200,H=1,P=1)
HUNT05_PREDP_100 <-calcInfection(hunt.mort.ad.m=0.05,max.predators = 100,H=1,P=1)
HUNT05_PREDP_120 <-calcInfection(hunt.mort.ad.m=0.05,max.predators = 120,H=1,P=1)
HUNT05_PREDP_150 <-calcInfection(hunt.mort.ad.m=0.05,max.predators = 150,H=1,P=1)
HUNT05_PREDP_180 <-calcInfection(hunt.mort.ad.m=0.05,max.predators = 180,H=1,P=1)
HUNT05_PREDP_200 <-calcInfection(hunt.mort.ad.m=0.05,max.predators = 200,H=1,P=1)
HUNT10_PREDP_100 <-calcInfection(hunt.mort.ad.m=0.1,max.predators = 100,H=1,P=1)
HUNT10_PREDP_120 <-calcInfection(hunt.mort.ad.m=0.1,max.predators = 120,H=1,P=1)
HUNT10_PREDP_150 <-calcInfection(hunt.mort.ad.m=0.1,max.predators = 150,H=1,P=1)
HUNT10_PREDP_180 <-calcInfection(hunt.mort.ad.m=0.1,max.predators = 180,H=1,P=1)
HUNT10_PREDP_200 <-calcInfection(hunt.mort.ad.m=0.1,max.predators = 200,H=1,P=1)
HUNT15_PREDP_100 <-calcInfection(hunt.mort.ad.m=0.15,max.predators = 100,H=1,P=1)
HUNT15_PREDP_120 <-calcInfection(hunt.mort.ad.m=0.15,max.predators = 120,H=1,P=1)
HUNT15_PREDP_150 <-calcInfection(hunt.mort.ad.m=0.15,max.predators = 150,H=1,P=1)
HUNT15_PREDP_180 <-calcInfection(hunt.mort.ad.m=0.15,max.predators = 180,H=1,P=1)
HUNT15_PREDP_200 <-calcInfection(hunt.mort.ad.m=0.15,max.predators = 200,H=1,P=1)
HUNT20_PREDP_100 <-calcInfection(hunt.mort.ad.m=0.2,max.predators = 100,H=1,P=1)
HUNT20_PREDP_120 <-calcInfection(hunt.mort.ad.m=0.2,max.predators = 120,H=1,P=1)
HUNT20_PREDP_150 <-calcInfection(hunt.mort.ad.m=0.2,max.predators = 150,H=1,P=1)
HUNT20_PREDP_180 <-calcInfection(hunt.mort.ad.m=0.2,max.predators = 180,H=1,P=1)
HUNT20_PREDP_200 <-calcInfection(hunt.mort.ad.m=0.2,max.predators = 200,H=1,P=1)
SCEN8_Hunt_PREDMax       <-list(HUNT0_PREDP_100=HUNT0_PREDP_100, HUNT0_PREDP_120=HUNT0_PREDP_120,
                                HUNT0_PREDP_150=HUNT0_PREDP_150, HUNT0_PREDP_180=HUNT0_PREDP_180,
                                HUNT0_PREDP_200=HUNT0_PREDP_200, HUNT05_PREDP_100=HUNT05_PREDP_100, 
                                HUNT05_PREDP_120=HUNT05_PREDP_120, HUNT05_PREDP_150=HUNT05_PREDP_150, 
                                HUNT05_PREDP_180=HUNT05_PREDP_180, HUNT05_PREDP_200=HUNT05_PREDP_200,
                                HUNT10_PREDP_100=HUNT10_PREDP_100, HUNT10_PREDP_120=HUNT10_PREDP_120,
                                HUNT10_PREDP_150=HUNT10_PREDP_150, HUNT10_PREDP_180=HUNT10_PREDP_180,
                                HUNT10_PREDP_200=HUNT10_PREDP_200, HUNT15_PREDP_100=HUNT15_PREDP_100, 
                                HUNT15_PREDP_120=HUNT15_PREDP_120, HUNT15_PREDP_150=HUNT15_PREDP_150,
                                HUNT15_PREDP_180=HUNT15_PREDP_180, HUNT15_PREDP_200=HUNT15_PREDP_200,
                                HUNT20_PREDP_100=HUNT20_PREDP_100, HUNT20_PREDP_120=HUNT20_PREDP_120,
                                HUNT20_PREDP_150=HUNT20_PREDP_150, HUNT20_PREDP_180=HUNT20_PREDP_180,
                                HUNT20_PREDP_200=HUNT20_PREDP_200 )

# COMBINING ALL SCENARIOS INTO ONE LIST

Hunt_Predmax<-list(SCEN8_Hunt_PREDMax=SCEN8_Hunt_PREDMax)
# LOOP COMBINING ALL SCENARIOS IN LONG FORM
Hunt_PREDMax_table<-data.frame()
for(k in 1:length(Hunt_Predmax)){
  for (j in 1:length(Hunt_Predmax[[k]])){
    for (w in 1:length(Hunt_Predmax[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        Hunt_PREDMax_table<- rbind(Hunt_PREDMax_table, data.frame(Scenario = names(Hunt_Predmax)[k],
                                                                  Hunt_rate_Max_Pred = names(Hunt_Predmax[[k]])[j],
                                                                  Category = names(Hunt_Predmax[[k]][[j]])[w],
                                                                  Age = names(ages)[i],
                                                                  Month = 1:ncol(Hunt_Predmax[[k]][[j]][[w]]),
                                                                  Count = sapply(1:ncol(Hunt_Predmax[[k]][[j]][[w]]), FUN = function(x) {sum(Hunt_Predmax[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}
Hunt_PREDMax_table<- Hunt_PREDMax_table %>% separate(Hunt_rate_Max_Pred, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                                               Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                                               Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                                               Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                                               Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                                               Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                                               Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                                               Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                                               Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                                               Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))


Hunt_PREDMax_table1<- Hunt_PREDMax_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(Hunt_PREDMax_table1))){
  Total_pop = Hunt_PREDMax_table1$Juv+Hunt_PREDMax_table1$Adult+Hunt_PREDMax_table1$Old
}
Hunt_PREDMax_table1<- add_column(Hunt_PREDMax_table1,Total_pop,.after = "Old")
Hunt_PREDMax_table1<-Hunt_PREDMax_table1 %>% pivot_longer(. , cols = !c("Scenario","Hunt_rate_Max_Pred","Hunt","Max_Pred","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

###### ECON analysis #########
nm_value_sen<-c("0","250","500")
Hunt_PREDMax_econ<-data.frame()
Hunt_PREDMax_econ_table<- Hunt_PREDMax_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  Hunt_PREDMax_econ<- rbind(Hunt_PREDMax_econ, data.frame(Hunt_PREDMax_econ_table %>% 
                                                                    mutate(MEAT_VALUE = 5080*TotalH,
                                                                           NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                           NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                           nm_sen = as.integer(nm_value_sen[i]))))
}

Hunt_PREDMax_econ_sen<- Hunt_PREDMax_econ %>% pivot_longer(. , cols = !c("Scenario","Hunt_rate_Max_Pred","Hunt","Max_Pred","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# NET BENEFIT AND HUNTING TABLE

# Total_pop
nb_huntPredmax_table<-Hunt_PREDMax_econ_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,Hunt_rate_Max_Pred,Hunt,Max_Pred,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_huntPredmax_table$Month%%12 == 7] <- 1 
mod[nb_huntPredmax_table$Month%%12 == 8] <- 2 
nb_huntPredmax_table<-nb_huntPredmax_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat
  ) %>% replace(is.na(.), 0)
nb_huntPredmax_table <- filter(nb_huntPredmax_table, mod > 0) 
#stargazer(t(nb_huntPredmax_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_all.tex")

NB_econ_table<-nb_huntPredmax_table %>% 
  pivot_longer(. , cols = !c("Scenario","Hunt_rate_Max_Pred","Hunt","Max_Pred","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nm_value_sen<-c("0","250","500")
NB_all_table<-data.frame()
for (j in c(10:34)){
for (i in c(1,2,3)){
  NB_econ<-NB_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Hunt_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
  Net_Ben<-sum(NB_econ[,j])
  NB_all_table<-rbind(NB_all_table, data.frame(hunt_pred_comp=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                               Net_Ben))
}
}

NB_all_table<- NB_all_table %>% separate(hunt_pred_comp, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = TRUE) 
NB_all_table<-NB_all_table %>% pivot_wider(., names_from = "Max_Pred", values_from= "Net_Ben")

# stargazer(t(NB_all_table),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "NB_all.tex")

#final susceptible population, final cwd prevalence, and cumulative CWD deaths

# susceptible pop
suspop_table<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    suspop_econ<-Hunt_PREDMax_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = Hunt_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    sus_pop<-sum(suspop_econ[,j])
    suspop_table<-rbind(suspop_table, data.frame(Year=Year_sen[i],hunt_pred_comp=names(suspop_econ[,j]),sus_pop))
  }
}
suspop_table<- suspop_table %>% separate(hunt_pred_comp, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = TRUE) 
suspop_table<-suspop_table %>% pivot_wider(., names_from = "Max_Pred", values_from= "sus_pop")


# infected pop
infpop_table<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
for (j in c(9:33)){
  infpop_econ<-Hunt_PREDMax_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = Hunt_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
  inf_pop<-sum(infpop_econ[,j])
  infpop_table<-rbind(infpop_table, data.frame(Year=Year_sen[i],hunt_pred_comp=names(infpop_econ[,j]),inf_pop))
}
}

infpop_table<- infpop_table %>% separate(hunt_pred_comp, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = TRUE) 
infpop_table<-infpop_table %>% pivot_wider(., names_from = "Max_Pred", values_from= "inf_pop")

#total pop
totpop_table<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    totpop_econ<-Hunt_PREDMax_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = Hunt_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    tot_pop<-sum(totpop_econ[,j])
    totpop_table<-rbind(totpop_table, data.frame(Year=Year_sen[i],hunt_pred_comp=names(totpop_econ[,j]),tot_pop))
  }
}

totpop_table<- totpop_table %>% separate(hunt_pred_comp, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = TRUE)
totpop_table<-totpop_table %>% pivot_wider(., names_from = "Max_Pred", values_from= "tot_pop")

# final cwd prevalence
PREV_hunt_premax <- Hunt_PREDMax_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_hunt_premax <- PREV_hunt_premax %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","Hunt_rate_Max_Pred","Hunt","Max_Pred","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

prev_table<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year)){
for (j in c(8:32)){
  prev_econ<-PREV_hunt_premax %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = Hunt_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
  prev_pop<-sum(prev_econ[,j])/12
  prev_table<-rbind(prev_table, data.frame(Year=Year_sen[i],hunt_pred_comp=names(prev_econ[,j]),prev_pop))
}
}

prev_table<- prev_table %>% separate(hunt_pred_comp, into = c("Hunt","Max_Pred"), sep = "_PRED", remove = TRUE) 
prev_table<- prev_table %>% pivot_wider(., names_from = "Max_Pred", values_from= "prev_pop")

# cummulative CWD


cummul_huntpred <- Hunt_PREDMax_table1 %>% pivot_wider(names_from = Category, values_from = Count)
cummul_huntpred <- cummul_huntpred %>% mutate(cummul_hunt = cumsum(TotalH),
                                              cummul_cwd = cumsum(TotalCWDdeath),
                                              cummul_pred = cumsum(TotalPreddeath)) %>% 
    pivot_longer(., cols = !c("Scenario","Hunt_rate_Max_Pred","Hunt","Max_Pred","Age",
                              "Month","Year"),names_to = "Category", values_to= "Count")
  
cummul_cwd_huntpred <-cummul_huntpred %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) 
cummul_cwd_huntpred <- cummul_cwd_huntpred %>% select("Hunt","Max_Pred","Month","Count") %>% 
  pivot_wider(names_from = Max_Pred,values_from = "Count")

# stargazer(t(cummul_cwd_huntpred),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "cumul_cwd.tex")

list_of_compM<-list("NB_all_table"=NB_all_table,"suspop_table"=suspop_table, "infpop_table"=infpop_table,"totpop_table"=totpop_table,"prev_table"=prev_table, "cummul_cwd"=cummul_cwd_huntpred)
write.xlsx(list_of_compM, file = "./NB_hunt_predmax_comp.xlsx")


# FEMALE HUNTING AND PREDATORS 

# SCEANRIO 8
HUNT0_PREDP_100  <-calcInfection(hunt.mort.ad.f=0,max.predators = 100,H=1,P=1)
HUNT0_PREDP_120  <-calcInfection(hunt.mort.ad.f=0,max.predators = 120,H=1,P=1)
HUNT0_PREDP_150  <-calcInfection(hunt.mort.ad.f=0,max.predators = 150,H=1,P=1)
HUNT0_PREDP_180  <-calcInfection(hunt.mort.ad.f=0,max.predators = 180,H=1,P=1)
HUNT0_PREDP_200  <-calcInfection(hunt.mort.ad.f=0,max.predators = 200,H=1,P=1)
HUNT025_PREDP_100 <-calcInfection(hunt.mort.ad.f=0.025,max.predators = 100,H=1,P=1)
HUNT025_PREDP_120 <-calcInfection(hunt.mort.ad.f=0.025,max.predators = 120,H=1,P=1)
HUNT025_PREDP_150 <-calcInfection(hunt.mort.ad.f=0.025,max.predators = 150,H=1,P=1)
HUNT025_PREDP_180 <-calcInfection(hunt.mort.ad.f=0.025,max.predators = 180,H=1,P=1)
HUNT025_PREDP_200 <-calcInfection(hunt.mort.ad.f=0.025,max.predators = 200,H=1,P=1)
HUNT05_PREDP_100 <-calcInfection(hunt.mort.ad.f=0.05,max.predators = 100,H=1,P=1)
HUNT05_PREDP_120 <-calcInfection(hunt.mort.ad.f=0.05,max.predators = 120,H=1,P=1)
HUNT05_PREDP_150 <-calcInfection(hunt.mort.ad.f=0.05,max.predators = 150,H=1,P=1)
HUNT05_PREDP_180 <-calcInfection(hunt.mort.ad.f=0.05,max.predators = 180,H=1,P=1)
HUNT05_PREDP_200 <-calcInfection(hunt.mort.ad.f=0.05,max.predators = 200,H=1,P=1)
HUNT075_PREDP_100 <-calcInfection(hunt.mort.ad.f=0.075,max.predators = 100,H=1,P=1)
HUNT075_PREDP_120 <-calcInfection(hunt.mort.ad.f=0.075,max.predators = 120,H=1,P=1)
HUNT075_PREDP_150 <-calcInfection(hunt.mort.ad.f=0.075,max.predators = 150,H=1,P=1)
HUNT075_PREDP_180 <-calcInfection(hunt.mort.ad.f=0.075,max.predators = 180,H=1,P=1)
HUNT075_PREDP_200 <-calcInfection(hunt.mort.ad.f=0.075,max.predators = 200,H=1,P=1)
HUNT10_PREDP_100 <-calcInfection(hunt.mort.ad.f=0.1,max.predators = 100,H=1,P=1)
HUNT10_PREDP_120 <-calcInfection(hunt.mort.ad.f=0.1,max.predators = 120,H=1,P=1)
HUNT10_PREDP_150 <-calcInfection(hunt.mort.ad.f=0.1,max.predators = 150,H=1,P=1)
HUNT10_PREDP_180 <-calcInfection(hunt.mort.ad.f=0.1,max.predators = 180,H=1,P=1)
HUNT10_PREDP_200 <-calcInfection(hunt.mort.ad.f=0.1,max.predators = 200,H=1,P=1)
SCEN8_HuntF_PREDMax       <-list(HUNT0_PREDP_100=HUNT0_PREDP_100, HUNT0_PREDP_120=HUNT0_PREDP_120,
                                HUNT0_PREDP_150=HUNT0_PREDP_150, HUNT0_PREDP_180=HUNT0_PREDP_180,
                                HUNT0_PREDP_200=HUNT0_PREDP_200, HUNT025_PREDP_100=HUNT025_PREDP_100, 
                                HUNT025_PREDP_120=HUNT025_PREDP_120, HUNT025_PREDP_150=HUNT025_PREDP_150, 
                                HUNT025_PREDP_180=HUNT025_PREDP_180, HUNT025_PREDP_200=HUNT025_PREDP_200,
                                HUNT05_PREDP_100=HUNT05_PREDP_100, HUNT05_PREDP_120=HUNT05_PREDP_120,
                                HUNT05_PREDP_150=HUNT05_PREDP_150, HUNT05_PREDP_180=HUNT05_PREDP_180,
                                HUNT05_PREDP_200=HUNT05_PREDP_200, HUNT075_PREDP_100=HUNT075_PREDP_100, 
                                HUNT075_PREDP_120=HUNT075_PREDP_120, HUNT075_PREDP_150=HUNT075_PREDP_150,
                                HUNT075_PREDP_180=HUNT075_PREDP_180, HUNT075_PREDP_200=HUNT075_PREDP_200,
                                HUNT10_PREDP_100=HUNT10_PREDP_100, HUNT10_PREDP_120=HUNT10_PREDP_120,
                                HUNT10_PREDP_150=HUNT10_PREDP_150, HUNT10_PREDP_180=HUNT10_PREDP_180,
                                HUNT10_PREDP_200=HUNT10_PREDP_200 )

# COMBINING ALL SCENARIOS INTO ONE LIST

HuntF_Predmax<-list(SCEN8_HuntF_PREDMax=SCEN8_HuntF_PREDMax)
# LOOP COMBINING ALL SCENARIOS IN LONG FORM
HuntF_PREDMax_table<-data.frame()
for(k in 1:length(HuntF_Predmax)){
  for (j in 1:length(HuntF_Predmax[[k]])){
    for (w in 1:length(HuntF_Predmax[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        HuntF_PREDMax_table<- rbind(HuntF_PREDMax_table, data.frame(Scenario = names(HuntF_Predmax)[k],
                                                                  HuntF_rate_Max_Pred = names(HuntF_Predmax[[k]])[j],
                                                                  Category = names(HuntF_Predmax[[k]][[j]])[w],
                                                                  Age = names(ages)[i],
                                                                  Month = 1:ncol(HuntF_Predmax[[k]][[j]][[w]]),
                                                                  Count = sapply(1:ncol(HuntF_Predmax[[k]][[j]][[w]]), FUN = function(x) {sum(HuntF_Predmax[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}
HuntF_PREDMax_table<- HuntF_PREDMax_table %>% separate(HuntF_rate_Max_Pred, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                                               Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                                               Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                                               Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                                               Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                                               Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                                               Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                                               Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                                               Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                                               Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))



HuntF_PREDMax_table1<- HuntF_PREDMax_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(HuntF_PREDMax_table1))){
  Total_pop = HuntF_PREDMax_table1$Juv+HuntF_PREDMax_table1$Adult+HuntF_PREDMax_table1$Old
}
HuntF_PREDMax_table1<- add_column(HuntF_PREDMax_table1,Total_pop,.after = "Old")
HuntF_PREDMax_table1<-HuntF_PREDMax_table1 %>% pivot_longer(. , cols = !c("Scenario","HuntF_rate_Max_Pred","HuntF","Max_Pred","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

###### ECON analysis #########
nm_value_sen<-c("0","250","500")
HuntF_PREDMax_econ<-data.frame()
HuntF_PREDMax_econ_table<- HuntF_PREDMax_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  HuntF_PREDMax_econ<- rbind(HuntF_PREDMax_econ, data.frame(HuntF_PREDMax_econ_table %>% 
                                                            mutate(MEAT_VALUE = 5080*TotalH,
                                                                   NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                   NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                   nm_sen = as.integer(nm_value_sen[i]))))
}

HuntF_PREDMax_econ_sen<- HuntF_PREDMax_econ %>% pivot_longer(. , cols = !c("Scenario","HuntF_rate_Max_Pred","HuntF","Max_Pred","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# NET BENEFIT AND HUNTING TABLE

# Total_pop
nb_huntFPredmax_table<-HuntF_PREDMax_econ_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,HuntF_rate_Max_Pred,HuntF,Max_Pred,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_huntFPredmax_table$Month%%12 == 7] <- 1 
mod[nb_huntFPredmax_table$Month%%12 == 8] <- 2 
nb_huntFPredmax_table<-nb_huntFPredmax_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat
  ) %>% replace(is.na(.), 0)
nb_huntFPredmax_table <- filter(nb_huntFPredmax_table, mod > 0) 
#stargazer(t(nb_huntFPredmax_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_allF.tex")

NB_econ_table<-nb_huntFPredmax_table %>% 
  pivot_longer(. , cols = !c("Scenario","HuntF_rate_Max_Pred","HuntF","Max_Pred","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nm_value_sen<-c("0","250","500")
NB_all_tableF<-data.frame()
for (j in c(10:34)){
  for (i in c(1,2,3)){
    NB_econ<-NB_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben<-sum(NB_econ[,j])
    NB_all_tableF<-rbind(NB_all_tableF, data.frame(hunt_pred_comp=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                                 Net_Ben))
  }
}

NB_all_tableF<- NB_all_tableF %>% separate(hunt_pred_comp, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = TRUE) 
NB_all_tableF<-NB_all_tableF %>% pivot_wider(., names_from = "Max_Pred", values_from= "Net_Ben")

# stargazer(t(NB_all_tableF),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "NB_allF.tex")

#final susceptible population, final cwd prevalence, and cumulative CWD deaths

# susceptible pop
suspop_tableF<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    suspop_econ<-HuntF_PREDMax_econ_sen %>% filter(Category %in% c("TotalSprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    sus_pop<-sum(suspop_econ[,j])
    suspop_tableF<-rbind(suspop_tableF, data.frame(Year=Year_sen[i],hunt_pred_comp=names(suspop_econ[,j]),sus_pop))
  }
}
suspop_tableF<- suspop_tableF %>% separate(hunt_pred_comp, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = TRUE) 
suspop_tableF<-suspop_tableF %>% pivot_wider(., names_from = "Max_Pred", values_from= "sus_pop")


# infected pop
infpop_tableF<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    infpop_econ<-HuntF_PREDMax_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    inf_pop<-sum(infpop_econ[,j])
    infpop_tableF<-rbind(infpop_tableF, data.frame(Year=Year_sen[i],hunt_pred_comp=names(infpop_econ[,j]),inf_pop))
  }
}

infpop_tableF<- infpop_tableF %>% separate(hunt_pred_comp, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = TRUE) 
infpop_tableF<-infpop_tableF %>% pivot_wider(., names_from = "Max_Pred", values_from= "inf_pop")

#total pop
totpop_tableF<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    totpop_econ<-HuntF_PREDMax_econ_sen %>% filter(Category %in% c("Totalprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    tot_pop<-sum(totpop_econ[,j])
    totpop_tableF<-rbind(totpop_tableF, data.frame(Year=Year_sen[i],hunt_pred_comp=names(totpop_econ[,j]),tot_pop))
  }
}

totpop_tableF<- totpop_tableF %>% separate(hunt_pred_comp, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = TRUE)
totpop_tableF<-totpop_tableF %>% pivot_wider(., names_from = "Max_Pred", values_from= "tot_pop")


# final cwd prevalence
PREV_hunt_premaxF <- HuntF_PREDMax_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_hunt_premaxF <- PREV_hunt_premaxF %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF_rate_Max_Pred","HuntF","Max_Pred","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

prev_tableF<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(8:32)){
    prev_econ<-PREV_hunt_premaxF %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    prev_pop<-sum(prev_econ[,j])/12
    prev_tableF<-rbind(prev_tableF, data.frame(Year=Year_sen[i],hunt_pred_comp=names(prev_econ[,j]),prev_pop))
  }
}

prev_tableF<- prev_tableF %>% separate(hunt_pred_comp, into = c("HuntF","Max_Pred"), sep = "_PRED", remove = TRUE) 
prev_tableF<- prev_tableF %>% pivot_wider(., names_from = "Max_Pred", values_from= "prev_pop")

# cummulative CWD


cummul_huntpredF <- HuntF_PREDMax_table1 %>% pivot_wider(names_from = Category, values_from = Count)
cummul_huntpredF <- cummul_huntpredF %>% mutate(cummul_hunt = cumsum(TotalH),
                                              cummul_cwd = cumsum(TotalCWDdeath),
                                              cummul_pred = cumsum(TotalPreddeath)) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF_rate_Max_Pred","HuntF","Max_Pred","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

cummul_cwd_huntpredF <-cummul_huntpredF %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) 
cummul_cwd_huntpredF <- cummul_cwd_huntpredF %>% select("HuntF","Max_Pred","Month","Count") %>% 
  pivot_wider(names_from = Max_Pred,values_from = "Count")

# stargazer(t(cummul_cwd_huntpredF),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "cumul_cwdF.tex")

list_of_compF<-list("NB_all_tableF"=NB_all_tableF,"suspop_tableF"=suspop_tableF, "infpop_tableF"=infpop_tableF,"totpop_tableF"=totpop_tableF,"prev_tableF"=prev_tableF, "cummul_cwdF"=cummul_cwd_huntpredF)
write.xlsx(list_of_compF, file = "./NB_huntF_predmax_comp.xlsx")



# FEMALE HUTNING AND MALE HUNTING
# SCEANRIO 8
HUNTF0_HHUNTM0  <-calcInfection(hunt.mort.ad.f=0,hunt.mort.ad.m=0,H=1,P=1)
HUNTF0_HHUNTM05  <-calcInfection(hunt.mort.ad.f=0,hunt.mort.ad.m=0.05,H=1,P=1)
HUNTF0_HHUNTM1  <-calcInfection(hunt.mort.ad.f=0,hunt.mort.ad.m=0.1,H=1,P=1)
HUNTF0_HHUNTM15  <-calcInfection(hunt.mort.ad.f=0,hunt.mort.ad.m=0.15,H=1,P=1)
HUNTF0_HHUNTM2  <-calcInfection(hunt.mort.ad.f=0,hunt.mort.ad.m=0.2,H=1,P=1)

HUNTF025_HHUNTM0 <-calcInfection(hunt.mort.ad.f=0.025,hunt.mort.ad.m=0,H=1,P=1)
HUNTF025_HHUNTM05 <-calcInfection(hunt.mort.ad.f=0.025,hunt.mort.ad.m=0.05,H=1,P=1)
HUNTF025_HHUNTM1 <-calcInfection(hunt.mort.ad.f=0.025,hunt.mort.ad.m=0.1,H=1,P=1)
HUNTF025_HHUNTM15 <-calcInfection(hunt.mort.ad.f=0.025,hunt.mort.ad.m=0.15,H=1,P=1)
HUNTF025_HHUNTM2 <-calcInfection(hunt.mort.ad.f=0.025,hunt.mort.ad.m=0.2,H=1,P=1)

HUNTF05_HHUNTM0 <-calcInfection(hunt.mort.ad.f=0.05,hunt.mort.ad.m=0,H=1,P=1)
HUNTF05_HHUNTM05 <-calcInfection(hunt.mort.ad.f=0.05,hunt.mort.ad.m=0.05,H=1,P=1)
HUNTF05_HHUNTM1 <-calcInfection(hunt.mort.ad.f=0.05,hunt.mort.ad.m=0.1,H=1,P=1)
HUNTF05_HHUNTM15 <-calcInfection(hunt.mort.ad.f=0.05,hunt.mort.ad.m=0.15,H=1,P=1)
HUNTF05_HHUNTM2 <-calcInfection(hunt.mort.ad.f=0.05,hunt.mort.ad.m=0.2,H=1,P=1)

HUNTF075_HHUNTM0 <-calcInfection(hunt.mort.ad.f=0.075,hunt.mort.ad.m=0,H=1,P=1)
HUNTF075_HHUNTM05 <-calcInfection(hunt.mort.ad.f=0.075,hunt.mort.ad.m=0.05,H=1,P=1)
HUNTF075_HHUNTM1 <-calcInfection(hunt.mort.ad.f=0.075,hunt.mort.ad.m=0.1,H=1,P=1)
HUNTF075_HHUNTM15 <-calcInfection(hunt.mort.ad.f=0.075,hunt.mort.ad.m=0.15,H=1,P=1)
HUNTF075_HHUNTM2 <-calcInfection(hunt.mort.ad.f=0.075,hunt.mort.ad.m=0.2,H=1,P=1)

HUNTF10_HHUNTM0 <-calcInfection(hunt.mort.ad.f=0.1,hunt.mort.ad.m=0,H=1,P=1)
HUNTF10_HHUNTM05 <-calcInfection(hunt.mort.ad.f=0.1,hunt.mort.ad.m=0.05,H=1,P=1)
HUNTF10_HHUNTM1 <-calcInfection(hunt.mort.ad.f=0.1,hunt.mort.ad.m=0.1,H=1,P=1)
HUNTF10_HHUNTM15 <-calcInfection(hunt.mort.ad.f=0.1,hunt.mort.ad.m=0.15,H=1,P=1)
HUNTF10_HHUNTM2 <-calcInfection(hunt.mort.ad.f=0.1,hunt.mort.ad.m=0.2,H=1,P=1)
SCEN8_HuntF_HuntM       <-list(HUNTF0_HHUNTM0=HUNTF0_HHUNTM0,HUNTF0_HHUNTM05=HUNTF0_HHUNTM05,
                               HUNTF0_HHUNTM1=HUNTF0_HHUNTM1,HUNTF0_HHUNTM15=HUNTF0_HHUNTM15,
                               HUNTF0_HHUNTM2=HUNTF0_HHUNTM2,HUNTF025_HHUNTM0=HUNTF025_HHUNTM0,
                               HUNTF025_HHUNTM05=HUNTF025_HHUNTM05,HUNTF025_HHUNTM1=HUNTF025_HHUNTM1,
                               HUNTF025_HHUNTM15=HUNTF025_HHUNTM15,HUNTF025_HHUNTM2=HUNTF025_HHUNTM2,
                               HUNTF05_HHUNTM0=HUNTF05_HHUNTM0,HUNTF05_HHUNTM05=HUNTF05_HHUNTM05,
                               HUNTF05_HHUNTM1=HUNTF05_HHUNTM1,HUNTF05_HHUNTM15=HUNTF05_HHUNTM15,
                               HUNTF05_HHUNTM2=HUNTF05_HHUNTM2,HUNTF075_HHUNTM0=HUNTF075_HHUNTM0,
                               HUNTF075_HHUNTM05=HUNTF075_HHUNTM05,HUNTF075_HHUNTM1=HUNTF075_HHUNTM1,
                               HUNTF075_HHUNTM15=HUNTF075_HHUNTM15,HUNTF075_HHUNTM2=HUNTF075_HHUNTM2,
                               HUNTF10_HHUNTM0=HUNTF10_HHUNTM0,HUNTF10_HHUNTM05=HUNTF10_HHUNTM05,
                               HUNTF10_HHUNTM1=HUNTF10_HHUNTM1,HUNTF10_HHUNTM15=HUNTF10_HHUNTM15,
                               HUNTF10_HHUNTM2=HUNTF10_HHUNTM2)

# COMBINING ALL SCENARIOS INTO ONE LIST

HuntF_HuntM<-list(SCEN8_HuntF_HuntM =SCEN8_HuntF_HuntM )
names(HuntF_HuntM[1])
# LOOP COMBINING ALL SCENARIOS IN LONG FORM
HuntF_HuntM_table<-data.frame()
for(k in 1:length(HuntF_HuntM)){
  for (j in 1:length(HuntF_HuntM[[k]])){
    for (w in 1:length(HuntF_HuntM[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        HuntF_HuntM_table<- rbind(HuntF_HuntM_table, data.frame(Scenario = names(HuntF_HuntM)[k],
                                                                    HuntF_rate_HuntM_rate = names(HuntF_HuntM[[k]])[j],
                                                                    Category = names(HuntF_HuntM[[k]][[j]])[w],
                                                                    Age = names(ages)[i],
                                                                    Month = 1:ncol(HuntF_HuntM[[k]][[j]][[w]]),
                                                                    Count = sapply(1:ncol(HuntF_HuntM[[k]][[j]][[w]]), FUN = function(x) {sum(HuntF_HuntM[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}
HuntF_HuntM_table<- HuntF_HuntM_table %>% separate(HuntF_rate_HuntM_rate, into = c("HuntF","HuntM"), sep = "_H", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                                                   Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                                                   Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                                                   Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                                                   Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                                                   Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                                                   Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                                                   Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                                                   Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                                                   Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))


HuntF_HuntM_table1<- HuntF_HuntM_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(HuntF_HuntM_table1))){
  Total_pop = HuntF_HuntM_table1$Juv+HuntF_HuntM_table1$Adult+HuntF_HuntM_table1$Old
}
HuntF_HuntM_table1<- add_column(HuntF_HuntM_table1,Total_pop,.after = "Old")
HuntF_HuntM_table1<-HuntF_HuntM_table1 %>% pivot_longer(. , cols = !c("Scenario","HuntF_rate_HuntM_rate","HuntF","HuntM","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

###### ECON analysis #########
nm_value_sen<-c("0","250","500")
HuntF_HuntM_econ<-data.frame()
HuntF_HuntM_econ_table<- HuntF_HuntM_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  HuntF_HuntM_econ<- rbind(HuntF_HuntM_econ, data.frame(HuntF_HuntM_econ_table %>% 
                                                              mutate(MEAT_VALUE = 5080*TotalH,
                                                                     NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                     NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                     nm_sen = as.integer(nm_value_sen[i]))))
}

HuntF_HuntM_econ_sen<- HuntF_HuntM_econ %>% pivot_longer(. , cols = !c("Scenario","HuntF_rate_HuntM_rate","HuntF","HuntM","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# NET BENEFIT AND HUNTING TABLE

# Total_pop
nb_huntFhuntM_table<-HuntF_HuntM_econ_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,HuntF_rate_HuntM_rate,HuntF,HuntM,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_huntFhuntM_table$Month%%12 == 7] <- 1 
mod[nb_huntFhuntM_table$Month%%12 == 8] <- 2 
nb_huntFhuntM_table<-nb_huntFhuntM_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat
  ) %>% replace(is.na(.), 0)
nb_huntFhuntM_table <- filter(nb_huntFhuntM_table, mod > 0) 
#stargazer(t(nb_huntFhuntM_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_allFM.tex")

NB_econ_table<-nb_huntFhuntM_table %>% 
  pivot_longer(. , cols = !c("Scenario","HuntF_rate_HuntM_rate","HuntF","HuntM","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nm_value_sen<-c("0","250","500")
NB_all_tableFM<-data.frame()
for (j in c(10:34)){
  for (i in c(1,2,3)){
    NB_econ<-NB_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_HuntM_rate, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben<-sum(NB_econ[,j])
    NB_all_tableFM<-rbind(NB_all_tableFM, data.frame(hunt_pred_comp=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                                  Net_Ben))
  }
}

NB_all_tableFM<- NB_all_tableFM %>% separate(hunt_pred_comp, into = c("HuntF","HuntM"), sep = "_H", remove = TRUE) 
NB_all_tableFM<-NB_all_tableFM %>% pivot_wider(., names_from = "HuntM", values_from= "Net_Ben")

# stargazer(t(NB_all_tableFM),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "NB_allFM.tex")

#final susceptible population, final cwd prevalence, and cumulative CWD deaths

# susceptible pop


suspop_tableFM<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    suspop_econ<-HuntF_HuntM_econ_sen %>% filter(Category %in% c("TotalSprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from =  HuntF_rate_HuntM_rate, values_from=Count) %>%  replace(is.na(.), 0)
    sus_pop<-sum(suspop_econ[,j])
    suspop_tableFM<-rbind(suspop_tableFM, data.frame(Year=Year_sen[i],hunt_pred_comp=names(suspop_econ[,j]),sus_pop))
  }
}
suspop_tableFM<- suspop_tableFM %>% separate(hunt_pred_comp, into = c("HuntF","HuntM"), sep = "_H", remove = TRUE) 
suspop_tableFM<-suspop_tableFM %>% pivot_wider(., names_from = "HuntM", values_from= "sus_pop")


# infected pop
infpop_tableFM<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    infpop_econ<-HuntF_HuntM_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_HuntM_rate, values_from=Count) %>%  replace(is.na(.), 0)
    inf_pop<-sum(infpop_econ[,j])
    infpop_tableFM<-rbind(infpop_tableFM, data.frame(Year=Year_sen[i],hunt_pred_comp=names(infpop_econ[,j]),inf_pop))
  }
}

infpop_tableFM<- infpop_tableFM %>% separate(hunt_pred_comp, into = c("HuntF","HuntM"), sep = "_H", remove = TRUE) 
infpop_tableFM<-infpop_tableFM %>% pivot_wider(., names_from = "HuntM", values_from= "inf_pop")

#total pop
totpop_tableFM<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:33)){
    totpop_econ<-HuntF_HuntM_econ_sen %>% filter(Category %in% c("Totalprey"),Year %in% c(Year_sen[i]))  %>% pivot_wider(names_from = HuntF_rate_Max_Pred, values_from=Count) %>%  replace(is.na(.), 0)
    tot_pop<-sum(totpop_econ[,j])
    totpop_tableFM<-rbind(totpop_tableFM, data.frame(Year=Year_sen[i],hunt_pred_comp=names(totpop_econ[,j]),tot_pop))
  }
}

totpop_tableFM<- totpop_tableFM %>% separate(hunt_pred_comp, into = c("HuntF","HuntM"), sep = "_PRED", remove = TRUE)
totpop_tableFM<-totpop_tableFM %>% pivot_wider(., names_from = "HuntM", values_from= "tot_pop")

# final cwd prevalence
PREV_hunt_premaxFM <- HuntF_HuntM_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_hunt_premaxFM <- PREV_hunt_premaxFM %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF_rate_HuntM_rate","HuntF","HuntM","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

prev_tableFM<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(8:32)){
    prev_econ<-PREV_hunt_premaxFM %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year[i])) %>% pivot_wider(names_from = HuntF_rate_HuntM_rate, values_from=Count) %>%  replace(is.na(.), 0)
    prev_pop<-sum(prev_econ[,j])/12
    prev_tableFM<-rbind(prev_tableFM<- data.frame(Year=Year[i],hunt_pred_comp=names(prev_econ[,j]),prev_pop))
  }
}

prev_tableFM<- prev_tableFM %>% separate(hunt_pred_comp, into = c("HuntF","HuntM"), sep = "_H", remove = TRUE) 
prev_tableFM<- prev_tableFM %>% pivot_wider(., names_from = "HuntM", values_from= "prev_pop")

# cummulative CWD


cummul_huntpredFM <- HuntF_HuntM_table1 %>% pivot_wider(names_from = Category, values_from = Count)
cummul_huntpredFM <- cummul_huntpredFM %>% mutate(cummul_hunt = cumsum(TotalH),
                                                cummul_cwd = cumsum(TotalCWDdeath),
                                                cummul_pred = cumsum(TotalPreddeath)) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF_rate_HuntM_rate","HuntF","HuntM","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

cummul_cwd_huntpredFM <-cummul_huntpredFM %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) 
cummul_cwd_huntpredFM <- cummul_cwd_huntpredFM %>% select("HuntF","HuntM","Month","Count") %>% 
  pivot_wider(names_from = HuntM,values_from = "Count")

# stargazer(t(cummul_cwd_huntpredFM),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "cumul_cwdFM.tex")

list_of_compFM<-list("NB_all_tableFM"=NB_all_tableFM,"suspop_tableFM"=suspop_tableFM, "infpop_tableFM"=infpop_tableFM, "totpop_tableFM"=totpop_tableFM, "prev_tableFM"=prev_tableFM, "cummul_cwdFM"=cummul_cwd_huntpredFM)
write.xlsx(list_of_compFM, file = "./NB_huntF_huntM_comp.xlsx")


#############################################################
############### ALL THREE COMPONENTS ########################
#############################################################


# SCEANRIO 8
hunt_f<-c(0,0.025,0.05,0.075,0.1)
hunt_m<-c(0,0.05,0.1,0.15,0.2)
num_pred<-c(100,120,150,180,200)

FMP_3way_list<-list()
for (j in 1:length(hunt_f)){
  for (k in 1:length(hunt_m)){
    for (i in 1:length(num_pred)){
      
      scen <-calcInfection(hunt.mort.ad.f=hunt_f[j],hunt.mort.ad.m=hunt_m[k],max.predators=num_pred[i],H=1,P=1)
      name <- paste('f_hunt',hunt_f[j],'_Pm_hunt',hunt_m[k],'_Ppred',num_pred[i],sep='_')
      FMP_3way_list[[name]] <- scen
}
}
}

length(FMP_3way_list)
length(FMP_3way_list[[1]][[1]])
# LOOP COMBINING ALL SCENARIOS IN LONG FORM
FMP_3way_table<-data.frame()
for(k in 1:length(FMP_3way_list)){
  for (j in 1:length(FMP_3way_list[[k]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        FMP_3way_table<- rbind(FMP_3way_table, data.frame(Scenario = names(FMP_3way_list)[k],
                                                                Category = names(FMP_3way_list[[k]])[j],
                                                                Age = names(ages)[i],
                                                                Month = 1:ncol(FMP_3way_list[[k]][[j]]),
                                                                Count = sapply(1:ncol(FMP_3way_list[[k]][[j]]), FUN = function(x) {sum(FMP_3way_list[[k]][[j]][vars,x])})))
      }
    }
}
FMP_3way_table<- FMP_3way_table %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = FALSE) %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                                           Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                                           Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                                           Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                                           Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                                           Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                                           Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                                           Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                                           Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                                           Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))


FMP_3way_table1<- FMP_3way_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(FMP_3way_table1))){
  Total_pop = FMP_3way_table1$Juv+FMP_3way_table1$Adult+FMP_3way_table1$Old
}
FMP_3way_table1<- add_column(FMP_3way_table1,Total_pop,.after = "Old")
FMP_3way_table1<-FMP_3way_table1 %>% pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

###### ECON analysis #########
nm_value_sen<-c("0","250","500")
FMP_3way_econ<-data.frame()
FMP_3way_econ_table<- FMP_3way_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  FMP_3way_econ<- rbind(FMP_3way_econ, data.frame(FMP_3way_econ_table %>% 
                                                          mutate(MEAT_VALUE = 5080*TotalH,
                                                                 NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                 NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                 nm_sen = as.integer(nm_value_sen[i]),
                                                                 undisc_NM_VALUEi=NM_VALUEi+MEAT_VALUE)))
}

FMP_3way_econ_sen<- FMP_3way_econ %>% pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# NET BENEFIT AND HUNTING TABLE

# Total_pop
nb_FMP_3way_table<-FMP_3way_econ_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,HuntF,HuntM,Num_Pred,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_FMP_3way_table$Month%%12 == 7] <- 1 
mod[nb_FMP_3way_table$Month%%12 == 8] <- 2 
nb_FMP_3way_table<-nb_FMP_3way_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat
  ) %>% replace(is.na(.), 0)
nb_FMP_3way_table <- filter(nb_FMP_3way_table, mod > 0) 
#stargazer(t(nb_huntFhuntM_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_allFM.tex")

NB_econ_table<-nb_FMP_3way_table %>% 
  pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nm_value_sen<-c("0","250","500")
NB_all_table_FMP_3way<-data.frame()
for (i in c(1,2,3)){
for (j in c(10:134)){
    NB_econ<-NB_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben<-sum(NB_econ[,j])
    NB_all_table_FMP_3way<-rbind(NB_all_table_FMP_3way, data.frame(Scenario=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                                     Net_Ben))
  }
}

NB_all_table_FMP_3way<- NB_all_table_FMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE) 
NB_all_table_FMP_3way<-NB_all_table_FMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "Net_Ben")

# stargazer(t(NB_all_tableFM),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "NB_allFM.tex")

#final susceptible population, final cwd prevalence, and cumulative CWD deaths

#predators
predators_tableFMP_3way<-data.frame()
predators_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_suspop_econ<-FMP_3way_table1 %>% filter(Category %in% c("Predators"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_sus_pop<-sum(avg_suspop_econ[,j])/12
    avg_suspop_tableFMP_3way<-rbind(avg_suspop_tableFMP_3way, data.frame(Scenario=names(suspop_econ[,j]),Year=Year_sen[i],avg_sus_pop))
  }
}
avg_suspop_tableFMP_3way<- avg_suspop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE) 
avg_suspop_tableFMP_3way<-avg_suspop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_sus_pop")



# susceptible pop

avg_suspop_tableFMP_3way<-data.frame()
final_suspop_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_suspop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalSprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_sus_pop<-sum(avg_suspop_econ[,j])/12
    avg_suspop_tableFMP_3way<-rbind(avg_suspop_tableFMP_3way, data.frame(Scenario=names(suspop_econ[,j]),Year=Year_sen[i],avg_sus_pop))
  }
}
avg_suspop_tableFMP_3way<- avg_suspop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE) 
avg_suspop_tableFMP_3way<-avg_suspop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_sus_pop")

for (k in 1:length(Month_sen)){
    for (j in c(9:133)){
    final_suspop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalSprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_month_pop<- sum(final_suspop_econ[,j])
    final_suspop_tableFMP_3way<-rbind(final_suspop_tableFMP_3way, data.frame(Scenario=names(final_suspop_econ[,j]),Month=Month_sen[k],endofyear_pop=final_month_pop))
  }
}

final_suspop_tableFMP_3way<- final_suspop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE) 
final_suspop_tableFMP_3way<- final_suspop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_pop")
suspop_tableFMP_3way<- cbind(avg_suspop_tableFMP_3way,final_suspop_tableFMP_3way)

# infected pop
avg_infpop_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_infpop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_inf_pop<-sum(avg_infpop_econ[,j])/12
    avg_infpop_tableFMP_3way<-rbind(avg_infpop_tableFMP_3way, data.frame(Year=Year_sen[i],Scenario=names(avg_infpop_econ[,j]),avg_inf_pop))
  }
}

avg_infpop_tableFMP_3way<- avg_infpop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
avg_infpop_tableFMP_3way<-avg_infpop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_inf_pop")

final_infpop_tableFMP_3way<-data.frame()
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    final_infpop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalIprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_inf_pop<-sum(final_infpop_econ[,j])
    final_infpop_tableFMP_3way<-rbind(final_infpop_tableFMP_3way, data.frame(Month=Month_sen[i],Scenario=names(final_infpop_econ[,j]),endofyear_inf_pop=final_inf_pop))
  }
}

final_infpop_tableFMP_3way<- final_infpop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
final_infpop_tableFMP_3way<- final_infpop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_inf_pop")
infpop_tableFMP_3way<- cbind(avg_infpop_tableFMP_3way, final_infpop_tableFMP_3way)


#total pop
avg_totpop_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_totpop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("Totalprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_tot_pop<-sum(avg_totpop_econ[,j])/12
    avg_totpop_tableFMP_3way<-rbind(avg_totpop_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_totpop_econ[,j]),avg_tot_pop))
  }
}

avg_totpop_tableFMP_3way<- avg_totpop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
avg_totpop_tableFMP_3way<- avg_totpop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_tot_pop")

final_totpop_tableFMP_3way<-data.frame()
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    final_totpop_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("Totalprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_tot_pop<-sum(final_totpop_econ[,j])
    final_totpop_tableFMP_3way<-rbind(final_totpop_tableFMP_3way, data.frame(Month=Month_sen[i], Scenario=names(final_totpop_econ[,j]),endofyear_tot_pop=final_tot_pop))
  }
}

final_totpop_tableFMP_3way<- final_totpop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
final_totpop_tableFMP_3way<- final_totpop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_tot_pop")
totpop_tableFMP_3way<- cbind(avg_totpop_tableFMP_3way, final_totpop_tableFMP_3way)


#pred deaths
avg_preddeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_preddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_preddeath<-sum(avg_preddeath_econ[,j])/12
    avg_preddeath_tableFMP_3way<-rbind(avg_preddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_preddeath_econ[,j]),avg_preddeath))
  }
}

avg_preddeath_tableFMP_3way<- avg_preddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
avg_preddeath_tableFMP_3way<- avg_preddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_preddeath")

cumm_preddeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    cumm_preddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_preddeath_econ<-cumm_preddeath_econ %>% filter(cumm_preddeath_econ[,j]>0)
    cumm_preddeath<-max(cumsum(cumm_preddeath_econ[,j]))
    cumm_preddeath_tableFMP_3way<-rbind(cumm_preddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),cumm_preddeath))
  }
}

cumm_preddeath_tableFMP_3way<- cumm_preddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
cumm_preddeath_tableFMP_3way<- cumm_preddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "cumm_preddeath")


totcumm_preddeath_tableFMP_3way<-data.frame()
  for (j in c(9:133)){
    totcumm_preddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    totcumm_preddeath_econ<-totcumm_preddeath_econ %>% filter(totcumm_preddeath_econ[,j]>0)
    totcumm_preddeath<-max(cumsum(totcumm_preddeath_econ[,j]))
    totcumm_preddeath_tableFMP_3way<-rbind(totcumm_preddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),totcumm_preddeath))
}

totcumm_preddeath_tableFMP_3way<- totcumm_preddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
totcumm_preddeath_tableFMP_3way<- totcumm_preddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "totcumm_preddeath")


totpred_tableFMP_3way<- cbind(avg_preddeath_tableFMP_3way, cumm_preddeath_tableFMP_3way,totcumm_preddeath_tableFMP_3way)

write.xlsx(totpred_tableFMP_3way, "./pred_test.xlsx")

# final cwd prevalence
PREV_FMP_3way <- FMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_FMP_3way <- PREV_FMP_3way %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF","HuntM","Num_Pred","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

avg_prev_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:20){
  for (j in c(8:132)){
    avg_prev_econ<-PREV_FMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_prev_pop<-sum(avg_prev_econ[,j])/12
    avg_prev_tableFMP_3way<-rbind(avg_prev_tableFMP_3way, data.frame(Year=Year[i],Scenario=names(avg_prev_econ[,j]),avg_prev_pop))
  }
}

avg_prev_tableFMP_3way<- avg_prev_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
avg_prev_tableFMP_3way<- avg_prev_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_prev_pop")

max_prev_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(8:132)){
    max_prev_econ<-PREV_FMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    max_prev_pop<-max(max_prev_econ[,j])
    max_prev_tableFMP_3way<-rbind(max_prev_tableFMP_3way, data.frame(Year=Year_sen[i],Scenario=names(max_prev_econ[,j]),max_prev_pop))
  }
}

max_prev_tableFMP_3way<- max_prev_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
max_prev_tableFMP_3way<- max_prev_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_prev_pop")



final_prev_tableFMP_3way<-data.frame()
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(8:132)){
    final_prev_econ<-PREV_FMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"),Month %in% c(Month_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_prev_pop<-sum(final_prev_econ[,j])
    final_prev_tableFMP_3way<-rbind(final_prev_tableFMP_3way, data.frame(Month=Month_sen[i], Scenario=names(final_prev_econ[,j]),endofyear_prev=final_prev_pop))
  }
}

final_prev_tableFMP_3way<- final_prev_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred"), sep = "_P", remove = TRUE)
final_prev_tableFMP_3way<- final_prev_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_prev")
totprev_tableFMP_3way<- cbind(avg_prev_tableFMP_3way, final_prev_tableFMP_3way)


# cummulative CWD


cummul_FMP_3way <- FMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
cummul_FMP_3way <- cummul_FMP_3way %>% mutate(cummul_hunt = cumsum(TotalH),
                                                  cummul_cwd = cumsum(TotalCWDdeath),
                                                  cummul_pred = cumsum(TotalPreddeath)) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF","HuntM","Num_Pred","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

cummul_cwd_FMP_3way <-cummul_FMP_3way %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) 
cummul_cwd_FMP_3way <- cummul_cwd_FMP_3way %>% select("HuntF","HuntM","Num_Pred","Month","Count") %>% 
  pivot_wider(names_from = HuntM,values_from = "Count")

# cwd deaths
avg_cwddeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_cwddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_cwddeath<-sum(avg_cwddeath_econ[,j])/12
    avg_cwddeath_tableFMP_3way<-rbind(avg_cwddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_cwddeath_econ[,j]),avg_cwddeath))
  }
}

avg_cwddeath_tableFMP_3way<- avg_cwddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
avg_cwddeath_tableFMP_3way<- avg_cwddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_cwddeath")

cumm_cwddeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    cumm_cwddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_cwddeath_econ<-cumm_cwddeath_econ %>% filter(cumm_cwddeath_econ[,j]>0)
    cumm_cwddeath<-max(cumsum(cumm_cwddeath_econ[,j]))
    cumm_cwddeath_tableFMP_3way<-rbind(cumm_cwddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),cumm_cwddeath))
  }
}

cumm_cwddeath_tableFMP_3way<- cumm_cwddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
cumm_cwddeath_tableFMP_3way<- cumm_cwddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_cwddeath")


totcumm_cwddeath_tableFMP_3way<-data.frame()
  for (j in c(9:133)){
    totcumm_cwddeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    totcumm_cwddeath_econ<-totcumm_cwddeath_econ %>% filter(totcumm_cwddeath_econ[,j]>0)
    totcumm_cwddeath<-max(cumsum(totcumm_cwddeath_econ[,j]))
    totcumm_cwddeath_tableFMP_3way<-rbind(totcumm_cwddeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),totcumm_cwddeath))
  }

totcumm_cwddeath_tableFMP_3way<- totcumm_cwddeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
totcumm_cwddeath_tableFMP_3way<- totcumm_cwddeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_cwddeath")


totcwd_tableFMP_3way<- cbind(avg_cwddeath_tableFMP_3way, cumm_cwddeath_tableFMP_3way,totcumm_cwddeath_tableFMP_3way)



# harvest deaths
avg_harvdeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    avg_harvdeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_harvdeath<-sum(avg_harvdeath_econ[,j])/12
    avg_harvdeath_tableFMP_3way<-rbind(avg_harvdeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_harvdeath_econ[,j]),avg_harvdeath))
  }
}

avg_harvdeath_tableFMP_3way<- avg_harvdeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
avg_harvdeath_tableFMP_3way<- avg_harvdeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_harvdeath")

cumm_harvdeath_tableFMP_3way<-data.frame()
Year_sen<-c("1","10","20")
for (i in 1:length(Year_sen)){
  for (j in c(9:133)){
    cumm_harvdeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_harvdeath_econ<-cumm_harvdeath_econ %>% filter(cumm_harvdeath_econ[,j]>0)
    cumm_harvdeath<-max(cumsum(cumm_harvdeath_econ[,j]))
    cumm_harvdeath_tableFMP_3way<-rbind(cumm_harvdeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),cumm_harvdeath))
  }
}

cumm_harvdeath_tableFMP_3way<- cumm_harvdeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
cumm_harvdeath_tableFMP_3way<- cumm_harvdeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_harvdeath")

totcumm_harvdeath_tableFMP_3way<-data.frame()
for (j in c(9:133)){
  totcumm_harvdeath_econ<-FMP_3way_econ_sen %>% filter(Category %in% c("TotalH"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_harvdeath_econ<-totcumm_harvdeath_econ %>% filter(totcumm_harvdeath_econ[,j]>0)
  totcumm_harvdeath<-max(cumsum(totcumm_harvdeath_econ[,j]))
  totcumm_harvdeath_tableFMP_3way<-rbind(totcumm_harvdeath_tableFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),totcumm_harvdeath))
}

totcumm_harvdeath_tableFMP_3way<- totcumm_harvdeath_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred"), sep = "_P", remove = TRUE)
totcumm_harvdeath_tableFMP_3way<- totcumm_harvdeath_tableFMP_3way %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_harvdeath")


totharv_tableFMP_3way<- cbind(avg_harvdeath_tableFMP_3way, cumm_harvdeath_tableFMP_3way,totcumm_harvdeath_tableFMP_3way)


# stargazer(t(cummul_cwd_huntpredFM),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "cumul_cwdFM.tex")

list_of_compFMP_3way<-list("NB_all_table_FMP_3way"=NB_all_table_FMP_3way,"suspop_tableFMP_3way"=suspop_tableFMP_3way, 
                     "infpop_tableFMP_3way"=infpop_tableFMP_3way, "totpop_tableFMP_3way"=totpop_tableFMP_3way,
                     "totpred_tableFMP_3way"=totpred_tableFMP_3way,"totprev_tableFMP_3way"=totprev_tableFMP_3way,
                     "totcwd_tableFMP_3way"=totcwd_tableFMP_3way,"totharv_tableFMP_3way"=totharv_tableFMP_3way)
write.xlsx(list_of_compFMP_3way, file = "./NB_3way_comp_updated.xlsx")



###### GRAPHS ######

# TOTAL POPULATION
#age
png("total_pop_base_age.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()

#female hunting
png("total_pop_base_fhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("Totalprey"), Age %in% c("Total_pop"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntF)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Female Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("total_pop_base_mhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntM)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Male Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("total_pop_base_pred.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying max number of Predators",
       y = "Elk Count", x = "Month")
dev.off()

# INFECTED POPULATION
png("inf_pop_base.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalIprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line()+
  theme_bw(base_size = 14)+
  labs(title = "Infected Elk Population over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()

# CWD DEATHS
#age
png("cwd_death_base_age.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()

#female hunting
png("cwd_death_base_fhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntF)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying Female Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("cwd_death_base_mhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntM)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying Male Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("cwd_death_base_pred.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying max number of Predators",
       y = "Elk Count", x = "Month")
dev.off()

# PREVALENCE

#ages
png("prev_time_base_age.png",width=640,height=420)
NB_table<- PREV_FMP_3way %>% filter(Category %in% c("PREV"), HuntF %in% c("f_hunt_0.05_"), 
                                    HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150"))
ggplot(data = NB_table, mapping = aes(x = Month, y = Count, color = as.character(Age))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Prevalence over Time",
       subtitle = "Base Combination",
       y = "Prevalence", x = "Month")
dev.off()

#female hunting
png("prev_time_base_fhunt.png",width=640,height=420)
NB_table<- PREV_FMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150"), 
                                                                           HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month, y = Count, color = HuntF)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Female Hunting",
       y = "Prevalence", x = "Month")
dev.off()

#male hunting
png("prev_time_base_mhunt.png",width=640,height=420)
NB_table<- PREV_FMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"), 
                                    Num_Pred %in% c("pred_150"))
ggplot(data = NB_table, mapping = aes(x = Month, y = Count, color = as.character(HuntM))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Male Hunting",
       y = "Prevalence", x = "Month")
dev.off()

# predators
png("prev_time_base_pred.png",width=640,height=420)
NB_table<- PREV_FMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"), 
                                    HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month, y = Count, color = as.character(Num_Pred))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying max number of Predators",
       y = "Prevalence", x = "Month")
dev.off()


# HARVEST DEATH
#age
png("harv_death_base_age.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1[which(FMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                              HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Harvest over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()

#female hunting
png("harv_death_base_fhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1[which(FMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), Age %in% c("Total_pop"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntF)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Female Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("harv_death_base_mhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1[which(FMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                Age %in% c("Total_pop"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntM)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Male Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("harv_death_base_pred.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1[which(FMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Num_Pred)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying max number of Predators",
       y = "Elk Count", x = "Month")
dev.off()

# PRED DEATHS
#age
png("pred_death_base_age.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()

# female hutning
png("pred_death_base_fhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), Age %in% c("Total_pop"), 
                                              HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntF)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying Female Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("pred_death_base_mhunt.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                              Age %in% c("Total_pop"), Num_Pred %in% c("pred_150")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = HuntM)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying Male Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("pred_death_base_pred.png",width=640,height=420)
pop_comp_healthy<- FMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                              HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying max number of Predators",
       y = "Elk Count", x = "Month")
dev.off()



# net benefit 

#undiscounted over time
png("undisc_net_ben_time_base.png",width=640,height=420)
NB_table<- FMP_3way_econ_sen[which(FMP_3way_econ_sen$Count>0),] %>% filter(Category %in% c("undisc_NM_VALUEi"),HuntF %in% c("f_hunt_0.05_"), 
                                                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150"))
ggplot(data = NB_table, mapping = aes(x = Month, y = Count, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit through Time",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Month")
dev.off()

#discounted over time
png("net_ben_time_base.png",width=640,height=420)
NB_table<- nb_FMP_3way_table[which(nb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                            HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150"))
ggplot(data = NB_table, mapping = aes(x = Month, y = NBPVi, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Present Value Net Benefit through Time",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Month")
dev.off()

#discounted over population
png("net_ben_pop_base.png",width=640,height=420)
NB_table<- nb_FMP_3way_table[which(nb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150"))
ggplot(data = NB_table, mapping = aes(x = TotalSprey, y = NBPVi,color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Present Value Net Benefit across Population",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()

#undiscounted over population
png("undisc_net_ben_pop_base.png",width=640,height=420)
NB_table<- FMP_3way_econ_sen%>% filter(Category %in% c("undisc_NM_VALUEi","Totalprey"),HuntF %in% c("f_hunt_0.05_"), 
                                         HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
#NB_table<- NB_table[which(NB_table$undisc_NM_VALUEi>0),]
ggplot(data = NB_table, mapping = aes(x = Totalprey, y = undisc_NM_VALUEi, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit across Population",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()











########### GENERATING CROSS FIGURE 2: SELECTION (0.1 AND 0.35) AND PER CAPITA KILL (0.2 AND 1) ############



# SCEANRIO 8
hunt_f<-c(0,0.025,0.05,0.075,0.1)
hunt_m<-c(0,0.05,0.1,0.15,0.2)
num_pred<-c(100,120,150,180,200)

crossFMP_3way_list<-list()
for (j in 1:length(hunt_f)){
  for (k in 1:length(hunt_m)){
    for (i in 1:length(num_pred)){
      scen <-calcInfection(hunt.mort.ad.f=hunt_f[j],hunt.mort.ad.m=hunt_m[k],max.predators=num_pred[i],r=0.1,k_max=0.2,H=1,P=1)
      name <- paste('f_hunt',hunt_f[j],'_Pm_hunt',hunt_m[k],'_Ppred',num_pred[i],'_Plow',sep='_')
      crossFMP_3way_list[[name]] <- scen
      scen <-calcInfection(hunt.mort.ad.f=hunt_f[j],hunt.mort.ad.m=hunt_m[k],max.predators=num_pred[i],r=0.35,k_max=1,H=1,P=1)
      name <- paste('f_hunt',hunt_f[j],'_Pm_hunt',hunt_m[k],'_Ppred',num_pred[i],'_Phigh',sep='_')
      crossFMP_3way_list[[name]] <- scen
    }
  }
}

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
crossFMP_3way_table<-data.frame()
for(k in 1:length(crossFMP_3way_list)){
  for (j in 1:length(crossFMP_3way_list[[k]])){
    for (i in 1:length(ages)) {
      vars<-unlist(mget(names(ages)[i]))
      crossFMP_3way_table<- rbind(crossFMP_3way_table, data.frame(Scenario = names(crossFMP_3way_list)[k],
                                                        Category = names(crossFMP_3way_list[[k]])[j],
                                                        Age = names(ages)[i],
                                                        Month = 1:ncol(crossFMP_3way_list[[k]][[j]]),
                                                        Count = sapply(1:ncol(crossFMP_3way_list[[k]][[j]]), FUN = function(x) {sum(crossFMP_3way_list[[k]][[j]][vars,x])})))
    }
  }
}

crossFMP_3way_table<- crossFMP_3way_table %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = FALSE) %>% 
  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                        Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                        Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                        Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                        Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20",
                        Month %in% c(241:252) ~ "21", Month %in% c(253:264) ~ "22",Month %in% c(265:276) ~ "23", Month %in% c(277:288) ~ "24",
                        Month %in% c(289:300) ~ "25", Month %in% c(301:312) ~ "26",Month %in% c(313:324) ~ "27", Month %in% c(325:336) ~ "28",
                        Month %in% c(337:348) ~ "29", Month %in% c(349:360) ~ "30"))

colnames(crossFMP_3way_table)<-c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Category","Age","Month","Count","Year")


crossFMP_3way_table1<- crossFMP_3way_table  %>% pivot_wider(names_from = Age, values_from = Count)

Total_pop = crossFMP_3way_table1$Juv+crossFMP_3way_table1$Adult+crossFMP_3way_table1$Old

crossFMP_3way_table1<- add_column(crossFMP_3way_table1,Total_pop,.after = "Old")
crossFMP_3way_table1<-crossFMP_3way_table1 %>% pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)


PREV_crossFMP_3way <- crossFMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_crossFMP_3way <- PREV_crossFMP_3way %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")










###### ECON analysis #########
nm_value_sen<-c("0","250","500")
crossFMP_3way_econ<-data.frame()
crossFMP_3way_econ_table<- crossFMP_3way_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  crossFMP_3way_econ<- rbind(crossFMP_3way_econ, data.frame(crossFMP_3way_econ_table %>% 
                                                    mutate(MEAT_VALUE = 5080*TotalH,
                                                           NM_PRICEi = as.integer(nm_value_sen[i]),
                                                           NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                           nm_sen = as.integer(nm_value_sen[i]),
                                                           undisc_NM_VALUEi=NM_VALUEi+MEAT_VALUE)))
}

crossFMP_3way_econ_sen<- crossFMP_3way_econ %>% pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)

# NET BENEFIT AND HUNTING TABLE

# Total_pop
crossnb_FMP_3way_table<-crossFMP_3way_econ_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,HuntF,HuntM,Num_Pred,predlevel,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[crossnb_FMP_3way_table$Month%%12 == 7] <- 1 
mod[crossnb_FMP_3way_table$Month%%12 == 8] <- 2 
crossnb_FMP_3way_table<-crossnb_FMP_3way_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat
  ) %>% replace(is.na(.), 0)
crossnb_FMP_3way_table <- filter(crossnb_FMP_3way_table, mod > 0) 
#stargazer(t(nb_huntFhuntM_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_allFM.tex")

crossNB_econ_table<-crossnb_FMP_3way_table %>% 
  pivot_longer(. , cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nm_value_sen<-c("0","250","500")
crossNB_all_table_FMP_3way<-data.frame()
for (i in c(1,2,3)){
  for (j in c(11:260)){
    NB_econ<-crossNB_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben<-sum(NB_econ[,j])
    crossNB_all_table_FMP_3way<-rbind(crossNB_all_table_FMP_3way, data.frame(Scenario=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                                                   Net_Ben))
  }
}

crossNB_all_table_FMP_3way<- crossNB_all_table_FMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE) 
crossNB_all_table_FMP_3way<-crossNB_all_table_FMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "Net_Ben")
crossNB_all_table_FMP_3way_low <-crossNB_all_table_FMP_3way %>% filter(predlevel %in% c("low"))
crossNB_all_table_FMP_3way_high <-crossNB_all_table_FMP_3way %>% filter(predlevel %in% c("high"))
  
# stargazer(t(NB_all_tableFM),                 # Export txt
#           summary = FALSE,
#           type = "latex",
#           digits=0,
#           out = "NB_allFM.tex")

#final susceptible population, final cwd prevalence, and cumulative CWD deaths

#predators

avg_predpop_econ<-crossFMP_3way_table1 %>% filter(Category %in% c("Predators"))
                                                  
avg_predpop_tableFMP_3way<-data.frame()
Year_sen<-as.integer(c("1":"30"))
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:258)){
    avg_predpop_econ<-crossFMP_3way_table1 %>% filter(Category %in% c("Predators"),Age %in% c("Total_pop"), Year==Year_sen[i])  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_pred_pop<-sum(avg_predpop_econ[,j]/18)/12
    avg_predpop_tableFMP_3way<-rbind(avg_predpop_tableFMP_3way, data.frame(Scenario=names(avg_predpop_econ[,j]),Year=Year_sen[i],avg_pred_pop))
  }
}
avg_predpop_tableFMP_3way<- avg_predpop_tableFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_predpop_tableFMP_3way1<-avg_predpop_tableFMP_3way %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_pred_pop")

avg_predpop_tableFMP_3wayt<-avg_predpop_tableFMP_3way %>% filter(predlevel %in% c("low"))
avg_predpop_tableFMP_3wayt1<-avg_predpop_tableFMP_3way %>% filter(predlevel %in% c("high"))

# susceptible pop

avg_suspop_tablecrossFMP_3way<-data.frame()
final_suspop_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
Month_sen<-c("12","120","240","360")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_suspop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalSprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_sus_pop<-sum(avg_suspop_econ[,j])/12
    avg_suspop_tablecrossFMP_3way<-rbind(avg_suspop_tablecrossFMP_3way, data.frame(Scenario=names(avg_suspop_econ[,j]),Year=Year_sen[i],avg_sus_pop))
  }
}
avg_suspop_tablecrossFMP_3way1<- avg_suspop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE) 
avg_suspop_tablecrossFMP_3way1<-avg_suspop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_sus_pop")

for (i in 1:length(Month_sen)){
  for (j in c(10:259)){
    final_suspop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalSprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_month_pop<- sum(final_suspop_econ[,j])
    final_suspop_tablecrossFMP_3way<-rbind(final_suspop_tablecrossFMP_3way, data.frame(Scenario=names(final_suspop_econ[,j]),Month=Month_sen[i],endofyear_pop=final_month_pop))
  }
}

final_suspop_tablecrossFMP_3way1<- final_suspop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE) 
final_suspop_tablecrossFMP_3way1<- final_suspop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_pop")
suspop_tablecrossFMP_3way<- cbind(avg_suspop_tablecrossFMP_3way1,final_suspop_tablecrossFMP_3way1)

# infected pop
avg_infpop_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_infpop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_inf_pop<-sum(avg_infpop_econ[,j])/12
    avg_infpop_tablecrossFMP_3way<-rbind(avg_infpop_tablecrossFMP_3way, data.frame(Year=Year_sen[i],Scenario=names(avg_infpop_econ[,j]),avg_inf_pop))
  }
}

avg_infpop_tablecrossFMP_3way1<- avg_infpop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_infpop_tablecrossFMP_3way1<-avg_infpop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_inf_pop")

final_infpop_tablecrossFMP_3way<-data.frame()
Month_sen<-c("12","120","240","360")
for (i in 1:length(Month_sen)){
  for (j in c(10:259)){
    final_infpop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalIprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_inf_pop<-sum(final_infpop_econ[,j])
    final_infpop_tablecrossFMP_3way<-rbind(final_infpop_tablecrossFMP_3way, data.frame(Month=Month_sen[i],Scenario=names(final_infpop_econ[,j]),endofyear_inf_pop=final_inf_pop))
  }
}

final_infpop_tablecrossFMP_3way1<- final_infpop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_infpop_tablecrossFMP_3way1<- final_infpop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_inf_pop")
infpop_tablecrossFMP_3way<- cbind(avg_infpop_tablecrossFMP_3way1, final_infpop_tablecrossFMP_3way1)


#total pop
avg_totpop_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_totpop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("Totalprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_tot_pop<-sum(avg_totpop_econ[,j])/12
    avg_totpop_tablecrossFMP_3way<-rbind(avg_totpop_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_totpop_econ[,j]),avg_tot_pop))
  }
}

avg_totpop_tablecrossFMP_3way1<- avg_totpop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_totpop_tablecrossFMP_3way1<- avg_totpop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_tot_pop")

final_totpop_tablecrossFMP_3way<-data.frame()
Month_sen<-c("12","120","240","360")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    final_totpop_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("Totalprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_tot_pop<-sum(final_totpop_econ[,j])
    final_totpop_tablecrossFMP_3way<-rbind(final_totpop_tablecrossFMP_3way, data.frame(Month=Month_sen[i], Scenario=names(final_totpop_econ[,j]),endofyear_tot_pop=final_tot_pop))
  }
}

final_totpop_tablecrossFMP_3way1<- final_totpop_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_totpop_tablecrossFMP_3way1<- final_totpop_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_tot_pop")
totpop_tablecrossFMP_3way<- cbind(avg_totpop_tablecrossFMP_3way1, final_totpop_tablecrossFMP_3way1)


#pred deaths
avg_preddeath_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_preddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_preddeath<-sum(avg_preddeath_econ[,j])/12
    avg_preddeath_tablecrossFMP_3way<-rbind(avg_preddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_preddeath_econ[,j]),avg_preddeath))
  }
}

avg_preddeath_tablecrossFMP_3way1<- avg_preddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_preddeath_tablecrossFMP_3way1<- avg_preddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_preddeath")

cumm_preddeath_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    cumm_preddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_preddeath_econ<-cumm_preddeath_econ %>% filter(cumm_preddeath_econ[,j]>0)
    cumm_preddeath<-max(cumsum(cumm_preddeath_econ[,j]))
    cumm_preddeath_tablecrossFMP_3way<-rbind(cumm_preddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),cumm_preddeath))
  }
}

cumm_preddeath_tablecrossFMP_3way1<- cumm_preddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
cumm_preddeath_tablecrossFMP_3way1<- cumm_preddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "cumm_preddeath")


totcumm_preddeath_tablecrossFMP_3way<-data.frame()
for (j in c(10:259)){
  totcumm_preddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalPreddeath"),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_preddeath_econ<-totcumm_preddeath_econ %>% filter(totcumm_preddeath_econ[,j]>0)
  totcumm_preddeath<-max(cumsum(totcumm_preddeath_econ[,j]))
  totcumm_preddeath_tablecrossFMP_3way<-rbind(totcumm_preddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),totcumm_preddeath))
}

totcumm_preddeath_tablecrossFMP_3way1<- totcumm_preddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_preddeath_tablecrossFMP_3way1<- totcumm_preddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "totcumm_preddeath")


totpred_tablecrossFMP_3way<- cbind(avg_preddeath_tablecrossFMP_3way, cumm_preddeath_tablecrossFMP_3way,totcumm_preddeath_tablecrossFMP_3way)

write.xlsx(totpred_tablecrossFMP_3way, "./pred_test.xlsx")

# final cwd prevalence
PREV_crossFMP_3way <- crossFMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_crossFMP_3way <- PREV_crossFMP_3way %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

avg_prev_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(9:258)){
    avg_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_prev_pop<-sum(avg_prev_econ[,j])/12
    avg_prev_tablecrossFMP_3way<-rbind(avg_prev_tablecrossFMP_3way, data.frame(Year=Year[i],Scenario=names(avg_prev_econ[,j]),avg_prev_pop))
  }
}

avg_prev_tablecrossFMP_3way1<- avg_prev_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_prev_tablecrossFMP_3way1<- avg_prev_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_prev_pop")

max_prev_tablecrossFMP_3way<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(9:258)){
    max_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    max_prev_pop<-max(max_prev_econ[,j])
    max_prev_tablecrossFMP_3way<-rbind(max_prev_tablecrossFMP_3way, data.frame(Year=Year_sen[i],Scenario=names(max_prev_econ[,j]),max_prev_pop))
  }
}

max_prev_tablecrossFMP_3way1<- max_prev_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
max_prev_tablecrossFMP_3way1<- max_prev_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "max_prev_pop")



final_prev_tablecrossFMP_3way<-data.frame()
Month_sen<-c("12","120","240","360")
for (i in 1:length(Month_sen)){
  for (j in c(9:258)){
    final_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"),Month %in% c(Month_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_prev_pop<-sum(final_prev_econ[,j])
    final_prev_tablecrossFMP_3way<-rbind(final_prev_tablecrossFMP_3way, data.frame(Month=Month_sen[i], Scenario=names(final_prev_econ[,j]),endofyear_prev=final_prev_pop))
  }
}

final_prev_tablecrossFMP_3way1<- final_prev_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_prev_tablecrossFMP_3way1<- final_prev_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_prev")
totprev_tablecrossFMP_3way<- cbind(avg_prev_tablecrossFMP_3way1, max_prev_tablecrossFMP_3way1, final_prev_tablecrossFMP_3way1)


# cummulative CWD


cummul_crossFMP_3way <- crossFMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
cummul_crossFMP_3way <- cummul_crossFMP_3way %>% mutate(cummul_hunt = cumsum(TotalH),
                                              cummul_cwd = cumsum(TotalCWDdeath),
                                              cummul_pred = cumsum(TotalPreddeath)) %>% 
  pivot_longer(., cols = !c("Scenario","HuntF","HuntM","Num_Pred","predlevel","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

cummul_cwd_crossFMP_3way <-cummul_crossFMP_3way %>% filter(Year %in% c("20"), Category %in% c("cummul_cwd"), Age %in% c("Total_pop")) 
cummul_cwd_crossFMP_3way <- cummul_cwd_crossFMP_3way %>% select("HuntF","HuntM","Num_Pred","predlevel","Month","Count") %>% 
  pivot_wider(names_from = HuntM,values_from = "Count")

# cwd deaths
avg_cwddeath_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_cwddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_cwddeath<-sum(avg_cwddeath_econ[,j])/12
    avg_cwddeath_tablecrossFMP_3way<-rbind(avg_cwddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_cwddeath_econ[,j]),avg_cwddeath))
  }
}

avg_cwddeath_tablecrossFMP_3way1<- avg_cwddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
avg_cwddeath_tablecrossFMP_3way1<- avg_cwddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_cwddeath")

cumm_cwddeath_tablecrossFMP_3way<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    cumm_cwddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_cwddeath_econ<-cumm_cwddeath_econ %>% filter(cumm_cwddeath_econ[,j]>0)
    cumm_cwddeath<-max(cumsum(cumm_cwddeath_econ[,j]))
    cumm_cwddeath_tablecrossFMP_3way<-rbind(cumm_cwddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),cumm_cwddeath))
  }
}

cumm_cwddeath_tablecrossFMP_3way1<- cumm_cwddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
cumm_cwddeath_tablecrossFMP_3way1<- cumm_cwddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_cwddeath")


totcumm_cwddeath_tablecrossFMP_3way<-data.frame()
for (j in c(10:259)){
  totcumm_cwddeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalCWDdeath"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_cwddeath_econ<-totcumm_cwddeath_econ %>% filter(totcumm_cwddeath_econ[,j]>0)
  totcumm_cwddeath<-max(cumsum(totcumm_cwddeath_econ[,j]))
  totcumm_cwddeath_tablecrossFMP_3way<-rbind(totcumm_cwddeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),totcumm_cwddeath))
}

totcumm_cwddeath_tablecrossFMP_3way1<- totcumm_cwddeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_cwddeath_tablecrossFMP_3way1<- totcumm_cwddeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_cwddeath")


totcwd_tablecrossFMP_3way<- cbind(avg_cwddeath_tablecrossFMP_3way1, cumm_cwddeath_tablecrossFMP_3way1,totcumm_cwddeath_tablecrossFMP_3way1)



# harvest deaths
avg_harvdeath_tablecrossFMP_3way<-data.frame()
Year_sen<-c("1","10","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    avg_harvdeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_harvdeath<-sum(avg_harvdeath_econ[,j])/12
    avg_harvdeath_tablecrossFMP_3way<-rbind(avg_harvdeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(avg_harvdeath_econ[,j]),avg_harvdeath))
  }
}

avg_harvdeath_tablecrossFMP_3way1<- avg_harvdeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
avg_harvdeath_tablecrossFMP_3way1<- avg_harvdeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_harvdeath")

cumm_harvdeath_tablecrossFMP_3way<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(10:259)){
    cumm_harvdeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_harvdeath_econ<-cumm_harvdeath_econ %>% filter(cumm_harvdeath_econ[,j]>0)
    cumm_harvdeath<-max(cumsum(cumm_harvdeath_econ[,j]))
    cumm_harvdeath_tablecrossFMP_3way<-rbind(cumm_harvdeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),cumm_harvdeath))
  }
}

cumm_harvdeath_tablecrossFMP_3way1<- cumm_harvdeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
cumm_harvdeath_tablecrossFMP_3way1<- cumm_harvdeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_harvdeath")

totcumm_harvdeath_tablecrossFMP_3way<-data.frame()
for (j in c(10:259)){
  totcumm_harvdeath_econ<-crossFMP_3way_econ_sen %>% filter(Category %in% c("TotalH"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_harvdeath_econ<-totcumm_harvdeath_econ %>% filter(totcumm_harvdeath_econ[,j]>0)
  totcumm_harvdeath<-max(cumsum(totcumm_harvdeath_econ[,j]))
  totcumm_harvdeath_tablecrossFMP_3way<-rbind(totcumm_harvdeath_tablecrossFMP_3way, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),totcumm_harvdeath))
}

totcumm_harvdeath_tablecrossFMP_3way1<- totcumm_harvdeath_tablecrossFMP_3way %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_harvdeath_tablecrossFMP_3way1<- totcumm_harvdeath_tablecrossFMP_3way1 %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_harvdeath")


totharv_tablecrossFMP_3way<- cbind(avg_harvdeath_tablecrossFMP_3way1, cumm_harvdeath_tablecrossFMP_3way1,totcumm_harvdeath_tablecrossFMP_3way1)

# compile all table into one giant excel file 

list_of_compcrossFMP_3way<-list("crossNB_all_table_FMP_3way"=crossNB_all_table_FMP_3way,"avg_predpop_tableFMP_3way"=avg_predpop_tableFMP_3way1,
                                "suspop_tablecrossFMP_3way"=suspop_tablecrossFMP_3way, 
                           "infpop_tablecrossFMP_3way"=infpop_tablecrossFMP_3way, "totpop_tablecrossFMP_3way"=totpop_tablecrossFMP_3way,
                           "totpred_tablecrossFMP_3way"=totpred_tablecrossFMP_3way,"totprev_tablecrossFMP_3way"=totprev_tablecrossFMP_3way,
                           "totcwd_tablecrossFMP_3way"=totcwd_tablecrossFMP_3way,"totharv_tablecrossFMP_3way"=totharv_tablecrossFMP_3way)
write.xlsx(list_of_compcrossFMP_3way, file = "./cross_NB_3way_compy30.xlsx")



###### GRAPHS ######


# cross base case for Figure 2

fhunt_level<-c("f_hunt_0_","f_hunt_0.025_","f_hunt_0.05_","f_hunt_0.075_","f_hunt_0.1_")
mhunt_level<-c("m_hunt_0_","m_hunt_0.05_","m_hunt_0.1_","m_hunt_0.15_","m_hunt_0.2_")
maxpred_level<-c("pred_100_","pred_120_","pred_150_","pred_180_","pred_200_")

# female hunting
png(paste0("cross_base_femalehighlow_wolf.png"),width=640,height=475)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Predators"),Age %in% c("Total_pop"), HuntF %in% c(fhunt_level[1],fhunt_level[5]) ,HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count/18, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25)+
  theme_bw(base_size = 18)+
  theme(legend.position=c("top"))+
  theme(legend.background = element_rect(linetype="solid", color="black"))+
  scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
  scale_linetype_manual("Harvest Intensity", values = c("solid",11), labels=c("High","Low"))+
  labs(y = "Wolf Count", x = "Year")

dev.off()


png(paste0("cross_base_femalehighlow_elk.png"),width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"), HuntF %in% c(fhunt_level[1],fhunt_level[5]),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25, show.legend = FALSE)+
  theme_bw(base_size = 18)+
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#332288","#CC6677"))+
  scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
  labs(y = "Elk Count", x = "Year")
dev.off()

png(paste0("cross_base_femalehighlow_prev.png"),width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), HuntF %in% c(fhunt_level[1],fhunt_level[5]), 
                                         HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), 
                                         Age %in% c("Total_pop"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25, show.legend = FALSE) +
  theme_bw(base_size = 18)+
  # theme(legend.position="bottom")+
   scale_color_manual(values = c("#332288","#CC6677"))+
  scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
   labs(y = "Prevalence", x = "Year")
dev.off()

png("crosscwd_death_female_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"),  HuntF %in% c(fhunt_level[1],fhunt_level[5]), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25,show.legend=FALSE)+
  theme_bw(base_size = 18)+
  scale_color_manual(values=c("#332288","#CC6677"))+
   scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
  labs(y = "CWD Death Count", x = "Year")
dev.off()

png("crosspred_death_female_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"),Age %in% c("Total_pop"),  HuntF %in% c(fhunt_level[1],fhunt_level[5]), 
                                                   HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25,show.legend=FALSE)+
  theme_bw(base_size = 18)+
  scale_color_manual(values=c("#332288","#CC6677"))+
  scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
  labs(y = "Predation Death Count", x = "Year")
dev.off()

png("crossnet_ben_time_female_highlow.png",width=640,height=420)
NB_table<- crossnb_FMP_3way_table[which(crossnb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c(fhunt_level[1],fhunt_level[5]), 
                                                                                     HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),
                                                                                     nm_sen %in% c("250"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = NBPVi, color=predlevel, linetype=HuntF)) +
  geom_line(size=1.25,show.legend=FALSE) +
  theme_bw(base_size = 18)+
  #facet_grid() +
  scale_color_manual(values=c( "#332288","#CC6677"))+
  scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
  labs(y = "Net Benefit ($)", x = "Year")
dev.off()



# male hunting
  png(paste0("cross_base_malehighlow_wolf.png"),width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Predators"),Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_") ,HuntM %in% c(mhunt_level[1],mhunt_level[5]), Num_Pred %in% c("pred_150_") ) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count/18, color=predlevel,linetype=HuntM)) +
    geom_line(size=1.25)+
    theme_bw(base_size = 18)+
    theme(legend.position=c("top"))+
    theme(legend.background = element_rect(linetype="solid", color="black"))+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11),labels=c("High","Low"))+
    labs(y = "Wolf Count", x = "Year")
   dev.off()
  
  png(paste0("cross_base_malehighlow_elk.png"),width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"),HuntM %in% c(mhunt_level[1],mhunt_level[5]), Num_Pred %in% c("pred_150_") ) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntM)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
    labs(y = "Elk Count", x = "Year")
  dev.off()
  
  png(paste0("cross_base_malehighlow_prev.png"),width=640,height=420)
  NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), HuntF %in% c("f_hunt_0.05_"), 
                                           HuntM %in% c(mhunt_level[1],mhunt_level[5]), Num_Pred %in% c("pred_150_"), 
                                           Age %in% c("Total_pop"))
  ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=HuntM)) +
    theme_bw(base_size = 18)+
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
    labs(y = "Prevalence", x = "Year")
  dev.off()
  
  png("crosscwd_death_male_highlow.png",width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"),  
                                                     HuntF %in% c("f_hunt_0.05_"), HuntM %in% c(mhunt_level[1],mhunt_level[5]),
                                                     Num_Pred %in% c("pred_150_") ) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel,linetype=HuntM)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual(values=c("#332288","#CC6677"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
    labs(y = "CWD Death Count", x = "Year")
  dev.off()
  
  png("crosspred_death_male_highlow.png",width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"),Age %in% c("Total_pop"),  HuntF %in% c("f_hunt_0.05_"), 
                                                     HuntM %in% c(mhunt_level[1],mhunt_level[5]), Num_Pred %in% c("pred_150_")) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel,linetype=HuntM)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual(values=c("#332288","#CC6677"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
    labs(y = "Predation Death Count", x = "Year")
  dev.off()
  
  png("crossnet_ben_time_male_highlow.png",width=640,height=420)
  NB_table<- crossnb_FMP_3way_table[which(crossnb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                                                                       HuntM %in% c(mhunt_level[1],mhunt_level[5]), Num_Pred %in% c("pred_150_"),
                                                                                       nm_sen %in% c("250"))
  ggplot(data = NB_table, mapping = aes(x = Month/12, y = NBPVi, color=predlevel,linetype=HuntM)) +
    geom_line(size=1.25,show.legend=FALSE) +
    theme_bw(base_size = 18)+
    #facet_grid() +
    scale_color_manual(values=c( "#332288","#CC6677"))+
    scale_linetype_manual("Harvest Intensity", values = c("solid",11))+
    labs(y = "Net Benefit ($)", x = "Year")
  dev.off()

# max predators 

  png(paste0("cross_base_numpredhighlow_wolf.png"),width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Predators"),Age %in% c("Total_pop"), 
                                                     HuntF %in% c("f_hunt_0.05_") ,HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5])) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count/18, color=predlevel, linetype=Num_Pred)) +
    geom_line(size=1.25)+
    theme_bw(base_size = 18)+
    theme(legend.position=c("top"))+
    theme(legend.background = element_rect(linetype="solid", color="black"))+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Max # Predators", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "Wolf Count", x = "Year")
  dev.off()
  
  png(paste0("cross_base_numpredhighlow_elk.png"),width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"), 
                                                     HuntF %in% c("f_hunt_0.05_"),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5]) ) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=Num_Pred)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Maximum Predator Population", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "Elk Count", x = "Year")
  dev.off()
  
  png(paste0("cross_base_numpredhighlow_prev.png"),width=640,height=420)
  NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), HuntF %in% c("f_hunt_0.05_"), 
                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5]), 
                                           Age %in% c("Total_pop"))
  ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color=predlevel,linetype=Num_Pred)) +
    theme_bw(base_size = 18)+
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual("Predator Efficiency", values = c("#332288","#CC6677"), labels=c("High","Low"))+
    scale_linetype_manual("Maximum Predator Population", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "Prevalence", x = "Year")
  dev.off()
  
  png("crosscwd_death_numpred_highlow.png",width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"),  
                                                     HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5]) ) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel,linetype=Num_Pred)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual(values=c("#332288","#CC6677"))+
    scale_linetype_manual("Maximum Predator Population", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "CWD Death Count", x = "Year")
  dev.off()
  
  png("crosspred_death_numpred_highlow.png",width=640,height=420)
  pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"),Age %in% c("Total_pop"),  
                                                     HuntF %in% c("f_hunt_0.05_"), 
                                                     HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5])) 
  ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel, linetype=Num_Pred)) +
    geom_line(size=1.25,show.legend=FALSE)+
    theme_bw(base_size = 18)+
    scale_color_manual(values=c("#332288","#CC6677"))+
    scale_linetype_manual("Maximum Predator Population", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "Predation Death Count", x = "Year")
  dev.off()
  
  png("crossnet_ben_time_numpred_highlow.png",width=640,height=420)
  NB_table<- crossnb_FMP_3way_table[which(crossnb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                                                                       HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c(maxpred_level[1],maxpred_level[5]),
                                                                                       nm_sen %in% c("250"))
  ggplot(data = NB_table, mapping = aes(x = Month/12, y = NBPVi, color=predlevel, linetype=Num_Pred)) +
    geom_line(size=1.25,show.legend=FALSE) +
    theme_bw(base_size = 18)+
    #facet_grid() +
    scale_color_manual(values=c( "#332288","#CC6677"))+
    scale_linetype_manual("Maximum Predator Population", values = c("solid",11), labels=c("High","Low"))+
    labs(y = "Net Benefit ($)", x = "Year")
  dev.off()
  


# TOTAL POPULATION
#age
png("crosstotal_pop_base_age_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"),HuntF %in% c("f_hunt_0.05_"),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 18)+
  scale_linetype_manual("Predator Efficiency", values = c("solid","dashed"), labels=c("high","low"))+
  labs(y = "Elk Count", x = "Year")
dev.off()

#female hunting
png("crosstotal_pop_base_fhunt_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),predlevel %in% c("high") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Female Hunting: High Predation",
       y = "Elk Count", x = "Year")
dev.off()

png("crosstotal_pop_base_fhunt_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),predlevel %in% c("low") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Female Hunting: Low Predation",
       y = "Elk Count", x = "Year")
dev.off()

png("crosstotal_pop_base_fhunt_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"),Age %in% c("Total_pop"),HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_linetype_manual("Predation level", values = c("solid","dashed"), labels=c("high","low"))+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Female Hunting: Predation Comparison",
       y = "Elk Count", x = "Year")
dev.off()

#male hunting
png("crosstotal_pop_base_mhunt_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"), predlevel %in% c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Male Hunting: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crosstotal_pop_base_mhunt_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"), predlevel %in% c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Male Hunting: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crosstotal_pop_base_mhunt_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying Male Hunting: Predation Comparison",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("crosstotal_pop_base_pred_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"),predlevel%in%c("high") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying max number of Predators: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crosstotal_pop_base_pred_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"),predlevel%in% c("low") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying max number of Predators: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crosstotal_pop_base_pred_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("Totalprey"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  #facet_grid(~Age)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Total Elk Population over Time",
       subtitle = "Varying max number of Predators: Predation Comparison",
       y = "Elk Count", x = "Month")
dev.off()

# INFECTED POPULATION
png("crossinf_pop_base_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalIprey"), Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  labs(title = "Infected Elk Population over Time",
       subtitle = "Base Combination",
       y = "Elk Count", x = "Month")
dev.off()


# CWD DEATHS
#age
png("crosscwd_death_base_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"),  HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel)) +
  geom_line(size=1.25,show.legend=FALSE)+
  theme_bw(base_size = 18)+
  scale_color_manual(values=c("#332288","#CC6677"))+
  labs(y = "CWD Death Count", x = "Year")
dev.off()

#female hunting
png("crosscwd_death_base_fhunt.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), Age %in% c("Total_pop"), HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying Female Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("crosscwd_death_base_mhunt.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), HuntF %in% c("f_hunt_0.05_"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying Male Hunting",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("crosscwd_death_base_pred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalCWDdeath"), HuntF %in% c("f_hunt_0.05_"), HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop") ) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "CWD deaths over Time",
       subtitle = "Varying max number of Predators",
       y = "Elk Count", x = "Month")
dev.off()

# PREVALENCE

#ages
png("crossprev_time_base_age.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), HuntF %in% c("f_hunt_0.05_"), 
                                    HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = Age, linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Prevalence over Time",
       subtitle = "Base Combination",
       y = "Prevalence", x = "Month")
dev.off()

#female hunting
png("crossprev_time_base_fhunt_highpred.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"), predlevel %in% c("high"), 
                                         HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Female Hunting: High Predation",
       y = "Prevalence", x = "Month")
dev.off()

png("crossprev_time_base_fhunt_lowpred.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"), predlevel %in% c("low"), 
                                         HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Female Hunting: Low Predation",
       y = "Prevalence", x = "Month")
dev.off()

png("crossprev_time_base_fhunt_highlow.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"), 
                                    HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = HuntF, linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Female Hunting: Predation Comparison",
       y = "Prevalence", x = "Month")
dev.off()

#male hunting
png("crossprev_time_base_mhunt_highlow.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"), 
                                    Num_Pred %in% c("pred_150_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = HuntM, linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying Male Hunting",
       y = "Prevalence", x = "Month")
dev.off()

# predators
png("crossprev_time_base_pred_highlow.png",width=640,height=420)
NB_table<- PREV_crossFMP_3way %>% filter(Category %in% c("PREV"), Age %in% c("Total_pop"), HuntF %in% c("f_hunt_0.05_"), 
                                    HuntM %in% c("m_hunt_0.1_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = Num_Pred,linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Prevalence over Time",
       subtitle = "Varying max number of Predators",
       y = "Prevalence", x = "Month")
dev.off()


# HARVEST DEATH
#age
png("crossharv_death_base_age_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel%in%c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Age)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Harvest over Time",
       subtitle = "Base Combination: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crossharv_death_base_age_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                          HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel%in%c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Age)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Age",values=c("#117733", "#88CCEE", "#E69F00","#CC6677"), labels=c("Adult","Juvenile","Old", "Total Population"))+
  labs(title = "Harvest over Time",
       subtitle = "Base Combination: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()

#female hunting
png("crossharv_death_base_fhunt_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), Age %in% c("Total_pop"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel%in%c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Female Hunting: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crossharv_death_base_fhunt_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), Age %in% c("Total_pop"), 
                                                                                          HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel%in%c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Female Hunting: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("crossharv_death_base_mhunt_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"),predlevel%in%c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Male Hunting: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crossharv_death_base_mhunt_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                          Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_"),predlevel%in%c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying Male Hunting: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()


#predators
png("crossharv_death_base_pred_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"),predlevel%in%c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying max number of Predators: High Predaton",
       y = "Elk Count", x = "Month")
dev.off()

png("crossharv_death_base_pred_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1[which(crossFMP_3way_table1$Count>0),]  %>% filter(Category %in% c("TotalH"), HuntF %in% c("f_hunt_0.05_"), 
                                                                                          HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"),predlevel%in%c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_point()+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Harvest over Time",
       subtitle = "Varying max number of Predators: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()

# PRED DEATHS
#age
png("crosspred_death_base_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"),Age %in% c("Total_pop"),  HuntF %in% c("f_hunt_0.05_"), 
                                              HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color=predlevel)) +
  geom_line(size=1.25,show.legend=FALSE)+
  theme_bw(base_size = 18)+
  scale_color_manual(values=c("#332288","#CC6677"))+
  labs(y = "Predation Death Count", x = "Year")
dev.off()

# female hutning
png("crosspred_death_base_fhunt_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), Age %in% c("Total_pop"), 
                                              HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntF, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Female Hunting Rate",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("2.5","5","7.5", "10", "0"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying Female Hunting: Predation Comparison",
       y = "Elk Count", x = "Month")
dev.off()

#male hunting
png("crosspred_death_base_mhunt_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                              Age %in% c("Total_pop"), Num_Pred %in% c("pred_150_")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = HuntM,linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Male Hunting Rate",values=c( "#E69F00", "#88CCEE", "#117733","#CC6677", "#332288"), labels=c("5","10","15", "20", "0"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying Male Hunting: Predation Comparison",
       y = "Elk Count", x = "Month")
dev.off()

#predators
png("crosspred_death_base_pred_highlow.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                              HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred, linetype=predlevel)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying max number of Predators: Predation Comparison",
       y = "Elk Count", x = "Month")
dev.off()


png("crosspred_death_base_pred_highpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                                   HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"), predlevel%in% c("high")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying max number of Predators: High Predation",
       y = "Elk Count", x = "Month")
dev.off()

png("crosspred_death_base_pred_lowpred.png",width=640,height=420)
pop_comp_healthy<- crossFMP_3way_table1 %>% filter(Category %in% c("TotalPreddeath"), HuntF %in% c("f_hunt_0.05_"), 
                                                   HuntM %in% c("m_hunt_0.1_"), Age %in% c("Total_pop"), predlevel%in% c("low")) 
ggplot(data = pop_comp_healthy, mapping = aes(x = Month/12, y = Count, color = Num_Pred)) +
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_manual("Max Predators",values=c("#88CCEE", "#117733", "#E69F00","#CC6677", "#332288"), labels=c("100","120","150", "180", "200"))+
  labs(title = "Predation Deaths over Time",
       subtitle = "Varying max number of Predators: Low Predation",
       y = "Elk Count", x = "Month")
dev.off()


# net benefit 

#undiscounted over time
png("crossundisc_net_ben_time_base_highlow.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen[which(crossFMP_3way_econ_sen$Count>0),] %>% filter(Category %in% c("undisc_NM_VALUEi"),HuntF %in% c("f_hunt_0.05_"), 
                                                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = as.character(nm_sen), linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit through Time",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Month")
dev.off()

png("crossundisc_net_ben_time_basehighpred.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen[which(crossFMP_3way_econ_sen$Count>0),] %>% filter(Category %in% c("undisc_NM_VALUEi"),HuntF %in% c("f_hunt_0.05_"), 
                                                                                     HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),predlevel%in%c("high"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit through Time: High Predation",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Month")
dev.off()

png("crossundisc_net_ben_time_base_lowpred.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen[which(crossFMP_3way_econ_sen$Count>0),] %>% filter(Category %in% c("undisc_NM_VALUEi"),HuntF %in% c("f_hunt_0.05_"), 
                                                                                     HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel %in% c("low"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = Count, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit through Time: Low Predation",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Month")
dev.off()


#discounted over time
png("crossnet_ben_time_base_highlow.png",width=640,height=420)
NB_table<- crossnb_FMP_3way_table[which(crossnb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),
                                                                           nm_sen %in% c("250"))
ggplot(data = NB_table, mapping = aes(x = Month/12, y = NBPVi, color=predlevel)) +
  geom_line(size=1.25,show.legend=FALSE) +
  theme_bw(base_size = 18)+
  #facet_grid() +
  scale_color_manual(values=c( "#332288","#CC6677"))+
  labs(y = "Net Benefit ($)", x = "Year")
dev.off()

#discounted over population
png("crossnet_ben_pop_base_highlow.png",width=640,height=420)
NB_table<- crossnb_FMP_3way_table[which(crossnb_FMP_3way_table$NBPVi>0),] %>% filter(HuntF %in% c("f_hunt_0.05_"), 
                                                                           HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"))
ggplot(data = NB_table, mapping = aes(x = TotalSprey, y = NBPVi,color = as.character(nm_sen),linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Present Value Net Benefit across Population: Predation Comparison",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()

#undiscounted over population
png("crossundisc_net_ben_pop_base_highlow.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen%>% filter(Category %in% c("undisc_NM_VALUEi","Totalprey"),HuntF %in% c("f_hunt_0.05_"), 
                                       HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = NB_table, mapping = aes(x = Totalprey, y = undisc_NM_VALUEi, color = as.character(nm_sen),linetype=predlevel)) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit across Population",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()

png("crossundisc_net_ben_pop_base_highpred.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen%>% filter(Category %in% c("undisc_NM_VALUEi","Totalprey"),HuntF %in% c("f_hunt_0.05_"), 
                                            HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"),predlevel%in%c("high")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = NB_table, mapping = aes(x = Totalprey, y = undisc_NM_VALUEi, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit across Population",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()

png("crossundisc_net_ben_pop_base_lowpred.png",width=640,height=420)
NB_table<- crossFMP_3way_econ_sen%>% filter(Category %in% c("undisc_NM_VALUEi","Totalprey"),HuntF %in% c("f_hunt_0.05_"), 
                                            HuntM %in% c("m_hunt_0.1_"), Num_Pred %in% c("pred_150_"), predlevel%in%c("low")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = NB_table, mapping = aes(x = Totalprey, y = undisc_NM_VALUEi, color = as.character(nm_sen))) +
  geom_line() +
  theme_bw(base_size = 14)+
  #facet_grid() +
  scale_color_manual("Non-Market Value",values=c("#117733", "#88CCEE", "#E69F00"), labels=c("0","250","500"))+
  labs(title = "Undiscounted Net Benefit across Population",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit ($)", x = "Elk Population")
dev.off()






















