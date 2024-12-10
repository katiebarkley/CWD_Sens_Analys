#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#                                                CROSS COMPARISON FIGURE 2                                                                  #
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################



require(tidyverse)
require(RSQLite)
require(DBI)
require(popbio)
require(openxlsx)
require(stargazer)
require(scales)

### MAKE SURE TO SET YOUR WORKING DIRECTORY TO THE FOLDER THAT HOLDS CWD_FUNCTION RSCRIPT

# LOAD IN THE FUNCTIONS
source("/Users/finnoff/Documents/UW/Katie B/CWDsims/code/CWD_FUNCTIONS.R")
#source("CWD_FUNCTIONS.R")
Juv = c(1:2)
Adult = c(3:13) 
Old = c(14:18)
ages<-list(Juv=Juv,Adult=Adult,Old=Old)

###### Predators, male harvest, and female harvest ##########

### This will take a while because there are a lot of runs ####
hunt_f<-seq(0,0.4,0.05)
hunt_m<-seq(0.05,0.85,0.1)
num_pred<-c(100,200)
crossFMPlist_low<-list()
for (j in 1:length(hunt_f)){
  for (k in 1:length(hunt_m)){
    for (i in 1:length(num_pred)){
      scen <-calcInfection(hunt.mort.ad.f=hunt_f[j],hunt.mort.ad.m=hunt_m[k],max.predators=num_pred[i],r=0.1,k_max=0.2,H=1,P=1)
      name <- paste('harvf',hunt_f[j],'_Pharvm',hunt_m[k],'_Pprednum',num_pred[i],'_Plow',sep='_')
      crossFMPlist_low[[name]] <- scen

    }
  }
}

hunt_f<-seq(0,0.4,0.05)
hunt_m<-seq(0.05,0.85,0.1)
num_pred<-c(100,200)
crossFMPlist_high<-list()
for (j in 1:length(hunt_f)){
  for (k in 1:length(hunt_m)){
    for (i in 1:length(num_pred)){
      scen <-calcInfection(hunt.mort.ad.f=hunt_f[j],hunt.mort.ad.m=hunt_m[k],max.predators=num_pred[i],r=0.35,k_max=1,H=1,P=1)
      name <- paste('harvf',hunt_f[j],'_Pharvm',hunt_m[k],'_Pprednum',num_pred[i],'_Phigh',sep='_')
      crossFMPlist_high[[name]] <- scen
      
    }
  }
}


# LOOP COMBINING ALL SCENARIOS IN LONG FORM
crossFMPtable_low<-data.frame()
for(k in 1:length(crossFMPlist_low)){
  for (j in 1:length(crossFMPlist_low[[k]])){
    for (i in 1:length(ages)) {
      vars<-unlist(mget(names(ages)[i]))
      crossFMPtable_low<- rbind(crossFMPtable_low, data.frame(Scenario = names(crossFMPlist_low)[k],
                                                                  Category = names(crossFMPlist_low[[k]])[j],
                                                                  Age = names(ages)[i],
                                                                  Month = 1:ncol(crossFMPlist_low[[k]][[j]]),
                                                                  Count = sapply(1:ncol(crossFMPlist_low[[k]][[j]]), FUN = function(x) {sum(crossFMPlist_low[[k]][[j]][vars,x])})))
    }
  }
}

crossFMPtable_high<-data.frame()
for(k in 1:length(crossFMPlist_high)){
  for (j in 1:length(crossFMPlist_high[[k]])){
    for (i in 1:length(ages)) {
      vars<-unlist(mget(names(ages)[i]))
      crossFMPtable_high<- rbind(crossFMPtable_high, data.frame(Scenario = names(crossFMPlist_high)[k],
                                                                  Category = names(crossFMPlist_high[[k]])[j],
                                                                  Age = names(ages)[i],
                                                                  Month = 1:ncol(crossFMPlist_high[[k]][[j]]),
                                                                  Count = sapply(1:ncol(crossFMPlist_high[[k]][[j]]), FUN = function(x) {sum(crossFMPlist_high[[k]][[j]][vars,x])})))
    }
  }
}

crossFMPtable_test_low<- crossFMPtable_low %>% separate(Scenario, into = c("harvf","harvm","numpred","pred_level"), sep = "_P", remove = FALSE) %>% 
  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                        Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                        Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                        Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                        Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20",
                        Month %in% c(241:252) ~ "21", Month %in% c(253:264) ~ "22",Month %in% c(265:276) ~ "23", Month %in% c(277:288) ~ "24",
                        Month %in% c(289:300) ~ "25", Month %in% c(301:312) ~ "26",Month %in% c(313:324) ~ "27", Month %in% c(325:336) ~ "28",
                        Month %in% c(337:348) ~ "29", Month %in% c(349:360) ~ "30"))

crossFMPtable_test_high<- crossFMPtable_high %>% separate(Scenario, into = c("harvf","harvm","numpred","pred_level"), sep = "_P", remove = FALSE) %>% 
  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                        Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                        Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                        Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                        Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20",
                        Month %in% c(241:252) ~ "21", Month %in% c(253:264) ~ "22",Month %in% c(265:276) ~ "23", Month %in% c(277:288) ~ "24",
                        Month %in% c(289:300) ~ "25", Month %in% c(301:312) ~ "26",Month %in% c(313:324) ~ "27", Month %in% c(325:336) ~ "28",
                        Month %in% c(337:348) ~ "29", Month %in% c(349:360) ~ "30"))


crossFMPtable_low1<- crossFMPtable_test_low  %>% pivot_wider(names_from = Age, values_from = Count)
crossFMPtable_high1<- crossFMPtable_test_high  %>% pivot_wider(names_from = Age, values_from = Count)

Total_pop = crossFMPtable_low1$Juv+crossFMPtable_low1$Adult+crossFMPtable_low1$Old
Total_pop = crossFMPtable_high1$Juv+crossFMPtable_high1$Adult+crossFMPtable_high1$Old

crossFMPtable_low1<- add_column(crossFMPtable_low1,Total_pop,.after = "Old")
crossFMPtable_low1<-crossFMPtable_low1 %>% pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","pred_level","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

crossFMPtable_high1<- add_column(crossFMPtable_high1,Total_pop,.after = "Old")
crossFMPtable_high1<-crossFMPtable_high1 %>% pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","pred_level","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)


PREV_crossFMP_low <- crossFMPtable_low1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_crossFMP_low <- PREV_crossFMP_low %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","harvf","harvm","numpred","pred_level","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

PREV_crossFMP_high <- crossFMPtable_high1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_crossFMP_high <- PREV_crossFMP_high %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","harvf","harvm","numpred","pred_level","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

############################ 3 way comp graphs #######################################

##### MALES ####

png("fig2_pred_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
prev_comp3way_test<- PREV_crossFMP_3way %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
                                                   Category %in% c("Predators"))


prev_comp3way_test2<-prev_comp3way_test %>%replace(.,prev_comp3way_test=="harvf_0_","harvf_0.00_")

Total_predator = prev_comp3way_test2$Count/18

prev_comp3way_test2<- add_column(prev_comp3way_test2,Total_predator,.after = "Count")

ggplot(data = prev_comp3way_test2, mapping = aes(x =Month, y = Total_predator, linetype=pred_level, color=harvf)) + 
  geom_line(size=1.25)+
  theme_bw(base_size=20)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme(legend.position = c(0.15,0.75),
        legend.key.size = unit(0.1,"cm"),
        legend.title = element_text(size=14),
        legend.key.width=unit(3,"line"),
        legend.text = element_text(size=12),
        legend.spacing = unit(0.2,"cm"))+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Predator Population: Adult Male Harvest = 20%",
       y = "Wolf Count", x = "Year") 
dev.off()




png("fig2_prey_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)

prev_compK<- PREV_crossFMP_3way %>% 
  filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
         Category %in% c("Totalprey"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, linetype = pred_level, color=harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = "none")+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Prey Population over time: Adult Male Harvest = 20%",
       y = "Elk Count", x = "Year") 
dev.off()

png("fig2_prev_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
prev_compK<- PREV_crossFMP_3way %>% 
  filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
         Category %in% c("PREV"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count*100, linetype = pred_level, color= harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = "none")+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Prevalence over time: Adult Male Harvest = 20%",
       y = "Prevalence (in %)", x = "Year") 
dev.off()

png("fig2_cwddeath_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
prev_compK<- PREV_crossFMP_3way %>% 
  filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
         Category %in% c("TotalCWDdeath"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, linetype = pred_level, color= harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = c(0.12,0.75),
        legend.key.size = unit(0.1,"cm"),
        legend.title = element_text(size=14),
        legend.key.width=unit(3,"line"),
        legend.text = element_text(size=12),
        legend.spacing = unit(0.2,"cm"))+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "CWD Death over time: Adult Male Harvest = 20%",
       y = "CWD Death", x = "Year") 
dev.off()

png("fig2_preddeath_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
prev_compK<- PREV_crossFMP_3way %>% 
  filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
         Category %in% c("TotalPreddeath"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, linetype = pred_level, color= harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = "none")+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Predation over time: Adult Male Harvest = 20%",
       y = "Predation", x = "Year") 
dev.off()

png("fig2_harvdeath_numpred_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
prev_compK<- PREV_crossFMP_3way %>% 
  filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"),Age %in% c("Total_pop"),
         Category %in% c("TotalH"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, linetype = pred_level, color= harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = "none")+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Harvest over time: Adult Male Harvest = 20%",
       y = "Harvest", x = "Year") 
dev.off()




###################################################################################################################
###################################################################################################################
#                                      Net benefit analysis and graph                                             #
###################################################################################################################
###################################################################################################################

# Low
nm_value_sen<-c("100","200","300")
FMP_econtable_low<-data.frame()
FMP_econtable_low2<- crossFMPtable_low1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

# paramters for NB

q<-0.0000123 # catchability coefficient 
c<-348.16       # cost of hunting day

for (i in c(1,2,3)){
  FMP_econtable_low<- rbind(FMP_econtable_low, data.frame(FMP_econtable_low2 %>% 
                                                            mutate(MEAT_VALUE = 1740*TotalH,
                                                                   Cost= ((c*TotalH)/(q*TotalSprey)),
                                                                   NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                   NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                   nm_sen = as.integer(nm_value_sen[i]),
                                                                   undisc_rev = NM_VALUEi+MEAT_VALUE,
                                                                   undisc_cost = ((c*TotalH)/(q*TotalSprey)),
                                                                   undisc_NM_VALUEi = NM_VALUEi+MEAT_VALUE-((c*TotalH)/(q*TotalSprey)))))
}

FMP_econtable_low<- FMP_econtable_low %>% pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","pred_level","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# High
nm_value_sen<-c("100","200","300")
FMP_econtable_high<-data.frame()
FMP_econtable_high2<- crossFMPtable_high1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  FMP_econtable_high<- rbind(FMP_econtable_high, data.frame(FMP_econtable_high2 %>% 
                                                            mutate(MEAT_VALUE = 1740*TotalH,
                                                                   Cost= ((c*TotalH)/(q*TotalSprey)),
                                                                   NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                   NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                   nm_sen = as.integer(nm_value_sen[i]),
                                                                   undisc_rev = NM_VALUEi+MEAT_VALUE,
                                                                   undisc_cost = ((c*TotalH)/(q*TotalSprey)),
                                                                   undisc_NM_VALUEi = NM_VALUEi+MEAT_VALUE-((c*TotalH)/(q*TotalSprey)))))
}

FMP_econtable_high<- FMP_econtable_high %>% pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","pred_level","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")%>% replace(is.na(.), 0)


# NET BENEFIT AND HUNTING TABLE

# Low
nb_FMPecon_low<-FMP_econtable_low %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,harvf,harvm,numpred,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE,Cost,undisc_rev,undisc_cost) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_FMPecon_low$Month%%12 == 7] <- 1 
mod[nb_FMPecon_low$Month%%12 == 8] <- 2 
nb_FMPecon_low<-nb_FMPecon_low%>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE,n=1),
    lagcost = lag(Cost,n=1),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(NM_VALUEi))), #3 percent discount rate
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(lagmeat))),
    NBPVi_cost = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(lagcost))),
    NBPVi=NBPVi_nm+NBPVi_meat-NBPVi_cost
  ) %>% replace(is.na(.), 0)
nb_FMPecon_low<- filter(nb_FMPecon_low, mod > 0) 

### getting total NB
nb_FMP_econtable_low<-nb_FMPecon_low %>% 
  pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)

nb_cost_test<-nb_FMPecon_low %>% filter(lagcost>0, harvf %in% c("harvf_0_","harvf_0.05_","harvf_0.1_"),  
                                        numpred %in% c("prednum_100_"), Age %in% c("Total_pop"), nm_sen %in% c("200"), mod %in% c("2"))
ggplot(nb_cost_test,mapping=aes(x=Year,y=lagcost,color=harvm))+
  geom_line(size=1.25)+
  theme_bw(base_size=20)+
  facet_grid(~harvf)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  #scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Male\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"))+
  labs(title = "Cost over time",
       y = "Cost ($)", x = "Year")

nb_rev_test<-nb_FMPecon_low %>% filter(undisc_rev>0, harvf %in% c("harvf_0_","harvf_0.05_","harvf_0.1_"), 
                                       numpred %in% c("prednum_100_"), Age %in% c("Total_pop"), nm_sen %in% c("200"), mod %in% c("2"))
ggplot(nb_rev_test,mapping=aes(x=Year,y=undisc_rev,color=harvm))+
  geom_line(size=1.25)+
  theme_bw(base_size=20)+
  facet_grid(~harvf)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Male\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"))+
  labs(title = "Revenue over time",
       y = "Revenue ($)", x = "Year")

nb_harv_test<-nb_FMPecon_low %>% filter(TotalH>0, harvf %in% c("harvf_0_","harvf_0.05_","harvf_0.1_"),numpred %in% c("prednum_100_"),
                                        Age %in% c("Total_pop"), nm_sen %in% c("200"))
ggplot(nb_harv_test,mapping=aes(x=Year,y=TotalH,color=harvm))+
  geom_line(size=1.25)+
  theme_bw(base_size=20)+
  facet_grid(~harvf)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Male\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"))+
  labs(title = "Harvest over time",
       y = "Elk", x = "Year")

nb_pop_test<-nb_FMPecon_low %>% filter(harvf %in% c("harvf_0_","harvf_0.05_","harvf_0.1_"),numpred %in% c("prednum_100_"),
                                        Age %in% c("Total_pop"), nm_sen %in% c("200"),mod %in% c("2"))
ggplot(nb_pop_test,mapping=aes(x=Year,y=TotalSprey,color=harvm))+
  geom_line(size=1.25)+
  theme_bw(base_size=20)+
  facet_grid(~harvf)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Male\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"))+
  labs(title = "Prey pop over time",
       y = "Elk", x = "Year")


# High
nb_FMPecon_high<-FMP_econtable_high %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,harvf,harvm,numpred,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[nb_FMPecon_high$Month%%12 == 7] <- 1 
mod[nb_FMPecon_high$Month%%12 == 8] <- 2 
nb_FMPecon_high<-nb_FMPecon_high%>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE,n=1),
    lagcost = lag(Cost,n=1),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(NM_VALUEi))), #3 percent discount rate
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(lagmeat))),
    NBPVi_cost = case_when(mod == 2 ~ ((1/((1.03)^(Year)))*(lagcost))),
    NBPVi=NBPVi_nm+NBPVi_meat-NBPVi_cost
  ) %>% replace(is.na(.), 0)
nb_FMPecon_high<- filter(nb_FMPecon_high, mod > 0) 

### getting total NB
nb_FMP_econtable_high<-nb_FMPecon_high %>% 
  pivot_longer(. , cols = !c("Scenario","harvf","harvm","numpred","Age",
                             "Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")  %>% 
  replace(is.na(.), 0)




############## When you have 3 predmax numbers (100, 150, 200) there are 64 columns 
#############   once you pivot wider in line 319. Since we changed this to 2 (100,200), 
#############    we only have 46 columns after pivoting wider. So I changed
#############     line 318 so it is c(11:46) instead of c(11:64).

# Low
nm_value_sen<-c("100","200","300")
nb_all_econtable_low<-data.frame()
for (i in c(2)){
  for (j in c(10:171)){
    NB_econ_low<-nb_FMP_econtable_low %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben_low<-sum(NB_econ_low[,j])
    nb_all_econtable_low<-rbind(nb_all_econtable_low, data.frame(Scenario=names(NB_econ_low[,j]),nm_sen=nm_value_sen[i],
                                                           Net_Ben_low))
  }
}

NB_allFMP_table_low<- nb_all_econtable_low %>% separate(Scenario, into = c("harvf","harvm","numpred", "pred_leve"), sep = "_P", remove = TRUE) 
NB_allFMP_table_low<-NB_allFMP_table_low %>% pivot_wider(., names_from = "nm_sen", values_from= "Net_Ben_low")

numpred_list<-c("prednum_100_","prednum_200_")
NB_allFMP_table_low2<-data.frame()
maxPV_low <-data.frame()
for (i in c(1,2)){
NB_allFMP_table_low2<-NB_allFMP_table_low %>% filter(numpred %in% numpred_list[i])
max_PVNB_low     <- max(NB_allFMP_table_low2[,5])
numeric_column <- unlist(NB_allFMP_table_low2[,5])
rowmax       <- as.numeric(which.max(numeric_column))
harvfmax         <- NB_allFMP_table_low2[rowmax,1]
harvmmax         <- NB_allFMP_table_low2[rowmax,2]
numpredmax         <- NB_allFMP_table_low2[rowmax,3]
maxPV_low        <- rbind(maxPV_low,data.frame(harvfmax,harvmmax,numpredmax,max_PVNB_low=max_PVNB_low))
}


# High
nm_value_sen<-c("100","200","300")
nb_all_econtable_high<-data.frame()
for (i in c(2)){
  for (j in c(10:171)){
    NB_econ_high<-nb_FMP_econtable_high %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben_high<-sum(NB_econ_high[,j])
    nb_all_econtable_high<-rbind(nb_all_econtable_high, data.frame(Scenario=names(NB_econ_high[,j]),nm_sen=nm_value_sen[i],
                                                                 Net_Ben_high))
  }
}

NB_allFMP_table_high<- nb_all_econtable_high %>% separate(Scenario, into = c("harvf","harvm","numpred", "pred_leve"), sep = "_P", remove = TRUE) 
NB_allFMP_table_high<-NB_allFMP_table_high %>% pivot_wider(., names_from = "nm_sen", values_from= "Net_Ben_high")

numpred_list<-c("prednum_100_","prednum_200_")
NB_allFMP_table_high2<-data.frame()
maxPV_high <-data.frame()
for (i in c(1,2)){
  NB_allFMP_table_high2<-NB_allFMP_table_high %>% filter(numpred %in% numpred_list[i])
  max_PVNB_high     <- max(NB_allFMP_table_high2[,5])
  numeric_column <- unlist(NB_allFMP_table_high2[,5])
  rowmax       <- as.numeric(which.max(numeric_column))
  harvfmax         <- NB_allFMP_table_high2[rowmax,1]
  harvmmax         <- NB_allFMP_table_high2[rowmax,2]
  numpredmax         <- NB_allFMP_table_high2[rowmax,3]
  maxPV_high        <- rbind(maxPV_high,data.frame(harvfmax,harvmmax,numpredmax,max_PVNB_high=max_PVNB_high))
}




################ Graphs #######################

# Low
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)

###NB_table<- nb_FMP_econ %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"), nm_sen %in% c("250"))
NB_table_low<- NB_allFMP_table_low %>% select(harvf,harvm,numpred,"200")
colnames(NB_table_low)<-c("harvf","harvm","numpred","PVNB")
NB_table_test<-NB_table_low %>%replace(.,NB_table_low=="harvf_0_","harvf_0.00_")
NB_table_test$harvm <- sub("harvm_0.", "", NB_table_test$harvm)
NB_table_test$harvm <- sub("_", "", NB_table_test$harvm)
NB_table_test$harvm <- as.numeric(NB_table_test$harvm)
NB_table_test$harvf <- as.factor(NB_table_test$harvf)
NB_table_test <- NB_table_test %>%
  arrange(harvf, harvm)

 #NB_table_test<-NB_table_test %>% filter(harvf %in% c("harvf_0.05_"))

ggplot(data = NB_table_test, mapping = aes(x = harvm, y = PVNB, color=harvf, group=harvf)) +
  geom_line(size=1.25)+
  # geom_point(data = maxPV_low, aes(x = harvmmax, y = max_PVNB_low, color = harvf), size = 3.5) +
  # geom_point(data = maxPV_high, aes(x = harvmmax, y = max_PVNB_high, color = harvf), size = 3.5) +
  facet_grid(~numpred, labeller = labeller(numpred = numpred_names))+
  theme_bw(base_size=20)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"),
                     labels=c("0","5","10","15","20","25","30","35","40"))+
  labs(title = "Net Benefit across various Harvest Levels \nLow Predator Efficiency",
       y = "Net Benefit", x = "Adult Male Harvest (%)")



# High
NB_table_high<- NB_allFMP_table_high %>% select(harvf,harvm,numpred,"3000")
colnames(NB_table_high)<-c("harvf","harvm","numpred","PVNB")
NB_table_test<-NB_table_high %>%replace(.,NB_table_high=="harvf_0_","harvf_0.00_")
NB_table_test$harvm <- sub("harvm_0.", "", NB_table_test$harvm)
NB_table_test$harvm <- sub("_", "", NB_table_test$harvm)
NB_table_test$harvm <- as.numeric(NB_table_test$harvm)
NB_table_test$harvf <- as.factor(NB_table_test$harvf)
NB_table_test <- NB_table_test %>%
  arrange(harvf, harvm)

ggplot(data = NB_table_test, mapping = aes(x = harvm, y = PVNB, color=harvf, group=harvf)) +
  geom_line(size=1.25)+
  # geom_point(data = maxPV_low, aes(x = harvmmax, y = max_PVNB_low, color = harvf), size = 3.5) +
  # geom_point(data = maxPV_high, aes(x = harvmmax, y = max_PVNB_high, color = harvf), size = 3.5) +
  facet_grid(~numpred, labeller = labeller(numpred = numpred_names))+
  theme_bw(base_size=20)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +  #scale_y_discrete(labels = function(x) substr(x, 7, 10)) + 
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"),
                     labels=c("0","5","10","15","20","25","30","35","40"))+
  labs(title = "Net Benefit across various Harvest Levels: \nHigh Predator Efficiency",
       y = "Net Benefit", x = "Adult Male Harvest (%)")





numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvm_0.25_="Male Harvest = 25",
  harvm_0.85_="Male Harvest = 85"
)

prev_compK<- PREV_crossFMP_low %>% 
  filter(harvm%in%c("harvm_0.25_","harvm_0.85_"),Age %in% c("Total_pop"),
         Category %in% c("Totalprey"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, linetype = harvm, color=harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller(numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  scale_linetype_manual("Adult Male Harvest %",values=c("solid","dashed"),labels=c("25","85"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"),
                     labels=c("0","5","10","15","20","25","30","35","40"))+  labs(title = "Prey Population over time",
       y = "Elk Count", x = "Year") 

prev_compK<- PREV_crossFMP_low %>% 
  filter(harvm%in%c("harvm_0.25_","harvm_0.85_"),Age %in% c("Total_pop"),
         Category %in% c("PREV"))
prev_compK_test<-prev_compK %>%replace(.,prev_compK=="harvf_0_","harvf_0.00_")

ggplot(data = prev_compK_test, mapping = aes(x = as.integer(Year), y = Count, color=harvf)) + 
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(harvm~numpred, labeller = labeller(numpred = numpred_names, harvm=huntval_names))+
  theme_bw(base_size=20)+
  theme(legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=18),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=16),
        legend.spacing = unit(0.2,"cm"),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels
  )+
  #scale_linetype_manual("Adult Male Harvest %",values=c("solid","dashed"),labels=c("25","85"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),
                     values=c("#88CCEE", "#332288","#E69F00","#117733","#CC6677","#CCFF33","#332288","#003300","#3300FF"),
                     labels=c("0","5","10","15","20","25","30","35","40"))+  labs(title = "Prevalence over time",
                                                                                  y = "Elk Count", x = "Year") 



















###############################################################################
###############################################################################
##############################################################################
#############################################################################

###### NB 3way Graph #######

# over time

png("netbenovertime_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
###NB_table<- nb_FMP_econ %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"), nm_sen %in% c("250"))
NB_table<- nb_FMP_econ %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"), nm_sen %in% c("250"))
NB_table_test<-NB_table %>%replace(.,NB_table=="harvf_0_","harvf_0.00_")

ggplot(data = NB_table_test, mapping = aes(x = as.integer(Year), y = NBPVi, linetype=pred_level,color=harvf)) +
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller(numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = c(0.9,0.8), 
        legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=12),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=10),
        legend.spacing = unit(0.2,"cm"))+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Net Benefit over Time: Adult Male Harvest = 20%",
       y = "Net Benefit", x = "Year")
dev.off()

# over population
png("netbenoverpop_1.png",height=480,width=680)
numpred_names <- c(
  prednum_100_="Low Max Pred (100)",
  prednum_200_="High Max Pred (200)"
)
huntval_names <- c(
  harvf_0_="Female Harvest = 0",
  harvf_0.1_="Female Harvest = 10"
)
###NB_table<- nb_FMP_econ %>% filter(harvm %in% c("harvm_0.2_"), harvf %in% c("harvf_0_"), nm_sen %in% c("250"))
NB_table<- nb_FMP_econ %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"), nm_sen %in% c("250"))
NB_table_test<-NB_table %>%replace(.,NB_table=="harvf_0_","harvf_0.00_")

ggplot(data = NB_table_test, mapping = aes(x = TotalSprey, y = NBPVi, linetype=pred_level, color = harvf)) +
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller( numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = c(0.9,0.7), 
        legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=12),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=10),
        legend.spacing = unit(0.2,"cm"))+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Net Benefit over Population: Adult Male Harvest = 20%",
       y = "Net Benefit", x = "Susceptible Elk Population")
dev.off()

#####################################################################################
################## NB over various harvest levels ###################################

nm_value_sen<-c("0","250","500")
nb_all_econ_table<-data.frame()
for (i in c(1,2,3)){
  for (j in c(11:46)){
    NB_econ<-nb_FMP_econ_table %>% filter(Category %in% c("NBPVi"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    Net_Ben<-sum(NB_econ[,j])
    nb_all_econ_table<-rbind(nb_all_econ_table, data.frame(Scenario=names(NB_econ[,j]),nm_sen=nm_value_sen[i],
                                                           Net_Ben))
  }
}



NB_allFMP_table<- nb_all_econ_table %>% separate(Scenario, into = c("harvf","harvm","numpred","pred_level"), sep = "_P", remove = TRUE) 
NB_allFMP_table<-NB_allFMP_table %>% pivot_wider(., names_from = "numpred", values_from= "Net_Ben")

# total low and high pred
NB_allFMP_table_low <-NB_allFMP_table %>% filter(pred_level %in% c("low"))
NB_allFMP_table_high <-NB_allFMP_table %>% filter(pred_level %in% c("high"))

# graph over male harvest values
NB_table<- nb_FMP_econ %>% filter(harvf %in% c("harvf_0_","harvf_0.1_"),harvm %in% c("harvm_0.2_"), nm_sen %in% c("250"))
NB_table_test<-NB_table %>%replace(.,NB_table=="harvf_0_","harvf_0.00_")

ggplot(data = NB_table_test, mapping = aes(x = harv_m, y = NBPVi, linetype=pred_level)) +
  geom_smooth(se=FALSE,size=1.25)+
  facet_grid(~numpred, labeller = labeller(numpred = numpred_names))+
  theme_bw(base_size=20)+
  theme(legend.position = c(0.9,0.8), 
        legend.key.size = unit(0.05,"cm"),
        legend.title = element_text(size=12),
        legend.key.width=unit(2.5,"line"),
        legend.text = element_text(size=10),
        legend.spacing = unit(0.2,"cm"))+
  scale_linetype_manual("Predation Level",values=c("solid","dashed"),labels=c("High","Low"))+
  scale_color_manual(expression("% Adult Female\nHarvested"),values=c("#117733", "#FF33CC", "#88CCEE"), labels=c("0","10"))+
  labs(title = "Sum of Discounted Net Benefit",
       y = "Total Net Benefit", x = "Male Harvest Rates")

#####################################################################################
#################################   EXCEL TABLES    #################################
#####################################################################################

#predators

avg_predpop_econ<-crossFMP_3way_table1 %>% filter(Category %in% c("Predators"))

#### We need to do the same change for the excel tables that we did in line 315. 
############ This excel goes from 62 to 44 so I changed line 416 to say c(9:44)
##############  instead of c(9:62). This goes for all the excel files after this one as well. 
################ Some were c(10:63) so I changed them to c(10:45). They should all work now :)

avg_predpop_tableFMP<-data.frame()
Year_sen<-as.integer(c("1":"30"))
Month_sen<-c("12","120","240")
for (i in 1:length(Year_sen)){
  for (j in c(9:44)){
    avg_predpop_econ<-crossFMP_3way_table1 %>% filter(Category %in% c("Predators"),Age %in% c("Total_pop"), Year==Year_sen[i])  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_pred_pop<-sum(avg_predpop_econ[,j]/18)/12
    avg_predpop_tableFMP<-rbind(avg_predpop_tableFMP, data.frame(Scenario=names(avg_predpop_econ[,j]),Year=Year_sen[i],avg_pred_pop))
  }
}
avg_predpop_tableFMP<- avg_predpop_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_predpop_tableFMP1<-avg_predpop_tableFMP %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_pred_pop")

avg_predpop_tableFMP_low<-avg_predpop_tableFMP %>% filter(predlevel %in% c("low"))
avg_predpop_tableFMP_high<-avg_predpop_tableFMP %>% filter(predlevel %in% c("high"))

# susceptible pop

avg_suspop_tableFMP<-data.frame()
final_suspop_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
Month_sen<-c("12","120","180","240","360")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_suspop_econ<-FMP_econ_table %>% filter(Category %in% c("TotalSprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_sus_pop<-sum(avg_suspop_econ[,j])/12
    avg_suspop_tableFMP<-rbind(avg_suspop_tableFMP, data.frame(Scenario=names(avg_suspop_econ[,j]),Year=Year_sen[i],avg_sus_pop))
  }
}
avg_suspop_tableFMP1<- avg_suspop_tableFMP %>% separate(Scenario, into = c("harvf","harvm","numpred","pred_level"), sep = "_P", remove = TRUE) 
avg_suspop_tableFMP1<-avg_suspop_tableFMP1 %>% pivot_wider(., names_from = "numpred", values_from= "avg_sus_pop")

for (i in 1:length(Month_sen)){
  for (j in c(10:45)){
    final_suspop_econ<-FMP_econ_table %>% filter(Category %in% c("TotalSprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from =  Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_month_pop<- sum(final_suspop_econ[,j])
    final_suspop_tableFMP<-rbind(final_suspop_tableFMP, data.frame(Scenario=names(final_suspop_econ[,j]),Month=Month_sen[i],endofyear_pop=final_month_pop))
  }
}

final_suspop_tableFMP1<- final_suspop_tableFMP %>% separate(Scenario, into = c("harvf","harvm","numpred","pred_level"), sep = "_P", remove = TRUE) 
final_suspop_tableFMP1<- final_suspop_tableFMP1 %>% pivot_wider(., names_from = "numpred", values_from= "endofyear_pop")
suspop_tableFMP<- cbind(avg_suspop_tableFMP1,final_suspop_tableFMP1)

# infected pop
avg_infpop_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_infpop_econ<-FMP_econ_table %>% filter(Category %in% c("TotalIprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_inf_pop<-sum(avg_infpop_econ[,j])/12
    avg_infpop_tableFMP<-rbind(avg_infpop_tableFMP, data.frame(Year=Year_sen[i],Scenario=names(avg_infpop_econ[,j]),avg_inf_pop))
  }
}

avg_infpop_tableFMP1<- avg_infpop_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_infpop_tableFMP1<-avg_infpop_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_inf_pop")

final_infpop_tableFMP<-data.frame()
Month_sen<-c("12","120","180","240","360")
for (i in 1:length(Month_sen)){
  for (j in c(10:45)){
    final_infpop_econ<-FMP_econ_table %>% filter(Category %in% c("TotalIprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_inf_pop<-sum(final_infpop_econ[,j])
    final_infpop_tableFMP<-rbind(final_infpop_tableFMP, data.frame(Month=Month_sen[i],Scenario=names(final_infpop_econ[,j]),endofyear_inf_pop=final_inf_pop))
  }
}

final_infpop_tableFMP1<- final_infpop_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_infpop_tableFMP1<- final_infpop_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_inf_pop")
infpop_tableFMP<- cbind(avg_infpop_tableFMP1, final_infpop_tableFMP1)


#total pop
avg_totpop_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_totpop_econ<-FMP_econ_table %>% filter(Category %in% c("Totalprey"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_tot_pop<-sum(avg_totpop_econ[,j])/12
    avg_totpop_tableFMP<-rbind(avg_totpop_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(avg_totpop_econ[,j]),avg_tot_pop))
  }
}

avg_totpop_tableFMP1<- avg_totpop_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_totpop_tableFMP1<- avg_totpop_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_tot_pop")

final_totpop_tableFMP<-data.frame()
Month_sen<-c("12","120","180","240","360")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    final_totpop_econ<-FMP_econ_table %>% filter(Category %in% c("Totalprey"),Month %in% c(Month_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_tot_pop<-sum(final_totpop_econ[,j])
    final_totpop_tableFMP<-rbind(final_totpop_tableFMP, data.frame(Month=Month_sen[i], Scenario=names(final_totpop_econ[,j]),endofyear_tot_pop=final_tot_pop))
  }
}

final_totpop_tableFMP1<- final_totpop_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_totpop_tableFMP1<- final_totpop_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_tot_pop")
totpop_tableFMP<- cbind(avg_totpop_tableFMP1, final_totpop_tableFMP1)


#pred deaths
avg_preddeath_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_preddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_preddeath<-sum(avg_preddeath_econ[,j])/12
    avg_preddeath_tableFMP<-rbind(avg_preddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(avg_preddeath_econ[,j]),avg_preddeath))
  }
}

avg_preddeath_tableFMP1<- avg_preddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_preddeath_tableFMP1<- avg_preddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_preddeath")

cumm_preddeath_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    cumm_preddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalPreddeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_preddeath_econ<-cumm_preddeath_econ %>% filter(cumm_preddeath_econ[,j]>0)
    cumm_preddeath<-max(cumsum(cumm_preddeath_econ[,j]))
    cumm_preddeath_tableFMP<-rbind(cumm_preddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),cumm_preddeath))
  }
}

cumm_preddeath_tableFMP1<- cumm_preddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
cumm_preddeath_tableFMP1<- cumm_preddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "cumm_preddeath")


totcumm_preddeath_tableFMP<-data.frame()
for (j in c(10:45)){
  totcumm_preddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalPreddeath"),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_preddeath_econ<-totcumm_preddeath_econ %>% filter(totcumm_preddeath_econ[,j]>0)
  totcumm_preddeath<-max(cumsum(totcumm_preddeath_econ[,j]))
  totcumm_preddeath_tableFMP<-rbind(totcumm_preddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_preddeath_econ[,j]),totcumm_preddeath))
}

totcumm_preddeath_tableFMP1<- totcumm_preddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_preddeath_tableFMP1<- totcumm_preddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "totcumm_preddeath")


totpred_tableFMP<- cbind(avg_preddeath_tableFMP1, cumm_preddeath_tableFMP)
totcumm_pred_tableFMP<-totcumm_preddeath_tableFMP1

# final cwd prevalence

PREV_crossFMP_3way <- crossFMP_3way_table1 %>% pivot_wider(names_from = Category, values_from = Count)
PREV_crossFMP_3way <- PREV_crossFMP_3way %>% 
  mutate(PREV = TotalIprey/Totalprey) %>% 
  pivot_longer(., cols = !c("Scenario","harvf","harvm","numpred","pred_level","Age",
                            "Month","Year"),names_to = "Category", values_to= "Count")

avg_prev_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(9:44)){
    avg_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_prev_pop<-sum(avg_prev_econ[,j])/12
    avg_prev_tableFMP<-rbind(avg_prev_tableFMP, data.frame(Year=Year_sen[i],Scenario=names(avg_prev_econ[,j]),avg_prev_pop))
  }
}

avg_prev_tableFMP1<- avg_prev_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
avg_prev_tableFMP1<- avg_prev_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "avg_prev_pop")

avg_prev_FMP1_low<-avg_prev_tableFMP1 %>% filter(predlevel %in% c("low"))
avg_prev_FMP1_high<-avg_prev_tableFMP1 %>% filter(predlevel %in% c("high"))

max_prev_tableFMP<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(9:44)){
    max_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"), Year %in% c(Year_sen[i])) %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    max_prev_pop<-max(max_prev_econ[,j])
    max_prev_tableFMP<-rbind(max_prev_tableFMP, data.frame(Year=Year_sen[i],Scenario=names(max_prev_econ[,j]),max_prev_pop))
  }
}

max_prev_tableFMP1<- max_prev_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
max_prev_tableFMP1<- max_prev_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "max_prev_pop")

max_prev_FMP1_low<-max_prev_tableFMP1 %>% filter(predlevel %in% c("low"))
max_prev_FMP1_high<-max_prev_tableFMP1 %>% filter(predlevel %in% c("high"))

final_prev_tableFMP<-data.frame()
Month_sen<-c("12","120","180","240","360")
for (i in 1:length(Month_sen)){
  for (j in c(9:44)){
    final_prev_econ<-PREV_crossFMP_3way %>% filter(Category %in% c("PREV"),Age %in% c("Total_pop"),Month %in% c(Month_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    final_prev_pop<-sum(final_prev_econ[,j])
    final_prev_tableFMP<-rbind(final_prev_tableFMP, data.frame(Month=Month_sen[i], Scenario=names(final_prev_econ[,j]),endofyear_prev=final_prev_pop))
  }
}

final_prev_tableFMP1<- final_prev_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_Pred","predlevel"), sep = "_P", remove = TRUE)
final_prev_tableFMP1<- final_prev_tableFMP1 %>% pivot_wider(., names_from = "Num_Pred", values_from= "endofyear_prev")

final_prev_FMP1_low<-final_prev_tableFMP1 %>% filter(predlevel %in% c("low"))
final_prev_FMP1_high<-final_prev_tableFMP1 %>% filter(predlevel %in% c("high"))

totprev_FMP_low<- cbind(avg_prev_FMP1_low, max_prev_FMP1_low, final_prev_FMP1_low)
totprev_FMP_high<- cbind(avg_prev_FMP1_high, max_prev_FMP1_high, final_prev_FMP1_high)



# CWD
# cwd deaths
avg_cwddeath_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_cwddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_cwddeath<-sum(avg_cwddeath_econ[,j])/12
    avg_cwddeath_tableFMP<-rbind(avg_cwddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(avg_cwddeath_econ[,j]),avg_cwddeath))
  }
}

avg_cwddeath_tableFMP1<- avg_cwddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
avg_cwddeath_tableFMP1<- avg_cwddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_cwddeath")

cumm_cwddeath_tableFMP<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    cumm_cwddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalCWDdeath"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_cwddeath_econ<-cumm_cwddeath_econ %>% filter(cumm_cwddeath_econ[,j]>0)
    cumm_cwddeath<-max(cumsum(cumm_cwddeath_econ[,j]))
    cumm_cwddeath_tableFMP<-rbind(cumm_cwddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),cumm_cwddeath))
  }
}

cumm_cwddeath_tableFMP1<- cumm_cwddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
cumm_cwddeath_tableFMP1<- cumm_cwddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_cwddeath")


totcumm_cwddeath_tableFMP<-data.frame()
for (j in c(10:45)){
  totcumm_cwddeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalCWDdeath"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_cwddeath_econ<-totcumm_cwddeath_econ %>% filter(totcumm_cwddeath_econ[,j]>0)
  totcumm_cwddeath<-max(cumsum(totcumm_cwddeath_econ[,j]))
  totcumm_cwddeath_tableFMP<-rbind(totcumm_cwddeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_cwddeath_econ[,j]),totcumm_cwddeath))
}

totcumm_cwddeath_tableFMP1<- totcumm_cwddeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_cwddeath_tableFMP1<- totcumm_cwddeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_cwddeath")


totcwd_tableFMP<- cbind(avg_cwddeath_tableFMP1, cumm_cwddeath_tableFMP1,totcumm_cwddeath_tableFMP1)

# harvest deaths
avg_harvdeath_tableFMP<-data.frame()
Year_sen<-c("1","10","15","20","30")
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    avg_harvdeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    avg_harvdeath<-sum(avg_harvdeath_econ[,j])/12
    avg_harvdeath_tableFMP<-rbind(avg_harvdeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(avg_harvdeath_econ[,j]),avg_harvdeath))
  }
}

avg_harvdeath_tableFMP1<- avg_harvdeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
avg_harvdeath_tableFMP1<- avg_harvdeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "avg_harvdeath")

cumm_harvdeath_tableFMP<-data.frame()
for (i in 1:length(Year_sen)){
  for (j in c(10:45)){
    cumm_harvdeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalH"),Year %in% c(Year_sen[i]),nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
    cumm_harvdeath_econ<-cumm_harvdeath_econ %>% filter(cumm_harvdeath_econ[,j]>0)
    cumm_harvdeath<-max(cumsum(cumm_harvdeath_econ[,j]))
    cumm_harvdeath_tableFMP<-rbind(cumm_harvdeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),cumm_harvdeath))
  }
}

cumm_harvdeath_tableFMP1<- cumm_harvdeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
cumm_harvdeath_tableFMP1<- cumm_harvdeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "cumm_harvdeath")

totcumm_harvdeath_tableFMP<-data.frame()
for (j in c(10:45)){
  totcumm_harvdeath_econ<-FMP_econ_table %>% filter(Category %in% c("TotalH"), nm_sen %in% c("0"))  %>% pivot_wider(names_from = Scenario, values_from=Count) %>%  replace(is.na(.), 0)
  totcumm_harvdeath_econ<-totcumm_harvdeath_econ %>% filter(totcumm_harvdeath_econ[,j]>0)
  totcumm_harvdeath<-max(cumsum(totcumm_harvdeath_econ[,j]))
  totcumm_harvdeath_tableFMP<-rbind(totcumm_harvdeath_tableFMP, data.frame(Year=Year_sen[i], Scenario=names(cumm_harvdeath_econ[,j]),totcumm_harvdeath))
}

totcumm_harvdeath_tableFMP1<- totcumm_harvdeath_tableFMP %>% separate(Scenario, into = c("HuntF","HuntM","Num_pred","predlevel"), sep = "_P", remove = TRUE)
totcumm_harvdeath_tableFMP1<- totcumm_harvdeath_tableFMP1 %>% pivot_wider(., names_from = "Num_pred", values_from= "totcumm_harvdeath")


totharv_tableFMP<- cbind(avg_harvdeath_tableFMP1, cumm_harvdeath_tableFMP1,totcumm_harvdeath_tableFMP1)

# compile all table into one giant excel file 

list_of_stats_FMP<-list("NB_all_table_FMP"=NB_allFMP_table,
                        "NB_all_table_low"=NB_allFMP_table_low,
                        "NB_all_table_high"=NB_allFMP_table_high,
                        "suspop_tableFMP"=suspop_tableFMP, 
                        "infpop_tableFMP"=infpop_tableFMP, "totpop_tableFMP"=totpop_tableFMP,
                        "avg_predpop_tableFMP"=avg_predpop_tableFMP1, "avg_predpop_lowpred"=avg_predpop_tableFMP_low,
                        "avg_predpop_highpred"=avg_predpop_tableFMP_high,
                        "totpred_tableFMP"=totpred_tableFMP,"totcumm_pred_tableFMP"=totcumm_pred_tableFMP,
                        "totprev_FMP_low"=totprev_FMP_low, "totprev_FMP_high"=totprev_FMP_high, "totcwd_tableFMP"=totcwd_tableFMP,
                        "totharv_tableFMP"=totharv_tableFMP)
write.xlsx(list_of_stats_FMP, file = "./NB_and_popstats_FMP4.xlsx")