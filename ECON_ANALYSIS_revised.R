
## Economic Analysis 

c = 144.94   # management cost per elk
v = 125 * 6  # economic value of hunting day ($) and average hunting days
m = 1441 # meat value of elk ($)
d = 0.03     # discount rate 

### HERE WE ARE PULLING IN THE DATA NEEDED TO MAKE THE TABLES

require(tidyverse)
require(RSQLite)
require(DBI)
require(stargazer)
require(openxlsx)

# LOAD IN THE FUNCTIONS
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/CWD_SCENARIO_SCRIPT.R")
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/HUNT_SENS_ANALYSIS.R")
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/PRED_SENS_killrate_ANALYSIS.R")
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/PRED_SENS_predmax_ANALYSIS.R")


# STOCK AND PRICE RELATIONSHIP
#pricecsv<-read.csv("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/cody_elk_prices.csv")
#plot(acc.price1 ~ stock, data = pricecsv, xlab="Stock", ylab="Price for Additional Elk")


# CREATING VALUES FOR COST , MEAT, AND NONMARKET

#add age classes into total pop= fawn+juv+adult
master_table1<- master_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(master_table))){
  Total_pop = master_table1$Juv+master_table1$Adult+master_table1$Old
}
master_table1<- add_column(master_table1,Total_pop,.after = "Old")
master_table1<-master_table1 %>% pivot_longer(. , cols = !c("Scenario","Run","Month","Year","Category"),names_to = "Age", values_to= "Count")

master_table1<-master_table1%>% pivot_wider(names_from = Category, values_from = Count) %>%
  mutate(PREV = TotalIprey/Totalpreyi) %>%
  pivot_longer(., cols = !c("Scenario","Run","Age","Month","Year"),names_to = "Category", values_to= "Count")


nm_value_sen<-c("0","250","500")
master_econ_table_sen<-data.frame()
master_econ_table<- master_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)

for (i in c(1,2,3)){
master_econ_table_sen<- rbind(master_econ_table_sen, data.frame(master_econ_table %>% 
  mutate(MEAT_VALUE = 5080*TotalH,
         NM_PRICEi = as.integer(nm_value_sen[i]),
         NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
         NM_PRICEh = as.integer(nm_value_sen[i]),
         NM_VALUEh = as.integer(nm_value_sen[i])*Totalpreyh,
         nm_sen = as.integer(nm_value_sen[i]))))
}

master_econ_table_sen<-master_econ_table_sen %>% pivot_longer(. , cols = !c("Scenario","Run","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")


# NET BENEFIT AND HUNTING TABLE

# Total_pop
netbenefit_table<-master_econ_table_sen %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,Age,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,TotalCWDdeath,TotalPreddeath,PREV)

mod<-vector()
mod[netbenefit_table$Month%%12 == 7] <- 1 
mod[netbenefit_table$Month%%12 == 8] <- 2 
netbenefit_table<-netbenefit_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat,
    
    NBPVh_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh))),
    NBPVh_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVh=NBPVh_nm+NBPVh_meat
  ) %>% replace(is.na(.), 0)
netbenefit_table <- filter(netbenefit_table, mod > 0) 
stargazer(t(netbenefit_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_all.tex")


# individual yearly tables 
netbenefit_scen<-netbenefit_table %>% filter(Scenario %in% c("SCEN1"), mod %in% c(2)) %>% select(Scenario, Month, Year, nm_sen,Totalpreyh, NM_PRICEh,NM_VALUEh, NBPVh_nm,NBPVh_meat,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen1,summary = FALSE,type = "latex",digits=0, out = "NB_scen1.tex")

netbenefit_scen2<-netbenefit_table %>% filter(Scenario %in% c("SCEN2")) %>% select(Scenario, Month, Year, Totalpreyh,nm_sen, NM_PRICEh,NM_VALUEh,TotalH, MEAT_VALUE, NBPVh_nm,NBPVh_meat,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen2,summary = FALSE,type = "latex", digits=0, out = "NB_scen2.tex")

netbenefit_scen3<-netbenefit_table %>% filter(Scenario %in% c("SCEN3"), mod %in% c(2)) %>% select(Scenario, Month, Year, nm_sen,TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi_nm,NBPVi_meat,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen3,summary = FALSE,type = "latex", digits=0, out = "NB_scen3.tex")

netbenefit_scen4<-netbenefit_table %>% filter(Scenario %in% c("SCEN4")) %>% select(Scenario, Month, Year, TotalSprey, nm_sen,NM_PRICEi,NM_VALUEi,TotalH, MEAT_VALUE,NBPVi_nm,NBPVi_meat,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4.tex")

netbenefit_scen5<-netbenefit_table %>% filter(Scenario %in% c("SCEN5"), mod %in% c(2)) %>% select(Scenario, Month, Year,nm_sen,Totalpreyh, NM_PRICEh,NM_VALUEh,NBPVh_nm,NBPVh_meat,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen5,summary = FALSE,type = "latex", digits=0, out = "NB_scen5.tex")

netbenefit_scen6<-netbenefit_table %>% filter(Scenario %in% c("SCEN6")) %>% select(Scenario, Month, Year, nm_sen,Totalpreyh, NM_PRICEh,NM_VALUEh,TotalH, MEAT_VALUE,NBPVh_nm,NBPVh_meat,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6.tex")

netbenefit_scen7<-netbenefit_table %>% filter(Scenario %in% c("SCEN7"), mod %in% c(2)) %>% select(Scenario, Month, Year, nm_sen,TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi_nm,NBPVi_meat,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen7,summary = FALSE,type = "latex", digits=0, out = "NB_scen7.tex")

netbenefit_scen8<-netbenefit_table %>% filter(Scenario %in% c("SCEN8")) %>% select(Scenario, Month, Year,nm_sen, TotalSprey, NM_PRICEi,NM_VALUEi,TotalH, MEAT_VALUE,NBPVi_nm,NBPVi_meat,NBPVi) %>% mutate_if(is.numeric, round)
# stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8.tex")

# individual yearly tables 
control<-netbenefit_table  %>% select(Scenario, Month, Year, nm_sen,TotalSprey,Totalpreyh,TotalCWDdeath,TotalH,TotalPreddeath) %>% mutate_if(is.numeric, round)

#writing to an excel file
list_of_datasets <- list("Scen1" = netbenefit_scen, "Scen2" = netbenefit_scen2,"Scen3" = netbenefit_scen3,"Scen4" = netbenefit_scen4,"Scen5" = netbenefit_scen5,"Scen6" = netbenefit_scen6,"Scen7" = netbenefit_scen7,"Scen8" = netbenefit_scen8)
write.xlsx(list_of_datasets, file = "./scenario_all_nmsen.xlsx")

write.xlsx(control, file = "./scenario_control_totalpop1.xlsx")


# infected table
netbenefit_sceni<-netbenefit_table %>% filter(Scenario %in% c("SCEN1","SCEN2","SCEN5","SCEN6")) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,NBPVh)

# healthy table
netbenefit_scenh<-netbenefit_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8")) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi)

# GRAPHING NET BENEFIT

# against total prey
NB_econ_table<-netbenefit_table %>% pivot_longer(. , cols = !c("Scenario","Age","Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count") 

nm_value_sen<-c("0","250","500")
NB_all_table<-data.frame()
for (i in c(1,2,3)){
NB_econh<-NB_econ_table %>% filter(Category %in% c("NBPVh"),Scenario %in% c("SCEN1","SCEN2","SCEN5","SCEN6"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count)
NB_econi<-NB_econ_table %>% filter(Category %in% c("NBPVi"),Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"),nm_sen %in% c(nm_value_sen[i]))  %>% pivot_wider(names_from = Scenario, values_from=Count)

NB1<-sum(NB_econh$SCEN1)
NB2<-sum(NB_econh$SCEN2)
NB3<-sum(NB_econi$SCEN3)
NB4<-sum(NB_econi$SCEN4)
NB5<-sum(NB_econh$SCEN5)
NB6<-sum(NB_econh$SCEN6)
NB7<-sum(NB_econi$SCEN7)
NB8<-sum(NB_econi$SCEN8)
NB_all_table<-rbind(NB_all_table, data.frame(nm_value_sen[i],NB1,NB2,NB3,NB4,NB5,NB6,NB7,NB8))
}
stargazer(t(NB_all_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_all.tex")
list_of_nm_table <- list("Scen1" = netbenefit_scen, "Scen2" = netbenefit_scen2,"Scen3" = netbenefit_scen3,"Scen4" = netbenefit_scen4,"Scen5" = netbenefit_scen5,
                         "Scen6" = netbenefit_scen6,"Scen7" = netbenefit_scen7,"Scen8" = netbenefit_scen8, "Sum of each Scenario nm"= NB_all_table)
write.xlsx(list_of_nm_table, file = "./scenarios_with_nm_val.xlsx")

# MAIN GRAPHS FOR PAPER

NB_table<- netbenefit_table %>% filter(Scenario %in% c("SCEN1","SCEN2","SCEN5","SCEN6"))
ggplot(data = NB_table, mapping = aes(x = Totalpreyh, y = NM_VALUEh, color = Scenario)) +
  geom_smooth(se=FALSE) +
  facet_grid(Scenario~.) +
  labs(title = "Net Benefits over Population: No Infection Present",
       subtitle = "Harvest Value, Non Market Value, net of Management Costs",
       y = "Net Benefit", x = "Elk Population")

NB_table<- netbenefit_table %>% filter(Scenario %in% c("SCEN4","SCEN8"))
ggplot(data = NB_table, mapping = aes(x = TotalSprey, y = PREV, color = Scenario)) +
  geom_smooth(se=FALSE) +
  facet_grid(Scenario~.) +
  labs(title = "Net Benefits over Population: Infection Present",
       subtitle = "Harvest Value, Non Market Value, net of Management Costs",
       y = "Net Benefit", x = "Susceptible Elk Population")


NB_table<- netbenefit_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"))
ggplot(data = NB_table, mapping = aes(x = Month, y = NBPVi, color = Scenario)) +
  geom_smooth() +
  facet_grid(Scenario~.) +
  labs(title = "Present Value Net Benefits through Time: Infection Present",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net benefit", x = "Month")

stargazer(t(NB_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_all.tex")


# ADULT MALE HUNTING NET BENEFIT 

AD_MALE_table1<-AD_MALE_table1%>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","Age","char","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count")

nm_value_sen<-c("0","250","500")
sens_econ_table<-data.frame()
master_econ_table<- AD_MALE_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)

for (i in c(1,2,3)){
  sens_econ_table<- rbind(sens_econ_table, data.frame(master_econ_table %>% 
                                                                    mutate(MEAT_VALUE = 5080*TotalH,
                                                                           NM_PRICEi = as.integer(nm_value_sen[i]),
                                                                           NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                                           NM_PRICEh = as.integer(nm_value_sen[i]),
                                                                           NM_VALUEh = as.integer(nm_value_sen[i])*Totalpreyh,
                                                                           nm_sen = as.integer(nm_value_sen[i]))))
}

sens_econ_table<-sens_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 


# NET BENEFIT AND HUNTING TABLE

netbenefitAM_table<-sens_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,Age,Hunt_perc,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,PREV) 

mod<-vector()
mod[netbenefitAM_table$Month%%12 == 7] <- 1 
mod[netbenefitAM_table$Month%%12 == 8] <- 2 
netbenefitAM_table<-netbenefitAM_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat,
    
    NBPVh_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh))),
    NBPVh_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVh=NBPVh_nm+NBPVh_meat
  ) %>% replace(is.na(.), 0)
netbenefitAM_table <- filter(netbenefitAM_table, mod > 0) 

# net benefit tables
NB_econ_tableM<-netbenefitAM_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 

NB_tableh<-data.frame()
scen_sens_name<-c("SCEN2_SENS","SCEN6_SENS")
nm_value_sen<-c("0","250","500")

for (k in c(1,2)){
for (i in c(1,2,3)){
NB_sens_econ<-NB_econ_tableM %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVh"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
NB_0<-sum(NB_sens_econ$S_0)
NB_05<-sum(NB_sens_econ$S_0.05)
NB_1<-sum(NB_sens_econ$S_0.1)
NB_15<-sum(NB_sens_econ$S_0.15)
NB_2<-sum(NB_sens_econ$S_0.2)
NB_tableh<-rbind(NB_tableh,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                     NB_0,NB_05,NB_1,NB_15,NB_2))
}
}

NB_tablei<-data.frame()
scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")

for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tableM %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVi"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
    NB_0<-sum(NB_sens_econ$S_0)
    NB_05<-sum(NB_sens_econ$S_0.05)
    NB_1<-sum(NB_sens_econ$S_0.1)
    NB_15<-sum(NB_sens_econ$S_0.15)
    NB_2<-sum(NB_sens_econ$S_0.2)
    NB_tablei<-rbind(NB_tablei,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                         NB_0,NB_05,NB_1,NB_15,NB_2))
  }
}

NB_table_M<-data.frame(NB_tableh,NB_tablei)
stargazer(t(NB_table_M),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_table_M.tex")


#Elasticity 
scen_sens_name<-c("SCEN2_SENS","SCEN6_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.05,0.1,0.15,0.2)
NB_elast_Mh<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tableh %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_0-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$NB_05-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$NB_1-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$NB_15-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$NB_2-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[5]-huntperc[3])/huntperc[3])
NB_elast_Mh<-rbind(NB_elast_Mh,data.frame(Scenario=scen_sens_name[k],
                                    nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.05,0.1,0.15,0.2)
NB_elast_Mi<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tablei %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_0-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$NB_05-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$NB_1-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$NB_15-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$NB_2-NB_elast_econ$NB_1)/NB_elast_econ$NB_1)/((huntperc[5]-huntperc[3])/huntperc[3])
    NB_elast_Mi<-rbind(NB_elast_Mi,data.frame(Scenario=scen_sens_name[k],
                                            nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

NB_elast_M<-data.frame(NB_elast_Mh,NB_elast_Mi)
stargazer(t(NB_elast_M),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_M.tex")

# ADULT FEMALE HUNTING SENS NET BENEFIT 

AD_FEMALE_table1<- AD_FEMALE_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(AD_FEMALE_table1))){
  Total_pop = AD_FEMALE_table1$Juv+AD_FEMALE_table1$Adult+AD_FEMALE_table1$Old
}
AD_FEMALE_table1<- add_column(AD_FEMALE_table1,Total_pop,.after = "Old")
AD_FEMALE_table1<-AD_FEMALE_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") 
AD_FEMALE_table1<-AD_FEMALE_table1%>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","Age","char","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count")

nm_value_sen<-c("0","250","500")
sens_econ_table<-data.frame()
master_econ_table<- AD_FEMALE_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)

for (i in c(1,2,3)){
  sens_econ_table<- rbind(sens_econ_table, data.frame(master_econ_table %>% 
                                                        mutate(MEAT_VALUE = 5080*TotalH,
                                                               NM_PRICEi = as.integer(nm_value_sen[i]),
                                                               NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                               NM_PRICEh = as.integer(nm_value_sen[i]),
                                                               NM_VALUEh = as.integer(nm_value_sen[i])*Totalpreyh,
                                                               nm_sen = as.integer(nm_value_sen[i]))))
}

sens_econ_table<-sens_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count")

# NET BENEFIT AND HUNTING TABLE

netbenefitAF_table<-sens_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,Hunt_perc,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,PREV) 

mod<-vector()
mod[netbenefitAF_table$Month%%12 == 7] <- 1 
mod[netbenefitAF_table$Month%%12 == 8] <- 2 
netbenefitAF_table<-netbenefitAF_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat,
    
    NBPVh_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh))),
    NBPVh_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVh=NBPVh_nm+NBPVh_meat
  ) %>% replace(is.na(.), 0)
netbenefitAF_table <- filter(netbenefitAF_table, mod > 0) 

# net benefit tables
NB_econ_tableF<-netbenefitAF_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 

NB_tableh<-data.frame()
scen_sens_name<-c("SCEN2_SENS","SCEN6_SENS")
nm_value_sen<-c("0","250","500")

for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tableF %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVh"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
    NB_0<-sum(NB_sens_econ$S_0)
    NB_025<-sum(NB_sens_econ$S_0.025)
    NB_05<-sum(NB_sens_econ$S_0.05)
    NB_075<-sum(NB_sens_econ$S_0.075)
    NB_1<-sum(NB_sens_econ$S_0.1)
    NB_tableh<-rbind(NB_tableh,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_0,NB_025,NB_05,NB_075,NB_1))
  }
}

NB_tablei<-data.frame()
scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")

for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tableF %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVh"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
    NB_0<-sum(NB_sens_econ$S_0)
    NB_025<-sum(NB_sens_econ$S_0.025)
    NB_05<-sum(NB_sens_econ$S_0.05)
    NB_075<-sum(NB_sens_econ$S_0.075)
    NB_1<-sum(NB_sens_econ$S_0.1)
    NB_tablei<-rbind(NB_tablei,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_0,NB_025,NB_05,NB_075,NB_1))
  }
}

NB_table_F<-data.frame(NB_tableh,NB_tablei)
stargazer(t(NB_table_F),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_table_F.tex")


#Elasticity 
scen_sens_name<-c("SCEN2_SENS","SCEN6_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.025,0.05,0.075,0.1)
NB_elast_Fh<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tableh %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_0-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$NB_025-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$NB_05-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$NB_075-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$NB_1-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[5]-huntperc[3])/huntperc[3])
    NB_elast_Fh<-rbind(NB_elast_Fh,data.frame(Scenario=scen_sens_name[k],
                                              nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.025,0.05,0.075,0.1)
NB_elast_Fi<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tablei %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_0-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$NB_025-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$NB_05-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$NB_075-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$NB_1-NB_elast_econ$NB_05)/NB_elast_econ$NB_05)/((huntperc[5]-huntperc[3])/huntperc[3])
    NB_elast_Fi<-rbind(NB_elast_Fi,data.frame(Scenario=scen_sens_name[k],
                                              nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

NB_elast_F<-data.frame(NB_elast_Fh,NB_elast_Fi)
stargazer(t(NB_elast_F),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_F.tex")

#export all tables to excel
list_of_NB_hunt_sens <- list("Adult Male Sens" = t(NB_table_M), "Adult Female Sens"=t(NB_table_F), "NB_elast_M"= t(NB_elast_M),"NB_elast_F"=t(NB_elast_F))
write.xlsx(list_of_NB_hunt_sens, file = "./scenario_NBhunt_sens.xlsx")

# ENVIRONMENTAL TRANSMISSION

env_master_table1<-env_master_table %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(env_master_table1))){
  Total_pop = env_master_table1$Fawn+env_master_table1$Juv+env_master_table1$Adult
}
env_master_table1<- add_column(env_master_table1,Total_pop,.after = "Adult")
env_master_table1<-env_master_table1 %>% pivot_longer(. , cols = !c("Scenario","char","ENV_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

trans_table<-env_master_table1 %>% filter(Age %in% c("Total_pop")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = trans_table, mapping = aes(x = Month, y = TotalSprey, color = ENV_perc)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "population changes with environmental trans",
       y = "total prey", x = "Month")


nm_value_sen<-c("0","250","500")
env_econ_table<-data.frame()
master_econ_table<- env_master_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)%>% 
  replace(is.na(.), 0)

for (i in c(1,2,3)){
  env_econ_table<- rbind(env_econ_table, data.frame(master_econ_table %>% 
                                                        mutate(MEAT_VALUE = 5080*TotalH,
                                                               NM_PRICEi = as.integer(nm_value_sen[i]),
                                                               NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                               nm_sen = as.integer(nm_value_sen[i]))))
}

env_econ_table <- env_econ_table %>% replace(is.na(.), 0) %>% pivot_longer(. , cols = !c("Scenario","char","ENV_perc","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") %>% replace(is.na(.), 0)

# NET BENEFIT TABLE

env_econ_table <- env_econ_table  %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,ENV_perc,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[env_econ_table$Month%%12 == 7] <- 1 
mod[env_econ_table$Month%%12 == 8] <- 2 
env_econ_table<-env_econ_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat) %>% replace(is.na(.), 0)
env_econ_table <- filter(env_econ_table, mod > 0)

env_econ_table <- env_econ_table %>% pivot_longer(. , cols = !c("Scenario","Age","ENV_perc","Month","mod","Year","nm_sen"),names_to = "Category", values_to= "Count")

# net benefit

NB_tablei<-data.frame()
scen_sens_name<-c("SCEN3","SCEN4","SCEN7","SCEN8")
nm_value_sen<-c("0","250","500")

for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_env_econ<-env_econ_table %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVi"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = ENV_perc, values_from=Count) 
    NB_0<-sum(NB_env_econ$"0")
    NB_01<-sum(NB_env_econ$"01")
    NB_025<-sum(NB_env_econ$"025")
    NB_05<-sum(NB_env_econ$"05")
    NB_tablei<-rbind(NB_tablei,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_0,NB_01,NB_025,NB_05))
  }
}
NB_env_trans<-NB_tablei
stargazer(t(NB_env_trans),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_env.tex")

# PREDATION (kill_rate) NET BENEFIT 

nm_value_sen<-c("0","250","500")
pred_econ_table<-data.frame()
pred_econ_table1<- PRED_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)

for (i in c(1,2,3)){
  pred_econ_table<- rbind(pred_econ_table, data.frame(pred_econ_table1 %>% 
                                                        mutate(MEAT_VALUE = 5080*TotalH,
                                                               NM_PRICEi = as.integer(nm_value_sen[i]),
                                                               NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                               NM_PRICEh = as.integer(nm_value_sen[i]),
                                                               NM_VALUEh = as.integer(nm_value_sen[i])*Totalpreyh,
                                                               nm_sen = as.integer(nm_value_sen[i]))))
}

pred_econ_table<-pred_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Kill_rate","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 


# NET BENEFIT AND HUNTING TABLE

netbenefitPRED_table<-pred_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,Age,Kill_rate,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE) 

mod<-vector()
mod[netbenefitPRED_table$Month%%12 == 7] <- 1 
mod[netbenefitPRED_table$Month%%12 == 8] <- 2 
netbenefitPRED_table<-netbenefitPRED_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat,
    
    NBPVh_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh))),
    NBPVh_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVh=NBPVh_nm+NBPVh_meat
  ) %>% replace(is.na(.), 0)
netbenefitPRED_table <- filter(netbenefitPRED_table, mod > 0) 

# net benefit tables
NB_econ_tablePRED<-netbenefitPRED_table %>% pivot_longer(. , cols = !c("Scenario","Age","Kill_rate","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 

NB_tableh<-data.frame()
scen_sens_name<-c("SCEN5_PRED","SCEN6_PRED")
nm_value_sen<-c("0","250","500")
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tablePRED %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVh"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Kill_rate, values_from=Count) 
    NB_20<-sum(NB_sens_econ$P_20)
    NB_50<-sum(NB_sens_econ$P_50)
    NB_80<-sum(NB_sens_econ$P_80)
    NB_100<-sum(NB_sens_econ$P_100)
    NB_120<-sum(NB_sens_econ$P_120)
    NB_tableh<-rbind(NB_tableh,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_20,NB_50,NB_80,NB_100,NB_120))
  }
}

NB_tablei<-data.frame()
scen_sens_name<-c("SCEN7_PRED","SCEN8_PRED")
nm_value_sen<-c("0","250","500")
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tablePRED %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVi"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Kill_rate, values_from=Count) 
    NB_20<-sum(NB_sens_econ$P_20)
    NB_50<-sum(NB_sens_econ$P_50)
    NB_80<-sum(NB_sens_econ$P_80)
    NB_100<-sum(NB_sens_econ$P_100)
    NB_120<-sum(NB_sens_econ$P_120)
    NB_tablei<-rbind(NB_tablei,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_20,NB_50,NB_80,NB_100,NB_120))
  }
}

NB_table_PRED<-data.frame(NB_tableh,NB_tablei)
stargazer(t(NB_table_PRED),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_table_PRED.tex")


#Elasticity 
scen_sens_name<-c("SCEN5_PRED","SCEN6_PRED")
nm_value_sen<-c("0","250","500")
killrate<-c(0.2,0.5,0.8,1,1.2)
NB_elast_predh<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tableh %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_20-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[1]-killrate[4])/killrate[4])
    NB_elast2 <-((NB_elast_econ$NB_50-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[2]-killrate[4])/killrate[4])
    NB_elast3 <-((NB_elast_econ$NB_80-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[3]-killrate[4])/killrate[4])
    NB_elast4 <-((NB_elast_econ$NB_100-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[4]-killrate[4])/killrate[4])
    NB_elast5 <-((NB_elast_econ$NB_120-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[5]-killrate[4])/killrate[4])
    NB_elast_predh<-rbind(NB_elast_predh,data.frame(Scenario=scen_sens_name[k],
                                              nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

scen_sens_name<-c("SCEN7_PRED","SCEN8_PRED")
nm_value_sen<-c("0","250","500")
killrate<-c(0.2,0.5,0.8,1,1.2)
NB_elast_predi<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tablei %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_20-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[1]-killrate[4])/killrate[4])
    NB_elast2 <-((NB_elast_econ$NB_50-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[2]-killrate[4])/killrate[4])
    NB_elast3 <-((NB_elast_econ$NB_80-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[3]-killrate[4])/killrate[4])
    NB_elast4 <-((NB_elast_econ$NB_100-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[4]-killrate[4])/killrate[4])
    NB_elast5 <-((NB_elast_econ$NB_120-NB_elast_econ$NB_100)/NB_elast_econ$NB_100)/((killrate[5]-killrate[4])/killrate[4])
    NB_elast_predi<-rbind(NB_elast_predi,data.frame(Scenario=scen_sens_name[k],
                                              nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

NB_elast_pred<-data.frame(NB_elast_predh,NB_elast_predi)
stargazer(t(NB_elast_pred),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_pred.tex")


#export all tables to excel
list_of_datasets <- list("NBHuntM" = NB_table_M, "NBHuntF"= NB_table_F, "NBPred" = NB_table_PRED, "NBenvtrans"=NB_env_trans,"NBelastM"=NB_elast_M,
                         "NBelastF"=NB_elast_F,"NBelastPred"=NB_elast_pred)
write.xlsx(list_of_datasets, file = "./scenario_NB_sens_all.xlsx")


# PREDATION (max predators) NET BENEFIT 

nm_value_sen<-c("0","250","500")
predmax_econ_table<-data.frame()
predmax_econ_table1<- PREDMax_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count)

for (i in c(1,2,3)){
  predmax_econ_table<- rbind(predmax_econ_table, data.frame(predmax_econ_table1 %>% 
                                                        mutate(MEAT_VALUE = 5080*TotalH,
                                                               NM_PRICEi = as.integer(nm_value_sen[i]),
                                                               NM_VALUEi = as.integer(nm_value_sen[i])*TotalSprey,
                                                               NM_PRICEh = as.integer(nm_value_sen[i]),
                                                               NM_VALUEh = as.integer(nm_value_sen[i])*Totalpreyh,
                                                               nm_sen = as.integer(nm_value_sen[i]))))
}

predmax_econ_table<-predmax_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Max_Pred","Age","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 


# NET BENEFIT AND HUNTING TABLE

netbenefitPREDmax_table<-predmax_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  select(.,Scenario,Age,Max_Pred,Month,Year,nm_sen,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE) 

mod<-vector()
mod[netbenefitPREDmax_table$Month%%12 == 7] <- 1 
mod[netbenefitPREDmax_table$Month%%12 == 8] <- 2 
netbenefitPREDmax_table<-netbenefitPREDmax_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    lagmeat = lag(MEAT_VALUE, n = 1, default = NA),
    NBPVi_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi))),
    NBPVi_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVi=NBPVi_nm+NBPVi_meat,
    
    NBPVh_nm = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh))),
    NBPVh_meat = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(lagmeat))),
    NBPVh=NBPVh_nm+NBPVh_meat
  ) %>% replace(is.na(.), 0)
netbenefitPREDmax_table <- filter(netbenefitPREDmax_table, mod > 0) 

# net benefit tables
NB_econ_tablePREDMax<-netbenefitPREDmax_table %>% pivot_longer(. , cols = !c("Scenario","Age","Max_Pred","Month","Year","nm_sen"),names_to = "Category", values_to= "Count") 

NB_tableh<-data.frame()
scen_sens_name<-c("SCEN5_PREDMax","SCEN6_PREDMax")
nm_value_sen<-c("0","250","500")
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tablePREDMax %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVh"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Max_Pred, values_from=Count) 
    NB_100<-sum(NB_sens_econ$P_100)
    NB_120<-sum(NB_sens_econ$P_120)
    NB_150<-sum(NB_sens_econ$P_150)
    NB_180<-sum(NB_sens_econ$P_180)
    NB_200<-sum(NB_sens_econ$P_200)
    NB_tableh<-rbind(NB_tableh,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_100,NB_120,NB_150,NB_180,NB_200))
  }
}

NB_tablei<-data.frame()
scen_sens_name<-c("SCEN7_PREDMax","SCEN8_PREDMax")
nm_value_sen<-c("0","250","500")
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_sens_econ<-NB_econ_tablePREDMax %>% filter(Scenario==scen_sens_name[k], Category %in% c("NBPVi"),nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Max_Pred, values_from=Count) 
    NB_100<-sum(NB_sens_econ$P_100)
    NB_120<-sum(NB_sens_econ$P_120)
    NB_150<-sum(NB_sens_econ$P_150)
    NB_180<-sum(NB_sens_econ$P_180)
    NB_200<-sum(NB_sens_econ$P_200)
    NB_tablei<-rbind(NB_tablei,data.frame(Scenario=scen_sens_name[k],nm_value=nm_value_sen[i],
                                          NB_100,NB_120,NB_150,NB_180,NB_200))
  }
}

NB_table_PREDMax<-data.frame(NB_tableh,NB_tablei)
stargazer(t(NB_table_PREDMax),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_table_PREDMax.tex")


#Elasticity 
scen_sens_name<-c("SCEN5_PREDMax","SCEN6_PREDMax")
nm_value_sen<-c("0","250","500")
maxpred<-c(1,1.2,1.5,1.8,2)
NB_elast_predh<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tableh %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_100-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[1]-maxpred[3])/maxpred[3])
    NB_elast2 <-((NB_elast_econ$NB_120-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[2]-maxpred[3])/maxpred[3])
    NB_elast3 <-((NB_elast_econ$NB_150-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[3]-maxpred[3])/maxpred[3])
    NB_elast4 <-((NB_elast_econ$NB_180-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[4]-maxpred[3])/maxpred[3])
    NB_elast5 <-((NB_elast_econ$NB_200-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[5]-maxpred[3])/maxpred[3])
    NB_elast_predh<-rbind(NB_elast_predh,data.frame(Scenario=scen_sens_name[k],
                                                    nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

scen_sens_name<-c("SCEN7_PREDMax","SCEN8_PREDMax")
nm_value_sen<-c("0","250","500")
maxpred<-c(1,1.2,1.5,1.8,2)
NB_elast_predi<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_tablei %>% filter(Scenario==scen_sens_name[k], nm_value==nm_value_sen[i]) 
    NB_elast1 <-((NB_elast_econ$NB_100-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[1]-maxpred[3])/maxpred[3])
    NB_elast2 <-((NB_elast_econ$NB_120-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[2]-maxpred[3])/maxpred[3])
    NB_elast3 <-((NB_elast_econ$NB_150-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[3]-maxpred[3])/maxpred[3])
    NB_elast4 <-((NB_elast_econ$NB_180-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[4]-maxpred[3])/maxpred[3])
    NB_elast5 <-((NB_elast_econ$NB_200-NB_elast_econ$NB_150)/NB_elast_econ$NB_150)/((maxpred[5]-maxpred[3])/maxpred[3])
    NB_elast_predi<-rbind(NB_elast_predi,data.frame(Scenario=scen_sens_name[k],
                                                    nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5))
  }
}

NB_elast_predMax<-data.frame(NB_elast_predh,NB_elast_predi)
stargazer(t(NB_elast_pred),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_pred.tex")


#export all tables to excel
list_of_datasets <- list("NBHuntM" = NB_table_M, "NBHuntF"= NB_table_F, "NBPred" = NB_table_PRED, "NBenvtrans"=NB_env_trans,"NBelastM"=NB_elast_M,
                         "NBelastF"=NB_elast_F,"NBelastPred"=NB_elast_pred,"NBelastPredMax"=NB_elast_predMax)
write.xlsx(list_of_datasets, file = "./scenario_NB_sens_all.xlsx")

############## TABLES FOR EXCEL #####################

# ADULT MALES HUNTING SENS PREVELANCE, ELASTICITY, AND AVERAGE PREY OVER TIMELINE TABLES
{
## population elast
elast_table <- data.frame()
elast_table26 <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
elast_name  <- c("Elast0","Elast05","Elast1","Elast15","Elast20") 
scen_name<-c("SCEN2_SENS","SCEN6_SENS")
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AM %>% 
      filter(Scenario %in% c(scen_name[k]),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table26<-rbind(elast_table26, data.frame( 
      Scenario=scen_name[k],
      Age=age_name[w],
      Hunt_perc=elast_name[j],
      avg_elast=mean(elast_table$ELASTICITY)))
  }
}
}

scen_name<-c("SCEN4_SENS","SCEN8_SENS")
elast_table <- data.frame()
elast_table48 <- data.frame()
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
    for (j in 1:5){
elast_table<- elasticity_AM %>% 
  filter(Scenario %in% c(scen_name[k]),
         Category %in% c("TotalSprey"),
         Year %in% c("20"),
         Age %in% c(age_name[w]),
         Elasticity_Sensitivity_Level %in% c(elast_name[j]))
elast_table48<-rbind(elast_table48, data.frame( 
  Scenario=scen_name[k],
  Age=age_name[w],
  Hunt_perc=elast_name[j],
  avg_elast=mean(elast_table$ELASTICITY)))
 }
}
}
elast_table_M<-cbind(elast_table26,elast_table48)
stargazer(elast_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastM.tex")

# prevalence elast table
age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev0","Prev05","Prev1","Prev15","Prev20") 
hunt_name  <- c("S_0","S_0.05","S_0.1","S_0.15","S_0.2") 
year<-c(as.character(0:19))

prev_table <- data.frame()
prev_table_M <- data.frame()
scen_name<-c("SCEN4_SENS","SCEN8_SENS")
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:length(prev_name)){
    prev_table<- prev_AM %>% 
      filter(Scenario %in% c(scen_name[k]),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
      prev_table_M<-rbind(prev_table_M, 
                         data.frame(Scenario=scen_name[k],
                                    age=age_name[w],
                                    Hunt_perc=hunt_name[j],
                                    avg_elast_prev=mean(prev_table$PREV_Elast)))
    }
}
}

prev_avg_table <- data.frame()
prev_avg_M <- data.frame()
scen_name<-c("SCEN4_SENS","SCEN8_SENS")
for (k in 1:length(scen_name)){
  for (w in 1:length(age_name)){
    for (j in 1:5){
      prev_avg_table<-PREV_DIFF_AM %>% filter(Scenario %in% c(scen_name[k]), 
                                              Category %in% c("PREV"),
                                              Age %in% c(age_name[w]),
                                              Hunt_perc %in% c(hunt_name[j]))
      prev_avg_M<-rbind(prev_avg_M, data.frame(Scenario=scen_name[k],
                                               age=age_name[w],
                                               Hunt_perc=hunt_name[j],
                                               avg_prev=mean(prev_avg_table$Count)))
    }
  }
}

prev_elast_M<-cbind(prev_avg_M,prev_table_M)
stargazer(prev_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastM.tex")

# prevalence count in the last year
prev_table_avg <- data.frame()
prev_table_avg_M <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
hunt_name  <- c("S_0","S_0.05","S_0.1","S_0.15","S_0.2") 
scen_name<-c("SCEN4_SENS","SCEN8_SENS")
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:length(hunt_name)){
    prev_table_avg<- PREV_DIFF_AM %>% 
      filter(Scenario %in% c(scen_name[k]),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table_avg_M<-rbind(prev_table_avg_M, data.frame(Scenario=scen_name[k],age=age_name[w],hunt_perc=hunt_name[j],avg_prev=mean(prev_table_avg$Count)))
  }
}
}
stargazer(prev_table_avg_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prev_elastM.tex")

# prey average for entire timeline
scen_name<- c("SCEN2_SENS","SCEN6_SENS")
prey_table <- data.frame()
prey_table26 <- data.frame()

for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- AD_MALE_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyh"),
             Hunt_perc %in% c(hunt_name[j]))
    prey_table26<-rbind(prey_table26, data.frame(Scenario=scen_name[w],
                                                 hunt_perc=hunt_name[j],
                                                 Pop_avg=mean(prey_table$Count)))
  }
}

scen_name<- c("SCEN4_SENS","SCEN8_SENS")
prey_table <- data.frame()
prey_table48 <- data.frame()
for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- AD_MALE_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyi"),
             Hunt_perc %in% c(hunt_name[j]))
    prey_table48<-rbind(prey_table48, data.frame(Scenario=scen_name[w],
                                                 hunt_perc=hunt_name[j],
                                                 Pop_avg=mean(prey_table$Count)))
  }
}
prey_table_M<-cbind(prey_table26,prey_table48)
stargazer(prey_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prey_table_M.tex")

# cummulative hunting elast

cummul_hunt_table_M <- data.frame()
cummul_table <- data.frame()
scen_name<- c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS")
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2")
for (k in 1:length(scen_name)){
for (j in 1:5){
    cummul_table<- cummul_hunt_AM_elast %>% 
      filter(Scenario %in% c(scen_name[k]),Cummulative_hunt %in% c(hunt_cummul_name[j]))
    cummul_hunt_table_M<-rbind(cummul_hunt_table_M, data.frame( 
      Scenario=scen_name[k],
      Hunt_perc=hunt_cummul_name[j],
      avg_elast_cummul_hunt=mean(cummul_table$Cummul_hunt_Elast)))
}
}
stargazer(cummul_hunt_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_M.tex")

# cummulative cwd elast
cummul_table <- data.frame()
cummul_cwd_table_M <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd0","Cummul_cwd05","Cummul_cwd1","Cummul_cwd15","Cummul_cwd2") 
scen_name<- c("SCEN4_SENS","SCEN8_SENS")
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_cwd_AM_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_cwd %in% c(cwd_cummul_name[j]))
  cummul_cwd_table_M<-rbind(cummul_cwd_table_M, data.frame(
    Scenario=scen_name[k],
    Hunt_perc=cwd_cummul_name[j],
    avg_elast_cummul_cwd=mean(cummul_table$Cummul_cwd_Elast)))
}
}
stargazer(cummul_cwd_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_cwd_table_M.tex")

# cummulative pred elast
cummul_table <- data.frame()
cummul_pred_table_M <- data.frame()
pred_cummul_name  <- c("Cummul_pred0","Cummul_pred05","Cummul_pred1","Cummul_pred15","Cummul_pred2") 
scen_name<- c("SCEN6_SENS","SCEN8_SENS")
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_pred_AM_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_pred %in% c(pred_cummul_name[j]))
  cummul_pred_table_M<-rbind(cummul_pred_table_M, data.frame( 
    Scenario=scen_name[k],
    Hunt_perc=pred_cummul_name[j],
    avg_elast_cummul_pred=mean(cummul_table$Cummul_pred_Elast)))
}
}
stargazer(cummul_pred_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_pred_table_M.tex")

tables_male<-list("cummul_cwd_table_M"=cummul_cwd_table_M,"cummul_hunt_table_M"=cummul_hunt_table_M,"cummul_pred_table_M"=cummul_pred_table_M,
                    "prey_table_M"=prey_table_M,"avg_prev_Year20_M"=prev_table_avg_M,"prev_elast_M"=prev_elast_M,"pop_elast_table_M"=elast_table_M)
write.xlsx(tables_male,"elast_and_prev_tables_M.xlsx")


# ADULT FEMALES HUNTING SENS ELASTICITY, PREVELANCE, AND AVERGAE PREY COUNT TABLES

elast_table <- data.frame()
elast_table26 <- data.frame()
age_name    <- c("Fawn","Juv","Adult")
elast_name  <- c("Elast0","Elast025","Elast05","Elast075","Elast10") 
scen_name   <- c("SCEN2_SENS","SCEN6_SENS")
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c(scen_name[k]),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table26<-rbind(elast_table26, data.frame(
      Scenario=scen_name[k],
      Age=age_name[w],
      Hunt_perc=elast_name[j],
      avg_elast=mean(elast_table$ELASTICITY)))
  }
}
}

scen_name<- c("SCEN4_SENS","SCEN8_SENS")
elast_table <- data.frame()
elast_table48 <- data.frame()
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c(scen_name[k]),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table48<-rbind(elast_table48, data.frame( 
      Scenario=scen_name[k],
      Age=age_name[w],
      Hunt_perc=elast_name[j],
      avg_elast=mean(elast_table$ELASTICITY)))
  }
  }
}
elast_table_F<-cbind(elast_table26,elast_table48)

stargazer(elast_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastF.tex")

# prev elast and avg elast

age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev0","Prev025","Prev05","Prev075","Prev10") 
scen_name   <- c("SCEN2_SENS","SCEN6_SENS")
hunt_name  <- c("S_0","S_0.025","S_0.05","S_0.075","S_0.1") 

prev_table <- data.frame()
prev_table48 <- data.frame()
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table<- prev_AF %>% 
      filter(Scenario %in% c(scen_name[k]),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table48<-rbind(prev_table48, data.frame(Scenario= scen_name[k],
                                                 age=age_name[w],
                                               hunt_perc=hunt_name[j],
                                               avg_elast_prev=mean(prev_table$PREV_Elast)))
  }
}
}

scen_name   <- c("SCEN4_SENS","SCEN8_SENS")
prev_avg_table <- data.frame()
prev_avg48 <- data.frame()
for(k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_avg_table<-PREV_DIFF_AF %>% filter(Scenario %in% c(scen_name[k]), 
                                            Category %in% c("PREV"),
                                            Age %in% c(age_name[w]),
                                            Hunt_perc %in% c(hunt_name[j]))
    prev_avg48<-rbind(prev_avg48, data.frame(Scenario=scen_name[k],
                                             age=age_name[w],
                                           Hunt_perc=hunt_name[j],
                                           avg_prev=mean(prev_avg_table$Count)))
  }
}
}
prev_elast_F<-cbind(prev_table48,prev_avg48)
stargazer(prev_elast_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prev_F.tex")

# prev count
age_name    <- c("Fawn","Juv","Adult","Total_pop")
hunt_name  <- c("S_0","S_0.025","S_0.05","S_0.075","S_0.1") 
scen_name   <- c("SCEN4_SENS","SCEN8_SENS")

prev_table_avg <- data.frame()
prev_table_avg_F <- data.frame()
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:length(hunt_name)){
    prev_table_avg<- PREV_DIFF_AF %>% 
      filter(Scenario %in% c(scen_name[k]),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table_avg_F<-rbind(prev_table_avg_F, data.frame(Scenario=scen_name[k],age=age_name[w],hunt_perc=hunt_name[j],avg_prev=mean(prev_table_avg$Count)))
  }
}
}

stargazer(prev_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevF.tex")

# prey average over entire timeline

hunt_name  <- c("S_0","S_0.025","S_0.05","S_0.075","S_0.1") 
scen_name<- c("SCEN2_SENS","SCEN6_SENS")
prey_table <- data.frame()
prey_table26 <- data.frame()
for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- AD_FEMALE_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyh"),
             Hunt_perc %in% c(hunt_name[j]))
    prey_table26<-rbind(prey_table26, data.frame(Scenario=scen_name[w],hunt_perc=hunt_name[j],Pop_avg=mean(prey_table$Count)))
  }
}

scen_name<- c("SCEN4_SENS","SCEN8_SENS")
prey_table <- data.frame()
prey_table48 <- data.frame()
for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- AD_FEMALE_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyi"),
             Hunt_perc %in% c(hunt_name[j]))
    prey_table48<-rbind(prey_table48, data.frame(Scenario=scen_name[w],hunt_perc=hunt_name[j],Pop_avg=mean(prey_table$Count)))
  }
}

prey_table_F<-cbind(prey_table26,prey_table48)
stargazer(prey_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevF.tex")

# cummulative hunting elast

cummul_table <- data.frame()
cummul_hunt_table_F <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1") 
scen_name<- c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS")

for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_hunt_AF_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_hunt_table_F<-rbind(cummul_hunt_table_F, data.frame(
    Scenario=scen_name[k],
    Hunt_perc=pred_cummul_name[j],
    avg_elast_cummul_hunt2=mean(cummul_table$Cummul_hunt_Elast)))
}
}
stargazer(cummul_hunt_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_F.tex")

# cummulative cwd death elast

scen_name<- c("SCEN4_SENS","SCEN8_SENS")
cummul_table <- data.frame()
cummul_cwd_table_F <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd0","Cummul_cwd025","Cummul_cwd05","Cummul_cwd075","Cummul_cwd1") 
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_cwd_AF_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_cwd %in% c(cwd_cummul_name[j]))
  cummul_cwd_table_F<-rbind(cummul_cwd_table_F, data.frame( 
    Scenario=scen_name[k],
    Hunt_perc=pred_cummul_name[j],
    avg_elast_cummul_cwd=mean(cummul_table$Cummul_cwd_Elast)))
} 
}
stargazer(cummul_cwd_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_F.tex")

# cummulative pred death elast
scen_name<- c("SCEN6_SENS","SCEN8_SENS")
cummul_table <- data.frame()
cummul_pred_table_F <- data.frame()
pred_cummul_name  <- c("Cummul_pred0","Cummul_pred025","Cummul_pred05","Cummul_pred075","Cummul_pred1") 
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_pred_AF_elast %>% 
    filter(Scenario %in% c(scen_name[k]), Cummulative_pred %in% c(pred_cummul_name[j]))
  cummul_pred_table_F<-rbind(cummul_pred_table_F, data.frame(
    Scenario=scen_name[k],
    Hunt_perc=pred_cummul_name[j],
    avg_elast_cummul_pred=mean(cummul_table$Cummul_pred_Elast)))
}
}
stargazer(cummul_pred_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_pred_table_F.tex")

tables_female<-list("cummul_cwd_table_F"=cummul_cwd_table_F,"cummul_hunt_table_F"=cummul_hunt_table_F,"cummul_pred_table_F"=cummul_pred_table_F,
                    "prey_table_F"=prey_table_F,"prev_table_F"=prev_table_F,"prev_elast_F"=prev_elast_F,"elast_table_F"=elast_table_F)
write.xlsx(tables_female,"elast_and_prev_tables_F.xlsx")

# PREDATION Kill Rate SENS PREVELANCE, ELASTICITY AND PREY COUNT AVERAGE OVER THE ENTIRE TIMEPERIOD

## population elast
elast_table <- data.frame()
elast_table26 <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
elast_name  <- c("Elast20","Elast50","Elast80","Elast100","Elast120") 

for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_PRED %>% 
      filter(Scenario %in% c("SCEN6_PRED"),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Predation_Level %in% c(elast_name[j]))
    elast_table26<-rbind(elast_table26, data.frame(
      Scenario="SCEN6_PRED",
      Age=age_name[w],
      Hunt_perc=elast_name[j],
      avg_elast=mean(elast_table$Predation)))
  }
}

elast_table <- data.frame()
elast_table48 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_PRED %>% 
      filter(Scenario %in% c("SCEN8_PRED"),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Predation_Level %in% c(elast_name[j]))
    elast_table48<-rbind(elast_table48, data.frame(
      Scenario="SCEN8_PRED",
      Age=age_name[w],
      Hunt_perc=elast_name[j],
      avg_elast=mean(elast_table$Predation)))
  }
}
elast_table_PRED<-cbind(elast_table26,elast_table48)
stargazer(elast_table_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastPRED.tex")

# prevelance elast table
age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev20","Prev50","Prev80","Prev100","Prev120") 
pred_name  <- c("P_20","P_50","P_80","P_100","P_120") 
year<-c(as.character(0:19))

prev_table <- data.frame()
prev_table_PRED <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:length(prev_name)){
    prev_table<- prev_PRED %>% 
      filter(Scenario %in% c("SCEN8_PRED"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table_PRED<-rbind(prev_table_PRED, 
                        data.frame(Scenario="SCEN8_PRED",
                                     age=age_name[w],
                                   Kill_rate=pred_name[j],
                                   avg_elast_prev=mean(prev_table$PREV_Elast)))
  }
}

pred_name  <- c("P_20","P_50","P_80","P_100","P_120") 
prev_avg_table <- data.frame()
prev_avg_PRED <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_avg_table<-PREV_DIFF_PRED %>% filter(Scenario %in% c("SCEN8_PRED"), 
                                            Category %in% c("PREV"),
                                            Age %in% c(age_name[w]),
                                            Kill_rate %in% c(pred_name[j]))
    prev_avg_PRED<-rbind(prev_avg_PRED, data.frame(Scenario="SCEN8_PRED",
                                                   age=age_name[w],
                                             Kill_rate=pred_name[j],
                                             avg_prev=mean(prev_avg_table$Count)))
  }
}
prev_elast_PRED<-cbind(prev_table_PRED,prev_avg_PRED)
stargazer(prev_elast_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastPRED.tex")

# prevalence count in last year
prev_table_avg <- data.frame()
prev_table_avg_PRED <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
pred_name  <- c("P_20","P_50","P_80","P_100","P_120") 
scen_name<- c("SCEN7_PRED","SCEN8_PRED")
for (k in 1:length(scen_name)){
for (w in 1:length(age_name)){
  for (j in 1:length(pred_name)){
    prev_table_avg<- PREV_DIFF_PRED %>% 
      filter(Scenario %in% c(scen_name[k]),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Kill_rate %in% c(pred_name[j]))
    prev_table_avg_PRED<-rbind(prev_table_avg_PRED, data.frame(Scenario=scen_name[k],age=age_name[w],kill_rate=pred_name[j],avg_prev=mean(prev_table_avg$Count)))
  }
}
}
stargazer(prev_table_avg_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevelastPRED.tex")

# prey average for entire timeline
scen_name<- c("SCEN5_PRED","SCEN6_PRED")
prey_table <- data.frame()
prey_table56 <- data.frame()
pred_name  <- c("P_20","P_50","P_80","P_100","P_120") 

for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- PRED_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyh"),
             Kill_rate %in% c(pred_name[j]))
    prey_table56<-rbind(prey_table56, data.frame(Scenario=scen_name[w],
                                                 kill_rate=pred_name[j],
                                                 Pop_avg=mean(prey_table$Count)))
  }
}

scen_name<- c("SCEN7_PRED","SCEN8_PRED")
prey_table <- data.frame()
prey_table78 <- data.frame()
for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- PRED_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyi"),
             Kill_rate %in% c(pred_name[j]))
    prey_table78<-rbind(prey_table78, data.frame(scen=scen_name[w],
                                                 kill_rate=pred_name[j],
                                                 Pop=mean(prey_table$Count)))
  }
}
prey_table_PRED<-cbind(prey_table56,prey_table78)
stargazer(prey_table_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prey_table_PRED.tex")

# cummulative hunting elast

cummul_hunt_table_PRED <- data.frame()
cummul_table <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt20","Cummul_hunt50","Cummul_hunt80","Cummul_hunt100","Cummul_hunt120") 
scen_name<-c("SCEN6_PRED","SCEN8_PRED")
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_hunt_PRED_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_hunt_table_PRED<-rbind(cummul_hunt_table_PRED, data.frame( 
    Scenario=scen_name[k],
    Kill_rate=pred_name[j],
    avg_elast_cummul_hunt=mean(cummul_table$Cummul_hunt_Elast)))
} 
}
stargazer(cummul_hunt_table_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_PRED.tex")

# cummulative cwd elast
cummul_table <- data.frame()
cummul_cwd_table_PRED <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd20","Cummul_cwd50","Cummul_cwd80","Cummul_cwd100","Cummul_cwd120") 
scen_name<-c("SCEN7_PRED","SCEN8_PRED")
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_cwd_PRED_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_cwd %in% c(cwd_cummul_name[j]))
  cummul_cwd_table_PRED<-rbind( cummul_cwd_table_PRED, data.frame( 
    Scenario=scen_name[k],
    Kill_rate=pred_name[j],
    avg_elast_cummul_cwd=mean(cummul_table$Cummul_cwd_Elast)))
} 
}
stargazer(cummul_cwd_table_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_cwd_table_PRED.tex")

# cummulative pred elast
cummul_table <- data.frame()
cummul_pred_table_PRED <- data.frame()
pred_cummul_name  <- c("Cummul_pred20","Cummul_pred50","Cummul_pred80","Cummul_pred100","Cummul_pred120") 
scen_name<-c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED")
for (k in 1:length(scen_name)){
for (j in 1:5){
  cummul_table<- cummul_pred_PRED_elast %>% 
    filter(Scenario %in% c(scen_name[k]),Cummulative_pred %in% c(pred_cummul_name[j]))
  cummul_pred_table_PRED<-rbind(cummul_pred_table_PRED, data.frame( 
    Scenario=scen_name[k],
    Kill_rate=pred_name[j],
    avg_elast_cummul_pred=mean(cummul_table$Cummul_pred_Elast)))
}
}
stargazer(cummul_pred_table_PRED,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_pred_table_PRED.tex")

tables_PRED<-list("cummul_cwd_table_PRED"=cummul_cwd_table_PRED,"cummul_hunt_table_PRED"=cummul_hunt_table_PRED,"cummul_pred_table_PRED"=cummul_pred_table_PRED,
                  "prey_table_PRED"=prey_table_PRED,"prev_table_PRED"=prev_table_avg_PRED,"prev_elast_PRED"=prev_elast_PRED,"elast_table_PRED"=elast_table_PRED)
write.xlsx(tables_PRED,"elast_and_prev_tables_predation.xlsx")


# PREDATION SENS Max Predators PREVELANCE, ELASTICITY AND PREY COUNT AVERAGE OVER THE ENTIRE TIMEPERIOD

## population elast
elast_table <- data.frame()
elast_table26 <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
elast_name  <- c("Elast100","Elast120","Elast150","Elast180","Elast200") 

for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_PREDMax %>% 
      filter(Scenario %in% c("SCEN6_PREDMax"),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Predation_Level %in% c(elast_name[j]))
    elast_table26<-rbind(elast_table26, data.frame(
      Scenario="SCEN6_PREDMax",
      Age=age_name[w],
      Max_Pred=elast_name[j],
      avg_elast=mean(elast_table$Predation)))
  }
}

elast_table <- data.frame()
elast_table48 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_PREDMax %>% 
      filter(Scenario %in% c("SCEN8_PREDMax"),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Predation_Level %in% c(elast_name[j]))
    elast_table48<-rbind(elast_table48, data.frame(
      Scenario="SCEN8_PREDMax",
      Age=age_name[w],
      Max_Pred=elast_name[j],
      avg_elast=mean(elast_table$Predation)))
  }
}
elast_table_PREDMax<-cbind(elast_table26,elast_table48)
stargazer(elast_table_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastPREDMax.tex")

# prevelance elast table
age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev100","Prev120","Prev150","Prev180","Prev200") 
pred_name  <- c("P_100","P_120","P_150","P_180","P_200") 
year<-c(as.character(0:19))

prev_table <- data.frame()
prev_table_PREDMax <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:length(prev_name)){
    prev_table<- prev_PREDMax %>% 
      filter(Scenario %in% c("SCEN8_PREDMax"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table_PREDMax<-rbind(prev_table_PREDMax, 
                           data.frame(Scenario="SCEN8_PREDMax",
                                      age=age_name[w],
                                      Max_Pred=pred_name[j],
                                      avg_elast_prev=mean(prev_table$PREV_Elast)))
  }
}

pred_name  <- c("P_100","P_120","P_150","P_180","P_200") 
prev_avg_table <- data.frame()
prev_avg_PREDMax <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_avg_table<-PREV_DIFF_PREDMax %>% filter(Scenario %in% c("SCEN8_PREDMax"), 
                                              Category %in% c("PREV"),
                                              Age %in% c(age_name[w]),
                                              Max_Pred %in% c(pred_name[j]))
    prev_avg_PREDMax<-rbind(prev_avg_PREDMax, data.frame(Scenario="SCEN8_PREDMax",
                                                   age=age_name[w],
                                                   Max_Pred=pred_name[j],
                                                   avg_prev=mean(prev_avg_table$Count)))
  }
}
prev_elast_PREDMax<-cbind(prev_table_PREDMax,prev_avg_PREDMax)
stargazer(prev_elast_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastPREDMax.tex")

# prevalence count in last year
prev_table_avg <- data.frame()
prev_table_avg_PREDMax <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
pred_name  <- c("P_100","P_120","P_150","P_180","P_200") 
scen_name<- c("SCEN7_PREDMax","SCEN8_PREDMax")
for (k in 1:length(scen_name)){
  for (w in 1:length(age_name)){
    for (j in 1:length(pred_name)){
      prev_table_avg<- PREV_DIFF_PREDMax %>% 
        filter(Scenario %in% c(scen_name[k]),
               Year %in% c("20"),
               Age %in% c(age_name[w]),
               Category %in% c("PREV"),
               Max_Pred %in% c(pred_name[j]))
      prev_table_avg_PREDMax<-rbind(prev_table_avg_PREDMax, data.frame(Scenario=scen_name[k],age=age_name[w],Max_Pred=pred_name[j],avg_prev=mean(prev_table_avg$Count)))
    }
  }
}
stargazer(prev_table_avg_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevelastPREDMax.tex")

# prey average for entire timeline
scen_name<- c("SCEN5_PREDMax","SCEN6_PREDMax")
prey_table <- data.frame()
prey_table56 <- data.frame()
pred_name  <- c("P_100","P_120","P_150","P_180","P_200") 

for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- PREDMax_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyh"),
             Max_Pred %in% c(pred_name[j]))
    prey_table56<-rbind(prey_table56, data.frame(Scenario=scen_name[w],
                                                 Max_Pred=pred_name[j],
                                                 Pop_avg=mean(prey_table$Count)))
  }
}

scen_name<- c("SCEN7_PREDMax","SCEN8_PREDMax")
prey_table <- data.frame()
prey_table78 <- data.frame()
for (w in 1:length(scen_name)){
  for (j in 1:5){
    prey_table<- PREDMax_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyi"),
             Max_Pred %in% c(pred_name[j]))
    prey_table78<-rbind(prey_table78, data.frame(scen=scen_name[w],
                                                 Max_Pred=pred_name[j],
                                                 Pop=mean(prey_table$Count)))
  }
}
prey_table_PREDMax<-cbind(prey_table56,prey_table78)
stargazer(prey_table_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prey_table_PREDMax.tex")

# cummulative hunting elast

cummul_hunt_table_PREDMax <- data.frame()
cummul_table <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt100","Cummul_hunt120","Cummul_hunt150","Cummul_hunt180","Cummul_hunt200") 
scen_name<-c("SCEN6_PREDMax","SCEN8_PREDMax")
for (k in 1:length(scen_name)){
  for (j in 1:5){
    cummul_table<- cummul_hunt_PREDMax_elast %>% 
      filter(Scenario %in% c(scen_name[k]),Cummulative_hunt %in% c(hunt_cummul_name[j]))
    cummul_hunt_table_PREDMax<-rbind(cummul_hunt_table_PREDMax, data.frame( 
      Scenario=scen_name[k],
      Max_Pred=pred_name[j],
      avg_elast_cummul_hunt=mean(cummul_table$Cummul_hunt_Elast)))
  } 
}
stargazer(cummul_hunt_table_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_PREDMax.tex")

# cummulative cwd elast
cummul_table <- data.frame()
cummul_cwd_table_PREDMax <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd100","Cummul_cwd120","Cummul_cwd150","Cummul_cwd180","Cummul_cwd200") 
scen_name<-c("SCEN7_PREDMax","SCEN8_PREDMax")
for (k in 1:length(scen_name)){
  for (j in 1:5){
    cummul_table<- cummul_cwd_PREDMax_elast %>% 
      filter(Scenario %in% c(scen_name[k]),Cummulative_cwd %in% c(cwd_cummul_name[j]))
    cummul_cwd_table_PREDMax<-rbind(cummul_cwd_table_PREDMax, data.frame( 
      Scenario=scen_name[k],
      Max_Pred=pred_name[j],
      avg_elast_cummul_cwd=mean(cummul_table$Cummul_cwd_Elast)))
  } 
}
stargazer(cummul_cwd_table_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_cwd_table_PREDMax.tex")

# cummulative pred elast
cummul_table <- data.frame()
cummul_pred_table_PREDMax <- data.frame()
pred_cummul_name  <- c("Cummul_pred100","Cummul_pred120","Cummul_pred150","Cummul_pred180","Cummul_pred200") 
scen_name<-c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax")
for (k in 1:length(scen_name)){
  for (j in 1:5){
    cummul_table<- cummul_pred_PREDMax_elast %>% 
      filter(Scenario %in% c(scen_name[k]),Cummulative_pred %in% c(pred_cummul_name[j]))
    cummul_pred_table_PREDMax<-rbind(cummul_pred_table_PREDMax, data.frame( 
      Scenario=scen_name[k],
      Max_pred=pred_name[j],
      avg_elast_cummul_pred=mean(cummul_table$Cummul_pred_Elast)))
  }
}
stargazer(cummul_pred_table_PREDMax,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_pred_table_PREDMax.tex")

tables_PREDMax<-list("cummul_cwd_table_PREDMax"=cummul_cwd_table_PREDMax,"cummul_hunt_table_PREDMax"=cummul_hunt_table_PREDMax,"cummul_pred_table_PREDMax"=cummul_pred_table_PREDMax,
                  "prey_table_PREDMax"=prey_table_PREDMax,"prev_table_PREDMax"=prev_table_avg_PREDMax,"prev_elast_PREDMax"=prev_elast_PREDMax,"elast_table_PREDMax"=elast_table_PREDMax)
write.xlsx(tables_PREDMax,"elast_and_prev_tables_predationMax.xlsx")
}

##### Graphs for Net Benefit tables #########

NB_econ_tableM
NB_econ_tableF
NB_econ_tablePRED
NB_econ_tablePREDMax


scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.025,0.05,0.075,0.1)
NB_elast_testF<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_econ_tableF %>% filter(Scenario==scen_sens_name[k],Category %in% c("NBPVi"), nm_sen==nm_value_sen[i]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
    NB_elast1 <-((NB_elast_econ$S_0-NB_elast_econ$S_0.05)/NB_elast_econ$S_0.05)
    neg100perc_change<-((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$S_0.025-NB_elast_econ$S_0.05)/NB_elast_econ$S_0.05)
    neg50perc_change<-((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$S_0.05-NB_elast_econ$S_0.05)/NB_elast_econ$S_0.05)
    no_perc_change<-((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$S_0.075-NB_elast_econ$S_0.05)/NB_elast_econ$S_0.05)
    pos50perc_change<-((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$S_0.1-NB_elast_econ$S_0.05)/NB_elast_econ$S_0.05)
    pos100perc_change<-((huntperc[5]-huntperc[3])/huntperc[3])
    NB_elast_testF<-rbind(NB_elast_testF,data.frame(Scenario=scen_sens_name[k],
                                                    Month=NB_elast_econ$Month,
                                                    nm_value=nm_value_sen[i],
                                                    NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5,
                                                    neg100perc_change,neg50perc_change,no_perc_change,pos50perc_change,pos100perc_change))
  }
}

NB_elast_testF[is.na(NB_elast_testF)] <- 0
NB_elast_testF<-NB_elast_testF %>% filter(NB_elast1!=0)
#NB_elast_testF<-NB_elast_testF %>% pivot_longer(. , cols = c("NB_elast1":"NB_elast5"),names_to = "Elasticity_Category", values_to= "Elasticity")
NB_elast_testF<-NB_elast_testF %>% pivot_longer(. , cols = c("neg100perc_change":"pos100perc_change"),names_to = "Hunting_Elast_Category", values_to= "Hunting_Elasticity")



scen_sens_name<-c("SCEN4_SENS","SCEN8_SENS")
nm_value_sen<-c("0","250","500")
huntperc<-c(0,0.05,0.1,0.15,0.2)
NB_elast_testM<-data.frame()
for (k in c(1,2)){
  for (i in c(1,2,3)){
    NB_elast_econ<-NB_econ_tableM %>% filter(Scenario==scen_sens_name[k],Category %in% c("NBPVi"), nm_sen==nm_value_sen[1]) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 
    NB_elast1 <-((NB_elast_econ$S_0-NB_elast_econ$S_0.1)/NB_elast_econ$S_0.1)
    neg100perc_change<-((huntperc[1]-huntperc[3])/huntperc[3])
    NB_elast2 <-((NB_elast_econ$S_0.05-NB_elast_econ$S_0.1)/NB_elast_econ$S_0.1)
    neg50perc_change<-((huntperc[2]-huntperc[3])/huntperc[3])
    NB_elast3 <-((NB_elast_econ$S_0.1-NB_elast_econ$S_0.1)/NB_elast_econ$S_0.1)
    no_perc_change<-((huntperc[3]-huntperc[3])/huntperc[3])
    NB_elast4 <-((NB_elast_econ$S_0.15-NB_elast_econ$S_0.1)/NB_elast_econ$S_0.1)
    pos50perc_change<-((huntperc[4]-huntperc[3])/huntperc[3])
    NB_elast5 <-((NB_elast_econ$S_0.2-NB_elast_econ$S_0.1)/NB_elast_econ$S_0.1)
    pos100perc_change<-((huntperc[5]-huntperc[3])/huntperc[3])
    NB_elast_testM<-rbind(NB_elast_testM,data.frame(Scenario=scen_sens_name[k],
                                                    Month=NB_elast_econ$Month,
                                                    nm_value=nm_value_sen[i],NB_elast1,NB_elast2,NB_elast3,NB_elast4,NB_elast5,
                                                    neg100perc_change,neg50perc_change,no_perc_change,pos50perc_change,pos100perc_change))
  }
}

NB_elast_testM[is.na(NB_elast_testM)] <- 0
NB_elast_testM<-NB_elast_testM %>% filter(NB_elast1!=0)
#NB_elast_testM<-NB_elast_testM %>% pivot_longer(. , cols = c("NB_elast1":"NB_elast5"),names_to = "Elasticity_Category", values_to= "Elasticity")
NB_elast_testM<-NB_elast_testM %>% pivot_longer(. , cols = c("neg100perc_change":"pos100perc_change"),names_to = "Hunting_Elast_Category", values_to= "Hunting_Elasticity")


NBPV_tableM<- NB_econ_tableM %>% filter(Category %in% c("NBPVi","NBPVh"),Scenario %in% c("SCEN8_SENS"))
CountM<-NBPV_tableM$Count
Hunt_percM<-NBPV_tableM$Hunt_perc

NBPV_table_comp<- NB_econ_tableF %>% filter(Category %in% c("NBPVi","NBPVh"), Scenario %in% c("SCEN8_SENS"))%>% 
  add_column(CountM,.after = "Count") %>% 
  add_column(Hunt_percM,.after = "Hunt_perc")

NBPV_table_compM<- NB_econ_tableM %>% filter(Category %in% c("NBPVi","NBPVh"), Scenario %in% c("SCEN8_SENS"))%>% 
  separate(Hunt_perc, into = c("char","Perc"), sep = "S_", remove = TRUE)

NBPV_table_compM<-NBPV_table_compM %>% mutate(perc_change=(as.numeric(Perc)-0.1)/0.1)


NBPV_table_compF<- NB_econ_tableF %>% filter(Category %in% c("NBPVi","NBPVh"), Scenario %in% c("SCEN8_SENS"))%>% 
  separate(Hunt_perc, into = c("char","Perc"), sep = "S_", remove = TRUE)

NBPV_table_compF<-NBPV_table_compF %>% mutate(perc_change=(as.numeric(Perc)-0.05)/0.05) 




