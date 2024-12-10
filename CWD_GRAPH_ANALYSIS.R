

### HERE WE ARE PULLING IN THE DATA NEEDED TO MAKE THE GRAPHS

require(tidyverse)
require(RSQLite)
require(DBI)

# LOAD IN THE FUNCTIONS
source("CWD_SCENARIO_SCRIPT.R")
source("HUNT_SENS_ANALYSIS.R")
source("PRED_SENS_killrate_ANALYSIS.R")
source("PRED_SENS_predmax_ANALYSIS.R")


### SCENARIO TABLES AND ASSOCITATED GRAPHS

## SCENARIO TABLES

scen_table<- master_table %>%
  #mutate(Total_Pop_H= case_when(Category %in%  c("FpreyA","MpreyA") ~ sum(Count)), 
  #Total_Pop_I= case_when(Category %in%  c("SFpreyA","SMpreyA","IFpreyA","IMpreyA") ~ sum(Count))) %>% 
  pivot_wider(names_from= Scenario, values_from = Count)

# SCENARIO 1: PREY
scen1_table<- master_table %>% filter(Scenario %in% "SCEN1",
                                      Category %in% c("FpreyA","MpreyA","Totalprey"))
ggplot(data = scen1_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category)) +
  labs(title = "Elk Population Count",
       subtitle = "Scenario 1: Prey Model",
       y = "Elk Count", x = "Month")

# SCENARIO 2: PREY WITH HUNTING
scen2_table<- master_table %>% filter(Scenario %in% "SCEN2",
                                      Category %in% c("FpreyA","MpreyA","Totalprey"))
ggplot(data = scen2_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category)) +
  labs(title = "Elk Population Count",
       subtitle = "Scenario 2: Prey Model with Hunting",
       y = "Elk Count", x = "Month")

# SCENARIO 3: PREY WITH INFECTION
scen3_table<- master_table %>% filter(Scenario %in% "SCEN3",
                                      Category %in% c("SFpreyA","SMpreyA","TotalSprey",
                                                      "Totalprey"))
ggplot(data = scen3_table, mapping = aes(x = Month, y = Count)) +
  geom_line() +
  facet_wrap(facets =  vars(Category))+
  labs(title = "Elk Population Count",
       subtitle = "Scenario 3: Prey Model with Infection",
       y = "Elk Count", x = "Month")

# SCENARIO 4: PREY WITH HUNITNG AND INFECTION
scen4_table<- master_table %>% filter(Scenario %in% "SCEN4",
                                      Category %in% c("SFpreyA","SMpreyA","TotalSprey",
                                                      "Totalprey"))
ggplot(data = scen4_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category))+
  labs(title = "Elk Population Count",
       subtitle = "Scenario 4: Prey Model with Hunting and Infection",
       y = "Elk Count", x = "Month")

# SCENARIO 5: PRED-RPREY 
scen5_table<- master_table %>% filter(Scenario %in% "SCEN5",
                                      Category %in% c("FpreyA","MpreyA","Totalprey"))
ggplot(data = scen5_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category)) +
  labs(title = "Elk Population Count",
       subtitle = "Scenario 5: Pred-Prey Model",
       y = "Elk Count", x = "Month")

# SCENARIO 6: PRED-PREY WITH HUNTING
scen6_table<- master_table %>% filter(Scenario %in% "SCEN6",
                                      Category %in% c("FpreyA","MpreyA","Totalprey"))
ggplot(data = scen6_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category))+
  labs(title = "Elk Population Count",
       subtitle = "Scenario 6: Pred-Prey Model with Hunting",
       y = "Elk Count", x = "Month")

# SCENARIO 7; PRED-PREY WITH INFECTION
scen7_table<- master_table %>% filter(Scenario %in% "SCEN7",
                                      Category %in% c("SFpreyA","SMpreyA","TotalSprey",
                                                      "Totalprey"))
ggplot(data = scen7_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category))+
  labs(title = "Elk Population Count",
       subtitle = "Scenario 7: Pred-Prey Model with Infection",
       y = "Elk Count", x = "Month")

# SCENARIO 8; PRED-pREY WITH HUNTING AND INFECTION
scen8_table<- master_table %>% filter(Scenario %in% "SCEN8",
                                      Category %in% c("SFpreyA","SMpreyA","TotalSprey",
                                                      "Totalprey"))
ggplot(data = scen8_table, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_wrap(facets =  vars(Category))+
  labs(title = "Elk Population Count",
       subtitle = "Scenario 8: Pred-Prey Model with Hunting and Infection",
       y = "Elk Count", x = "Month")

## TABLES FOR COMPARISON OF DIFFERENT CATEGORIES

## COMPARISON TABLES

# POPULATION

# COMPARISON OF 4 AND 8 HEALTHY POP

pop_comp_healthy<- master_table %>% filter(Scenario %in% c("SCEN4","SCEN8"),
                                           Category %in% c("Totalpreyi"))
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Category",values=c("#88CCEE", "#CC6677"), labels=c("Total Population: Scen4","Total Population: Scen8"))+
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 4 and 8: Population",
       y = "Elk Count", x = "Month")

pop_comp_healthy<- master_table1 %>% filter(Scenario %in% c("SCEN2","SCEN6"), Age %in% c("Total_pop"),
                                           Category %in% c("Totalpreyh"))
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_line()+
  facet_grid(~Scenario)+
  scale_color_manual("Category",values=c("#88CCEE", "#CC6677"), labels=c("Total Population: Scen2","Total Population: Scen6"))+
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 2 and 6: Population",
       y = "Elk Count", x = "Month")

pop_comp_healthy<- master_table %>% filter(Scenario %in% c("SCEN2","SCEN4"),Category %in% c("Totalpreyh","Totalpreyi")) %>% 
  mutate(Count = ifelse(Count==0, NA, Count))
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Category)) +
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Category",values=c("#88CCEE", "#CC6677"), labels=c("Total Population: Scen2","Total Population: Scen4"))+
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 2 and 4: Population",
       y = "Elk Count", x = "Month")


pop_comp_healthy<- master_table %>% filter(Scenario %in% c("SCEN6","SCEN8"),Category %in% c("Totalpreyh","Totalpreyi")) %>% 
  mutate(Count = ifelse(Count==0, NA, Count))
ggplot(data = pop_comp_healthy, mapping = aes(x = Month, y = Count, color = Category)) +
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Category",values=c("#88CCEE", "#CC6677"), labels=c("Total Population: Scen6","Total Population: Scen8"))+
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 6 and 8: Population",
       y = "Elk Count", x = "Month")


# COMPARISON OF 4 AND 8 INFECTED POP

pop_comp_inf<- master_table %>% filter(Scenario %in% c("SCEN4","SCEN8"),
                                           Category %in% c("IFpreyA","IMpreyA"))
ggplot(data = pop_comp_inf, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_line()+
  facet_grid(Age~.)+
  #geom_smooth() +
  #facet_grid(Scenario ~ Category) +
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 4 and 8: Infected",
       y = "Elk Count", x = "Month")

pop_comp_infected<- master_table %>% filter(Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = pop_comp_infected, mapping = aes(x = Month, y = Count, color = Scenario)) +
  #geom_line() +
  geom_smooth() +
  #facet_grid(Category ~ Scenario) 
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Scenario 5-8",
       y = "Elk Count", x = "Month")

pop_comp_total<-master_table %>% filter(Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = pop_comp_total, mapping = aes(x = Month, y = Count, color = Scenario)) +
  #geom_line() +
  geom_smooth() +
  #facet_grid(Category ~ Scenario) 
  labs(title = "Elk Population Count",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")

# CWD TABLE

CWD_comp3478<- master_table1 %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"),Age %in% c("Fawn","Juv","Adult"), Category %in% c("TotalCWDdeath"))
ggplot(data = CWD_comp3478, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  scale_color_manual("Category",values=c("#88CCEE", "#CC6677", "#E69F00"), labels=c("Fawn","Juv","Adult"))+
  facet_grid(~Scenario)+
  labs(title = "CWD Death Count",
       subtitle = "Comparison of all Scenarios",
       y = "Elk Count", x = "Month")

CWD_comp3478<- master_table1 %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"),Age %in% c("Total_pop"), Category %in% c("TotalCWDdeath"))
ggplot(data = CWD_comp3478, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  scale_color_manual("Category",values=c("#CC6677"), labels=c("Total CWD death"))+
  facet_grid(~Scenario)+
  labs(title = "CWD Death Count",
       subtitle = "Comparison of all Scenarios",
       y = "Elk Count", x = "Month")

CWD_comp48<- master_table %>% filter(Scenario %in% c("SCEN4","SCEN8"),
                                       Category %in% c("FCWDdeath","MCWDdeath"))
ggplot(data = CWD_comp48, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_grid(Category ~ Scenario)+
  labs(title = "CWD Death Count by Age",
       subtitle = "Comparison of 4 and 8",
       y = "Elk Count", x = "Month")


CWD_comp_total<- master_table %>% filter(Category %in% c("FCWDdeath","MCWDdeath"))
ggplot(data = CWD_comp_total, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_smooth() +
  facet_grid(Category ~ Scenario)+
  labs(title = "CWD Death Count Total",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")

# PREVELANCE COMP
prev_comp<- PREV_DIFF %>% 
  filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"),Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("PREV"))
ggplot(data = prev_comp, mapping = aes(x = Month, y = Count, color = Category)) + 
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Category",values=c("#CC6677"), labels=c("Prevelance"))+
  labs(title = "Prevalence over time",
       y = "Prevalence (in %)", x = "Month") 

prev_comp_am<- PREV_DIFF_AM %>% filter(Scenario %in% c("SCEN8_SENS"),
                                       Category %in% c("TotalSprey","PREV")) %>% pivot_wider(.,names_from = "Category", values_from = "Count")
ggplot(data = prev_comp_am, mapping = aes(x = TotalSprey, y = PREV, color = Hunt_perc)) +
  geom_smooth(se=FALSE) +
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100% (-)","50% (-)","Base ","50% (+)","100% (+)"))+
  labs(title = "Prevalence over Population: Scenario 8",
       subtitle = "Adult Male Hunting",
       y = "Prevalence", x = "Susceptible Elk Population")

prev_comp_am<- PREV_DIFF_AF %>% filter(Scenario %in% c("SCEN8_SENS"),
                                       Category %in% c("TotalSprey","PREV")) %>% pivot_wider(.,names_from = "Category", values_from = "Count")
ggplot(data = prev_comp_am, mapping = aes(x = TotalSprey, y = PREV, color = Hunt_perc)) +
  geom_smooth(se=FALSE) +
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100% (-)","50% (-)","Base ","50% (+)","100% (+)"))+
  labs(title = "Prevalence over Population: Scenario 8",
       subtitle = "Adult Female Hunting",
       y = "Prevalence", x = "Susceptible Elk Population")



# HUNTING TABLES

hunt_comp2458<- master_table[which(master_table$Count>0),] %>% filter(Category %in% c("FpreyH","MpreyH"))
ggplot(data = hunt_comp2458, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_line() +
  facet_grid(Category~ Scenario, scales="free_x")+
  labs(title = "Harvested Elk Count by Age",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")

hunt_comp_total<- master_table1[which(master_table1$Count>0),] %>% filter(Scenario %in% c("SCEN2","SCEN4","SCEN6","SCEN8"),Category %in% c("TotalH"), Age %in% c("Total_pop"))
ggplot(data = hunt_comp_total, mapping = aes(x = Month, y = Count, color = Category)) +
  geom_line() +
  facet_grid(~Scenario, scales="free_x")+
  labs(title = "Harvested Elk Count Total",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")

hunt_comp_total<- master_table %>% filter(Category %in% c("TotalHI"))
ggplot(data = hunt_comp_total, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_smooth() +
  labs(title = "Harvested Elk Count Total",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")
  #facet_grid(Category ~ Scenario) 


# PREDATION TABLES

# ALL
pred_comp5678<- master_table %>% filter(Category %in% c("TotalPreddeath"))
ggplot(data = pred_comp5678, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_smooth() +
  facet_grid(Age~.)+
  labs(title = "Predation Death Count Total",
       subtitle = "Comparison of All Scenarios",
       y = "Elk Count", x = "Month")

# HEALTHY
pred_comp56<- master_table %>% filter(Scenario %in% c("SCEN5","SCEN6"),
                                      Category %in% c("FPreddeath","MPreddeath"))
ggplot(data = pred_comp56, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_smooth() +
  facet_grid(Category ~ Scenario) +
  labs(title = "Predation Death Count by Age and Sex",
       subtitle = "Comparison of Scenario 5 and 6",
       y = "Elk Count", x = "Month")

pred_comp56<- master_table1 %>% filter(Scenario %in% c("SCEN5","SCEN6","SCEN7","SCEN8"),Age %in% c("Total_pop"),Category %in% c("TotalPreddeath"))
ggplot(data = pred_comp56, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_smooth() +
  scale_color_manual("Category",values=c("#CC6677"), labels=c("Total Predation"))+
  facet_grid(~Scenario) +
  labs(title = "Predation Death Count Total",
       subtitle = "Comparison of all Scenarios",
       y = "Elk Count", x = "Month")

pred_comp56<- master_table1 %>% filter(Scenario %in% c("SCEN6","SCEN8"),Age %in% c("Total_pop"),Category %in% c("TotalPreddeath"))
ggplot(data = pred_comp56, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_smooth() +
  scale_color_manual("Category",values=c("#CC6677"), labels=c("Total Predation"))+
  facet_grid(~Scenario) +
  labs(title = "Predation Death Count Total",
       subtitle = "Comparison of Scenario 6 and 8",
       y = "Elk Count", x = "Month")

# INFECTED
pred_compS78<- master_table %>% filter(Scenario %in% c("SCEN7","SCEN8"),
                                      Category %in% c("SFPreddeath","SMPreddeath"))
ggplot(data = pred_compS78, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_smooth() +
  facet_grid(Category ~ Scenario) +
  labs(title = "Predation Death Count by Age and Sex (Susceptible)",
       subtitle = "Comparison of Scenario 7 and 8",
       y = "Elk Count", x = "Month")

pred_compI78<- master_table %>% filter(Scenario %in% c("SCEN7","SCEN8"),
                                      Category %in% c("IFPreddeath","IMPreddeath"))
ggplot(data = pred_compI78, mapping = aes(x = Month, y = Count, color = Age)) +
  geom_smooth() +
  facet_grid(Category ~ Scenario) +
  labs(title = "Predation Death Count by Age and Sex (Infected)",
       subtitle = "Comparison of Scenario 7 and 8",
       y = "Elk Count", x = "Month")


## SENSITIVITY COMPARISON GRAPHS

# ADULT MALE HUNTING 

# COMPARISON OF SCENARIOS FOR ONE AGE (ADULTS)
ad_male_pop_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"),
         Age %in% c("Adult"))
ggplot(data = ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  labs(title = "Adult Elk Population Sensitivity",
       subtitle = "ADULT MALE HUNTING",
       y = "Elk Count", x = "Month") 

# COMPARISON OF AGES FOR ONE SCENARIO (SCENARIO 4)
ad_male_pop_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS"),
         Category %in% c("SFpreyA","SMpreyA"))
ggplot(data = ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Category~Age)+
  labs(title = "Elk Count Sensitivity: Scenario 4",
       subtitle = "ADULT MALE HUNTING",
       y = "Elk Count", x = "Month") 

# LARGE COMPARISON OF BOTH?

# POPULATION COMPARISON
ad_male_pop_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),Age %in% c("Fawn","Juv","Adult"),Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+  
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Difference in Elk Count Sensitivity From Base", subtitle = "ADULT MALE HUNTING", y = "Difference in Elk Count", x = "Month")

ad_male_pop_comp<- POP_DIFF_AM %>%
filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),Age %in% c("Total_pop"), Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent)) + 
  geom_smooth()+
  facet_grid(~Scenario)+  
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Difference in Elk Count Sensitivity of Total Population From Base", subtitle = "ADULT MALE HUNTING", y = "Difference in Elk Count", x = "Month")

# CLose up on Juv due to small changes
ad_male_pop_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalpreyi","Totalpreyh"),
         Age %in% c("Juv")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent))+ geom_smooth()+
  facet_grid(~Scenario)+   
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  theme(legend.position = "none")+
  labs(title = "Difference in Elk Count Sensitivity From Base: Juveniles",y = "Difference in Elk Count", x = "Month")

# CWD COMPARISON
ad_male_cwd_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = ad_male_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "CWD death Sensitivity",
       subtitle = "ADULT MALE HUNTING",
       y = "Elk Count", x = "Month") 

ad_male_cwd_comp<- POP_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = ad_male_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "CWD death Sensitivity for Total Population",
       subtitle = "ADULT MALE HUNTING",
       y = "Elk Count", x = "Month") 

# PREVELANCE COMP
ad_male_prev_comp<- PREV_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("PREV"),
         Hunt_perc %in% c("S_0.1"))
ggplot(data = ad_male_prev_comp, mapping = aes(x = Month, y = Count, color = Hunt_perc)) + 
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Category",values=c("#CC6677"), labels=c("Prevelance"))+
  labs(title = "Prevalence over time",
       y = "Prevalence (in %)", x = "Month") 

ad_male_prev_comp<- PREV_DIFF_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"),
         Category %in% c("PREV"))
ggplot(data = ad_male_prev_comp, mapping = aes(x = Month, y = Count, color = Hunt_perc)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Prevalence Sensitivity of Total Population",
       subtitle = "ADULT MALE HUNTING",
       y = "Prevalence (in %)", x = "Month") 

# ELASTICITIES
ad_male_elast<- elasticity_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = ad_male_elast, mapping = aes(x = Month, y = ELASTICITY, color = Elasticity_Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Age~ Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Population Elasticity",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_male_elast<- elasticity_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"), 
         Age %in% c("Total_pop"),
         Category %in% c("TotalSprey","Totalpreyh"))
ggplot(data = ad_male_elast, mapping = aes(x = Month, y = ELASTICITY, color = Elasticity_Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Age~ Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Population Elasticity for all ages",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_male_prev_elast<- prev_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = ad_male_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Prevelance Elasticity for all ages",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_male_prev_elast<- prev_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"))
ggplot(data = ad_male_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Prevelance Elasticity for Total Population",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_male_cwd_elast<- cwd_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"))
ggplot(data = ad_male_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "CWD death Elasticity for all ages",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_male_cwd_elast<- cwd_AM %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"))
ggplot(data = ad_male_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "CWD death Elasticity for Total Population",
       subtitle = "ADULT MALE HUNTING",
       y = "Elasticity", x = "Month") 


# Close up on Juv due to small changes
ad_male_elast<- elasticity_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalpreyi","Totalpreyh"),
         Age %in% c("Juv"))
ggplot(data = ad_male_elast, mapping = aes(x = Month, y = ELASTICITY, color = Elasticity_Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Scenario~.)+   
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Populatin Elasticity: Juveniles", subtitle = "ADULT MALE HUNTING", y = "Elasticity", x = "Month")

# ADULT FEMALE HUNTING 

# LARGE COMPARISON OF BOTH?

# POPULATION COMPARISON

ad_female_pop_comp<- POP_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+  
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Difference in Elk Count Sensitivity From Base", subtitle = "ADULT FEMALE HUNTING", y = "Difference in Elk Count", x = "Month")

ad_female_pop_comp<- POP_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),Age %in% c("Total_pop"), Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent)) + 
  geom_smooth()+
  facet_grid(~Scenario)+  
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Difference in Elk Count Sensitivity of Total Population From Base", subtitle = "ADULT FEMALE HUNTING", y = "Difference in Elk Count", x = "Month")



# CLose up on Juv due to small changes
ad_female_pop_comp<- POP_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalpreyi","Totalpreyh"),
         Age %in% c("Juv")) %>% mutate(Hunt_Percent=Sensitivity_Level)
ggplot(data = ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Hunt_Percent))+ geom_smooth()+
  facet_grid(~Scenario)+   
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","5","10 (Base)","15","20"))+
  labs(title = "Difference in Elk Count Sensitivity From Base: Juveniles", subtitle = "ADULT MALE HUNTING", y = "Difference in Elk Count", x = "Month")

# CWD COMPARISON
ad_female_cwd_comp<- POP_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = ad_female_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "CWD death Sensitivity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elk Count", x = "Month") 

ad_female_cwd_comp<- POP_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = ad_female_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "CWD death Sensitivity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elk Count", x = "Month") 

# PREVELANCE COMPARISON
# 
ad_female_prev_comp<- PREV_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("PREV"))
ggplot(data = ad_female_prev_comp, mapping = aes(x = Month, y = Count, color = Hunt_perc)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Prevalence Sensitivity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Prevalence (in %)", x = "Month") 

ad_female_prev_comp<- PREV_DIFF_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"),
         Category %in% c("PREV"))
ggplot(data = ad_female_prev_comp, mapping = aes(x = Month, y = Count, color = Hunt_perc)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Prevalence Sensitivity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Prevalence (in %)", x = "Month") 


# ELASTICITIES
ad_female_elast<- elasticity_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("TotalSprey","Totalpreyh"))
ggplot(data = ad_female_elast, mapping = aes(x = Month, y = ELASTICITY, color = Elasticity_Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(Age~ Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Population Elasticity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_female_elast<- elasticity_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"),
         Category %in% c("TotalSprey","Totalpreyh"))
ggplot(data = ad_female_elast, mapping = aes(x = Month, y = ELASTICITY, color = Elasticity_Sensitivity_Level)) + 
  geom_smooth()+
  facet_grid(~ Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Population Elasticity",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_female_prev_elast<- prev_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = ad_female_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Prevelance Elasticity for all ages",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_female_prev_elast<- prev_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"))
ggplot(data = ad_female_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "Prevelance Elasticity for Total Population",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_female_cwd_elast<- cwd_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Fawn","Juv","Adult"))
ggplot(data = ad_female_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "CWD death Elasticity for all ages",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 

ad_female_cwd_elast<- cwd_AF %>% 
  filter(Scenario %in% c("SCEN2_SENS","SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Age %in% c("Total_pop"))
ggplot(data = ad_female_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0","2.5","5 (Base)","7.5","10"))+
  labs(title = "CWD death Elasticity for Total Population",
       subtitle = "ADULT FEMALE HUNTING",
       y = "Elasticity", x = "Month") 
# HISTORICAL HUNTING RATES

{

# ADULT MALE HUNTING 

# COMPARISON OF SCENARIOS FOR ONE AGE (ADULTS)
h_ad_male_pop_comp<- POP_DIFF_H_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"),
         Age %in% c("adult"))
ggplot(data = h_ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  labs(title = "Adult Elk Population Historical Sensitivity",
       subtitle = "ADULT MALE HUTNING",
       y = "Elk Count", x = "Month") 

# COMPARISON OF AGES FOR ONE SCENARIO (SCENARIO 4)
h_ad_male_pop_comp<- POP_DIFF_H_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS"),
         Category %in% c("Totalprey"))
ggplot(data = h_ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) + 
  geom_smooth()+
  facet_grid(~ Age)+
  labs(title = "Elk Count Historical Sensitivity: Scenario 4",
       subtitle = "ADULT MALE HUTNING",
       y = "Elk Count", x = "Month") 

# LARGE COMPARISON OF BOTH?
h_ad_male_pop_comp<- POP_DIFF_H_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"))
ggplot(data = h_ad_male_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) + 
  geom_smooth()+
  facet_grid(Scenario ~ Age)+
  labs(title = "Elk Count Historical Sensitivity",
       subtitle = "ADULT MALE HUTNING",
       y = "Elk Count", x = "Month") 

# ELASTICITIES
h_ad_male_elast<- elasticity_H_AM %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"),
         Age %in% c("adult"))
ggplot(data = h_ad_male_comp, mapping = aes(x = Month, y = ELASTICITY, color = LEVEL_DIFF)) + 
  geom_smooth()+
  facet_grid(~ Scenario)+
  labs(title = "Elk Count Historical Elasticity",
       subtitle = "ADULT MALE HUTNING",
       y = "Elk Count", x = "Month") 

# ADULT FEMALE HUNTING 

# COMPARISON OF SCENARIOS FOR ONE AGE (ADULTS)
h_ad_female_pop_comp<- POP_DIFF_H_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"),
         Age %in% c("adult"))
ggplot(data = h_ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) +
  geom_smooth()+
  facet_grid(~Scenario)+
  labs(title = "Adult Elk Population Historical Sensitivity",
       subtitle = "ADULT FEMALE HUTNING",
       y = "Elk Count", x = "Month") 

# COMPARISON OF AGES FOR ONE SCENARIO (SCENARIO 4)
h_ad_female_pop_comp<- POP_DIFF_H_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS"),
         Category %in% c("Totalprey"))
ggplot(data = h_ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) +
  geom_smooth()+
  facet_grid(~ Age)+
  labs(title = "Elk Count Historical Sensitivity: Scenario 4",
       subtitle = "ADULT FEMALE HUTNING",
       y = "Elk Count", x = "Month") 

# LARGE COMPARISON OF BOTH?
h_ad_female_pop_comp<- POP_DIFF_H_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"))
ggplot(data = h_ad_female_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = LEVEL_SENS)) + 
  geom_smooth()+
  facet_grid(Scenario ~ Age)+
  labs(title = "Elk Count Historical Sensitivity",
       subtitle = "ADULT FEMALE HUTNING",
       y = "Elk Count", x = "Month") 

# ELASTICITIES
h_ad_female_elast<- elasticity_H_AF %>% 
  filter(Scenario %in% c("SCEN4_SENS","SCEN6_SENS","SCEN8_SENS"),
         Category %in% c("Totalprey"),
         Age %in% c("adult"))
ggplot(data = h_ad_female_elast, mapping = aes(x = Month, y = ELASTICITY, color = LEVEL_DIFF)) + 
  geom_smooth()+
  facet_grid(~ Scenario)+
  labs(title = "Elk Count Historical Elasticity",
       subtitle = "ADULT FEMALE HUTNING",
       y = "Elk Count", x = "Month") 

  }


# ENVIRONEMENTAL TRANSMISSION

pop_comp_env<- env_master_table %>% filter(Category %in% c("Totalpreyi"))
ggplot(data = pop_comp_env, mapping = aes(x = Month, y = Count, color = ENV_perc)) +
  geom_line()+
  facet_grid(Age~Scenario)+
  #geom_smooth() +
  #facet_grid(Scenario ~ Category) +
  labs(title = "Elk Population Count",
       subtitle = "Comparison of Environmental Transmission (%)",
       y = "Elk Count", x = "Month")

## PREDATION SENSITIVITY (Kill_rate)

# POPULATION COMPARISON
pred_pop_comp<- POP_DIFF_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),Age %in% c("Fawn","Juv","Adult"),Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Kill_rate=Predation_Level)
ggplot(data = pred_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Kill_rate)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+  
  scale_color_manual("Kill_rate",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c(".2",".5",".8","1 (Base)","1.2"))+
  labs(title = "Difference in Elk Count Sensitivity From Base", subtitle = "Predation", y = "Difference in Elk Count", x = "Month")

# CWD COMPARISON
pred_cwd_comp<- POP_DIFF_PRED %>% 
  filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = pred_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Predation_Level)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "CWD death Sensitivity",
       subtitle = "Predation",
       y = "Elk Count", x = "Month") 

pred_cwd_comp<- POP_DIFF_PRED %>% 
  filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Total_pop"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = pred_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Predation_Level)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c(".2",".5",".8","1 (Base)","1.2"))+
  labs(title = "CWD death Sensitivity",
       subtitle = "Predation",
       y = "Elk Count", x = "Month") 

# PREVELANCE COMP
pred_prev_comp<- PREV_DIFF_PRED %>% 
  filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("PREV"))
ggplot(data = pred_prev_comp, mapping = aes(x = Month, y = Count, color = Kill_rate)) + 
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c(".2",".5",".8","1 (Base)","1.2"))+
  labs(title = "Prevalence Sensitivity",
       subtitle = "Predation",
       y = "Prevalence (in %)", x = "Month") 

pred_prev_comp<- PREV_DIFF_PRED %>% 
  filter(Scenario %in% c("SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Total_pop"),
         Category %in% c("PREV"))
ggplot(data = pred_prev_comp, mapping = aes(x = Month, y = Count, color = Kill_rate)) + 
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c(".2",".5",".8","1 (Base)","1.2"))+
  labs(title = "Prevalence Sensitivity of Total Population",
       subtitle = "Predation",
       y = "Prevalence (in %)", x = "Month") 

# ELASTICITIES

#population
pred_elast<- elasticity_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = pred_elast, mapping = aes(x = Month, y = Predation, color = Elasticity_Predation_Level)) + 
  geom_smooth()+
  facet_grid(Age~ Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "Population Elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

pred_elast<- elasticity_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Total_pop"),
         Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = pred_elast, mapping = aes(x = Month, y = Predation, color = Elasticity_Predation_Level)) + 
  geom_smooth()+
  facet_grid(~ Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "Population Elasticity for Total Population",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

#prevalence
pred_prev_elast<- prev_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = pred_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "Prevalence Elasticity for all ages",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

pred_prev_elast<- prev_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Total_pop")) 
ggplot(data = pred_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "Prevalence Elasticity for Total Population",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

# SEE prev_test TO UNDERSTAND THE CRAZY ASS ELASTICITIES

#cwd deaths
pred_cwd_elast<- cwd_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = pred_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "CWD death elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

pred_cwd_elast<- cwd_PRED %>% 
  filter(Scenario %in% c("SCEN5_PRED","SCEN6_PRED","SCEN7_PRED","SCEN8_PRED"),
         Age %in% c("Total_pop")) 
ggplot(data = pred_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Kill_Rate (Elk/Wolf)",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("0.2","0.5","0.8","1 (Base)","1.2"))+
  labs(title = "CWD death elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

## PREDATION SENSITIVITY (Max Predators)

# POPULATION COMPARISON
predmax_pop_comp<- POP_DIFF_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("Totalpreyi","Totalpreyh")) %>% mutate(Max_Pred=Predation_Level)
ggplot(data = predmax_pop_comp, mapping = aes(x = Month, y = POP_DIFF, color = Max_Pred)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+  
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Difference in Elk Count Sensitivity From Base", subtitle = "ADULT MALE HUNTING", y = "Difference in Elk Count", x = "Month")

# CWD COMPARISON
predMax_cwd_comp<- POP_DIFF_PREDMax %>% 
  filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = predMax_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Predation_Level)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "CWD death Sensitivity for all ages",
       subtitle = "Predation",
       y = "Elk Count", x = "Month") 

predmax_cwd_comp<- POP_DIFF_PREDMax %>% 
  filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Total_pop"),
         Category %in% c("TotalCWDdeath"))
ggplot(data = predmax_cwd_comp, mapping = aes(x = Month, y = POP_DIFF, color = Predation_Level)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "CWD death Sensitivity for Total Population",
       subtitle = "Predation",
       y = "Elk Count", x = "Month") 

# PREVELANCE COMP
predmax_prev_comp<- PREV_DIFF_PREDMax %>% 
  filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("PREV"))
ggplot(data = predmax_prev_comp, mapping = aes(x = Month, y = Count, color = Max_Pred)) + 
  geom_line()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Prevalence Sensitivity for all ages",
       subtitle = "Predation",
       y = "Prevalence (in %)", x = "Month") 

predmax_prev_comp<- PREV_DIFF_PREDMax %>% 
  filter(Scenario %in% c("SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Total_pop"),
         Category %in% c("PREV"))
ggplot(data = predmax_prev_comp, mapping = aes(x = Month, y = Count, color = Max_Pred)) + 
  geom_line()+
  facet_grid(~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Prevalence Sensitivity of Total Population",
       subtitle = "Predation",
       y = "Prevalence (in %)", x = "Month") 

# ELASTICITIES

#population
predmax_elast<- elasticity_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Fawn","Juv","Adult"),
         Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = predmax_elast, mapping = aes(x = Month, y = Predation, color = Elasticity_Predation_Level)) + 
  geom_smooth()+
  facet_grid(Age~ Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Population Elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

predmax_elast<- elasticity_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Total_pop"),
         Category %in% c("Totalpreyi","Totalpreyh"))
ggplot(data = predmax_elast, mapping = aes(x = Month, y = Predation, color = Elasticity_Predation_Level)) + 
  geom_smooth()+
  facet_grid(~ Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Population Elasticity for Total Population",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

#prevalence
predmax_prev_elast<- prev_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = predmax_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Prevalence Elasticity for all ages",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

predmax_prev_elast<- prev_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Total_pop")) 
ggplot(data = predmax_prev_elast, mapping = aes(x = Month, y = PREV_Elast, color = Prevalence)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "Prevalence Elasticity for Total Population",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

#cwd deaths
predmax_cwd_elast<- cwd_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Fawn","Juv","Adult")) 
ggplot(data = predmax_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(Age~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "CWD death elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 

predmax_cwd_elast<- cwd_PREDMax %>% 
  filter(Scenario %in% c("SCEN5_PREDMax","SCEN6_PREDMax","SCEN7_PREDMax","SCEN8_PREDMax"),
         Age %in% c("Total_pop")) 
ggplot(data = predmax_cwd_elast, mapping = aes(x = Month, y = CWD_Elast, color = CWD)) + 
  geom_smooth()+
  facet_grid(~Scenario)+
  scale_color_manual("Max # of Predators",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100","120","150 (Base)","180","200"))+
  labs(title = "CWD death elasticity",
       subtitle = "Predation",
       y = "Elasticity", x = "Month") 




