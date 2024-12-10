
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
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/CWD_SENS_ANALYSIS.R")


# STOCK AND PRICE RELATIONSHIP
pricecsv<-read.csv("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/cody_elk_prices.csv")
plot(acc.price1 ~ stock, data = pricecsv, xlab="Stock", ylab="Price for Additional Elk")


# CREATING VALUES FOR COST , MEAT, AND NONMARKET

# add age classes into total pop= fawn+juv+adult
master_table1<- master_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(master_table))){
  Total_pop = master_table1$Fawn+master_table1$Juv+master_table1$Adult
}
master_table1<- add_column(master_table1,Total_pop,.after = "Adult")
master_table1<-master_table1 %>% pivot_longer(. , cols = !c("Scenario","Run","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

master_table1<-master_table1%>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","Run","Age","Month","Year"),names_to = "Category", values_to= "Count")

master_econ_table<- master_table1 %>%  filter(Age %in% c("Total_pop"))%>%  pivot_wider(names_from = Category, values_from = Count) %>% replace(is.na(.), 0)
master_econ_table<- master_econ_table %>% 
  mutate(MEAT_VALUE = 5080*TotalH,
         NM_PRICEi = case_when(TotalSprey<1000 ~ 25000,
                  (TotalSprey<2000 & TotalSprey>1000) ~ 12500,
                  (TotalSprey<4000 &TotalSprey>2000) ~ 5000,
                  (TotalSprey<10000 &TotalSprey>4000) ~ 2500,
                  (TotalSprey<20000 &TotalSprey>10000) ~ 500,
                  TotalSprey>20000 ~ 100),
         NM_VALUEi = case_when(TotalSprey<1000 ~ 25000*TotalSprey,
                        (TotalSprey<2000 & TotalSprey>1000) ~ 12500*TotalSprey,
                        (TotalSprey<4000 & TotalSprey>2000) ~ 5000*TotalSprey,
                        (TotalSprey<10000 & TotalSprey>4000) ~ 2500*TotalSprey,
                        (TotalSprey<20000 & TotalSprey>10000) ~ 500*TotalSprey,
                         TotalSprey>20000 ~ 100*TotalSprey),
         NM_PRICEh = case_when(Totalpreyh<1000 ~ 25000,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500,
                               (Totalpreyh<4000 &Totalpreyh>2000) ~ 5000,
                               (Totalpreyh<10000 &Totalpreyh>4000) ~ 2500,
                               (Totalpreyh<20000 &Totalpreyh>10000) ~ 500,
                               Totalpreyh>20000 ~ 100),
         NM_VALUEh = case_when(Totalpreyh<1000 ~ 25000*Totalpreyh,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500*Totalpreyh,
                               (Totalpreyh<4000 & Totalpreyh>2000) ~ 5000*Totalpreyh,
                               (Totalpreyh<10000 & Totalpreyh>4000) ~ 2500*Totalpreyh,
                               (Totalpreyh<20000 & Totalpreyh>10000) ~ 500*Totalpreyh,
                         Totalpreyh>20000 ~ 100*Totalpreyh))

master_econ_table<-master_econ_table %>% pivot_longer(. , cols = !c("Scenario","Run","Age","Month","Year"),names_to = "Category", values_to= "Count") %>% replace(is.na(.), 0)
master_econ_table<- filter(master_econ_table, Count > 0)

# NET BENEFIT AND HUNTING TABLE

# ADULTS NB (FAWN AND JUV *****NOT****** INCLUDED)
netbenefit_table<-master_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,Month,Year,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,TotalCWDdeath,TotalPreddeath,PREV) %>% replace(is.na(.), 0)

mod<-vector()
mod[netbenefit_table$Month%%12 == 7] <- 1 
mod[netbenefit_table$Month%%12 == 8] <- 2 
netbenefit_table<-netbenefit_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    NBPVi = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi+MEAT_VALUE))),
    NBPVh = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh+MEAT_VALUE)))
  ) %>% replace(is.na(.), 0)
netbenefit_table <- filter(netbenefit_table, mod > 0) 
stargazer(t(netbenefit_table),summary = FALSE,type = "latex", digits=1, out = "NB_scen_all.tex")


# individual yearly tables 
netbenefit_scen<-netbenefit_table %>% filter(Scenario %in% c("SCEN1"), mod %in% c(2)) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen1,summary = FALSE,type = "latex",digits=0, out = "NB_scen1.tex")

netbenefit_scen2<-netbenefit_table %>% filter(Scenario %in% c("SCEN2")) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,TotalH, MEAT_VALUE, NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen2,summary = FALSE,type = "latex", digits=0, out = "NB_scen2.tex")

netbenefit_scen3<-netbenefit_table %>% filter(Scenario %in% c("SCEN3"), mod %in% c(2)) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen3,summary = FALSE,type = "latex", digits=0, out = "NB_scen3.tex")

netbenefit_scen4<-netbenefit_table %>% filter(Scenario %in% c("SCEN4")) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,TotalH, MEAT_VALUE,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4.tex")

netbenefit_scen5<-netbenefit_table %>% filter(Scenario %in% c("SCEN5"), mod %in% c(2)) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen5,summary = FALSE,type = "latex", digits=0, out = "NB_scen5.tex")

netbenefit_scen6<-netbenefit_table %>% filter(Scenario %in% c("SCEN6")) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,TotalH, MEAT_VALUE,NBPVh) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6.tex")

netbenefit_scen7<-netbenefit_table %>% filter(Scenario %in% c("SCEN7"), mod %in% c(2)) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi) %>% mutate_if(is.numeric, round)
#stargazer(netbenefit_scen7,summary = FALSE,type = "latex", digits=0, out = "NB_scen7.tex")

netbenefit_scen8<-netbenefit_table %>% filter(Scenario %in% c("SCEN8")) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,TotalH, MEAT_VALUE,NBPVi) %>% mutate_if(is.numeric, round)
# stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8.tex")

# individual yearly tables 
control<-netbenefit_table  %>% select(Scenario, Month, Year, TotalSprey,Totalpreyh,TotalCWDdeath,TotalH,TotalPreddeath) %>% mutate_if(is.numeric, round)

#writing to an excel file
list_of_datasets <- list("Scen1" = netbenefit_scen, "Scen2" = netbenefit_scen2,"Scen3" = netbenefit_scen3,"Scen4" = netbenefit_scen4,"Scen5" = netbenefit_scen5,"Scen6" = netbenefit_scen6,"Scen7" = netbenefit_scen7,"Scen8" = netbenefit_scen8)
write.xlsx(list_of_datasets, file = "./scenario_all.xlsx")

write.xlsx(control, file = "./scenario_control_totalpop1.xlsx")


# infected table
netbenefit_sceni<-netbenefit_table %>% filter(Scenario %in% c("SCEN1","SCEN2","SCEN5","SCEN6")) %>% select(Scenario, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh,NBPVh)

# healthy table
netbenefit_scenh<-netbenefit_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8")) %>% select(Scenario, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi,NBPVi)

# GRAPHING NET BENEFIT

# against total prey
NB_econ_table<-netbenefit_table %>% pivot_longer(. , cols = !c("Scenario","Age","Month","mod","Year"),names_to = "Category", values_to= "Count") 

NB_econ<-NB_econ_table %>% filter(Category %in% c("NBPVi","NBPVh"))  %>% pivot_wider(names_from = Scenario, values_from=Count)

NB1<-sum(NB_econ$SCEN1)
NB2<-sum(NB_econ$SCEN2)
NB3<-sum(NB_econ$SCEN3)
NB4<-sum(NB_econ$SCEN4)
NB5<-sum(NB_econ$SCEN5)
NB6<-sum(NB_econ$SCEN6)
NB7<-sum(NB_econ$SCEN7)
NB8<-sum(NB_econ$SCEN8)
NB_all_table<-data.frame(NB1,NB2,NB3,NB4,NB5,NB6,NB7,NB8)
stargazer(t(NB_all_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_all.tex")
list_of_datasets <- list("Scen1" = netbenefit_scen, "Scen2" = netbenefit_scen2,"Scen3" = netbenefit_scen3,"Scen4" = netbenefit_scen4,"Scen5" = netbenefit_scen5,"Scen6" = netbenefit_scen6,"Scen7" = netbenefit_scen7,"Scen8" = netbenefit_scen8, "Sum of each Scenario"= NB_all_table)
write.xlsx(list_of_datasets, file = "./scenario_nb_totalpop1.xlsx")

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

# against CWD death

NB_table<- master_econ_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"))
ggplot(data = NB_table, mapping = aes(x = TotalCWDdeath, y = NBPVi, color = Scenario)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "Present Value Net Benefits through Time: Infection Present",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit", x = "CWD death count")

# against susceptible pop

NB_table<- master_econ_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"))
ggplot(data = NB_table, mapping = aes(x = Totalpreyh, y = NBPVi, color = Scenario)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "Present Value Net Benefits through Time: Infection Present",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit", x = "Susceptible Prey Population level")

# against nat death

NB_table<- master_econ_table %>% filter(Scenario %in% c("SCEN3","SCEN4","SCEN7","SCEN8"))
ggplot(data = NB_table, mapping = aes(x = TotalD, y = NBPVi, color = Scenario)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "Present Value Net Benefits through Time: Infection Present",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Net Benefit", x = "Total Natural Death")

NBPV_table<- master_econ_table %>% filter(Category %in% c("NBPVi","NBPVh"))
ggplot(data = NBPV_table, mapping = aes(x = Month, y = Count, color = Scenario)) +
  geom_smooth(se=FALSE) +
  #facet_grid(~Category) +
  labs(title = "Present Value Net Benefits",
       subtitle = "Meat Value, Non Market Value, and Cost of Management",
       y = "Dollars", x = "Month")



# ADULT MALE HUNTING NET BENEFIT 

AD_MALE_table1<-AD_MALE_table1%>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","Age","char","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count")

sens_econ_table<-AD_MALE_table1 %>%  filter(Age %in% c("Total_pop")) %>% pivot_wider(names_from = Category, values_from = Count)%>% 
  mutate(MEAT_VALUE = 5080*TotalH,
         NM_PRICEi = case_when(TotalSprey<1000 ~ 25000,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500,
                               (TotalSprey<4000 &TotalSprey>2000) ~ 5000,
                               (TotalSprey<10000 &TotalSprey>4000) ~ 2500,
                               (TotalSprey<20000 &TotalSprey>10000) ~ 500,
                               TotalSprey>20000 ~ 100),
         NM_VALUEi = case_when(TotalSprey<1000 ~ 25000*TotalSprey,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500*TotalSprey,
                               (TotalSprey<4000 & TotalSprey>2000) ~ 5000*TotalSprey,
                               (TotalSprey<10000 & TotalSprey>4000) ~ 2500*TotalSprey,
                               (TotalSprey<20000 & TotalSprey>10000) ~ 500*TotalSprey,
                               TotalSprey>20000 ~ 100*TotalSprey),
         NM_PRICEh = case_when(Totalpreyh<1000 ~ 25000,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500,
                               (Totalpreyh<4000 &Totalpreyh>2000) ~ 5000,
                               (Totalpreyh<10000 &Totalpreyh>4000) ~ 2500,
                               (Totalpreyh<20000 &Totalpreyh>10000) ~ 500,
                               Totalpreyh>20000 ~ 100),
         NM_VALUEh = case_when(Totalpreyh<1000 ~ 25000*Totalpreyh,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500*Totalpreyh,
                               (Totalpreyh<4000 & Totalpreyh>2000) ~ 5000*Totalpreyh,
                               (Totalpreyh<10000 & Totalpreyh>4000) ~ 2500*Totalpreyh,
                               (Totalpreyh<20000 & Totalpreyh>10000) ~ 500*Totalpreyh,
                               Totalpreyh>20000 ~ 100*Totalpreyh))

sens_econ_table<-sens_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count") %>% replace(is.na(.), 0)
sens_econ_table<- filter(sens_econ_table, Count > 0)

# NET BENEFIT AND HUNTING TABLE

netbenefitAM_table<-sens_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,Hunt_perc,Month,Year,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,PREV) %>% replace(is.na(.), 0)

mod<-vector()
mod[netbenefitAM_table$Month%%12 == 7] <- 1 
mod[netbenefitAM_table$Month%%12 == 8] <- 2 
netbenefitAM_table<-netbenefitAM_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    NBPVi = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi+MEAT_VALUE))),
    NBPVh = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh+MEAT_VALUE)))
  ) %>% replace(is.na(.), 0)
netbenefitAM_table <-  filter(netbenefitAM_table ,mod > 0) 

# individual yearly tables 
{
# SCEN 4
netbenefitAM_scen4<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_0.tex")
netbenefitAM_scen4<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_05.tex")
netbenefitAM_scen4<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_1.tex")
netbenefitAM_scen4<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.15")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_15.tex")
netbenefitAM_scen4<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.2")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_2.tex")

# SCEN 6
netbenefitAM_scen6<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEi,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_0.tex")
netbenefitAM_scen6<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_05.tex")
netbenefitAM_scen6<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_1.tex")
netbenefitAM_scen6<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.15")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_15.tex")
netbenefitAM_scen6<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.2")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_2.tex")

# SCEN 8
netbenefitAM_scen8<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_0.tex")
netbenefitAM_scen8<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_05.tex")
netbenefitAM_scen8<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_1.tex")
netbenefitAM_scen8<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.15")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_15.tex")
netbenefitAM_scen8<-netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.2")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_2.tex")
}

# scenario 2 
NB_econ_table<-netbenefitAM_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count") 

NB2_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN2_SENS"), Category %in% c("NBPVi","NBPVh")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB2_0<-sum(NB2_sens_econ$S_0)
NB2_05<-sum(NB2_sens_econ$S_0.05)
NB2_1<-sum(NB2_sens_econ$S_0.1)
NB2_15<-sum(NB2_sens_econ$S_0.15)
NB2_2<-sum(NB2_sens_econ$S_0.2)
NB2_table<-data.frame(NB2_0,NB2_05,NB2_1,NB2_15,NB2_2)

# scenario 4 
NB_econ_table<-netbenefitAM_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count") 


NB_table<- netbenefitAM_table %>% filter(Scenario %in% c("SCEN8_SENS"))
ggplot(data = NB_table, mapping = aes(x = TotalSprey, y = NM_VALUEi, color = Hunt_perc)) +
  geom_smooth(se=FALSE) +  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288"), labels=c("100% (-)","50% (-)","Base","50% (+)","100% (+)"))+
  labs(title = "Net Benefit over Population: Scenario 8",
       subtitle = "Adult Male Hunting: Harvest Value, Non Market Value, net of Management Costs",
       y = "Net Benefit", x = "Susceptible Elk Population")

NB4_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN4_SENS"), Category %in% c("NBPVi","NBPVh")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB4_0<-sum(NB4_sens_econ$S_0)
NB4_05<-sum(NB4_sens_econ$S_0.05)
NB4_1<-sum(NB4_sens_econ$S_0.1)
NB4_15<-sum(NB4_sens_econ$S_0.15)
NB4_2<-sum(NB4_sens_econ$S_0.2)
NB4_table<-data.frame(NB4_0,NB4_05,NB4_1,NB4_15,NB4_2)

# scenario 6

NB6_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN6_SENS"), Category %in% c("NBPVh")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB6_0<-sum(NB6_sens_econ$S_0)
NB6_05<-sum(NB6_sens_econ$S_0.05)
NB6_1<-sum(NB6_sens_econ$S_0.1)
NB6_15<-sum(NB6_sens_econ$S_0.15)
NB6_2<-sum(NB6_sens_econ$S_0.2)
NB6_table<-data.frame(NB6_0,NB6_05,NB6_1,NB6_15,NB6_2)

# scenario 8

NB8_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN8_SENS"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB8_0<-sum(NB8_sens_econ$S_0)
NB8_05<-sum(NB8_sens_econ$S_0.05)
NB8_1<-sum(NB8_sens_econ$S_0.1)
NB8_15<-sum(NB8_sens_econ$S_0.15)
NB8_2<-sum(NB8_sens_econ$S_0.2)
NB8_table<-data.frame(NB8_0,NB8_05,NB8_1,NB8_15,NB8_2)


NB_sens_table<-data.frame(NB2_table,NB4_table,NB6_table,NB8_table)
stargazer(t(NB_sens_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_sens.tex")


#Elasticity 

# elasticities of nb to hunting perc
huntperc<-c(0,0.05,0.1,0.15,0.2)
NB2_elast1 <-((NB2_table[1]-NB2_table[3])/NB2_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB2_elast2 <-((NB2_table[2]-NB2_table[3])/NB2_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB2_elast3 <-((NB2_table[3]-NB2_table[3])/NB2_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB2_elast4 <-((NB2_table[4]-NB2_table[3])/NB2_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB2_elast5 <-((NB2_table[5]-NB2_table[3])/NB2_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB2_list <- data.frame(NB2_elast1 ,NB2_elast2,NB2_elast3,NB2_elast4,NB2_elast5)

NB4_elast1 <-((NB4_table[1]-NB4_table[3])/NB4_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB4_elast2 <-((NB4_table[2]-NB4_table[3])/NB4_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB4_elast3 <-((NB4_table[3]-NB4_table[3])/NB4_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB4_elast4 <-((NB4_table[4]-NB4_table[3])/NB4_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB4_elast5 <-((NB4_table[5]-NB4_table[3])/NB4_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB4_list <- data.frame(NB4_elast1 ,NB4_elast2,NB4_elast3,NB4_elast4,NB4_elast5)

NB6_elast1 <-((NB6_table[1]-NB6_table[3])/NB6_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB6_elast2 <-((NB6_table[2]-NB6_table[3])/NB6_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB6_elast3 <-((NB6_table[3]-NB6_table[3])/NB6_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB6_elast4 <-((NB6_table[4]-NB6_table[3])/NB6_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB6_elast5 <-((NB6_table[5]-NB6_table[3])/NB6_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB6_list <- data.frame(NB6_elast1 ,NB6_elast2,NB6_elast3,NB6_elast4,NB6_elast5)

NB8_elast1 <-((NB8_table[1]-NB8_table[3])/NB8_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB8_elast2 <-((NB8_table[2]-NB8_table[3])/NB8_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB8_elast3 <-((NB8_table[3]-NB8_table[3])/NB8_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB8_elast4 <-((NB8_table[4]-NB8_table[3])/NB8_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB8_elast5 <-((NB8_table[5]-NB8_table[3])/NB8_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB8_list <- data.frame(NB8_elast1 ,NB8_elast2,NB8_elast3,NB8_elast4,NB8_elast5)

NB_elast_table_M<-data.frame(NB2_list,NB4_list,NB6_list,NB8_list)

stargazer(t(NB_elast_table_M),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_M.tex")
write.xlsx(NB_elast_table_M, file = "./scenario_nb_elast_M.xlsx")

# ADULT FEMALE HUNTING SENS NET BENEFIT 

AD_FEMALE_table1<- AD_FEMALE_table  %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(AD_FEMALE_table1))){
  Total_pop = AD_FEMALE_table1$Fawn+AD_FEMALE_table1$Juv+AD_FEMALE_table1$Adult
}
AD_FEMALE_table1<- add_column(AD_FEMALE_table1,Total_pop,.after = "Adult")
AD_FEMALE_table1<-AD_FEMALE_table1 %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)
AD_FEMALE_table1<-AD_FEMALE_table1%>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(PREV = TotalIprey/Totalpreyi) %>% 
  pivot_longer(., cols = !c("Scenario","Age","char","Hunt_perc","Month","Year"),names_to = "Category", values_to= "Count")

sens_table<-AD_FEMALE_table1 %>% filter(Age %in% c("Total_pop")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = sens_table, mapping = aes(x = Month, y = TotalSprey, color = Hunt_perc)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "population changes with hunting changes",
       y = "total prey", x = "Month")

sens_econ_table<-AD_FEMALE_table1  %>% filter(Age %in% c("Total_pop")) %>% pivot_wider(names_from = Category, values_from = Count) %>% 
  mutate(MEAT_VALUE = 5080*TotalH,
         NM_PRICEi = case_when(TotalSprey<1000 ~ 25000,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500,
                               (TotalSprey<4000 &TotalSprey>2000) ~ 5000,
                               (TotalSprey<10000 &TotalSprey>4000) ~ 2500,
                               (TotalSprey<20000 &TotalSprey>10000) ~ 500,
                               TotalSprey>20000 ~ 100),
         NM_VALUEi = case_when(TotalSprey<1000 ~ 25000*TotalSprey,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500*TotalSprey,
                               (TotalSprey<4000 & TotalSprey>2000) ~ 5000*TotalSprey,
                               (TotalSprey<10000 & TotalSprey>4000) ~ 2500*TotalSprey,
                               (TotalSprey<20000 & TotalSprey>10000) ~ 500*TotalSprey,
                               TotalSprey>20000 ~ 100*TotalSprey),
         NM_PRICEh = case_when(Totalpreyh<1000 ~ 25000,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500,
                               (Totalpreyh<4000 &Totalpreyh>2000) ~ 5000,
                               (Totalpreyh<10000 &Totalpreyh>4000) ~ 2500,
                               (Totalpreyh<20000 &Totalpreyh>10000) ~ 500,
                               Totalpreyh>20000 ~ 100),
         NM_VALUEh = case_when(Totalpreyh<1000 ~ 25000*Totalpreyh,
                               (Totalpreyh<2000 & Totalpreyh>1000) ~ 12500*Totalpreyh,
                               (Totalpreyh<4000 & Totalpreyh>2000) ~ 5000*Totalpreyh,
                               (Totalpreyh<10000 & Totalpreyh>4000) ~ 2500*Totalpreyh,
                               (Totalpreyh<20000 & Totalpreyh>10000) ~ 500*Totalpreyh,
                               Totalpreyh>20000 ~ 100*Totalpreyh))

sens_econ_table<-sens_econ_table %>% pivot_longer(. , cols = !c("Scenario","char","Hunt_perc","Age","Month","Year"),names_to = "Category", values_to= "Count") %>% replace(is.na(.), 0)
sens_econ_table<- filter(sens_econ_table, Count > 0)


# NET BENEFIT AND HUNTING TABLE

netbenefitAF_table<-sens_econ_table %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,Hunt_perc,Month,Year,TotalSprey,NM_PRICEi,NM_VALUEi,Totalpreyh,NM_PRICEh,NM_VALUEh,TotalH,MEAT_VALUE,PREV) %>% replace(is.na(.), 0)

mod<-vector()
mod[netbenefitAF_table$Month%%12 == 7] <- 1 
mod[netbenefitAF_table$Month%%12 == 8] <- 2 
netbenefitAF_table<-netbenefitAF_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year = as.integer(Year)-1,
    NBPVi = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi+MEAT_VALUE))),
    NBPVh = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEh+MEAT_VALUE)))
  ) %>% replace(is.na(.), 0)
netbenefitAF_table <- filter(netbenefitAF_table, mod > 0) 

# individual yearly tables 
{
# SCEN 4
netbenefitAF_scen4<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_0.tex")
netbenefitAF_scen4<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.025")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_025.tex")
netbenefitAF_scen4<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_05.tex")
netbenefitAF_scen4<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.075")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_075.tex")
netbenefitAF_scen4<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN4_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen4,summary = FALSE,type = "latex", digits=0, out = "NB_scen4_1.tex")

# SCEN 6
netbenefitAF_scen6<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_0.tex")
netbenefitAF_scen6<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.025")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_025.tex")
netbenefitAF_scen6<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_05.tex")
netbenefitAF_scen6<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.075")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_075.tex")
netbenefitAF_scen6<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN6_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, Totalpreyh, NM_PRICEh,NM_VALUEh, TotalH, MEAT_VALUE,NBPVh)
#stargazer(netbenefit_scen6,summary = FALSE,type = "latex", digits=0, out = "NB_scen6_1.tex")

# SCEN 8
netbenefitAF_scen8<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_0.tex")
netbenefitAF_scen8<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.025")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_025.tex")
netbenefitAF_scen8<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.05")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_05.tex")
netbenefitAF_scen8<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.075")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_075.tex")
netbenefitAF_scen8<-netbenefitAF_table %>% filter(Scenario %in% c("SCEN8_SENS"),
                                                  Hunt_perc %in% c("S_0.1")) %>% select(Scenario, Hunt_perc, Month, Year, TotalSprey, NM_PRICEi,NM_VALUEi, TotalH, MEAT_VALUE,NBPVi)
#stargazer(netbenefit_scen8,summary = FALSE,type = "latex", digits=0, out = "NB_scen8_1.tex")
}

# scenario 2

NB_table<- netbenefitAF_table %>% mutate(hunt_case="AD_Female_Hunting") %>% filter(Scenario %in% c("SCEN8_SENS"))

NB_table2<- netbenefitAM_table %>% mutate(hunt_case="AD_Male_Hunting") %>% filter(Scenario %in% c("SCEN8_SENS"))

NB_big<-rbind(NB_table,NB_table2)
ggplot(data = NB_big, mapping = aes(x = TotalSprey, y = NM_VALUEi, color = Hunt_perc)) +
  geom_smooth(se=FALSE) + 
  facet_grid(hunt_case~.)+
  scale_color_manual("Hunt_Percent",values=c("#88CCEE", "#CC6677", "#E69F00", "#117733", "#332288","#888888","#661100"), labels=c("0","2.5","5 ","7.5","10","15","20"))+
  labs(title = "Net Benefit over Population: Scenario 8 ",
       subtitle = "Harvest Value, Non Market Value, net of Management Costs", y = "Net Benefit", x = "Susceptible Elk Population")

NB_econ_table<-netbenefitAF_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","mod","Year"),names_to = "Category", values_to= "Count") 

NB2_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN2_SENS"), Category %in% c("NBPVh")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB2_0<-sum(NB2_sens_econ$S_0)
NB2_025<-sum(NB2_sens_econ$S_0.025)
NB2_05<-sum(NB2_sens_econ$S_0.05)
NB2_075<-sum(NB2_sens_econ$S_0.075)
NB2_1<-sum(NB2_sens_econ$S_0.1)
NB2_table<-data.frame(NB2_0,NB2_025,NB2_05,NB2_075,NB2_1)

# scenario 4 
NB_econ_table<-netbenefitAF_table %>% pivot_longer(. , cols = !c("Scenario","Age","Hunt_perc","Month","mod","Year"),names_to = "Category", values_to= "Count") 

NB4_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN4_SENS"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB4_0<-sum(NB4_sens_econ$S_0)
NB4_025<-sum(NB4_sens_econ$S_0.025)
NB4_05<-sum(NB4_sens_econ$S_0.05)
NB4_075<-sum(NB4_sens_econ$S_0.075)
NB4_1<-sum(NB4_sens_econ$S_0.1)
NB4_table<-data.frame(NB4_0,NB4_025,NB4_05,NB4_075,NB4_1)

# scenario 6
NB6_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN6_SENS"), Category %in% c("NBPVh")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB6_0<-sum(NB6_sens_econ$S_0)
NB6_025<-sum(NB6_sens_econ$S_0.025)
NB6_05<-sum(NB6_sens_econ$S_0.05)
NB6_075<-sum(NB6_sens_econ$S_0.075)
NB6_1<-sum(NB6_sens_econ$S_0.1)
NB6_table<-data.frame(NB6_0,NB6_025,NB6_05,NB6_075,NB6_1)

# scenario 8
NB8_sens_econ<-NB_econ_table %>% filter(Scenario %in% c("SCEN8_SENS"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = Hunt_perc, values_from=Count) 

NB8_0<-sum(NB8_sens_econ$S_0)
NB8_025<-sum(NB8_sens_econ$S_0.025)
NB8_05<-sum(NB8_sens_econ$S_0.05)
NB8_075<-sum(NB8_sens_econ$S_0.075)
NB8_1<-sum(NB8_sens_econ$S_0.1)
NB8_table<-data.frame(NB8_0,NB8_025,NB8_05,NB8_075,NB8_1)


NB_sens_table_F<-data.frame(NB2_table,NB4_table,NB6_table,NB8_table)
stargazer(t(NB_sens_table_F),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_sens.tex")

# elasticities of nb to hunting perc
huntperc<-c(0,0.025,0.05,0.075,0.1)
NB2_table[1]
NB2_elast1 <-((NB2_table[1]-NB2_table[3])/NB2_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB2_elast2 <-((NB2_table[2]-NB2_table[3])/NB2_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB2_elast3 <-((NB2_table[3]-NB2_table[3])/NB2_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB2_elast4 <-((NB2_table[4]-NB2_table[3])/NB2_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB2_elast5 <-((NB2_table[5]-NB2_table[3])/NB2_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB2_list <- data.frame(NB2_elast1 ,NB2_elast2,NB2_elast3,NB2_elast4,NB2_elast5)
  
NB4_elast1 <-((NB4_table[1]-NB4_table[3])/NB4_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB4_elast2 <-((NB4_table[2]-NB4_table[3])/NB4_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB4_elast3 <-((NB4_table[3]-NB4_table[3])/NB4_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB4_elast4 <-((NB4_table[4]-NB4_table[3])/NB4_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB4_elast5 <-((NB4_table[5]-NB4_table[3])/NB4_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB4_list <- data.frame(NB4_elast1 ,NB4_elast2,NB4_elast3,NB4_elast4,NB4_elast5)

NB6_elast1 <-((NB6_table[1]-NB6_table[3])/NB6_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB6_elast2 <-((NB6_table[2]-NB6_table[3])/NB6_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB6_elast3 <-((NB6_table[3]-NB6_table[3])/NB6_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB6_elast4 <-((NB6_table[4]-NB6_table[3])/NB6_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB6_elast5 <-((NB6_table[5]-NB6_table[3])/NB6_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB6_list <- data.frame(NB6_elast1 ,NB6_elast2,NB6_elast3,NB6_elast4,NB6_elast5)

NB8_elast1 <-((NB8_table[1]-NB8_table[3])/NB8_table[3])/((huntperc[1]-huntperc[3])/huntperc[3])
NB8_elast2 <-((NB8_table[2]-NB8_table[3])/NB8_table[3])/((huntperc[2]-huntperc[3])/huntperc[3])
NB8_elast3 <-((NB8_table[3]-NB8_table[3])/NB8_table[3])/((huntperc[3]-huntperc[3])/huntperc[3])
NB8_elast4 <-((NB8_table[4]-NB8_table[3])/NB8_table[3])/((huntperc[4]-huntperc[3])/huntperc[3])
NB8_elast5 <-((NB8_table[5]-NB8_table[3])/NB8_table[3])/((huntperc[5]-huntperc[3])/huntperc[3])
NB8_list <- data.frame(NB8_elast1 ,NB8_elast2,NB8_elast3,NB8_elast4,NB8_elast5)

NB_elast_table_F<-data.frame(NB2_list,NB4_list,NB6_list,NB8_list)
write.xlsx(NB_elast_table_F, file = "./scenario_nb_elast_F.xlsx")
stargazer(t(NB_elast_table_F),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "NB_elast_F.tex")

#export all tables to excel
list_of_datasets2 <- list("Adult Male Sens" = t(NB_sens_table), "Adult Female Sens"=t(NB_sens_table_F), "NB_elast_M"= t(NB_elast_table_M),"NB_elast_F"=t(NB_elast_table_F))
write.xlsx(list_of_datasets2, file = "./scenario_nb_sens.xlsx")

# environmental trans

env_master_table1<-env_master_table %>%  pivot_wider(names_from = Age, values_from = Count)
for (i in length(nrow(env_master_table1))){
  Total_pop = env_master_table1$Fawn+env_master_table1$Juv+env_master_table1$Adult
}
env_master_table1<- add_column(env_master_table1,Total_pop,.after = "Adult")
env_master_table1<-env_master_table1 %>% pivot_longer(. , cols = !c("Scenario","char","ENV_perc","Month","Year","Category"),names_to = "Age", values_to= "Count") %>% replace(is.na(.), 0)

trans_table<-env_econ_table %>% filter(Age %in% c("Total_pop")) %>% 
  pivot_wider(names_from = Category, values_from = Count)
ggplot(data = trans_table, mapping = aes(x = Month, y = TotalSprey, color = ENV_perc)) +
  geom_smooth() +
  facet_grid(~Scenario) +
  labs(title = "population changes with environmental trans",
       y = "total prey", x = "Month")


env_econ_table<-env_master_table1 %>% filter(Age %in% c("Total_pop")) %>% 
  pivot_wider(names_from = Category, values_from = Count) %>%  
  mutate(MEAT_VALUE = 5080*TotalH,
         NM_PRICEi = case_when(TotalSprey<1000 ~ 25000,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500,
                               (TotalSprey<4000 &TotalSprey>2000) ~ 5000,
                               (TotalSprey<10000 &TotalSprey>4000) ~ 2500,
                               (TotalSprey<20000 &TotalSprey>10000) ~ 500,
                               TotalSprey>20000 ~ 100),
         NM_VALUEi = case_when(TotalSprey<1000 ~ 25000*TotalSprey,
                               (TotalSprey<2000 & TotalSprey>1000) ~ 12500*TotalSprey,
                               (TotalSprey<4000 & TotalSprey>2000) ~ 5000*TotalSprey,
                               (TotalSprey<10000 & TotalSprey>4000) ~ 2500*TotalSprey,
                               (TotalSprey<20000 & TotalSprey>10000) ~ 500*TotalSprey,
                               TotalSprey>20000 ~ 100*TotalSprey))

env_econ_table <- env_econ_table %>% replace(is.na(.), 0) %>% pivot_longer(. , cols = !c("Scenario","char","ENV_perc","Age","Month","Year"),names_to = "Category", values_to= "Count") %>% replace(is.na(.), 0)

env_econ_table <- env_econ_table  %>% pivot_wider(names_from = Category, values_from = Count)%>% select(.,Scenario,Age,ENV_perc,Month,Year,TotalSprey,NM_PRICEi,NM_VALUEi,TotalH,MEAT_VALUE) %>% replace(is.na(.), 0)

mod<-vector()
mod[env_econ_table$Month%%12 == 7] <- 1 
mod[env_econ_table$Month%%12 == 8] <- 2 
env_econ_table<-env_econ_table %>% add_column(.,mod,.after = "Month")%>%
  mutate(
    Year=as.integer(Year)-1,
    NBPVi = case_when(mod == 2 ~ ((1/((1.1)^(Year)))*(NM_VALUEi+MEAT_VALUE))),
  ) %>% replace(is.na(.), 0)
env_econ_table <- filter(env_econ_table, mod > 0)

env_econ_table <- env_econ_table %>% pivot_longer(. , cols = !c("Scenario","Age","ENV_perc","Month","mod","Year"),names_to = "Category", values_to= "Count")

# scenario 3 
NB3_env_econ <- env_econ_table %>% filter(Scenario %in% c("SCEN3"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = ENV_perc, values_from=Count) 

NB_scen3_envperc0<-sum(NB3_env_econ$"0")
NB_scen3_envperc01<-sum(NB3_env_econ$"01")
NB_scen3_envperc025<-sum(NB3_env_econ$"025")
NB_scen3_envperc05<-sum(NB3_env_econ$"05")
NB3_table<-data.frame(NB_scen3_envperc0,NB_scen3_envperc01,NB_scen3_envperc025,NB_scen3_envperc05)

# scenario 4
NB4_env_econ<-env_econ_table %>% filter(Scenario %in% c("SCEN4"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = ENV_perc, values_from=Count) 

NB_scen4_envperc0<-sum(NB4_env_econ$"0")
NB_scen4_envperc01<-sum(NB4_env_econ$"01")
NB_scen4_envperc025<-sum(NB4_env_econ$"025")
NB_scen4_envperc05<-sum(NB4_env_econ$"05")
NB4_table<-data.frame(NB_scen4_envperc0,NB_scen4_envperc01,NB_scen4_envperc025,NB_scen4_envperc05)

# scenario 7
NB7_env_econ<-env_econ_table %>% filter(Scenario %in% c("SCEN7"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = ENV_perc, values_from=Count) 

NB_scen7_envperc0<-sum(NB7_env_econ$"0")
NB_scen7_envperc01<-sum(NB7_env_econ$"01")
NB_scen7_envperc025<-sum(NB7_env_econ$"025")
NB_scen7_envperc05<-sum(NB7_env_econ$"05")
NB7_table<-data.frame(NB_scen7_envperc0,NB_scen7_envperc01,NB_scen7_envperc025,NB_scen7_envperc05)

# scenario 8
NB8_env_econ<-env_econ_table %>% filter(Scenario %in% c("SCEN8"), Category %in% c("NBPVi")) %>% pivot_wider(names_from = ENV_perc, values_from=Count) 

NB_scen8_envperc0<-sum(NB8_env_econ$"0")
NB_scen8_envperc01<-sum(NB8_env_econ$"01")
NB_scen8_envperc025<-sum(NB8_env_econ$"025")
NB_scen8_envperc05<-sum(NB8_env_econ$"05")
NB8_table<-data.frame(NB_scen8_envperc0,NB_scen8_envperc01,NB_scen8_envperc025,NB_scen8_envperc05)


NB_env_table<-data.frame(NB3_table,NB4_table,NB7_table,NB8_table)
stargazer(t(NB_env_table),                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=0,
          out = "NB_env.tex")


#export all tables to excel
list_of_datasets <- list("AMale_hunt" = NB_sens_table, "AFemale_hunt"= NB_sens_table_F, "EnvTrans" = NB_env_table)
write.xlsx(NB_env_table, file = "./scenario_env_trans.xlsx")

## POP ELASTICITY TABLES

# ADULT MALES
elast_table <- data.frame()
elast_table2 <- data.frame()
age_name    <- c("Fawn","Juv","Adult")
elast_name  <- c("Elast0","Elast05","Elast1","Elast15","Elast20") 

for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AM %>% 
      filter(Scenario %in% c(c("SCEN2_SENS")),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table2<-rbind(elast_table2, data.frame( 
      avg_elast2=mean(elast_table$ELASTICITY)))
  }
}
  
elast_table <- data.frame()
elast_table4 <- data.frame()
for (w in 1:length(age_name)){
    for (j in 1:5){
elast_table<- elasticity_AM %>% 
  filter(Scenario %in% c(c("SCEN4_SENS")),
         Category %in% c("TotalSprey"),
         Year %in% c("20"),
         Age %in% c(age_name[w]),
         Elasticity_Sensitivity_Level %in% c(elast_name[j]))
elast_table4<-rbind(elast_table4, data.frame( 
  avg_elast4=mean(elast_table$ELASTICITY)))
 }
 }
 
elast_table <- data.frame()
elast_table6 <- data.frame()
  for (w in 1:length(age_name)){
     for (j in 1:5){
       elast_table<- elasticity_AM %>% 
         filter(Scenario %in% c("SCEN6_SENS"),
                Category %in% c("Totalpreyh"),
                Year %in% c("20"),
                Age %in% c(age_name[w]),
                Elasticity_Sensitivity_Level %in% c(elast_name[j]))
       elast_table6<-rbind(elast_table6, data.frame( 
         avg_elast6=mean(elast_table$ELASTICITY)))
   }
   }

elast_table <- data.frame()
elast_table8 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AM %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table8<-rbind(elast_table8, data.frame( 
      avg_elast8=mean(elast_table$ELASTICITY)))
  }
}

elast_table_M<-cbind(elast_table2,elast_table4,elast_table6,elast_table8)
stargazer(elast_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastM.tex")
write.xlsx(elast_table_M,"elastM.xlsx")

## PREVELANCE TABLES

# ADULT MALES

age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev0","Prev05","Prev1","Prev15","Prev20") 
hunt_name  <- c("S_0","S_0.05","S_0.1","S_0.15","S_0.2") 
year<-c(as.character(0:19))

prev_table <- data.frame()
prev_table4 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:length(prev_name)){
    prev_table<- prev_AM %>% 
      filter(Scenario %in% c("SCEN4_SENS"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
      prev_table4<-rbind(prev_table4, 
                         data.frame(age=age_name[w],
                                    prev=prev_name[j],
                                    avg_elast_prev4=mean(prev_table$PREV_Elast)))
    }
  }

prev_avg_table <- data.frame()
prev_avg4 <- data.frame()
  for (w in 1:length(age_name)){
    for (j in 1:5){
      prev_avg_table<-PREV_DIFF_AM %>% filter(Scenario %in% c("SCEN4_SENS"), 
                                              Category %in% c("PREV"),
                                              Age %in% c(age_name[w]),
                                              Hunt_perc %in% c(hunt_name[j]))
      prev_avg4<-rbind(prev_avg4, data.frame(age=age_name[w],
                                                 prev=hunt_name[j],
                                                 avg4=mean(prev_avg_table$Count)))
    }
  }


prev_table <- data.frame()
prev_table8 <- data.frame()

for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table<- prev_AM %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table8<-rbind(prev_table8, data.frame(age=age_name[w],
                                               prev=prev_name[j],
                                               avg_elast_prev8=mean(prev_table$PREV_Elast)))
  }
}


prev_avg_table <- data.frame()
prev_avg8 <- data.frame()
  for (w in 1:length(age_name)){
    for (j in 1:5){
      prev_avg_table<-PREV_DIFF_AM %>% filter(Scenario %in% c("SCEN8_SENS"), 
                                              Category %in% c("PREV"),
                                              Age %in% c(age_name[w]),
                                              Hunt_perc %in% c(hunt_name[j]))
      prev_avg8<-rbind(prev_avg8, data.frame(age=age_name[w],
                                             prev=hunt_name[j],
                                             avg8=mean(prev_avg_table$Count)))
    }
  }

prev_elast_M<-cbind(prev_table4,prev_avg4,prev_table8,prev_avg8)
stargazer(prev_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastM.tex")
write.xlsx(prev_elast_M, "prevavg_table_elast_M.xlsx", rowNames=TRUE)

prev_table_avg <- data.frame()
prev_table4_avg <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
hunt_name  <- c("S_0","S_0.05","S_0.1","S_0.15","S_0.2") 

prev_table_avg <- data.frame()
prev_table4_avg <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table_avg<- PREV_DIFF_AM %>% 
      filter(Scenario %in% c("SCEN4_SENS"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table4_avg<-rbind(prev_table4_avg, data.frame(avg_prev4=mean(prev_table_avg$Count),hunt_perc=hunt_name[j],age=age_name[w]))
  }
}

prev_table_avg <- data.frame()
prev_table8_avg <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table_avg<- PREV_DIFF_AM %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table8_avg<-rbind(prev_table8_avg, data.frame( avg_prev8=mean(prev_table_avg$Count),hunt_perc=hunt_name[j],age=age_name[w]))
  }
}

prev_table_M<-cbind(prev_table4_avg,prev_table8_avg)
stargazer(prev_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastM.tex")
write.xlsx(prev_table_M, "prev_table_count_M.xlsx", rowNames=TRUE)


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
    prey_table26<-rbind(prey_table26, data.frame(scen=scen_name[w],
                                                 hunt_perc=hunt_name[j],
                                                 Pop=mean(prey_table$Count)))
  }
}

scen_name<- c("SCEN4_SENS","SCEN8_SENS")
prey_table <- data.frame()
prey_table48 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prey_table<- AD_MALE_table1 %>% 
      filter(Scenario %in% c(scen_name[w]),
             Age %in% c("Total_pop"),
             Category %in% c("Totalpreyi"),
             Hunt_perc %in% c(hunt_name[j]))
    prey_table48<-rbind(prey_table48, data.frame(scen=scen_name[w],
                                                 hunt_perc=hunt_name[j],
                                                 Pop=mean(prey_table$Count)))
  }
}


prey_table_M<-cbind(prey_table26,prey_table48)
stargazer(prey_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prey_table_M.tex")
write.xlsx(prey_table_M, "prey_table_count_M.xlsx", rowNames=TRUE)
write.xlsx(prey_table48, "preyS_table_count_M.xlsx", rowNames=TRUE)

# cummulative hunting elast

cummul_table <- data.frame()
cummul_table2 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
    cummul_table<- cummul_hunt_AM2 %>% 
      filter(Cummulative_hunt4 %in% c(hunt_cummul_name[j]))
    cummul_table2<-rbind(cummul_table2, data.frame( 
      avg_elast_cummul_hunt2=mean(cummul_table$Cummul_hunt_Elast4)))

}

cummul_table <- data.frame()
cummul_table4 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AM4 %>% 
    filter(Cummulative_hunt4 %in% c(hunt_cummul_name[j]))
  cummul_table4<-rbind(cummul_table4, data.frame( 
    avg_elast_cummul_hunt4=mean(cummul_table$Cummul_hunt_Elast4)))
  
}
cummul_table <- data.frame()
cummul_table6 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AM6 %>% 
    filter(Cummulative_hunt4 %in% c(hunt_cummul_name[j]))
  cummul_table6<-rbind(cummul_table6, data.frame( 
    avg_elast_cummul_hunt6=mean(cummul_table$Cummul_hunt_Elast4)))
  
}
cummul_table <- data.frame()
cummul_table8 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AM8 %>% 
    filter(Cummulative_hunt4 %in% c(hunt_cummul_name[j]))
  cummul_table8<-rbind(cummul_table8, data.frame( 
    avg_elast_cummul_hunt8=mean(cummul_table$Cummul_hunt_Elast4)))
  
}
cummul_hunt_table_M<-cbind(cummul_table2,cummul_table4,cummul_table6,cummul_table8)
stargazer(cummul_hunt_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_M.tex")

write.xlsx(cummul_hunt_table_M, file = "./scenario_cumulative_M.xlsx",rowNames=TRUE)

cummul_table <- data.frame()
cummul_table4 <- data.frame()
cwd_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
  cummul_table<- cummul_cwd_AM4 %>% 
    filter(Cummulative_hunt4 %in% c(cwd_cummul_name[j]))
  cummul_table4<-rbind(cummul_table4, data.frame( 
    avg_elast_cummul_cwd4=mean(cummul_table$Cummul_hunt_Elast4)))
  
}
cummul_table <- data.frame()
cummul_table8 <- data.frame()
cwd_cummul_name  <- c("Cummul_hunt0","Cummul_hunt05","Cummul_hunt1","Cummul_hunt15","Cummul_hunt2") 

for (j in 1:5){
  cummul_table<- cummul_cwd_AM8 %>% 
    filter(Cummulative_hunt4 %in% c(cwd_cummul_name[j]))
  cummul_table8<-rbind(cummul_table8, data.frame( 
    avg_elast_cummul_hunt8=mean(cummul_table$Cummul_hunt_Elast4)))
}  

cummul_cwd_table_M<-cbind(cummul_table4,cummul_table8)
stargazer(cummul_cwd_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_M.tex")
write.xlsx(cummul_cwd_table_M, file = "./scenario_cumulative_cwd_M.xlsx",rowNames=TRUE)


# ADULT FEMALES

elast_table <- data.frame()
elast_table2 <- data.frame()
age_name    <- c("Fawn","Juv","Adult")
elast_name  <- c("Elast0","Elast025","Elast05","Elast075","Elast10") 

for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c("SCEN2_SENS"),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table2<-rbind(elast_table2, data.frame( 
      avg_elast2=mean(elast_table$ELASTICITY)))
  }
}

elast_table <- data.frame()
elast_table4 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c("SCEN4_SENS"),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table4<-rbind(elast_table4, data.frame( 
      avg_elast4=mean(elast_table$ELASTICITY)))
  }
}

elast_table <- data.frame()
elast_table6 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c("SCEN6_SENS"),
             Category %in% c("Totalpreyh"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table6<-rbind(elast_table6, data.frame( 
      avg_elast6=mean(elast_table$ELASTICITY)))
  }
}

elast_table <- data.frame()
elast_table8 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    elast_table<- elasticity_AF %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Category %in% c("TotalSprey"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Elasticity_Sensitivity_Level %in% c(elast_name[j]))
    elast_table8<-rbind(elast_table8, data.frame( 
      avg_elast8=mean(elast_table$ELASTICITY)))
  }
}

elast_table_F<-cbind(elast_table2,elast_table4,elast_table6,elast_table8)
stargazer(elast_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "elastF.tex")
write.xlsx(elast_table_F,"elastF.xlsx")

## PREVELANCE TABLES

# ADULT FEMALES prev elast

age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev0","Prev025","Prev05","Prev075","Prev10") 

prev_table <- data.frame()
prev_table4 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table<- prev_AF %>% 
      filter(Scenario %in% c("SCEN4_SENS"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table4<-rbind(prev_table4, data.frame(age=age_name[w],
                                               prev=prev_name[j],
                                               avg_elast_prev4=mean(prev_table$PREV_Elast)))
  }
}

prev_avg_table <- data.frame()
prev_avg4 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_avg_table<-PREV_DIFF_AF %>% filter(Scenario %in% c("SCEN4_SENS"), 
                                            Category %in% c("PREV"),
                                            Age %in% c(age_name[w]),
                                            Hunt_perc %in% c(hunt_name[j]))
    prev_avg4<-rbind(prev_avg4, data.frame(age=age_name[w],
                                           prev=hunt_name[j],
                                           avg4=mean(prev_avg_table$Count)))
  }
}

prev_table <- data.frame()
prev_table8 <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
prev_name  <- c("Prev0","Prev025","Prev05","Prev075","Prev10") 

for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table<- prev_AF %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Prevalence %in% c(prev_name[j]))
    prev_table8<-rbind(prev_table8, data.frame( 
      avg_elast_prev8=mean(prev_table$PREV_Elast)))
  }
}

prev_avg_table <- data.frame()
prev_avg8 <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_avg_table<-PREV_DIFF_AF %>% filter(Scenario %in% c("SCEN8_SENS"), 
                                            Category %in% c("PREV"),
                                            Age %in% c(age_name[w]),
                                            Hunt_perc %in% c(hunt_name[j]))
    prev_avg8<-rbind(prev_avg8, data.frame(age=age_name[w],
                                           prev=hunt_name[j],
                                           avg8=mean(prev_avg_table$Count)))
  }
}

prev_elast_F<-cbind(prev_table4,prev_avg4,prev_table8,prev_avg8)
stargazer(prev_elast_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prev_F.tex")
write.xlsx(prev_elast_F, file = "./prev_elast_F.xlsx",rowNames=TRUE)
write.xlsx(prev_avg4, file = "./prev4.xlsx",rowNames=TRUE)
# prev count
prev_table_avg <- data.frame()
prev_table4_avg <- data.frame()
age_name    <- c("Fawn","Juv","Adult","Total_pop")
hunt_name  <- c("S_0","S_0.025","S_0.05","S_0.075","S_0.1") 

prev_table_avg <- data.frame()
prev_table4_avg <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table_avg<- PREV_DIFF_AF %>% 
      filter(Scenario %in% c("SCEN4_SENS"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table4_avg<-rbind(prev_table4_avg, data.frame(avg_prev4=mean(prev_table_avg$Count),hunt_perc=hunt_name[j],age=age_name[w]))
  }
}

prev_table_avg <- data.frame()
prev_table8_avg <- data.frame()
for (w in 1:length(age_name)){
  for (j in 1:5){
    prev_table_avg<- PREV_DIFF_AF %>% 
      filter(Scenario %in% c("SCEN8_SENS"),
             Year %in% c("20"),
             Age %in% c(age_name[w]),
             Category %in% c("PREV"),
             Hunt_perc %in% c(hunt_name[j]))
    prev_table8_avg<-rbind(prev_table8_avg, data.frame( avg_prev8=mean(prev_table_avg$Count),hunt_perc=hunt_name[j],age=age_name[w]))
  }
}

prev_table_F<-cbind(prev_table4_avg,prev_table8_avg)
stargazer(prev_table_M,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevF.tex")
write.xlsx(prev_table_F, "prev_table_count_F.xlsx", rowNames=TRUE)

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
    prey_table26<-rbind(prey_table26, data.frame(Pop=mean(prey_table$Count),hunt_perc=hunt_name[j],scen=scen_name[w]))
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
    prey_table48<-rbind(prey_table48, data.frame(Pop=mean(prey_table$Count),hunt_perc=hunt_name[j],scen=scen_name[w]))
  }
}

prey_table_F<-cbind(prey_table26,prey_table48)
stargazer(prey_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "prevF.tex")
write.xlsx(prey_table_F, "prey_table_count_F.xlsx", rowNames=TRUE)
write.xlsx(prey_table48, "preyS_table_count_F.xlsx", rowNames=TRUE)


# cummulative hunting elast

cummul_table <- data.frame()
cummul_table2 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AF2 %>% 
    filter(Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_table2<-rbind(cummul_table2, data.frame( 
    avg_elast_cummul_hunt2=mean(cummul_table$Cummul_hunt_Elast)))
  
}

cummul_table <- data.frame()
cummul_table4 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AF4 %>% 
    filter(Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_table4<-rbind(cummul_table4, data.frame( 
    avg_elast_cummul_hunt4=mean(cummul_table$Cummul_hunt_Elast)))
  
}
cummul_table <- data.frame()
cummul_table6 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AF6 %>% 
    filter(Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_table6<-rbind(cummul_table6, data.frame( 
    avg_elast_cummul_hunt6=mean(cummul_table$Cummul_hunt_Elast)))
  
}
cummul_table <- data.frame()
cummul_table8 <- data.frame()
hunt_cummul_name  <- c("Cummul_hunt0","Cummul_hunt025","Cummul_hunt05","Cummul_hunt075","Cummul_hunt1") 

for (j in 1:5){
  cummul_table<- cummul_hunt_AF8 %>% 
    filter(Cummulative_hunt %in% c(hunt_cummul_name[j]))
  cummul_table8<-rbind(cummul_table8, data.frame( 
    avg_elast_cummul_hunt8=mean(cummul_table$Cummul_hunt_Elast)))
  
}
cummul_hunt_table_F<-cbind(cummul_table2,cummul_table4,cummul_table6,cummul_table8)
stargazer(cummul_hunt_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_F.tex")
write.xlsx(cummul_hunt_table_F, file = "./cummul_hunt_table_F.xlsx",rowNames=TRUE)


# cummulative cwd death elast

cummul_table <- data.frame()
cummul_table4 <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd0","Cummul_cwd025","Cummul_cwd05","Cummul_cwd075","Cummul_cwd1") 

for (j in 1:5){
  cummul_table<- cummul_cwd_AF4 %>% 
    filter(Cummulative_cwd %in% c(cwd_cummul_name[j]))
  cummul_table4<-rbind(cummul_table4, data.frame( 
    avg_elast_cummul_cwd4=mean(cummul_table$Cummul_cwd_Elast)))
  
}

cummul_table <- data.frame()
cummul_table8 <- data.frame()
cwd_cummul_name  <- c("Cummul_cwd0","Cummul_cwd025","Cummul_cwd05","Cummul_cwd075","Cummul_cwd1") 
for (j in 1:5){
  cummul_table<- cummul_cwd_AF8 %>% 
    filter(Cummulative_cwd %in% c(cwd_cummul_name[j]))
  cummul_table8<-rbind(cummul_table8, data.frame( 
    avg_elast_cummul_cwd8=mean(cummul_table$Cummul_cwd_Elast)))
  
}
cummul_cwd_table_F<-cbind(cummul_table4,cummul_table8)
stargazer(cummul_cwd_table_F,                 # Export txt
          summary = FALSE,
          type = "latex",
          digits=4,
          out = "cummul_hunt_table_F.tex")
write.xlsx(cummul_cwd_table_F, file = "./cummul_cwd_table_F.xlsx",rowNames=TRUE)


# export tables to excel
list_of_datasets <- list("Scen1" = netbenefit_scen, "Scen2" = netbenefit_scen2,"Scen3" = netbenefit_scen3,"Scen4" = netbenefit_scen4,"Scen5" = netbenefit_scen5,"Scen6" = netbenefit_scen6,"Scen7" = netbenefit_scen7,"Scen8" = netbenefit_scen8, "Sum of each Scenario"= NB_all_table, "A_M Hunt" = t(NB_sens_table), "A_F Hunt"=t(NB_sens_table_F), "EnvTrans" = t(NB_env_table), "A_M Elast" = elast_table_M, "A_F Elast" = elast_table_F)
write.xlsx(list_of_datasets, file = "./scenario_nb_totalpop1.xlsx",rowNames=TRUE)


cummulative<- read.csv("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/scenario_control_totalpop.csv")


