

### Start by loading in the packages and functions needed for pulling data. I connect to a database to store all of the data I am generating.

require(tidyverse)
require(RSQLite)
require(DBI)
require(popbio)


# LOAD IN THE FUNCTIONS
source("C:/Users/katie/Dropbox/Katie B/CWD Scenarios/CWD_FUNCTIONS.R")

# CONNECT TO DATABASE
dbConn<- dbConnect(SQLite(), "ElkCWD_Tables.sqlite")

### I code all scenarios with their respective parameters. There are 8 senarios all with varying degrees of complexity. I was able to create two functions (with and without disease) for the base where there is no hunting and no predation. When H=1, Hunting is allowed in the scenario. When P=1, predation is added to the scenario. 

Fawn = c(1:2)
Juv = c(3:6) 
Adult = c(7:18)
ages<-list(Fawn=Fawn,Juv=Juv,Adult=Adult)


# PREY WITH INFECTION
ENV0<-calcInfection(H=0,P=0)
ENV01<-calcInfection(H=0,P=0, env.foi=0.01)
ENV025<-calcInfection(H=0,P=0, env.foi=0.025)
ENV05<-calcInfection(H=0,P=0, env.foi=0.05)
SCEN3_SENS_ENV      <-list(ENV0=ENV0,
                           ENV01=ENV01,
                           ENV025=ENV025,
                           ENV05=ENV05)

# PREY WITH HUNTING AND INFECTION
ENV0<-calcInfection(H=1,P=0)
ENV01<-calcInfection(H=1,P=0, env.foi=0.01)
ENV025<-calcInfection(H=1,P=0, env.foi=0.025)
ENV05<-calcInfection(H=1,P=0, env.foi=0.05)
SCEN4_SENS_ENV      <-list(ENV0=ENV0,
                           ENV01=ENV01,
                           ENV025=ENV025,
                           ENV05=ENV05)

#PREY WITH PREDATION AND INFECTION
ENV0<-calcInfection(H=0,P=1)
ENV01<-calcInfection(H=0,P=1, env.foi=0.01)
ENV025<-calcInfection(H=0,P=1, env.foi=0.025)
ENV05<-calcInfection(H=0,P=1, env.foi=0.05)
SCEN7_SENS_ENV      <-list(ENV0=ENV0,
                           ENV01=ENV01,
                           ENV025=ENV025,
                           ENV05=ENV05)

# PREY WITH PREDATION, HUNTING, AND INFECTION
ENV0<-calcInfection(H=1,P=1)
ENV01<-calcInfection(H=1,P=1, env.foi=0.01)
ENV025<-calcInfection(H=1,P=1, env.foi=0.025)
ENV05<-calcInfection(H=1,P=1, env.foi=0.05)
SCEN8_SENS_ENV      <-list(ENV0=ENV0,
                           ENV01=ENV01,
                           ENV025=ENV025,
                           ENV05=ENV05)

### In this chunk, I am gathering all of the data from each scenario and putting it in one grand data frame.

# COMBINING ALL SCENARIOS INTO ONE LIST
scenario_total<-list(SCEN3=SCEN3_SENS_ENV,SCEN4=SCEN4_SENS_ENV,
                     SCEN7=SCEN7_SENS_ENV,SCEN8=SCEN8_SENS_ENV)

# NAMES OF SCENARIOS FOR LOOP
scenario_name<-c("SCEN3","SCEN4","SCEN7","SCEN8")

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
env_master_table<-data.frame()
for(k in 1:length(scenario_total)){
  for (j in 1:length(scenario_total[[k]])){
    for (w in 1:length(scenario_total[[k]][[j]])){
      for (i in 1:length(ages)) {
        vars<-unlist(mget(names(ages)[i]))
        env_master_table<- rbind(env_master_table, data.frame(Scenario = names(scenario_total)[k],
                                                        ENV_perc = names(scenario_total[[k]])[j],
                                                        Category = names(scenario_total[[k]][[j]])[w],
                                                        Age = names(ages)[i],
                                                        Month = 1:ncol(scenario_total[[k]][[j]][[w]]),
                                                        Count = sapply(1:ncol(scenario_total[[k]][[j]][[w]]), FUN = function(x) {sum(scenario_total[[k]][[j]][[w]][vars,x])})))
      }
    }
  }
}

env_master_table<- env_master_table %>%   separate(ENV_perc, into = c("char","ENV_perc"), sep = "V", remove = FALSE) %>%  mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                                                                                                            Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                                                                                                            Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                                                                                                            Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                                                                                                            Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                                                                                                            Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                                                                                                            Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                                                                                                            Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                                                                                                            Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                                                                                                            Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

env_master_table<-env_master_table %>% pivot_wider(names_from = Category, values_from = Count) %>% mutate(Totalpreyi = TotalSprey+TotalIprey)

env_master_table<-env_master_table %>% pivot_longer(. , cols = !c("Scenario","char","ENV_perc","Age","Month","Year"),names_to = "Category", values_to= "Count")

### In this last portion, I am saving the results to a database and then querying that datbase to make sure the tables show up. At the end, I disconnect from the database.

# SAVE RESULTS TO DATABASE
dbWriteTable(dbConn, name = "tblSimulationResults_ENV_all", value = master_table, overwrite = TRUE, append = FALSE)


# disconnect
dbDisconnect(dbConn)

