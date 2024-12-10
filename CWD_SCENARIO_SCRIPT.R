
  
### Start by loading in the packages and functions needed for pulling data. I connect to a database to store all of the data I am generating.

require(tidyverse)
require(RSQLite)
require(DBI)
require(popbio)


# LOAD IN THE FUNCTIONS
source("CWD_FUNCTIONS.R")

# CONNECT TO DATABASE
#dbConn<- dbConnect(SQLite(), "ElkCWD_Tables.sqlite")

### I code all scenarios with their respective parameters. There are 8 scenarios all with varying degrees of complexity. I was able to create two functions (with and without disease) for the base where there is no hunting and no predation. When H=1, Hunting is allowed in the scenario. When P=1, predation is added to the scenario. 

Juv = c(1:2)
Adult = c(3:6) 
Old = c(7:18)
ages<-list(Juv=Juv,Adult=Adult, Old=Old)


# PREY MODEL
SCEN1<-calcHealthy(H=0,P=0)

# PREY WITH HUNTING
SCEN2<-calcHealthy(H=1,P=0)

# PREY WITH INFECTION
SCEN3<-calcInfection(H=0,P=0)

# PREY WITH HUNTING AND INFECTION
SCEN4<-calcInfection(H=1,P=0)

# PREY WITH PREDATION
SCEN5<-calcHealthy(H=0,P=1)

# PREY WITH PREDATION AND HUNTING
SCEN6<-calcHealthy(H=1,P=1)

#PREY WITH PREDATION AND INFECTION
SCEN7<-calcInfection(H=0,P=1)

# PREY WITH PREDATION, HUNTING, AND INFECTION
SCEN8<-calcInfection(H=1,P=1)

### In this chunk, I am gathering all of the data from each scenario and putting it in one grand data frame.

# COMBINING ALL SCENARIOS INTO ONE LIST
scenario_total<-list(SCEN1=SCEN1,SCEN2=SCEN2,SCEN3=SCEN3,SCEN4=SCEN4,SCEN5=SCEN5,SCEN6=SCEN6,SCEN7=SCEN7,SCEN8=SCEN8)

# NAMES OF SCENARIOS FOR LOOP
scenario_name<-c("SCEN1","SCEN2","SCEN3","SCEN4","SCEN5","SCEN6","SCEN7","SCEN8")

names(scenario_total[[8]])[10]

# LOOP COMBINING ALL SCENARIOS IN LONG FORM
master_table<-data.frame()
for(k in 1:length(scenario_total)){
  for (i in 1:length(ages)) {
    for (j in 1:length(scenario_total[[k]])){
      vars<-unlist(mget(names(ages)[i]))
      master_table<- rbind(master_table, data.frame(Scenario = scenario_name[k], 
                                                    Run = 1,
                                                    Category = names(scenario_total[[k]])[j],
                                                    Age = names(ages)[i],
                                                    Month = 1:ncol(scenario_total[[k]][[j]]),
                                                    Count = sapply(1:ncol(scenario_total[[k]][[j]]), FUN = function(x) {sum(scenario_total[[k]][[j]][vars,x])})))
    }
  }
}

master_table<- master_table %>% mutate(Year=case_when(Month %in% c(1:12) ~ "1", Month %in% c(13:24) ~ "2",
                                                      Month %in% c(25:36) ~ "3", Month %in% c(37:48) ~ "4",
                                                      Month %in% c(49:60) ~ "5", Month %in% c(61:72) ~ "6",
                                                      Month %in% c(73:84) ~ "7", Month %in% c(85:96) ~ "8",
                                                      Month %in% c(97:108) ~ "9", Month %in% c(109:120) ~ "10",
                                                      Month %in% c(121:132) ~ "11", Month %in% c(133:144) ~ "12",
                                                      Month %in% c(145:156) ~ "13", Month %in% c(157:168) ~ "14",
                                                      Month %in% c(169:180) ~ "15", Month %in% c(181:192) ~ "16",
                                                      Month %in% c(193:204) ~ "17", Month %in% c(205:216) ~ "18",
                                                      Month %in% c(217:228) ~ "19", Month %in% c(229:240) ~ "20"))

master_table<-master_table %>% pivot_wider(names_from = Category, values_from = Count)%>%
  mutate(Totalpreyi = TotalSprey+TotalIprey,Totalpreyh = FpreyA+MpreyA)

master_table<-master_table %>% pivot_longer(. , cols = !c("Scenario","Run","Age","Month","Year"),names_to = "Category", values_to= "Count")

### In this last portion, I am saving the results to a database and then querying that datbase to make sure the tables show up. At the end, I disconnect from the database.

# SAVE RESULTS TO DATABASE
# dbWriteTable(dbConn, name = "tblSimulationResults_Scen_all", value = master_table, overwrite = TRUE, append = FALSE)
# 
# # LIST OF TABLES 
# dbListTables(dbConn)
# 
# # CALLING A TABLE
# dat<- dbReadTable(dbConn, name = "tblSimulationResults_Scen_all")
# 
# # QUERYING THE DATABASE FOR EACH SCENARIO (IF YOU WANT)
# # 
# 
# tabl1<- dbSendQuery(dbConn, "SELECT * FROM tblSimulationResults_Scen_all WHERE Scenario = 'SCEN1'") %>% 
#   dbFetch() 
# tabl2<- dbSendQuery(dbConn, "SELECT * FROM tblSimulationResults_Scen_all WHERE Scenario = 'SCEN2'") %>% 
#   dbFetch()
# tabl3<- dbSendQuery(dbConn, "SELECT * FROM tblSimulationResults_Scen_all WHERE Scenario = 'SCEN3'") %>% 
#   dbFetch()
# tabl4<- dbSendQuery(dbConn, "SELECT * FROM tblSimulationResults_Scen_all WHERE Scenario = 'SCEN4'") %>% 
#   dbFetch()
# 
# 
# # disconnect
# dbDisconnect(dbConn)
# 
