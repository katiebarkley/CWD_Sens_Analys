# CWD_Sens_Analys
An analysis of work originally by Paul Cross to investigate the effect of various male and female hunting rate on NB.

Various files exist for this document to look at different combiantions of parameters. The original code is labelled "cwd_detmod_predation_late.R". You do not need to run this to get the others to work. 

First, run the CWD_FUNCTIONS.R file to generate the functions needed for each of the other files. 
Next, you can run any of the code besdies "ECON_ANALYSIS.R" and "GRAPH_ANALYSIS.R". These two files must be run (and possibly altered depending on which file you run) AFTER you select a sensitivity analysis you wish to explore. The names of the files other than the one mentioned below descirbe what parameters are investigated in the sensitivity analysis.

The file "Cross_Fig2_CodeandGraph_barkley.R" is the most compact version of the sensitivity analysis and looks specifically at recreating Figure 2 from the paper "Examination of the interaction between age- specific predation and chronic disease in the Greater Yellowstone Ecosystem" by Brandell et al. 2021. This file contains the economic analysis specific to the sensitivity analysis of varying male and female harvest, predation effectiveness, and the number of predators. It is the most recent version and therefore it is all contained in one file for ease. This file also contains code to export all tables created to excel. 
