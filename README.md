# Vir_interaction
 
This repository contains the code for the paper: Characterising the asynchronous resurgence of common respiratory viruses following the onset of the COVID-19 pandemic: a systematic analysis

All the fitting, analysis and figure generation can be run from "main.R". This loads in all the dependencies and functions. It has to be run in the order specified, and opens any files where manual edits can be made (e.g. type of simulation outcome). All the code and data (which is publically available) are in the repo, and can be run directly from the master script. 

Authors: Chenkai Zhao, Tiantian Zhang, Ling Guo, Shiqi Sun, Yumeng Miao, Chee Fu Yung, Jane Tomlinson, Kirill Stolyarov, Zakhar Shchomak, Yong Poovorawan, David James Nokes, Carmen Muñoz-Almagro, Michal Mandelboim, James W Keck, Joanne Marie Langley, Terho Heikkinen, Jikui Deng, Philippe Colson, Giorgi Chakhunashvili, Mauricio T. Caballero, Louis Bont, Daniel R Feikin, Harish Nair, Xin Wang, You Li for the Respiratory Virus Global Epidemiology Network
See LICENSE file for licensing details.

Corresponding author: You Li, You.Li@njmu.edu.cn 
                      Xin Wang, Xin.Wang@njmu.edu.cn

Input/Output files - see paper for references: 
- Data/Full-text table-sample.xlsx: sample data

Package versions are: 
readxl_1.4.3
finalfit_1.0.8
tidyverse_2.0.0
data.table_1.16.0
mapdata_2.3.1
tidygeocoder_1.0.5
foreach_1.5.2
doParallel_1.0.17
MASS_7.3-60
lmtest0.9-40
sandwich3.1-0
lme4_1.1-35.5
merTools_0.6.2
sjstats_0.19.0
glmmTMB_1.1.8
metafor_4.6-0


All analysis was done in R version 4.2.2 on macOS Sonoma 14.6.1. 
