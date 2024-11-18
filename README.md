# Vir_interaction
 
This repository contains the code for the paper: **Characterising the asynchronous resurgence of common respiratory viruses following the onset of the COVID-19 pandemic: a systematic analysis**

Authors: Chenkai Zhao, Tiantian Zhang, Ling Guo, Shiqi Sun, Yumeng Miao, Chee Fu Yung, Jane Tomlinson, Kirill Stolyarov, Zakhar Shchomak, Yong Poovorawan, David James Nokes, Carmen Muñoz-Almagro, Michal Mandelboim, James W Keck, Joanne Marie Langley, Terho Heikkinen, Jikui Deng, Philippe Colson, Giorgi Chakhunashvili, Mauricio T. Caballero, Louis Bont, Daniel R Feikin, Harish Nair, Xin Wang, You Li for the Respiratory Virus Global Epidemiology Network

Corresponding author: 
You Li, You.Li@njmu.edu.cn 
Xin Wang, Xin.Wang@njmu.edu.cn

All the fitting, analysis and figure generation can be run from "main.R". This loads in all the dependencies and functions. It has to be run in the order specified, and opens any files where manual edits can be made (e.g. type of simulation outcome). All the code and data (which is publically available) are in the repo, and can be run directly from the master script. 

**IMPORTANT:**
- Please confirm that **Rtools** is installed on your computer, otherwise the code cannot run. 
- Please install the necessary packages at first.
- This analysis uses parallel computing. Please modify it according to the number of CPU cores you wish to use.
- In sensitivity analyses 4 and 5, if you want to save the correct `Matching_Recir.pdf` and `Matching_Peak.pdf` files, you need to run the scripts `Code/Sens4.R` and `Code/Sens5.R` line by line. If you run these two scripts using `source()` function in `Code/main.r`, you will get the same results, but the display of the figures will be different.



Input files: 
- All_Vir.RData: This file is the processed result of the `Full-text table-V1.8.1.xlsx` file, used for all analyses in this research.
- Location.RData: This file contains coordinate information for each research site. You can directly use the existing file. If you want to run the source code to obtain coordinate information, you need a Google Developer account and configure the Google map API.
- Data/Full-text table-V1.8.1.xlsx: All collected data from research paper, surveillance and RSV GEN.

Output files - see paper for references: 
- When the code runs, it will generate a file path named Output, and all running results will be saved to this path.


All analysis was done in R version 4.2.2 on macOS Sonoma 14.6.1. Package versions are: 
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
glmmTMB_1.1.9  
metafor_4.6-0  



See LICENSE file for licensing details.