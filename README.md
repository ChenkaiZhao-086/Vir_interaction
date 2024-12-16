# Asynchronous resurgence of common respiratory viruses
 
This repository contains the code for the paper: **Characterising the asynchronous resurgence of common respiratory viruses following the onset of the COVID-19 pandemic**

Authors: Chenkai Zhao, Tiantian Zhang, Ling Guo, Shiqi Sun, Yumeng Miao, Chee Fu Yung, Jane Tomlinson, Kirill Stolyarov, Zakhar Shchomak, Yong Poovorawan, David James Nokes, Carmen Muñoz-Almagro, Michal Mandelboim, James W Keck, Joanne Marie Langley, Terho Heikkinen, Jikui Deng, Philippe Colson, Giorgi Chakhunashvili, Mauricio T. Caballero, Louis Bont, Daniel R Feikin, Harish Nair, Xin Wang, You Li for the Respiratory Virus Global Epidemiology Network

Corresponding author: 
You Li, You.Li@njmu.edu.cn 
Xin Wang, Xin.Wang@njmu.edu.cn

All the fitting, analysis and figure generation can be run from "main.R". This loads in all the dependencies and functions. It has to be run in the order specified, and opens any files where manual edits can be made (e.g. type of simulation outcome). All the code and data (which is publically available) are in the repo, and can be run directly from the master script. 

**IMPORTANT:**
- Please confirm that **Rtools** is installed on your computer, otherwise the code cannot run. 
- If errors occur during code execution, please try installing `glmmTMB`, `TMB` and `Matrix` from source, and ensure their versions match the information below.
- Please install the necessary packages at first.
- This analysis uses parallel computing. Please modify it according to the number of CPU cores you wish to use.
- In sensitivity analyses 4 and 5, if you want to save the correct `Matching_Recir.pdf` and `Matching_Peak.pdf` files, you need to run the scripts `Code/Sens4.R` and `Code/Sens5.R` line by line. If you run these two scripts using `source()` function in `Code/main.r`, you will get the same results, but the display of the figures will be different.



Input files: 
- All_Vir.RData: This file is the processed result of the `Full-text table-V1.8.1.xlsx` file, used for all analyses in this research.
- Location.RData: This file contains coordinate information for each research site. You can directly use the existing file. If you want to run the source code to obtain coordinate information, you need a Google Developer account and configure the Google map API.
- Data/Full-text table-V1.8.1.xlsx: All collected data from research paper, surveillance and RSV GEN.

Output files - see paper for references: 
- When the code runs, it will generate a file path named Output, and all running results will be saved to this path.


All analyses were completed on a MacBook with an M2 MAX chip and 64GB of memory. The session info was as follow:
```
R version 4.3.1 (2023-06-16)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 15.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] metafor_4.6-0       numDeriv_2016.8-1.1 metadat_1.2-0       glmmTMB_1.1.9       sjstats_0.19.0      merTools_0.6.2      arm_1.14-4         
 [8] lme4_1.1-35.5       Matrix_1.6-1.1      sandwich_3.1-0      lmtest_0.9-40       zoo_1.8-12          MASS_7.3-60         doParallel_1.0.17  
[15] iterators_1.0.14    foreach_1.5.2       tidygeocoder_1.0.5  mapdata_2.3.1       maps_3.4.2          data.table_1.16.0   lubridate_1.9.3    
[22] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4         purrr_1.0.2         readr_2.1.5         tidyr_1.3.1         tibble_3.2.1       
[29] ggplot2_3.5.1       tidyverse_2.0.0     finalfit_1.0.8      readxl_1.4.3       

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1    fastmap_1.2.0       mathjaxr_1.6-0      promises_1.3.0      digest_0.6.37       rpart_4.1.23        estimability_1.5.1 
 [8] mime_0.12           timechange_0.3.0    lifecycle_1.0.4     survival_3.7-0      magrittr_2.0.3      compiler_4.3.1      rlang_1.1.4        
[15] tools_4.3.1         utf8_1.2.4          abind_1.4-5         withr_3.0.1         datawizard_0.10.0   nnet_7.3-19         grid_4.3.1         
[22] fansi_1.0.6         jomo_2.7-6          xtable_1.8-4        colorspace_2.1-1    future_1.34.0       mice_3.16.0         emmeans_1.10.2     
[29] globals_0.16.3      scales_1.3.0        insight_0.20.3      cli_3.6.3           mvtnorm_1.2-6       generics_0.1.3      performance_0.12.2 
[36] rstudioapi_0.16.0   tzdb_0.4.0          minqa_1.2.8         splines_4.3.1       cellranger_1.1.0    vctrs_0.6.5         boot_1.3-30        
[43] glmnet_4.1-8        hms_1.1.3           mitml_0.4-5         listenv_0.9.1       glue_1.7.0          blme_1.0-5          parallelly_1.38.0  
[50] nloptr_2.1.1        pan_1.9             codetools_0.2-20    stringi_1.8.4       shape_1.4.6.1       gtable_0.3.5        later_1.3.2        
[57] broom.mixed_0.2.9.5 munsell_0.5.1       pillar_1.9.0        furrr_0.3.1         htmltools_0.5.8.1   TMB_1.9.15          R6_2.5.1           
[64] shiny_1.9.1         lattice_0.22-6      backports_1.5.0     broom_1.0.6         httpuv_1.6.15       Rcpp_1.0.13         coda_0.19-4.1      
[71] nlme_3.1-166        mgcv_1.9-1          pkgconfig_2.0.3             
```

See LICENSE file for licensing details.