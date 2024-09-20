# Ad hoc analysis
# In this analysis, we will focus on the following databases:
# c("S61", "S62A", "S63A", "S64", "S74", "S117")
# NREVSS, Seattle, Houston, Korea, Canada national, Scotland (except RV, AdV)


c("S61", "S62A", "S63A", "S64", "S74", "S117")



# MainDat, MainDat_IFV,
suppressWarnings(rm(
  MainAnalysis_Recir_REM, MainAnalysis_Peak_REM, MainAnalysis_Recir_IFV,
  MainAnalysis_Peak_IFV, TwoVirTable, TwoVirTable_Main, IFV_split, TwoVirTable_IFV, TwoVirTable_Main_IFV,
  TwoVirReport_Recir, TwoVirReport_Peak, TwoVirReport_Recir_IFV, TwoVirReport_Peak_IFV,
  SensDat1, SensDat1_IFV,
  Sens1_MainAnalysis_Recir_REM, Sens1_MainAnalysis_Recir_IFV, Sens1_MainAnalysis_Peak_REM, Sens1_MainAnalysis_Peak_IFV,
  cl, FilePath.Sens1
))



set.seed(971889)


# 2. Single virus analysis

FilePath.Sens1 <- CreateSubFolder(FilePath, "Ad hoc")

SensDat1 <- All_Vir[Study_ID %in% c("S61", "S62A", "S63A", "S64", "S74", "S117") &
  Index_of_Wave != 0 &
  Virus_name %in% c("RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IFVA", "IFVB")] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IFVA", "IFVB", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV")
  ) %>%
  mutate(
    Time_interval = as.numeric(Time_interval),
    Peak_interval = as.numeric(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  )


SensDat1 <- SensDat1[!(Study_ID == "S117" & Virus_name %in% c("RV", "AdV"))]



Sens1_MainAnalysis_Recir_REM <- Calu.SingleVir(SensDat1,
  target = "Time_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.Sens1, "MainAnalysis_Recir.pdf"), width = 16, height = 16, report = F
)

Sens1_MainAnalysis_Peak_REM <- Calu.SingleVir(SensDat1,
  target = "Peak_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.Sens1, "MainAnalysis_Peak.pdf"), width = 16, height = 16, report = F
)

fwrite(Sens1_MainAnalysis_Recir_REM, paste0(FilePath.Sens1, "Main_recir.csv"), row.names = F)
fwrite(Sens1_MainAnalysis_Peak_REM, paste0(FilePath.Sens1, "Main_peak.csv"), row.names = F)
