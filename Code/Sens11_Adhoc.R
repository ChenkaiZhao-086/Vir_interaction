# Ad hoc analysis
# More than 1000 cases for each virus


suppressWarnings(rm(
  MainAnalysis_Recir_REM, MainAnalysis_Peak_REM, MainAnalysis_Recir_IFV,
  MainAnalysis_Peak_IFV, TwoVirTable, TwoVirTable_Main, IFV_split, TwoVirTable_IFV, TwoVirTable_Main_IFV,
  TwoVirReport_Recir, TwoVirReport_Peak, TwoVirReport_Recir_IFV, TwoVirReport_Peak_IFV,
  SensDat1, SensDat1_IFV,
  Sens1_MainAnalysis_Recir_REM, Sens1_MainAnalysis_Recir_IFV, Sens1_MainAnalysis_Peak_REM, Sens1_MainAnalysis_Peak_IFV,
  cl, FilePath.Sens1, NewDat, PercentIncrease,
  MainDat_Sec, TwoVirTable_Sec, TwoVirReport_Recir_Sec, TwoVirReport_Peak_Sec, TwoVirTable_Diff,
  TwoVirReport_Recir_Diff, TwoVirReport_Peak_Diff
))

set.seed(971889)
cl <- makeCluster(10)
registerDoParallel(cl)

# 2. Single virus analysis

FilePath.Sens1 <- CreateSubFolder(FilePath, "Ad hoc")

SensDat1 <- All_Vir[Study_ID %in% c("S61", "S62A", "S63A", "S64", "S74", "S117", "S122", "S125B") &
  Index_of_Wave != 0 &
  Virus_name %in% c("RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IAV", "IBV")] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IAV", "IBV", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV")
  ) %>%
  mutate(
    Time_interval = as.numeric(Time_interval),
    Peak_interval = as.numeric(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  )

SensDat1 <- SensDat1[!(Study_ID == "S117" & Virus_name %in% c("RV", "AdV"))]

# Sens1_MainAnalysis_Recir_REM <- Calu.SingleVir(SensDat1,
#   target = "Time_interval", func = "REM", plot = TRUE, save = TRUE,
#   path = paste0(FilePath.Sens1, "Pooling_Recir_Part1.pdf"), width = 12, height = 12, report = FALSE
# )

# Sens1_MainAnalysis_Peak_REM <- Calu.SingleVir(SensDat1,
#   target = "Peak_interval", func = "REM", plot = TRUE, save = TRUE,
#   path = paste0(FilePath.Sens1, "Pooling_Peak_Part1.pdf"), width = 12, height = 12, report = FALSE
# )

# fwrite(Sens1_MainAnalysis_Recir_REM, paste0(FilePath.Sens1, "Pooling_Recir_Part1.csv"), row.names = FALSE)
# fwrite(Sens1_MainAnalysis_Peak_REM, paste0(FilePath.Sens1, "Pooling_Peak_Part1.csv"), row.names = FALSE)


# NewDat <- copy(SensDat1)
# PercentIncrease <- Calu.Percent(NewDat, target = "Time_interval")
# fwrite(PercentIncrease, paste0(FilePath.Sens1, "PercentIncrease_Part1.csv"), row.names = FALSE)
# fwrite(OldWave, paste0(FilePath.Sens1, "OldWave_Part1.csv"), row.names = FALSE)


# 3. Virus-virus analysis -------------------------------------------------
#### First wave -----------------------------------------------------------
Sens1_TwoVirTable <- foreach(
  df = split(SensDat1[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

Sens1_TwoVirReport_Recir <- ReportTwoVir(dat = Sens1_TwoVirTable, index = "last")
Sens1_TwoVirReport_Peak <- ReportTwoVir(dat = Sens1_TwoVirTable, index = "peak")

fwrite(Sens1_TwoVirReport_Recir, paste0(FilePath.Sens1, "Two_recir.csv"), row.names = FALSE)
fwrite(Sens1_TwoVirReport_Peak, paste0(FilePath.Sens1, "Two_peak.csv"), row.names = FALSE)

### Second wave -----------------------------------------------------------
Sens1_MainDat_Sec <- SensDat1[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
Sens1_TwoVirTable_Sec <- foreach(
  df = split(Sens1_MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

Sens1_TwoVirReport_Recir_Sec <- ReportTwoVir(dat = Sens1_TwoVirTable_Sec, index = "last")
Sens1_TwoVirReport_Peak_Sec <- ReportTwoVir(dat = Sens1_TwoVirTable_Sec, index = "peak")

fwrite(Sens1_TwoVirReport_Recir_Sec, paste0(FilePath.Sens1, "Two_recir_Sec.csv"), row.names = FALSE)
fwrite(Sens1_TwoVirReport_Peak_Sec, paste0(FilePath.Sens1, "Two_peak_Sec.csv"), row.names = FALSE)

### Plotting -----------------------------------------------------------
# Recir
Sens1_MergeTable_Recir <- Merge.Sensitivity(Sens1_TwoVirReport_Recir, Sens1_TwoVirReport_Recir_Sec)
Sens1_Matching_Recir <- Matching.Plot(Sens1_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Recir)
dev.off()

# Peak
Sens1_MergeTable_Peak <- Merge.Sensitivity(Sens1_TwoVirReport_Peak, Sens1_TwoVirReport_Peak_Sec)
Sens1_Matching_Peak <- Matching.Plot(Sens1_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Peak)
dev.off()

stopCluster(cl)
