# Sensitivity analysis 2
# Filter data that only from high-income countries and from none high-income countries (UM_LM_L)

suppressWarnings(rm(
  cl, ColIndex, MergedDat, FilePath.Sens1, MainAnalysis_Peak_REM, MainAnalysis_Recir_REM, Sens1_MainDat_Sec,
  NewDat, OldWave, PercentIncrease, Sens1_MainAnalysis_Peak_REM, Sens2_MainAnalysis_Peak_REM,
  Sens1_MainAnalysis_Recir_REM, Sens1_MainAnalysis_Recir_REM, Sens1_recir, SensDat1, SensDat2, Sens1_TwoVirReport_Peak,
  Sens1_TwoVirReport_Peak_Sec, Sens1_TwoVirReport_Recir, Sens1_TwoVirReport_Recir_Sec, Sens1_TwoVirTable, Sens1_TwoVirTable_Sec,
  Sens2_TwoVirTable, Sens2_TwoVirReport_Recir, Sens2_TwoVirReport_Peak, Sens2_MainDat_Sec, Sens2_TwoVirTable_Sec,
  Sens2_TwoVirReport_Recir_Sec, Sens2_TwoVirReport_Peak_Sec, Sens1_MergeTable_Recir, Sens2_MergeTable_Recir,
  Sens1_Matching_Recir, Sens2_Matching_Recir, Sens1_MergeTable_Peak, Sens2_MergeTable_Peak, Sens1_Matching_Peak,
  Sens2_Matching_Peak
))

set.seed(971889)
cl <- makeCluster(10)
registerDoParallel(cl)

# 2. Single virus analysis
FilePath.Sens1 <- CreateSubFolder(FilePath, "2.From High-income")
### High-income countries -----------------------------------------------------------
SensDat1 <- MainDat[Class == "H"]

### None high-income countries -----------------------------------------------------------
SensDat2 <- MainDat[Class != "H"]

# 3. Virus-virus analysis -------------------------------------------------
#### High-income countries ------------------------------------------------
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

fwrite(Sens1_TwoVirReport_Recir, paste0(FilePath.Sens1, "Two_recir_Part1.csv"), row.names = FALSE)
fwrite(Sens1_TwoVirReport_Peak, paste0(FilePath.Sens1, "Two_peak_Part1.csv"), row.names = FALSE)

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

fwrite(Sens1_TwoVirReport_Recir_Sec, paste0(FilePath.Sens1, "Two_recir_Sec_Part1.csv"), row.names = FALSE)
fwrite(Sens1_TwoVirReport_Peak_Sec, paste0(FilePath.Sens1, "Two_peak_Sec_Part1.csv"), row.names = FALSE)

#### None high-income countries ------------------------------------------------
#### First wave -----------------------------------------------------------
Sens2_TwoVirTable <- foreach(
  df = split(SensDat2[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

Sens2_TwoVirReport_Recir <- ReportTwoVir(dat = Sens2_TwoVirTable, index = "last")
Sens2_TwoVirReport_Peak <- ReportTwoVir(dat = Sens2_TwoVirTable, index = "peak")

fwrite(Sens2_TwoVirReport_Recir, paste0(FilePath.Sens1, "Two_recir_Part2.csv"), row.names = FALSE)
fwrite(Sens2_TwoVirReport_Peak, paste0(FilePath.Sens1, "Two_peak_Part2.csv"), row.names = FALSE)

### Second wave -----------------------------------------------------------
Sens2_MainDat_Sec <- SensDat2[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
Sens2_TwoVirTable_Sec <- foreach(
  df = split(Sens2_MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

Sens2_TwoVirReport_Recir_Sec <- ReportTwoVir(dat = Sens2_TwoVirTable_Sec, index = "last")
Sens2_TwoVirReport_Peak_Sec <- ReportTwoVir(dat = Sens2_TwoVirTable_Sec, index = "peak")

fwrite(Sens2_TwoVirReport_Recir_Sec, paste0(FilePath.Sens1, "Two_recir_Sec_Part2.csv"), row.names = FALSE)
fwrite(Sens2_TwoVirReport_Peak_Sec, paste0(FilePath.Sens1, "Two_peak_Sec_Part2.csv"), row.names = FALSE)


### Plotting -----------------------------------------------------------
### First wave ---------------------------------------------------------
# Recir
Sens1_MergeTable_Recir <- Merge.Sensitivity(Sens1_TwoVirReport_Recir, Sens1_TwoVirReport_Recir_Sec)
Sens1_Matching_Recir <- Matching.Plot(Sens1_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_High.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Recir)
dev.off()

# Peak
Sens1_MergeTable_Peak <- Merge.Sensitivity(Sens1_TwoVirReport_Peak, Sens1_TwoVirReport_Peak_Sec)
Sens1_Matching_Peak <- Matching.Plot(Sens1_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_High.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Peak)
dev.off()


### Second wave ---------------------------------------------------------
# Recir
Sens2_MergeTable_Recir <- Merge.Sensitivity(Sens2_TwoVirReport_Recir, Sens2_TwoVirReport_Recir_Sec)
Sens2_Matching_Recir <- Matching.Plot(Sens2_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_Low.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Recir)
dev.off()

# Peak
Sens2_MergeTable_Peak <- Merge.Sensitivity(Sens2_TwoVirReport_Peak, Sens2_TwoVirReport_Peak_Sec)
Sens2_Matching_Peak <- Matching.Plot(Sens2_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Low.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Peak)
dev.off()

stopCluster(cl)
