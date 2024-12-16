# Sensitivity analysis 1
# Filter data that only in the temperate zone (including the southern and northern hemisphere) and the tropical zone

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
FilePath.Sens1 <- CreateSubFolder(FilePath, "1.Temperate climate")
### Temperate region -----------------------------------------------------------
SensDat1 <- MainDat[(lat >= 23.5 & lat <= 66.5) | (lat <= -23.5 & lat >= -66.5)]

Sens1_MainAnalysis_Recir_REM <- Calu.SingleVir(SensDat1,
  target = "Time_interval", func = "REM", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Recir_Part1.pdf"), width = 12, height = 12, report = FALSE
)
Sens1_recir <- copy(Sens1_MainAnalysis_Recir_REM) # For South hemisphere calculation

Sens1_MainAnalysis_Peak_REM <- Calu.SingleVir(SensDat1,
  target = "Peak_interval", func = "REM", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Peak_Part1.pdf"), width = 12, height = 12, report = FALSE
)

fwrite(Sens1_MainAnalysis_Recir_REM, paste0(FilePath.Sens1, "Pooling_Recir_Part1.csv"), row.names = FALSE)
fwrite(Sens1_MainAnalysis_Peak_REM, paste0(FilePath.Sens1, "Pooling_Peak_Part1.csv"), row.names = FALSE)

### Tropical region -----------------------------------------------------------
SensDat2 <- MainDat[lat <= 23.5 & lat >= -23.5]

Sens2_MainAnalysis_Recir_REM <- Calu.SingleVir(SensDat2,
  target = "Time_interval", func = "REM", plot = FALSE, save = FALSE, report = FALSE
)
ColIndex <- Sens1_recir[, 1:2]
MergedDat <- merge(ColIndex, Sens2_MainAnalysis_Recir_REM, all.x = TRUE)
Calu.SingleVir.plot(MergedDat,
  target = "Time_interval", save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Recir_Part2.pdf"), width = 12, height = 12
)

Sens2_MainAnalysis_Peak_REM <- Calu.SingleVir(SensDat2,
  target = "Peak_interval", func = "REM", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Peak_Part2.pdf"), width = 12, height = 12, report = FALSE
)

fwrite(Sens2_MainAnalysis_Recir_REM, paste0(FilePath.Sens1, "Pooling_Recir_Part2.csv"), row.names = FALSE)
fwrite(Sens2_MainAnalysis_Peak_REM, paste0(FilePath.Sens1, "Pooling_Peak_Part2.csv"), row.names = FALSE)

NewDat <- copy(SensDat2)
PercentIncrease <- Calu.Percent(NewDat, target = "Time_interval")
fwrite(PercentIncrease, paste0(FilePath.Sens1, "PercentIncrease_Part2.csv"), row.names = FALSE)
fwrite(OldWave, paste0(FilePath.Sens1, "OldWave_Part2.csv"), row.names = FALSE)


# 3. Virus-virus analysis -------------------------------------------------
#### Temperate region -----------------------------------------------------
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

#### Tropical region -----------------------------------------------------------
#### First wave -----------------------------------------------------------
Sens2_TwoVirTable <- foreach(
  df = split(SensDat2[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
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


### Ploting --------------------------------------------------------------
### First wave -----------------------------------------------------------
# Recir
Sens1_MergeTable_Recir <- Merge.Sensitivity(Sens1_TwoVirReport_Recir, Sens1_TwoVirReport_Recir_Sec)
Sens1_Matching_Recir <- Matching.Plot(Sens1_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_Temp.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Recir)
dev.off()

# Peak
Sens1_MergeTable_Peak <- Merge.Sensitivity(Sens1_TwoVirReport_Peak, Sens1_TwoVirReport_Peak_Sec)
Sens1_Matching_Peak <- Matching.Plot(Sens1_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Temp.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Peak)
dev.off()


### Second wave -----------------------------------------------------------
# Recir
Sens2_MergeTable_Recir <- Merge.Sensitivity(Sens2_TwoVirReport_Recir, Sens2_TwoVirReport_Recir_Sec)
Sens2_Matching_Recir <- Matching.Plot(Sens2_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_Trop.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Recir)
dev.off()

# Peak
Sens2_MergeTable_Peak <- Merge.Sensitivity(Sens2_TwoVirReport_Peak, Sens2_TwoVirReport_Peak_Sec)
Sens2_Matching_Peak <- Matching.Plot(Sens2_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Trop.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Peak)
dev.off()

stopCluster(cl)
