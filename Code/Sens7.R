# Sensitivity analysis 7
# Filter data from literature and from database

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
FilePath.Sens1 <- CreateSubFolder(FilePath, "7.From paper and database")
### From literature -----------------------------------------------------------
SensDat1 <- MainDat[Study_ID %in% c(
  "S03", "S07", "S19", "S29A", "S29B", "S30", "S38", "S49", "S50A", "S50B", "S50C", "S51", "S52", "S53", "S54", "S55",
  "S56", "S57", "S59A", "S94A", "S95", "S96A", "S98", "S99", "S101", "S103", "S104",
  "S106", "S107", "S108", "S109", "S111", "S113", "S114", "S115", "S116"
)]

### From database -----------------------------------------------------------
SensDat2 <- MainDat[Study_ID %in% c(
  "S61", "S62A", "S62B", "S63A", "S63B", "S64", "S65D", "S65F", "S65M", "S65O", "S65Q", "S65U", "S65V",
  "S66", "S67", "S68B", "S68C", "S68D", "S69A", "S69B", "S69D", "S70A", "S70B", "S70C", "S70D",
  "S70E", "S70K", "S70M", "S71C", "S71F", "S73", "S75", "S78A", "S80", "S81", "S83A", "S83B",
  "S84", "S85", "S86", "S87", "S117", "S88", "S89A", "S90A", "S91A", "S92A", "S93A", "S118A", "S119A", "S120A",
  "S121A", "S122", "S123A", "S124A", "S125A", "S126A", "S127A", "S128A", "S129A"
)]

# 3. Virus-virus analysis -------------------------------------------------
#### From literature ------------------------------------------------------
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

#### From database ------------------------------------------------------
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
pdf(paste0(FilePath.Sens1, "Matching_Recir_Paper.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Recir)
dev.off()

# Peak
Sens1_MergeTable_Peak <- Merge.Sensitivity(Sens1_TwoVirReport_Peak, Sens1_TwoVirReport_Peak_Sec)
Sens1_Matching_Peak <- Matching.Plot(Sens1_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Paper.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Peak)
dev.off()


### Second wave ---------------------------------------------------------
# Recir
Sens2_MergeTable_Recir <- Merge.Sensitivity(Sens2_TwoVirReport_Recir, Sens2_TwoVirReport_Recir_Sec)
Sens2_Matching_Recir <- Matching.Plot(Sens2_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_DB.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Recir)
dev.off()

# Peak
Sens2_MergeTable_Peak <- Merge.Sensitivity(Sens2_TwoVirReport_Peak, Sens2_TwoVirReport_Peak_Sec)
Sens2_Matching_Peak <- Matching.Plot(Sens2_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_DB.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Peak)
dev.off()

stopCluster(cl)
