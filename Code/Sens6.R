# Sensitivity analysis 6
# Filter data that report the outcome as proportion and as cases

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
FilePath.Sens1 <- CreateSubFolder(FilePath, "6.All proportion")
### All proportion -----------------------------------------------------------
SensDat1 <- All_Vir[Index_of_Wave != 0 & Virus_name %in% c("RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IAV", "IBV") &
  !Study_ID %in% c(
    "S29A", "S59B", "S62A", "S63A", "S90B", "S91B", "S93B", "S94B", "S96B", "S118B",
    "S119B", "S120B", "S121B", "S123B", "S124B", "S125B", "S126B", "S127B", "S128B", "S129B"
  )] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IAV", "IBV", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV"),
    Time_interval = as.integer(Time_interval),
    Peak_interval = as.integer(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  ) %>%
  setDT()

### All case -----------------------------------------------------------
SensDat2 <- All_Vir[Index_of_Wave != 0 & Virus_name %in% c("RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IAV", "IBV") &
  !Study_ID %in% c(
    "S29B", "S59A", "S62B", "S63B", "S90A", "S91A", "S93A", "S94A", "S96A", "S118A",
    "S119A", "S120A", "S121A", "S123A", "S124A", "S125A", "S126A", "S127A", "S128A", "S129A"
  )] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IAV", "IBV", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV"),
    Time_interval = as.integer(Time_interval),
    Peak_interval = as.integer(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  ) %>%
  setDT()

# 3. Virus-virus analysis -------------------------------------------------
#### All proportion -------------------------------------------------------
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

#### All case -------------------------------------------------------------
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


### Plotting -------------------------------------------------------------
### First wave -----------------------------------------------------------
# Recir
Sens1_MergeTable_Recir <- Merge.Sensitivity(Sens1_TwoVirReport_Recir, Sens1_TwoVirReport_Recir_Sec)
Sens1_Matching_Recir <- Matching.Plot(Sens1_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_Prop.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Recir)
dev.off()

# Peak
Sens1_MergeTable_Peak <- Merge.Sensitivity(Sens1_TwoVirReport_Peak, Sens1_TwoVirReport_Peak_Sec)
Sens1_Matching_Peak <- Matching.Plot(Sens1_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Prop.pdf"), width = 14, height = 14)
replayPlot(Sens1_Matching_Peak)
dev.off()


### Second wave -----------------------------------------------------------
# Recir
Sens2_MergeTable_Recir <- Merge.Sensitivity(Sens2_TwoVirReport_Recir, Sens2_TwoVirReport_Recir_Sec)
Sens2_Matching_Recir <- Matching.Plot(Sens2_MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Recir_Case.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Recir)
dev.off()

# Peak
Sens2_MergeTable_Peak <- Merge.Sensitivity(Sens2_TwoVirReport_Peak, Sens2_TwoVirReport_Peak_Sec)
Sens2_Matching_Peak <- Matching.Plot(Sens2_MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Peak_Case.pdf"), width = 14, height = 14)
replayPlot(Sens2_Matching_Peak)
dev.off()

stopCluster(cl)
