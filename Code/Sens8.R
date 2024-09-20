# Sensitivity analysis 8
# Filter out the studies that only from database

suppressWarnings(rm(
  MainAnalysis_Recir_REM, MainAnalysis_Peak_REM, MainAnalysis_Recir_IFV,
  MainAnalysis_Peak_IFV, TwoVirTable, TwoVirTable_Main, IFV_split, TwoVirTable_IFV, TwoVirTable_Main_IFV,
  TwoVirReport_Recir, TwoVirReport_Peak, TwoVirReport_Recir_IFV, TwoVirReport_Peak_IFV,
  SensDat1, SensDat1_IFV,
  Sens1_MainAnalysis_Recir_REM, Sens1_MainAnalysis_Recir_IFV, Sens1_MainAnalysis_Peak_REM, Sens1_MainAnalysis_Peak_IFV,
  cl, FilePath.Sens1, NewDat, PercentIncrease
))

set.seed(971889)


cl <- makeCluster(10)
registerDoParallel(cl)

# 2. Single virus analysis

FilePath.Sens1 <- CreateSubFolder(FilePath, "8.From DB")

SensDat1 <- MainDat[Study_ID %in% c(
  "S61", "S62A", "S62B", "S63A", "S63B", "S64", "S65A", "S65D", "S65F", "S65M", "S65O", "S65Q", "S65U", "S65V",
  "S66", "S67", "S68A", "S68B", "S68C", "S68D", "S69A", "S69B", "S69C", "S69D", "S70A", "S70B", "S70C", "S70D",
  "S70E", "S70K", "S70L", "S70M", "S71C", "S71E", "S71F", "S73", "S74", "S75", "S78A", "S80", "S81", "S83A", "S83B",
  "S84", "S85", "S86", "S87", "S117", "S88", "S89A", "S90A", "S91A", "S92A", "S93A", "S118A", "S119A", "S120A",
  "S121A", "S122", "S123A", "S124A", "S125A", "S126A", "S127A", "S128A", "S129A"
)]


Sens1_MainAnalysis_Recir_REM <- Calu.SingleVir(SensDat1,
  target = "Time_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.Sens1, "MainAnalysis_Recir.pdf"), width = 16, height = 20, report = F
)

Sens1_MainAnalysis_Peak_REM <- Calu.SingleVir(SensDat1,
  target = "Peak_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.Sens1, "MainAnalysis_Peak.pdf"), width = 16, height = 20, report = F
)

fwrite(Sens1_MainAnalysis_Recir_REM, paste0(FilePath.Sens1, "Main_recir.csv"), row.names = F)
fwrite(Sens1_MainAnalysis_Peak_REM, paste0(FilePath.Sens1, "Main_peak.csv"), row.names = F)



# 3. Virus-virus analysis -

TwoVirTable <- foreach(
  df = split(SensDat1[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, NPIRef = "last")
}
TwoVirTable_Main <- copy(TwoVirTable)


TwoVirReport_Recir <- ReportTwoVir(
  dat = TwoVirTable, index = "last", tidy = FALSE, func = "Liner",
  save = TRUE, path = paste0(FilePath.Sens1, "TwoVirRecir_RefLast.pdf"), width = 12, height = 10
)
TwoVirReport_Peak <- ReportTwoVir(
  dat = TwoVirTable, index = "peak", tidy = FALSE, save = TRUE,
  path = paste0(FilePath.Sens1, "TwoVirPeak_RefLast.pdf"), width = 12, height = 10
)
fwrite(TwoVirReport_Recir, paste0(FilePath.Sens1, "Two_recir.csv"), row.names = F)
fwrite(TwoVirReport_Peak, paste0(FilePath.Sens1, "Two_peak.csv"), row.names = F)

TwoVirReport_Recir %>%
  as_tibble() %>%
  mutate(
    ID = paste0(FirstVir, "-", SecVir),
    FirstVir = as.character(FirstVir)
  ) %>%
  filter(mean >= 0) %>%
  group_nest(FirstVir) %>%
  mutate(fig = purrr::map(data, plot.TwoVir, path = paste0(FilePath.Sens1, "recir"), width = 12, height = 10))

TwoVirReport_Peak %>%
  as_tibble() %>%
  mutate(
    ID = paste0(FirstVir, "-", SecVir),
    FirstVir = as.character(FirstVir)
  ) %>%
  filter(mean >= 0) %>%
  group_nest(FirstVir) %>%
  mutate(fig = purrr::map(data, plot.TwoVir, path = paste0(FilePath.Sens1, "peak"), width = 12, height = 10))


stopCluster(cl)
