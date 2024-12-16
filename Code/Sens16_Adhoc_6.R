# Ad hoc analysis
# Canada -- multiple levels

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

### Extra function -------------------------------------------------------
Calu.Canda <- function(dat, plot = T, save = F, path = NULL, width, height) {
  RecircDat <- dat[, .(num = 1, mean = mean(Time_interval, na.rm = T), sd = NA), keyby = .(Virus_name, Index_of_Wave)]

  BeforePandemic <- RecircDat[Index_of_Wave < 0]
  BeforePandemic <- BeforePandemic[, .SD[order(-Index_of_Wave)], keyby = .(Virus_name)][, c("mean", "sd") := .(shift(mean, n = 1L, fill = 0), shift(sd, n = 1L, fill = 0)),
    keyby = .(Virus_name)
  ][, c("mean", "sd") := .(-mean, -sd)][, c("lci", "uci") := .(0, 0)][, ci := 0, keyby = .(Virus_name)][, Cumday := round(cumsum(as.numeric(mean)), 2), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday, 0), round(Cumday, 0)), keyby = .(Virus_name)]

  AfterPandemic <- RecircDat[Index_of_Wave > 0]
  AfterPandemic <- AfterPandemic[, c("lci", "uci", "ci") := .(0, 0, NA)][, Cumday := cumsum(as.numeric(mean)), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday, 0), round(Cumday, 2))]

  RecircDat <- bind_rows(BeforePandemic, AfterPandemic)
  RecircDat <- RecircDat[order(Virus_name, Index_of_Wave)]

  Calu.SingleVir.plot(RecircDat, target = "Time_interval", save, path, width = width, height = height)

  return(RecircDat)
}

Calu.Canda.Peak <- function(dat, plot = T, save = F, path = NULL, width, height) {
  RecircDat <- dat[, .(num = 1, mean = mean(Peak_interval, na.rm = T), sd = NA), keyby = .(Virus_name, Index_of_Wave)]

  BeforePandemic <- RecircDat[Index_of_Wave < 0]
  BeforePandemic <- BeforePandemic[, .SD[order(-Index_of_Wave)], keyby = .(Virus_name)][, c("mean", "sd") := .(shift(mean, n = 1L, fill = 0), shift(sd, n = 1L, fill = 0)),
    keyby = .(Virus_name)
  ][, c("mean", "sd") := .(-mean, -sd)][, c("lci", "uci") := .(0, 0)][, ci := 0, keyby = .(Virus_name)][, Cumday := round(cumsum(as.numeric(mean)), 2), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday, 0), round(Cumday, 0)), keyby = .(Virus_name)]

  AfterPandemic <- RecircDat[Index_of_Wave > 0]
  AfterPandemic <- AfterPandemic[, c("lci", "uci", "ci") := .(0, 0, NA)][, Cumday := cumsum(as.numeric(mean)), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday, 0), round(Cumday, 2))]

  RecircDat <- bind_rows(BeforePandemic, AfterPandemic)
  RecircDat <- RecircDat[order(Virus_name, Index_of_Wave)]

  Calu.SingleVir.plot(RecircDat, target = "Peak_interval", save, path, width = width, height = height)

  return(RecircDat)
}

ReportCanada <- function(dat, index = c("peak", "last"),
                         save = F, path, width = 12, height = 10) {
  target <- switch(index,
    "last" = "Vir_VirTimeInterval",
    "peak" = "Vir_VirPeakInterval"
  )

  Canda.cal <- function(dat, target) {
    getmean <- dat[, ..target][[1]]
    Table <- data.frame(
      FirstVir = dat$FirstVir[1],
      SecVir = dat$SecVir[1],
      num = dat$num[1],
      mean = getmean,
      se = 0,
      lci = getmean,
      uci = getmean
    )
    return(Table)
  }

  dat <- setDT(dat)
  Report2Vir <- dat[, Vir_ID := paste0(FirstVir, "_", SecVir)]
  Report2Vir$Vir_VirTimeInterval <- as.numeric(Report2Vir$Vir_VirTimeInterval)
  Report2Vir$Vir_VirPeakInterval <- as.numeric(Report2Vir$Vir_VirPeakInterval)
  Report2Vir <- Report2Vir[, num := .N, by = "Vir_ID"]
  Report2Vir_split <- split(Report2Vir, by = "Vir_ID")
  Report2Vir <- do.call(rbind, lapply(Report2Vir_split, Canda.cal, target = target))
  Report2Vir <- Report2Vir[order(-Report2Vir$mean), ]

  VirOrder <- unique(Report2Vir$FirstVir)
  Report2Vir <- Report2Vir %>%
    mutate(
      across(ends_with("Vir"), as.factor),
      across(ends_with("Vir"), ~ fct_relevel(., VirOrder)),
      across(c(3:7), as.numeric),
      across(c(3:7), ~ round(., 0))
    )

  Report2Vir <- Report2Vir %>%
    mutate(
      FirstVir = factor(FirstVir, levels = VirOrder),
      SecVir = factor(SecVir, levels = VirOrder)
    ) %>%
    filter(as.numeric(FirstVir) < as.numeric(SecVir))

  return(Report2Vir)
}
# 1. Plot epidemic character and world map --------------------------------
FilePath.Sens1 <- CreateSubFolder(FilePath, "Geographic")

### geographic difference
Geographic <- All_Vir[Country == "Canada" &
  Index_of_Wave != 0 & Index_of_Wave <= 2 &
  Virus_name %in% c("RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IAV", "IBV") &
  !Study_ID %in% c("S121A", "S121B")] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IAV", "IBV", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV")
  ) %>%
  mutate(
    Time_interval = as.numeric(Time_interval),
    Peak_interval = as.numeric(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  )

Geographic_Canada <- Geographic[Study_ID == "S74"]
Geographic_Province <- Geographic[Study_ID %in% c("S68A", "S69C", "S70L", "S71E", "S73")]
Geographic_City <- Geographic[Study_ID %in% c(
  "S68B", "S68C", "S68D", "S69A", "S69B", "S69D",
  "S70A", "S70B", "S70C", "S70D", "S70E", "S70K",
  "S70M", "S71C", "S71F"
)]

Calu.Canda(Geographic_Canada,
  plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Canada.pdf"), width = 12, height = 12
)
Calu.SingleVir(Geographic_Province,
  target = "Time_interval", func = "Liner", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Province.pdf"), width = 12, height = 12, report = FALSE
)
Calu.SingleVir(Geographic_City,
  target = "Time_interval", func = "Liner", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_City.pdf"), width = 12, height = 12, report = FALSE
)


Calu.Canda.Peak(Geographic_Canada,
  plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Canada_peak.pdf"), width = 12, height = 12
)
Calu.SingleVir(Geographic_Province,
  target = "Peak_interval", func = "Liner", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_Province_peak.pdf"), width = 12, height = 12, report = FALSE
)
Calu.SingleVir(Geographic_City,
  target = "Peak_interval", func = "Liner", plot = TRUE, save = TRUE,
  path = paste0(FilePath.Sens1, "Pooling_City_peak.pdf"), width = 12, height = 12, report = FALSE
)


##########################################################################
### Country level -------------------------------------------------------
##########################################################################
TwoVirTable <- foreach(
  df = split(Geographic_Canada[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

TwoVirReport_Recir <- ReportCanada(dat = TwoVirTable, index = "last")
TwoVirReport_Peak <- ReportCanada(dat = TwoVirTable, index = "peak")

### Second wave -----------------------------------------------------------
MainDat_Sec <- Geographic_Canada[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
TwoVirTable_Sec <- foreach(
  df = split(MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

TwoVirReport_Recir_Sec <- ReportCanada(dat = TwoVirTable_Sec, index = "last")
TwoVirReport_Peak_Sec <- ReportCanada(dat = TwoVirTable_Sec, index = "peak")


### Plotting --------------------------------------------------------------
# Recir
MergeTable_Recir <- Merge.Sensitivity(TwoVirReport_Recir, TwoVirReport_Recir_Sec)
MergeTable_Recir <- MergeTable_Recir %>% arrange(-mean)

Matching_Recir <- Matching.Plot(MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Country.pdf"), width = 14, height = 14)
replayPlot(Matching_Recir)
dev.off()

# Peak
MergeTable_Peak <- Merge.Sensitivity(TwoVirReport_Peak, TwoVirReport_Peak_Sec)
MergeTable_Peak <- MergeTable_Peak %>% arrange(-mean)

Matching_Peak <- Matching.Plot(MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Country_Peak.pdf"), width = 14, height = 14)
replayPlot(Matching_Peak)
dev.off()

##########################################################################
### Province level -------------------------------------------------------
##########################################################################
TwoVirTable <- foreach(
  df = split(Geographic_Province[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

TwoVirReport_Recir <- ReportTwoVir(dat = TwoVirTable, index = "last")
TwoVirReport_Peak <- ReportTwoVir(dat = TwoVirTable, index = "peak")

### Second wave -----------------------------------------------------------
MainDat_Sec <- Geographic_Province[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
TwoVirTable_Sec <- foreach(
  df = split(MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

TwoVirReport_Recir_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "last")
TwoVirReport_Peak_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "peak")


### Plotting --------------------------------------------------------------
# Recir
MergeTable_Recir <- Merge.Sensitivity(TwoVirReport_Recir, TwoVirReport_Recir_Sec)
MergeTable_Recir <- MergeTable_Recir %>% arrange(-mean)

Matching_Recir <- Matching.Plot(MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_Province.pdf"), width = 14, height = 14)
replayPlot(Matching_Recir)
dev.off()

# Peak
MergeTable_Peak <- Merge.Sensitivity(TwoVirReport_Peak, TwoVirReport_Peak_Sec)
MergeTable_Peak <- MergeTable_Peak %>% arrange(-mean)

Matching_Peak <- Matching.Plot(MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_Province_Peak.pdf"), width = 14, height = 14)
replayPlot(Matching_Peak)
dev.off()

##########################################################################
### City level -----------------------------------------------------------
##########################################################################
TwoVirTable <- foreach(
  df = split(Geographic_City[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

TwoVirReport_Recir <- ReportTwoVir(dat = TwoVirTable, index = "last")
TwoVirReport_Peak <- ReportTwoVir(dat = TwoVirTable, index = "peak")

### Second wave -----------------------------------------------------------
MainDat_Sec <- Geographic_City[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
TwoVirTable_Sec <- foreach(
  df = split(MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

TwoVirReport_Recir_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "last")
TwoVirReport_Peak_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "peak")


### Plotting --------------------------------------------------------------
# Recir
MergeTable_Recir <- Merge.Sensitivity(TwoVirReport_Recir, TwoVirReport_Recir_Sec)
MergeTable_Recir <- MergeTable_Recir %>% arrange(-mean)

Matching_Recir <- Matching.Plot(MergeTable_Recir)
pdf(paste0(FilePath.Sens1, "Matching_City.pdf"), width = 14, height = 14)
replayPlot(Matching_Recir)
dev.off()

# Peak
MergeTable_Peak <- Merge.Sensitivity(TwoVirReport_Peak, TwoVirReport_Peak_Sec)
MergeTable_Peak <- MergeTable_Peak %>% arrange(-mean)

Matching_Peak <- Matching.Plot(MergeTable_Peak)
pdf(paste0(FilePath.Sens1, "Matching_City_Peak.pdf"), width = 14, height = 14)
replayPlot(Matching_Peak)
dev.off()

stopCluster(cl)
