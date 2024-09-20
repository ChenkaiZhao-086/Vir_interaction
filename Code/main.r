# rm(list=ls())

packages <- c(
  "readxl", "finalfit", "tidyverse", "data.table", "mapdata", "tidygeocoder", "foreach", "doParallel", "MASS",
  "lmtest", "sandwich", "lme4", "merTools", "sjstats", "glmmTMB", "metafor"
)

for (i in packages) {
  suppressPackageStartupMessages(library(i, character.only = TRUE, quietly = TRUE))
}


source("Code/func.R")

# 0. Data import ----------------------------------------------------------
# source("Code/DataPrepare.R")
### This RData is extracted data with longitude and latitude
load("~/Documents/600_Project/610_System_review/All_Vir.RData")

FilePath <- CreateMainFolder(path = "Output/", FolderName = "FinalLap") # CreateMainFolder(path = "Output/", Date = TRUE)


set.seed(971889)

cl <- makeCluster(4)
registerDoParallel(cl)

# 1. Plot epidemic character and world map --------------------------------

FilePath.sec1 <- CreateSubFolder(FilePath, "0.Check")

Vir <- All_Vir %>%
  filter(Virus_name %in% c(
    "IV", "RSV", "PIV", "MPV", "sCoV", "RV", "AdV", "IAV", "IBV",
    "H1N1", "H3N2", "PIV_1", "PIV_2", "PIV_3", "PIV_4"
  )) %>%
  filter(keep == T) %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(
      Virus_name, "IV", "IAV", "H1N1", "H3N2", "IBV", "RSV",
      "PIV", "PIV_1", "PIV_2", "PIV_3", "PIV_4", "MPV", "sCoV", "RV", "AdV"
    )
  )

foreach(
  df = split(Vir, by = "Study_ID"),
  .packages = packages
) %dopar% {
  plot.CountryTrend(VirData = df, save = T, path = FilePath.sec1, multiply = 6, width = 16, height = 8)
}

### Plot research location

ResLoca <- copy(All_Vir)
ResLoca <- ResLoca %>%
  as.data.frame() %>%
  dplyr::select(Study_ID, Country, Location, lat, long) %>%
  distinct(Study_ID, Country, Location, .keep_all = T)
SysRev_ID <- c(
  "S03", "S07", "S19", "S29A", "S29B", "S30", "S38", "S49", "S50A", "S50B", "S50C", "S51", "S52", "S53", "S54", "S55",
  "S56", "S57", "S59A", "S59B", "S94A", "S94B", "S95", "S96A", "S96B", "S98", "S99", "S101", "S103", "S104",
  "S106", "S107", "S108", "S109", "S111", "S113", "S114", "S115", "S116"
)
Surve_ID <- c(
  "S61", "S62A", "S62B", "S63A", "S63B", "S64", "S65A", "S65D", "S65F", "S65M", "S65O", "S65Q", "S65U", "S65V",
  "S66", "S67", "S68A", "S68B", "S68C", "S68D", "S69A", "S69B", "S69C", "S69D", "S70A", "S70B", "S70C", "S70D",
  "S70E", "S70K", "S70L", "S70M", "S71C", "S71E", "S71F", "S73", "S74", "S75", "S78A", "S80", "S81", "S83A", "S83B",
  "S84", "S85", "S86", "S87", "S117", "S88"
)
RSVGEN_ID <- c(
  "S89A", "S89B", "S90A", "S90B", "S91A", "S91B", "S92A", "S92B", "S93A", "S93B",
  "S118A", "S118B", "S119A", "S119B", "S120A", "S120B", "S121A", "S121B", "S122",
  "S123A", "S123B", "S124A", "S124B", "S125A", "S125B", "S126A", "S126B", "S127A",
  "S127B", "S128A", "S128B", "S129A", "S129B"
)

ResLoca[ResLoca$Study_ID %in% SysRev_ID, "Label"] <- "Systematic literature search"
ResLoca[ResLoca$Study_ID %in% Surve_ID, "Label"] <- "Surveillance report"
ResLoca[ResLoca$Study_ID %in% RSVGEN_ID, "Label"] <- "RSV GEN"
ResLoca <- ResLoca %>%
  mutate(
    Lebel = as_factor(Label),
    Label = fct_relevel(Label, "Systematic literature search", "Surveillance report", "RSV GEN")
  )

ResLoca <- unique(as.data.table(ResLoca), by = c("Country", "Location", "Label"))


WorldMap <- map_data("world") %>% filter(region != "Antarctica")

ggsave(
  ggplot() +
    geom_polygon(
      data = WorldMap, aes(x = long, y = lat, group = group),
      fill = "#EEf1f0", colour = "grey40"
    ) +
    geom_hline(yintercept = 23.44, linetype = "dashed") +
    geom_hline(yintercept = -23.44, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    geom_point(data = ResLoca, aes(x = long, y = lat, color = Label), size = 3.5, alpha = 0.8) +
    scale_y_continuous(breaks = c(-23.5, 0, 23.5), labels = c("-23.5", "0", "23.5")) +
    scale_color_manual(values = c(
      "Systematic literature search" = "#F44336",
      "Surveillance report" = "#1f77b4", "RSV GEN" = "#66aa1a"
    )) +
    theme_minimal() +
    labs(color = "Data source") +
    theme(
      panel.background = element_blank(),
      axis.text.y = element_text(face = "bold", size = 16),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 16),
      legend.text = element_text(size = 16)
    ),
  filename = paste0(FilePath, "WorldMap.pdf"), width = 14, height = 8
)


# 2. Single virus analysis ------------------------------------------------

FilePath.sec2 <- CreateSubFolder(FilePath, "1.Main")

MainDat <- All_Vir[!Study_ID %in% c(
  "S29A", "S59B", "S62A", "S63A", "S89B", "S90B", "S91B", "S92B", "S93B", "S94B", "S96B",
  "S118B", "S119B", "S120B", "S121B", "S122", "S123B", "S124B", "S125B", "S126B", "S127B", "S128B", "S129B"
) & # These a
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

# MainDat_IFV <- All_Vir[!Study_ID %in% c("S29A", "S59B", "S62A", "S63A", "S89B", "S90B", "S91B", "S92B", "S93B", "S94B", "S96B") & # 这些是case报告，在主分析中保留ratio的数据
#   Index_of_Wave != 0 & Virus_name %in% c("IFV", "IFVA", "IFVB", "IFVA_H1N1", "IFVA_H3N2", "PIV", "PIV_1", "PIV_2", "PIV_3", "PIV_4")] %>%
#   mutate(
#     Virus_name = as_factor(Virus_name),
#     Virus_name = fct_relevel(Virus_name, "IFV", "IFVA", "IFVB", "IFVA_H1N1", "IFVA_H3N2", "PIV", "PIV_1", "PIV_2", "PIV_3", "PIV_4"),
#     Time_interval = as.numeric(Time_interval),
#     Peak_interval = as.numeric(Peak_interval),
#     hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
#   )


MainAnalysis_Recir_REM <- Calu.SingleVir(MainDat,
  target = "Time_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.sec2, "MainAnalysis_Recir.pdf"), width = 16, height = 20, report = F
)

MainAnalysis_Peak_REM <- Calu.SingleVir(MainDat,
  target = "Peak_interval", func = "REM", plot = T, save = T,
  path = paste0(FilePath.sec2, "MainAnalysis_Peak.pdf"), width = 16, height = 20, report = F
)

fwrite(MainAnalysis_Recir_REM, paste0(FilePath.sec2, "Main_recir.csv"), row.names = F)
fwrite(MainAnalysis_Peak_REM, paste0(FilePath.sec2, "Main_peak.csv"), row.names = F)

NewDat <- copy(MainDat)
PercentIncrease <- Calu.Percent(NewDat, target = "Time_interval")
fwrite(PercentIncrease, paste0(FilePath.sec2, "PercentIncrease.csv"), row.names = F)
fwrite(OldWave, paste0(FilePath.sec2, "OldWave.csv"), row.names = F)

# 3. Virus-virus analysis -------------------------------------------------

TwoVirTable <- foreach(
  df = split(MainDat[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, NPIRef = "last")
}
TwoVirTable_Main <- copy(TwoVirTable)

### All virus-virus analysis
TwoVirReport_Recir <- ReportTwoVir(
  dat = TwoVirTable, index = "last", tidy = FALSE, func = "Liner",
  save = TRUE, path = paste0(FilePath.sec2, "TwoVirRecir_RefLast.pdf"), width = 12, height = 10
)
TwoVirReport_Peak <- ReportTwoVir(
  dat = TwoVirTable, index = "peak", tidy = FALSE, save = TRUE,
  path = paste0(FilePath.sec2, "TwoVirPeak_RefLast.pdf"), width = 12, height = 10
)
fwrite(TwoVirReport_Recir, paste0(FilePath.sec2, "Two_recir.csv"), row.names = F)
fwrite(TwoVirReport_Peak, paste0(FilePath.sec2, "Two_peak.csv"), row.names = F)

TwoVirReport_Recir %>%
  as_tibble() %>%
  mutate(
    ID = paste0(FirstVir, "-", SecVir),
    FirstVir = as.character(FirstVir)
  ) %>%
  filter(mean >= 0) %>%
  group_nest(FirstVir) %>%
  mutate(fig = purrr::map(data, plot.TwoVir, path = paste0(FilePath.sec2, "recir"), width = 12, height = 10))

TwoVirReport_Peak %>%
  as_tibble() %>%
  mutate(
    ID = paste0(FirstVir, "-", SecVir),
    FirstVir = as.character(FirstVir)
  ) %>%
  filter(mean >= 0) %>%
  group_nest(FirstVir) %>%
  mutate(fig = purrr::map(data, plot.TwoVir, path = paste0(FilePath.sec2, "peak"), width = 12, height = 10))


stopCluster(cl)

# 5. Sensitivity analysis -------------------------------------------------

# Sensitivity analysis 4
# Filter out the studies that only in the temperate zone (including the southern and northern hemisphere)
source("Code/Sens1.R")

# Sensitivity analysis 3
# Filter out the studies that only in the tropical zone
source("Code/Sens2.R")

# Sensitivity analysis 1
# Filter out the studies that only in the northern hemisphere
# source("Code/Sens3.R")

# Sensitivity analysis 2
# Filter out the studies that only in the southern hemisphere
# source("Code/Sens4.R")

# Sensitivity analysis 5
# Filter out the studies that only report the outcome as proportion
source("Code/Sens5.R")

# Sensitivity analysis 6
# Filter out the studies that only report the outcome as case
source("Code/Sens6.R")

# Sensitivity analysis 7
# Filter out the studies that only from literature
source("Code/Sens7.R")

# Sensitivity analysis 8
# Filter out the studies that only from database
source("Code/Sens8.R")

# Sensitivity analysis 9
# Filter out the studies that only from high-income countries
source("Code/Sens9.R")

# Sensitivity analysis 10
# Filter out the studies that only from none high-income countries (UM_LM_L)
source("Code/Sens10.R")

# Sensitivity analysis 11
# Filter out the studies that only high-quality studies (≥5)
source("Code/Sens11.R")

# Sensitivity analysis 12
# Filter out the studies that only report on a monthly basis
source("Code/Sens12.R")

# Sensitivity analysis 13
# Filter out the studies that only report on a weekly basis
source("Code/Sens13.R")

# Sensitivity analysis 14
# Filter out the studies with all age groups (remove studies conducted in adolescents (<18), infants (<5))
source("Code/Sens14.R")

# Sensitivity analysis 15
# Filter out the studies in the Asia region
# source("Code/Sens15.R")

# Sensitivity analysis 16
# Filter out the studies in the Europe region
# source("Code/Sens16.R")

# Sensitivity analysis 17
# Filter out the studies in the North America region (Canada, USA)
# source("Code/Sens17.R")

# Sensitivity analysis 18
# Filter out the studies in the Latin America region
# source("Code/Sens18.R")

# Sensitivity analysis 19
# Filter out the studies in the North hemisphere temperate zone (23.5 above)
source("Code/Sens19.R")

# Sensitivity analysis 20
# Filter out the studies in the South hemisphere temperate zone (-23.5 below)
source("Code/Sens20.R")

# Sensitivity analysis 21
# Filter out the studies that include the IFV subtype
source("Code/Sens21.R")

# Sensitivity analysis 22
# Filter out the studies that include the PIV subtype
source("Code/Sens22.R")

# Sensitivity analysis Adhoc
source("Code/Sens23_Adhoc.R")
