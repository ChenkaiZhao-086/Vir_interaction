# rm(list=ls())

### !!! IMPORTANT !!! ####
### Please confirm that Rtools is installed on your computer, otherwise the following code cannot run.
### You can use the following code to check.
# pkgbuild::has_build_tools()

# Please confirm that the following  packages have been installed.
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
load("All_Vir.RData")
if (!file.exists("Output")) {
  dir.create("Output")
}
FilePath <- CreateMainFolder(path = "Output/", FolderName = "Revise") # CreateMainFolder(path = "Output/", Date = TRUE)

set.seed(971889)
cl <- makeCluster(10) # Please modify the number of cores you wish to use.
registerDoParallel(cl)

# 1. Plot epidemic character and world map --------------------------------
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
  "S118B", "S119B", "S120B", "S121B", "S122", "S123B", "S124B", "S125B", "S126B", "S127B", "S128B", "S129B",
  "S65A", "S68A", "S69C", "S70L", "S71E", "S74"
) &
  Index_of_Wave != 0 & Index_of_Wave <= 2 &
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

MainAnalysis_Recir_REM <- Calu.SingleVir(MainDat,
  target = "Time_interval", func = "REM", plot = TRUE, save = TRUE,
  path = paste0(FilePath.sec2, "Pooling_Recir.pdf"), width = 12, height = 12, report = FALSE
)

MainAnalysis_Peak_REM <- Calu.SingleVir(MainDat,
  target = "Peak_interval", func = "REM", plot = TRUE, save = TRUE,
  path = paste0(FilePath.sec2, "Pooling_Peak.pdf"), width = 12, height = 12, report = FALSE
)

fwrite(MainAnalysis_Recir_REM, paste0(FilePath.sec2, "Pooling_Recir.csv"), row.names = FALSE)
fwrite(MainAnalysis_Peak_REM, paste0(FilePath.sec2, "Pooling_Peak.csv"), row.names = FALSE)

# 3. Virus-virus analysis -------------------------------------------------
#### First wave -----------------------------------------------------------
TwoVirTable <- foreach(
  df = split(MainDat[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"), # Only BoV and HRV in these two studies
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 1, NPIRef = "last")
}

TwoVirReport_Recir <- ReportTwoVir(dat = TwoVirTable, index = "last")
TwoVirReport_Peak <- ReportTwoVir(dat = TwoVirTable, index = "peak")

fwrite(TwoVirReport_Recir, paste0(FilePath.sec2, "Two_recir.csv"), row.names = FALSE)
fwrite(TwoVirReport_Peak, paste0(FilePath.sec2, "Two_peak.csv"), row.names = FALSE)

### Second wave -----------------------------------------------------------
MainDat_Sec <- MainDat[Index_of_Wave == 2][, m_case := .N, by = .(Study_ID)][m_case > 1]
TwoVirTable_Sec <- foreach(
  df = split(MainDat_Sec[!Study_ID %in% c("S50A", "S50C")], by = "Study_ID"),
  .combine = rbind,
  .packages = packages
) %dopar% {
  Calu.TwoVirInterval(df, Wave = 2, NPIRef = "last")
}

TwoVirReport_Recir_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "last")
TwoVirReport_Peak_Sec <- ReportTwoVir(dat = TwoVirTable_Sec, index = "peak")

fwrite(TwoVirReport_Recir_Sec, paste0(FilePath.sec2, "Two_recir_Sec.csv"), row.names = FALSE)
fwrite(TwoVirReport_Peak_Sec, paste0(FilePath.sec2, "Two_peak_Sec.csv"), row.names = FALSE)

### Plotting --------------------------------------------------------------
# Recir
MergeTable_Recir <- merge(TwoVirReport_Recir, TwoVirReport_Recir_Sec,
  by = c("FirstVir", "SecVir"), all = TRUE, suffixes = c("", "_Sec")
)
MergeTable_Recir <- MergeTable_Recir %>% arrange(-mean)

Matching_Recir <- Matching.Plot(MergeTable_Recir)
pdf(paste0(FilePath.sec2, "Matching_Recir.pdf"), width = 14, height = 14)
replayPlot(Matching_Recir)
dev.off()

# Peak
MergeTable_Peak <- merge(TwoVirReport_Peak, TwoVirReport_Peak_Sec,
  by = c("FirstVir", "SecVir"), all = TRUE, suffixes = c("", "_Sec")
)
MergeTable_Peak <- MergeTable_Peak %>% arrange(-mean)

Matching_Peak <- Matching.Plot(MergeTable_Peak)
pdf(paste0(FilePath.sec2, "Matching_Peak.pdf"), width = 14, height = 14)
replayPlot(Matching_Peak)
dev.off()

GlobalLevel <- levels(MergeTable_Recir$FirstVir)

stopCluster(cl)

# 5. Sensitivity analysis -------------------------------------------------

# Sensitivity analysis 1
# Filter data that in the temperate zone (including the southern
# and northern hemisphere) and the tropical zone
source("Code/Sens1.R")

# Sensitivity analysis 2
# Filter data that from high-income countries and from none high-income countries (UM_LM_L)
source("Code/Sens2.R")

# Sensitivity analysis 3
# Filter data from the North hemisphere temperate zone (23.5 above) and the South hemisphere temperate zone (-23.5 below)
source("Code/Sens3.R")

# Sensitivity analysis 4
# Filter out the studies that include the IFV subtype
# source("Code/Sens4.R") # Please run this code line by line instead of source the whole code.

# Sensitivity analysis 5
# Filter out the studies that include the PIV subtype
# source("Code/Sens5.R") # Please run this code line by line instead of source the whole code.

# Sensitivity analysis 6
# Filter data that report the outcome as proportion and as cases
source("Code/Sens6.R")

# Sensitivity analysis 7
# Filter data from literature and from database
source("Code/Sens7.R")

# Sensitivity analysis 8
# Filter data report on monthly basis and on weekly basis
source("Code/Sens8.R")

# Sensitivity analysis 9
# Filter out the studies that only high-quality studies (≥5)
source("Code/Sens9.R")

# Sensitivity analysis 10
# Filter out the studies with all age groups (remove studies conducted in adolescents (<18), infants (<5))
source("Code/Sens10.R")

# Sensitivity analysis Adhoc
source("Code/Sens11_Adhoc.R")

# Sensitivity analysis Adhoc--Remove Canada
source("Code/Sens12_Adhoc_2.R")

# Sensitivity analysis Adhoc--Remove China
source("Code/Sens13_Adhoc_3.R")

# Sensitivity analysis Adhoc--Remove Finland
source("Code/Sens14_Adhoc_4.R")

# Sensitivity analysis Adhoc--Only summary data
source("Code/Sens15_Adhoc_5.R")

# Sensitivity analysis Adhoc--Canada multiple levels
source("Code/Sens16_Adhoc_6.R")
