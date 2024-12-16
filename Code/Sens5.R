# Sensitivity analysis 5
# Filter out the studies that include the PIV subtype

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

FilePath.Sens1 <- CreateSubFolder(FilePath, "5.PIV")
SensDat1 <- All_Vir[!Study_ID %in% c(
  "S29A", "S59B", "S62A", "S63A", "S89B", "S90B", "S91B", "S92B", "S93B", "S94B", "S96B",
  "S118B", "S119B", "S120B", "S121B", "S122", "S123B", "S124B", "S125B", "S126B", "S127B", "S128B", "S129B",
  "S65A", "S68A", "S69C", "S70L", "S71E", "S74"
) &
  Index_of_Wave != 0 & Virus_name %in% c("IAV", "IBV", "RSV", "PIV_1", "PIV_2", "PIV_3", "PIV_4", "MPV", "sCoV", "RV", "AdV")] %>%
  mutate(
    Virus_name = as_factor(Virus_name),
    Virus_name = fct_relevel(Virus_name, "IAV", "IBV", "RSV", "PIV_1", "PIV_2", "PIV_3", "PIV_4", "MPV", "sCoV", "RV", "AdV"),
    Time_interval = as.numeric(Time_interval),
    Peak_interval = as.numeric(Peak_interval),
    hemisphere = if_else(lat > 0, "North hemisphere", "South hemisphere")
  ) %>%
  setDT()

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

### Plotting --------------------------------------------------------------
# Recir
Sens1_MergeTable_Recir <- Sens1_TwoVirReport_Recir
Sens1_MergeTable_Recir[, 3:12] <- NA
colnames(Sens1_MergeTable_Recir) <- c(
  "FirstVir", "SecVir", "num", "mean", "se", "lci", "uci",
  "num_Sec", "mean_Sec", "se_Sec", "lci_Sec", "uci_Sec"
)
## Re-order the columns
Sens1_TwoVirReport_Recir$FirstVir <- factor(Sens1_TwoVirReport_Recir$FirstVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)
Sens1_TwoVirReport_Recir$SecVir <- factor(Sens1_TwoVirReport_Recir$SecVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)

Sens1_TwoVirReport_Recir_Sec$FirstVir <- factor(Sens1_TwoVirReport_Recir_Sec$FirstVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)
Sens1_TwoVirReport_Recir_Sec$SecVir <- factor(Sens1_TwoVirReport_Recir_Sec$SecVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)

for (i in 1:55) {
  FirstVir_A <- Sens1_TwoVirReport_Recir[i, "FirstVir"]
  SecVir_A <- Sens1_TwoVirReport_Recir[i, "SecVir"]
  FirstVir_B <- Sens1_TwoVirReport_Recir_Sec[i, "FirstVir"]
  SecVir_B <- Sens1_TwoVirReport_Recir_Sec[i, "SecVir"]

  DirectMatch_A <- which(Sens1_MergeTable_Recir$FirstVir == FirstVir_A & Sens1_MergeTable_Recir$SecVir == SecVir_A)
  ReverseMatch_A <- which(Sens1_MergeTable_Recir$FirstVir == SecVir_A & Sens1_MergeTable_Recir$SecVir == FirstVir_A)
  if (length(ReverseMatch_A) > 0) {
    temp <- FirstVir_A
    FirstVir_A <- SecVir_A
    SecVir_A <- temp
  }

  DirectMatch_B <- which(Sens1_MergeTable_Recir$FirstVir == FirstVir_B & Sens1_MergeTable_Recir$SecVir == SecVir_B)
  ReverseMatch_B <- which(Sens1_MergeTable_Recir$FirstVir == SecVir_B & Sens1_MergeTable_Recir$SecVir == FirstVir_B)
  if (length(ReverseMatch_B) > 0) {
    temp <- FirstVir_B
    FirstVir_B <- SecVir_B
    SecVir_B <- temp
  }

  if (length(DirectMatch_A) > 0) {
    Sens1_MergeTable_Recir[Sens1_MergeTable_Recir$FirstVir == FirstVir_A & Sens1_MergeTable_Recir$SecVir == SecVir_A, 3:7] <-
      Sens1_TwoVirReport_Recir[Sens1_TwoVirReport_Recir$FirstVir == FirstVir_A & Sens1_TwoVirReport_Recir$SecVir == SecVir_A, 3:ncol(Sens1_TwoVirReport_Recir)]
  } else if (length(ReverseMatch_A) > 0) {
    Sens1_MergeTable_Recir[Sens1_MergeTable_Recir$FirstVir == FirstVir_A & Sens1_MergeTable_Recir$SecVir == SecVir_A, 3:7] <-
      -Sens1_TwoVirReport_Recir[Sens1_TwoVirReport_Recir$FirstVir == SecVir_A & Sens1_TwoVirReport_Recir$SecVir == FirstVir_A, 3:ncol(Sens1_TwoVirReport_Recir)]
  }

  if (length(DirectMatch_B) > 0) {
    Sens1_MergeTable_Recir[Sens1_MergeTable_Recir$FirstVir == FirstVir_B & Sens1_MergeTable_Recir$SecVir == SecVir_B, 8:12] <-
      Sens1_TwoVirReport_Recir_Sec[Sens1_TwoVirReport_Recir_Sec$FirstVir == FirstVir_B & Sens1_TwoVirReport_Recir_Sec$SecVir == SecVir_B, 3:7]
  } else if (length(ReverseMatch_B) > 0) {
    Sens1_MergeTable_Recir[Sens1_MergeTable_Recir$FirstVir == FirstVir_B & Sens1_MergeTable_Recir$SecVir == SecVir_B, 8:12] <-
      -Sens1_TwoVirReport_Recir_Sec[Sens1_TwoVirReport_Recir_Sec$FirstVir == SecVir_B & Sens1_TwoVirReport_Recir_Sec$SecVir == FirstVir_B, 3:7]
  }
}

oma <- c(4, 4, 4, 4)

opar <- par(
  mfrow = c(11, 11), # Setting matrix
  mar = c(0, 0, 0, 0), # Setting margins (each panel)
  oma = c(4, 4, 4, 4) # Setting outer margins (whole plot)
)

on.exit(par(opar))

# Main loop to draw each panel
for (i in 1:11) {
  for (j in 1:11) {
    # Set up plotting area
    plot(c(1:4), c(1:4),
      xlab = "", ylab = "",
      axes = FALSE, type = "n", # Blank plot
      oma = c(4, 4, 4, 4)
    )

    if (i == j) {
      # Diagonal text
      par(usr = c(0, 1, 0, 1))
      # Create a standardized coordinate system c(0, 1) x c(0, 1)
      text(
        x = 0.5, y = 0.5, labels = levels(Sens1_MergeTable_Recir[, 1])[i],
        cex = 3, font = 2
      )
      box()
    } else if (i < j) {
      # Upper panel
      Panel.ErrorBar(i, j, Sens1_MergeTable_Recir, 900, -550)
    } else {
      # Lower panel
      Panel.Num(i, j, Sens1_MergeTable_Recir, 2)
    }
  }
}

Sens1_MergeTable_Recir_Fig <- recordPlot()

pdf(paste0(FilePath.Sens1, "Matching_Recir.pdf"), width = 16, height = 16)
replayPlot(Sens1_MergeTable_Recir_Fig)
dev.off()


# Peak
Sens1_MergeTable_Peak <- Sens1_TwoVirReport_Peak
Sens1_MergeTable_Peak[, 3:12] <- NA
colnames(Sens1_MergeTable_Peak) <- c(
  "FirstVir", "SecVir", "num", "mean", "se", "lci", "uci",
  "num_Sec", "mean_Sec", "se_Sec", "lci_Sec", "uci_Sec"
)
## Re-order the columns
Sens1_TwoVirReport_Peak$FirstVir <- factor(Sens1_TwoVirReport_Peak$FirstVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)
Sens1_TwoVirReport_Peak$SecVir <- factor(Sens1_TwoVirReport_Peak$SecVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)

Sens1_TwoVirReport_Peak_Sec$FirstVir <- factor(Sens1_TwoVirReport_Peak_Sec$FirstVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)
Sens1_TwoVirReport_Peak_Sec$SecVir <- factor(Sens1_TwoVirReport_Peak_Sec$SecVir,
  levels = c(
    "RV", "PIV_3", "sCoV", "RSV", "AdV",
    "MPV", "PIV_4", "IAV", "PIV_1", "PIV_2", "IBV"
  )
)

for (i in 1:55) {
  FirstVir_A <- Sens1_TwoVirReport_Peak[i, "FirstVir"]
  SecVir_A <- Sens1_TwoVirReport_Peak[i, "SecVir"]
  FirstVir_B <- Sens1_TwoVirReport_Peak_Sec[i, "FirstVir"]
  SecVir_B <- Sens1_TwoVirReport_Peak_Sec[i, "SecVir"]

  DirectMatch_A <- which(Sens1_MergeTable_Peak$FirstVir == FirstVir_A & Sens1_MergeTable_Peak$SecVir == SecVir_A)
  ReverseMatch_A <- which(Sens1_MergeTable_Peak$FirstVir == SecVir_A & Sens1_MergeTable_Peak$SecVir == FirstVir_A)
  if (length(ReverseMatch_A) > 0) {
    temp <- FirstVir_A
    FirstVir_A <- SecVir_A
    SecVir_A <- temp
  }

  DirectMatch_B <- which(Sens1_MergeTable_Peak$FirstVir == FirstVir_B & Sens1_MergeTable_Peak$SecVir == SecVir_B)
  ReverseMatch_B <- which(Sens1_MergeTable_Peak$FirstVir == SecVir_B & Sens1_MergeTable_Peak$SecVir == FirstVir_B)
  if (length(ReverseMatch_B) > 0) {
    temp <- FirstVir_B
    FirstVir_B <- SecVir_B
    SecVir_B <- temp
  }

  if (length(DirectMatch_A) > 0) {
    Sens1_MergeTable_Peak[Sens1_MergeTable_Peak$FirstVir == FirstVir_A & Sens1_MergeTable_Peak$SecVir == SecVir_A, 3:7] <-
      Sens1_TwoVirReport_Peak[Sens1_TwoVirReport_Peak$FirstVir == FirstVir_A & Sens1_TwoVirReport_Peak$SecVir == SecVir_A, 3:ncol(Sens1_TwoVirReport_Peak)]
  } else if (length(ReverseMatch_A) > 0) {
    Sens1_MergeTable_Peak[Sens1_MergeTable_Peak$FirstVir == FirstVir_A & Sens1_MergeTable_Peak$SecVir == SecVir_A, 3:7] <-
      -Sens1_TwoVirReport_Peak[Sens1_TwoVirReport_Peak$FirstVir == SecVir_A & Sens1_TwoVirReport_Peak$SecVir == FirstVir_A, 3:ncol(Sens1_TwoVirReport_Peak)]
  }

  if (length(DirectMatch_B) > 0) {
    Sens1_MergeTable_Peak[Sens1_MergeTable_Peak$FirstVir == FirstVir_B & Sens1_MergeTable_Peak$SecVir == SecVir_B, 8:12] <-
      Sens1_TwoVirReport_Peak_Sec[Sens1_TwoVirReport_Peak_Sec$FirstVir == FirstVir_B & Sens1_TwoVirReport_Peak_Sec$SecVir == SecVir_B, 3:7]
  } else if (length(ReverseMatch_B) > 0) {
    Sens1_MergeTable_Peak[Sens1_MergeTable_Peak$FirstVir == FirstVir_B & Sens1_MergeTable_Peak$SecVir == SecVir_B, 8:12] <-
      -Sens1_TwoVirReport_Peak_Sec[Sens1_TwoVirReport_Peak_Sec$FirstVir == SecVir_B & Sens1_TwoVirReport_Peak_Sec$SecVir == FirstVir_B, 3:7]
  }
}

oma <- c(4, 4, 4, 4)

opar <- par(
  mfrow = c(11, 11), # Setting matrix
  mar = c(0, 0, 0, 0), # Setting margins (each panel)
  oma = c(4, 4, 4, 4) # Setting outer margins (whole plot)
)

on.exit(par(opar))

# Main loop to draw each panel
for (i in 1:11) {
  for (j in 1:11) {
    # Set up plotting area
    plot(c(1:4), c(1:4),
      xlab = "", ylab = "",
      axes = FALSE, type = "n", # Blank plot
      oma = c(4, 4, 4, 4)
    )

    if (i == j) {
      # Diagonal text
      par(usr = c(0, 1, 0, 1))
      # Create a standardized coordinate system c(0, 1) x c(0, 1)
      text(
        x = 0.5, y = 0.5, labels = levels(Sens1_MergeTable_Peak[, 1])[i],
        cex = 3, font = 2
      )
      box()
    } else if (i < j) {
      # Upper panel
      Panel.ErrorBar(i, j, Sens1_MergeTable_Peak, 900, -550)
    } else {
      # Lower panel
      Panel.Num(i, j, Sens1_MergeTable_Peak, 2)
    }
  }
}

Sens1_MergeTable_Peak_Fig <- recordPlot()

pdf(paste0(FilePath.Sens1, "Matching_Peak.pdf"), width = 16, height = 16)
replayPlot(Sens1_MergeTable_Peak_Fig)
dev.off()

stopCluster(cl)
