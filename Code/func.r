#' @title Create one Main folder for Result output
#'
#' @description Create one Main folder for Result output. The folder name is current date or a specific name
CreateMainFolder <- function(path, FolderName = NULL, Date = NULL) {
  if (!is.null(FolderName)) {
    if (!file.exists(paste0(path, FolderName))) {
      dir.create(paste0(path, FolderName))
    }
    FolderPath <- paste0(path, FolderName, "/")
  }

  if (!is.null(Date)) {
    CurrentDate <- format(Sys.time(), "%Y%m%d")
    if (!file.exists(paste0(path, CurrentDate))) {
      dir.create(paste0(path, CurrentDate))
    }
    FolderPath <- paste0(path, CurrentDate, "/")
  }

  return(FolderPath)
}

CreateSubFolder <- function(path, folder) {
  if (!file.exists(paste0(path, folder))) {
    dir.create(paste0(path, folder))
  }
  return(paste0(path, folder, "/"))
}

#' This function is used in the data cleaning stage. Delete blank in the GMR data set
#' @param x data.table of Google Mobility Report
#' @return cleaner GMR data
GMR.CheckBlank <- function(x) {
  grepl("[[:alpha:]]|[[:digit:]]|[[:punct:]]", x, perl = TRUE)
}


#' This function is used in the data cleaning stage.
#' @param dat cleaned GMR data
#' @return a data set that extends the study time to a specified range. NA in Location, sub_region_1 and etc. were filled
expand.date <- function(dat) {
  Count <- dat$Country[1]
  merge_dat <- merge(DateExpand[Country == Count], dat, by.x = c("Country", "date"), by.y = c("Country", "date"), all = T) %>%
    fill(Location, sub_region_1, sub_region_2, Study_ID, .direction = "downup")
  return(merge_dat)
}


#' Plot the trend of OxCGRT
#' @param dat cleaned OxCGRT data
#' @param index the index want to plot. e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @return a plot
OxCGRT.plot <- function(dat, index, save = F, path = NULL) {
  index_name <- rlang::quo_text(enquo(index))
  location <- unique(dat$Country)
  fig <- ggplot() +
    geom_line(data = dat, aes(date, {{ index }}), linewidth = 1.2, linetype = 2, alpha = 0.7, colour = "#223a58") +
    scale_x_date(name = "Date", date_labels = "%Y\nW%U", breaks = "8 weeks") +
    scale_y_continuous(
      name = sprintf(
        "OxCGRT index\n%s",
        str_replace_all(str_extract(index_name, regex("[A-Za-z]+")), "\\B[A-Z]", " \\0")
      ),
      limits = c(0, 100)
    ) +
    labs(title = location) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1)
    )

  if (save == T) {
    ggsave(fig, file = paste0(path, location, "_OxCGRT", ".pdf"), width = 10, height = 6)
  }

  print(fig)
}


#' Plot the trend of GMR
#' @param dat cleaned GMR data
#' @param index the index want to plot. e.g. retail_and_recreation_percent_change_from_baseline
#' @return a plot
GMR.plot <- function(dat, index, save = F, path = NULL) {
  location <- unique(dat$Country)
  fig <- ggplot() +
    geom_line(data = dat, aes(date, {{ index }}), linewidth = 1.2, linetype = 2, alpha = 0.7, colour = "#223a58") +
    scale_x_date(name = "Date", date_labels = "%Y\nW%U", breaks = "8 weeks") +
    scale_y_continuous(name = "Google Mobility Report\nRetail and recreation percent change from baseline") +
    labs(title = location) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1)
    )

  if (save == T) {
    ggsave(fig, file = paste0(path, location, "_GMR", ".pdf"), width = 10, height = 6)
  }

  print(fig)
}


#' This function is used to generate the plot title, like Ottawa, Province of Ontario, Canada
#' @param dat plot data set. Only in one study
#' @return a character of plot title
plot.GetTitle <- function(dat) {
  Country <- unique(dat$Country)
  if (Country == "Taiwan") {
    return("Taiwan, China")
  }

  Location <- unique(dat$Location)
  if (is.na(Location)) {
    return(Country)
  } else {
    return(paste0(Location, ", ", Country))
  }
}


#' This function is used to scale data to 0-1
#' @param multiply amplify the data
#' @return a series of number
ScaleToOne <- function(x, multiply = 1) {
  # x_shift = x+0.001
  # y=log(na.omit(x_shift))
  # return((y - min(y))/(max(y) - min(y)) * multiply)
  # return((x - mean(y)) / sd(y))
  x_shift <- x + 0.001
  y <- log(x_shift)
  scaled_value <- (y - min(y, na.rm = T)) / (max(y, na.rm = T) - min(y, na.rm = T)) * multiply
  return(scaled_value)
}


#' This function is used to generate the polygon data which used to plot small triangle (represent re-circulation of virus and intensity) in the plot
#' @param dat basic data. e.g. All_Vir
#' @param ID Study_ID
#' @param multiply a parameter in ScaleToOne()
#' @return a series of number
plot.GetPolygonDat <- function(dat, ID, multiply = 1) {
  AfterPandemic <- dat %>%
    filter(Study_ID == ID) %>%
    dplyr::select(Study_ID, Virus_ID, Virus_name, Index_of_Wave, ReCirculation_time, Peak_time, ReCirculation_rate, Peak_occurs) %>%
    mutate(Virus_ID = as.numeric(str_extract(Virus_ID, "\\d.*"))) %>%
    arrange(Virus_ID, Index_of_Wave) %>%
    filter(Index_of_Wave >= 0)

  BeforePandemic <- dat %>%
    filter(Study_ID == ID) %>%
    dplyr::select(
      Study_ID, Virus_ID, Virus_name, Index_of_Wave, ReCirculation_time, Peak_time, ReCirculation_rate, Peak_occurs,
      Last_circ_time, Last_peak_time, Last_circ_rate
    ) %>%
    mutate(
      Index_of_Wave = if_else(Index_of_Wave %in% c(0, 1), -0.5, Index_of_Wave),
      Virus_ID = as.numeric(str_extract(Virus_ID, "\\d.*"))
    ) %>%
    filter(Index_of_Wave < 0) %>% # Index_of_Wave == -1
    dplyr::select(-ReCirculation_time, -Peak_time, -ReCirculation_rate) %>%
    rename(
      ReCirculation_time = Last_circ_time,
      Peak_time = Last_peak_time,
      ReCirculation_rate = Last_circ_rate
    )

  PosiLable <- seq(5, 90, length.out = length(unique(dat$Virus_name)))

  PolygonDat <- bind_rows(list(AfterPandemic, BeforePandemic)) %>%
    pivot_longer(cols = ReCirculation_time:Peak_time, names_to = "label", values_to = "time") %>%
    mutate(n = ifelse(label == "Peak_time", 2, 1)) %>%
    uncount(n)

  PosiDat <- PolygonDat %>%
    dplyr::select(Study_ID, Virus_ID, Virus_name) %>%
    distinct() %>%
    arrange(Virus_name) %>%
    mutate(posi = rev(PosiLable))

  PolygonDat <- PolygonDat %>%
    left_join(PosiDat, by = join_by(Study_ID, Virus_ID, Virus_name)) %>%
    mutate(
      index = row_number(),
      ReCirculation_rate_Scale = if_else(label == "ReCirculation_time", 0,
        if_else(index %% 2 == 0 & label != "ReCirculation_time", 0, ScaleToOne(ReCirculation_rate, multiply))
      ),
      ReCirculation_rate_Scale = ReCirculation_rate_Scale + posi
    ) %>%
    group_by(Study_ID, Virus_ID, Index_of_Wave) %>%
    mutate(group = cur_group_id())

  return(PolygonDat)
}


#' This function is used to generate the plot of epidemic trend
#' @param O_GData a list of OxCGMR data set or in one Study_ID. The list should split by the "Study_ID" column
#' @param VirData input the All_Vir data set or All_Vir data in one Study_ID. For aesthetics, some main virus coule be filtered and "Virus_name" column should converted into factor and relevel
#' @param OxCindex index of OxCGRT data, e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @param GMRindex index of GMR data, e.g. retail_and_recreation_percent_change_from_baseline
#' @param multiply a parameter in ScaleToOne(). To generate higher triangle in plot
#' @return a plot
plot.CountryTrend <- function(VirData, multiply = 8, # O_GData, OxCindex, GMRindex,
                              save = F, path = NULL, width = 12, height = 6) {
  ID <- unique(VirData$Study_ID) # unique(O_GData$Study_ID)
  print(ID)

  PolygonDat <- VirData %>% filter(Study_ID == ID)
  if (any(PolygonDat$Virus_name == "IFVA")) {
    PolygonDat <- PolygonDat %>%
      filter(Virus_name != "IFV") %>%
      plot.GetPolygonDat(., ID, multiply = multiply)
  } else {
    PolygonDat <- PolygonDat %>%
      plot.GetPolygonDat(., ID, multiply = multiply)
  }


  # Get end date of study. Used to add one vertical line in plot
  EndDate <- VirData %>%
    filter(Study_ID == ID) %>%
    dplyr::select(Study_Period) %>%
    distinct() %>%
    str_extract(., "(?<=-).*")

  title <- plot.GetTitle(VirData)

  PosiLable <- seq(5, 90, length.out = length(unique(PolygonDat$Virus_name)))

  # get the name of each virus
  LableDat <- PolygonDat %>%
    ungroup() %>%
    arrange(time) %>%
    dplyr::select(Virus_name, time, posi) %>%
    distinct(Virus_name, .keep_all = T) %>%
    mutate(
      time = min(time) - 60,
      posi = posi + 4
    )

  fig <- ggplot() +
    # geom_segment(data = SegmentDat, aes(x = ReCirculation_time, xend = Peak_time, y = Virus_ID, yend = Virus_ID, color = factor(Virus_name))) +
    # geom_segment(data = SegmentDat, aes(x = Peak_time, xend = Peak_time, Virus_ID, yend = (ReCirculation_rate/10), color = (Virus_name), size = ReCirculation_rate), linewidth = 3) +
    geom_vline(xintercept = as.Date(EndDate), linewidth = 1, linetype = 2, colour = "#bc3b29") +
    geom_vline(xintercept = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")), linewidth = 0.5, linetype = 2, alpha = 0.7) +
    # geom_hline(yintercept = 100 * 100/250, linewidth = 1, linetype = 2, colour = "grey75") +
    geom_hline(yintercept = c(PosiLable, PosiLable + multiply), linewidth = 0.3, linetype = 3, alpha = 0.5) +
    geom_hline(yintercept = c(PosiLable + multiply), linewidth = 0.3, linetype = 3, alpha = 0.5) +
    # geom_line(data = O_GData, aes(date, {{OxCindex}}, color = "OxCGRT"), linewidth = 1.2, linetype = 2, alpha = 0.6) +
    # geom_line(data = O_GData, aes(date, {{GMRindex}}, color = "GMR"), linewidth = 1.2, linetype = 4, alpha = 0.3) +
    # scale_color_manual(values = c("OxCGRT" = "#4dbbd5", "GMR" = "#d62728")) +
    geom_polygon(
      data = PolygonDat, aes(x = time, y = ReCirculation_rate_Scale, group = group, fill = Virus_name),
      color = "grey25", alpha = 0.75, show.legend = F
    ) +
    scale_x_date(limits = c(min(LableDat$time), as.Date(EndDate)), name = "Date", date_labels = "%Y\nW%U", breaks = "16 weeks") +
    # xlim(NA, as.Date(EndDate) + 15) +
    # scale_y_continuous(name = "OxCGRT index\nGovernment Response Index",
    #                    breaks = c(0, 25, 50, 75, 100), labels = c("0", "25", "50", "75", "100"), limits = c(0, 100), expand = c(0,0.1),
    #                    sec.axis = sec_axis(~., name = "Google Mobility Report\nRetail and recreation percent change from baseline",
    #                                        breaks = seq(0, 100, 20), labels = paste0(seq(0, 250, 50)))) +
    scale_fill_manual(values = c(
      "#a6cee3", "#2ca02c", "#9467bd", "#ff7f0e", "#1b9e77", "#8c564b", "#17becf", "#e377c2", "#bcbd22",
      "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#1f77b4"
    )) +
    geom_label(data = LableDat, aes(x = time, y = posi, label = Virus_name), size = 5) +
    labs(title = title, y = NULL, color = "") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 14, colour = "black"),
      axis.text.y = element_blank(),
      axis.title = element_text(size = 14, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
    )


  suppressWarnings(print(fig))

  if (save == T) {
    ggsave(fig, file = paste0(path, ID, "_", title, ".pdf"), width = width, height = height)
  }
}


#' Negative binomial distribution model
NBM.cal <- function(dat, index) {
  formula <- as.formula(paste0(index, " ~ 1"))
  model <- glm.nb(formula, data = dat)
  SummModel <- summary(model)

  mean <- exp(SummModel[["coefficients"]][1, 1])
  cal.mean <- SummModel[["coefficients"]][1, 1]
  se <- coeftest(model, vcov = vcovCL(model, cluster = dat$Country))[2]
  t_value <- qt(1 - 0.05 / 2, df = model[["df.residual"]])
  lci <- exp(mean - t_value * se)
  uci <- exp(mean + t_value * se)
  return(Res = data.frame(
    mean = mean,
    cal.mean = cal.mean,
    se = se,
    t_value = t_value,
    lci = lci,
    uci = uci,
    ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")
  ))
}

#'  Random effect model
REM.cal <- function(dat, index) {
  formula <- as.formula(paste0(index, "~ 1 + (1 | Country)"))
  model <- glmmTMB(formula, family = nbinom2(), data = dat)
  SummModel <- suppressWarnings(summary(model))
  mean <- exp(SummModel[["coefficients"]][["cond"]][1, 1])
  cal.mean <- SummModel[["coefficients"]][["cond"]][1, 1]
  se <- SummModel[["coefficients"]][["cond"]][1, 2]
  lci <- exp(SummModel[["coefficients"]][["cond"]][1, 1] - 1.96 * se)
  uci <- exp(SummModel[["coefficients"]][["cond"]][1, 1] + 1.96 * se)
  return(Res = data.frame(
    mean = mean,
    cal.mean = cal.mean,
    se = se,
    lci = lci,
    uci = uci,
    ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")
  ))
}

#' This function is used to generate the plot of onset-onset interval based on the Calu.SingleVir()
#' @param dat onset-onset interval data
#' @param target which target to use, Time_interval and Peak_interval
#' @return a plot
Calu.SingleVir.plot <- function(dat, target, save, path, width = 16, height = 8, ...) {
  if (target == "Time_interval") {
    title <- "onset"
  } else {
    title <- "peak"
  }

  # ExpandVirList <- expand.grid(Virus_name = dat$Virus_name %>% unique(),
  #                              Index_of_Wave = c(-4:7)[-5]) # dat$Index_of_Wave %>% unique())
  #
  # FigDat <- right_join(dat, ExpandVirList) %>% arrange(Virus_name, Index_of_Wave) %>%
  FigDat <- dat %>%
    arrange(Virus_name, Index_of_Wave) %>%
    mutate(
      Pandemic = if_else(Index_of_Wave %in% as.character(seq(-6, -1)), "Before the pandemic",
        if_else(Index_of_Wave == 1, "After the pandemic\n(the first onset)",
          "After the pandemic\n(the second and later onsets)"
        )
      ),
      Pandemic = fct_relevel(
        Pandemic, "Before the pandemic", "After the pandemic\n(the first onset)",
        "After the pandemic\n(the second and later onsets)"
      ),
      across(c(mean, lci, uci), function(x) if_else(x < 0, -x, x)),
      group = as.factor(paste0(Virus_name, "_", Index_of_Wave))
    ) %>%
    filter(Index_of_Wave != -1) %>%
    mutate(
      Index_of_Wave = if_else(Index_of_Wave < 0, Index_of_Wave + 1, Index_of_Wave),
      Index_of_Wave = as.factor(Index_of_Wave)
    )

  fig <- ggplot(FigDat) +
    geom_hline(yintercept = c(0, 365, 365 * 2, 365 * 3, 365 * 4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
    geom_errorbar(aes(x = fct_rev(Index_of_Wave), ymin = lci, ymax = uci, colour = Pandemic), linewidth = 1, width = 0.8) +
    geom_point(aes(x = fct_rev(Index_of_Wave), y = mean, colour = Pandemic), size = 3) +
    scale_color_manual(values = c(
      "After the pandemic\n(the second and later onsets)" = "#4dbbd5", "After the pandemic\n(the first onset)" = "#ff7f0e",
      "Before the pandemic" = "#d62728"
    )) +
    scale_y_continuous(
      limits = c(0, 1500), breaks = c(0, 365, 365 * 2, 365 * 3, 365 * 4),
      labels = c("0", "1", "2", "3", "4"), expand = c(0, 0)
    ) +
    labs(x = "Index of epidemics", y = paste0("Time lapsed since the previous ", title, " (in years)"), color = "Period") +
    theme_bw() +
    coord_flip() +
    facet_grid(Virus_name ~ ., scales = "free", space = "free") +
    theme(
      axis.text.x = element_text(size = 24, colour = "black"),
      axis.text.y = element_text(size = 20, colour = "black"),
      axis.title.x = element_text(size = 26, face = "bold", colour = "black", vjust = -1),
      axis.title.y = element_text(size = 26, face = "bold", colour = "black", vjust = 2.5),
      strip.text = element_text(size = 24, face = "bold", colour = "black"),
      strip.background = element_rect(colour = "grey20", fill = "white", size = 0.6),
      legend.title = element_text(size = 20, face = "bold", colour = "black"),
      legend.text = element_text(size = 20, colour = "black"),
      plot.margin = unit(c(1, 1.5, 1, 1.5), "cm")
    )

  print(fig)

  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }
}

#' This function is used to calculate onset-onset interval or peak-peak interval
#' @param dat analysis data set e.g. MainDat
#' @param target which target to use, Time_interval and Peak_interval
#' @param func which function to calculate
#' @param report generate a tidy report table
#' @return a plot and a table
Calu.SingleVir <- function(dat, target = c("Time_interval", "Peak_interval"), func = c("Liner", "NBM", "REM"),
                           plot = T, save = F, path = NULL, width = 12, height = 6, report = F, ...) {
  RecircDat <- switch(func,
    "Liner" = {
      suppressWarnings(dat[, .(
        num = .N, mean = mean(eval(str2expression(target)), na.rm = T),
        sd = sd(eval(str2expression(target)), na.rm = T)
      ),
      keyby = .(Virus_name, Index_of_Wave)
      ])
    },
    "NBM" = {
      suppressWarnings(dat[!is.na(eval(str2expression(target)))][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, NBM.cal, index = target)][, data := NULL])
    },
    "REM" = {
      suppressWarnings(dat[!is.na(eval(str2expression(target)))][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, REM.cal, index = target)][, data := NULL])
    }
  )

  if (min(dat$Index_of_Wave) < -1) {
    if (func == "Liner") {
      BeforePandemic <- RecircDat[Index_of_Wave < 0]
      BeforePandemic <- na.omit(BeforePandemic)
      BeforePandemic <- BeforePandemic[, .SD[order(-Index_of_Wave)], keyby = .(Virus_name)][, c("mean", "sd") := .(shift(mean, n = 1L, fill = 0), shift(sd, n = 1L, fill = 0)), keyby = .(Virus_name)][, c("mean", "sd") := .(-mean, -sd)][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ci := paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")"), keyby = .(Virus_name)][, Cumday := round(cumsum(as.numeric(mean)), 2), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday - 1.96 * sd / sqrt(num), 0), round(Cumday + 1.96 * sd / sqrt(num), 2)), keyby = .(Virus_name)]

      AfterPandemic <- RecircDat[Index_of_Wave > 0]
      AfterPandemic <- na.omit(AfterPandemic)
      AfterPandemic <- AfterPandemic[, Cumday := cumsum(as.numeric(mean)), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(round(Cumday - 1.96 * sd / sqrt(num), 0), round(Cumday + 1.96 * sd / sqrt(num), 2))]

      RecircDat <- bind_rows(BeforePandemic, AfterPandemic)
      RecircDat <- RecircDat[order(Virus_name, Index_of_Wave)]
    } else {
      RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)

      BeforePandemic <- RecircDat[Index_of_Wave < 0]
      BeforePandemic <- na.omit(BeforePandemic)
      BeforePandemic <- BeforePandemic[, .SD[order(-Index_of_Wave)], keyby = .(Virus_name)][, ":="(mean = unlist(shift(mean, n = 1L, fill = 0)),
        se = unlist(shift(se, n = 1L, fill = 0)),
        lci = unlist(shift(lci, n = 1L, fill = 0)),
        uci = unlist(shift(uci, n = 1L, fill = 0))), keyby = .(Virus_name)][, ":="(mean = -round(mean, 0),
        lci = -round(lci, 0),
        uci = -round(uci, 0))][, ":="(Cumday = round(cumsum(mean)),
        ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
      keyby = .(Virus_name)
      ][, c("Cumlci", "Cumuci") := .((Cumday - (mean - lci)), Cumday + (mean - lci))]

      AfterPandemic <- RecircDat[Index_of_Wave > 0]
      AfterPandemic <- na.omit(AfterPandemic)
      AfterPandemic <- AfterPandemic[, ":="(Cumday = round(cumsum(mean), 0),
        mean = round(mean, 0),
        lci = round(lci, 0),
        uci = round(uci, 0),
        ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
      keyby = .(Virus_name)
      ][, c("Cumlci", "Cumuci") := .((Cumday - (mean - lci)), Cumday + (mean - lci))]

      RecircDat <- bind_rows(BeforePandemic, AfterPandemic)
      RecircDat <- RecircDat[order(Virus_name, Index_of_Wave)]
    }
  } else {
    if (func == "Liner") {
      RecircDat <- RecircDat[, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ci := paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")"), keyby = .(Virus_name)][, ":="(Cumday = cumsum(as.numeric(mean)),
        ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
      RecircDat <- na.omit(RecircDat)
    } else {
      RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
      RecircDat <- RecircDat[, ":="(Cumday = round(cumsum(mean), 0),
        ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
      keyby = .(Virus_name)
      ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))] %>% drop_na()
    }
  }

  if (plot == T) {
    Calu.SingleVir.plot(RecircDat, target, save, path, width = width, height = height, ...)
  }


  if (report == T) {
    ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
    ReportTable <- setnames(
      ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"),
      c("Virus", "Index of epidemics", "Report number", "95% Confidence interval")
    )

    return(ReportTable)
  } else {
    return(RecircDat)
  }
}
# Calu.SingleVir <- function(dat, target = c("Time_interval", "Peak_interval"), func = c("Liner", "NBM", "REM"), excludeNA = T,
#                            plot = T, save = F, path = NULL, width = 12, height = 6, report = F) {
#   switch(func,
#          "Liner" = {
#            RecircDat <- dat[, .(num = .N, mean = mean(eval(str2expression(target)), na.rm = T), sd = sd(eval(str2expression(target)), na.rm = T)), keyby = .(Virus_name, Index_of_Wave)
#            ][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))
#            ][, ':='(Cumday = cumsum(as.numeric(mean)),
#                     ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")), keyby = .(Virus_name)
#            ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
#          },
#          "NBM" = {
#            RecircDat <- dat[!is.na(eval(str2expression(target)))
#            ][, num := .N, by = .(Virus_name, Index_of_Wave)
#            ][num != 1
#            ][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave)
#            ][, Res := lapply(data, NBM.cal, index = target)
#            ][, data := NULL]
#            RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
#            RecircDat <- RecircDat[, ':='(Cumday = cumsum(mean),
#                                          ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")),
#                                   keyby = .(Virus_name)
#            ][, c("Cumlci", "Cumuci") := .((Cumday - (mean - lci)), Cumday + (mean - lci))]
#          },
#          "REM" = {
#            RecircDat <- dat[!is.na(eval(str2expression(target)))
#            ][, num := .N, by = .(Virus_name, Index_of_Wave)
#            ][num != 1
#            ][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave)
#            ][, Res := lapply(data, REM.cal, index = target)
#            ][, data := NULL]
#            RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
#            RecircDat <- RecircDat[, ':='(Cumday = cumsum(mean),
#                                          ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")),
#                                   keyby = .(Virus_name)
#            ][, c("Cumlci", "Cumuci") := .((Cumday - (mean - lci)), Cumday + (mean - lci))]
#          }
#   )
#
#
#   if (excludeNA == T) {
#     RecircDat <- RecircDat %>% drop_na()
#   }
#
#   if (plot == T) {
#     if (target == "Time_interval") {
#       title <- "Time interval between each start of circulation"
#     } else{
#       title <- "Time interval between each peak of circulation"
#     }
#
#     fig <- ggplot(RecircDat) +
#       geom_hline(yintercept = c(0, 365, 365*2, 365*3, 365*4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
#       geom_errorbar(aes(x = fct_rev(Virus_name), ymin = Cumlci, ymax = Cumuci), linewidth = 1, width = 0.4, colour = "#08203e") +
#       geom_point(aes(x = fct_rev(Virus_name), y = Cumday), colour = "#852237", size = 3) +
#       geom_text(aes(x = fct_rev(Virus_name), y = Cumday, label = Index_of_Wave), alpha = 0.7, size = 7, nudge_x = 0.3) +
#       scale_y_continuous(limits = c(0, 2500), breaks = c(0, 365, 365*2, 365*3, 365*4),
#                          labels = c("--", "1 year", "2 year", "3 year", "4 year"), expand = c(0, 0)) +
#       labs(title = title, x = "Virus", y = "Time interval") +
#       theme_bw() +
#       theme(axis.text = element_text(size = 14, colour = "black"),
#             axis.title = element_text(size = 16, face = "bold", colour = "black"),
#             plot.title = element_text(size = 18, face = "bold", colour = "black"),
#             plot.margin = unit(c(0.5,1,0.5,1),"cm")) +
#       coord_flip()
#
#     print(fig)
#   }
#
#   if (save == T) {
#     ggsave(fig, file = path, width = width, height = height)
#   }
#
#   if (report == T) {
#     ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
#     ReportTable <- setnames(ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"),
#                             c("Virus", "Index of epidemics", "Report number", "95% Confidence interval"))
#
#     return(ReportTable)
#   }else{
#     return(RecircDat)
#   }
# }


#' This function is used to generate the reference date of OxC or GMR
#' @param dat OxCGMR data set. In the world level, mean of each index should be calculate.
#' @param Discount threshold of NPI discount
#' @param type calculate in OxCGRT or GMR
#' @param buffer If the NPI returns above the threshold range within the buffer time, this time is not calculated
#' @param index index of OxCGRT data, e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @return a Date, indicate the reference date
Calu.NPIDate <- function(dat, Discount = 0.9, type = c("OxC", "GMR"), buffer = 28, index, Cutoff = 365) {
  index_name <- substitute(index)
  switch(type,
    "OxC" = {
      CutoffDate <- as.Date(dat[!is.na(dat[[index_name]]), date][1]) + Cutoff
      SubsetDat <- subset(dat, date <= CutoffDate)
      MaxDate <- as.Date(SubsetDat[which.max(SubsetDat[[index_name]]), date])
      MaxOxC <- max(SubsetDat[[index_name]], na.rm = TRUE)
      DiscountOxC <- Discount * MaxOxC

      NPI_Table <- SubsetDat[, .(date, lable = (eval(index_name) <= DiscountOxC))][
        is.na(lable), lable := FALSE
      ][date > MaxDate, .(date, lable, checkNPI = frollsum(lable, buffer, na.rm = TRUE, align = "left"))]

      # OxC_GMR_global %>%
      #   dplyr::select(date, GovernmentResponseIndex_WeightedAverage_ForDisplay) %>%
      #   mutate(lable = OxC_GMR_global$GovernmentResponseIndex_WeightedAverage_ForDisplay <= DiscountOxC) %>%
      #   filter(date > MaxDate) %>%
      #   mutate(checkNPI = slide(lable, sum, .before = 0, .after = 27))
      return(tibble(
        Study_ID = unique(dat[, Study_ID]),
        NPIDate = as.Date(NPI_Table[checkNPI == buffer, date][1])
      ))
    },
    "GMR" = {
      CutoffDate <- as.Date(dat[!is.na(dat[[index_name]]), date][1]) + Cutoff
      SubsetDat <- subset(dat, date <= CutoffDate)
      MinDate <- as.Date(SubsetDat[which.min(SubsetDat[[index_name]]), date])
      MinGMR <- min(SubsetDat[[index_name]], na.rm = TRUE)
      DiscountGMR <- (1 - Discount + 1) * MinGMR
      DiscountDate <- dat[date >= MinDate & eval(index_name) >= DiscountGMR, date][1]

      NPI_Table <- SubsetDat[, .(date, lable = (eval(index_name) >= DiscountGMR))][
        is.na(lable), lable := FALSE
      ][date > DiscountDate, .(date, lable, checkNPI = frollsum(lable, buffer, na.rm = TRUE, align = "left"))]
      return(tibble(
        Study_ID = unique(dat[, Study_ID]),
        NPIDate = as.Date(NPI_Table[checkNPI == buffer, date][1])
      ))
    }
  )
}


#' This function is similar to Calu.SingleVir(), but applies to the reference OxC_GMR dataset. The reference date comes from the Calu.NPIDate() function
#' @param dat analysis data set e.g. MainDat
#' @param NPIdat analysis data of NPI in each study
#' @param target which target to use, Time_interval and Peak_interval
#' @param excludeNA whether exclude NA in the RecircDat. Mainly in some Virus with many cirulation
#' @param report generate a tidy report table
#' @param NPIDate a date generated by Calu.NPIDate()
#' @return a plot and a table
Calu.SingleVir.Ref <- function(dat, NPIdat, target = c("Time_interval", "Peak_interval"), func = c("Liner", "NBM", "REM"), excludeNA = T, plot = T, save = F, path = NULL, width = 12,
                               height = 6, report = F) {
  RecircDat <- merge.data.table(dat, NPIdat, by = "Study_ID", all.x = T) %>%
    drop_na(NPIDate) %>%
    filter(Index_of_Wave >= 0) %>%
    setDT()

  RecircDat <- RecircDat[, ":="(Recir = ifelse(Index_of_Wave == 1, ReCirculation_time - NPIDate, Time_interval),
    Peak = ifelse(Index_of_Wave == 1, Peak_time - NPIDate, Peak_interval))] %>%
    filter(Recir > 0 & Peak > 0)

  switch(target,
    "Time_interval" = {
      switch(func,
        "Liner" = {
          RecircDat <- RecircDat[, .(num = .N, mean = mean(Recir, na.rm = T), sd = sd(Recir, na.rm = T)), keyby = .(Virus_name, Index_of_Wave)][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "NBM" = {
          RecircDat <- RecircDat[!is.na(Recir)][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, NBM.cal, index = "Recir")][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "REM" = {
          RecircDat <- RecircDat[!is.na(Recir)][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, REM.cal, index = "Recir")][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        }
      )
    },
    "Peak_interval" = {
      switch(func,
        "Liner" = {
          RecircDat <- RecircDat[, .(num = .N, mean = mean(Peak, na.rm = T), sd = sd(Peak, na.rm = T)), keyby = .(Virus_name, Index_of_Wave)][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "NBM" = {
          RecircDat <- RecircDat[!is.na(Peak)][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, NBM.cal, index = "Peak")][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "REM" = {
          RecircDat <- RecircDat[!is.na(Peak)][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, REM.cal, index = "Peak")][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        }
      )
    }
  )

  if (excludeNA == T) {
    RecircDat <- RecircDat %>% drop_na()
  }

  if (plot == T) {
    if (target == "Time_interval") {
      title <- "onset"
    } else {
      title <- "peak"
    }

    fig <- ggplot(RecircDat) +
      geom_hline(yintercept = c(0, 365, 365 * 2, 365 * 3, 365 * 4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
      geom_errorbar(aes(x = fct_rev(Virus_name), ymin = Cumlci, ymax = Cumuci), linewidth = 1, width = 0.4, colour = "#08203e") +
      geom_point(aes(x = fct_rev(Virus_name), y = Cumday), colour = "#852237", size = 3) +
      scale_y_continuous(
        breaks = c(0, 365, 365 * 2, 365 * 3, 365 * 4),
        labels = c("Reference NPI", "1 year", "2 year", "3 year", "4 year"), expand = c(0, 0)
      ) +
      labs(x = "Virus", y = paste0("Time lapsed since the previous ", title, " (in years)")) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, face = "bold", colour = "black"),
        plot.title = element_text(size = 18, face = "bold", colour = "black"),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
      ) +
      coord_flip()

    print(fig)
  }

  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }

  if (report == T) {
    ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
    ReportTable <- setnames(
      ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"),
      c("Virus", "Index of epidemics", "Report number", "95% Confidence interval")
    )

    return(ReportTable)
  } else {
    return(RecircDat)
  }
}


#' This function is used to find the virus re-circulation time for each Virus_name in each study. And the mean value of NPI information in the specific time range
#' @param dat a list, split by Virus_name and Study_ID
#' @param OxCdat
#' @param threshold A time range, if earlier than the NPI's reporting time, calculates the mean NPI of the reporting time to the recovery time. Otherwise, calculate the mean NPI over this time range
#' @return
Calu.NPIinfo <- function(dat, OxCdat, threshold = 180,
                         OxCindex = GovernmentResponseIndex_WeightedAverage_ForDisplay,
                         GMRindex = retail_and_recreation_percent_change_from_baseline) {
  OxCindex_name <- substitute(OxCindex)
  GMRindex_name <- substitute(GMRindex)
  RecirDate <- dat[1, ReCirculation_time]
  ID <- dat[1, Study_ID]
  RecirDateEarly <- RecirDate - threshold
  NpiStarDate <- OxCdat[Study_ID == ID & !is.na(eval(OxCindex_name)), .(date)][1, date]
  GMRStarDate <- OxCdat[Study_ID == ID & !is.na(eval(GMRindex_name)), .(date)][1, date]

  CheckPointDateNPI <- if_else(RecirDateEarly >= NpiStarDate, NpiStarDate, RecirDateEarly)
  CheckPointDateGMR <- if_else(RecirDateEarly >= GMRStarDate, GMRStarDate, RecirDateEarly)

  NPI <- OxCdat[date >= CheckPointDateNPI & date <= RecirDate, .(meanNPI = mean(eval(OxCindex_name), na.rm = T))]
  GMR <- OxCdat[date >= CheckPointDateGMR & date <= RecirDate, .(meanGMR = mean(eval(GMRindex_name), na.rm = T))]
  return(bind_cols(list(dat, NPI, GMR, CheckPointDateNPI = CheckPointDateNPI, CheckPointDateGMR = CheckPointDateGMR)))
}


#' This function calculates the R square and regression coefficient for each virus
#' @param dat a data set generated by Calu.NPIinfo()
#' @return a list, contain R square, coef table and etc.
Calu.regression <- function(dat, func = c("Liner", "NBM", "REM")) {
  VirName <- as.character(unique(dat$Virus_name))
  switch(func,
    "Liner" = {
      Reg1 <- try(lm(as.numeric(Time_interval) ~ 1, data = dat))
      Reg2 <- try(lm(as.numeric(Time_interval) ~ meanGMR, data = dat))
      Reg3 <- try(lm(as.numeric(Time_interval) ~ meanNPI, data = dat))
      Reg4 <- try(lm(as.numeric(Time_interval) ~ meanGMR + meanNPI, data = dat))

      Summ <- lapply(list(Reg1, Reg2, Reg3, Reg4), function(model) {
        suppressWarnings(summary(model))
      })

      aic <- c(AIC(Reg1), AIC(Reg2), AIC(Reg3), AIC(Reg4))
      aic <- ifelse(aic == -Inf, 999, aic)

      model <- eval(str2expression(paste0("Reg", which.min(aic))))

      Summ <- Summ[[which.min(aic)]]

      SummList <- list(
        VirName = VirName,
        lm = model,
        R_square = Summ$r.squared,
        R_square_adj = Summ$adj.r.squared,
        SummTable = Summ$coefficients,
        Intercept = Summ$coefficients[1, 1],
        Intercept_se = Summ$coefficients[1, 2]
      ) # ,
      # Coef = c(Summ$coefficients[2,1], Summ$coefficients[3,1]),
      # Coef_se = c(Summ$coefficients[2,2], Summ$coefficients[3,2]))
    },
    "NBM" = {
      Reg <- glm.nb(as.numeric(Time_interval) ~ meanNPI + meanGMR, data = dat)
      Summ <- summary(Reg)
      SummList <- list(
        VirName = VirName,
        lm = Reg,
        SummTable = Summ$coefficients,
        Intercept = Summ$coefficients[1, 1],
        Intercept_se = Summ$coefficients[1, 2]
      ) # ,
      # Coef = c(Summ$coefficients[2,1], Summ$coefficients[3,1]),
      # Coef_se = c(Summ$coefficients[2,2], Summ$coefficients[3,2]))
    },
    "REM" = {
      Reg1 <- try(glmmTMB(as.numeric(Time_interval) ~ 1 + (1 | Country), data = dat, family = nbinom2()))
      Reg2 <- try(glmmTMB(as.numeric(Time_interval) ~ meanNPI + (1 | Country), data = dat, family = nbinom2()))
      Reg3 <- try(glmmTMB(as.numeric(Time_interval) ~ meanGMR + (1 | Country), data = dat, family = nbinom2()))
      Reg4 <- try(glmmTMB(as.numeric(Time_interval) ~ meanGMR + meanNPI + (1 | Country), data = dat, family = nbinom2()))
      # R2 <- r2_glmer(Reg)

      Summ <- lapply(list(Reg1, Reg2, Reg3, Reg4), function(model) {
        suppressWarnings(summary(model))
      })

      aic <- sapply(Summ, function(summary) {
        tryCatch(summary[["AICtab"]][["AIC"]], error = function(e) NA)
      })

      model <- eval(str2expression(paste0("Reg", which.min(aic))))

      Summ <- Summ[[which.min(aic)]]
      # Reg <- glmer(as.numeric(Time_interval) ~ meanNPI + meanGMR + (1 | Country), family = nbinom2(), data = dat)
      # Summ <- suppressWarnings(summary(Reg))
      #
      SummList <- list(
        VirName = VirName,
        model = model,
        lm = Summ[["call"]][["formula"]],
        # R2 = R2,
        AIC = Summ[["AICtab"]][["AIC"]],
        SummTable = Summ[["coefficients"]][["cond"]],
        Intercept = Summ[["coefficients"]][["cond"]][1, 1],
        Intercept_se = Summ[["coefficients"]][["cond"]][1, 2]
      ) # ,
      # Coef = c(Summ[["coefficients"]][["cond"]][2,1], Summ[["coefficients"]][["cond"]][3,1]),
      # Coef_se = c(Summ[["coefficients"]][["cond"]][2,2], Summ[["coefficients"]][["cond"]][3,2]))
    }
  )
  return(SummList)
}


#' This function builds a prediction model based on Calu.regression() and calculates the predicted value
#' @param dat a data frame, consist by various combine of OxC and GMR
#' @param coefficients coef from Calu.regression()
#' @param  intercept from Calu.regression()
#' @param VirName from Calu.regression()
#' @return
# Calu.pred <- function(dat, coefficients, intercept, VirName) {
#   dat_int <- cbind(1, dat)
#   pred <- as.matrix(dat_int) %*% c(intercept, coefficients)
#   return(bind_cols(list(dat, pred = pred, Virus_name = VirName)))
# }
Calu.pred2 <- function(dat, model, VirName, func = c("Liner", "NBM", "REM")) {
  switch(func,
    "Liner" = {
      pred <- predict(model, newdata = dat, interval = "prediction")
      table <- bind_cols(list(dat, pred, Virus_name = VirName)) %>%
        rename("pred" = "fit", "lci" = "lwr", "uci" = "upr", "OxC" = "meanNPI", "GMR" = "meanGMR")
    },
    "NBM" = {
      pred <- predict(model, newdata = dat, type = "response", se.fit = T)
      pred <- data.frame(fit = pred$fit, se = pred$se.fit) %>%
        mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se)
      table <- bind_cols(list(dat, pred, Virus_name = VirName)) %>%
        rename("pred" = "fit", "lci" = "lwr", "uci" = "upr", "OxC" = "meanNPI", "GMR" = "meanGMR")
    },
    "REM" = {
      # pred <- predict(model, newdata = dat, type = "response", se.fit = TRUE, re.form = NA)
      # pred <- data.frame(fit = pred$fit, se = pred$se.fit) %>%
      #   mutate(lwr = fit-1.96*se, upr = fit+1.96*se)
      pred <- predictInterval(merMod = model, newdata = dat, n.sims = 1000, .parallel = T, type = "probability")
      table <- bind_cols(list(dat, pred, Virus_name = VirName)) %>%
        rename("pred" = "fit", "lci" = "lwr", "uci" = "upr", "OxC" = "meanNPI", "GMR" = "meanGMR")
    }
  )
  return(table)
}


Calu.pred.specific <- function(dat, NPIPara) {
  table <- dat[["SummTable"]]
  NPIest <- ifelse("meanNPI" %in% rownames(table), dat[["SummTable"]]["meanNPI", 1], 0)
  NPIse <- ifelse("meanNPI" %in% rownames(table), dat[["SummTable"]]["meanNPI", 1], 0)

  GMRest <- ifelse("meanGMR" %in% rownames(table), dat[["SummTable"]]["meanGMR", 1], 0)
  GMRse <- ifelse("meanGMR" %in% rownames(table), dat[["SummTable"]]["meanGMR", 1], 0)

  set.seed(911)
  CoefMatrix <- matrix(
    c(
      rnorm(1000, dat[["SummTable"]][1, 1], dat[["SummTable"]][1, 2]),
      rnorm(1000, NPIest, NPIse),
      rnorm(1000, GMRest, GMRse)
    ),
    ncol = 1000, byrow = T
  )
  PredMatrix <- exp(NPIPara %*% CoefMatrix)

  n_rows <- nrow(NPIPara)
  mean_values <- apply(PredMatrix, 1, median)
  lci_values <- apply(PredMatrix, 1, quantile, probs = 0.05)
  uci_values <- apply(PredMatrix, 1, quantile, probs = 0.95)

  Report <- tibble(
    Virus_name = rep(dat[["VirName"]], each = n_rows),
    OxC = NPIPara[, 2], GMR = NPIPara[, 3],
    mean = mean_values,
    lci = lci_values,
    uci = uci_values
  )
  return(Report)
}

#' This function is used to plot based on the predicted value from Calu.pred(), producing a picture with OxC on the x-axis and GMR on the y-axis, with the colour representing the time interval of recirculation
#' @param dat a list, split by Virus_name
#' @return  a picture with OxC on the x-axis and GMR on the y-axis, with the colour representing the time interval of recirculation
plot.PredValue <- function(dat, save = F, path = NULL, width = 12, height = 6) {
  VirName <- unique(dat$Virus_name)
  title <- sprintf("Prediction of %s", VirName)
  fig <- ggplot(dat, aes(x = OxC, y = GMR)) +
    geom_tile(aes(fill = pred)) +
    scale_fill_gradient(low = "#074ba9", high = "#be134c") +
    labs(
      x = "OxCGRT index\nGovernment Response Index",
      y = "Google Mobility Report\nRetail and recreation percent change from baseline",
      fill = "Predicted value",
      title = title
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
    )
  print(fig)
  if (save == T) {
    ggsave(fig, file = paste0(path, "Pred_of_", VirName, ".pdf"), width = width, height = height)
  }
}



#' @param dat_modify
#' @param NPIRef
#' @param VirPair
Calu.TwoVirReportTable <- function(dat_modify, VirPair, NPIRef = c("last", "refday")) {
  switch(NPIRef,
    "last" = {
      ReportTable <- tibble(
        FirstVir = VirPair[1],
        SecVir = VirPair[2],
        FirstVirRecirTime = dat_modify[Virus_name == VirPair[1], ReCirculation_time],
        SecondVirRecirTime = dat_modify[Virus_name == VirPair[2], ReCirculation_time],
        RefDate = dat_modify[Virus_name == VirPair[1], Last_circ_time],
        FirstVirRecirInterval = FirstVirRecirTime - RefDate,
        Vir_VirTimeInterval = SecondVirRecirTime - FirstVirRecirTime,
        FirstVirPeakTime = dat_modify[Virus_name == VirPair[1], Peak_time],
        SecondVirPeakTime = dat_modify[Virus_name == VirPair[2], Peak_time],
        FirstVirPeakInterval = FirstVirPeakTime - RefDate,
        Vir_VirPeakInterval = SecondVirPeakTime - FirstVirPeakTime,
        Study_ID = unique(dat_modify[, Study_ID]),
        Country = unique(dat_modify[, Country])
      )
    },
    "refday" = {
      ReportTable <- tibble(
        FirstVir = VirPair[1],
        SecVir = VirPair[2],
        FirstVirRecirTime = dat_modify[Virus_name == VirPair[1], ReCirculation_time],
        SecondVirRecirTime = dat_modify[Virus_name == VirPair[2], ReCirculation_time],
        RefDate = dat_modify[Virus_name == VirPair[1], NPIDate],
        FirstVirRecirInterval = FirstVirRecirTime - RefDate,
        Vir_VirTimeInterval = SecondVirRecirTime - FirstVirRecirTime,
        FirstVirPeakTime = dat_modify[Virus_name == VirPair[1], Peak_time],
        SecondVirPeakTime = dat_modify[Virus_name == VirPair[2], Peak_time],
        FirstVirPeakInterval = FirstVirPeakTime - RefDate,
        Vir_VirPeakInterval = SecondVirPeakTime - FirstVirPeakTime,
        Study_ID = unique(dat_modify[, Study_ID]),
        Country = unique(dat_modify[, Country])
      )
    }
  )
  return(ReportTable)
}


#' This function is used to calculate the time interval between two viruses in each study
#' @param dat based on the total data set of each region
#' @param NPIRef whether to use the date of reaching the threshold of a certain NPI index as the reference time.
#'                If so, an additional data set of the reference date should be provided when passing in the data
#' @param NPIdat provide the NPI reference date, generated by Calu.NPIDate()
Calu.TwoVirInterval <- function(dat, NPIRef = c("last", "refday"), NPIdat = NULL) {
  dat_modify <- dat[Index_of_Wave == 1]
  ID <- unique(dat_modify[, Study_ID])
  print(ID)
  if (NPIRef == "refday") {
    if (is.null(NPIdat)) {
      stop("Provide NPIdat")
    } else {
      ID <- unique(dat_modify[, Study_ID])
      dat_modify <- merge.data.table(dat_modify, NPIdat, by = "Study_ID", all.x = T) %>%
        drop_na(NPIDate) %>%
        setDT()

      if (nrow(dat_modify) == 0) {
        warning(sprintf("!!! %s: No NPI info in this data set. The study was excluded !!!", ID))
        return(NULL)
      }
    }
  }

  VirPairList <- expand.grid(FirstVir = unique(dat_modify[, Virus_name]), SecondVir = unique(dat_modify[, Virus_name])) %>%
    setDT()
  VirPairList <- VirPairList[VirPairList$FirstVir != VirPairList$SecondVir, ][, lapply(.SD, as.character)]

  SplitList <- split(VirPairList, by = c("FirstVir", "SecondVir"))

  TwoVirTable <- do.call(
    rbind,
    lapply(SplitList, function(DatList) {
      VirPair <- DatList %>% unlist()
      Calu.TwoVirReportTable(dat_modify, VirPair = VirPair, NPIRef = NPIRef)
    })
  )

  return(TwoVirTable %>% arrange(FirstVir))
}



ReportTwoVir <- function(dat, index = c("peak", "last"), func = c("Liner", "NBM", "REM"), tidy = T,
                         save = F, path, width = 12, height = 10) {
  target <- switch(index,
    "last" = "Vir_VirTimeInterval",
    "peak" = "Vir_VirPeakInterval"
  )

  dat <- setDT(dat)

  Report2Vir <- dat[, Vir_ID := paste0(FirstVir, "_", SecVir)][, .(num = .N, mean = mean(eval(str2expression(target)), na.rm = T), sd = sd(eval(str2expression(target)), na.rm = T)),
    keyby = .(FirstVir, SecVir)
  ][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, c("mean", "lci", "uci") := .(round(mean, 0), round(lci, 0), round(uci, 0))][order(-mean)]

  VirOrder <- unique(Report2Vir$FirstVir)
  Report2Vir <- Report2Vir %>%
    mutate(
      across(ends_with("Vir"), as.factor),
      across(ends_with("Vir"), ~ fct_relevel(., VirOrder)),
      across(c(4:7), as.numeric)
    )

  # Report2Vir <- Report2Vir %>%
  #   mutate(across(ends_with("Vir"), as.factor),
  #          across(ends_with("Vir"), ~fct_relevel(., 'RV', 'sCoV', 'PIV', "PIV_1", "PIV_2", "PIV_3", "PIV_4", 'RSV', 'AdV',
  #                                                'MPV', "IAV", "H3N2", "H1N1", 'IBV')), # 'IFVA', 'IFVB', 'RSV', 'PIV', 'MPV', 'sCoV', 'RV', 'AdV'
  #          across(c(4:7), as.numeric))

  Mark <- max(Report2Vir$mean) + 50 - (max(Report2Vir$mean) %% 50)

  fig <- Report2Vir %>%
    mutate(mean = as.integer(mean)) %>%
    ggplot(., aes(x = fct_rev(SecVir), y = fct_rev(FirstVir), fill = mean)) +
    geom_tile(data = subset(Report2Vir), color = "white", lwd = 1.5, linetype = 1) +
    geom_text(data = subset(Report2Vir), aes(label = as.integer(mean)), color = "white", size = 6, fontface = "bold") +
    scale_fill_gradient2(
      low = "#00468BFF", mid = "#ADB6B6FF", high = "#ED0000FF",
      limits = c(-Mark, Mark),
      breaks = c(-Mark, -Mark / 2, 0, Mark / 2, Mark),
      midpoint = 0, na.value = NA
    ) +
    coord_fixed() +
    labs(fill = "Time interval") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 16, face = "bold", colour = "black"),
      axis.title = element_blank(),
      legend.title = element_text(size = 16, face = "bold", colour = "black"),
      legend.text = element_text(size = 16, colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
    )

  suppressWarnings(print(fig))

  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }

  if (tidy == T) {
    ReportTable <- Report2Vir %>%
      filter(mean >= 0) %>%
      arrange(-mean) %>%
      mutate(ci = paste0(mean, " (", lci, ", ", uci, ")")) %>%
      dplyr::select(FirstVir, SecVir, num, ci)
    return(ReportTable)
  } else {
    return(Report2Vir)
  }
}


plot.TwoVir <- function(dat, path, width = 12, height = 10, save = T) {
  ID <- unique(str_extract(dat$ID, "^[^-]+")) # "[:alpha:]*"

  fig <- ggplot(dat) +
    geom_hline(yintercept = c(0, 365, 365 * 2, 365 * 3), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
    geom_errorbar(aes(x = ID, ymin = lci, ymax = uci), linewidth = 1, width = 0.4, colour = "#08203e") +
    geom_point(aes(x = ID, y = mean), colour = "#852237", size = 3) +
    scale_y_continuous(
      breaks = c(0, 365, 365 * 2, 365 * 3), # limits = c(-100, 750),
      labels = c("First virus resurge", "1 year", "2 years", "3 years")
    ) + # limits = c(0, 750), expand = c(0, 0)
    labs(title = NULL, x = NULL, y = NULL) + # x = "Virus pair", y = "Time interval"
    theme_bw() +
    theme(
      axis.text = element_text(size = 32, colour = "black"),
      axis.title = element_text(size = 24, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
    ) +
    coord_flip() #+
  # facet_wrap(vars(SecVir), ncol = 4)

  if (save == T) {
    ggsave(fig, file = paste0(path, "TwoVir_", ID, ".pdf"), width = width, height = height)
  }
  return(fig)
}


Calu.SingleVir.group <- function(dat, target = c("Time_interval", "Peak_interval"), func = c("Liner", "NBM", "REM"), excludeNA = T, plot = T, save = F, path = NULL, width = 12,
                                 height = 6, report = F) {
  target <- substitute(target)
  switch(target,
    "Time_interval" = {
      switch(func,
        "Liner" = {
          RecircDat <- dat[, .(num = .N, mean = mean(Time_interval, na.rm = T), sd = sd(Time_interval, na.rm = T)), keyby = .(Virus_name, Index_of_Wave, hemisphere)][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")), keyby = .(Virus_name, hemisphere)][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "NBM" = {
          RecircDat <- dat[!is.na(Time_interval)][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, hemisphere, hemisphere)][, Res := lapply(data, NBM.cal, index = Time_interval)][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name, hemisphere)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * se, Cumday + 1.96 * se)]
        },
        "REM" = {
          RecircDat <- dat[!is.na(Time_interval)][, num := .N, by = .(Virus_name, Index_of_Wave, hemisphere)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, hemisphere)][, Res := lapply(data, REM.cal, index = Time_interval)][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name, hemisphere)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * se, Cumday + 1.96 * se)]
        }
      )
    },
    "Peak_interval" = {
      switch(func,
        "Liner" = {
          RecircDat <- dat[, .(num = .N, mean = mean(Peak_interval, na.rm = T), sd = sd(Peak_interval, na.rm = T)), keyby = .(Virus_name, Index_of_Wave, hemisphere)][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))][, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")), keyby = .(Virus_name)][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
        },
        "NBM" = {
          RecircDat <- dat[!is.na(Peak_interval)][, num := .N, by = .(Virus_name, Index_of_Wave, hemisphere)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, hemisphere)][, Res := lapply(data, NBM.cal, index = Peak_interval)][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * se, Cumday + 1.96 * se)]
        },
        "REM" = {
          RecircDat <- dat[!is.na(Peak_interval)][, num := .N, by = .(Virus_name, Index_of_Wave, hemisphere)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, hemisphere)][, Res := lapply(data, REM.cal, index = Peak_interval)][, data := NULL]
          RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
          RecircDat <- RecircDat[, ":="(Cumday = cumsum(as.numeric(mean)),
            ci = paste0(round(mean, digits = 0), "(", round(lci, 0), ", ", round(uci, 0), ")")),
          keyby = .(Virus_name)
          ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * se, Cumday + 1.96 * se)]
        }
      )
    }
  )

  if (excludeNA == T) {
    RecircDat <- RecircDat %>% drop_na()
  }

  if (plot == T) {
    if (target == "Time_interval") {
      title <- "Time interval between each start of circulation"
    } else {
      title <- "Time interval between each peak of circulation"
    }

    fig <- ggplot(RecircDat) +
      geom_hline(yintercept = c(0, 365, 365 * 2, 365 * 3, 365 * 4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
      geom_errorbar(aes(x = fct_rev(Virus_name), ymin = Cumlci, ymax = Cumuci, group = hemisphere, colour = hemisphere), linewidth = 1, width = 0.4, position = position_dodge(width = 0.5)) + # , colour = "#08203e"
      geom_point(aes(x = fct_rev(Virus_name), y = Cumday, group = hemisphere, colour = hemisphere), size = 3, position = position_dodge(width = 0.5)) + # , colour = "#852237"
      scale_color_manual(values = c("South hemisphere" = "#4dbbd5", "North hemisphere" = "#d62728")) +
      scale_y_continuous(
        limits = c(0, 1900), breaks = c(0, 365, 365 * 2, 365 * 3, 365 * 4),
        labels = c("Last circulation", "1 year", "2 year", "3 year", "4 year"), expand = c(0, 0)
      ) +
      labs(title = title, x = "Virus", y = "Time interval", color = "Location") +
      theme_bw() +
      theme(
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, face = "bold", colour = "black"),
        plot.title = element_text(size = 18, face = "bold", colour = "black"),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
      ) +
      coord_flip()

    # geom_line(data = O_GData, aes(date, {{GMRindex}}, color = "GMR"), linewidth = 1.2, linetype = 4, alpha = 0.3) +
    #   scale_color_manual(values = c("OxCGRT" = "#4dbbd5", "GMR" = "#d62728")) +
    #   scale_fill_manual(values = c("#a6cee3", "#2ca02c" , "#9467bd", "#ff7f0e", "#1b9e77", "#8c564b", "#17becf", "#e377c2","#bcbd22",
    #                                         "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#1f77b4"))


    print(fig)
  }

  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }

  if (report == T) {
    ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
    ReportTable <- setnames(
      ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"),
      c("Virus", "Index of epidemics", "Report number", "95% Confidence interval")
    )

    return(ReportTable)
  } else {
    return(RecircDat)
  }
}

Calu.meta <- function(data, yi, sei, method, num) {
  MetaReg <- rma(yi = yi, sei = sei, method = method, data = data)
  return(data.frame(
    num2 = unique(num),
    mean_meta = MetaReg[["b"]],
    se_meta = MetaReg[["se"]]
  ))
}

Calu.counterfactual <- function(num, rep.num, mean, se) {
  SampleDat <- matrix(rnorm(n = num * rep.num, mean = mean, sd = se), nrow = num)
  return(data.frame(
    median = apply(SampleDat, 1, function(x) quantile(x, probs = 0.5)),
    lci = apply(SampleDat, 1, function(x) quantile(x, probs = 0.025)),
    uci = apply(SampleDat, 1, function(x) quantile(x, probs = 0.975))
  ))
}

Calu.counterfactual2 <- function(rep.num, mean, se) {
  SampleDat <- matrix(rnorm(n = 1, mean = mean, sd = se), nrow = 1)
  return(data.frame(
    median = apply(SampleDat, 1, function(x) quantile(x, probs = 0.5)),
    lci = apply(SampleDat, 1, function(x) quantile(x, probs = 0.025)),
    uci = apply(SampleDat, 1, function(x) quantile(x, probs = 0.975))
  ))
}

Calu.diff <- function(mean_meta, se_meta, cal.mean, se, rep.num = 1000) {
  BeforeMatrix <- matrix(rnorm(n = rep.num, mean = mean_meta, sd = se_meta), nrow = 1) %>% exp()
  AfterMatrix <- matrix(rnorm(n = rep.num, mean = cal.mean, sd = se), nrow = 1) %>% exp()
  DiffMatrix <- AfterMatrix - BeforeMatrix
  return(data.frame(
    median = quantile(DiffMatrix, probs = 0.5),
    lci = quantile(DiffMatrix, probs = 0.025),
    uci = quantile(DiffMatrix, probs = 0.975)
  ))
}

Calu.AllDiff <- function(data, rep.num) {
  Before <- t(apply(data, 1, function(dt) rnorm(rep.num, mean = dt["mean_meta"], sd = dt["se_meta"]))) %>%
    exp() %>%
    colSums()
  After <- t(apply(data, 1, function(dt) rnorm(rep.num, mean = dt["cal.mean"], sd = dt["se"]))) %>%
    exp() %>%
    colSums()

  AllDiff <- After - Before
  return(data.frame(
    median = quantile(AllDiff, probs = 0.5),
    lci = quantile(AllDiff, probs = 0.025),
    uci = quantile(AllDiff, probs = 0.975)
  ))
}

Calu.CumDiff <- function(data, rep.num) {
  Before <- t(apply(data, 1, function(dt) rnorm(rep.num, mean = dt["mean_meta"], sd = dt["se_meta"]))) %>% exp()
  Before <- apply(Before, 2, cumsum)
  After <- t(apply(data, 1, function(dt) rnorm(rep.num, mean = dt["cal.mean"], sd = dt["se"]))) %>% exp()
  After <- apply(After, 2, cumsum)

  AllDiff <- After - Before
  CItable <- t(apply(AllDiff, 1, quantile, c(0.025, 0.5, 0.975)))
  colnames(CItable) <- c("c_lci", "c_median", "c_uci")
  return(data.table(CItable))
}


Calu.SpeAcc <- function(data, rep.num = 10) {
  dat <- copy(data)
  dat <- dat[, newmean := shift(cal.mean, n = -1, fill = 0)][, newse := shift(se, n = -1, fill = 0)]
  BeforeMatrix <- t(apply(dat, 1, function(dt) rnorm(rep.num, mean = dt["mean_meta"], sd = dt["se_meta"]))) %>% exp()
  AfterMatrix <- t(apply(dat, 1, function(dt) rnorm(rep.num, mean = dt["newmean"], sd = dt["newse"]))) %>% exp()

  DiffMatrix <- AfterMatrix - BeforeMatrix
  CItable <- t(apply(DiffMatrix, 1, quantile, c(0.025, 0.5, 0.975)))
  colnames(CItable) <- c("lci", "median", "uci")

  BeforeMatrix2 <- t(apply(dat, 1, function(dt) rnorm(rep.num, mean = dt["mean_meta"], sd = dt["se_meta"]))) %>% exp()
  AfterMatrix2 <- t(apply(dat, 1, function(dt) rnorm(rep.num, mean = dt["cal.mean"], sd = dt["se"]))) %>% exp()

  SumMateix <- (AfterMatrix + BeforeMatrix) - (BeforeMatrix2 + AfterMatrix2)
  AccTable <- t(t(apply(SumMateix, 1, quantile, c(0.5))))
  colnames(AccTable) <- "acc"
  AccTable <- data.table(AccTable)

  return(data.table(CItable, AccTable))
}


Creat.Table <- function(dat) {
  VirusName <- dat[["VirName"]]

  SummTable <- dat[["SummTable"]] %>% as.data.frame()

  GMR <- ifelse(is.na(SummTable["meanGMR", "Estimate"]), "–",
    paste0(
      format(round(SummTable["meanGMR", "Estimate"], 0), nsmall = 2, digits = 2), "(",
      format(round(SummTable["meanGMR", "Estimate"] - 1.96 * SummTable["meanGMR", "Std. Error"], 0), nsmall = 2, digits = 2), ", ",
      format(round(SummTable["meanGMR", "Estimate"] + 1.96 * SummTable["meanGMR", "Std. Error"], 0), nsmall = 2, digits = 2), ")"
    )
  )

  OxC <- ifelse(is.na(SummTable["meanNPI", "Estimate"]), "–",
    paste0(
      format(round(SummTable["meanNPI", "Estimate"], 2), nsmall = 2, digits = 2), "(",
      format(round(SummTable["meanNPI", "Estimate"] - 1.96 * SummTable["meanNPI", "Std. Error"], 2), nsmall = 2, digits = 2), ", ",
      format(round(SummTable["meanNPI", "Estimate"] + 1.96 * SummTable["meanNPI", "Std. Error"], 2), nsmall = 2, digits = 2), ")"
    )
  )

  R_square <- format(dat[["R_square"]], nsmall = 2, digits = 2)

  return(data.table(
    "Virus name" = VirusName,
    "Google mobility report" = GMR,
    OxCGRT = OxC,
    "R square" = R_square
  ))
}


plot.speed <- function(data, save = F, path = NULL, width = 12, height = 6) {
  BeforePandemic <- data[, num2 := max(Index_of_Wave), by = .(Virus_name)][Index_of_Wave < -1, .(Virus_name, Index_of_Wave, cal.mean, se, num2)][, .(data = list(.SD)), by = .(Virus_name)][, Res := lapply(data, function(dt) Calu.meta(yi = dt$cal.mean, sei = dt$se, method = "FE", num = dt$num2, data = dt))][, !("data")]

  AfterPandemic <- data[Index_of_Wave > 0, .(Virus_name, Index_of_Wave, cal.mean, se)][, .(data = list(.SD)), by = .(Virus_name)]

  MergedData <- merge(unnest(BeforePandemic, cols = c(Res)), unnest(AfterPandemic, cols = c(data)), all.y = T) %>% setDT()

  CaluSpeed <- MergedData[, .(data = list(.SD)), by = .(Virus_name)][, cumdiff := lapply(data, function(dt) Calu.CumDiff(data = dt, rep.num = 1000))][, speed := lapply(data, function(dt) Calu.SpeAcc(data = dt, rep.num = 1000))][, !("data")]
  CaluSpeed <- unnest(CaluSpeed, cols = c(cumdiff, speed)) %>% setDT()

  PlotDat <- CaluSpeed[, .SD[-c(1, .N)], by = Virus_name] %>% drop_na()

  # CorRes <- cor.test(x = CaluSpeed[, .SD[-1], by = Virus_name]$acc,
  #                    y = CaluSpeed[, .SD[-1], by = Virus_name]$c_median, method = "spearman")

  fig <- ggplot(PlotDat, aes(x = acc, y = c_median)) +
    geom_smooth(method = lm) +
    geom_path(aes(colour = Virus_name), linewidth = 1) +
    geom_point(size = 2.5) +
    labs(
      x = "Difference between two neighboring onset-onset interval",
      y = "Difference between the observed and expected onsets", color = "Virus name"
    ) +
    scale_color_manual(values = c("#a6cee3", "#2ca02c", "#9467bd", "#ff7f0e", "#1b9e77", "#bcbd22", "#e7298a", "#e6ab02", "#1f77b4")) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 16, colour = "black"),
      axis.title = element_text(size = 20, face = "bold", colour = "black"),
      legend.title = element_text(size = 16, face = "bold", colour = "black"),
      legend.text = element_text(size = 14, colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
    )

  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }
}



Calu.Percent <- function(dat, target = c("Time_interval", "Peak_interval")) {
  dat <- dat[Index_of_Wave < 0, Index_of_Wave := -2]

  RecircDat <- suppressWarnings(dat[!is.na(eval(str2expression(target)))][, num := .N, by = .(Virus_name, Index_of_Wave)][num != 1][, .(data = list(.SD)), by = .(Virus_name, Index_of_Wave, num)][, Res := lapply(data, REM.cal, index = target)][, data := NULL])

  RecircDat <- unnest(RecircDat, cols = c(Res)) %>% setDT(.)
  RecircDat_Old <- copy(RecircDat)[Index_of_Wave == -2, c(1, 3, 5, 6)]
  colnames(RecircDat_Old) <- c("Virus_name", "num_old", "mean_old", "se_old")
  RecircDat_New <- copy(RecircDat)[Index_of_Wave > 0, c(1, 2, 3, 5, 6)]

  PercentIncrease <- merge(RecircDat_Old, RecircDat_New, by = "Virus_name", all = T)

  PercentIncrease <- na.omit(PercentIncrease)

  PercentIncrease <- PercentIncrease[, .(
    old = list(list(mean_old = mean_old, se_old = se_old)),
    new = list(list(cal_mean = cal.mean, se = se))
  ), by = .(Virus_name, Index_of_Wave)]
  PercentIncrease <- PercentIncrease[, OldSample := lapply(1:.N, function(i) {
    exp(rnorm(1000, mean = old[[i]]$mean_old, sd = old[[i]]$se_old))
  })][, NewSample := lapply(1:.N, function(i) {
    exp(rnorm(1000, mean = new[[i]]$cal_mean, sd = new[[i]]$se))
  })]

  PercentIncrease <- PercentIncrease[, PercentIncr := lapply(1:.N, function(i) {
    quantile((NewSample[[i]] - OldSample[[i]]) / OldSample[[i]] * 100, c(.05, .5, 0.95))
  })]

  PercentIncrease <- PercentIncrease[, ":="(lci = round(sapply(PercentIncr, function(x) x[1]), 0),
    PI = round(sapply(PercentIncr, function(x) x[2]), 0),
    uci = round(sapply(PercentIncr, function(x) x[3]), 0))][, CI := paste0(PI, " (", lci, ", ", uci, ")")][, ":="(old = NULL, new = NULL, OldSample = NULL, NewSample = NULL, PercentIncr = NULL)]

  setcolorder(PercentIncrease, c("Virus_name", "Index_of_Wave", "PI", "lci", "uci", "CI"))

  # PercentIncrease <- PercentIncrease[, PercentIncr := (exp(cal.mean-mean_old)-1)*100
  # ][, PI_se := PercentIncr*sqrt(se_old^2+se^2)
  # ][, lci := PercentIncr - 1.96 * PI_se
  # ][, uci := PercentIncr + 1.96 * PI_se
  # ][, c("PercentIncr", "lci", "uci") := .(round(PercentIncr, 2), round(lci, 2), round(uci, 2))
  # ][, CI := paste0(PercentIncr, "(", lci, ", ", uci, ")")]

  OldWave <- copy(RecircDat_Old)
  OldWave <- OldWave[, ":="(mean = round(exp(mean_old), 0),
    lci = round(exp(mean_old - 1.96 * se_old), 0),
    uci = round(exp(mean_old + 1.96 * se_old), 0),
    mean_old = NULL, se_old = NULL, num_old = NULL)]
  OldWave <<- OldWave

  return(PercentIncrease)
}
