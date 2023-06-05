
#' This function is used in the data cleaning stage. Delete blank in the GMR data set
#' @param x data.table of Google Mobility Report
#' @return cleaner GMR data
GMR.CheckBlank <- function(x) {

  grepl("[[:alpha:]]|[[:digit:]]|[[:punct:]]", x, perl = TRUE) # 检查GMR中的每一元素是否为字母，数字或者字符。perl是一个拓展的Regex
}


#' This function is used in the data cleaning stage.
#' @param dat cleaned GMR data
#' @return a data set that extends the study time to a specified range. NA in Location, sub_region_1 and etc. were filled 
expand.date <- function(dat){
  Count <- dat$Country[1]
  merge_dat <- merge(DateExpand[Country == Count], dat, by.x = c("Country", "date"), by.y = c("Country", "date"), all = T) %>% 
    fill(Location, sub_region_1, sub_region_2, Study_ID, .direction = "downup")
  return(merge_dat)
}


#' Plot the trend of OxCGRT
#' @param dat cleaned OxCGRT data
#' @param index the index want to plot. e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @return a plot
OxCGRT.plot <- function(dat, index, save = F, path = NULL){
  index_name <- rlang::quo_text(enquo(index)) # 这里的quo_text用来将index再重新解析成字符串
  location = unique(dat$Country)
  fig <- ggplot() +
    geom_line(data = dat, aes(date, {{index}}), linewidth = 1.2, linetype = 2, alpha = 0.7, colour = "#223a58") + 
    scale_x_date(name = "Date", date_labels = "%Y\nW%U", breaks = "8 weeks") + 
    scale_y_continuous(name = sprintf("OxCGRT index\n%s", 
                                      str_replace_all(str_extract(index_name, regex("[A-Za-z]+")), "\\B[A-Z]", " \\0")),
                       limits = c(0,100))+  
 # 后面一段用来增加空格
    labs(title = location) +
    theme_bw() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, face = "bold", colour = "black"),
          plot.title = element_text(size = 18, face = "bold", colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"),
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1))
  
  if (save == T) {
    ggsave(fig, file = paste0(path, location,"_OxCGRT" , ".pdf"), width = 10, height = 6)
  }
  
  print(fig)
}


#' Plot the trend of GMR
#' @param dat cleaned GMR data
#' @param index the index want to plot. e.g. retail_and_recreation_percent_change_from_baseline
#' @return a plot
GMR.plot <- function(dat, index, save = F, path = NULL){
  location = unique(dat$Country)
  fig <- ggplot() +
    geom_line(data = dat, aes(date, {{index}}), linewidth = 1.2, linetype = 2, alpha = 0.7, colour = "#223a58") + 
    scale_x_date(name = "Date", date_labels = "%Y\nW%U", breaks = "8 weeks") + 
    scale_y_continuous(name = "Google Mobility Report\nRetail and recreation percent change from baseline")+  
    labs(title = location) +
    theme_bw() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, face = "bold", colour = "black"),
          plot.title = element_text(size = 18, face = "bold", colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"),
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1))
  
  if (save == T) {
    ggsave(fig, file = paste0(path, location,"_GMR" , ".pdf"), width = 10, height = 6)
  }
  
  print(fig)
}


#' This function is used to generate the plot title, like Ottawa, Province of Ontario, Canada
#' @param dat plot data set. Only in one study
#' @return a character of plot title
plot.GetTitle <- function(dat){
  Country = unique(dat$Country)
  if(Country == "Taiwan") {
    return("Taiwan, China")
  }
  
  Location = unique(dat$Location)
  if (is.na(Location)) {
    return(Country)
  }else {
    return(paste0(Location, ", ", Country))
  }
}


#' This function is used to scale data to 0-1
#' @param multiply amplify the data
#' @return a series of number
ScaleToOne <- function(x, multiply = 1){
  # x_shift = x+0.001
  # y=log(na.omit(x_shift))
  # return((y - min(y))/(max(y) - min(y)) * multiply)
  #return((x - mean(y)) / sd(y))
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
plot.GetPolygonDat <- function(dat, ID, multiply = 1){
  AfterPandemic <- dat %>% 
    filter(Study_ID == ID) %>% 
    select(Study_ID, Virus_ID, Virus_name, Index_of_Wave, ReCirculation_time, Peak_time, ReCirculation_rate, Peak_occurs) %>% 
    mutate(Virus_ID = as.numeric(str_extract(Virus_ID, "\\d.*"))) %>% 
    arrange(Virus_ID, Index_of_Wave)
  
  BeforePandemic <- dat %>% 
    filter(Study_ID == ID) %>% 
    select(Study_ID, Virus_ID, Virus_name, Index_of_Wave, ReCirculation_time, Peak_time, ReCirculation_rate, Peak_occurs, 
           Last_circ_time, Last_peak_time, Last_circ_rate) %>% 
    mutate(Index_of_Wave = if_else(Index_of_Wave %in% c(0,1), -1, Index_of_Wave),
           Virus_ID = as.numeric(str_extract(Virus_ID, "\\d.*"))) %>% 
    filter(Index_of_Wave == -1) %>% 
    select(-ReCirculation_time, -Peak_time, -ReCirculation_rate) %>% 
    rename(ReCirculation_time = Last_circ_time,
           Peak_time = Last_peak_time,
           ReCirculation_rate = Last_circ_rate)
  
  PosiLable <- seq(5, 90, length.out = length(unique(BeforePandemic$Virus_name))) # length.out 用于在指定的范围内生成指定数量个数字
  
  PolygonDat <- bind_rows(list(AfterPandemic, BeforePandemic)) %>% 
    pivot_longer(cols = ReCirculation_time:Peak_time, names_to = "label", values_to = "time") %>% 
    mutate(n = ifelse(label == "Peak_time", 2, 1)) %>% 
    uncount(n) # 用来将包含Peak_time的行复制一行，形成三角形的底边
  
  PosiDat <- PolygonDat %>% select(Study_ID, Virus_ID, Virus_name) %>% distinct() %>% arrange(Virus_name) %>% mutate(posi = rev(PosiLable))
  
  PolygonDat <- PolygonDat %>% left_join(PosiDat, by = join_by(Study_ID, Virus_ID, Virus_name)) %>% 
    mutate(index = row_number(), #用来根据是否可整除将底边对应的一个坐标归一化，另一个变为0
           ReCirculation_rate_Scale = if_else(label == "ReCirculation_time", 0, 
                                              if_else(index%%2 == 0 & label != "ReCirculation_time", 0, ScaleToOne(ReCirculation_rate, multiply))),
           ReCirculation_rate_Scale = ReCirculation_rate_Scale + posi) %>% 
    group_by(Study_ID, Virus_ID, Index_of_Wave) %>%
    mutate(group = cur_group_id()) # 增加每组的编号，用于画成单独的三角形
  
  return(PolygonDat)
}


#' This function is used to generate the plot of epidemic trend
#' @param O_GData a list of OxCGMR data set or in one Study_ID. The list should split by the "Study_ID" column
#' @param VirData input the All_Vir data set or All_Vir data in one Study_ID. For aesthetics, some main virus coule be filtered and "Virus_name" column should converted into factor and relevel
#' @param OxCindex index of OxCGRT data, e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @param GMRindex index of GMR data, e.g. retail_and_recreation_percent_change_from_baseline
#' @param multiply a parameter in ScaleToOne(). To generate higher triangle in plot
#' @return a plot
plot.CountryTrend <- function(O_GData, VirData, OxCindex, GMRindex, multiply = 8, 
                              save = F, path = NULL, width = 12, height = 6){
  
  ID <- unique(O_GData$Study_ID)
  print(ID)
  
  PolygonDat <- VirData %>% filter(Study_ID == ID) %>% plot.GetPolygonDat(., ID, multiply = multiply)
  
  # Get end date of study. Used to add one vertical line in plot
  EndDate <- VirData %>% filter(Study_ID == ID) %>% select(Study_Period) %>% distinct() %>% str_extract(., "(?<=-).*")
  
  title <- plot.GetTitle(O_GData)
  
  PosiLable <- seq(5, 90, length.out = length(unique(PolygonDat$Virus_name))) 
  # 5和90是最低和最高的位置，可变。length.out 用于在指定的范围内生成指定数量个数字，后一个unique操作用于识别出这个研究有多少个病毒
  
  # get the name of each virus
  LableDat <- PolygonDat %>% ungroup() %>% arrange(time) %>% 
    select(Virus_name, time, posi) %>% 
    distinct(Virus_name, .keep_all = T) %>% 
    mutate(time = min(time)-60, 
           posi = posi+4)
  
  fig <- ggplot() +
      # geom_segment(data = SegmentDat, aes(x = ReCirculation_time, xend = Peak_time, y = Virus_ID, yend = Virus_ID, color = factor(Virus_name))) +
      # geom_segment(data = SegmentDat, aes(x = Peak_time, xend = Peak_time, Virus_ID, yend = (ReCirculation_rate/10), color = (Virus_name), size = ReCirculation_rate), linewidth = 3) +
      geom_vline(xintercept = as.Date(EndDate), linewidth = 1, linetype = 2, colour = "#bc3b29") + 
      geom_vline(xintercept = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")), linewidth = 0.5, linetype = 2, alpha = 0.7) +
      geom_hline(yintercept = 100 * 100/250, linewidth = 1, linetype = 2, colour = "grey75") +
      geom_hline(yintercept = c(PosiLable, PosiLable + multiply), linewidth = 0.3, linetype = 3, alpha = 0.5) +
      geom_hline(yintercept = c(PosiLable + multiply), linewidth = 0.3, linetype = 3, alpha = 0.5) +
      geom_line(data = O_GData, aes(date, {{OxCindex}}, color = "OxCGRT"), linewidth = 1.2, linetype = 2, alpha = 0.6) + 
      geom_line(data = O_GData, aes(date, {{GMRindex}}, color = "GMR"), linewidth = 1.2, linetype = 4, alpha = 0.3) +
    scale_color_manual(values = c("OxCGRT" = "#4dbbd5", "GMR" = "#d62728")) +
      geom_polygon(data = PolygonDat, aes(x = time, y = ReCirculation_rate_Scale, group = group, fill = Virus_name), 
                   color = "grey25", alpha = 0.75, show.legend = F) +
      scale_x_date(name = "Date", date_labels = "%Y\nW%U", breaks = "8 weeks") + 
      scale_y_continuous(name = "OxCGRT index\nGovernment Response Index", 
                         breaks = c(0, 25, 50, 75, 100), labels = c("0", "25", "50", "75", "100"), limits = c(0, 100), expand = c(0,0.1),
                         sec.axis = sec_axis(~., name = "Google Mobility Report\nRetail and recreation percent change from baseline",
                                             breaks = seq(0, 100, 20), labels = paste0(seq(0, 250, 50)))) + 
      scale_fill_manual(values = c("#a6cee3", "#2ca02c" , "#9467bd", "#ff7f0e", "#1b9e77", "#8c564b", "#17becf", "#e377c2","#bcbd22", 
                                            "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#1f77b4")) + 
                                              geom_label(data = LableDat, aes(x = time, y = posi, label = Virus_name), size = 6) + 
      labs(title = title, color = "") +
      theme_bw() +
      theme(panel.grid=element_blank(), 
            axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 18, face = "bold", colour = "black"),
            plot.margin = unit(c(0.5,1,0.5,1),"cm"))
     
  
  suppressWarnings(print(fig))
  
  if (save == T) {
    ggsave(fig, file = paste0(path, ID, "_", title, ".pdf"), width = width, height = height)
  }
}


#' This function is used to generate the plot of epidemic trend and report table
#' @param dat analysis data set e.g. MainDat
#' @param target which target to use, Time_interval and Peak_interval
#' @param excludeNA whether exclude NA in the RecircDat. Mainly in some Virus with many cirulation
#' @param report generate a tidy report table
#' @return a plot and a table 
Calu.SingleVir <- function(dat, target = c("Time_interval", "Peak_interval"), excludeNA = T, plot = T, save = F, path = NULL, width = 12, 
                           height = 6, report = F) {
  target <- substitute(target)
  # 在data.table中需要使用substitute和eval才能得到与!!或者{{}}相同的效果
  RecircDat <- dat[, .(num = .N, mean = mean(eval(target), na.rm = T), sd = sd(eval(target), na.rm = T)), keyby = .(Virus_name, Index_of_Wave)
  ][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))
  ][, ':='(Cumday = cumsum(as.integer(mean)), 
           ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")), keyby = .(Virus_name)
  ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
  
  
  if (excludeNA == T) {
    RecircDat <- RecircDat %>% drop_na()
  }
  
  if (plot == T) {
    if (target == "Time_interval") {
      title <- "Time interval between each start of circulation"
    } else{
      title <- "Time interval between each peak of circulation"
    }
    
    fig <- ggplot(RecircDat) +
      geom_hline(yintercept = c(0, 365, 365*2, 365*3, 365*4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
      geom_errorbar(aes(x = fct_rev(Virus_name), ymin = Cumlci, ymax = Cumuci), linewidth = 1, width = 0.4, colour = "#08203e") +
      geom_point(aes(x = fct_rev(Virus_name), y = Cumday), colour = "#852237", size = 3) +
      scale_y_continuous(limits = c(0, 1900), breaks = c(0, 365, 365*2, 365*3, 365*4), 
                         labels = c("Last circulation", "1 year", "2 year", "3 year", "4 year"), expand = c(0, 0)) +
      labs(title = title, x = "Virus", y = "Time interval") +
      theme_bw() +
      theme(axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 18, face = "bold", colour = "black"),
            plot.margin = unit(c(0.5,1,0.5,1),"cm")) +
      coord_flip()
    
    print(fig)
  }
  
  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }
  
  if (report == T) {
    ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
    ReportTable <- setnames(ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"), 
                            c("Virus", "Index of wave", "Report number", "95% Confidence interval"))
    
    return(ReportTable)
  }else{
    return(RecircDat)
  }
}


#' This function is used to generate the reference date of OxC or GMR
#' @param dat OxCGMR data set. In the world level, mean of each index should be calculate. 
#' @param Discount threshold of NPI discount
#' @param type calculate in OxCGRT or GMR
#' @param buffer If the NPI returns above the threshold range within the buffer time, this time is not calculated
#' @param index index of OxCGRT data, e.g. GovernmentResponseIndex_WeightedAverage_ForDisplay
#' @return a Date, indicate the reference date
Calu.NPIDate <- function(dat, Discount = 0.9, type = c("OxC", "GMR"), buffer = 28, index, Cutoff = 365) {
  index_name <- substitute(index)
  switch (type,
    "OxC" = {
      CutoffDate <- as.Date(dat[!is.na(dat[[index_name]]), date][1]) + Cutoff
      # CutoffDate 这个用来找一个限制的天数，因为在有些地方GMR或OxC最高的位置不一定在2020年
      SubsetDat <- subset(dat, date <= CutoffDate)
      MaxDate <- as.Date(SubsetDat[which.max(SubsetDat[[index_name]]), date])
      MaxOxC <- max(SubsetDat[[index_name]], na.rm = TRUE)
      DiscountOxC <- Discount * MaxOxC
    
      NPI_Table <- dat[, .(date, lable = (eval(index_name) <= DiscountOxC))][
        is.na(lable), lable := FALSE
      ][
        date > MaxDate, .(date, lable, checkNPI = frollsum(lable, buffer, na.rm = TRUE, align = "left"))]
      # 这里的28是对所有的logic求和，当有28个T的时候认为随后的28天都比这一天高，那么这一天就是要取到的阈值
    
      # 上述代码的dplyr版本
      # OxC_GMR_global %>%
      #   select(date, GovernmentResponseIndex_WeightedAverage_ForDisplay) %>%
      #   mutate(lable = OxC_GMR_global$GovernmentResponseIndex_WeightedAverage_ForDisplay <= DiscountOxC) %>%
      #   filter(date > MaxDate) %>%
      #   mutate(checkNPI = slide(lable, sum, .before = 0, .after = 27))
      return(tibble(Study_ID = unique(dat[, Study_ID]),
                    NPIDate = as.Date(NPI_Table[checkNPI == buffer, date][1])))
      },
    
    "GMR" = {
      CutoffDate <- as.Date(dat[!is.na(dat[[index_name]]), date][1]) + Cutoff
      # CutoffDate 这个用来找一个限制的天数，因为在有些地方GMR或OxC最高的位置不一定在2020年
      SubsetDat <- subset(dat, date <= CutoffDate)
      MaxDate <- as.Date(SubsetDat[which.max(SubsetDat[[index_name]]), date])
      MaxGMR <- max(SubsetDat[[index_name]], na.rm = TRUE)
      DiscountGMR <- Discount * MaxGMR
      DiscountDate <- dat[date >= MaxDate & eval(index_name) <= DiscountGMR, date][1]
      # if (MaxGMR<0) {
      #   DiscountGMR <- Discount * MaxGMR
      # } else{
      #   DiscountGMR <- (1-Discount+1) * MaxGMR # discount这里对于GMR要用原始数据，用加100的话需要变成1.2
      # }
    
      NPI_Table <- dat[, .(date, lable = (eval(index_name) <= DiscountGMR))][
        is.na(lable), lable := FALSE
      ][
        date > DiscountDate, .(date, lable, checkNPI = frollsum(lable, buffer, na.rm = TRUE, align = "left"))]
      return(tibble(Study_ID = unique(dat[, Study_ID]), 
                    NPIDate = as.Date(NPI_Table[checkNPI == buffer, date][1])))
    }
  )
}


#' This function is similar to Calu.SingleVir(), but applies to the reference OxC_GMR dataset. The reference date comes from the Calu.NPIDate() function
#' @param dat analysis data set e.g. MainDat
#' @param NPIdat analysis data of NPI in each study
#' @param target which target to use, Time_interval and Peak_interval
#' @param excludeNA whether exclude NA in the RecircDat. Mainly in some Virus with many cirulation
#' @param report generate a tidy report table
#' @param NPIDate a date generated by Calu.NPIDate() # 这个暂时去掉
#' @return a plot and a table 
Calu.SingleVir.Ref <- function(dat, NPIdat, target = c("Time_interval", "Peak_interval"), excludeNA = T, plot = T, save = F, path = NULL, width = 12,
                               height = 6, report = F) {
  # alloc.col(dat, 1) 这两句适用于一个同一个参考日期，暂时去掉
  # set(dat, j = "NPIDate", value = NPIDate) # 这两句等同a[, NPIDate := NPIDate]，为了避免出现warning
  
  RecircDat <- merge.data.table(MainDat, NPIdat, by = "Study_ID", all.x = T) %>%  # 合并原始数据和NPI日期
    drop_na(NPIDate) %>%  # 去掉没有提供这些参考时间的的地区，如中国，马达加斯加
    setDT()

  RecircDat <- RecircDat[, ":="(Recir = ifelse(Index_of_Wave == 1, ReCirculation_time - NPIDate, Time_interval), 
                 Peak = ifelse(Index_of_Wave == 1, Peak_time - NPIDate, Peak_interval))]
  
  switch (target,
    "Time_interval" = {
      RecircDat <- RecircDat[, .(num = .N, mean = mean(Recir, na.rm = T), sd = sd(Recir, na.rm = T)), keyby = .(Virus_name, Index_of_Wave)
      ][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))
      ][, ':='(Cumday = cumsum(as.integer(mean)), 
               ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")), keyby = .(Virus_name)
      ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
      },
    "Peak_interval" = {
      RecircDat <- RecircDat[, .(num = .N, mean = mean(Peak, na.rm = T), sd = sd(Peak, na.rm = T)), keyby = .(Virus_name, Index_of_Wave)
      ][, c("lci", "uci") := .(mean - 1.96 * sd / sqrt(num), mean + 1.96 * sd / sqrt(num))
      ][, ':='(Cumday = cumsum(as.integer(mean)), 
               ci = paste0(round(mean, digits = 2), "(", round(lci, 2), ", ", round(uci,2), ")")), keyby = .(Virus_name)
      ][, c("Cumlci", "Cumuci") := .(Cumday - 1.96 * sd / sqrt(num), Cumday + 1.96 * sd / sqrt(num))]
      }
  )
  
  if (excludeNA == T) {
    RecircDat <- RecircDat %>% drop_na()
  }
  
  if (plot == T) {
    if (target == "Time_interval") {
      title <- "Time interval between each start of circulation and NPI threshold"
    } else{
      title <- "Time interval between each peak of circulation and NPI threshold"
    }
    
    fig <- ggplot(RecircDat) +
      geom_hline(yintercept = c(0, 365, 365*2, 365*3, 365*4), linetype = 2, linewidth = 1.2, alpha = 0.7, colour = "grey75") +
      geom_errorbar(aes(x = fct_rev(Virus_name), ymin = Cumlci, ymax = Cumuci), linewidth = 1, width = 0.4, colour = "#08203e") +
      geom_point(aes(x = fct_rev(Virus_name), y = Cumday), colour = "#852237", size = 3) +
      scale_y_continuous(breaks = c(0, 365, 365*2, 365*3, 365*4), 
                         labels = c("Reference NPI", "1 year", "2 year", "3 year", "4 year"), expand = c(0, 0)) +
      labs(title = title, x = "Virus", y = "Time interval") +
      theme_bw() +
      theme(axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 18, face = "bold", colour = "black"),
            plot.margin = unit(c(0.5,1,0.5,1),"cm")) +
      coord_flip()
    
    print(fig)
  }
  
  if (save == T) {
    ggsave(fig, file = path, width = width, height = height)
  }
  
  if (report == T) {
    ReportTable <- RecircDat[, .(Virus_name, Index_of_Wave, num, ci)]
    ReportTable <- setnames(ReportTable, c("Virus_name", "Index_of_Wave", "num", "ci"), 
                            c("Virus", "Index of wave", "Report number", "95% Confidence interval"))
    
    return(ReportTable)
  }else{
    return(RecircDat)
  }
}


#' This function is used to find the virus recirculation time for each Virus_name in each study. And the mean value of NPI information in the specific time range
#' @param dat a list, split by Virus_name and Study_ID
#' @param OxCdat 
#' @param threshold A time range, if earlier than the NPI's reporting time, calculates the mean NPI of the reporting time to the recovery time. Otherwise, calculate the mean NPI over this time range
#' @return 
Calu.NPIinfo <- function(dat, OxCdat, threshold=180, 
                         OxCindex = GovernmentResponseIndex_WeightedAverage_ForDisplay, 
                         GMRindex = retail_and_recreation_percent_change_from_baseline) {
  OxCindex_name <- substitute(OxCindex)
  GMRindex_name <- substitute(GMRindex)
  RecirDate <- dat[1, ReCirculation_time] 
  ID <- dat[1, Study_ID]
  RecirDateEarly <- RecirDate - threshold # 恢复流行的时间前推180天
  NpiStarDate <- OxCdat[Study_ID == ID & !is.na(eval(OxCindex_name)), .(date)][1, date]
  GMRStarDate <- OxCdat[Study_ID == ID & !is.na(eval(GMRindex_name)), .(date)][1, date]
  
  CheckPointDateNPI <- if_else(RecirDateEarly >= NpiStarDate, NpiStarDate, RecirDateEarly)
  # 判断恢复流行前推时间和NPI时间哪个更靠前，如果前推的时间早于NPI有记录的时间，选择NPI有记录的时间
  CheckPointDateGMR <- if_else(RecirDateEarly >= GMRStarDate, GMRStarDate, RecirDateEarly)
  
  NPI <- OxCdat[date >= CheckPointDateNPI & date <= RecirDate, .(meanNPI = mean(eval(OxCindex_name), na.rm = T))] 
  GMR <- OxCdat[date >= CheckPointDateGMR & date <= RecirDate, .(meanGMR = mean(eval(GMRindex_name), na.rm = T))]
  return(bind_cols(list(dat, NPI, GMR, CheckPointDateNPI = CheckPointDateNPI, CheckPointDateGMR = CheckPointDateGMR)))
}


#' This function calculates the R square and regression coefficient for each virus
#' @param dat a data set generated by Calu.NPIinfo()
#' @return a list, contain R square, coef table and etc.
Calu.regression<- function(dat){
  VirName <- unique(dat$Virus_name)
  Summ <- summary(lm(as.integer(Time_interval) ~ meanNPI + meanGMR, data = dat))
  SummList <- list(VirName = VirName,
                   R_square = Summ$r.squared,
                   R_square_adj = Summ$adj.r.squared,
                   SummTable = Summ$coefficients,
                   Intercept = Summ$coefficients[1,1],
                   Coef = c(Summ$coefficients[2,1], Summ$coefficients[3,1]))
  return(SummList)
}


#' This function builds a prediction model based on Calu.regression() and calculates the predicted value
#' @param dat a data frame, consist by various combine of OxC and GMR
#' @param coefficients coef from Calu.regression()
#' @param  intercept from Calu.regression()
#' @param VirName from Calu.regression()
#' @return 
Calu.pred <- function(dat, coefficients, intercept, VirName) {
  # 添加截距
  dat_int <- cbind(1, dat)
  # 计算预测值
  pred <- as.matrix(dat_int) %*% c(intercept, coefficients)
  return(bind_cols(list(dat, pred = pred, Virus_name = VirName)))
}


#' This function is used to plot based on the predicted value from Calu.pred(), producing a picture with OxC on the x-axis and GMR on the y-axis, with the colour representing the time interval of recirculation
#' @param dat a list, split by Virus_name
#' @return  a picture with OxC on the x-axis and GMR on the y-axis, with the colour representing the time interval of recirculation
plot.PredValue <- function(dat, save = F, path = NULL, width = 12, height = 6){
  VirName <- unique(dat$Virus_name)
  title <- sprintf("Prediction of %s", VirName)
  fig <- ggplot(dat, aes(x = OxC, y = GMR)) + 
    geom_tile(aes(fill = pred)) +
    scale_fill_gradient(low = "#074ba9", high = "#be134c") +
    labs(x = "OxCGRT index\nGovernment Response Index", 
         y = "Google Mobility Report\nRetail and recreation percent change from baseline",
         fill = "Predicted value",
         title = title) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, face = "bold", colour = "black"),
          plot.title = element_text(size = 18, face = "bold", colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"))
  print(fig)
  if (save == T) {
    ggsave(fig, file = paste0(path, "Pred_of_", VirName, ".pdf"), width = width, height = height)
  }
}

