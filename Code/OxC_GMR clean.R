library(readxl)
library(finalfit)
library(tidyverse)
library(data.table)
library(mapdata)
library(tidygeocoder)

source("Code/func.R")

#     !!!!!! 1.5及以后的版本为了匹配国家收入的分类，将Country的名称进行了修改，如果要重新运行这个代码，需要使用1.4及以前的版本
###### NPI data cleaning
OxC_GMR_dat <- read_excel("Data/Full-text table-V1.4.xlsx", sheet = "General") %>% as.data.table()
  # left_join(., Income, by = join_by(Country)) %>% 
  # rowwise() %>% 
  # mutate(across(starts_with("Q"), as.logical), quality = sum(c_across(Q1:Q8), na.rm = T)) %>% as.data.table()

### Prepare general dataset for both OxCGRT and GMR data
LocaIdentify <- OxC_GMR_dat %>% filter(keep) %>% dplyr::select(Study_ID, Country, Location, GMR_sub1, GMR_sub2) %>% 
  setkey(., Country, GMR_sub1, GMR_sub2) # for other country and location

DateExpand <- expand_grid(date = seq(as.Date("2020/01/01"), as.Date("2023/5/31"), by = "day"), 
                          Country = unique(LocaIdentify$Country)) %>% setDT(.) # expand date to research range
DateExpand <- DateExpand[, date := as.IDate(date, format = '%Y-%m-%d')] # change date format

### OxCGRT data
# OxCGRT <- paste0("Data/OxCGRT/", list.files("Data/OxCGRT")) %>% lapply(fread) %>% rbindlist() # combine 4 OxCGRT files
# fwrite(OxCGRT, "Data/OxCGRT/OxCGRT_All.csv")
OxCGRT <- fread("Data/OxCGRT/OxCGRT_All.csv") %>% 
  mutate(CountryName = if_else(CountryName == "Czech Republic", "Czechia", CountryName)) # OxCGRT not include Mexico

OxCGRT_filter <- OxCGRT[
  CountryName %chin% unique(OxC_GMR_dat$Country), 
  c("CountryName", "Date", "EconomicSupportIndex_ForDisplay", grep("_WeightedAverage_ForDisplay", names(OxCGRT), value = TRUE)), with = F
][, Date := as.IDate(as.character(Date), format = '%Y%m%d')] # Simplify OxCGRT data
# 在上面的代码中，value 用于将对象作为名称而不是索引处理， with指选择列的时候不自动引用

OxCGRT_filter <- merge(DateExpand, OxCGRT_filter, by.x = c("Country", "date"), by.y = c("CountryName", "Date"), all = T)

## Check OxCGRT plot
# lapply(split(OxCGRT_filter, OxCGRT_filter$Country), OxCGRT.plot, index = GovernmentResponseIndex_WeightedAverage_ForDisplay)


### Google Mobility Report data
GMR <- fread("Data/Global_Mobility_Report.csv") # not include China and Madagascar
GMR <- GMR[, c("country_region", "sub_region_1", "sub_region_2") := lapply(.SD, function(x) ifelse(GMR.CheckBlank(x), x, NA)), 
           .SDcols = c("country_region", "sub_region_1", "sub_region_2")] # delete blank space in GMR

GMR_filter <- GMR[country_region %chin% unique(OxC_GMR_dat$Country), 
                  c("country_region", "sub_region_1", "sub_region_2", "date", grep("_baseline", names(GMR), value = TRUE)), with = F] 

## Get GMR data from research location in General dataset. OxCGRT only include country level data, don't need this operation
GMR_Identify <- setkey(GMR_filter, country_region, sub_region_1, sub_region_2)

Identify <- GMR_Identify[LocaIdentify, on = .(country_region = Country, sub_region_1 = GMR_sub1, sub_region_2 = GMR_sub2), nomatch = 0] %>% 
  relocate(Location, .after = country_region)  %>% 
   merge(DateExpand, ., by.x = c("Country", "date"), by.y = c("country_region", "date"), all = T) # expand date to research range

Identify_split <- split(Identify, by = c("Country", "Location", "Study_ID"))

GMR_DateExpand <- lapply(Identify_split, expand.date) %>% rbindlist(.)
# GMR_DateExpand <- lapply(Identify_split, function(dat){
#   Count <- dat$country_region[1]
#   merge_dat <- merge(DateExpand[Country == Count], dat, by.x = c("Country", "date"), by.y = c("country_region", "date"), all = T) %>% 
#     fill(Location, sub_region_1, sub_region_2, Study_ID, .direction = "downup")
#   return(merge_dat)
# }) %>% rbindlist(.)


### Combine OxCGRT and GMR data
OxC_GMR <- merge(OxCGRT_filter, GMR_DateExpand, by.x = c("Country", "date"), by.y = c("Country", "date"), all = T)
# OxC_GMR <- OxC_GMR[order(Country, Location, date, Study_ID)][, Study_ID:=NULL]
# OxC_GMR <- unique(OxC_GMR, by = c("Country", "date", "Location"))

### Add China data
HZ29A <- copy(OxC_GMR[Country == "China"])
HZ29B <- copy(OxC_GMR[Country == "China"])
SH38 <- copy(OxC_GMR[Country == "China"])
SZ55 <- copy(OxC_GMR[Country == "China"])
WH51 <- copy(OxC_GMR[Country == "China"])
S53 <- copy(OxC_GMR[Country == "Madagascar"])
HZ29A[, ":="(Location = "Hangzhou",
             Study_ID = "S29A")]
HZ29B[, ":="(Location = "Hangzhou",
             Study_ID = "S29B")]
SH38[, ":="(Location = "Shanghai",
            Study_ID = "S38")]
SZ55[, ":="(Location = "Shenzhen",
            Study_ID = "S55")]
WH51[, ":="(Location = "Wuhan",
            Study_ID = "S51")]
S53[, Study_ID := "S53"]

OxC_GMR <- rbindlist(list(HZ29A, HZ29B, SH38, SZ55, WH51, S53, OxC_GMR))
OxC_GMR <- OxC_GMR[!is.na(Study_ID)]

# unique(OxC_GMR$Study_ID)
# 
# OxC_GMR[is.na(Study_ID)]


## Check GMR plot
# lapply(split(OxC_GMR, by = c("Country", "Study_ID")), GMR.plot, index = retail_and_recreation_percent_change_from_baseline)


rm(list = c(ls()[ls() != "OxC_GMR"]))
save.image("~/Documents/600_Project/610_System_review/OxCGRT_GMR data.RData")
