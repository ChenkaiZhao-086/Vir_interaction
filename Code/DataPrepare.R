FileName <- "Data/Full-text table-V1.7.xlsx"

Income <- read_excel("Data/income.xlsx") # World bank income classification

General <- read_excel(FileName, sheet = "General") %>%
  left_join(., Income, by = join_by(Country)) %>%
  rowwise() %>%
  mutate(across(starts_with("Q"), as.logical),
    quality = sum(c_across(Q1:Q8), na.rm = T)
  ) %>%
  as.data.table()
Vir_list <- read_excel(FileName, sheet = "Vir-list", col_types = c(
  "text", "text", "text", "text", "text", "text", "text", "text",
  "text", "numeric", "date", "date", "numeric", "text", "date",
  "date", "numeric", "text", "text", "numeric"
)) %>% as.data.table()

All_Vir <- right_join(General, Vir_list, multiple = "all") %>%
  filter(keep) %>%
  mutate(across(ends_with("_time"), as.Date),
    Time_interval = ReCirculation_time - Last_circ_time,
    Peak_interval = Peak_time - Last_peak_time,
    Ratio_rate = (ReCirculation_rate / Last_circ_rate) * 100,
    ReCir_peak_interval = Peak_time - ReCirculation_time,
    Last_peak_interval = Last_peak_time - Last_circ_time
  ) %>%
  mutate(Virus_ID = case_when(
    Virus_name == "IFV" ~ "V1",
    Virus_name == "RSV" ~ "V2",
    Virus_name == "HPIV" ~ "V3",
    Virus_name == "HMPV" ~ "V4",
    Virus_name == "HCoV" ~ "V5",
    Virus_name == "HRV" ~ "V6",
    Virus_name == "HAdV" ~ "V7",
    Virus_name == "HBoV" ~ "V8",
    Virus_name == "HEV" ~ "V9",
    Virus_name == "IFVA" ~ "V10",
    Virus_name == "IFVB" ~ "V11",
    Virus_name == "RSVA" ~ "V12",
    Virus_name == "RSVB" ~ "V13",
    Virus_name == "HPIV1" ~ "V14",
    Virus_name == "HPIV2" ~ "V15",
    Virus_name == "HPIV3" ~ "V16",
    Virus_name == "HPIV4" ~ "V17",
    Virus_name == "OtherHPIV" ~ "V18",
    Virus_name == "IFVA_H1N1" ~ "V19",
    Virus_name == "IFVA_H3N2" ~ "V20",
    Virus_name == "IFVA_NST" ~ "V21",
    Virus_name == "IFVB_VIC" ~ "V22",
    Virus_name == "IFVB_YM" ~ "V23",
    Virus_name == "IFVB_LU" ~ "V24",
    Virus_name == "HCoV_CoVHKU1" ~ "V24",
    Virus_name == "HCoV_CoVNL63" ~ "V24",
    Virus_name == "HCoV_CoVOC43" ~ "V24",
    Virus_name == "HCoV_CoV229E" ~ "V24"
  )) %>%
  mutate(Unique_ID = paste0(Study_ID, Report_time_interval, Outcome_type, Virus_ID))

# LocationInfo <- General %>% filter(keep) %>%
#   mutate(addr = if_else(is.na(Location), Country, paste0(Location, ", ", Country))) %>% dplyr::select(Study_ID, addr) %>%
#   geocode(address = addr, method = 'google') # Get Longitude and Latitude
# save(LocationInfo, file = "~/Documents/600_Project/610_System_review/Location.RData")
load("~/Documents/600_Project/610_System_review/Location.RData")
## This operation based on Google map API and need Google developer account

All_Vir <- All_Vir %>% full_join(LocationInfo, by = join_by(Study_ID))
All_Vir <- All_Vir %>%
  mutate(Virus_name = case_when(
    Virus_name == "IFV" ~ "IV",
    Virus_name == "RSV" ~ "RSV",
    Virus_name == "HPIV" ~ "PIV",
    Virus_name == "HMPV" ~ "MPV",
    Virus_name == "HCoV" ~ "sCoV",
    Virus_name == "HRV" ~ "RV",
    Virus_name == "HAdV" ~ "AdV",
    Virus_name == "IFVA" ~ "IAV",
    Virus_name == "IFVB" ~ "IBV",
    Virus_name == "IFVA_H1N1" ~ "H1N1",
    Virus_name == "IFVA_H3N2" ~ "H3N2",
    Virus_name == "HPIV1" ~ "PIV_1",
    Virus_name == "HPIV2" ~ "PIV_2",
    Virus_name == "HPIV3" ~ "PIV_3",
    Virus_name == "HPIV4" ~ "PIV_4"
  ))


# rm(list = c(ls()[!ls() %in% c("All_Vir", "General", "Vir_list", "LocationInfo")]))
save(All_Vir, file = "~/Documents/600_Project/610_System_review/All_Vir.RData")
