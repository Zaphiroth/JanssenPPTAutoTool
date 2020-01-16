# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         10-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ChanPerfFunc <- function(page, 
                         save_location, 
                         sales.data, 
                         sales.data.add, 
                         mapping) {
  
  # page info
  position1 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 1) %>% 
    distinct()
  
  position2 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 2) %>% 
    distinct()
  
  position3 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 3) %>% 
    distinct()
  
  position4 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 4) %>% 
    distinct()
  
  position5 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 5) %>% 
    distinct()
  
  # period frame
  latest.year <- stri_sub(max(sales.data$month), 1, 4)
  
  # channel
  brand.perf <- bind_rows(sales.data, sales.data.add) %>% 
    filter(stri_sub(month, 1, 4) == latest.year) %>% 
    mutate(bu = if_else(grepl("Alliance Management", franchise), "Alliance Management", franchise),
           division = ifelse(model %in% c("MR_Hospital", "EC", "RPD"), "DC", 
                             ifelse(model %in% c("NDC bottom up"), "NDC", 
                                    model))) %>% 
    group_by(bu, division) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(channel = ifelse(bu %in% c("Hematology", "Immunology", "Solid Tumor", "Actelion"), "IPD", 
                            ifelse(bu %in% c("Neurology", "Psychiatry", "Derm/Anti-allergy", "Primary Care"), "CPD", 
                                   ifelse(bu %in% c("Alliance Management"), "Alliance Management", 
                                          NA)))) %>% 
    filter(!is.na(channel), !is.na(division))
  
  chan.perf <- brand.perf %>% 
    group_by(bu = channel, division) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup()
  
  total.perf <- brand.perf %>% 
    group_by(bu = "Total", division) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup()
  
  perf <- brand.perf %>% 
    select(-channel) %>% 
    bind_rows(chan.perf, total.perf) %>% 
    setDT() %>% 
    melt(id.vars = c("division", "bu")) %>% 
    unite("variable", division, variable) %>% 
    distinct() %>%
    dcast(bu ~ variable, value.var = "value", fill = 0) %>% 
    right_join(position1[c("Display_name")], by = c("bu" = "Display_name")) %>% 
    mutate(Sellout_sales = DC_sales + NDC_sales,
           Sellout_sales_p = DC_sales_p + NDC_sales_p,
           Sellout_target = DC_target + NDC_target,
           Sellout_ach = Sellout_sales / Sellout_target,
           Sellout_gr = Sellout_sales / Sellout_sales_p - 1,
           DC_ach = DC_sales / DC_target,
           DC_gr = DC_sales / DC_sales_p - 1,
           NDC_ach = NDC_sales / NDC_target,
           NDC_gr = NDC_sales / NDC_sales_p - 1,
           incremental = (Sellout_sales - Sellout_sales_p) / sum(total.perf$sales - total.perf$sales_p)) %>% 
    mutate(Sellout_sales = Sellout_sales / 1000000,
           DC_sales = DC_sales / 1000000,
           NDC_sales = NDC_sales / 1000000) %>% 
    select("BU" = "bu", "Sellout Sales" = "Sellout_sales", "Sellout Ach%" = "Sellout_ach", 
           "Sellout GR%" = "Sellout_gr", "Incremental%" = "incremental", "DC Sales" = "DC_sales", 
           "DC Ach%" = "DC_ach", "DC GR%" = "DC_gr", "NDC Sales" = "NDC_sales", 
           "NDC Ach%" = "NDC_ach", "NDC GR%" = "NDC_gr")
  
  perf[is.na(perf) | perf == Inf | perf == -Inf | perf == 0] <- NA
  
  perf.table <- perf %>% 
    mutate(`Sellout Sales` = NA,
           `Incremental%` = NA,
           `DC Sales` = NA,
           `NDC Sales` = NA)
  
  # write out
  write.xlsx(perf.table, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(perf[, c("BU", "Sellout Sales")], paste0(save_location, "/", position2$Filename[1], ".xlsx"))
  write.xlsx(perf[, c("BU", "Incremental%")], paste0(save_location, "/", position3$Filename[1], ".xlsx"))
  write.xlsx(perf[, c("BU", "DC Sales")], paste0(save_location, "/", position4$Filename[1], ".xlsx"))
  write.xlsx(perf[, c("BU", "NDC Sales")], paste0(save_location, "/", position5$Filename[1], ".xlsx"))
}
