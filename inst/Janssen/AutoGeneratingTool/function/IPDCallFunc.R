# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         10-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


CallFunc <- function(page, 
                     save_location, 
                     sales.data, 
                     call.data, 
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
  
  # brand
  brand.sel <- stri_split_fixed(position1$Brand[1], " & ")[[1]]
  
  # time frame
  latest.month <- last(sort(unique(sales.data$month)))
  latest.month.fmt <- paste0(stri_sub(latest.month, 1, 4), "/", stri_sub(latest.month, 5, 6), "/01")
  month.list <- seq.Date(from = as.Date(latest.month.fmt) %m-% months(6), by = "month", length.out = 6)
  month.list.fmt <- stri_sub(stri_replace_all_fixed(month.list, "-", ""), 1, 6)
  
  latest.year <- stri_sub(max(month.list.fmt), 1, 4)
  
  month.table <- data.frame(brand = brand.sel,
                            month = month.list.fmt)
  
  # sales trend
  trend <- sales.data %>% 
    filter(brand %in% brand.sel, month %in% month.list.fmt) %>% 
    right_join(month.table, by = c("brand", "month")) %>% 
    mutate(brand = paste0(brand.sel, collapse = " & ")) %>% 
    group_by(month, brand) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(ach = sales / target,
           ach = ifelse(is.na(ach) | is.infinite(ach), NA, ach),
           growth = sales / sales_p - 1,
           growth = ifelse(is.na(growth) | is.infinite(growth), NA, growth)) %>% 
    mutate(sales = sales / 1000000) %>%
    arrange(month) %>% 
    select("Brand" = "brand", "Month" = "month", "Sales" = "sales", 
           "Sales Ach%" = "ach", "GR%" = "growth")
  
  # call frequence and coverage
  sel.call <- call.data %>% 
    filter(brand %in% brand.sel, month %in% month.list.fmt) %>% 
    right_join(month.table, by = c("brand", "month")) %>% 
    mutate(brand = position1$Brand[1]) %>% 
    group_by(month, brand) %>% 
    summarise(doctor_A = sum(doctor_A, na.rm = TRUE),
              doctor_B = sum(doctor_B, na.rm = TRUE),
              pro_call_A = sum(pro_call_A, na.rm = TRUE),
              pro_call_B = sum(pro_call_B, na.rm = TRUE),
              visit_doc_A = sum(visit_doc_A, na.rm = TRUE),
              visit_doc_B = sum(visit_doc_B, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(visit_doc_AB = visit_doc_A + visit_doc_B,
           doctor_AB = doctor_A + doctor_B,
           call_freq_A = pro_call_A / doctor_A,
           call_freq_B = pro_call_B / doctor_B,
           call_coverage_AB = visit_doc_AB / doctor_AB)
  
  call.freq <- sel.call %>% 
    select("Brand" = "brand", "Month" = "month", "Call Frequency_A" = "call_freq_A", 
           "Call Frequency_B" = "call_freq_B")
  
  call.coverage <- sel.call %>% 
    select("Brand" = "brand", "Month" = "month", "AB Coverage" = "call_coverage_AB")
  
  # write out
  write.xlsx(trend, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(call.freq, paste0(save_location, "/", position2$Filename[1], ".xlsx"))
  write.xlsx(call.coverage, paste0(save_location, "/", position3$Filename[1], ".xlsx"))
}
