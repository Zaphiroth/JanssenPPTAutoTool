# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         10-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


SalesPerfSegFunc <- function(page, 
                             save_location, 
                             sales.data, 
                             market.data, 
                             freq.data, 
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
  
  position6 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 6) %>% 
    distinct()
  
  position7 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 7) %>% 
    distinct()
  
  position8 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 8) %>% 
    distinct()
  
  # brand, decile
  brand.sel <- position1$Brand[1]
  segment <- c(position1$Display_name[1], position2$Display_name[1], position3$Display_name[1])
  
  # time frame
  latest.month <- last(sort(unique(sales.data$month)))
  latest.month.fmt <- paste0(stri_sub(latest.month, 1, 4), "/", stri_sub(latest.month, 5, 6), "/01")
  month.list <- seq.Date(from = as.Date(latest.month.fmt) %m-% months(6), by = "month", length.out = 6)
  month.list.fmt <- stri_sub(stri_replace_all_fixed(month.list, "-", ""), 1, 6)
  
  latest.year <- stri_sub(max(month.list.fmt), 1, 4)
  
  month.table <- data.frame(brand = brand.sel,
                            month = month.list.fmt)
  
  # sales performance
  sales.trend <- sales.data %>% 
    filter(brand %in% brand.sel, month %in% month.list.fmt) %>% 
    right_join(month.table, by = c("brand", "month")) %>% 
    mutate(ach = sales / target,
           ach = ifelse(is.na(ach) | is.infinite(ach), NA, ach),
           growth = sales / sales_p - 1,
           growth = ifelse(is.na(growth) | is.infinite(growth), NA, growth)) %>% 
    mutate(sales = sales / 1000000) %>% 
    arrange(month) %>% 
    select("Brand" = "brand", "Decile" = "decile", "Month" = "month", "Sales" = "sales", 
           "Sales Ach%" = "ach", "GR%" = "growth")
  
  trend1 <- filter(sales.trend, Decile == position1$Display_name[1])
  trend2 <- filter(sales.trend, Decile == position2$Display_name[2])
  trend3 <- filter(sales.trend, Decile == position3$Display_name[3])
  
  # YTD sales
  ytd <- sales.data %>% 
    filter(brand %in% brand.sel, stri_sub(month, 1, 4) == latest.year) %>% 
    arrange(month) %>% 
    group_by(brand, decile) %>% 
    summarise(date = last(month),
              sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(brand) %>% 
    mutate(total_sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(date = paste0("YTD ", stri_sub(date, 1, 4), "M", as.numeric(stri_sub(date, 5, 6))),
           sales_contr = sales / total_sales,
           ach = sales / target,
           ach = ifelse(is.na(ach) | is.infinite(ach), NA, ach),
           growth = sales / sales_p - 1,
           growth = ifelse(is.na(growth) | is.infinite(growth), NA, growth)) %>% 
    mutate(sales = sales / 1000000) %>% 
    select("Brand" = "brand", "Date" = "date", "Decile" = "decile", "DC" = "sales", "Growth%" = "growth", 
           "Ach%" = "ach", "Sales Contr%" = "sales_contr") %>% 
    setDT() %>% 
    melt(id.vars = c("Brand", "Decile"), measure.vars = c("Date", "DC", "Growth%", "Ach%", "Sales Contr%"))
  
  ytd1 <- filter(ytd, Decile == position4$Display_name[1])
  ytd2 <- filter(ytd, Decile == position5$Display_name[2])
  ytd3 <- filter(ytd, Decile == position6$Display_name[3])
  
  # call frequence and event coverage
  call.freq <- freq.data %>% 
    filter(brand %in% brand.sel, month == max(month.list.fmt), decile %in% segment) %>%
    adorn_totals("row", fill = "Total", na.rm = FALSE, name = brand.sel) %>% 
    mutate(month = max(month.list.fmt),
           visit_doc_AB = visit_doc_A + visit_doc_B,
           doctor_AB = doctor_A + doctor_B,
           call_freq_A = pro_call_A / doctor_A,
           call_freq_B = pro_call_B / doctor_B,
           call_coverage_AB = visit_doc_AB / doctor_AB,
           event_coverage_AB = event_AB / hcp_AB,
           event_coverage_AB = ifelse(is.na(event_coverage_AB) | is.infinite(event_coverage_AB), NA, event_coverage_AB)) %>%
    setDT() %>% 
    melt(id.vars = c("brand", "decile", "month"), 
         measure.vars = c("call_freq_A", "call_freq_B", "call_coverage_AB", "event_coverage_AB"), 
         variable.name = "KPI") %>% 
    dcast(brand + month + KPI ~ decile) %>% 
    select("Brand" = "brand", "Month" = "month", "KPI", segment, "Total")
  
  # share trend
  if (position1$Display_name[1] != "RA/AS") {
    share.trend <- sales.data %>% 
      filter(brand %in% brand.sel, month %in% month.list.fmt, decile %in% segment) %>% 
      left_join(market.data, by = c("brand", "decile", "month")) %>% 
      group_by(brand, decile, month) %>% 
      summarise(sales = sum(sales, na.rm = TRUE),
                market_size = sum(market_size, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(share = sales / market_size,
             share = ifelse(is.na(share) | is.infinite(share), NA, share)) %>% 
      setDT() %>% 
      dcast(brand + month ~ decile, value.var = "share") %>% 
      select("Brand" = "brand", "Month" = "month", segment)
  }
  
  # write out
  write.xlsx(trend1, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(trend2, paste0(save_location, "/", position2$Filename[1], ".xlsx"))
  write.xlsx(trend3, paste0(save_location, "/", position3$Filename[1], ".xlsx"))
  write.xlsx(ytd1, paste0(save_location, "/", position4$Filename[1], ".xlsx"))
  write.xlsx(ytd2, paste0(save_location, "/", position5$Filename[1], ".xlsx"))
  write.xlsx(ytd3, paste0(save_location, "/", position6$Filename[1], ".xlsx"))
  
  if (position1$Display_name[1] != "RA/AS") {
    write.xlsx(share.trend, paste0(save_location, "/", position7$Filename[1], ".xlsx"))
  }
  
  write.xlsx(call.freq, paste0(save_location, "/", position8$Filename[1], ".xlsx"))
}
