# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         10-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


SalesPerfFunc <- function(page, 
                          save_location, 
                          sales.data = sales.month.fmt, 
                          mapping = mapping.table) {
  
  # page info
  position1 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 1) %>% 
    distinct()
  
  position2 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 2) %>% 
    distinct()
  
  # brand
  brand.sel <- position1$Brand[1]
  
  # time frame
  latest.month <- last(sort(unique(sales.data$month)))
  latest.month.fmt <- paste0(stri_sub(latest.month, 1, 4), "/", stri_sub(latest.month, 5, 6), "/01")
  month.list <- seq.Date(from = as.Date(latest.month.fmt) %m-% months(24), by = "month", length.out = 24)
  month.list.fmt <- stri_sub(stri_replace_all_fixed(month.list, "-", ""), 1, 6)
  
  latest.year <- stri_sub(max(month.list.fmt), 1, 4)
  
  month.table <- data.frame(brand = brand.sel,
                            month = month.list.fmt)
  
  # sales trend
  trend <- sales.data %>% 
    filter(brand %in% brand.sel, month %in% month.list.fmt) %>% 
    right_join(month.table, by = c("brand", "month")) %>% 
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
           "Growth%" = "growth", "Ach%" = "ach")
  
  # YTD sales
  ytd <- sales.data %>% 
    filter(brand %in% brand.sel, stri_sub(month, 1, 4) == latest.year) %>% 
    group_by(month, brand) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(month) %>% 
    group_by(brand) %>% 
    summarise(date = last(month),
              sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(date = paste0("YTD ", stri_sub(date, 1, 4), "M", as.numeric(stri_sub(date, 5, 6))),
           ach = sales / target,
           ach = ifelse(is.na(ach) | is.infinite(ach), NA, ach),
           growth = sales / sales_p - 1,
           growth = ifelse(is.na(growth) | is.infinite(growth), NA, growth)) %>% 
    mutate(sales = sales / 1000000) %>% 
    select("Brand" = "brand", "Date" = "date", "Sellout" = "sales", "Growth%" = "growth", "Ach%" = "ach") %>% 
    setDT() %>% 
    melt(id.vars = "Brand", measure.vars = c("Date", "Sellout", "Growth%", "Ach%"))
  
  # write out
  write.xlsx(trend, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(ytd, paste0(save_location, "/", position2$Filename[1], ".xlsx"))
}
