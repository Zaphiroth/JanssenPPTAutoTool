# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         27-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


KeyPerfRegionFunc <- function(page, 
                              save_location, 
                              sales.data = sales.perf.fmt, 
                              mapping = mapping.table) {
  
  # page info
  position1 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 1) %>% 
    distinct()
  
  position2 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 2) %>% 
    distinct()
  
  position4 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 4) %>% 
    distinct()
  
  # brand
  brand.sel <- position1$Brand[1]
  
  # region
  region.sel <- position1$Region[1]
  
  # month
  latest.month <- max(sales.data$month)
  latest.year <- stri_sub(latest.month, 1, 4)
  
  month.mapping <- data.frame(brand = brand.sel,
                              month = paste0(latest.year, stri_pad_left(1:12, 2, 0)))
  
  latest.month1 <- as.character(as.numeric(latest.month) - 1)
  
  # format
  sales.data.fmt <- sales.data %>% 
    filter(brand == brand.sel, region == region.sel)
  
  # sales performance
  sales.trend <- sales.data.fmt %>% 
    group_by(brand, month, model) %>% 
    summarise(sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(sales = sales / 1000000) %>% 
    setDT() %>% 
    dcast(brand + month ~ model, value.var = "sales", fill = NA) %>% 
    right_join(month.mapping, by = c("brand", "month")) %>% 
    select("Brand" = "brand", "Month" = "month", "MR_Hospital", "NDC bottom up")
  
  # YTD
  sales.ytd <- sales.data.fmt %>% 
    filter(stri_sub(month, 1, 4) == latest.year, !is.na(region)) %>% 
    group_by(model, month) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(model) %>% 
    mutate(sales_gr1 = sales[month == latest.month],
           sales_gr2 = sales[month == latest.month1]) %>% 
    ungroup() %>% 
    group_by(model) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_gr1 = first(sales_gr1),
              sales_gr2 = first(sales_gr2)) %>% 
    ungroup() %>% 
    adorn_totals("row", NA, name = "Sellout") %>% 
    mutate(gr = sales_gr1 / sales_gr2 - 1,
           ach = sales / target,
           model = ifelse(model == "MR_Hospital", "DC", 
                          ifelse(model == "NDC bottom up", "NDC", 
                                 model))) %>% 
    mutate(sales = sales / 1000000) %>% 
    select("YTD" = "model", "Value" = "sales", "Gr% MOM-Average" = "gr", "Achi%" = "ach") %>% 
    right_join(position2[c("Display_name")], by = c("YTD" = "Display_name"))
  
  # sales performance by segment
  sales.seg <- sales.data.fmt %>% 
    filter(stri_sub(month, 1, 4) == latest.year, !is.na(type), model == "MR_Hospital") %>% 
    group_by(type, month) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(type) %>% 
    mutate(sales_gr1 = sales[month == latest.month],
           sales_gr2 = sales[month == latest.month1],
           target_gr1 = target[month == latest.month]) %>% 
    ungroup() %>% 
    group_by(type) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_gr1 = first(sales_gr1),
              sales_gr2 = first(sales_gr2),
              target_gr1 = first(target_gr1)) %>% 
    ungroup() %>% 
    adorn_totals("row", NA) %>% 
    mutate(ach = sales_gr1 / target_gr1,
           gr = sales_gr1 / sales_gr2 - 1,
           ach_ytd = sales / target) %>% 
    mutate(sales = sales / 1000000,
           sales_gr1 = sales_gr1 / 1000000) %>% 
    select("Segment*" = "type", "DC" = "sales_gr1", "Achi%" = "ach", "MoM Gr%" = "gr", 
           "YTD DC" = "sales", "YTD Achi%" = "ach_ytd") %>% 
    right_join(position4[c("Display_name")], by = c("Segment*" = "Display_name"))
  
  # write out
  sales.trend[is.na(sales.trend) | sales.trend == Inf | sales.trend == -Inf | sales.trend == 0] <- NA
  sales.ytd[is.na(sales.ytd) | sales.ytd == Inf | sales.ytd == -Inf | sales.ytd == 0] <- NA
  sales.seg[is.na(sales.seg) | sales.seg == Inf | sales.seg == -Inf | sales.seg == 0] <- NA
  
  write.xlsx(sales.trend, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(sales.ytd, paste0(save_location, "/", position2$Filename[1], ".xlsx"))
  write.xlsx(sales.seg, paste0(save_location, "/", position4$Filename[1], ".xlsx"))
}
