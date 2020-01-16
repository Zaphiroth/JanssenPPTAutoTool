# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         27-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


KeyPerfFunc <- function(page, 
                        save_location, 
                        sales.data, 
                        mapping) {
  
  # page info
  position1 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 1) %>% 
    distinct()
  
  position2 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 2) %>% 
    distinct()
  
  position3 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 3) %>% 
    distinct()
  
  position4 <- mapping %>% 
    filter(Category == "Launch", Page == page, Position == 4) %>% 
    distinct()
  
  # brand
  brand.sel <- position1$Brand[1]
  
  # month
  latest.month <- max(sales.data$month)
  latest.year <- stri_sub(latest.month, 1, 4)
  
  month.mapping <- data.frame(brand = brand.sel,
                              month = paste0(latest.year, stri_pad_left(1:12, 2, 0)))
  
  latest.month1 <- as.character(as.numeric(latest.month) - 1)
  
  # format
  sales.data.fmt <- sales.data %>% 
    filter(brand %in% brand.sel)
  
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
  
  # sales performance by region
  sales.perf.latest <- sales.data.fmt %>% 
    filter(month %in% c(latest.month, latest.month1), model == "MR_Hospital") %>% 
    group_by(region, month) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE)) %>% 
    ungroup() %>% 
    setDT() %>% 
    melt(id.vars = c("region", "month")) %>% 
    filter(!(month %in% latest.month1 & variable %in% c("target"))) %>% 
    mutate(variable = if_else(month %in% latest.month1, "sales_pm", as.character(variable))) %>% 
    setDT() %>% 
    dcast(region ~ variable, value.var = "value") %>% 
    adorn_totals("row", NA) %>% 
    mutate(ach = sales / target,
           gr = sales / sales_pm - 1) %>% 
    mutate(sales = sales / 1000000) %>% 
    select("Region" = "region", "DC" = "sales", "DC Ach%" = "ach", "DC MoM Gr%" = "gr")
  
  sales.perf.type <- sales.data.fmt %>% 
    filter(month == latest.month) %>% 
    mutate(total = sum(sales, na.rm = TRUE)) %>% 
    group_by(region) %>% 
    mutate(total = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(region, type) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              total = first(total)) %>% 
    ungroup() %>% 
    filter(type %in% c("Dara SUPER", "Dara STAR", "Dara SEED")) %>% 
    setDT() %>% 
    dcast(region + total ~ type, value.var = "sales", fill = NA) %>% 
    adorn_totals("row", NA) %>% 
    mutate(`Dara SUPER` = `Dara SUPER` / total,
           `Dara STAR` = `Dara STAR` / total,
           `Dara SEED` = `Dara SEED` / total) %>% 
    select("Region" = "region", "Dara SUPER", "Dara STAR", "Dara SEED")
  
  sales.perf.ytd <- sales.data.fmt %>% 
    filter(stri_sub(month, 1, 4) == latest.year, !is.na(region)) %>% 
    group_by(region, model) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE)) %>% 
    ungroup() %>% 
    setDT() %>% 
    melt(id.vars = c("region", "model"), measure.vars = c("sales", "target")) %>% 
    mutate(model = ifelse(model == "MR_Hospital", "DC", 
                          ifelse(model == "NDC bottom up", "NDC", 
                                 model))) %>% 
    unite("variable", model, variable, sep = "_") %>% 
    setDT() %>% 
    dcast(region ~ variable, value.var = "value", fill = NA) %>% 
    adorn_totals("row", NA) %>% 
    mutate(sales = DC_sales + NDC_sales,
           target = DC_target + NDC_target,
           ach = sales / target) %>% 
    mutate(DC_sales = DC_sales / 1000000,
           NDC_sales = NDC_sales / 1000000,
           sales = sales / 1000000) %>% 
    select("Region" = "region", "YTD DC" = "DC_sales", "YTD NDC" = "NDC_sales", 
           "YTD Sellout" = "sales", "YTD Ach%" = "ach")
  
  sales.region <- sales.perf.latest %>% 
    left_join(sales.perf.type, by = "Region") %>% 
    left_join(sales.perf.ytd, by = "Region") %>% 
    right_join(position3[c("Display_name")], by = c("Region" = "Display_name"))
  
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
           sales_gr2 = sales[month == latest.month1]) %>% 
    ungroup() %>% 
    group_by(type) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_gr1 = first(sales_gr1),
              sales_gr2 = first(sales_gr2)) %>% 
    ungroup() %>% 
    adorn_totals("row", NA) %>% 
    mutate(gr = sales_gr1 / sales_gr2 - 1,
           ach = sales / target) %>% 
    mutate(sales = sales / 1000000,
           sales_gr1 = sales_gr1 / 1000000) %>% 
    select("Segment*" = "type", "Sales" = "sales_gr1", "MoM Gr%" = "gr", "YTD Sales" = "sales", 
           "YTD Achi%" = "ach") %>% 
    right_join(position4[c("Display_name")], by = c("Segment*" = "Display_name"))
  
  # write out
  sales.trend[is.na(sales.trend) | sales.trend == Inf | sales.trend == -Inf | sales.trend == 0] <- NA
  sales.region[is.na(sales.region) | sales.region == Inf | sales.region == -Inf | sales.region == 0] <- NA
  sales.ytd[is.na(sales.ytd) | sales.ytd == Inf | sales.ytd == -Inf | sales.ytd == 0] <- NA
  sales.seg[is.na(sales.seg) | sales.seg == Inf | sales.seg == -Inf | sales.seg == 0] <- NA
  
  write.xlsx(sales.trend, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(sales.ytd, paste0(save_location, "/", position2$Filename[1], ".xlsx"))
  write.xlsx(sales.region, paste0(save_location, "/", position3$Filename[1], ".xlsx"))
  write.xlsx(sales.seg, paste0(save_location, "/", position4$Filename[1], ".xlsx"))
}
