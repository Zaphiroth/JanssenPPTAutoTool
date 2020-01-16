# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         10-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


MarketPerfFunc <- function(page, 
                           save_location, 
                           sales.data, 
                           sales.data.add, 
                           ims.market.data, 
                           mapping) {
  
  # page info
  position1 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 1) %>% 
    distinct()
  
  position2 <- mapping %>% 
    filter(Category == "IPD", Page == page, Position == 2) %>% 
    distinct()
  
  # period frame
  latest.year <- stri_sub(max(sales.data$month), 1, 4)
  
  ims.market.date <- max(ims.market.data$date)
  
  # brand selection
  brand1 <- c("Velcade", "Imbruvica", "Dacogen", "Darzalex", "Zytiga", "Remicade", 
              "Simponi", "Edurant", "Prezcobix", "Stelara")
  
  brand2 <- c("Tracleer", "Opsumit", "Zavesca", "Uptravi")
  
  # ims
  ims.market.data[is.na(ims.market.data)] <- 0
  
  ims.market.fmt <- ims.market.data %>% 
    mutate(market_share = sales / sum(sales, na.rm = TRUE),
           market_share_p = sales_p / sum(sales_p, na.rm = TRUE),
           share_change = market_share - market_share_p) %>% 
    select(brand, market_share, share_change, EI)
  
  # channel
  brand.perf <- sales.data[sales.data$brand %in% brand1, ] %>% 
    bind_rows(sales.data.add[sales.data.add$brand %in% brand2, ]) %>% 
    filter(stri_sub(month, 1, 4) == latest.year, franchise %in% mapping$Display_name) %>% 
    group_by(bu = franchise, ipd = brand) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(ims.market.fmt, by = c("ipd" = "brand"))
  
  ipd.perf <- brand.perf %>% 
    group_by(ipd = bu) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE),
              market_share = sum(market_share, na.rm = TRUE),
              share_change = sum(share_change, na.rm = TRUE),
              EI = sum(EI, na.rm = TRUE)) %>% 
    ungroup()
  
  total.perf <- brand.perf %>% 
    group_by(ipd = "IPD Total") %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              target = sum(target, na.rm = TRUE),
              sales_p = sum(sales_p, na.rm = TRUE),
              market_share = sum(market_share, na.rm = TRUE),
              share_change = sum(share_change, na.rm = TRUE),
              EI = sum(EI, na.rm = TRUE)) %>% 
    ungroup()
  
  perf <- brand.perf %>% 
    select(-bu) %>% 
    bind_rows(ipd.perf, total.perf) %>% 
    right_join(position1[c("Display_name")], by = c("ipd" = "Display_name")) %>% 
    mutate(contri = sales / total.perf$sales[1],
           gr = sales / sales_p - 1,
           ach = sales / target) %>% 
    mutate(sales = sales / 1000000) %>% 
    select("IPD" = "ipd", "Sellout" = "sales", "Contri.%" = "contri", "Gr%" = "gr", 
           "Ach%" = "ach", "YTD MS%" = "market_share", "Chang vs. LY" = "share_change", 
           "EI")
  
  names(perf)[names(perf) == "YTD MS%"] <- paste0(ims.market.date, " YTD MS%")
  perf[is.na(perf) | perf == Inf | perf == -Inf | perf == 0] <- NA
  
  perf.table <- perf %>% 
    mutate(Sellout = NA)
  
  # write out
  write.xlsx(perf.table, paste0(save_location, "/", position1$Filename[1], ".xlsx"))
  write.xlsx(perf[, c("IPD", "Sellout")], paste0(save_location, "/", position2$Filename[1], ".xlsx"))
}
