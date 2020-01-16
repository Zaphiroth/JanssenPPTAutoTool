# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janseen dashboard
# Purpose:      Mainbody, CPD_RPD and CPD_Sellout_by_channel_by_model
#               Tables and Charts Generator
# programmer:   Jessica Liu
# modifier:     Xin Huang
# Date:         01-07-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


CPD_Daktarin_family_generator <- function(save_location,
                                          mapping_tbl,
                                          inter_market,
                                          PCMC_mapping,
                                          retail_mkt_daktarin,
                                          dist){
  
  # preset
  BU_target <-
    c("Primary Care", "Neurology", "Derm/Anti-allergy", "Psychiatry")
  IPD_target <-
    c("Hematology", "Immunology", "Solid Tumor")
  PC_target_brand <-
    c("Motilium", "Motrin", "Gold_Daktarin", "Daktarin", "AdultCold")
  Neuro_target_brand <-
    c("Sibelium", "Topamax", "Cipramil")
  DA_target_brand <-
    c("Rhinocort", "Sporanox")
  PSY_brand <-
    c("Invega", "Concerta", "Invega Trinza", "Sustenna")
  
  cpd_f3_time <- inter_market %>%
    select(`Year/Month`) %>%
    distinct() %>%
    arrange(desc(`Year/Month`))
  
  cpd_f3_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  
  cpd_f3_month <- as.numeric(substr(max(cpd_f3_time), 5, 6))
  YTD_time_max <- max(cpd_f3_time$`Year/Month`)
  YTD_time_min <- min(cpd_f3_time$`Year/Month`[grepl(cpd_f3_year_max, cpd_f3_time$`Year/Month`)])
  YTD_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  YTD_year_min <- as.numeric(substr(min(cpd_f3_time), 1, 4))
  
  YTD_month <- as.numeric(substr(max(cpd_f3_time), 5, 6))
  YTD_month_max <- YTD_month
  
  cpd_f3_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  
  inter_market_m <- inter_market %>% 
    filter(`Year/Month` <= YTD_time_max & `Year/Month` >= YTD_time_min) %>%
    select(Division, E2, Franchise, Model, N8,
           `Year/Month`, `产品名称（Product)`,
           `品牌（Brand)`, `渠道（Channel)`, 
           Province = `行政省名称（Province.Name)`,
           pre_volume = `去年销售数量（Last.Year.Sales)`,
           pre_sales = `去年销售金额（Last.Year.Sales.AMT)`,
           target = `本年指标金额（Target.AMT)`,
           volume = `本年销售数量（Sales.QTY）`,
           sales = `本年销售金额（Sales_AMT)`,
           CY_Ach = `CY.Ach.(Val)`,
           PY_Ach = `PY.Ach.(Val)`) 
  
  inter_market_m1 <- inter_market_m %>% 
    left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8")
  
  ## P52
  p52_2_1 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`)))) %>% 
    filter(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS") & Model %in% c("RPD")) %>% 
    group_by(`品牌（Brand)`, Channel) %>% 
    summarise(Sellout = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    spread(`品牌（Brand)`, Sellout) %>% 
    mutate(contri_dak = Daktarin/sum(Daktarin, na.rm = T),
           contri_gold = Gold_Daktarin/sum(Gold_Daktarin, na.rm = T))
  
  p52_2_2 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray", "Daktarin Powder")) %>% 
    filter(Channel %in% c("IDS", "KA DS") & Model %in% c("RPD")) %>% 
    group_by(Product, Channel) %>% 
    summarise(Sellout = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    spread(Product, Sellout) %>% 
    mutate(contri_powder = `Daktarin Powder`/sum(`Daktarin Powder`, na.rm = T),
           contri_spray = `Daktarin Spray`/sum(`Daktarin Spray`, na.rm = T))
  
  KPI_by_product_by_channel_KPI <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray") | `品牌（Brand)` %in% c("Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS") & Model %in% c("RPD")) %>% 
    group_by(Channel) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
    ungroup()
  
  p52_2_3 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray") | `品牌（Brand)` %in% c("Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS") & Model %in% c("RPD")) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
    mutate(Channel = "Total") %>% 
    full_join(KPI_by_product_by_channel_KPI) %>% 
    select(Channel, everything())
  
  p52_3 <- inter_market %>% 
    select(Division, E2, Franchise, Model, N8,
           `Year/Month`, `产品名称（Product)`,
           `品牌（Brand)`, `渠道（Channel)`, 
           Province = `行政省名称（Province.Name)`,
           pre_volume = `去年销售数量（Last.Year.Sales)`,
           pre_sales = `去年销售金额（Last.Year.Sales.AMT)`,
           target = `本年指标金额（Target.AMT)`,
           volume = `本年销售数量（Sales.QTY）`,
           sales = `本年销售金额（Sales_AMT)`,
           CY_Ach = `CY.Ach.(Val)`,
           PY_Ach = `PY.Ach.(Val)`) %>%
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Dakgold Cream 20mg/g 1*15g"), "Gold_Daktarin", NA)),
           date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
    arrange(date) %>% 
    filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
    filter(!is.na(Product) & Channel %in% c("KA DS") & Model %in% c("RPD")) %>% 
    group_by(Product, `Year/Month`) %>% 
    summarise(Sellout = sum(sales, na.rm = T)/1000000) %>% 
    ungroup() %>% 
    spread(Product, Sellout)
  
  ##-- Note: Retail
  cpd_f52_4 <- retail_mkt_daktarin[1:which(retail_mkt_daktarin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("AFTotal", "DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS",
                                    "DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS", "DING KE / QILU PHARM / ANTIFUNGALS")) %>% 
    mutate(`Value.('000RMB)` = ifelse(`Value.('000RMB)` %in% c("DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS"), "Daktarin Spray",
                                      ifelse(`Value.('000RMB)` %in% c("DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS"), "Gold_Daktarin",`Value.('000RMB)`))) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(Year, Month,`Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(AFTotal = first(Value_ttl),
           MS = Value_ttl/AFTotal) %>% 
    unite_("date", c("Year", "Month"), sep = "") %>% 
    filter(`Value.('000RMB)` != "AFTotal") %>% 
    select(-c(Value_ttl, AFTotal)) %>% 
    spread(`Value.('000RMB)`, MS) %>% 
    mutate(date_m = ymd(paste(gsub("M", "", date), "01" , sep = ""))) %>% 
    filter(date_m <= last(date_m) & date_m >= last(date_m) - months(x = 11)) %>% 
    select(-date_m)
  
  
  
  write.xlsx(p52_2_1, paste0(save_location, "/", "cpd_f52_2_1", ".xlsx"), sep = ",")
  
  write.xlsx(p52_2_2, paste0(save_location, "/", "cpd_f52_2_2", ".xlsx"), sep = ",")
  
  write.xlsx(p52_2_3, paste0(save_location, "/", "cpd_f52_2_3", ".xlsx"), sep = ",")
  
  write.xlsx(p52_3, paste0(save_location, "/", "cpd_f52_3", ".xlsx"), sep = ",")
  
  write.xlsx(cpd_f52_4, paste0(save_location, "/", "cpd_f52_4", ".xlsx"), sep = ",")
  
  
  ##-- P53
  
  p53_2_1 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`)))) %>% 
    filter(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS", "CSHQ") & Model %in% c("NDC bottom up")) %>% 
    group_by(`品牌（Brand)`, Channel) %>% 
    summarise(Sellout = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    spread(`品牌（Brand)`, Sellout) %>% 
    mutate(contri_dak = Daktarin/sum(Daktarin, na.rm = T),
           contri_gold = Gold_Daktarin/sum(Gold_Daktarin, na.rm = T))
  
  p53_2_2 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray", "Daktarin Powder")) %>% 
    filter(Channel %in% c("IDS", "KA DS", "CSHQ") & Model %in% c("NDC bottom up")) %>% 
    group_by(Product, Channel) %>% 
    summarise(Sellout = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    spread(Product, Sellout) %>% 
    mutate(contri_powder = `Daktarin Powder`/sum(`Daktarin Powder`, na.rm = T),
           contri_spray = `Daktarin Spray`/sum(`Daktarin Spray`, na.rm = T))
  
  KPI_by_product_by_channel_KPI <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray") | `品牌（Brand)` %in% c("Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS", "CSHQ") & Model %in% c("NDC bottom up")) %>% 
    group_by(Channel) %>% 
    summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
    ungroup()
  
  p53_2_3 <- inter_market_m %>% 
    mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                            ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                   ifelse(`渠道（Channel)` == "连锁分店", "KA DS", `渠道（Channel)`))),
           Product = ifelse(`产品名称（Product)`  %in% c("Daktarin Spray 1x30ml", "Daktarin Spray 1x15ml"), "Daktarin Spray",
                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1X40g", "Daktarin Powder 1*20g"), "Daktarin Powder", `产品名称（Product)`))) %>% 
    filter(Product %in% c("Daktarin Spray") | `品牌（Brand)` %in% c("Gold_Daktarin")) %>% 
    filter(Channel %in% c("IDS", "KA DS", "CSHQ") & Model %in% c("NDC bottom up")) %>% 
    summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
    mutate(Channel = "Total") %>% 
    full_join(KPI_by_product_by_channel_KPI) %>% 
    select(Channel, everything())
  
  write.xlsx(p53_2_1, paste0(save_location, "/", "cpd_f53_2_1", ".xlsx"), sep = ",")
  
  write.xlsx(p53_2_2, paste0(save_location, "/", "cpd_f53_2_2", ".xlsx"), sep = ",")
  
  write.xlsx(p53_2_3, paste0(save_location, "/", "cpd_f53_2_3", ".xlsx"), sep = ",")
  
  product <- mapping_tbl %>% 
    filter(Page == "cpd_f53_3_1") %>% 
    select(Product) %>% 
    distinct() %>% 
    unlist()
  
  model <- mapping_tbl %>% 
    filter(Page == "cpd_f53_3_1") %>% 
    select(Model) %>% 
    distinct() %>% 
    unlist()
  
  p53_3_1 <- dist %>% 
    filter(Product %in% product & Model %in% model & Key.Customer %in% c("连锁Top800")) %>% 
    mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
    arrange(date) %>% 
    filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
    group_by(`Year/Month`, Product) %>% 
    summarise(prec = sum(`R6铺点数量`, na.rm = T)/sum(`母体数量`, na.rm = T)) %>% 
    ungroup() %>% 
    spread(Product, prec)
  
  product <- mapping_tbl %>% 
    filter(Page == "cpd_f53_4_1") %>% 
    select(Product) %>% 
    distinct() %>% 
    unlist()
  
  model <- mapping_tbl %>% 
    filter(Page == "cpd_f53_4_1") %>% 
    select(Model) %>% 
    distinct() %>% 
    unlist()
  
  p53_4_1 <- dist %>% 
    filter(Product %in% product & Model %in% model & Key.Customer %in% c("连锁Top800")) %>% 
    mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
    arrange(date) %>% 
    filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
    group_by(`Year/Month`) %>% 
    summarise(prec = sum(`R6铺点数量`, na.rm = T)/sum(`母体数量`, na.rm = T)) %>% 
    ungroup()
  
  write.xlsx(p53_3_1, paste0(save_location, "/", "cpd_f53_3_1", ".xlsx"), sep = ",")
  write.xlsx(p53_4_1, paste0(save_location, "/", "cpd_f53_4_1", ".xlsx"), sep = ",")
  
}
