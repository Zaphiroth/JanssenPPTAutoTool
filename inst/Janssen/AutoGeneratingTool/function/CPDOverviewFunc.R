# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janseen dashboard
# Purpose:      Overview Tables and Charts Generator
# programmer:   Jessica Liu
# modifier:     Xin Huang
# Date:         07-11-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# overview for generator tables and charts ---------------------------
CPD_overview_function <- function(save_location,
                                  mapping_tbl,
                                  retail_mkt_daktarin,
                                  inter_market,
                                  lookup,
                                  exter_hos,
                                  cpd_f3_time,
                                  retail_mkt_motrin,
                                  retail_mkt_motilium,
                                  retail_mkt_rhinocort,
                                  inventory,
                                  PCMC_mapping,
                                  MR_call_drop,
                                  MR_call_undrop,
                                  MR_call){
  
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
  # YTD_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  # YTD_year_min <- as.numeric(substr(min(cpd_f3_time), 1, 4))

  # YTD_month <- as.numeric(substr(max(cpd_f3_time), 5, 6))
  # YTD_month_max <- YTD_month

  cpd_f3_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  
  ##---------------------------- CPD_f3 ------------------------------------------
  YTD_year_max <- as.numeric(substr(max(cpd_f3_time), 1, 4))
  YTD_year_min <- YTD_year_max - 1
  
  YTD_month_max <- as.numeric(substr(max(cpd_f3_time), 5, 6))
  
  final_page <- "cpd_f3"
  
  inter_market_m <- inter_market %>% 
    filter(`Year/Month` <= YTD_time_max & `Year/Month` >= YTD_time_min)
  
  category <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Category) %>% 
    distinct() %>% 
    unlist()
  
  display_name <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Display.name) %>% 
    distinct() %>% 
    unlist()
  
  BU_target <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(BU_target) %>% 
    distinct() %>% 
    unlist()
  
  lookup_m <- lookup %>% 
    mutate(Name_EN = ifelse(Name_EN %in% c("Invega Trinza", "Sustenna"), "LAT",
                            ifelse(Name_EN %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", Name_EN)))
  
  cpd_f3_EI_hos_m1 <- exter_hos %>% 
    left_join(lookup_m %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                                 "Product" = "Name_External_Hos_Product")) %>% 
    mutate(BU = ifelse(Product %in% c("Tylenol Cold       Jjs"), "Primary Care", BU),
           Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", Name_EN)) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_min) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    group_by(Defined.Market, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup()
  
  cpd_f3_EI_hos_m2 <- exter_hos %>% 
    left_join(lookup_m %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                                 "Product" = "Name_External_Hos_Product")) %>% 
    mutate(BU = ifelse(Product %in% c("Tylenol Cold       Jjs"), "Primary Care", BU),
           Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", Name_EN)) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    group_by(Name_EN, Defined.Market, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    left_join(cpd_f3_EI_hos_m1, by = c("Defined.Market", "Year")) %>% 
    mutate(MS = EI.x/EI.y) %>% 
    select(Name_EN, Defined.Market, Year, MS) %>% 
    filter(!is.na(Name_EN), !is.na(Defined.Market)) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-"))) %>% 
    setDT() %>% 
    dcast(Name_EN + Defined.Market ~ year_type, value.var = "MS") %>% 
    mutate(EI_hos = (current/past)*100) %>% 
    filter(!is.na(Name_EN)) %>% 
    select(Name_EN, Defined.Market, EI_hos)
  
  retail_mkt_daktarin_m <- retail_mkt_daktarin[1:which(retail_mkt_daktarin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("AFTotal","Daktarin Family")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(Year, `Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `Daktarin Family`/AFTotal) %>% 
    select(Year, MS) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-")),
           `Strategic Brand` = "Daktarin Family") %>% 
    setDT() %>% 
    dcast(`Strategic Brand` ~ year_type, value.var = "MS") %>% 
    mutate(EI_retail = (current/past)*100) %>% 
    select(`Strategic Brand`, EI_retail)
  
  retail_mkt_motrin_m <- retail_mkt_motrin[1:which(retail_mkt_motrin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("Pain PEDTotal", "MOTRIN / J & J SHANGHAI / GENERAL PAIN RELIEF")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(Year, `Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `MOTRIN / J & J SHANGHAI / GENERAL PAIN RELIEF`/`Pain PEDTotal`) %>% 
    select(Year, MS) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-")),
           `Strategic Brand` = "Motrin") %>% 
    setDT() %>% 
    dcast(`Strategic Brand` ~ year_type, value.var = "MS") %>% 
    mutate(EI_retail = (current/past)*100) %>% 
    select(`Strategic Brand`, EI_retail)
  
  retail_mkt_motilium_m <- retail_mkt_motilium[1:which(retail_mkt_motilium$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("GITotal", "MOTILIUM / JANSSEN-XIAN / DIGESTIVE TRACT AND STOMACH PRODUCTS")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(Year, `Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `MOTILIUM / JANSSEN-XIAN / DIGESTIVE TRACT AND STOMACH PRODUCTS`/`GITotal`) %>% 
    select(Year, MS) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-")),
           `Strategic Brand` = "Motilium") %>% 
    setDT() %>% 
    dcast(`Strategic Brand` ~ year_type, value.var = "MS") %>% 
    mutate(EI_retail = (current/past)*100) %>% 
    select(`Strategic Brand`, EI_retail)
  
  retail_mkt_rhinocort_m <- retail_mkt_rhinocort[1:which(retail_mkt_rhinocort$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("Nasal Topical (OTC+RX)", "RHINOCORT AQUA / ASTRAZENECA / Nasal corticosteroids without anti-infectives")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(Year, `Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `RHINOCORT AQUA / ASTRAZENECA / Nasal corticosteroids without anti-infectives`/`Nasal Topical (OTC+RX)`) %>% 
    select(Year, MS) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-")),
           `Strategic Brand` = "Rhinocort") %>% 
    setDT() %>% 
    dcast(`Strategic Brand` ~ year_type, value.var = "MS") %>% 
    mutate(EI_retail = (current/past)*100) %>% 
    select(`Strategic Brand`, EI_retail)
  
  retail_EI_combine <- bind_rows(retail_mkt_daktarin_m, retail_mkt_motrin_m, retail_mkt_motilium_m, retail_mkt_rhinocort_m)
  
  cpd_f3_m1 <- inter_market_m %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Invega Trinza", "Sustenna"), "LAT", 
                                ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`))) %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(`品牌（Brand)` %in% display_name) %>%
    group_by(`Strategic Brand` = `品牌（Brand)`, Franchise) %>% 
    summarise(`YTD Sales Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `YTD Sales Growth%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1) %>%
    ungroup() %>% 
    left_join(cpd_f3_EI_hos_m2, by = c("Strategic Brand" = "Name_EN")) %>% 
    left_join(retail_EI_combine, by = "Strategic Brand") %>% 
    mutate(`Strategic Brand` = factor(`Strategic Brand`, levels = display_name),
           Franchise = factor(Franchise, levels = BU_target)) %>% 
    arrange(`Strategic Brand`) %>% 
    select(BU = Franchise, everything(), -Defined.Market)
  
  
  cpd_f3_m1_f <- as.formula("`BU`* `Strategic Brand` ~ (`YTD Sales Ach%` + `YTD Sales Growth%` + `EI_hos` + `EI_retail`) * Heading() * (identity)")
  
  rownum <- with(cpd_f3_m1, RowNum(list(cpd_f3_m1$BU, cpd_f3_m1$`Strategic Brand`)))
  
  cpd_f3_m1_2 <- tabular(`BU`* `Strategic Brand`*I(rownum) ~ (`YTD Sales Ach%` + `YTD Sales Growth%` + `EI_hos` + `EI_retail`) * AllObs(cpd_f3_m1, within = list(BU, `Strategic Brand`, rownum)), cpd_f3_m1)
  
  write.table.tabular(cpd_f3_m1_2, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  
  tmp[] <- lapply(tmp, function(x) {
    x[grepl("NA", x, perl = FALSE)] <- ""
    x
  })
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f3.xlsx"))
  
  ##---------------------------- CPD_f4 ------------------------------------------
  final_page <- "cpd_f4"
  
  category <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Category) %>% 
    distinct() %>% 
    unlist()
  
  display_name <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Display.name) %>% 
    distinct() %>% 
    unlist()
  
  inter_market_m1 <- inter_market_m %>% 
    mutate(Model_m = ifelse(Model %in% c("NDC bottom up"), "NDC bottom up", "DC"))
  
  Total <- inter_market_m1 %>% 
    summarise(Sales = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),          
              `GR%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              Sales2 = (sum(`本年销售金额（Sales_AMT)`[!`品牌（Brand)` %in% c("Lexapro")], na.rm = T) - sum(`去年销售金额（Last.Year.Sales.AMT)`[!`品牌（Brand)` %in% c("Lexapro")], na.rm = T))/1000000,
              Sales_DC = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/1000000,
              `Ach%_DC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "DC"], na.rm = T),          
              `GR%_DC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "DC"], na.rm = T) - 1),
              Sales_NDC = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/1000000,
              `Ach%_NDC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "NDC bottom up"], na.rm = T),          
              `GR%_NDC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "NDC bottom up"], na.rm = T) - 1)) %>% 
    mutate(`Mn RMB`  = "Total") %>% 
    select(`Mn RMB`, everything())
  
  cpd_inclu_lex <- inter_market_m1 %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(Franchise %in% BU_target) %>% 
    summarise(Sales = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),          
              `GR%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              Sales2 = Sales - sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              Sales_DC = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/1000000,
              `Ach%_DC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "DC"], na.rm = T),          
              `GR%_DC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "DC"], na.rm = T) - 1),
              Sales_NDC = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/1000000,
              `Ach%_NDC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "NDC bottom up"], na.rm = T),          
              `GR%_NDC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "NDC bottom up"], na.rm = T) - 1)) %>% 
    mutate(`Mn RMB`  = "CPD incl. Lex") %>% 
    select(`Mn RMB`, everything())
  
  cpd_exclu_lex <- inter_market_m1 %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    summarise(Sales = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),          
              `GR%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              Sales2 = Sales - sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              Sales_DC = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/1000000,
              `Ach%_DC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "DC"], na.rm = T),          
              `GR%_DC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "DC"], na.rm = T) - 1),
              Sales_NDC = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/1000000,
              `Ach%_NDC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "NDC bottom up"], na.rm = T),          
              `GR%_NDC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "NDC bottom up"], na.rm = T) - 1)) %>% 
    mutate(`Mn RMB` = "CPD excl. Lex",
           `Incremental%` = Sales2/Total$Sales2) %>% 
    select(`Mn RMB`, everything())
  
  cpd_f4_m1 <- inter_market_m1 %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% c(BU_target, IPD_target, "Alliance Management - XJP")) %>% 
    group_by(`Mn RMB` = Franchise) %>%
    summarise(Sales = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),          
              `GR%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              Sales2 = Sales - sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              Sales_DC = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/1000000,
              `Ach%_DC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "DC"], na.rm = T),          
              `GR%_DC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "DC"], na.rm = T) - 1),
              Sales_NDC = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/1000000,
              `Ach%_NDC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "NDC bottom up"], na.rm = T),          
              `GR%_NDC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "NDC bottom up"], na.rm = T) - 1)) %>% 
    ungroup() %>% 
    mutate(`Incremental%` = Sales2/Total$Sales2)
  
  ipd <- inter_market_m1 %>% 
    filter(Franchise %in% IPD_target) %>% 
    summarise(Sales = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),          
              `GR%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              Sales2 = Sales - sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              Sales_DC = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/1000000,
              `Ach%_DC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "DC"], na.rm = T),          
              `GR%_DC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "DC"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "DC"], na.rm = T) - 1),
              Sales_NDC = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/1000000,
              `Ach%_NDC` = sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`本年指标金额（Target.AMT)`[Model_m == "NDC bottom up"], na.rm = T),          
              `GR%_NDC` = (sum(`本年销售金额（Sales_AMT)`[Model_m == "NDC bottom up"], na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`[Model_m == "NDC bottom up"], na.rm = T) - 1)) %>% 
    mutate(`Mn RMB` = "IPD",
           `Incremental%` = Sales2/Total$Sales2) %>% 
    select(`Mn RMB`, everything())
  
  cpd_f4_m2 <- bind_rows(Total, cpd_f4_m1, cpd_inclu_lex, cpd_exclu_lex, ipd) %>% 
    mutate(`Mn RMB` = factor(`Mn RMB`, levels = display_name)) %>% 
    arrange(`Mn RMB`) %>% 
    select(`Mn RMB`, Sales, `Ach%`, `GR%`, `Incremental%`, everything(), -Sales2)
  
  cpd_f4_sales <- cpd_f4_m2 %>% 
    select(`Mn RMB`, Sales)
  
  cpd_f4_incre <- cpd_f4_m2 %>% 
    select(`Mn RMB`, `Incremental%`)
  
  cpd_f4_salesdc<- cpd_f4_m2 %>% 
    select(`Mn RMB`, Sales_DC)
  
  cpd_f4_salesndc <- cpd_f4_m2 %>% 
    select(`Mn RMB`, Sales_NDC)
  
  # write.xlsx(cpd_f4_m2, "03_Outputs/CPD/tmp.xlsx")
  # 
  # tmp <- read.xlsx("03_Outputs/CPD/tmp.xlsx")
  # 
  # tmp <- tmp %>% 
  #   mutate(Sales = "",
  #          `Incremental%` = "",
  #          Sales_DC = "",
  #          Sales_NDC = "")
  # 
  # tmp[] <- lapply(tmp, function(x) {
  #   x[grepl("NA", x, perl = FALSE)] <- ""
  #   x
  # })
  
  cpd_f4_m2[is.na(cpd_f4_m2) ] <- ""
  
  colnames(cpd_f4_m2) <- c("Mn RMB", "Sales", "Ach%", "GR%", "Incremental%", 
                           "Sales", "Ach%", "GR%", "Sales", "Ach%", "GR%")
  
  write.xlsx(cpd_f4_m2, paste0(save_location, "/cpd_f4.xlsx"))
  write.xlsx(cpd_f4_sales, paste0(save_location, "/cpd_f4_1.xlsx"))
  write.xlsx(cpd_f4_incre, paste0(save_location, "/cpd_f4_2.xlsx"))
  write.xlsx(cpd_f4_salesdc, paste0(save_location, "/cpd_f4_3.xlsx"))
  write.xlsx(cpd_f4_salesndc, paste0(save_location, "/cpd_f4_4.xlsx"))
  
  ##---------------------------- CPD_f5 ------------------------------------------
  
  final_page <- "cpd_f5"
  
  category <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Category) %>% 
    distinct() %>% 
    unlist()
  
  display_name <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Display.name) %>% 
    distinct() %>% 
    unlist()
  
  BU_target <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(BU_target) %>% 
    distinct() %>% 
    unlist()
  
  PSY_f5_brand <- c("Invega", "Concerta", "Sustenna")
  
  MTD_CPD_total <- inter_market %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    filter(Franchise %in% BU_target & !(`品牌（Brand)` %in% "Lexapro")) %>%
    filter(`Year/Month` == YTD_time_max) %>% 
    summarise(`Ach%_MTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_MTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    mutate(` ` = "tmp1")
  
  MTD_f5_total <- inter_market %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    filter(Franchise %in% BU_target & `品牌（Brand)` %in% c(PC_target_brand, Neuro_target_brand, DA_target_brand, PSY_f5_brand)) %>%
    filter(`Year/Month` == YTD_time_max) %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    summarise(`Ach%_MTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_MTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    mutate(` ` = "tmp2")
  
  MTD_CPD_brand <- inter_market %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    filter(Franchise %in% BU_target & `品牌（Brand)` %in% c(PC_target_brand, Neuro_target_brand, DA_target_brand, PSY_f5_brand)) %>%
    filter(`Year/Month` == YTD_time_max) %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>%
    group_by(` ` = `品牌（Brand)`, BU = Franchise) %>% 
    summarise(`Ach%_MTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_MTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    ungroup()
  
  contri_total <- inter_market %>% 
    filter(Franchise %in% BU_target & !(`品牌（Brand)` %in% "Lexapro")) %>%
    filter(`Year/Month` <= YTD_time_max & `Year/Month` >= YTD_time_min) %>%
    summarise(`Sellout_YTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000)
  
  cpd_f5_total <- inter_market_m %>% 
    filter(Franchise %in% BU_target & !(`品牌（Brand)` %in% "Lexapro")) %>%
    summarise(`Sellout_YTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Contri.%_YTD` = `Sellout_YTD`/contri_total$Sellout_YTD,
              `Ach%_YTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_YTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    bind_cols(MTD_CPD_total) %>% 
    mutate(`Mn RMB` = "CPD Total")
  
  f5_total <- inter_market_m %>% 
    # mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    filter(Franchise %in% BU_target & `品牌（Brand)` %in% c(PC_target_brand, Neuro_target_brand, DA_target_brand, PSY_f5_brand)) %>%
    summarise(`Sellout_YTD`= sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Contri.%_YTD` = `Sellout_YTD`/cpd_f5_total[1,1],
              `Ach%_YTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_YTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    bind_cols(MTD_f5_total) %>% 
    mutate(`Mn RMB` = "Total")
  
  # cpd_f5_MS_m1 <- exter_hos %>% 
  #   left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
  #                                                                                                              "Product" = "Name_External_Hos_Product")) %>%
  #   mutate(BU = ifelse(Product %in% c("Tylenol Cold       Jjs"), "Primary Care", 
  #                      ifelse(Product %in% c("Daktarin       Xjs", "Daktarin Gold      Xjs"), "Primary Care", BU)),
  #          Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", 
  #                           ifelse(Product %in% c("Daktarin       Xjs", "Daktarin Gold      Xjs"), "Daktarin Family", Name_EN))) %>% 
  #   filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_min) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
  #   filter(KPI %in% c("Volume ('000)")) %>% 
  #   group_by(BU, Name_EN, Year) %>% 
  #   summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
  #   spread(Year, EI)
  # 
  # cpd_f5_MS_m2 <- exter_hos %>% 
  #   left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
  #                                                                                                              "Product" = "Name_External_Hos_Product")) %>% 
  #   mutate(BU = ifelse(Product %in% c("Tylenol Cold       Jjs"), "Primary Care", 
  #                      ifelse(Product %in% c("Daktarin       Jjs", "Daktarin Gold      Jjs"), "Primary Care", BU)),
  #         Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", 
  #                          ifelse(Product %in% c("Daktarin       Jjs", "Daktarin Gold      Jjs"), "Daktarin Family", Name_EN))) %>% 
  #   filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_min) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
  #   filter(KPI %in% c("Volume ('000)")) %>% 
  #   group_by(BU, Year) %>% 
  #   summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
  #   spread(Year, EI) %>% 
  #   left_join(cpd_f5_MS_m1, by = c("BU")) %>% 
  #   mutate(MS_YTD = `2019.y`/`2019.x`,
  #          `Change vs. LY MS%` = `2019.y`/`2019.x` - `2018.y`/`2018.x`) %>% 
  #   select(BU, Name_EN, `Change vs. LY MS%`, MS_YTD)
  
  cpd_f5_MS_m1 <- exter_hos %>% 
    left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                               "Product" = "Name_External_Hos_Product")) %>%
    mutate(Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", 
                            ifelse(Product %in% c("Daktarin           Xjs", "Daktarin Gold      Xjs"), 
                                   "Daktarin Family", Name_EN))) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_min) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    group_by(Defined.Market, Name_EN, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    filter(!is.na(Name_EN)) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-"))) %>% 
    setDT() %>% 
    dcast(Defined.Market + Name_EN ~ year_type, value.var = "EI")
  
  cpd_f5_MS_m2 <- exter_hos %>% 
    left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                               "Product" = "Name_External_Hos_Product")) %>% 
    mutate(Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", 
                            ifelse(Product %in% c("Daktarin           Xjs", "Daktarin Gold      Xjs"), 
                                   "Daktarin Family", Name_EN))) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_min) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    group_by(Defined.Market, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-"))) %>% 
    setDT() %>% 
    dcast(Defined.Market ~ year_type, value.var = "EI") %>% 
    left_join(cpd_f5_MS_m1, by = c("Defined.Market")) %>% 
    mutate(MS_YTD = current.y/current.x,
           `Change vs. LY MS%` = current.y/current.x - past.y/past.x) %>% 
    select(Defined.Market, Name_EN, `Change vs. LY MS%`, MS_YTD) %>% 
    ungroup()
  
  
  ##-- Dartarin + Dartarin Gold
  cpd_f5_inventory <- inventory %>% 
    mutate(`Brand En` = ifelse(`Brand En` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `Brand En`),
           `Year/Month` = as.numeric(stri_sub(stri_replace_all_fixed(`Year/Month`, "-", ""), 1, 6))) %>% 
    filter(`Year/Month` == YTD_time_max) %>% 
    filter(Franchise %in% BU_target & `Brand En` %in% display_name) %>% 
    group_by(`Brand En`, Tier) %>% 
    summarise(`Inventory Days` = sum(`Inventory Days`, na.rm = T)) %>% 
    ungroup() %>% 
    filter(Tier %in% c("1", "2")) %>% 
    spread(Tier, `Inventory Days`) 
  
  daktarin_ms <- retail_mkt_daktarin[1:which(retail_mkt_daktarin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("AFTotal","Daktarin Family")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% YTD_year_max & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(`Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `Daktarin Family`/AFTotal,
           `Strategic Brand` = "Daktarin Family") %>% 
    select(`Strategic Brand`, MS)
  
  rhinocort_ms <- retail_mkt_rhinocort[1:which(retail_mkt_rhinocort$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("Nasal Topical (OTC+RX)", "RHINOCORT AQUA / ASTRAZENECA / Nasal corticosteroids without anti-infectives")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% YTD_year_max & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(`Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `RHINOCORT AQUA / ASTRAZENECA / Nasal corticosteroids without anti-infectives`/`Nasal Topical (OTC+RX)`,
           `Strategic Brand` = "Rhinocort") %>% 
    select(`Strategic Brand`, MS)
  
  motrin_ms <- retail_mkt_motrin[1:which(retail_mkt_motrin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
    filter(`Value.('000RMB)` %in% c("Pain PEDTotal", "MOTRIN / J & J SHANGHAI / GENERAL PAIN RELIEF")) %>% 
    gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
    separate(Date, c("Year", "Month"), sep = "M") %>% 
    filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
    group_by(`Value.('000RMB)`) %>% 
    summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    spread(`Value.('000RMB)`, Value_ttl) %>% 
    mutate(MS = `MOTRIN / J & J SHANGHAI / GENERAL PAIN RELIEF`/`Pain PEDTotal`,
           `Strategic Brand` = "Motrin") %>% 
    select(`Strategic Brand`, MS)
  
  retail_ms_combine <- bind_rows(daktarin_ms, rhinocort_ms, motrin_ms)
  
  cpd_f5_EI_hos_m1 <- exter_hos %>% 
    left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                               "Product" = "Name_External_Hos_Product")) %>% 
    mutate(Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", Name_EN)) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    filter(!is.na(Name_EN)) %>% 
    group_by(Defined.Market, Name_EN, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup()
  
  cpd_f5_EI_hos_m2 <- exter_hos %>% 
    left_join(lookup %>% select(BU, Name_EN, Name_External_Hos_Defined.MKT, Name_External_Hos_Product), by = c("Defined.Market" = "Name_External_Hos_Defined.MKT", 
                                                                                                               "Product" = "Name_External_Hos_Product")) %>% 
    mutate(Name_EN = ifelse(Product %in% c("Tylenol Cold       Jjs"), "AdultCold", Name_EN)) %>% 
    filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
    filter(KPI %in% c("Volume ('000)")) %>% 
    group_by(Defined.Market, Year) %>% 
    summarise(EI = sum(as.numeric(Value), na.rm = T)) %>% 
    ungroup() %>% 
    left_join(cpd_f5_EI_hos_m1, by = c("Defined.Market", "Year")) %>% 
    mutate(MS = EI.y/EI.x) %>% 
    select(Defined.Market, Name_EN, Year, MS) %>% 
    mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                              ifelse(Year == YTD_year_max - 1, "past", 
                                     "-"))) %>% 
    setDT() %>% 
    dcast(Defined.Market + Name_EN ~ year_type, value.var = "MS") %>% 
    mutate(EI_hos = current/past*100) %>% 
    filter(!is.na(Name_EN)) %>% 
    select(Defined.Market, Name_EN, EI_hos) %>% 
    ungroup()
  
  ##-- EI issues
  cpd_f5_combine <- inter_market_m %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
    filter(Franchise %in% BU_target & `品牌（Brand)` %in% display_name) %>%
    group_by(` ` = `品牌（Brand)`, `Mn RMB` = Franchise) %>% 
    summarise(`Sellout_YTD`= sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Contri.%_YTD` = `Sellout_YTD`/cpd_f5_total[1,1],
              `Ach%_YTD` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%_YTD` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>%
    ungroup() %>% 
    left_join(MTD_CPD_brand, by = c("Mn RMB" = "BU", " ")) %>% 
    left_join(cpd_f5_MS_m2 %>% select(-Defined.Market), by = c(" " = "Name_EN")) %>% 
    left_join(cpd_f5_EI_hos_m2 %>% select(-Defined.Market), by = c(" " = "Name_EN")) %>% 
    left_join(retail_EI_combine, by = c(" " = "Strategic Brand")) %>% 
    left_join(cpd_f5_inventory, by = c(" " = "Brand En")) %>% 
    left_join(retail_ms_combine, by = c(" " = "Strategic Brand")) %>% 
    mutate(MS_YTD = ifelse(` ` %in% c("Daktarin Family", "Motrin", "Rhinocort"), MS, MS_YTD)) %>% 
    full_join(cpd_f5_total) %>% 
    full_join(f5_total) %>% 
    ungroup() %>% 
    rename("T1 Inventory Day_Aug" = `1`,
           "T2 Inventory Day_Aug" = `2`) %>% 
    mutate(`Mn RMB` = factor(`Mn RMB`, levels = BU_target),
           ` ` = factor(` `, levels = display_name)) %>% 
    arrange(`Mn RMB`, ` `)
  
  cpd_f5_m1_f <- as.formula("`Mn RMB` * ` ` ~ (Sellout_YTD + `Contri.%_YTD` + `Ach%_YTD` + `Gr%_YTD` + `Ach%_MTD` + `Gr%_MTD` + `MS_YTD` + `Change vs. LY MS%` + `EI_hos` + `EI_retail` + `T1 Inventory Day_Aug` +`T2 Inventory Day_Aug`) * AllObs(cpd_f5_combine, within = list(`Mn RMB`, ` `, rownum))")
  
  rownum <- with(cpd_f5_combine, RowNum(list(cpd_f5_combine$`Mn RMB`, cpd_f5_combine$` `)))
  
  cpd_f5_m1_1 <- tabular(cpd_f5_m1_f, cpd_f5_combine)
  
  write.table.tabular(cpd_f5_m1_1, paste0(save_location, "/tmp.xlsx"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.xlsx"))
  tmp[1, 2] <- ""
  tmp[2, 2] <- ""
  
  tmp[] <- lapply(tmp, function(x) {
    x[grepl("NA", x, perl = FALSE)] <- ""
    x
  })
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f5.xlsx"), sep = ",")
  
  cpd_f5_sellout <- tmp %>% 
    select(`Mn RMB`, ` `, `Sellout_YTD`)
  
  write.xlsx(cpd_f5_sellout, paste0(save_location, "/cpd_f5_1.xlsx"), sep = ",")
  
  ##---------------------------- CPD_f6 ------------------------------------------
  
  final_page <- "cpd_f6"  
  
  display_name <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Display.name) %>% 
    distinct() %>% 
    unlist()
  
  BU_target <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(BU_target) %>% 
    distinct() %>% 
    unlist()
  
  CPD_incl_Lex <- inter_market_m %>% 
    filter(Franchise %in% BU_target) %>% 
    summarise(`Sales ACT Last Year` = sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              Target = sum(`本年指标金额（Target.AMT)`, na.rm = T)/1000000, 1,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    mutate(Model = "CPD incl. Lex") %>% 
    select(Model, `Sales ACT Last Year`, `Sales ACT Current Year`, Target, `Ach%`, `Gr%`)
  
  CPD_minus <- inter_market_m %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(!(`品牌（Brand)` %in% "Lexapro")) %>% 
    summarise(total_contri = (sum(`本年销售金额（Sales_AMT)`, na.rm = T) - sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T))/1000000)
  
  CPD_excl_Lex <- inter_market_m %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(!(`品牌（Brand)` %in% "Lexapro")) %>% 
    summarise(`Sales ACT Last Year` = sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              Target = sum(`本年指标金额（Target.AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              `Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/CPD_minus$total_contri) %>% 
    mutate(Model = "CPD excl. Lex") %>% 
    select(Model, `Sales ACT Last Year`, `Sales ACT Current Year`, Target, `Ach%`, `Gr%`, `Growth Contribution%`)
  
  CPD_inter_pcmc <- inter_market_m %>% 
    left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8")
  
  DC <- CPD_inter_pcmc %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(!(`品牌（Brand)` %in% "Lexapro") & !(Model %in% c("NDC bottom up"))) %>% 
    summarise(`Sales ACT Last Year` = sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              Target = sum(`本年指标金额（Target.AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              `Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/CPD_minus$total_contri) %>% 
    mutate(Model = "DC") %>% 
    select(Model, `Sales ACT Last Year`, `Sales ACT Current Year`, Target, `Ach%`, `Gr%`, `Growth Contribution%`)
  
  CPD_by_model <- CPD_inter_pcmc %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(!(`品牌（Brand)` %in% "Lexapro")) %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Sales ACT Last Year` = sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              Target = sum(`本年指标金额（Target.AMT)`, na.rm = T)/1000000,
              `Ach%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              `Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/CPD_minus$total_contri) %>% 
    ungroup()
  
  level <- c("tmp1", "tmp2", "CPD")
  cpd_f6 <- bind_rows(CPD_incl_Lex, CPD_excl_Lex, DC, CPD_by_model) %>% 
    mutate(Model = factor(Model, levels = display_name)) %>% 
    arrange(Model) %>% 
    mutate(` ` = factor(c("tmp1", "tmp2", rep("CPD", 6)), levels = level))
  
  cpd_f6_f <- as.formula("` `* `Model` ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + `Target` + `Ach%` + `Gr%` + `Growth Contribution%`) * Heading() * (identity)")
  
  rownum <- with(cpd_f6, RowNum(list(cpd_f6$` `, cpd_f6$Model)))
  
  cpd_f6_m <- tabular(` ` * `Model` * I(rownum) ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + `Target` + `Ach%` + `Gr%` + `Growth Contribution%`) * AllObs(cpd_f6, within = list(` `, `Model`, rownum)), cpd_f6)
  
  
  write.table.tabular(cpd_f6_m, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  tmp[] <- lapply(tmp, function(x) {
    x[grepl("NA", x, perl = FALSE)] <- ""
    x
  })
  
  tmp[1:2,1] <- ""
  names(tmp)[which(names(tmp) == "Model")] <- ""
  write.xlsx(tmp, paste0(save_location, "/cpd_f6.xlsx"))
  
  cpd_by_model_contri <- CPD_inter_pcmc %>% 
    filter(Franchise %in% BU_target) %>% 
    filter(!(`品牌（Brand)` %in% "Lexapro")) %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(Sellout_past = sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T),
              Sellout_current = sum(`本年销售金额（Sales_AMT)`, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(`Contri.Last.Year` = Sellout_past/sum(Sellout_past, na.rm = T),
           `Contri.Current.Year` = Sellout_current/sum(Sellout_current, na.rm = T))
  
  cpd_by_model_contri_ttl <- cpd_by_model_contri %>% 
    summarise(Sellout_past = sum(Sellout_past, na.rm = T),
              Sellout_current = sum(Sellout_current, na.rm = T)) %>% 
    mutate(Model = "Sellout") %>% 
    select(Model, everything())
  
  write.xlsx(cpd_by_model_contri, paste0(save_location, "/cpd_f6_2.xlsx"))
  write.xlsx(cpd_by_model_contri_ttl, paste0(save_location, "/cpd_f6_3.xlsx"))
  
  
  ##---------------------------- CPD_P8 ------------------------------------------
  final_page <- "cpd_f8"
  
  BU_target <- c("Primary Care", "Neurology", "Derm/Anti-allergy", "Psychiatry")
  
  cpd_exclu_lex_f8_total <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    summarise(Sales = round(sum(`本年销售金额（Sales_AMT)`, na.rm = T)))
  
  cpd_f8 <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    group_by(Province = `行政省名称（Province.Name)`) %>% 
    summarise(`Contr.%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/cpd_exclu_lex_f8_total[1,1],
              `Ach%(MLP)` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%(STP)` = (sum(`CY.Ach.(Val)`, na.rm = T)/sum(`PY.Ach.(Val)`, na.rm = T) - 1),
              `Gr%(MLP)` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    ungroup() %>% 
    arrange(desc(`Contr.%`)) %>% 
    filter(row_number()<=15)
  
  cpd_f8_other <-  inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & !(`行政省名称（Province.Name)` %in% c(cpd_f8$Province))) %>% 
    summarise(`Contr.%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/cpd_exclu_lex_f8_total[1,1],
              `Ach%(MLP)` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%(STP)` = (sum(`CY.Ach.(Val)`, na.rm = T)/sum(`PY.Ach.(Val)`, na.rm = T) - 1),
              `Gr%(MLP)` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    arrange(desc(`Contr.%`)) %>% 
    mutate(Province = "Others") %>% 
    select(Province, everything())
  
  TOP_15_total <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    filter(`行政省名称（Province.Name)` %in% c(cpd_f8$Province)) %>% 
    summarise(`Contr.%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/cpd_exclu_lex_f8_total[1,1],
              `Ach%(MLP)` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%(STP)` = (sum(`CY.Ach.(Val)`, na.rm = T)/sum(`PY.Ach.(Val)`, na.rm = T) - 1),
              `Gr%(MLP)` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    mutate(Province = "TOP 15 Total") %>% 
    select(Province, everything())
  
  cpd_exclu_lex_f8 <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    summarise(`Contr.%` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/cpd_exclu_lex_f8_total[1,1],
              `Ach%(MLP)` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%(STP)` = (sum(`CY.Ach.(Val)`, na.rm = T)/sum(`PY.Ach.(Val)`, na.rm = T) - 1),
              `Gr%(MLP)` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    mutate(Province = "CPD excl. Lex") %>% 
    select(Province, everything())
  
  cpd_f8_m1 <- cpd_exclu_lex_f8 %>% 
    full_join(TOP_15_total) %>% 
    full_join(cpd_f8) %>% 
    full_join(cpd_f8_other)
  write.xlsx(cpd_f8_m1, paste0(save_location, "/cpd_f8.xlsx"))
  
  ##---------------------------- CPD_P9 ------------------------------------------
  
  final_page <- "cpd_f9_2"
  
  category <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Category) %>% 
    distinct() %>% 
    unlist()
  
  display_name <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Display.name) %>% 
    distinct() %>% 
    unlist()
  
  cpd_f9_1_m1 <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
    group_by(Province = `行政省名称（Province.Name)`) %>% 
    summarise(`CPD excl. Lex` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000,
              `Ach%(MLP)` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Gr%(MLP)` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1)) %>% 
    ungroup() %>% 
    arrange(desc(`CPD excl. Lex`)) %>% 
    filter(row_number()<=15)
  
  # cpd_f9_1_m2 <- t(cpd_f9_1_m1)
  # cpd_f9_1_m2 <- as.data.frame(cpd_f9_1_m2) %>% 
  #   mutate(` ` = rownames(cpd_f9_1_m2)) %>% 
  #   select(` `, everything())
  # names(cpd_f9_1_m2) <- cpd_f9_1_m2[1, ]
  # cpd_f9_1_m2 <- cpd_f9_1_m2[2:4, ]
  
  cpd_f9_1_m2 <- t(cpd_f9_1_m1)
  
  colnames(cpd_f9_1_m2) <- cpd_f9_1_m2[1, ]
  
  cpd_f9_1_m2 <- as.data.frame(cpd_f9_1_m2[-1, ]) %>% 
    mutate(Province = c("CPD excl. Lex", "Ach%(MLP)", "Gr%(MLP)" )) %>% 
    select(Province, everything())
  
  cpd_f9_2_total <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & `行政省名称（Province.Name)` %in% (unique(cpd_f8$Province))) %>% 
    summarise(`GR.` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              `Ach.` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Contr.` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000)/sum(cpd_f9_1_m1$`CPD excl. Lex`, na.rm = T)) %>% 
    mutate(` ` = "Top 15 Total")
  
  cpd_f9_2 <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & `行政省名称（Province.Name)` %in% (unique(cpd_f8$Province))) %>% 
    group_by(` ` = Model) %>% 
    summarise(`GR.` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`去年销售金额（Last.Year.Sales.AMT)`, na.rm = T) - 1),
              `Ach.` = sum(`本年销售金额（Sales_AMT)`, na.rm = T)/sum(`本年指标金额（Target.AMT)`, na.rm = T),
              `Contr.` = (sum(`本年销售金额（Sales_AMT)`, na.rm = T)/1000000)/sum(cpd_f9_1_m1$`CPD excl. Lex`, na.rm = T)) %>% 
    ungroup() %>% 
    full_join(cpd_f9_2_total) %>% 
    mutate(` ` = ifelse(` ` %in% "MR_Hospital", "MR_HOSPITAL", ` `),
           ` ` = factor(` `, levels = display_name)) %>% 
    arrange(` `) 
  
  cpd_f9_3 <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & `行政省名称（Province.Name)` %in% (unique(cpd_f8$Province))) %>% 
    group_by(Model, `行政省名称（Province.Name)`) %>% 
    summarise(Sellout =  sum(`本年销售金额（Sales_AMT)`, na.rm = T)) %>% 
    ungroup() %>% 
    spread(Model, Sellout) %>% 
    group_by(`行政省名称（Province.Name)`) %>% 
    mutate(EC = ifelse(is.na(EC), 0, EC),
           Total = EC + MR_Hospital + `NDC bottom up` + RPD,
           EC_contri = EC/Total,
           MR_Hospital_contri = MR_Hospital/Total,
           `NDC bottom up_contri` = `NDC bottom up`/Total,
           RETAIL_contri = RPD/Total) %>% 
    ungroup() %>% 
    rename("RETAIL" = RPD) %>% 
    arrange(desc(Total)) %>% 
    select(-Total)
  
  write.xlsx(cpd_f9_2, paste0(save_location, "/cpd_f9_2.xlsx"), sep = ",")
  write.xlsx(cpd_f9_3, paste0(save_location, "/cpd_f9_1.xlsx"), sep = ",")
  write.xlsx(cpd_f9_1_m2, paste0(save_location, "/cpd_f9_3.xlsx"), sep = ",")
  
  ##---------------------------- CPD_P10&P11 ------------------------------------------
  
  for (final_page in c("cpd_f10", "cpd_f11")) {
    # final_page <- "cpd_f10"
    # final_page <- "cpd_f11"
    
    display_name <- mapping_tbl %>% 
      filter(Page == final_page) %>% 
      select(Display.name) %>% 
      distinct() %>% 
      unlist()
    
    Type <- mapping_tbl %>% 
      filter(Page == final_page) %>% 
      select(Type) %>% 
      distinct() %>% 
      unlist()
    
    if(Type == "selected"){
      
      MR_call_sep <- MR_call_drop
      MR_call_ttl <- MR_call %>% 
        filter(E8 != "TBA" & !(City %in% c("天津市", "上海市")))
      
    }else{
      
      MR_call_sep <- MR_call_undrop
      MR_call_ttl <- MR_call
      
    }
    
    cpd_call <-  MR_call_sep %>% 
      filter(Measure.Names %in% c("Call Rate(拜访率）", "FCR Day(协访天数)")) %>% 
      spread(Measure.Names, Measure.Values) %>% 
      rename("Call per Day" = `Call Rate(拜访率）`,
             "FCR Day_DSM" = `FCR Day(协访天数)`) %>% 
      mutate(Franchise = ifelse(N2 == "D&A", "Derm/Anti-allergy",
                                ifelse(N2 == "Neuro", "Neurology",
                                       ifelse(N2 == "PC", "Primary Care",
                                              ifelse(N2 == "PSY", "Psychiatry", N2))))) %>% 
      select(-N2)
    
    cpd_callA <- MR_call_ttl %>%
      filter(substr(`HCP.Level`, 1, 1) %in% c("A") & `Year/Month` == YTD_time_max) %>%
      filter(!Brand %in% c("Risperdal", "Consta")) %>% 
      group_by(Franchise) %>%
      summarise(`Call Frequency_A` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
      ungroup()
    
    cpd_callB <- MR_call_ttl %>%
      filter(substr(`HCP.Level`, 1, 1) %in% c("B") & `Year/Month` == YTD_time_max) %>%
      filter(!Brand %in% c("Risperdal", "Consta")) %>% 
      group_by(Franchise) %>%
      summarise(`Call Frequency_B` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
      ungroup()
    
    cpd_callAB <- MR_call_ttl %>%
      filter(substr(`HCP.Level`, 1, 1) %in% c("A", "B") & `Year/Month` == YTD_time_max) %>%
      filter(!Brand %in% c("Risperdal", "Consta")) %>% 
      group_by(Franchise) %>%
      summarise(`Call Frequency_AB` = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
      ungroup()
    
    cpd_KPI_perf <- cpd_call %>% 
      left_join(cpd_callA, by = "Franchise") %>% 
      left_join(cpd_callB, by = "Franchise") %>% 
      left_join(cpd_callAB, by = "Franchise") %>% 
      select(BU = Franchise, everything()) %>% 
      mutate(Division = factor("CPD"),
             BU = factor(BU, levels = display_name))
    
    rownum <- with(cpd_KPI_perf, RowNum(list(cpd_KPI_perf$Division, cpd_KPI_perf$BU)))
    
    cpd_KPI_perf_m <- tabular(`Division` * `BU` * I(rownum) ~ (`Call per Day` + `Call Frequency_A` + `Call Frequency_B` + `Call Frequency_AB` + `FCR Day_DSM`) * AllObs(cpd_KPI_perf, within = list(`Division`, `BU`, rownum)), cpd_KPI_perf)
    
    write.table.tabular(cpd_KPI_perf_m, paste0(save_location, "/", final_page, ".csv"), sep = ",")
    
    tmp <- fread(paste0(save_location, "/", final_page, ".csv"))
    
    names(tmp)[1] <- " "
    write.xlsx(tmp, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
  }
}
