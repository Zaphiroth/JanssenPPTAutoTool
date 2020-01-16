# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janseen dashboard
# Purpose:      Mainbody, CPD_RPD and CPD_Sellout_by_channel_by_model
#               Tables and Charts Generator
# programmer:   Jessica Liu
# modifier:     Xin Huang
# Date:         01-07-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


CPD_RPD_generator <- function(save_location,
                              mapping_tbl,
                              inter_market,
                              scope,
                              retail_direct,
                              extcrstree,
                              retail_all,
                              PCMC_mapping){
  
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
  
  # final_page <- " "
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
  
  
  scope_m <- scope %>% 
    left_join(retail_direct %>% 
                select(ORGID, ORGNAME), by = c("Terminal Code" = "ORGID")) %>% 
    left_join(extcrstree %>% 
                select(DRUGSTORE_ID4, KA), by = c("Terminal Code" = "DRUGSTORE_ID4")) %>% 
    left_join(retail_all %>% 
                select(ORGID, ISATA), by = c("Terminal Code" = "ORGID")) %>% 
    mutate(Model = ifelse(!is.na(ORGNAME), "DC", "NDC"),
           `KA/NKA` = ifelse(!is.na(KA), KA, "IDS"),
           `N/Y` = ifelse(ISATA == "Y", "OLD", "NEW"),
           Type = ifelse(`KA/NKA` %in% c("KA", "NKA"), paste0(`KA/NKA`, "_", Model), "IDS"),
           sales = `本年销售金额（Sales_AMT)`)
  
  ## MTD的数据单独给表
  level <- c("NEW", "OLD", "Motrin", "Gold_Daktarin", "Daktarin", "Motilium",
             "Band Aid", "XSM", "Rhinocort", "others")
  
  # cpd_f55_1_YTD <- scope_m %>% 
  #   summarise(`2018Act` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019Act` = sum(sales, na.rm = T),
  #             `2019Target` = sum(`本年指标金额（Target AMT)`, na.rm = T))
  
  cpd_f55_2_1 <- scope_m %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Motrin", "Gold_Daktarin", "Daktarin",
                                                   "Motilium", "Band Aid", "XSM", "Rhinocort"), `品牌（Brand)`, "others")) %>% 
    group_by(`品牌（Brand)`) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1) %>% 
    ungroup()
  
  # cpd_f55_2_2 <- scope_m %>% 
  #   mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Motrin", "Gold_Daktarin", "Daktarin",
  #                                                  "Motilium", "Band Aid", "XSM", "Rhinocort"), `品牌（Brand)`, "others")) %>% 
  #   group_by(`品牌（Brand)`) %>% 
  #   summarise(`2018 YTD` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019 YTD` = sum(sales, na.rm = T))
  
  cpd_f55_2_3 <- scope_m %>% 
    mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Motrin", "Gold_Daktarin", "Daktarin",
                                                   "Motilium", "Band Aid", "XSM", "Rhinocort"), `品牌（Brand)`, "others")) %>% 
    group_by(`N/Y`) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1) %>% 
    ungroup() %>% 
    full_join(cpd_f55_2_1, by = c("N/Y" = "品牌（Brand)", "Ach%", "Gr%")) %>% 
    mutate(`N/Y` = factor(`N/Y`, levels = level)) %>% 
    arrange(`N/Y`) %>% 
    rename("RPD Total" = `N/Y`)
  
  cpd_f55_2_ttl <- scope_m %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1)
  
  cpd_f55_3_1 <- scope_m %>% 
    group_by(`KA/NKA`) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1) %>% 
    ungroup()
  
  # cpd_f55_3_2_ttl <- scope_m %>% 
  #   summarise(`2018 YTD` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019 TYD` = sum(sales, na.rm = T))
  
  # cpd_f55_3_2 <- scope_m %>% 
  #   group_by(`KA/NKA`) %>% 
  #   summarise(`2018 YTD` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019 TYD` = sum(sales, na.rm = T),
  #             `Contr. 2018` = `2018 YTD`/cpd_f55_3_2_ttl$`2018 YTD`,
  #             `Contr. 2019` = `2019 TYD`/cpd_f55_3_2_ttl$`2019 TYD`)
  
  cpd_f55_4_1 <- scope_m %>% 
    filter(`KA/NKA` %in% c("NKA", "KA")) %>% 
    group_by(Model) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1) %>% 
    ungroup()
  
  cpd_f55_4_ttl <- scope_m %>% 
    filter(`KA/NKA` %in% c("NKA", "KA")) %>% 
    summarise(`Ach%` = sum(sales, na.rm = T)/sum(`本年指标金额（Target AMT)`, na.rm = T),
              `Gr%` = sum(sales, na.rm = T)/sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T) - 1) %>% 
    mutate(Model = "NKA&KA Total") %>% 
    full_join(cpd_f55_4_1) %>% 
    mutate(Model = ifelse(Model == "NDC", "HQ cover", 
                          ifelse(Model == "DC", "Direct Cover", Model)),
           Model = factor(Model, levels = c("NKA&KA Total", "Direct Cover", "HQ cover"))) %>% 
    arrange(Model) %>% 
    select(Model, everything())
  
  # cpd_f55_4_2_ttl <- scope_m %>% 
  #   filter(`KA/NKA` %in% c("NKA", "KA")) %>% 
  #   summarise(`2018 YTD` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019 TYD` = sum(sales, na.rm = T))
  
  # cpd_f55_4_2 <- scope_m %>% 
  #   filter(`KA/NKA` %in% c("NKA", "KA")) %>% 
  #   group_by(Model) %>% 
  #   summarise(`2018 YTD` = sum(`去年销售金额（Last Year Sales AMT)`, na.rm = T),
  #             `2019 TYD` = sum(sales, na.rm = T),
  #             `Contr. 2018` = `2018 YTD`/cpd_f55_4_2_ttl$`2018 YTD`,
  #             `Contr. 2019` = `2019 TYD`/cpd_f55_4_2_ttl$`2019 TYD`) %>% 
  #   mutate(Model = ifelse(Model == "NDC", "HQ cover", "Direct Cover"),
  #          Model = factor(Model, levels = c("Direct Cover", "HQ cover"))) %>% 
  #   arrange(Model) %>% 
  #   select(Model, everything())
  
  #-- P54
  BU_target <- c("Psychiatry", "Derm/Anti-allergy", "Neurology", "Primary Care")
  
  cpd_exclu_lex_top_total <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Model %in% c("RPD") & Franchise %in% BU_target) %>% 
    summarise(Sales = round(sum(sales, na.rm = T)))
  
  cpd_top <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Model %in% c("RPD") & Franchise %in% BU_target) %>% 
    group_by(Province) %>% 
    summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
              `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1,
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
    ungroup() %>% 
    arrange(desc(`Contr.%`)) %>% 
    filter(row_number()<=15)
  
  cpd_top_other <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Model %in% c("RPD") & Franchise %in% BU_target & !(Province %in% c(cpd_top$Province))) %>% 
    summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
              `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1,
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
    mutate(Province = "Others") %>% 
    select(Province, everything())
  
  TOP_15_total <- inter_market_m %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro")))  & Model %in% c("RPD") & Franchise %in% BU_target) %>% 
    filter(Province %in% c(cpd_top$Province)) %>% 
    summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
              `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1,
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
    mutate(Province = "TOP 15 Total") %>% 
    select(Province, everything())
  
  cpd_exclu_lex_top <- inter_market %>% 
    filter(`Year/Month` <= YTD_time_max & `Year/Month` >= YTD_time_min) %>% 
    filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Model %in% c("RPD") & Franchise %in% BU_target) %>% 
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
    summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
              `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
              `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1,
              `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
    mutate(Province = "Total Sellout") %>% 
    select(Province, everything())
  
  p54 <- cpd_exclu_lex_top %>% 
    full_join(TOP_15_total) %>% 
    full_join(cpd_top) %>% 
    full_join(cpd_top_other) 
  
  write.xlsx(p54, paste0(save_location, "/", "cpd_f54", ".xlsx"), sep = ",")
  
}
