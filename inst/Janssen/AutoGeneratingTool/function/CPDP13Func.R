# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janseen dashboard
# Purpose:      Mainbody, CPD_RPD and CPD_Sellout_by_channel_by_model
#               Tables and Charts Generator
# programmer:   Jessica Liu
# modifier:     Xin Huang
# Date:         01-07-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


CPD_Sellout_by_channel_by_model_generator <- function(save_location,
                                                      mapping_tbl,
                                                      inter_market,
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
  
  # final_page <- "cpd_f31"
  
  # display_name <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Display.name) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # BU_target <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(BU_target) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # Type <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Type) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # Table.type <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Table.type) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # brand <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Brand) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # product <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Product) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # model <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Model) %>% 
  #   distinct() %>% 
  #   unlist()
  # 
  # province <- mapping_tbl %>% 
  #   filter(Page == final_page) %>% 
  #   select(Province) %>% 
  #   distinct() %>% 
  #   unlist()
  
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
  
  ##---------------------------- CPD_f13 ------------------------------------------
  
  strategic_product <- c("Concerta", "Invega", "Invega Trinza", "Sustenna")
  
  cpd_f13_tb1_m1 <- inter_market_m %>% 
    filter(Franchise %in% c("Psychiatry") & `品牌（Brand)` %in% strategic_product) %>% 
    group_by(Model) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>%
    ungroup() %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    mutate(Cat = "PSY strategic",
           rank = 1:2) %>% 
    select(-total_growth)
  
  cpd_f13_tb1_m2 <- inter_market_m %>% 
    filter(Franchise %in% c("Psychiatry") & `品牌（Brand)` %in% strategic_product) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>%
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    mutate(Model = "Sell Out",
           Cat = "PSY strategic ttl")
  
  strategic_product_m <- c("Consta", "Risperdal")
  
  cpd_f13_tb1_m3 <- inter_market_m %>% 
    filter(Franchise %in% c("Psychiatry") & `品牌（Brand)` %in% strategic_product_m) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth),
           Model = "Sell Out",
           Cat = "Consta&Risperdal")
  
  cpd_f13_tb1_m4 <- inter_market %>% 
    filter(Franchise %in% c("Psychiatry")) %>% 
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
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1) %>% 
    mutate(Model = "Sell Out",
           Cat = "PSY Overall")
  
  cpd_f13_tb1 <- cpd_f13_tb1_m1 %>% 
    full_join(cpd_f13_tb1_m2) %>% 
    full_join(cpd_f13_tb1_m3) %>% 
    full_join(cpd_f13_tb1_m4) %>% 
    mutate(Model = factor(Model),
           Cat = factor(Cat))
  
  rownum <- with(cpd_f13_tb1, RowNum(list(cpd_f13_tb1$Cat, cpd_f13_tb1$Model)))
  
  cpd_f13_tb1_2 <- tabular(`Cat`* `Model`*I(rownum) ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + Target + `Ach%` + `Gr%` + `Growth Contribution%`) * AllObs(cpd_f13_tb1, within = list(Cat, `Model`, rownum)), cpd_f13_tb1)
  
  write.table.tabular(cpd_f13_tb1_2, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  
  
  tmp$Model[tmp$Model == "MR\\_Hospital"] <- "MR_Hospital"
  tmp$Cat[tmp$Cat == "Consta\\&Risperdal"] <- "Consta&Risperdal"
  
  tmp <- tmp %>% 
    mutate(Model = factor(Model, levels = c("MR_Hospital", "NDC bottom up", "Sell Out"))) %>% 
    arrange(Model)
  names(tmp)[which(names(tmp) == "Cat")] <- ""
  names(tmp)[which(names(tmp) == "Model")] <- ""
  
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f13_1.xlsx"))
  
  cpd_f13_chart <- inter_market_m %>% 
    filter(Franchise %in% c("Psychiatry") & `品牌（Brand)` %in% strategic_product) %>% 
    group_by(Model) %>% 
    summarise(`Last Year YTD` = sum(pre_sales, na.rm = T),
              `Current Year YTD` = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(`Last Year cont.` = `Last Year YTD`/sum(`Last Year YTD`, na.rm = T),
           `Current Year cont.` = `Current Year YTD`/sum(`Current Year YTD`, na.rm = T))
  
  write.xlsx(cpd_f13_chart, paste0(save_location, "/cpd_f13_2.xlsx"))
  
  ##---------------------------- CPD_f22 ------------------------------------------
  
  cpd_f22_tb1_m1 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Neurology") & !(`品牌（Brand)` %in% c("Lexapro"))) %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    ungroup() %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    mutate(Cat = "Neurology EXCL. Lexapro",
           rank = 1:3)
  
  cpd_f22_tb1_m2 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Neurology") & !(`品牌（Brand)` %in% c("Lexapro"))) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    mutate(Model = "TTL",
           Cat = "Neurology EXCL. Lexapro")
  
  cpd_f22_tb1_m3 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Neurology") & `品牌（Brand)` %in% c("Lexapro")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1) %>% 
    mutate(Model = "Sell Out",
           Cat = "Lexapro")
  
  cpd_f22_tb1_m4 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Neurology")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              `Growth Contribution%` = 1) %>% 
    mutate(Model = "Sell Out",
           Cat = "Neuro overall(Incl. Lex)")
  
  cpd_f22_tb1_DC <- inter_market_m1 %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    filter(Franchise %in% c("Neurology") & Model %in% c("MR_Hospital", "PCMC")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              `Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/cpd_f13_tb1_m2$total_growth) %>% 
    mutate(Model = "DC",
           Cat = "Neurology EXCL. Lexapro")
  
  cpd_f22_tb1 <- cpd_f22_tb1_DC %>% 
    full_join(cpd_f22_tb1_m1) %>% 
    full_join(cpd_f22_tb1_m2) %>% 
    full_join(cpd_f22_tb1_m3) %>% 
    full_join(cpd_f22_tb1_m4) %>% 
    mutate(Model = factor(Model),
           Cat = factor(Cat))
  
  rownum <- with(cpd_f22_tb1, RowNum(list(cpd_f22_tb1$Cat, cpd_f22_tb1$Model)))
  
  cpd_f22_tb1_2 <- tabular(`Cat`* `Model`*I(rownum) ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + Target + `Ach%` + `Gr%` + `Growth Contribution%`) * AllObs(cpd_f22_tb1, within = list(Cat, `Model`, rownum)), cpd_f22_tb1)
  
  write.table.tabular(cpd_f22_tb1_2, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  
  tmp$Model[tmp$Model == "MR\\_Hospital"] <- "MR_Hospital"
  
  tmp <- tmp %>% 
    mutate(Model = factor(Model, levels = c("DC", "MR", "PCMC", "NDC bottom up", "TTL", "Sell Out"))) %>% 
    arrange(Model)
  
  names(tmp)[which(names(tmp) == "Cat")] <- ""
  names(tmp)[which(names(tmp) == "Model")] <- ""
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f22_1.xlsx"))
  
  cpd_f22_chart <- inter_market_m1 %>% 
    filter(Franchise %in% c("Neurology") & `品牌（Brand)` != "Lexapro") %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Last Year YTD` = sum(pre_sales, na.rm = T),
              `Current Year YTD` = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(`Last Year cont.` = `Last Year YTD`/sum(`Last Year YTD`, na.rm = T),
           `Current Year cont.` = `Current Year YTD`/sum(`Current Year YTD`, na.rm = T))
  
  write.xlsx(cpd_f22_chart, paste0(save_location, "/cpd_f22_2.xlsx"))
  
  
  ##---------------------------- CPD_f31 ------------------------------------------
  
  cpd_f31_tb1_m1 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Derm/Anti-allergy")) %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    ungroup() %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    select(-total_growth)
  
  cpd_f31_tb1_m2 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Derm/Anti-allergy")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth),
           Model = "D&A Sell Out") %>% 
    select(-total_growth)
  
  cpd_f31_tb1_DC <- inter_market_m1 %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    filter(Franchise %in% c("Derm/Anti-allergy")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales[Model != "NDC bottom up"], na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales[Model != "NDC bottom up"], na.rm = T)/1000000,
              Target = sum(target[Model != "NDC bottom up"], na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth),
           Model = "DC") %>% 
    select(-total_growth)
  
  cpd_f31_tb1 <- cpd_f31_tb1_DC %>% 
    full_join(cpd_f31_tb1_m1) %>% 
    full_join(cpd_f31_tb1_m2) %>% 
    mutate(Cat = "Derm/Anti-allergy") %>% 
    mutate(Model = factor(Model, levels = c("DC", "MR", "PCMC", "RPD", "EC", "NDC bottom up", "D&A Sell Out")),
           Cat = factor(Cat))
  
  rownum <- with(cpd_f31_tb1, RowNum(list(cpd_f31_tb1$Cat, cpd_f31_tb1$Model)))
  
  cpd_f31_tb1_2 <- tabular(`Cat`* `Model`*I(rownum) ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + Target + `Ach%` + `Gr%` + `Growth Contribution%`) * AllObs(cpd_f31_tb1, within = list(Cat, `Model`, rownum)),
                           cpd_f31_tb1)
  
  write.table.tabular(cpd_f31_tb1_2, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  
  tmp$Model[tmp$Model == "D\\&A Sell Out"] <- "D&A Sell Out"
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f31_1.xlsx"))
  
  cpd_f31_chart <- inter_market_m1 %>% 
    filter(Franchise %in% c("Derm/Anti-allergy") & `品牌（Brand)` != "Lexapro") %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Current Year YTD` = sum(sales, na.rm = T),
              `Last Year YTD` = sum(pre_sales, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(`Last Year cont.` = `Last Year YTD`/sum(`Last Year YTD`, na.rm = T),
           `Current Year cont.` = `Current Year YTD`/sum(`Current Year YTD`, na.rm = T))
  
  write.xlsx(cpd_f31_chart, paste0(save_location, "/cpd_f31_2.xlsx"))
  
  
  ##---------------------------- CPD_f40 ------------------------------------------
  
  cpd_f40_tb1_m1 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Primary Care")) %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    ungroup() %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth)) %>% 
    select(-total_growth) %>% 
    mutate(Cat = "Primary Care")
  
  
  cpd_f40_tb1_m2 <- inter_market_m1 %>% 
    filter(Franchise %in% c("Primary Care")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1) %>% 
    mutate(Model = "PC Sell Out",
           Cat = "tmp")
  
  cpd_f40_tb1_DC <- inter_market_m1 %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    filter(Franchise %in% c("Primary Care")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales[Model != "NDC bottom up"], na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales[Model != "NDC bottom up"], na.rm = T)/1000000,
              Target = sum(target[Model != "NDC bottom up"], na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1,
              total_growth = (sum(sales, na.rm = T) - sum(pre_sales, na.rm = T))/1000000) %>% 
    mutate(`Growth Contribution%` = (`Sales ACT Current Year` - `Sales ACT Last Year`)/sum(total_growth),
           Model = "DC",
           Cat = "Primary Care") %>% 
    select(-total_growth)
  
  cpd_f40_tb1_medicalDC <- inter_market_m1 %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    filter(Franchise %in% c("Primary Care") & Model_m1 %in% c("PCMC", "MR")) %>% 
    summarise(`Sales ACT Last Year` = sum(pre_sales, na.rm = T)/1000000,
              `Sales ACT Current Year` = sum(sales, na.rm = T)/1000000,
              Target = sum(target, na.rm = T)/1000000,
              `Ach%` = `Sales ACT Current Year`/Target,
              `Gr%` = `Sales ACT Current Year`/`Sales ACT Last Year` - 1) %>% 
    mutate(Model = "tmp",
           Cat = "Medical DC*(All brands)")
  
  cpd_f40_tb1 <- cpd_f40_tb1_DC %>% 
    full_join(cpd_f40_tb1_m1) %>% 
    full_join(cpd_f40_tb1_m2) %>% 
    full_join(cpd_f40_tb1_medicalDC) %>% 
    mutate(Model = factor(Model, levels = c("PC Sell Out", "DC", "MR", "PCMC", "RPD", "EC", "NDC bottom up", "tmp")),
           Cat = factor(Cat)) %>% 
    arrange(Model)
  
  rownum <- with(cpd_f40_tb1, RowNum(list(cpd_f40_tb1$Cat, cpd_f40_tb1$Model)))
  
  cpd_f40_tb1_2 <- tabular(`Cat`* `Model`*I(rownum) ~ (`Sales ACT Last Year` + `Sales ACT Current Year` + Target + `Ach%` + `Gr%` + `Growth Contribution%`) * AllObs(cpd_f40_tb1, within = list(Cat, `Model`, rownum)),
                           cpd_f40_tb1)
  
  write.table.tabular(cpd_f40_tb1_2, paste0(save_location, "/tmp.csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/tmp.csv"))
  
  tmp$Model[tmp$Model == "tmp"] <- ""
  tmp$Cat[tmp$Cat == "tmp"] <- ""
  
  tmp <- tmp %>% 
    mutate(Model = factor(Model, levels = c("PC Sell Out", "DC", "MR", "PCMC", 
                                            "RPD", "EC", "NDC bottom up", " "))) %>% 
    arrange(Model)
  
  colnames(tmp)[colnames(tmp) == "Cat"] <- ""
  colnames(tmp)[colnames(tmp) == "Model"] <- ""
  
  write.xlsx(tmp, paste0(save_location, "/cpd_f40_1.xlsx"))
  
  cpd_f40_chart <- inter_market_m1 %>% 
    filter(Franchise %in% "Primary Care" & `品牌（Brand)` != "Lexapro") %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
    group_by(Model = Model_m1) %>% 
    summarise(`Last Year YTD` = sum(pre_sales, na.rm = T),
              `Current Year YTD` = sum(sales, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(`Last Year cont.` = `Last Year YTD`/sum(`Last Year YTD`, na.rm = T),
           `Current Year cont.` = `Current Year YTD`/sum(`Current Year YTD`, na.rm = T)) %>% 
    mutate(Model = factor(Model, levels = c("EC", "MR", "NDC bottom up", "PCMC", "RPD"))) %>% 
    arrange(Model)
  
  write.xlsx(cpd_f40_chart, paste0(save_location, "/cpd_f40_2.xlsx"))
  
}
