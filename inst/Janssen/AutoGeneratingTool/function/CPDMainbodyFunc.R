# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janseen dashboard
# Purpose:      Mainbody, CPD_RPD and CPD_Sellout_by_channel_by_model
#               Tables and Charts Generator
# programmer:   Jessica Liu
# modifier:     Xin Huang
# Date:         01-07-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


CPD_mainbody_generator <- function(final_page,
                                   save_location,
                                   mapping_tbl,
                                   inter_market,
                                   exter_hos,
                                   PCMC_mapping,
                                   brand_name_map,
                                   MR_call,
                                   MR_event,
                                   jinyao_pre,
                                   jinyao_current,
                                   dist,
                                   inventory,
                                   retail_mkt_daktarin,
                                   cpd_f3_time){
  
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

  # final_page <- "cpd_f49_2_2"
  print(final_page)
  
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
  
  Type <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Type) %>% 
    distinct() %>% 
    unlist()
  
  Table.type <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Table.type) %>% 
    distinct() %>% 
    unlist()
  
  brand <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Brand) %>% 
    distinct() %>% 
    unlist()
  
  product <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Product) %>% 
    distinct() %>% 
    unlist()
  
  model <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Model) %>% 
    distinct() %>% 
    unlist()
  
  province <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Province) %>% 
    distinct() %>% 
    unlist()
  
  IMS_product <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(IMS.Product) %>% 
    distinct() %>% 
    unlist()
  
  Defined_market <- mapping_tbl %>% 
    filter(Page == final_page) %>% 
    select(Defined.market) %>% 
    distinct() %>% 
    unlist()
  
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
           PY_Ach = `PY.Ach.(Val)`) %>% 
    left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>% 
    mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                              Teamname %in% c("PC_MC"), "PCMC", "MR"),
           Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
           Model = Model_m1)
  
  if(Table.type == "Chart"){
    
    if(Type == "IMS CHPA MS"){
      
      if(Defined_market == "Concerta"){
        
        ims_ms_his <- exter_hos %>% 
          filter(Defined.Market %in% 
                   Defined_market & Product %in% IMS_product) %>% 
          filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & 
                   as.numeric(Month) <= (YTD_month_max - 1)) %>% 
          filter(KPI %in% c("Volume ('000)")) %>% 
          group_by(Product, Year) %>% 
          summarise(YTDSellout = sum(as.numeric(Value), na.rm = T)) %>% 
          ungroup() %>% 
          group_by(Year) %>% 
          mutate(`YTD MS` = YTDSellout/sum(YTDSellout, na.rm = T)) %>% 
          ungroup()
        
      }else{
        
        ims_ms_his <- exter_hos %>% 
          filter(Defined.Market %in% Defined_market) %>% 
          filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & 
                   as.numeric(Month) <= (YTD_month_max - 1)) %>% 
          filter(KPI %in% c("Volume ('000)")) %>% 
          mutate(Product = ifelse(Product %in% IMS_product, Product, "Others")) %>% 
          group_by(Product, Year) %>% 
          summarise(YTDSellout = sum(as.numeric(Value), na.rm = T)) %>% 
          ungroup() %>% 
          group_by(Year) %>% 
          mutate(`YTD MS` = YTDSellout/sum(YTDSellout, na.rm = T)) %>% 
          ungroup()
        
      }
      
      ims_ms_his_m <- as.data.table(ims_ms_his) %>% 
        dcast(Product ~ Year, value.var = c("YTDSellout", "YTD MS")) %>% 
        mutate(Product = factor(Product, levels = IMS_product)) %>% 
        arrange(Product) %>% 
        filter(!is.na(Product))
      
      ims_ms_his_m[] <- lapply(ims_ms_his_m,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(ims_ms_his_m, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Province_by_model"){
      
      if(BU_target != "Psychiatry"){
        
        inter_market_m <- inter_market_m %>% 
          left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8")
        
      }
      
      if(is.na(brand)){
        
        cpd_his_map_m1 <- inter_market_m %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          group_by(Province) %>% 
          summarise(`CPD excl. Lex` = sum(sales, na.rm = T)/1000000,
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1),
                    `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
          ungroup() %>% 
          arrange(desc(`CPD excl. Lex`)) %>% 
          filter(row_number() <= 15)
        
      }else{
        
        cpd_his_map_m1 <- inter_market_m %>% 
          mutate(`品牌（Brand)` = 
                   ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"),
                          "Daktarin Family", `品牌（Brand)`)) %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                   Franchise %in% BU_target & `品牌（Brand)` %in% brand) %>% 
          group_by(Province) %>% 
          summarise(`CPD excl. Lex` = sum(sales, na.rm = T)/1000000,
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
          ungroup() %>% 
          arrange(desc(`CPD excl. Lex`)) %>% 
          filter(row_number() <= 15)
        
      }
      
      cpd_his_map_m2 <- t(cpd_his_map_m1)
      
      colnames(cpd_his_map_m2) <- cpd_his_map_m2[1, ]
      
      cpd_his_map_m2 <- as.data.frame(cpd_his_map_m2[-1, ]) %>% 
        mutate(Province = c("CPD excl. Lex", "Ach%(MLP)", "Gr%(MLP)", "Gr%(VOL)")) %>% 
        select(Province, everything())
      
      cpd_his_map_m2[] <- lapply(cpd_his_map_m2,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_his_map_m2, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Sellout_by_month"){
      
      if(is.na(product)){
        
        sellout_by_month <- inter_market %>%
          left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>% 
          mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                    Teamname %in% c("PC_MC"), "PCMC", "MR"),
                 Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
                 `品牌（Brand)` = ifelse(`品牌（Brand)` %in% 
                                        c("Invega Trinza", "Sustenna"), 
                                      "LAT", `品牌（Brand)`),
                 sales = `本年销售金额（Sales_AMT)`,
                 date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
          arrange(date) %>% 
          filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
          filter(Franchise %in% BU_target & `品牌（Brand)` %in% brand) %>% 
          group_by(`Year/Month`, Model = Model_m1) %>% 
          summarise(sum_by_month = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(`Year/Month`, sum_by_month)
        
        
        sellout_by_month[] <- lapply(sellout_by_month,function(x) {
          x[is.na(x)|
              grepl("NA", x, perl = FALSE) |
              grepl("Inf", x, perl = FALSE) |
              grepl("NaN", x, perl = FALSE) ] <- ""
          x
        })
        
        write.xlsx(sellout_by_month, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
        
        
      }else{
        
        ##-- WARNING!!!!!
        if(brand == "Rhinocort"){
          
          sellout_by_month <- inter_market %>%
            # select(-Teamname) %>%
            left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>% 
            mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                      Teamname %in% c("PC_MC"), "PCMC", "MR"),
                   Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
                   `品牌（Brand)` = ifelse(`品牌（Brand)` %in% 
                                          c("Invega Trinza", "Sustenna"), "LAT",`品牌（Brand)`),
                   sales = `本年销售金额（Sales_AMT)`,
                   date = ymd(paste(gsub("M", "", `Year/Month`), "01", sep = ""))) %>% 
            arrange(date) %>% 
            filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
            filter(Franchise %in% BU_target, `品牌（Brand)` %in% brand, `产品名称（Product)` %in% product, Model == "MR_Hospital") %>% 
            group_by(`Year/Month`, `产品名称（Product)`) %>% 
            summarise(sum_by_month = sum(sales, na.rm = T)) %>% 
            ungroup() %>% 
            spread(`Year/Month`, sum_by_month)
          
        }else{
          
          sellout_by_month <- inter_market %>%
            left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>%
            mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
                   Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
                   `品牌（Brand)_m` = ifelse(`品牌（Brand)` %in% c("Invega Trinza", "Sustenna"), "LAT",
                                          ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", 
                                                 `品牌（Brand)`)),
                   sales = `本年销售金额（Sales_AMT)`,
                   date = ymd(paste(gsub("M", "", `Year/Month`), "01", sep = ""))) %>%
            arrange(date) %>%
            filter(date <= last(date) & date >= last(date) - months(x = 11)) %>%
            filter(Franchise %in% BU_target & `品牌（Brand)_m` %in% brand) %>%
            group_by(`Year/Month`) %>%
            summarise(sum_by_month = sum(sales, na.rm = T)) %>%
            ungroup() %>% 
            spread(`Year/Month`, sum_by_month) %>%
            mutate(`产品名称（Product)` ="Sellout")
          
        }
        
        sellout_inventory_by_month <- inventory %>%
          mutate(`Brand En` = ifelse(`Brand En` %in% c("Daktarin", "Gold Daktarin"), "Daktarin Family", `Brand En`)) %>%
          filter(Franchise %in% BU_target & `Brand En` %in% brand) %>%
          filter(`Year/Month` <= last(`Year/Month`) & `Year/Month` >= last(`Year/Month`) - months(x = 11)) %>%
          filter(Tier %in% c("1", "2")) %>%
          group_by(`Year/Month`, Tier) %>%
          summarise(inventory = sum(`Inventory Days`, na.rm = T)) %>%
          ungroup() %>%
          rename("产品名称（Product)" = Tier) %>%
          mutate(`Year/Month` = ifelse(month(`Year/Month`) %in% 1:9, paste0(year(`Year/Month`), "0", month(`Year/Month`)),
                                       paste0(year(`Year/Month`), month(`Year/Month`)))) %>%
          spread(`Year/Month`, inventory) %>%
          mutate(`产品名称（Product)` = ifelse(`产品名称（Product)` == "1", "T1 Inventory", "T2 Inventory")) %>%
          full_join(sellout_by_month)
        
        sellout_inventory_by_month[] <- lapply(sellout_inventory_by_month,function(x) {
          x[is.na(x)|
              grepl("NA", x, perl = FALSE) |
              grepl("Inf", x, perl = FALSE) |
              grepl("NaN", x, perl = FALSE) ] <- ""
          x
        })
        
        write.xlsx(sellout_inventory_by_month, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      }
      
      
      
      # sellout_by_month[] <- lapply(sellout_by_month,function(x) {
      #   x[is.na(x)|
      #       grepl("NA", x, perl = FALSE) |
      #       grepl("Inf", x, perl = FALSE) |
      #       grepl("NaN", x, perl = FALSE) ] <- ""
      #   x
      # })
      # 
      # sellout_inventory_by_month[] <- lapply(sellout_inventory_by_month,function(x) {
      #   x[is.na(x)|
      #       grepl("NA", x, perl = FALSE) |
      #       grepl("Inf", x, perl = FALSE) |
      #       grepl("NaN", x, perl = FALSE) ] <- ""
      #   x
      # })
      # 
      # write.xlsx(sellout_by_month, paste0("03_Outputs/CPD/", final_page, ".xlsx"), sep = ",")
      # write.xlsx(sellout_inventory_by_month, paste0("03_Outputs/CPD/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "KPI Trend"){
      
      if(brand == "LAT"){
        
        MR_call <- MR_call %>% 
          mutate(Brand = 
                   ifelse(Brand %in% c("Invega Trinza", "Sustenna"), "LAT", Brand))
        
      }else{
        
        MR_call <- MR_call
        
      }
      
      Frequency <- MR_call %>% 
        filter(!(City %in% c("上海市", "天津市")) & (E8 != "TBA")) %>% 
        mutate(`HCP.Level` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                    ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B", `HCP.Level`)),
               date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
        arrange(date) %>% 
        filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
        filter(Franchise %in% BU_target & Brand %in% brand & `HCP.Level` %in% c("A", "B")) %>%
        group_by(`Year/Month`, `HCP.Level`) %>% 
        summarise(Frequency = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
        ungroup() %>% 
        spread(HCP.Level, Frequency)
      
      Frequency_A <- Frequency %>% 
        select(`Year/Month`, A) %>% 
        rename("Frequency A" = A)
      
      Frequency_B <- Frequency %>% 
        select(`Year/Month`, B) %>% 
        rename("Frequency B" = B)
      
      AB_coverage <- MR_call %>% 
        filter(!(City %in% c("上海市", "天津市")) & (E8 != "TBA")) %>% 
        filter(Franchise %in% BU_target & Brand %in% brand & 
                 substr(`HCP.Level`, 1, 1) %in% c("A", "B")) %>%
        mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
        arrange(date) %>% 
        filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
        group_by(`Year/Month`) %>% 
        summarise(AB_coverage = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
        ungroup()
      
      brand_name_map_m <- brand_name_map %>% 
        mutate(`品牌名称` = ifelse(grepl("同", `品牌名称`), brandname, `品牌名称`),
               brandname = ifelse(brandname == "LAT(invega trinza+sustenna)",
                                  "LAT", brandname)) %>% 
        select(-X3)
      
      MR_event_m <- MR_event %>% 
        left_join(brand_name_map_m, by = c("Brand" = "品牌名称")) %>% 
        mutate(brandname = ifelse(Brand %in% c("善妥达", "善思达"), "LAT", brandname))
      
      Meeting <- MR_event_m %>% 
        filter(Franchise %in% BU_target & tolower(brandname) %in% tolower(brand)) %>% 
        mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
        arrange(date) %>% 
        filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
        group_by(`Year/Month`) %>% 
        count() %>% 
        ungroup() %>% 
        rename("Meeting" = n)
      
      write.xlsx(Frequency_A, paste0(save_location, "/", final_page, "_3.xlsx"), sep = ",")
      write.xlsx(Frequency_B, paste0(save_location, "/", final_page, "_4.xlsx"), sep = ",")
      write.xlsx(AB_coverage, paste0(save_location, "/", final_page, "_2.xlsx"), sep = ",")
      write.xlsx(Meeting, paste0(save_location, "/", final_page, "_1.xlsx"), sep = ",")
      
      if(length(display_name) == 5){
        
        ms_by_month <- exter_hos %>% 
          mutate(date = ymd(paste(Year, Month, "01"))) %>% 
          filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
          filter(Defined.Market %in% Defined_market) %>% 
          filter(KPI %in% c("Volume ('000)")) %>% 
          mutate(flag = ifelse(Product %in% IMS_product, "1", "0")) %>% 
          group_by(Year, Month, flag) %>% 
          summarise(Value = sum(as.numeric(Value), na.rm = T)) %>% 
          ungroup() %>% 
          mutate(MS = Value/sum(Value, na.rm = T)) %>% 
          filter(flag == "1") %>% 
          select(-flag)
        
        ms_by_month[] <- lapply(ms_by_month,function(x) {
          x[is.na(x)|
              grepl("NA", x, perl = FALSE) |
              grepl("Inf", x, perl = FALSE) |
              grepl("NaN", x, perl = FALSE) ] <- ""
          x
        })
        
        write.xlsx(ms_by_month, paste0(save_location, "/", final_page, "_5.xlsx"), sep = ",") 
      }
      
    }else if(Type == "Sellout_by_model"){
      
      ##-- WARNING!!!!! mapping issue
      cpd_sellout_by_model_his <- inter_market_m %>% 
        mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & `品牌（Brand)` %in% brand) %>% 
        group_by(Model) %>% 
        summarise(sellout_p = sum(pre_sales, na.rm = T),
                  sellout_c = sum(sales, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(contribion_p = sellout_p/sum(sellout_p, na.rm = T),
               contribution_c = sellout_c/sum(sellout_c, na.rm = T),
               Model = factor(Model, levels = display_name))
      
      cpd_sellout_by_model_his[] <- lapply(cpd_sellout_by_model_his,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_sellout_by_model_his, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Jinyao"){
      
      jinyao_m <- jinyao_current %>% 
        select(Terminal.Code, `产品编号（Product.Code)`, Value_current = CY_Ach, 
               channel_current = `渠道（Channel)`) %>% 
        filter(!(channel_current %in% c("单体店", "连锁分店"))) %>% 
        left_join(jinyao_pre %>%
                    select(Terminal.Code, `产品编号（Product.Code)`, 
                           Value_pre = CY_Ach, channel_pre = `渠道（Channel)`) %>% 
                    filter(!(channel_pre %in% c("单体店", "连锁分店"))),
                  by = c("Terminal.Code", "产品编号（Product.Code)")) %>% 
        filter((Value_pre == 0 | is.na(Value_pre)) & (Value_current > 0)) %>% 
        mutate(channel = 
                 ifelse(!(channel_current %in% 
                            c("社区中心(站)", "城市公立医院",  "县域公立医院",
                              "民营医院", "乡镇医院(村诊室)",  "专业公共卫生机构")), 
                        "其他", channel_current)) %>% 
        group_by(channel) %>% 
        summarise(sellout = sum(Value_current, na.rm = T),
                  n = n()) %>% 
        ungroup() %>% 
        mutate(contri = sellout/sum(sellout, na.rm = T),
               prec_n = n/sum(n, na.rm = T))
      
      jinyao_m[] <- lapply(jinyao_m,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      }) 
      
      write.xlsx(jinyao_m, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if (Type == "KPI_by_product"){
      
      inter_market_tmp <- inter_market_m %>%
        mutate(Product = ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 30ml", "MSL 30ml",
                                ifelse(`产品名称（Product)` == "Motrin Suspension Drops 15ml", "MSD 15ml",
                                       ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 35ml", "MSL 35ml",
                                              ifelse(`产品名称（Product)` == "Motrin Suspension Drops 20ml", "MSD 20ml",
                                                     ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 100ml", "MSL 100ml", 
                                                            ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1*20g", "Daktarin Powder 1X40g"), "Daktarin Powder",
                                                                   ifelse(`产品名称（Product)` %in% c("Daktarin Spray 1x15ml", "Daktarin Spray 1x30ml"), "Daktarin Spray",
                                                                          ifelse(`产品名称（Product)` %in% c("Daktarin Cream 20mg/g 1*20g", "Daktarin Cream 1*15g"), "Daktarin", 
                                                                                 ifelse(`产品名称（Product)` %in% c("Dakgold Cream 20mg/g 1*15g"), "Gold_Daktarin", 
                                                                                        `产品名称（Product)`))))))))),
               Brand = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
        filter((Franchise %in% BU_target) & Brand %in% brand & (Model %in% model))
      
      if(model == "RPD"){
        
        inter_market_tmp <- inter_market_tmp
        
        
      }else{
        
        inter_market_tmp <- inter_market_tmp %>% 
          filter(`渠道（Channel)` %in% c("单体店", "连锁总部", "连锁分店"))
        
      }
      
      kpi_by_product_chart <- inter_market_tmp %>% 
        group_by(Product) %>% 
        summarise(sellout_p = sum(pre_sales, na.rm = T),
                  sellout_c = sum(sales, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(Contri_p = sellout_p/sum(sellout_p, na.rm = T),
               Contri_c = sellout_c/sum(sellout_c, na.rm = T)) %>% 
        # mutate(Product = factor(Product, levels = display_name)) %>% 
        arrange(Product)
      
      kpi_by_product_chart[] <- lapply(kpi_by_product_chart,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(kpi_by_product_chart, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Distri_by_month"){
      
      distri_by_month <- dist %>% 
        filter(Province %in% province & Product %in% product & Model %in% model) %>% 
        mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
        arrange(date) %>% 
        filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
        group_by(`Year/Month`, Product) %>% 
        summarise(prec = sum(`R6铺点数量`, na.rm = T)/sum(`母体数量`, na.rm = T)) %>% 
        ungroup() %>% 
        spread(Product, prec)
      
      distri_by_month_CSHQ <- dist %>% 
        filter(Province %in% province & Product %in% product &
                 Model %in% model & Key.Customer %in% c("连锁Top800")) %>% 
        mutate(date = ymd(paste(gsub("M", "", `Year/Month`), "01" , sep = ""))) %>% 
        arrange(date) %>% 
        filter(date <= last(date) & date >= last(date) - months(x = 11)) %>% 
        group_by(`Year/Month`, Product) %>% 
        summarise(prec = sum(`R6铺点数量`, na.rm = T)/sum(`母体数量`, na.rm = T)) %>% 
        ungroup() %>% 
        spread(Product, prec)
      
      distri_by_month[] <- lapply(distri_by_month,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      # distri_by_month_CSHQ[] <- lapply(distri_by_month_CSHQ,function(x) {
      #   x[is.na(x)|
      #       grepl("NA", x, perl = FALSE) |
      #       grepl("Inf", x, perl = FALSE) |
      #       grepl("NaN", x, perl = FALSE) ] <- ""
      #   x
      # })
      
      write.xlsx(distri_by_month, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      # write.xlsx(distri_by_month_CSHQ, paste0("03_Outputs/CPD/",
      #                                         final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "KPI_by_product_by_channel"){
      
      KPI_by_product_by_channel_contri <- inter_market_m %>% 
        mutate(Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                                ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                       ifelse(`渠道（Channel)` == "连锁分店", "KA DS",
                                              `渠道（Channel)`)))) %>% 
        filter(`产品名称（Product)` %in% 
                 c("Motrin Suspension Liquid 100ml", "Motrin Suspension Drops 20ml") & 
                 Model %in% "NDC bottom up") %>% 
        filter(Channel %in% c("IDS", "CSHQ", "KA DS")) %>% 
        group_by(Product = `产品名称（Product)`, Channel) %>% 
        summarise(Sellout = sum(sales, na.rm = T)) %>% 
        ungroup() %>% 
        spread(Product, Sellout) %>% 
        mutate(contri_MSD20 = `Motrin Suspension Drops 20ml`/sum(`Motrin Suspension Drops 20ml`, na.rm = T),
               contri_MSL100 = `Motrin Suspension Liquid 100ml`/sum(`Motrin Suspension Liquid 100ml`, na.rm = T))
      
      KPI_by_product_by_channel_contri[] <- lapply(KPI_by_product_by_channel_contri,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(KPI_by_product_by_channel_contri, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Province_by_model_his"){
      
      # if(BU_target != "Psychiatry"){
      #    
      #   inter_market_m1 <- inter_market_m %>% 
      #     left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>% 
      #     mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & Teamname %in% c("PC_MC"), "PCMC", "MR"),
      #            Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
      #            Model = Model_m1)
      #   
      # }else{
      #   
      #   inter_market_m1 <- inter_market_m
      #   
      # }
      
      
      if(is.na(brand)){
        
        tmp <- inter_market_m %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          group_by(Province) %>% 
          summarise(sellout = sum(sales, na.rm = T)/1000000) %>% 
          ungroup() %>% 
          arrange(desc(sellout)) %>% 
          filter(row_number() <= 15)
        
        name <- inter_market_m %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & 
                   Province %in% (unique(tmp$Province))) %>% 
          distinct(Model) %>% 
          unlist()
        
        province_by_model_his <- inter_market_m %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target & 
                   Province %in% (unique(tmp$Province))) %>% 
          group_by(Model, Province) %>% 
          summarise(Sellout =  sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, Sellout)
        
      }else{
        
        tmp <- inter_market_m %>% 
          mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% 
                                        c("Daktarin", "Gold_Daktarin"), 
                                      "Daktarin Family", `品牌（Brand)`)) %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          filter(`品牌（Brand)` %in% brand) %>% 
          group_by(Province) %>% 
          summarise(sellout = sum(sales, na.rm = T)/1000000) %>% 
          ungroup() %>% 
          arrange(desc(sellout)) %>% 
          filter(row_number() <= 15)
        
        name <- inter_market_m %>% 
          mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in%
                                        c("Daktarin", "Gold_Daktarin"),
                                      "Daktarin Family", `品牌（Brand)`)) %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                   Franchise %in% BU_target & 
                   Province %in% (unique(tmp$Province))) %>% 
          filter(`品牌（Brand)` %in% brand) %>% 
          distinct(Model) %>% 
          unlist()
        
        province_by_model_his <- inter_market_m %>% 
          mutate(`品牌（Brand)` =
                   ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), 
                          "Daktarin Family", `品牌（Brand)`)) %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                   Franchise %in% BU_target & Province %in% (unique(tmp$Province))) %>% 
          filter(`品牌（Brand)` %in% brand) %>% 
          group_by(Model, Province) %>% 
          summarise(Sellout =  sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, Sellout)
        
      }
      
      chk <- data.frame(Total = apply(province_by_model_his[, -1], 1, sum, na.rm = T))
      
      chk1 <- province_by_model_his %>% 
        bind_cols(chk)
      
      for(name in names(chk1)[-c(1, ncol(chk1))]){
        
        names(chk1)[names(chk1) == name] <- "x"
        
        name_m <- paste0(name, "_m")
        
        chk1 <- chk1 %>% 
          mutate(x = ifelse(is.na(x), 0, x),
                 x_m = x/Total)
        
        names(chk1)[names(chk1) == "x"] <- name
        names(chk1)[names(chk1) == "x_m"] <- paste0(name, "_contri")
        
        print(name)
        
      }
      
      chk2 <- chk1 %>% 
        select(-Total) %>% 
        mutate(Province = factor(Province, levels = tmp$Province)) %>% 
        arrange(Province)
      
      chk2[] <- lapply(chk2,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(chk2, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
      
    }else if(Type == "Contribution_by_model"){
      
      if(BU_target == "Derm/Anti-allergy"){
        
        inter_market_tmp <- inter_market_m %>% 
          mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                    Teamname %in% c("PC_MC"), "PCMC", "MR"),
                 Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) 
        
        cpd_f17_1_m1 <- inter_market_tmp %>% 
          filter(Franchise %in% BU_target & `品牌（Brand)` %in% display_name) %>%
          group_by(Class = `品牌（Brand)`, Model = Model_m1) %>% 
          summarise(sellout = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, sellout)
        
        rhinocort_by_product <- inter_market_tmp %>% 
          filter(`品牌（Brand)` == "Rhinocort") %>% 
          group_by(Class = `产品名称（Product)`, Model = Model_m1) %>% 
          summarise(sellout = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, sellout)
        
        cpd_f17_1_m2 <- cpd_f17_1_m1 %>% 
          full_join(rhinocort_by_product)
        
      }else if(BU_target == "Primary Care"){
        
        inter_market_tmp <- inter_market_m %>% 
          mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                    Teamname %in% c("PC_MC"), "PCMC", "MR"),
                 Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) 
        
        cpd_f17_1_m1 <- inter_market_tmp %>% 
          filter(Franchise %in% BU_target & `品牌（Brand)` %in% display_name) %>%
          group_by(Class = `品牌（Brand)`, Model = Model_m1) %>% 
          summarise(sellout = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, sellout)
        
        others <- inter_market_tmp %>% 
          mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% 
                                        display_name, display_name, "Others")) %>% 
          filter(`品牌（Brand)` == "Others") %>% 
          group_by(Class = `品牌（Brand)`, Model = Model_m1) %>% 
          summarise(sellout = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, sellout)
        
        cpd_f17_1_m2 <- cpd_f17_1_m1 %>% 
          full_join(others)
        
      }else{
        
        cpd_f17_1_m1 <- inter_market_m %>% 
          filter(Franchise %in% BU_target & `品牌（Brand)` %in% display_name) %>%
          group_by(Class = `品牌（Brand)`, Model) %>% 
          summarise(sellout = sum(sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, sellout)
        
        cpd_f17_1_m2 <- cpd_f17_1_m1
        
      }
      
      chk <- data.frame(Total = apply(cpd_f17_1_m2[, -1], 1, sum, na.rm = T))
      
      chk1 <- cpd_f17_1_m2 %>% 
        bind_cols(chk)
      
      for(name in names(chk1)[-c(1, ncol(chk1))]){
        
        names(chk1)[names(chk1) == name] <- "x"
        
        name_m <- paste0(name, "_m")
        
        chk1 <- chk1 %>% 
          mutate(x = ifelse(is.na(x), 0, x),
                 x_m = x/Total)
        
        names(chk1)[names(chk1) == "x"] <- name
        names(chk1)[names(chk1) == "x_m"] <- paste0(name, "_contri")
        
        print(name)
        
      }
      
      cpd_f17_1_m3 <- chk1 %>% 
        select(-Total) %>% 
        ungroup() %>% 
        mutate(Class = factor(Class, levels = display_name)) %>% 
        arrange(Class)
      
      cpd_f17_1_m3[] <- lapply(cpd_f17_1_m3,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_f17_1_m3, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
    } 
    
  }
  
  if(Table.type == "Table"){
    
    if(Type == "IMS CHPA MS"){
      
      if(Defined_market == "Concerta"){
        
        ims_ms_ei <- exter_hos %>% 
          filter(Defined.Market %in% Defined_market & Product %in% IMS_product) %>% 
          filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
          filter(KPI %in% c("Volume ('000)")) %>% 
          group_by(Product, Year) %>% 
          summarise(Value = sum(as.numeric(Value), na.rm = T)) %>% 
          ungroup() %>% 
          group_by(Year) %>% 
          mutate(`YTD MS` = Value/sum(Value, na.rm = T)) %>% 
          ungroup() %>% 
          mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                                    ifelse(Year == YTD_year_max - 1, "past", 
                                           "-"))) %>% 
          setDT() %>% 
          dcast(Product ~ year_type, value.var = "YTD MS") %>% 
          mutate(EI = current/past*100) %>% 
          select(Product, `YTD MS` = current, EI) %>% 
          mutate(Product = factor(Product, levels = IMS_product)) %>% 
          arrange(Product) %>% 
          filter(!is.na(Product))
        
      }else{
        
        ims_ms_ei <- exter_hos %>% 
          filter(Defined.Market %in% Defined_market) %>% 
          filter(as.numeric(Year) %in% c(YTD_year_max, YTD_year_max - 1) & as.numeric(Month) <= (YTD_month_max - 1)) %>% 
          filter(KPI %in% c("Volume ('000)")) %>% 
          mutate(Product = ifelse(Product %in% IMS_product, Product, "Others")) %>% 
          group_by(Product, Year) %>% 
          summarise(Value = sum(as.numeric(Value), na.rm = T)) %>% 
          ungroup() %>% 
          group_by(Year) %>% 
          mutate(`YTD MS` = Value/sum(Value, na.rm = T)) %>% 
          ungroup() %>% 
          mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                                    ifelse(Year == YTD_year_max - 1, "past", 
                                           "-"))) %>% 
          setDT() %>% 
          dcast(Product ~ year_type, value.var = "YTD MS") %>% 
          mutate(EI = current/past*100) %>% 
          select(Product, `YTD MS` = current, EI) %>% 
          mutate(Product = factor(Product, levels = IMS_product)) %>% 
          arrange(Product) %>% 
          filter(!is.na(Product))
        
      }
      
      ims_ms_ei[] <- lapply(ims_ms_ei,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(ims_ms_ei, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Province"){
      
      BU_target <- mapping_tbl %>% 
        filter(Page == final_page) %>% 
        select(BU_target) %>% 
        distinct() %>% 
        unlist()
      
      cpd_exclu_lex_top_total <- inter_market_m %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
        summarise(Sales = round(sum(sales, na.rm = T)))
      
      cpd_top <- inter_market_m %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
        group_by(Province) %>% 
        summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
                  `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1,
                  `Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1)) %>% 
        ungroup() %>% 
        arrange(desc(`Contr.%`)) %>% 
        filter(row_number()<=15)
      
      cpd_top_other <- inter_market_m %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                 Franchise %in% BU_target & 
                 !(Province %in% c(cpd_top$Province))) %>% 
        summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
                  `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(STP)` = (sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1),
                  `Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1)) %>% 
        mutate(Province = "Others") %>% 
        select(Province, everything())
      
      TOP_15_total <- inter_market_m %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
        filter(Province %in% c(cpd_top$Province)) %>% 
        summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
                  `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(STP)` = (sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1),
                  `Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1)) %>% 
        mutate(Province = "TOP 15 Total") %>% 
        select(Province, everything())
      
      cpd_exclu_lex_top <- inter_market_m %>% 
        filter(`Year/Month` <= YTD_time_max & `Year/Month` >= YTD_time_min) %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
        summarise(`Contr.%` = sum(sales, na.rm = T)/cpd_exclu_lex_top_total$Sales,
                  `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(STP)` = (sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1),
                  `Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1)) %>% 
        mutate(Province = "Total Sellout") %>% 
        select(Province, everything())
      
      cpd_top_m1 <- cpd_exclu_lex_top %>% 
        full_join(TOP_15_total) %>% 
        full_join(cpd_top) %>% 
        full_join(cpd_top_other) 
      
      cpd_top_m1[] <- lapply(cpd_top_m1,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_top_m1, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Province_by_model"){
      
      if(BU_target != "Psychiatry"){
        
        inter_market_m1 <- inter_market_m %>% 
          select(-Teamname) %>%
          left_join(PCMC_mapping %>% filter(Teamname %in% "PC_MC"), by = "N8") %>% 
          mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                    Teamname %in% c("PC_MC"), "PCMC", "MR"),
                 Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model),
                 Model = Model_m1,
                 Model = ifelse(Model == "MR", "MR_Hospital", Model)) 
        
      }else{
        
        inter_market_m1 <- inter_market_m %>% 
          mutate(Model = ifelse(Model == "MR", "MR_Hospital", Model)) 
        
      }
      
      if(is.na(brand)){
        
        tmp <- inter_market_m %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          group_by(Province) %>% 
          summarise(sellout = sum(sales, na.rm = T)/1000000) %>% 
          ungroup() %>% 
          arrange(desc(sellout)) %>% 
          filter(row_number() <= 15)
        
        top_15_total_sales <- inter_market_m %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>%
          summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    sales = sum(sales, na.rm = T)) %>% 
          mutate(`Contr.%` = sales/sum(sales, na.rm = T),
                 Model = "Top 15 Total")
        
        cpd_map_by_model_total <- inter_market_m1 %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>%
          summarise(`Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1),
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Contr.%` = sum(sales, na.rm = T)/top_15_total_sales$sales) %>% 
          mutate(Model = "Top 15 Total") %>% 
          select(Model, everything())
        
        cpd_map_by_model <- inter_market_m1 %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>% 
          group_by(Model) %>% 
          summarise(`Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1),
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Contr.%` = sum(sales, na.rm = T)/top_15_total_sales$sales) %>% 
          ungroup() %>% 
          full_join(top_15_total_sales %>% select(-sales)) %>% 
          mutate(Model = factor(Model, levels = display_name)) %>% 
          arrange(Model)
        
      }else{
        
        inter_market_m2 <- inter_market_m1 %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% 
                                        c("Daktarin", "Gold_Daktarin"), 
                                      "Daktarin Family", `品牌（Brand)`))
        
        tmp <- inter_market_m2 %>% 
          filter(!(`品牌（Brand)` %in% c("Lexapro")) & Franchise %in% BU_target) %>% 
          filter(`品牌（Brand)` %in% brand) %>% 
          group_by(Province) %>% 
          summarise(sellout = sum(sales, na.rm = T)/1000000) %>% 
          ungroup() %>% 
          arrange(desc(sellout)) %>% 
          filter(row_number() <= 15)
        
        top_15_total_sales <- inter_market_m2 %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>%
          filter(`品牌（Brand)` %in% brand) %>% 
          summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    sales = sum(sales, na.rm = T)) %>% 
          mutate(`Contr.%` = sales/sum(sales, na.rm = T),
                 Model = "Top 15 Total")
        
        cpd_map_by_model_total <- inter_market_m2 %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>%
          filter(`品牌（Brand)` %in% brand) %>% 
          summarise(`Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1),
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Contr.%` = sum(sales, na.rm = T)/top_15_total_sales$sales) %>% 
          mutate(Model = "Top 15 Total") %>% 
          select(Model, everything())
        
        cpd_map_by_model <- inter_market_m2 %>% 
          filter(!((`品牌（Brand)` %in% c("Lexapro"))) & Franchise %in% BU_target) %>% 
          filter(Province %in% tmp$Province) %>% 
          filter(`品牌（Brand)` %in% brand) %>% 
          group_by(Model) %>% 
          summarise(`Gr%(MLP)` = (sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1),
                    `Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `Contr.%` = sum(sales, na.rm = T)/top_15_total_sales$sales) %>% 
          ungroup() %>% 
          full_join(top_15_total_sales %>% select(-sales)) %>% 
          mutate(Model = factor(Model, levels = display_name)) %>% 
          arrange(Model)
        
      }
      
      cpd_map_by_model[] <- lapply(cpd_map_by_model,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_map_by_model, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "Sellout_by_model"){
      
      inter_market_m1 <- inter_market_m
      
      cpd_sellout_by_model <- inter_market_m1 %>% 
        mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                  Teamname %in% c("PC_MC"), "PCMC", "MR"),
               Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
        mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% c("Dakatarin", "Gold_Daktarin"),
                                    "Daktarin Family", `品牌（Brand)`)) %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                 Franchise %in% BU_target & `品牌（Brand)` %in% brand) %>% 
        group_by(Model = Model_m1) %>% 
        summarise(`Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        ungroup()
      
      cpd_sellout_by_model_ttl <- inter_market_m1 %>% 
        mutate(Model_m = ifelse(Model %in% c("MR_Hospital") & 
                                  Teamname %in% c("PC_MC"), "PCMC", "MR"),
               Model_m1 = ifelse(Model %in% c("MR_Hospital"), Model_m, Model)) %>% 
        mutate(`品牌（Brand)` = 
                 ifelse(`品牌（Brand)` %in% c("Dakatarin", "Gold_Daktarin"),
                        "Daktarin Family", `品牌（Brand)`)) %>% 
        filter(!((`品牌（Brand)` %in% c("Lexapro"))) & 
                 Franchise %in% BU_target & `品牌（Brand)` %in% brand) %>% 
        summarise(`Ach%(MLP)` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        mutate(Model = "Total") %>% 
        full_join(cpd_sellout_by_model) %>% 
        mutate(Model = factor(Model, levels = display_name)) %>% 
        arrange(Model) %>% 
        select(Model, everything())
      
      cpd_sellout_by_model_ttl[] <- lapply(cpd_sellout_by_model_ttl,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(cpd_sellout_by_model_ttl, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
      
    }else if(Type == "KPI_by_product"){
      
      inter_market_tmp <- inter_market_m %>%
        mutate(Product = 
                 ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 30ml", "MSL 30ml",
                        ifelse(`产品名称（Product)` == "Motrin Suspension Drops 15ml", "MSD 15ml",
                               ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 35ml", "MSL 35ml",
                                      ifelse(`产品名称（Product)` == "Motrin Suspension Drops 20ml", "MSD 20ml",
                                             ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 100ml", "MSL 100ml", 
                                                    ifelse(`产品名称（Product)` %in% c("Daktarin Powder 1*20g", "Daktarin Powder 1X40g"), "Daktarin Powder",
                                                           ifelse(`产品名称（Product)` %in% c("Daktarin Spray 1x15ml", "Daktarin Spray 1x30ml"), "Daktarin Spray",
                                                                  ifelse(`产品名称（Product)` %in% c("Daktarin Cream 20mg/g 1*20g", "Daktarin Cream 1*15g"), "Daktarin", 
                                                                         ifelse(`产品名称（Product)` %in% c("Dakgold Cream 20mg/g 1*15g"), "Gold_Daktarin",`产品名称（Product)`))))))))),
               Brand = ifelse(`品牌（Brand)` %in% c("Daktarin", "Gold_Daktarin"), "Daktarin Family", `品牌（Brand)`)) %>% 
        filter((Franchise %in% BU_target) & Brand %in% brand & (Model %in% model))
      
      
      if(model == "RPD"){
        
        inter_market_tmp <- inter_market_tmp
        
      }else{
        
        inter_market_tmp <- inter_market_tmp %>% 
          filter(`渠道（Channel)` %in% c("单体店", "连锁总部", "连锁分店"))
        
      }
      
      kpi_by_product_m1_tbl <- inter_market_tmp %>% 
        group_by(Product) %>% 
        summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        ungroup()
      
      kpi_by_product_m2_tbl <- inter_market_tmp %>% 
        summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        mutate(Product = brand) %>% 
        full_join(kpi_by_product_m1_tbl) %>% 
        select(Product, everything()) 
      
      # if(brand == "Daktarin Family"){
      #   
      #   tmp <- inter_market_tmp %>% 
      #     group_by(Product = Brand) %>% 
      #     summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
      #               `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
      #               `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) 
      #   
      #   tmp1 <- inter_market_tmp %>% 
      #     group_by(Product = `品牌（Brand)`) %>% 
      #     summarise(`Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
      #               `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
      #               `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1)
      #   
      #   kpi_by_product_m2_tbl <- kpi_by_product_m2_tbl %>% 
      #     filter(Product %in% c("Daktarin Powder", "Daktarin Spray")) %>% 
      #     full_join(tmp1) %>% 
      #     full_join(tmp)
      #   
      # }
      
      kpi_by_product_m2_tbl_m <- kpi_by_product_m2_tbl %>% 
        mutate(Product = factor(Product, levels = display_name)) %>% 
        arrange(Product)
      
      kpi_by_product_m2_tbl[] <- lapply(kpi_by_product_m2_tbl,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      write.xlsx(kpi_by_product_m2_tbl, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "KPI_by_product_by_channel"){
      
      inter_market_tmp <- inter_market_m %>%
        mutate(Product = ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 30ml", "MSL 30ml",
                                ifelse(`产品名称（Product)` == "Motrin Suspension Drops 15ml", "MSD 15ml",
                                       ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 35ml", "MSL 35ml",
                                              ifelse(`产品名称（Product)` == "Motrin Suspension Drops 20ml", "MSD 20ml",
                                                     ifelse(`产品名称（Product)` == "Motrin Suspension Liquid 100ml", "MSL 100ml", 
                                                            `产品名称（Product)`))))),
               Channel = ifelse(`渠道（Channel)` == "单体店", "IDS",
                                ifelse(`渠道（Channel)` == "连锁总部", "CSHQ",
                                       ifelse(`渠道（Channel)` == "连锁分店", "KA DS", 
                                              `渠道（Channel)`))))
      
      
      KPI_by_product_by_channel_ttl <- inter_market_tmp %>% 
        filter(Product %in% c("MSL 100ml", "MSD 20ml") & Model %in% model) %>% 
        filter(Channel %in% c("IDS", "CSHQ", "KA DS")) %>% 
        group_by(Product) %>% 
        summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        ungroup() %>% 
        mutate(Channel = "Total")
      
      KPI_by_product_by_channel <- inter_market_tmp %>% 
        filter(Product %in% c("MSL 100ml", "MSD 20ml") & Model %in% model) %>% 
        filter(Channel %in% c("IDS", "CSHQ", "KA DS")) %>% 
        group_by(Channel, Product) %>% 
        summarise(`Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Gr%(STP)` = sum(CY_Ach, na.rm = T)/sum(PY_Ach, na.rm = T) - 1) %>% 
        ungroup() %>% 
        full_join(KPI_by_product_by_channel_ttl) %>% 
        mutate(` ` = factor(Product, levels = c("MSL 100ml", "MSD 20ml")),
               Channel = factor(Channel, levels = display_name)) %>% 
        arrange(Channel)
      
      KPI_by_product_by_channel_m <-
        tabular(Heading() * Channel ~ ` ` * (`Gr%(MLP)` + `Gr%(STP)`) * Heading() * (identity), 
                data = KPI_by_product_by_channel)
      
      write.table.tabular(KPI_by_product_by_channel_m, paste0(save_location, "/tmp.csv"), sep = ",")
      
      tmp <- fread(paste0(save_location, "/tmp.csv"), skip = 1)
      
      # names(tmp) <- rep("", ncol(tmp))
      
      tmp[] <- lapply(tmp,function(x) {
        x[is.na(x)|
            grepl("NA", x, perl = FALSE) |
            grepl("Inf", x, perl = FALSE) |
            grepl("NaN", x, perl = FALSE) ] <- ""
        x
      })
      
      names(tmp) <- rep("", ncol(tmp))
      
      write.xlsx(tmp, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
    }else if(Type == "MS_Daktarin"){
      
      ## only one table in this type
      cpd_f50_4 <-
        retail_mkt_daktarin[1:which(retail_mkt_daktarin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
        filter(`Value.('000RMB)` %in% 
                 c("AFTotal", "DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS",
                   "DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS",
                   "Daktarin Powder", "Daktarin Cream")) %>% 
        mutate(`Value.('000RMB)` =
                 ifelse(`Value.('000RMB)` %in% 
                          c("DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS"),
                        "Daktarin Spray",
                        ifelse(`Value.('000RMB)` %in% 
                                 c("DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS"), 
                               "Gold_Daktarin",`Value.('000RMB)`))) %>% 
        gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
        separate(Date, c("Year", "Month"), sep = "M") %>% 
        filter(Year %in% c(YTD_year_min, YTD_year_max) &
                 (!is.na(Year)) & (!is.na(Month))) %>% 
        group_by(Year, `Value.('000RMB)`) %>% 
        summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
        ungroup() %>% 
        mutate(AFTotal = first(Value_ttl),
               MS = Value_ttl/AFTotal) %>% 
        select(Year, `MKT Share(YTD)` = MS, `Value.('000RMB)`) %>% 
        mutate(year_type = ifelse(Year == YTD_year_max, "current", 
                                  ifelse(Year == YTD_year_max - 1, "past", 
                                         "-"))) %>% 
        setDT() %>% 
        dcast(`Value.('000RMB)` ~ year_type, value.var = "MKT Share(YTD)") %>% 
        mutate(`Change vs. LY` = current - past,
               EI = current/past*100) %>% 
        select(Product = `Value.('000RMB)`, `MKT Share(YTD)` = current,
               `Change vs. LY`, EI) %>% 
        filter(Product != "AFTotal") %>% 
        mutate(Product = ifelse(Product == "DAKTARIN / JANSSEN-XIAN / ANTIFUNGALS", 
                                "DAKTARIN", Product),
               Product = factor(Product, levels = display_name)) %>% 
        arrange(Product)
      
      # tmp[] <- lapply(tmp,function(x) {
      #   x[is.na(x)|
      #       grepl("NA", x, perl = FALSE) |
      #       grepl("Inf", x, perl = FALSE) |
      #       grepl("NaN", x, perl = FALSE) ] <- ""
      #   x
      # })
      
      write.xlsx(cpd_f50_4, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
      
      
    }else if(Type == "Retail_MS_by_month"){
      
      cpd_f53_4 <- retail_mkt_daktarin[1:which(retail_mkt_daktarin$`Value.('000RMB)` == "Value Share") - 1, ] %>% 
        filter(`Value.('000RMB)` %in% c("AFTotal", "DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS",
                                        "DING KE / QILU PHARM / ANTIFUNGALS",
                                        "DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS")) %>% 
        mutate(`Value.('000RMB)` =
                 ifelse(`Value.('000RMB)` %in% 
                          c( "DAKTARIN SPARY / JANSSEN-XIAN / ANTIFUNGALS"), 
                        "DAK Spray",
                        ifelse(`Value.('000RMB)` %in% 
                                 c("DAKTARIN GOLD / JANSSEN-XIAN / ANTIFUNGALS"), 
                               "Gold_Daktarin",
                               ifelse(`Value.('000RMB)` %in% 
                                        c("DING KE / QILU PHARM / ANTIFUNGALS"), 
                                      "DING KE", `Value.('000RMB)`)))) %>% 
        gather(key = "Date", value = "Value", -`Value.('000RMB)`) %>% 
        separate(Date, c("Year", "Month"), sep = "M") %>% 
        filter(Year %in% c(YTD_year_min, YTD_year_max) & (!is.na(Year)) & (!is.na(Month))) %>% 
        group_by(Year, `Value.('000RMB)`, Month) %>% 
        summarise(Value_ttl = sum(as.numeric(Value), na.rm = T)) %>% 
        ungroup() %>% 
        spread(`Value.('000RMB)`, Value_ttl) %>% 
        mutate(Gold_Dak_MS = Gold_Daktarin/AFTotal,
               Ding_Ke_MS = `DING KE`/AFTotal,
               DAK_Spray_MS = `DAK Spray`/AFTotal) %>% 
        select(-AFTotal)
      
    }else if(Type == "Province_Top_Ten"){
      
      ##-- P47_2 province 
      province_top_ten <- inter_market_m %>% 
        filter(Franchise %in% BU_target & 
                 `品牌（Brand)` %in% brand & Model_m1 %in% c("MR", "PCMC")) %>% 
        group_by(Province) %>% 
        summarise(`Sales value Mil(YTD)` = sum(sales, na.rm = T)/1000000,
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
        ungroup() %>% 
        mutate(`Contr.%` = 
                 `Sales value Mil(YTD)`/sum(`Sales value Mil(YTD)`, na.rm = T)) %>% 
        arrange(desc(`Sales value Mil(YTD)`)) %>% 
        filter(row_number() <= 10) %>% 
        select(Province, `Sales value Mil(YTD)`, `Contr.%`, `Ach%`, `Gr%(MLP)`)
      
      province_ttl_sale <- inter_market_m %>% 
        filter(Franchise %in% BU_target & 
                 `品牌（Brand)` %in% brand & Model_m1 %in% c("MR", "PCMC")) %>% 
        summarise(sales = sum(sales, na.rm = T)/1000000)
      
      province_top_ten_ttl <- inter_market_m %>% 
        filter(Franchise %in% BU_target & 
                 `品牌（Brand)` %in% brand & Model_m1 %in% c("MR", "PCMC")) %>% 
        filter(Province %in% province_top_ten$Province) %>% 
        summarise(`Sales value Mil(YTD)` = sum(sales, na.rm = T)/1000000,
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `Gr%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1) %>% 
        mutate(`Contr.%` = `Sales value Mil(YTD)`/province_ttl_sale$sales,
               Province = "Top10 total") %>% 
        select(Province, `Sales value Mil(YTD)`, `Contr.%`, `Ach%`, `Gr%(MLP)`)
      
      write.xlsx(province_top_ten_ttl, paste0(save_location, "/", final_page, "_1.xlsx"), sep = ",")
      write.xlsx(province_top_ten, paste0(save_location, "/", final_page, "_2.xlsx"), sep = ",")
      
    }else{
      
      # 实际上为P17-2 type
      sell_brandmod_ttl_m1 <- inter_market_m %>% 
        filter(Franchise == BU_target & 
                 !(`品牌（Brand)` %in% c("Lexapro", "Rhinocort"))) %>% 
        group_by(Model) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                    sum(pre_volume, na.rm = T),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        ungroup() %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T),
               `品牌（Brand)` = "Total")
      
      sell_brandmod_ttl_m2 <- inter_market_m %>% 
        filter(Franchise == BU_target & !(`品牌（Brand)` %in% c("Lexapro", "Rhinocort"))) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                    sum(pre_volume, na.rm = T),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T),
               Model = "YTD",
               `品牌（Brand)` = "Total")
      
      sell_brandmod_ttl_m3 <- inter_market_m %>% 
        filter(`Year/Month` == YTD_time_max) %>% 
        filter(Franchise == BU_target & !(`品牌（Brand)` %in% 
                                            c("Lexapro", "Rhinocort"))) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = ((sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                                     sum(pre_volume, na.rm = T)),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T),
               Model = "MTD",
               `品牌（Brand)` = "Total")
      
      sell_brandmod_ttl_m4 <- sell_brandmod_ttl_m2 %>% 
        full_join(sell_brandmod_ttl_m3) %>% 
        full_join(sell_brandmod_ttl_m1)
      
      sell_brandmod_m1 <- inter_market_m %>% 
        filter(Franchise == BU_target & !(`品牌（Brand)` %in% 
                                            c("Lexapro", "Rhinocort"))) %>% 
        mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% display_name, 
                                    `品牌（Brand)`, "Others")) %>% 
        group_by(Model, `品牌（Brand)`) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                    sum(pre_volume, na.rm = T),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        ungroup() %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T))
      
      sell_brandmod_m2 <- inter_market_m %>% 
        filter(Franchise == BU_target & 
                 !(`品牌（Brand)` %in% c("Lexapro", "Rhinocort"))) %>% 
        mutate(`品牌（Brand)` = ifelse(`品牌（Brand)` %in% display_name,
                                    `品牌（Brand)`, "Others")) %>% 
        group_by(`品牌（Brand)`) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                    sum(pre_volume, na.rm = T),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        ungroup() %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T)) %>% 
        mutate(Model = "YTD")
      
      sell_brandmod_m3 <- inter_market_m %>% 
        filter(`Year/Month` == YTD_time_max) %>% 
        filter(Franchise == BU_target & !(`品牌（Brand)` %in% 
                                            c("Lexapro", "Rhinocort"))) %>% 
        mutate(`品牌（Brand)` = 
                 ifelse(`品牌（Brand)` %in% display_name, `品牌（Brand)`, "Others")) %>% 
        group_by(`品牌（Brand)`) %>% 
        summarise(Sellout = sum(sales, na.rm = T),
                  `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                  `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                  `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                    sum(pre_volume, na.rm = T),
                  `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
        ungroup() %>% 
        mutate(`Contr.%` = Sellout/sum(Sellout, na.rm = T)) %>% 
        mutate(Model = "MTD")
      
      sell_brandmod_m4 <- sell_brandmod_m1 %>% 
        full_join(sell_brandmod_m2) %>% 
        full_join(sell_brandmod_m3) %>% 
        full_join(sell_brandmod_ttl_m4)
      
      if(BU_target == "Psychiatry"){
        
        sell_brandmod_LAT_m1 <- inter_market_m %>% 
          filter(Franchise == "Psychiatry" & 
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna")) %>% 
          group_by(Model) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          ungroup() %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m1$Sellout,
                 `品牌（Brand)` = "PPLAT")
        
        sell_brandmod_LAT_m2 <- inter_market_m %>% 
          filter(Franchise == "Psychiatry" & 
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna")) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m2$Sellout,
                 Model = "YTD",
                 `品牌（Brand)` = "PPLAT")
        
        sell_brandmod_LAT_m3 <- inter_market_m %>% 
          filter(`Year/Month` == YTD_time_max) %>% 
          filter(Franchise == "Psychiatry" & 
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna")) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m3$Sellout,
                 Model = "MTD",
                 `品牌（Brand)` = "PPLAT")
        
        sell_brandmod_LAT_m4 <- sell_brandmod_LAT_m2 %>% 
          full_join(sell_brandmod_LAT_m3) %>% 
          full_join(sell_brandmod_LAT_m1)
        
        sell_brandmod_strategic_m1 <- inter_market_m %>% 
          filter(Franchise == "Psychiatry" &
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna",
                                      "Concerta", "Invega")) %>% 
          group_by(Model) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          ungroup() %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m1$Sellout,
                 `品牌（Brand)` = "PSY Strategic")
        
        sell_brandmod_strategic_m2 <- inter_market_m %>% 
          filter(Franchise == "Psychiatry" & 
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna",
                                      "Concerta", "Invega")) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m2$Sellout,
                 Model = "YTD",
                 `品牌（Brand)` = "PSY Strategic")
        
        sell_brandmod_strategic_m3 <- inter_market_m %>% 
          filter(`Year/Month` == YTD_time_max) %>% 
          filter(Franchise == "Psychiatry" & 
                   `品牌（Brand)` %in% c("Invega Trinza", "Sustenna",
                                      "Concerta", "Invega")) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m3$Sellout,
                 Model = "MTD",
                 `品牌（Brand)` = "PSY Strategic")
        
        sell_brandmod_strategic_m4 <- sell_brandmod_strategic_m2 %>% 
          full_join(sell_brandmod_strategic_m3) %>% 
          full_join(sell_brandmod_strategic_m1)
        
        sell_brandmod_final <- sell_brandmod_m4 %>% 
          full_join(sell_brandmod_strategic_m4) %>% 
          full_join(sell_brandmod_LAT_m4)
        
      }else if(BU_target == "Derm/Anti-allergy"){
        
        sell_brandmod_rhinocort_m1 <- inter_market_m %>% 
          filter(Franchise == "Derm/Anti-allergy" & `品牌（Brand)` == "Rhinocort") %>% 
          group_by(`品牌（Brand)` = `产品名称（Product)`, Model) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          ungroup() %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m1$Sellout)
        
        sell_brandmod_rhinocort_m2 <- inter_market_m %>% 
          filter(Franchise == "Derm/Anti-allergy" & `品牌（Brand)` == "Rhinocort") %>% 
          group_by(`品牌（Brand)` = `产品名称（Product)`, Model) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          ungroup() %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m2$Sellout,
                 Model = "YTD")
        
        sell_brandmod_rhinocort_m3 <- inter_market_m %>% 
          filter(`Year/Month` == YTD_time_max) %>% 
          filter(Franchise == "Derm/Anti-allergy" & `品牌（Brand)` == "Rhinocort") %>% 
          group_by(`品牌（Brand)` = `产品名称（Product)`, Model) %>% 
          summarise(Sellout = sum(sales, na.rm = T),
                    `Ach%` = sum(sales, na.rm = T)/sum(target, na.rm = T),
                    `GR%(MLP)` = sum(sales, na.rm = T)/sum(pre_sales, na.rm = T) - 1,
                    `Growth Vol.` = (sum(volume, na.rm = T) - sum(pre_volume, na.rm = T))/
                      sum(pre_volume, na.rm = T),
                    `Growth Price.` = `GR%(MLP)` - `Growth Vol.`) %>% 
          ungroup() %>% 
          mutate(`Contr.%` = Sellout/sell_brandmod_ttl_m3$Sellout,
                 Model = "MTD")
        
        sell_brandmod_final <- sell_brandmod_m4 %>% 
          full_join(sell_brandmod_rhinocort_m3) %>% 
          full_join(sell_brandmod_rhinocort_m1) %>% 
          full_join(sell_brandmod_rhinocort_m2)
        
      }else{
        
        sell_brandmod_final <- sell_brandmod_m4
        
      }
      
      colume_level <- sell_brandmod_final %>% 
        select(Model) %>% 
        distinct() %>% 
        filter(!(Model %in% c("YTD", "MTD"))) %>% 
        unlist()
      
      sell_brandmod_final_m <- sell_brandmod_final %>% 
        ungroup() %>% 
        mutate(` ` = factor(Model, levels = c("YTD", "MTD", colume_level)),
               Brand = factor(`品牌（Brand)`, levels = display_name)) %>% 
        arrange()
      
      sell_brandmod_final_formula <- 
        tabular(Heading() * `Brand` ~ 
                  ` ` * (`Contr.%` + `Ach%` + `GR%(MLP)` + `Growth Vol.` + `Growth Price.`) *
                  Heading() * (identity), 
                data = sell_brandmod_final_m)
      
      write.table.tabular(sell_brandmod_final_formula, paste0(save_location, "/tmp.csv"), sep = ",")
      
      tmp <- fread(paste0(save_location, "/tmp.csv"), skip = 1)
      
      names(tmp) <- rep("", ncol(tmp))
      
      tmp[1, 1] <- "Brand"
      
      if(BU_target == "Derm/Anti-allergy"){
        
        sell_brandmod_growth_ttl <- inter_market_m %>%
          filter(Franchise == BU_target) %>%
          mutate(Model = ifelse(Model == "NDC bottom up", Model, "DC")) %>% 
          group_by(Model) %>% 
          summarise(growth = sum(sales, na.rm = T) - sum(pre_sales, na.rm = T)) %>% 
          ungroup() %>% 
          mutate(`品牌（Brand)` = "Total") %>% 
          spread(Model, growth)
        
        sell_brandmod_growth_m1 <- inter_market_m %>% 
          filter(Franchise == BU_target & `品牌（Brand)` != "Rhinocort") %>% 
          mutate(Model = ifelse(Model == "NDC bottom up", Model, "DC")) %>% 
          group_by(`品牌（Brand)`, Model) %>% 
          summarise(growth = sum(sales, na.rm = T) - sum(pre_sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, growth) 
        
        sell_brandmod_growth_m2 <- inter_market_m %>% 
          filter(Franchise == BU_target & `品牌（Brand)` == "Rhinocort") %>% 
          mutate(Model = ifelse(Model == "NDC bottom up", Model, "DC")) %>% 
          group_by(`品牌（Brand)` = `产品名称（Product)`, Model) %>% 
          summarise(growth = sum(sales, na.rm = T) - sum(pre_sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, growth)
        
        sell_brandmod_growth_m3 <- sell_brandmod_growth_m1 %>% 
          full_join(sell_brandmod_growth_m2) %>% 
          full_join(sell_brandmod_growth_ttl)
        
        chk <- data.frame(Total = apply(sell_brandmod_growth_m3[, -1], 1, sum, na.rm = T))
        
        chk1 <- sell_brandmod_growth_m3 %>% 
          bind_cols(chk)
        
        for(name in names(chk1)[-c(1, ncol(chk1))]){
          
          names(chk1)[names(chk1) == name] <- "x"
          
          name_m <- paste0(name, "_m")
          
          chk1 <- chk1 %>% 
            mutate(x = ifelse(is.na(x), 0, x),
                   x_m = x/Total)
          
          names(chk1)[names(chk1) == "x"] <- name
          names(chk1)[names(chk1) == "x_m"] <- paste0(name, "_contri")
          
          print(name)
          
        }
        
        sell_brandmod_growth_m4 <- chk1 %>% 
          mutate(Model = factor("GR contribution")) %>% 
          select(`品牌（Brand)`,
                 DC = DC_contri,
                 NDC = `NDC bottom up_contri`,
                 Model) %>% 
          mutate(Brand = factor(`品牌（Brand)`),
                 ` ` = factor(Model)) %>% 
          arrange(Brand)
        
        sell_brandmod_growth_m4_tab <-
          tabular(Heading() * `Brand` ~ ` ` * (`DC` + `NDC`) * Heading() * (identity), 
                  data = sell_brandmod_growth_m4)
        
        write.table.tabular(sell_brandmod_growth_m4_tab, paste0(save_location, "/tmp.csv"), sep = ",")
        
        tmp1 <- fread(paste0(save_location, "/tmp.csv"))
        
        tmp1[1, 1] <- "Brand"
        
        tmp2 <- tmp %>% bind_cols(tmp1[,-1])
        
        names(tmp2) <- rep("", ncol(tmp2))
        
        write.xlsx(tmp2, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
        
      }else if(BU_target == "Primary Care"){
        
        sell_brandmod_growth_ttl <- inter_market_m %>%
          filter(Franchise == "Primary Care") %>%
          mutate(Model = ifelse(Model == "NDC bottom up", Model, "DC")) %>% 
          group_by(Model) %>% 
          summarise(growth = sum(sales, na.rm = T) - sum(pre_sales, na.rm = T)) %>% 
          ungroup() %>% 
          mutate(`品牌（Brand)` = "Total") %>% 
          spread(Model, growth)
        
        sell_brandmod_growth_m1 <- inter_market_m %>% 
          filter(Franchise == "Primary Care") %>% 
          mutate(Model = ifelse(Model == "NDC bottom up", Model, "DC"),
                 `品牌（Brand)` = ifelse(`品牌（Brand)` %in%
                                        display_name, `品牌（Brand)`, "Others")) %>% 
          group_by(`品牌（Brand)`, Model) %>% 
          summarise(growth = sum(sales, na.rm = T) - sum(pre_sales, na.rm = T)) %>% 
          ungroup() %>% 
          spread(Model, growth) 
        
        sell_brandmod_growth_m2 <- sell_brandmod_growth_m1 %>% 
          full_join(sell_brandmod_growth_ttl)
        
        chk <- data.frame(Total = apply(sell_brandmod_growth_m2[, -1], 1, sum, na.rm = T))
        
        chk1 <- sell_brandmod_growth_m2 %>% 
          bind_cols(chk)
        
        for(name in names(chk1)[-c(1, ncol(chk1))]){
          
          names(chk1)[names(chk1) == name] <- "x"
          
          name_m <- paste0(name, "_m")
          
          chk1 <- chk1 %>% 
            mutate(x = ifelse(is.na(x), 0, x),
                   x_m = x/Total)
          
          names(chk1)[names(chk1) == "x"] <- name
          names(chk1)[names(chk1) == "x_m"] <- paste0(name, "_contri")
          
          print(name)
          
        }
        
        sell_brandmod_growth_m4 <- chk1 %>% 
          mutate(Model = factor("GR contribution")) %>% 
          select(`品牌（Brand)`,
                 DC = DC_contri,
                 NDC = `NDC bottom up_contri`,
                 Model) %>% 
          mutate(Brand = factor(`品牌（Brand)`, levels = display_name),
                 ` ` = factor(Model)) %>% 
          arrange(Brand)
        
        sell_brandmod_growth_m4_tab <- 
          tabular(Heading() * `Brand` ~ ` ` * (`DC` + `NDC`) * Heading() * (identity), 
                  data = sell_brandmod_growth_m4)
        
        write.table.tabular(sell_brandmod_growth_m4_tab, paste0(save_location, "/tmp.csv"), sep = ",")
        
        tmp1 <- fread(paste0(save_location, "/tmp.csv"))
        
        tmp1[1, 1] <- "Brand"
        
        tmp2 <- tmp %>% bind_cols(tmp1[,-1])
        
        # names(tmp2) <- rep("", ncol(tmp2))
        
        tmp2[] <- lapply(tmp2,function(x) {
          x[is.na(x)|
              grepl("NA", x, perl = FALSE) |
              grepl("Inf", x, perl = FALSE) |
              grepl("NaN", x, perl = FALSE) ] <- ""
          x
        })
        
        names(tmp2) <- rep("", ncol(tmp2))
        
        write.xlsx(tmp2, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
        
        
      }else{
        
        write.xlsx(tmp, paste0(save_location, "/", final_page, ".xlsx"), sep = ",")
        
      }
    }
    
  }
  
}
