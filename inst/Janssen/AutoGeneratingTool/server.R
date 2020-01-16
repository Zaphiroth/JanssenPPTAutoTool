# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen PPT
# Purpose:      Ui
# programmer:   Zhe Liu
# Date:         20-11-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


server <- function(input, output, session) {
  # directory choose
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  shinyDirChoose(input, "cpd_raw", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyDirChoose(input, "ipd_raw", roots = volumes, session = session, restrictions = system.file(package = "base"))
  
  shinyDirChoose(input, "cpd_save", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyDirChoose(input, "ipd_save", roots = volumes, session = session, restrictions = system.file(package = "base"))
  
  ##---- CPD ----
  # mapping table
  CPDMapping <- reactive({
    if (is.null(input$cpd_mapping)) {
      return(NULL)
    }
    
    inFile <- input$cpd_mapping
    
    mapping <- read.xlsx(inFile$datapath) %>% 
      setDF() %>% 
      distinct()
    
    mapping
  })
  
  # raw data
  CPDRaw <- reactive({
    if (is.integer(input$cpd_raw)) {
      return(NULL)
      
    } else {
      dir <- parseDirPath(volumes, input$cpd_raw)
      dir
    }
  })
  
  output$cpd_raw_path <- renderPrint({
    if (is.null(CPDRaw())) {
      cat("No directory has been selected. ")
      
    } else {
      CPDRaw()
    }
  })
  
  # save directory
  CPDSave <- reactive({
    if (is.integer(input$cpd_save)) {
      return(NULL)
      
    } else {
      dir <- parseDirPath(volumes, input$cpd_save)
      dir
    }
  })
  
  output$cpd_save_path <- renderPrint({
    if (is.integer(input$cpd_save)) {
      cat("No directory has been selected. ")
    } else {
      CPDSave()
    }
  })
  
  # run
  observeEvent(input$cpd_run, {
    isolate({
      withProgress(message = "Generating CPD table files...", style = "notification", value = 0, {
        setProgress(0.05)
        
        # 4BU
        MR_call_drop <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/MR_SFE_DIY_Report_sele.xlsx"))
        MR_call_undrop <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/MR_SFE_DIY_Report_raw.xlsx"))
        MR_call <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/MR_Call_DIY_Report_by_Brand_data.xlsx"))
        MR_event <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/MR_Event_DIY_Report.xlsx"))
        inventory <- read_xlsx(paste0(CPDRaw(), "/internal/4BU/Distributor_Sellout&Inventory_DIY_Report.xlsx"))
        jinyao_pre <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/Total_Market_Terminal_Sales_DIY_Report_motrinLY.xlsx")) %>% rename(CY_Ach = `CY.Ach.(Val)`)
        jinyao_current <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/Total_Market_Terminal_Sales_DIY_Report_motrinCY.xlsx")) %>% rename(CY_Ach = `CY.Ach.(Val)`)
        inter_market <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/Total_Market_Terminal_Sales_DIY_Report_data.xlsx"))
        dist <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/Coverage_DIY_Brand&Product_Report.xlsx"))
        PCMC_mapping <- read.xlsx(paste0(CPDRaw(), "/internal/4BU/N8_PCMC.xlsx"))
        brand_name_map <- read.xlsx(paste0(CPDRaw(), "/internal/brandname.xlsx"))
        
        setProgress(0.2)
        
        # RPD
        extcrstree <- read_xlsx(paste0(CPDRaw(), "/internal/RPD/ExtCRSTree.xlsx"))
        retail_all <- read_xlsx(paste0(CPDRaw(), "/internal/RPD/Retail_all.xlsx"))
        retail_direct <- read_xlsx(paste0(CPDRaw(), "/internal/RPD/Retail_direct.xlsx"))
        scope <- read_xlsx(paste0(CPDRaw(), "/internal/RPD/Total_Market_Terminal_Sales_DIY_Report_CYYTD.xlsx"), sheet = "Total_Market_Terminal_Sales_DIY")
        
        # deepdive
        ranking <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/ranking.xlsx"))
        sales_org <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/salesorg.xlsx"), sheet = "Sheet1")
        yuanbiandian <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/dah.xlsx"))
        sellout <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/Total_Market_Terminal_Sales_DIY_Report_deepdive.xlsx"), sheet = "sellout")
        region_RSM <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/Total_Market_Terminal_Sales_DIY_Report_deepdive.xlsx"), sheet = "BY RSM")
        frequency <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/MR_Call_DIY_Report_by_Brand_data_NS.xlsx"), sheet = "MR_Call_DIY_Report_by_Brand（医院团")
        meeting <- read.xlsx(paste0(CPDRaw(), "/internal/deepdive/MR_Event_DIY_Report_NS.xlsx"), sheet = "MR_Event_DIY_Report（医院团队会议透视报告）")
        
        # hos
        exter_hos <- read.xlsx(paste0(CPDRaw(), "/external/hos/QueryResult.xlsx"), sheet = "Result Table", startRow = 15, colNames = TRUE)
        lookup <- read.xlsx(paste0(CPDRaw(), "/external/hos/lookup.xlsx"))
        
        # retail
        retail_mkt_motrin <- read.xlsx(paste0(CPDRaw(), "/external/retail/Pain PED.xlsx"))
        retail_mkt_rhinocort <- read.xlsx(paste0(CPDRaw(), "/external/retail/AA Topical.xlsx"))
        retail_mkt_daktarin <- read.xlsx(paste0(CPDRaw(), "/external/retail/AF.xlsx"))
        retail_mkt_motilium <- read.xlsx(paste0(CPDRaw(), "/external/retail/GI.xlsx"))
        
        setProgress(0.5)
        
        # overview
        CPD_overview_function(save_location = CPDSave(),
                              mapping_tbl = CPDMapping(),
                              retail_mkt_daktarin = retail_mkt_daktarin,
                              inter_market = inter_market,
                              lookup = lookup,
                              exter_hos = exter_hos,
                              cpd_f3_time = cpd_f3_time,
                              retail_mkt_motrin = retail_mkt_motrin,
                              retail_mkt_motilium = retail_mkt_motilium,
                              retail_mkt_rhinocort = retail_mkt_rhinocort,
                              inventory = inventory,
                              PCMC_mapping = PCMC_mapping,
                              MR_call_drop = MR_call_drop,
                              MR_call_undrop = MR_call_undrop,
                              MR_call = MR_call)
        
        setProgress(0.6)
        
        # mainbody
        excluding_page <- paste(c("f13", "f22", "f31", "f40", "f52", "f53"), collapse = "|")
        
        final_page_mainbody <- CPDMapping() %>%
          filter(Category == "Mainbody") %>%
          select(Page) %>%
          distinct() %>%
          unlist() %>%
          unname()
        
        final_page_mainbody <- final_page_mainbody[!is.na(final_page_mainbody)]
        final_page_mainbody <- c(final_page_mainbody[!grepl(excluding_page, final_page_mainbody)],
                                 "cpd_f52_1_1", "cpd_f52_1_2", "cpd_f53_1_1", "cpd_f53_1_2")
        
        map(final_page_mainbody, CPD_mainbody_generator,
            save_location = CPDSave(),
            mapping_tbl = CPDMapping(),
            inter_market = inter_market,
            exter_hos = exter_hos,
            PCMC_mapping = PCMC_mapping,
            brand_name_map = brand_name_map,
            MR_call = MR_call,
            MR_event = MR_event,
            jinyao_pre = jinyao_pre,
            jinyao_current = jinyao_current,
            dist = dist,
            inventory = inventory,
            retail_mkt_daktarin = retail_mkt_daktarin)
        
        setProgress(0.8)
        
        # RPD
        CPD_RPD_generator(save_location = CPDSave(),
                          mapping_tbl = CPDMapping(),
                          inter_market = inter_market,
                          scope = scope,
                          retail_direct = retail_direct,
                          extcrstree = extcrstree,
                          retail_all = retail_all,
                          PCMC_mapping = PCMC_mapping)
        
        # p13
        CPD_Sellout_by_channel_by_model_generator(save_location = CPDSave(),
                                                  mapping_tbl = CPDMapping(),
                                                  inter_market = inter_market,
                                                  PCMC_mapping = PCMC_mapping)
        
        # p52-53
        CPD_Daktarin_family_generator(save_location = CPDSave(),
                                      mapping_tbl = CPDMapping(),
                                      inter_market = inter_market,
                                      PCMC_mapping = PCMC_mapping,
                                      retail_mkt_daktarin = retail_mkt_daktarin,
                                      dist = dist)
        
        setProgress(0.9)
        
        # delete tmp
        files_list <- list.files(CPDSave())
        all_csv_files <- files_list[grepl("csv|tmp", files_list)]
        file.remove(paste(CPDSave(), "/", all_csv_files, sep = ""))
        
        setProgress(1)
      })
    })
  })
  
  
  ##---- IPD ----
  # mapping table
  IPDMapping <- reactive({
    if (is.null(input$ipd_mapping)) {
      return(NULL)
    }
    
    inFile <- input$ipd_mapping
    
    mapping <- read.xlsx(inFile$datapath) %>% 
      setDF() %>% 
      distinct()
    
    mapping
  })
  
  # raw data
  IPDRaw <- reactive({
    if (is.integer(input$ipd_raw)) {
      return(NULL)
      
    } else {
      dir <- parseDirPath(volumes, input$ipd_raw)
      dir
    }
  })
  
  output$ipd_raw_path <- renderPrint({
    if (is.null(IPDRaw())) {
      cat("No directory has been selected. ")
      
    } else {
      IPDRaw()
    }
  })
  
  # save directory
  IPDSave <- reactive({
    if (is.integer(input$ipd_save)) {
      return(NULL)
      
    } else {
      dir <- parseDirPath(volumes, input$ipd_save)
      dir
    }
  })
  
  output$ipd_save_path <- renderPrint({
    if (is.integer(input$ipd_save)) {
      cat("No directory has been selected. ")
      
    } else {
      IPDSave()
    }
  })
  
  # run
  observeEvent(input$ipd_run, {
    isolate({
      withProgress(message = "Generating IPD table files...", style = "notification", value = 0, {
        setProgress(0.05)
        
        # IPD
        sales.month <- read.xlsx(paste0(IPDRaw(), "/IPD/Total_Market_Terminal_Sales_DIY_Report_Total_Sellout_by_month.xlsx"))
        sales.month.add <- read.xlsx(paste0(IPDRaw(), "/IPD/NTS_DIY_Query_Report_Actelion_NTS_by_month.xlsx"))
        call.report <- read.xlsx(paste0(IPDRaw(), "/IPD/MR_Call_DIY_Report_by_Brand_IPD.xlsx"))
        ipd.sales <- read.xlsx(paste0(IPDRaw(), "/IPD/Total_Market_Terminal_Sales_DIY_Report_IPD_by_potential.xlsx"))
        ipd.org <- read.xlsx(paste0(IPDRaw(), "/IPD/org.xlsx"))
        ipd.ybd <- read.xlsx(paste0(IPDRaw(), "/IPD/dah.xlsx"))
        market <- read.xlsx(paste0(IPDRaw(), "/IPD/Market_Share_by_potential.xlsx"), startRow = 12)
        market.ytd <- read.xlsx(paste0(IPDRaw(), "/IPD/Market_Share_IPD_YTD_current.xlsx"), startRow = 12)
        market.ytd.p <- read.xlsx(paste0(IPDRaw(), "/IPD/Market_Share_IPD_YTD_past.xlsx"), startRow = 12)
        event.report <- read.xlsx(paste0(IPDRaw(), "/IPD/MR_Event_DIY_Report_IPD.xlsx"))
        hcp.profile <- read.xlsx(paste0(IPDRaw(), "/IPD/MR_HCP_Profile_DIY_Report_IPD.xlsx"))
        
        # launch
        sales_perf <- read.xlsx(paste0(IPDRaw(), "/launch/Sales_Performance.xlsx"))
        call_record <- read.xlsx(paste0(IPDRaw(), "/launch/Call.xlsx")) %>% filter(!(E8 %in% c("TBA")))
        HCP_profile <- read.xlsx(paste0(IPDRaw(), "/launch/HCP_Profile.xlsx"))
        event_record <- read.xlsx(paste0(IPDRaw(), "/launch/Events.xlsx"))
        
        setProgress(0.3)
        
        # preset
        brand.mapping <- data.frame(brand = c("Velcade", "Dara", "Imbruvica", "Dacogen", "Zytiga", "Remicade", "Simponi"),
                                    brand_cn = c("万珂", "兆珂", "亿珂", "达珂", "泽珂", "类克", "欣普尼"))
        
        brand.page <- data.frame(brand = c("Velcade", "Imbruvica", "Remicade", "Simponi", "Zytiga"),
                                 page_code1 = c(5, 8, 11, 16, 19),
                                 page_code2 = c(6, 9, 18, 18, 20),
                                 page_code3 = c(7, 10, 12, 17, 21))
        
        page1 <- 2
        page2 <- c(4, 5, 6, 7, 8)
        page3 <- c(9, 10, 11, 12)
        
        # format
        market.ytd.fmt <- market.ytd %>% 
          filter(KPI %in% c("EI", "Volume ('000)")) %>% 
          mutate(value = as.numeric(Value),
                 brand = stri_sub(Product, 1, -4),
                 brand = trimws(brand, "right"),
                 Month = stri_pad_left(Month, 2, 0)) %>% 
          unite("date", Year, Month, sep = "") %>% 
          setDT() %>% 
          dcast(brand + date ~ KPI, fun.aggregate = sum, value.var = "value") %>% 
          group_by(brand, date) %>% 
          summarise(EI = sum(EI, na.rm = TRUE),
                    sales = sum(`Volume ('000)`, na.rm = TRUE)) %>% 
          ungroup()
        
        market.ytd.p.fmt <- market.ytd.p %>% 
          filter(KPI %in% c("EI", "Volume ('000)")) %>% 
          mutate(value = as.numeric(Value),
                 brand = stri_sub(Product, 1, -4),
                 brand = trimws(brand, "right")) %>% 
          setDT() %>% 
          dcast(brand ~ KPI, fun.aggregate = sum, value.var = "value") %>% 
          group_by(brand) %>% 
          summarise(EI_p = sum(EI, na.rm = TRUE),
                    sales_p = sum(`Volume ('000)`, na.rm = TRUE)) %>% 
          ungroup()
        
        ims.market <- left_join(market.ytd.fmt, market.ytd.p.fmt, by = "brand")
        
        sales.month.fmt <- sales.month %>% 
          select("month" = "Year/Month",
                 "franchise" = "Franchise",
                 "brand" = "品牌（Brand)",
                 "model" = "Model",
                 "sales" = "本年销售金额（Sales_AMT)",
                 "target" = "本年指标金额（Target.AMT)",
                 "sales_p" = "去年销售金额（Last.Year.Sales.AMT)") %>% 
          filter(month != "Summary")
        
        sales.month.add.fmt <- sales.month.add %>% 
          select("month" = "Year/Month",
                 "brand" = "Brand",
                 "sales" = "NTS",
                 "target" = "FBP",
                 "sales_p" = "Last.Year.NTS") %>% 
          filter(month != "Summary") %>% 
          distinct() %>% 
          mutate(month = as.Date(as.numeric(month), origin = "1899-12-30"),
                 month = gsub("-", "", month),
                 month = stri_sub(month, 1, 6),
                 franchise = "Actelion",
                 model = "DC")
        
        call.fmt <- call.report %>% 
          select("month" = "Year/Month",
                 "brand" = "Brand",
                 "hosp_code" = "Hospital.Code",
                 "hospital" = "Hospital",
                 "hcp_level" = "HCP.Level",
                 "pro_call" = "professionalcall",
                 "visit_doc" = "visiteddoctor",
                 "doctor" = "Doctor") %>% 
          filter(month != "Total") %>% 
          mutate(hcp_level = stri_sub(hcp_level, 1, 1)) %>% 
          filter(hcp_level %in% c("A", "B")) %>% 
          group_by(brand, month, hcp_level) %>% 
          summarise(pro_call = sum(pro_call, na.rm = TRUE),
                    visit_doc = sum(visit_doc, na.rm = TRUE),
                    doctor = sum(doctor, na.rm = TRUE)) %>% 
          ungroup() %>% 
          setDT() %>% 
          melt(id.vars = c("month", "brand", "hcp_level"), 
               measure.vars = c("pro_call", "visit_doc", "doctor")) %>% 
          unite("type", variable, hcp_level) %>% 
          dcast(month + brand ~ type, fill = 0)
        
        ipd.sales.fmt <- ipd.sales %>% 
          select("month" = "Year/Month",
                 "brand" = "品牌（Brand)",
                 "province" = "行政省名称（Province.Name)",
                 "city" = "城市名称（City.Name)",
                 "tml_code" = "Terminal.Code",
                 "sales" = "本年销售金额（Sales_AMT)",
                 "target" = "本年指标金额（Target.AMT)",
                 "sales_p" = "去年销售金额（Last.Year.Sales.AMT)") %>% 
          left_join(brand.mapping, by = "brand")
        
        ipd.org.fmt <- ipd.org %>% 
          distinct(tml_code = ORGID, terminal = ORGNAME, brand_cn = BRANDNAME, decile = Decile) %>% 
          mutate(decile = ifelse(decile %in% c("10", "9", "8"), "Decile 10-8", 
                                 ifelse(decile %in% c("7", "6", "5"), "Decile 7-5", 
                                        ifelse(decile %in% c("4", "3", "2", "1"), "Decile 4-1", 
                                               decile))),
                 ta = stri_sub(tml_code, 1, 1),
                 decile = ifelse(ta == "F", "RA/AS", 
                                 ifelse(ta == "E", "IBD", 
                                        ifelse(ta == "P", "PSO", 
                                               decile))))
        
        ipd.ybd.fmt <- ipd.ybd %>% 
          distinct(tml_code = `药店编码`,
                   brand_cn = `品牌`,
                   hosp_code = `对应医院编码`)
        
        total.trend <- ipd.org.fmt %>% 
          left_join(ipd.ybd.fmt, by = c("tml_code", "brand_cn")) %>% 
          right_join(ipd.sales.fmt, by = c("tml_code", "brand_cn")) %>% 
          mutate(hosp_code = ifelse(is.na(hosp_code), tml_code, hosp_code),
                 month = as.character(month)) %>% 
          group_by(brand, decile, month) %>% 
          summarise(sales = sum(sales, na.rm = TRUE),
                    target = sum(target, na.rm = TRUE),
                    sales_p = sum(sales_p, na.rm = TRUE)) %>% 
          ungroup()
        
        market.fmt <- market %>% 
          mutate(`Defined.Market` = ifelse(`Defined.Market` == "Remicade/Simponi", "Remicade", `Defined.Market`)) %>% 
          bind_rows(filter(market, `Defined.Market` == "Remicade/Simponi")) %>% 
          mutate(`Defined.Market` = ifelse(`Defined.Market` == "Remicade/Simponi", "Simponi", `Defined.Market`),
                 Month = stri_pad_left(Month, 2, 0)) %>% 
          unite("month", Year, Month, sep = "") %>% 
          left_join(ipd.org.fmt[, c("tml_code", "decile")], by = c("BI.Code" = "tml_code")) %>% 
          group_by(brand = `Defined.Market`, decile, month) %>% 
          summarise(market_size = sum(Value, na.rm = TRUE) * 1000) %>% 
          ungroup()
        
        hcp.fmt <- hcp.profile %>% 
          select("hcp_code" = "HCP.Code",
                 "hcp_level" = "HCP.Level",
                 "hosp_code" = "Hospital.Code",
                 "decile" = "Decile") %>% 
          filter(!is.na(hcp_code), stri_sub(hcp_level, 1, 1) %in% c("A", "B")) %>% 
          distinct() %>% 
          mutate(decile = ifelse(decile %in% c("10", "9", "8"), "Decile 10-8", 
                                 ifelse(decile %in% c("7", "6", "5"), "Decile 7-5", 
                                        ifelse(decile %in% c("4", "3", "2", "1"), "Decile 4-1", 
                                               decile))),
                 ta = stri_sub(hosp_code, 1, 1),
                 decile = ifelse(ta == "F", "RA/AS", 
                                 ifelse(ta == "E", "IBD", 
                                        ifelse(ta == "P", "PSO", 
                                               decile)))) %>% 
          group_by(decile) %>% 
          summarise(hcp_AB = n()) %>% 
          ungroup()
        
        event.fmt <- event.report %>% 
          mutate(Hcp.Code = as.numeric(Hcp.Code)) %>% 
          left_join(hcp.profile[, c("HCP.Code", "HCP.Level")], by = c("Hcp.Code" = "HCP.Code")) %>% 
          mutate(Hcp.Level = if_else(is.na(Hcp.Level), HCP.Level, Hcp.Level)) %>% 
          select("month" = "Year/Month",
                 "brand_cn" = "Brand",
                 "hcp_code" = "Hcp.Code",
                 "hcp_level" = "Hcp.Level",
                 "ta" = "Ta",
                 "decile" = "Decile") %>% 
          left_join(brand.mapping, by = "brand_cn") %>% 
          filter(!is.na(month), !is.na(brand), !is.na(hcp_code), stri_sub(hcp_level, 1, 1) %in% c("A", "B")) %>%
          distinct() %>% 
          mutate(decile = ifelse(decile %in% c("10", "9", "8"), "Decile 10-8", 
                                 ifelse(decile %in% c("7", "6", "5"), "Decile 7-5", 
                                        ifelse(decile %in% c("4", "3", "2", "1"), "Decile 4-1", 
                                               decile))),
                 ta = stri_sub(ta, -2, -1),
                 decile = ifelse(ta == "风湿", "RA/AS", 
                                 ifelse(ta == "消化", "IBD", 
                                        ifelse(ta == "皮科", "PSO", 
                                               decile))),
                 month = as.numeric(month)) %>% 
          group_by(month, brand, decile) %>% 
          summarise(event_AB = n()) %>% 
          ungroup()
        
        event.coverage <- event.fmt %>% 
          left_join(hcp.fmt, by = "decile") %>% 
          group_by(brand, decile, month) %>% 
          summarise(event_AB = sum(event_AB, na.rm = TRUE),
                    hcp_AB = sum(hcp_AB, na.rm = TRUE)) %>% 
          ungroup() %>% 
          select(brand, decile, month, event_AB, hcp_AB)
        
        freq.coverage <- call.report %>% 
          select("month" = "Year/Month",
                 "brand" = "Brand",
                 "hosp_code" = "Hospital.Code",
                 "hospital" = "Hospital",
                 "hcp_level" = "HCP.Level",
                 "pro_call" = "professionalcall",
                 "visit_doc" = "visiteddoctor",
                 "doctor" = "Doctor") %>% 
          filter(month != "Total") %>% 
          left_join(ipd.org.fmt[, c("tml_code", "decile")], by = c("hosp_code" = "tml_code")) %>% 
          mutate(decile = ifelse(decile %in% c("10", "9", "8"), "Decile 10-8",
                                 ifelse(decile %in% c("7", "6", "5"), "Decile 7-5",
                                        ifelse(decile %in% c("4", "3", "2", "1"), "Decile 4-1",
                                               decile))),
                 ta = stri_sub(hosp_code, 1, 1),
                 decile = ifelse(ta == "F", "RA/AS", 
                                 ifelse(ta == "E", "IBD", 
                                        ifelse(ta == "P", "PSO", 
                                               decile)))) %>%
          mutate(month = as.numeric(month),
                 hcp_level = stri_sub(hcp_level, 1, 1)) %>% 
          filter(hcp_level %in% c("A", "B")) %>% 
          group_by(brand, decile, month, hcp_level) %>% 
          summarise(pro_call = sum(pro_call, na.rm = TRUE),
                    visit_doc = sum(visit_doc, na.rm = TRUE),
                    doctor = sum(doctor, na.rm = TRUE)) %>% 
          ungroup() %>% 
          setDT() %>% 
          melt(id.vars = c("brand", "decile", "month", "hcp_level"), 
               measure.vars = c("pro_call", "visit_doc", "doctor")) %>% 
          unite("type", variable, hcp_level) %>% 
          dcast(brand + decile + month ~ type, fill = 0) %>% 
          left_join(event.coverage, by = c("brand", "decile", "month"))
        
        sales.perf.fmt <- sales_perf %>% 
          select("month" = "Year/Month",
                 "brand" = "品牌（Brand)",
                 "model" = "Model",
                 "region" = "N4",
                 "type" = "Type",
                 "sales" = "本年销售金额（Sales_AMT)",
                 "target" = "本年指标金额（Target.AMT)",
                 "sales_p" = "去年销售金额（Last.Year.Sales.AMT)") %>% 
          group_by(brand, month = as.character(month), model, region, type) %>% 
          summarise(sales = sum(sales, na.rm = TRUE),
                    target = sum(target, na.rm = TRUE),
                    sales_p = sum(sales_p, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(region = stri_sub(stri_replace_all_fixed(region, "-", ""), -3, -1))
        
        setProgress(0.5)
        
        # overview
        ChanPerfFunc(page = 3, 
                     save_location = IPDSave(), 
                     sales.data = sales.month.fmt, 
                     sales.data.add = sales.month.add.fmt, 
                     mapping = IPDMapping())
        
        MarketPerfFunc(page = 4, 
                       save_location = IPDSave(), 
                       sales.data = sales.month.fmt, 
                       sales.data.add = sales.month.add.fmt, 
                       ims.market.data = ims.market, 
                       mapping = IPDMapping())
        
        setProgress(0.6)
        
        # part1
        map(brand.page$page_code1, SalesPerfFunc, 
            save_location = IPDSave(), 
            sales.data = sales.month.fmt, 
            mapping = IPDMapping())
        
        # part2
        map(brand.page$page_code2, CallFunc, 
            save_location = IPDSave(), 
            sales.data = sales.month.fmt, 
            call.data = call.fmt, 
            mapping = IPDMapping())
        
        # part3
        map(brand.page$page_code3, SalesPerfSegFunc, 
            save_location = IPDSave(), 
            sales.data = total.trend, 
            market.data = market.fmt, 
            freq.data = freq.coverage, 
            mapping = IPDMapping())
        
        setProgress(0.8)
        
        # launch
        KeyPerfFunc(page = 2, 
                    save_location = IPDSave(), 
                    sales.data = sales.perf.fmt, 
                    mapping = IPDMapping())
        
        map(page2, KeyPerfRegionFunc, 
            save_location = IPDSave(), 
            sales.data = sales.perf.fmt, 
            mapping = IPDMapping())
        
        HCPFunc(page = page3, 
                save_location = IPDSave(), 
                hcp = HCP_profile, 
                call = call_record, 
                event = event_record, 
                mapping = IPDMapping())
        
        setProgress(0.9)
        
        # delete tmp
        files_list <- list.files(IPDSave())
        all_csv_files <- files_list[grepl("csv|tmp", files_list)]
        file.remove(paste(IPDSave(), "/", all_csv_files, sep = ""))
        
        setProgress(1)
      })
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
}