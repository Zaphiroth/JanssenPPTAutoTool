# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen Dashboard
# Purpose:      Function
# programmer:   Zhe Liu
# Date:         27-12-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


HCPFunc <- function(page, 
                    save_location, 
                    hcp = HCP_profile, 
                    call = call_record, 
                    event = event_record, 
                    mapping = mapping.table) {
  
  # page info
  page <- sort(page)
  
  position1.1 <- mapping %>% 
    filter(Category == "Launch", Page == page[1], Position == 1) %>% 
    distinct()
  
  position1.2 <- mapping %>% 
    filter(Category == "Launch", Page == page[1], Position == 2) %>% 
    distinct()
  
  position2.1 <- mapping %>% 
    filter(Category == "Launch", Page == page[2], Position == 1) %>% 
    distinct()
  
  position2.2 <- mapping %>% 
    filter(Category == "Launch", Page == page[2], Position == 2) %>% 
    distinct()
  
  position3.1 <- mapping %>% 
    filter(Category == "Launch", Page == page[3], Position == 1) %>% 
    distinct()
  
  position3.2 <- mapping %>% 
    filter(Category == "Launch", Page == page[3], Position == 2) %>% 
    distinct()
  
  position4.1 <- mapping %>% 
    filter(Category == "Launch", Page == page[4], Position == 1) %>% 
    distinct()
  
  position4.2 <- mapping %>% 
    filter(Category == "Launch", Page == page[4], Position == 2) %>% 
    distinct()
  
  #################### coverage by level include TBA ####################
  ##-- U: not count
  vars <- c("Doctor Segment", "Target HCP#", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  
  target_hcp_withoutAB <- hcp %>% 
    filter(HCP.Level != "U") %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_total <- hcp %>%  
    filter(HCP.Level != "U") %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(`Target HCP#` = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  target_hcp <- target_hcp_AB %>% 
    full_join(target_hcp_withoutAB) %>% 
    full_join(target_hcp_total)
  
  ##-- call coverage by level
  call_coverage_withoutAB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  
  call_coverage_AB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  call_coverage_total <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T)) %>% 
    mutate(`Doctor Segment` = "Total")
  
  
  call_coverage <- target_hcp %>% 
    left_join(call_coverage_AB %>% 
                full_join(call_coverage_withoutAB) %>% 
                full_join(call_coverage_total),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = c(0.9, NA, NA, NA, NA)) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Call Coverage (TBA included)")
  
  ##-- event coverage by level: without AB
  vars <- c("Doctor Segment", "Dara SEED", "Dara STAR", "Dara SUPER")
  hcp_event_by_level_withoutAB <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_withoutAB <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_withoutAB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## with AB
  hcp_event_by_level_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`, Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_AB <- event %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A", "B"), "A&B", Hcp.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`, Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_AB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## total
  hcp_event_by_level_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)
  
  event_by_level_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)%>% 
    left_join(hcp_event_by_level_ttl, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ##-- combine them together: is there a better way to deal with the prob.?
  
  MR_event_act_withoutAB <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n())
  
  MR_hcp_act_withoutAB <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_withoutAB, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_AB <- event %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A", "B"), "A&B", Hcp.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) 
  
  MR_hcp_act_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_AB, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  MR_hcp_act_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    left_join(MR_event_act_ttl, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  vars <- c("Doctor Segment", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  MR_event_coverage <- MR_hcp_act_AB %>% 
    full_join(MR_hcp_act_withoutAB) %>% 
    full_join(MR_hcp_act_ttl) %>% 
    left_join(event_by_level_AB %>% 
                full_join(event_by_level_withoutAB) %>% 
                full_join(event_by_level_ttl),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = NA) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "MR Event Coverage*")
  
  #-- set level
  level <- position1.1$Display_name
  
  HCP_coverage_by_level <- call_coverage %>% 
    bind_rows(MR_event_coverage) %>% 
    mutate(` ` = if_else(variable == "Target HCP#", "Target HCP#", ` `),
           `Doctor Segment` = factor(`Doctor Segment`, levels = level),
           ` ` = factor(` `, levels = c("Target HCP#", "Call Coverage (TBA included)", "MR Event Coverage*"))) %>% 
    arrange(`Doctor Segment`) %>% 
    setDT() %>% 
    dcast(`Doctor Segment` + ` ` ~ variable)
  
  HCP_coverage_by_level <- tabular((`Doctor Segment`) * Heading() ~ ` ` * (`Target HCP#` + `Target` + `Actual` + `Dara SUPER` + `Dara STAR` + `Dara SEED`) * Heading() * (identity), 
                                   data = HCP_coverage_by_level)
  HCP_coverage_by_level <- HCP_coverage_by_level[, -c(2, 3, 4, 5, 6, 7, 13, 14)]
  
  write.table.tabular(HCP_coverage_by_level, paste0(save_location, "/", position1.1$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position1.1$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position1.1$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### coverage by region include TBA ####################
  level <- position1.2$Display_name
  
  call_coverage_by_region_ttl <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(`Contact call coverage%` = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    spread(HCP.Level_m, `Contact call coverage%`) %>% 
    mutate(Region = "Total")
  
  call_coverage_by_region <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(`Contact call coverage%` = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    ungroup() %>% 
    spread(HCP.Level_m, `Contact call coverage%`) %>% 
    full_join(call_coverage_by_region_ttl) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "Contact Call Coverage% (TBA included)")
  
  hcp_event_coverage_by_region_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  hcp_event_coverage_by_region <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    full_join(hcp_event_coverage_by_region_ttl) %>% 
    ungroup(Region) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region)
  
  event_coverage_by_region_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(Hcp.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  event_coverage_by_region <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Region, Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    ungroup() %>% 
    spread(Hcp.Level_m, ttl) %>% 
    full_join(event_coverage_by_region_ttl) %>% 
    left_join(hcp_event_coverage_by_region, by = "Region") %>% 
    mutate(A = A.x/A.y,
           B = B.x/B.y,
           C = C.x/C.y) %>% 
    select(Region, A, B, C) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "MR Event Coverage%*")
  
  coverage_by_region <- call_coverage_by_region %>% 
    bind_rows(event_coverage_by_region) %>% 
    mutate(Region = factor(Region, levels = level),
           ` ` = factor(` `, levels = c("Contact Call Coverage% (TBA included)", "MR Event Coverage%*")))
  
  coverage_by_region <- tabular((Region) * Heading() ~ ` ` * (A + B + C) * Heading() * (identity), 
                                data = coverage_by_region)
  
  write.table.tabular(coverage_by_region, paste0(save_location, "/", position1.2$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position1.2$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position1.2$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### frequency by level include TBA(without AB)#####################
  vars <- c("Doctor Segment", "Target HCP#", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  
  target_hcp_withoutAB_freq <- hcp %>% 
    filter(HCP.Level != "U") %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_total_freq <- hcp %>%  
    filter(HCP.Level != "U") %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(`Target HCP#` = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  target_hcp_freq <- target_hcp_withoutAB_freq %>% 
    full_join(target_hcp_total_freq)
  
  ##-- call frequency by level
  call_freq_withoutAB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(professionalcall[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(professionalcall[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(professionalcall[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  call_freq_total <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    summarise(Actual = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(professionalcall[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(professionalcall[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(professionalcall[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T)) %>% 
    mutate(`Doctor Segment` = "Total")
  
  
  call_freq <- target_hcp_freq %>% 
    left_join(call_freq_withoutAB %>% 
                full_join(call_freq_total),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = c(4, 2, NA, NA)) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Avg. Monthly Call freq. (TBA included)")
  
  ##-- event frequency by level: without AB
  vars <- c("Doctor Segment", "Dara SUPER", "Dara STAR", "Dara SEED")
  hcp_event_by_level_withoutAB_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_withoutAB_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_withoutAB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## total
  hcp_event_by_level_ttl_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)
  
  event_by_level_ttl_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)%>% 
    left_join(hcp_event_by_level_ttl, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ##-- combine them together: is there a better way to deal with the prob.?
  
  MR_event_act_withoutAB_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n())
  
  MR_hcp_act_withoutAB_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_withoutAB_freq, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_ttl_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  MR_hcp_act_ttl_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    left_join(MR_event_act_ttl_freq, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_freq <- MR_hcp_act_withoutAB_freq %>% 
    full_join(MR_hcp_act_ttl_freq) %>% 
    left_join(event_by_level_withoutAB_freq %>% 
                full_join(event_by_level_ttl_freq),
              by = "Doctor Segment") %>% 
    mutate(Target = NA) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Avg. Event freq.")
  
  #-- set level
  level <- position3.1$Display_name
  
  HCP_freq_by_level <- call_freq %>% 
    bind_rows(MR_event_freq) %>% 
    mutate(` ` = if_else(variable == "Target HCP#", "Target HCP#", ` `),
           `Doctor Segment` = factor(`Doctor Segment`, levels = level),
           ` ` = factor(` `, levels = c("Target HCP#", "Avg. Monthly Call freq. (TBA included)", "Avg. Event freq."))) %>% 
    arrange(`Doctor Segment`) %>% 
    setDT() %>% 
    dcast(`Doctor Segment` + ` ` ~ variable)
  
  HCP_freq_by_level <- tabular((`Doctor Segment`) * Heading() ~ ` ` * (`Target HCP#` + `Target` + `Actual` + `Dara SUPER` + `Dara STAR` + `Dara SEED`) * Heading() * (identity), 
                               data = HCP_freq_by_level)
  HCP_freq_by_level <- HCP_freq_by_level[, -c(2, 3, 4, 5, 6, 7, 13, 14)]
  
  write.table.tabular(HCP_freq_by_level, paste0(save_location, "/", position3.1$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position3.1$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position3.1$Filename[1], ".xlsx"), colNames = FALSE)  
  
  
  #################### frequency by region include TBA(without AB) #################### 
  call_freq_by_region_ttl <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(`Contact call frequency%` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    spread(HCP.Level_m, `Contact call frequency%`) %>% 
    mutate(Region = "Total")
  
  call_freq_by_region <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(`Contact call frequency%` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    ungroup() %>% 
    spread(HCP.Level_m, `Contact call frequency%`) %>% 
    full_join(call_freq_by_region_ttl) %>% 
    mutate(` ` = "Contact Call Frequency (TBA included)")
  
  hcp_event_freq_by_region_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by( HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  hcp_event_freq_by_region <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    full_join(hcp_event_freq_by_region_ttl)
  
  event_freq_by_regiont_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(Hcp.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  event_freq_by_region <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Region, Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    ungroup() %>% 
    spread(Hcp.Level_m, ttl) %>% 
    full_join(event_freq_by_regiont_ttl) %>% 
    left_join(hcp_event_coverage_by_region, by = "Region") %>% 
    mutate(A = A.x/A.y,
           B = B.x/B.y,
           C = C.x/C.y) %>% 
    select(Region, A, B, C) %>% 
    mutate(` ` = "MR Event Frequency")
  
  level <- position3.2$Display_name
  
  freq_by_region <- call_freq_by_region%>% 
    bind_rows(event_freq_by_region) %>% 
    mutate(Region = factor(Region, levels = level),
           ` ` = factor(` `, levels = c("Contact Call Frequency (TBA included)", "MR Event Frequency")))
  
  freq_by_region <- tabular((Region) * Heading() ~ ` ` * (A + B + C) * Heading() * (identity), 
                            data = freq_by_region)
  
  write.table.tabular(freq_by_region, paste0(save_location, "/", position3.2$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position3.2$Filename[1], ".csv"))
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position3.2$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### coverage by level exclude TBA ####################
  ##-- U: not count
  vars <- c("Doctor Segment", "Target HCP#", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  
  target_hcp_withoutAB <- hcp %>% 
    filter(HCP.Level != "U") %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_total <- hcp %>%  
    filter(HCP.Level != "U") %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(`Target HCP#` = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  target_hcp <- target_hcp_AB %>% 
    full_join(target_hcp_withoutAB) %>% 
    full_join(target_hcp_total)
  
  ##-- call coverage by level
  call_coverage_withoutAB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  
  call_coverage_AB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  call_coverage_total <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    summarise(Actual = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(visiteddoctor[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(visiteddoctor[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(visiteddoctor[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T)) %>% 
    mutate(`Doctor Segment` = "Total")
  
  
  call_coverage <- target_hcp %>% 
    left_join(call_coverage_AB %>% 
                full_join(call_coverage_withoutAB) %>% 
                full_join(call_coverage_total),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = c(0.9, NA, NA, NA, NA)) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Call Coverage (TBA excluded)")
  
  ##-- event coverage by level: without AB
  vars <- c("Doctor Segment", "Dara SEED", "Dara STAR", "Dara SUPER")
  hcp_event_by_level_withoutAB <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_withoutAB <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_withoutAB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## with AB
  hcp_event_by_level_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`, Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_AB <- event %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A", "B"), "A&B", Hcp.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`, Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_AB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## total
  hcp_event_by_level_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)
  
  event_by_level_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)%>% 
    left_join(hcp_event_by_level_ttl, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ##-- combine them together: is there a better way to deal with the prob.?
  
  MR_event_act_withoutAB <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n())
  
  MR_hcp_act_withoutAB <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_withoutAB, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_AB <- event %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A", "B"), "A&B", Hcp.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) 
  
  MR_hcp_act_AB <- hcp %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A", "B"), "A&B", HCP.Level)) %>% 
    filter(`Doctor Segment` %in% c("A&B")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_AB, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  MR_hcp_act_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    left_join(MR_event_act_ttl, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  vars <- c("Doctor Segment", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  MR_event_coverage <- MR_hcp_act_AB %>% 
    full_join(MR_hcp_act_withoutAB) %>% 
    full_join(MR_hcp_act_ttl) %>% 
    left_join(event_by_level_AB %>% 
                full_join(event_by_level_withoutAB) %>% 
                full_join(event_by_level_ttl),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = NA) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "MR Event Coverage*")
  
  #-- set level
  level <- position2.1$Display_name
  
  HCP_coverage_by_level <- call_coverage %>% 
    bind_rows(MR_event_coverage) %>% 
    mutate(` ` = if_else(variable == "Target HCP#", "Target HCP#", ` `),
           `Doctor Segment` = factor(`Doctor Segment`, levels = level),
           ` ` = factor(` `, levels = c("Target HCP#", "Call Coverage (TBA excluded)", "MR Event Coverage*"))) %>% 
    arrange(`Doctor Segment`) %>% 
    setDT() %>% 
    dcast(`Doctor Segment` + ` ` ~ variable)
  
  HCP_coverage_by_level <- tabular((`Doctor Segment`) * Heading() ~ ` ` * (`Target HCP#` + `Target` + `Actual` + `Dara SUPER` + `Dara STAR` + `Dara SEED`) * Heading() * (identity), 
                                   data = HCP_coverage_by_level)
  HCP_coverage_by_level <- HCP_coverage_by_level[, -c(2, 3, 4, 5, 6, 7, 13, 14)]
  
  write.table.tabular(HCP_coverage_by_level, paste0(save_location, "/", position2.1$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position2.1$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position2.1$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### coverage by region exclude TBA ####################
  
  level <- position2.2$Display_name
  
  call_coverage_by_region_ttl <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(`Contact call coverage%` = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    spread(HCP.Level_m, `Contact call coverage%`) %>% 
    mutate(Region = "Total")
  
  call_coverage_by_region <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(`Contact call coverage%` = sum(visiteddoctor, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    ungroup() %>% 
    spread(HCP.Level_m, `Contact call coverage%`) %>% 
    full_join(call_coverage_by_region_ttl) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "Contact Call Coverage% (TBA excluded)")
  
  hcp_event_coverage_by_region_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  hcp_event_coverage_by_region <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    full_join(hcp_event_coverage_by_region_ttl) %>% 
    ungroup(Region) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region)
  
  event_coverage_by_region_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(Hcp.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  event_coverage_by_region <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    distinct(Hcp.Code, .keep_all = T) %>% 
    group_by(Region, Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    ungroup() %>% 
    spread(Hcp.Level_m, ttl) %>% 
    full_join(event_coverage_by_region_ttl) %>% 
    left_join(hcp_event_coverage_by_region, by = "Region") %>% 
    mutate(A = A.x/A.y,
           B = B.x/B.y,
           C = C.x/C.y) %>% 
    select(Region, A, B, C) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "MR Event Coverage%*")
  
  coverage_by_region <- call_coverage_by_region %>% 
    bind_rows(event_coverage_by_region) %>% 
    mutate(Region = factor(Region, levels = level),
           ` ` = factor(` `, levels = c("Contact Call Coverage% (TBA excluded)", "MR Event Coverage%*")))
  
  coverage_by_region <- tabular((Region) * Heading() ~ ` ` * (A + B + C) * Heading() * (identity), 
                                data = coverage_by_region)
  
  write.table.tabular(coverage_by_region, paste0(save_location, "/", position2.2$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position2.2$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position2.2$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### frequency by level exclude TBA(without AB)#####################
  vars <- c("Doctor Segment", "Target HCP#", "Actual", "Dara SUPER", "Dara STAR", "Dara SEED")
  
  target_hcp_withoutAB_freq <- hcp %>% 
    filter(HCP.Level != "U") %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(`Target HCP#` = n())
  
  target_hcp_total_freq <- hcp %>%  
    filter(HCP.Level != "U") %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(`Target HCP#` = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  target_hcp_freq <- target_hcp_withoutAB_freq %>% 
    full_join(target_hcp_total_freq)
  
  ##-- call frequency by level
  call_freq_withoutAB <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(professionalcall[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(professionalcall[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(professionalcall[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T))
  
  call_freq_total <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    summarise(Actual = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T),
              `Dara SEED` = sum(professionalcall[Hospital.Type == "Dara SEED"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SEED"], na.rm = T),
              `Dara STAR` = sum(professionalcall[Hospital.Type == "Dara STAR"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara STAR"], na.rm = T),
              `Dara SUPER` = sum(professionalcall[Hospital.Type == "Dara SUPER"], na.rm = T)/sum(Doctor[Hospital.Type == "Dara SUPER"], na.rm = T)) %>% 
    mutate(`Doctor Segment` = "Total")
  
  
  call_freq <- target_hcp_freq %>% 
    left_join(call_freq_withoutAB %>% 
                full_join(call_freq_total),
              by = "Doctor Segment") %>% 
    select(vars) %>% 
    mutate(Target = c(4, 2, NA, NA)) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Avg. Monthly Call freq. (TBA excluded)")
  
  ##-- event frequency by level: without AB
  vars <- c("Doctor Segment", "Dara SUPER", "Dara STAR", "Dara SEED")
  hcp_event_by_level_withoutAB_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars)
  
  event_by_level_withoutAB_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Hospital.Type, `Doctor Segment`) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    select(vars) %>% 
    left_join(hcp_event_by_level_withoutAB, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ## total
  hcp_event_by_level_ttl_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)
  
  event_by_level_ttl_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    group_by(Hospital.Type) %>% 
    summarise(ttl = n()) %>% 
    spread(Hospital.Type, ttl) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    select(vars)%>% 
    left_join(hcp_event_by_level_ttl, by = "Doctor Segment") %>% 
    mutate(`Dara SEED` = `Dara SEED.x`/`Dara SEED.y`,
           `Dara STAR` = `Dara STAR.x`/`Dara STAR.y`,
           `Dara SUPER` = `Dara SUPER.x`/`Dara SUPER.y`) %>% 
    select(vars)
  
  ##-- combine them together: is there a better way to deal with the prob.?
  
  MR_event_act_withoutAB_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n())
  
  MR_hcp_act_withoutAB_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(`Doctor Segment` = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                     ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                            ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(`Doctor Segment`) %>% 
    summarise(Actual = n()) %>% 
    left_join(MR_event_act_withoutAB_freq, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_act_ttl_freq <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total")
  
  MR_hcp_act_ttl_freq <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    summarise(Actual = n()) %>% 
    mutate(`Doctor Segment` = "Total") %>% 
    left_join(MR_event_act_ttl_freq, by = "Doctor Segment") %>% 
    mutate(Actual = Actual.y/Actual.x) %>% 
    select(`Doctor Segment`, Actual)
  
  MR_event_freq <- MR_hcp_act_withoutAB_freq %>% 
    full_join(MR_hcp_act_ttl_freq) %>% 
    left_join(event_by_level_withoutAB_freq %>% 
                full_join(event_by_level_ttl_freq),
              by = "Doctor Segment") %>% 
    mutate(Target = NA) %>% 
    setDT() %>% 
    melt(id.vars = "Doctor Segment") %>% 
    mutate(` ` = "Avg. Event freq.")
  
  #-- set level
  level <- position4.1$Display_name
  
  HCP_freq_by_level <- call_freq %>% 
    bind_rows(MR_event_freq) %>% 
    mutate(` ` = if_else(variable == "Target HCP#", "Target HCP#", ` `),
           `Doctor Segment` = factor(`Doctor Segment`, levels = level),
           ` ` = factor(` `, levels = c("Target HCP#", "Avg. Monthly Call freq. (TBA excluded)", "Avg. Event freq."))) %>% 
    arrange(`Doctor Segment`) %>% 
    setDT() %>% 
    dcast(`Doctor Segment` + ` ` ~ variable)
  
  HCP_freq_by_level <- tabular((`Doctor Segment`) * Heading() ~ ` ` * (`Target HCP#` + `Target` + `Actual` + `Dara SUPER` + `Dara STAR` + `Dara SEED`) * Heading() * (identity), 
                               data = HCP_freq_by_level)
  HCP_freq_by_level <- HCP_freq_by_level[, -c(2, 3, 4, 5, 6, 7, 13, 14)]
  write.table.tabular(HCP_freq_by_level, paste0(save_location, "/", position4.1$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position4.1$Filename[1], ".csv"))
  tmp <- apply(tmp, 2, function(x) {
    gsub("\\", "", x, fixed = TRUE)
  })
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position4.1$Filename[1], ".xlsx"), colNames = FALSE)
  
  
  #################### frequency by region exclude TBA(without AB) #################### 
  
  level <- position4.2$Display_name
  
  call_freq_by_region_ttl <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(HCP.Level_m) %>% 
    summarise(`Contact call frequency%` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    spread(HCP.Level_m, `Contact call frequency%`) %>% 
    mutate(Region = "Total")
  
  call_freq_by_region <- call %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(`Contact call frequency%` = sum(professionalcall, na.rm = T)/sum(Doctor, na.rm = T)) %>% 
    ungroup() %>% 
    spread(HCP.Level_m, `Contact call frequency%`) %>% 
    full_join(call_freq_by_region_ttl) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "Contact Call Frequency (TBA excluded)")
  
  hcp_event_freq_by_region_ttl <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by( HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  hcp_event_freq_by_region <- hcp %>% 
    filter(N4 != "合计" & HCP.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           HCP.Level_m = ifelse(substr(`HCP.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`HCP.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`HCP.Level`, 1, 1) %in% c("C"), "C", HCP.Level)))) %>% 
    distinct(HCP.Code, .keep_all = T) %>% 
    group_by(Region, HCP.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(HCP.Level_m, ttl) %>% 
    full_join(hcp_event_freq_by_region_ttl)
  
  event_freq_by_regiont_ttl <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    spread(Hcp.Level_m, ttl) %>% 
    mutate(Region = "Total")
  
  event_freq_by_region <- event %>% 
    filter(N4 != "合计" & Hcp.Level != c("U")) %>% 
    mutate(Region = ifelse(!is.na(N4), stri_sub(stri_replace_all_fixed(N4, "-", ""), -3, -1), N4),
           Hcp.Level_m = ifelse(substr(`Hcp.Level`, 1, 1) %in% c("A"), "A",
                                ifelse(substr(`Hcp.Level`, 1, 1) %in% c("B"), "B",
                                       ifelse(substr(`Hcp.Level`, 1, 1) %in% c("C"), "C", Hcp.Level)))) %>% 
    group_by(Region, Hcp.Level_m) %>% 
    summarise(ttl = n()) %>% 
    ungroup() %>% 
    spread(Hcp.Level_m, ttl) %>% 
    full_join(event_freq_by_regiont_ttl) %>% 
    left_join(hcp_event_coverage_by_region, by = "Region") %>% 
    mutate(A = A.x/A.y,
           B = B.x/B.y,
           C = C.x/C.y) %>% 
    select(Region, A, B, C) %>% 
    mutate(Region = factor(Region, levels = level)) %>% 
    arrange(Region) %>% 
    mutate(` ` = "MR Event Frequency")
  
  freq_by_region <- call_freq_by_region%>% 
    bind_rows(event_freq_by_region) %>% 
    mutate(Region = factor(Region, levels = level),
           ` ` = factor(` `, levels = c("Contact Call Frequency (TBA excluded)", "MR Event Frequency")))
  
  freq_by_region <- tabular((Region) * Heading() ~ ` ` * (A + B + C) * Heading() * (identity), 
                            data = freq_by_region)
  write.table.tabular(freq_by_region, paste0(save_location, "/", position4.2$Filename[1], ".csv"), sep = ",")
  
  tmp <- fread(paste0(save_location, "/", position4.2$Filename[1], ".csv"))
  tmp[1, 1] <- tmp[2, 1]
  write.xlsx(tmp, paste0(save_location, "/", position4.2$Filename[1], ".xlsx"), colNames = FALSE)
}
