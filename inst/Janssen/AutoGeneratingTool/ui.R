# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen PPT
# Purpose:      Ui
# programmer:   Zhe Liu
# Date:         20-11-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    title = strong("Janssen PPT Auto Generator"),
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 400,
    
    sidebarMenu(
      menuItem(
        text = strong("CPD"),
        tabName = "cpd",
        startExpanded = TRUE
      ),
      
      menuItem(
        text = strong("IPD"),
        tabName = "ipd",
        startExpanded = FALSE
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "cpd",
        boxPlus(
          title = strong("1. CPD Mapping Table"),
          status = "primary",
          width = 12,
          closable = FALSE,
          fileInput("cpd_mapping", label = NULL)
        ),
        boxPlus(
          title = strong("2. CPD Raw Data"),
          status = "primary",
          width = 12,
          closable = FALSE,
          shinyDirButton("cpd_raw", label = "Choose the input folder", title = "Please select a folder"),
          verbatimTextOutput("cpd_raw_path")
        ),
        boxPlus(
          title = strong("3. CPD Run"),
          status = "primary",
          width = 12,
          closable = FALSE,
          shinyDirButton("cpd_save", label = "Choose an output folder", title = "Please select a folder"),
          verbatimTextOutput("cpd_save_path"),
          actionButton("cpd_run", label = strong("RUN"))
        )
      ),
      
      tabItem(
        tabName = "ipd",
        boxPlus(
          title = strong("1. IPD Mapping Table"),
          status = "primary",
          width = 12,
          closable = FALSE,
          fileInput("ipd_mapping", label = NULL)
        ),
        boxPlus(
          title = strong("2. IPD Raw Data"),
          status = "primary",
          width = 12,
          closable = FALSE,
          shinyDirButton("ipd_raw", label = "Choose the input folder", title = "Please select a folder"),
          verbatimTextOutput("ipd_raw_path")
        ),
        boxPlus(
          title = strong("3. IPD Run"),
          status = "primary",
          width = 12,
          closable = FALSE,
          shinyDirButton("ipd_save", label = "Choose an output folder", title = "Please select a folder"),
          verbatimTextOutput("ipd_save_path"),
          actionButton("ipd_run", label = strong("RUN"))
        )
      )
    )
    
  )
)





























