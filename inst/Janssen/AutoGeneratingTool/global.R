# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Janssen PPT
# Purpose:      Tables generation
# programmer:   Zhe Liu
# Date:         20-11-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


if(Sys.getenv('SHINY_PORT') == "") {
  options(shiny.maxRequestSize=1000*1024^2,
          warn = -1)
}

##---- loading the required packages ----
# suppressPackageStartupMessages({
#   require(zip)
#   require(openxlsx)
#   require(readxl)
#   require(writexl)
#   require(RcppRoll)
#   require(plyr)
#   require(stringi)
#   require(feather)
#   require(RODBC)
#   require(MASS)
#   require(car)
#   require(data.table)
#   require(plotly)
#   require(tidyverse)
#   require(lubridate)
#   require(janitor)
#   require(digest)
#   require(tables)
#   require(DT)
#   require(shiny)
#   require(shinydashboard)
#   require(shinydashboardPlus)
#   require(shinyjs)
#   require(shinyFiles)
# })


##---- source function ----
source("./function/CPDOverviewFunc.R", encoding = "UTF-8")
source("./function/CPDMainbodyFunc.R", encoding = "UTF-8")
source("./function/CPDRPDFunc.R", encoding = "UTF-8")
source("./function/CPDP13Func.R", encoding = "UTF-8")
source("./function/CPDP5253Func.R", encoding = "UTF-8")

source("./function/IPDChannelFunc.R", encoding = "UTF-8")
source("./function/IPDMarketFunc.R", encoding = "UTF-8")
source("./function/IPDPerfFunc.R", encoding = "UTF-8")
source("./function/IPDCallFunc.R", encoding = "UTF-8")
source("./function/IPDPerfSegFunc.R", encoding = "UTF-8")
source("./function/LaunchPerfFunc.R", encoding = "UTF-8")
source("./function/LaunchRegionFunc.R", encoding = "UTF-8")
source("./function/LaunchHCPFunc.R", encoding = "UTF-8")
