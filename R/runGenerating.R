#' Setup the prerequest env
#'
#' @author Zhe Liu
#' @description Setup the prerequest env
#' @example prerequest()
#'
prerequest <- function() {
  if (!requireNamespace("pacman")) install.packages("pacman")
  pacman::p_load("zip",
                 "openxlsx",
                 "readxl",
                 "writexl",
                 "RcppRoll",
                 "plyr",
                 "stringi",
                 "feather",
                 "RODBC",
                 "MASS",
                 "car",
                 "data.table",
                 "plotly",
                 "tidyverse",
                 "lubridate",
                 "janitor",
                 "digest",
                 "tables",
                 "DT",
                 "shiny",
                 "shinydashboard",
                 "shinydashboardPlus",
                 "shinyjs",
                 "shinyFiles")
}


#' Initiate the shiny app
#'
#' @export
#' @author Zhe Liu
#' @description Initiate the shiny app to do the generating
#' @example runGenerating()
#'
runGenerating <- function() {
  prerequest()

  appDir <- system.file("Janssen",
                        "AutoGeneratingTool",
                        package = "JanssenPPTAutoTool")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `JanssenPPTAutoTool`.",
         call. = FALSE)
  }

  shiny::runApp(appDir,
                launch.browser = TRUE,
                display.mode = "normal")
}
