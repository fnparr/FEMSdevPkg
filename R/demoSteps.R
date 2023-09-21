# demoSteps.R  file with functions to simplify FEMSdevPkg demonstration

# *****************
#' startDemo ()
#' 
#' installs sample code and sample data into ~/mycode and ~/mydate folders
#' clears the R environment 
#' @export
startDemo <- function(){
  mycodedir <- "~/mycode"
  installSampleCode(mycodedir)
  
  rm(list=ls())
  mydatadir <- "~/mydata"
  installSampleData(mydatadir)
}

# *******************
#' createPortfolioFromExcelData()
#' 
#' reads contract data and risk factor data from CSV files to create
#' and return a portfolio of ACTUS contract
#' @export
createPortfolioFromExcelData <- function(){
  mydatadir <- "~/mydata"
  cdfn  <- paste0(mydatadir,"/BondPortfolio.csv")
  ptf   <-  samplePortfolio(cdfn)
  return(ptf)
}
