# ****************
# runDemo.R    - package function to run a Shiny demo app
#' runDaDFiR3demo()  Launches a shiny demo application
#' 
#' Launches a browser controlled application plotting the provided sample 
#' reference rate data in a display window. You can chose between plots 
#' of  5yTreasuryBill_fallingRates, 5yTreasuryBill_recoveringRates, etc 
#' 
#' @export
#' @importFrom shiny runApp
runDaDFiR3demo <- function (){
  appDir <- system.file("shiny-examples","DaDFiR3-Demo", package = "FEMSdevPkg")
  shiny::runApp(appDir)
}
#' runDaDFiR3demoV2()  Launches updated shiny demo application
#' 
#' Launches a browser controlled application plotting the provided sample 
#' reference rate data in a display window. You can chose between plots 
#' of  5yTreasuryBill_fallingRates, 5yTreasuryBill_recoveringRates, etc 
#' 
#' @export
#' @importFrom shiny runApp
runDaDFiR3demo2 <- function (){
  appDir <- system.file("shiny-examples","DaDFiR3-Demo2", package = "FEMSdevPkg")
  shiny::runApp(appDir)
}
#' runDaDFiR3demo3()  Launches  latest Rshiny demo application
#' 
#' Demo3 is a  reactive (browser controlled) application with tabs for: 
#' (1) plotting  provided selected risk scenario reference rate data 
#' in a display window. You can chose between "rising", "falling", "steady" 
#' and "recovering" for US 5-year Treasury bills; (2) specifying an ACTUS 
#' contact and viewing a plot of its generated cashflows for the specified 
#' interest rate reick factor scenario; (3) selecting a portfolio and viewing 
#' plots of income and liquidity; month by month or cumulative for a selected 
#' interest rate scenario; (4) performing the same analysis for an uploaded 
#' portfolio of ACTUS contracts; (5) viewing a help page which includes 
#' instruction on how to configurer the demo to work with a specified ACTUS 
#' server - with additional details for when it is implemented as a Docker container 
#' 
#' @export
#' @importFrom shiny runApp 
runDaDFiR3demo3 <- function (){
  appDir <- system.file("shiny-examples","DaDFiR3-Demo3", package = "FEMSdevPkg")
  shiny::runApp(appDir)
}
