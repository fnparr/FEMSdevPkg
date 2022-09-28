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