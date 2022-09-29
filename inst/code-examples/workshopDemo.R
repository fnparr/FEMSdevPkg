# Demonstration of FEMSdevPkg capabilities for workshop
library(FEMSdevPkg)
library(utils)
# 0.   clear the environment 
rm(list=ls())
# 1.  Open updated shiny application with browser controlled concept examples.
runDaDFiR3demo2()

#2. Demonstration of FEMSdevPkg functions 
startDemo()

#3. create portfolio of ACTUS contracts from spreadsheet (CSV)  data 
ptf <- createPortfolioFromExcelData()

#    a variable rate Principal AT Maturity (bond) 
vrpam119 <- getContract(ptf, '119')

#4.  create rising and falling interest rate scenario
rfx_falling <- sampleReferenceIndex( "~/mydata/UST5Y_fallingRates.csv",
                                     "UST5Y_fallingRates", "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex( "~/mydata/UST5Y_risingRates.csv",
                                     "UST5Y_risingRates", "YC_EA_AAA",100)

#5  Locate an ACTUS server
serverURL <- "https://demo.actusfrf.org:8080/"

#6  Call out to generate cashflows for the falling interest rate scenario  
evs_falling <- generateEventSeries(vrpam119, list(rfx_falling), serverURL)
evs_falling$events_df

#7  Call out to generate cashflows for the rising interest rate scenario
evs_rising <- generateEventSeries(vrpam119, list(rfx_rising), serverURL)
evs_rising$events_df

#8 side by side plots
par(mfrow = c(1,2))
cashflowPlot(evs_falling)
cashflowPlot(evs_rising)   
