#  introductoryDemo   -  documentation with example of package us
#  0. To access this demo user needs to have created a directory into which 
#     files can be copied - we show directory ~/mycode then issuing R commands: 
library(FEMSdevPkg)
mycodedir <- "~/mycode"
installSampleCode(mycodedir)

# 1.0  First example: create a portfolio from sample data, generateEvents
library(FEMSdevPkg)
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- paste0(mydatadir,"/BondPortfolio.csv")
rfdfn <- paste0(mydatadir,"/RiskFactors.csv")
ptf   <-  samplePortfolio(cdfn,rfdfn)
unlist(ptf$contracts[[1]]$contractTerms)
unlist(lapply(ptf$contracts,function(x){return(x$isCompound)}))
unlist(ptf$riskFactors)
serverURL <- "https://demo.actusfrf.org:8080/"
cfls  <- generateEvents(ptf,serverURL)
unlist(lapply(cfls,function(x){return(x$status)}))

# 2.0  Open a shiny application allowing browser controlled review/examples 
runDaDFiR3demo()

# 3.0  Create a single, (fixed rate) PAM bond with in line term specification,
#      generate an EventSeries of cash flow events for that Contract and Plot 
pam1 <- bond("2013-12-31", maturity = "5 years", nominal = 50000,
coupon = 0.02, couponFreq = "1 years", role = "long")
unlist(pam1$contractTerms)
evs1 <- generateEventSeries(pam1, list(), serverURL)
unlist(list(contractID = evs1$contractID,
            contractType=evs1$contractType,
            statusDate= evs1$statusDate,
            riskFactors = evs1$riskFactors
          ))
evs1$events_df
cashflowPlot(evs1)

# 4 Choose and extract a single contract from the sample portfolio; generate
#   EventSeries of cashflow events and plot for visual review 
cids <- getContractIDs(ptf)   # find the contractID in the portfolio
cids
cntr1 <- getContract(ptf,cids[1]) # get the contract with the selected cid
evs2 <- generateEventSeries(cntr1, list(), serverURL)
evs2$events_df
cashflowPlot(evs2)   # a very simple  zero-coupon bond

# 5.  Create sample ReferenceIndex objects - rising and falling projections ( 
#      for the same marketObjectCode  YC_EA_AAA   
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
falling_fp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rising_fp <-  paste0(mydatadir,"/UST5Y_risingRates.csv")
rfx_falling <- sampleReferenceIndex(falling_fp,"UST5Y_fallingRates", 
                                    "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                   "YC_EA_AAA",100)

# 6.  Extract a Varying Rate PAM from the sample portfolio with "YC_EA_AAA" 
#     as its marketObjectCode, and show different results for future 
#     dependent on whether rising or falling interest rates are projected 
#     sample PAMs ID = '112', '113', '114' have varying rates based on 
#     marketObjectCode "YC_EA_AAA". View ~/mydata/BondPortfolio.csv to validate
# 6.1  Initialize portfolio ptf as in 1.0 
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- paste0(mydatadir,"/BondPortfolio.csv")
rfdfn <- paste0(mydatadir,"/RiskFactors.csv")
ptf   <-  samplePortfolio(cdfn,rfdfn)
# 6.2  Extract varying rate contract with contractID = '119'
vrpam119 <- getContract(ptf, '119')

# 6.3  create rising and falling interest rate projections as in 5.0 
falling_fp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rising_fp <-  paste0(mydatadir,"/UST5Y_risingRates.csv")
rfx_falling <- sampleReferenceIndex(falling_fp,"UST5Y_fallingRates", 
                                    "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates", 
                                   "YC_EA_AAA",100)

# 6.4  generate EventSeries cashflows for this PAM, for rising and falling 
#      interest rate scenarios as in 4. with cashflow plots for visual 
#      comparisons as 
serverURL <- "https://demo.actusfrf.org:8080/"
evs_falling <- generateEventSeries(vrpam119, list(rfx_falling), serverURL)
evs_falling$events_df
cashflowPlot(evs_falling)   
# compare with the same contract but a rising interest rate projection/Scenario 
evs_rising <- generateEventSeries(vrpam119, list(rfx_rising), serverURL)
evs_rising$events_df
cashflowPlot(evs_rising)   
