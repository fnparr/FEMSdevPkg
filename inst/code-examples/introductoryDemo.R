#  introductoryDemo   -  documentation with example of package us
#  0. To access this demo user needs to have created a directory into which 
#     files can be copied - we show directory ~/mycode then issuing R commands: 
library(FEMSdevBase)
mycodedir <- "~/mycode"
installSampleCode(mycodedir)

# 1.0  First example: create a portfolio from sample data, generateEvents
library(FEMSdevPkg)
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)

# 2.0   better to create and test single contracts of each contract type 
#       before building portfolios

serverURL <- "https://demo.actusfrf.org:8080/"
# serverURL <- "http://ractus.ch:8080/"

# 2.1  Create a single, (fixed rate) PAM bond with in line term specification,
#      generate an EventSeries of cash flow events for that Contract and Plot 
pam1 <- bondvr("2013-12-31", maturity = "5 years", nominal = 1000,
             coupon = 0.02, paymentFreq = "1 year", role = "long",
             rateResetFreq = "Fixed rate")
unlist(pam1$contractTerms)

evs1 <- generateEventSeries(pam1, list(), serverURL)
unlist(list(contractID = evs1$contractID,
            contractType=evs1$contractType,
            statusDate= evs1$statusDate,
            riskFactors = evs1$riskFactors
))
evs1$events_df
cashflowPlot(evs1)

# 3.1  Initialize some interest rate riskfactors for YC_EA_AAA for VR contracts 
falling_fp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rising_fp <-  paste0(mydatadir,"/UST5Y_risingRates.csv")
rfx_falling <- sampleReferenceIndex(falling_fp,"UST5Y_fallingRates", 
                                    "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                   "YC_EA_AAA",100)

# 3.2  Now generate a variable rate PAM RRMOC="YC_EA_AAA" by default 
pam2 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
               coupon = 0.02, paymentFreq = "3 months", role = "long",
               rateResetFreq = "1 years", rateResetSpread = 0.01 )
unlist(pam2$contractTerms)

evs2 <- generateEventSeries(pam2, list(rfx_falling), serverURL)
evs2$events_df
cashflowPlot(evs2)

# 3.2  Now generate a variable rate PAM RRMOC="YC_EA_AAA" by default 

evs3<- generateEventSeries(pam2, list(rfx_rising), serverURL)
evs3$events_df
cashflowPlot(evs3)

# Testing PAMs with 6 month payment frequency
# 3.3  Variable rate 
pam21 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
                coupon = 0.02, paymentFreq = "6 months", role = "long",
                rateResetFreq = "1 years", rateResetSpread = 0.01 )
unlist(pam21$contractTerms)
evs21 <- generateEventSeries(pam21, list(rfx_falling), serverURL)
evs21$events_df
cashflowPlot(evs21)

# Testing with 6 month payment frequency
# 3.4  Fixed Rate  
pam22 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
                coupon = 0.02, paymentFreq = "6 months", role = "long",
                rateResetFreq = "Fixed rate", rateResetSpread = 0.01 )
unlist(pam21$contractTerms)
evs21 <- generateEventSeries(pam21, list(rfx_falling), serverURL)
evs21$events_df
cashflowPlot(evs21)

# 4.1 Testing mortgage( ) implicit fixed rate 
ann1 <- mortgage("2020-12-31",maturity ="20 years", nominal= 1000, 
                 coupon = 0.08, paymentFreq = "1 year", role= "long")
unlist(ann1$contractTerms)
evs4 <- generateEventSeries(ann1, list(), serverURL)
evs4$events_df
cashflowPlot(evs4)

# 4.2 Testing Mortgage( ) explicit fixed rate 
ann2 <- mortgage("2020-12-31",maturity ="20 years", nominal= 1000, 
                 coupon = 0.08, paymentFreq = "1 year", role= "long",
                 rateResetFreq = "Fixed rate")
unlist(ann2$contractTerms)
evs5 <- generateEventSeries(ann2, list(), serverURL)
evs5$events_df
cashflowPlot(evs5)

# 4.3 Testing Mortgage( ) variable rate  
ann3 <- mortgage("2020-12-31", maturity = "20 years", nominal = 1000,
                 coupon =0.08 , paymentFreq = "1 year", role = "long",
                 rateResetFreq = "1 year", rateResetSpread = 0.03 )
unlist(ann3$contractTerms)
evs6 <- generateEventSeries(ann3, list(rfx_rising), serverURL)
evs6$events_df
cashflowPlot(evs6)

# -------- TESTED up to HERE  _ FNP 31st October 2022 

cdfn  <- paste0(mydatadir,"/AnnuityPortfolio.csv")
rfdfn <- paste0(mydatadir,"/RiskFactors.csv")
ptf   <-  samplePortfolio(cdfn = cdfn,rfdfn)
unlist(ptf$contracts[[1]]$contractTerms)
unlist(lapply(ptf$contracts,function(x){return(x$isCompound)}))
unlist(ptf$riskFactors)

cfls  <- generateEvents(ptf,serverURL)
unlist(lapply(cfls,function(x){return(x$status)}))
unlist(cfls[[1]])

#1.7  simulate and plot Portfolio Monthly data falling rates 
 plotlist <- simulatePortfolio(ptf, serverURL, list(rfx_falling),
                          rfx_falling$riskFactorID)
 
 # display the plots - fo falling rates scenario 
 plotlist[["monthly income"]]
 plotlist[["cumulative income"]]
 plotlist[["monthly liquidity change"]]
 plotlist[["accumulated liquidity"]]
 
 
# 2.0  Open a shiny application allowing browser controlled review/examples 
runDaDFiR3demo()

# 2.1  Open updated shiny application with browser controlled review examples
runDaDFiR3demo2()

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
par(mfrow = c(1,2))
cashflowPlot(evs_falling)   
# compare with the same contract but a rising interest rate projection/Scenario 
evs_rising <- generateEventSeries(vrpam119, list(rfx_rising), serverURL)
evs_rising$events_df
cashflowPlot(evs_rising)   

