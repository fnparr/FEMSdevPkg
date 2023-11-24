# start clean version Here
# Step 1.0 SETUP ENVIRONMENT, LIBRARIES, SAMPLE DATA in ~/mydata
# install.packages("devtools"). # if needed or click on devtools in packages
library(devtools) 
# we want FEMSdevPkg not FEMSdevBase because NOT trying to deploy to cloud 
# check whether FEMSdevPkg in packages list - select V 0.0.7 - quickest
# if NOT > install_github("fnparr/FEMSdevPkg")
library(FEMSdevPkg)
#  skip installing sampleCode ! think not needed because library
#  Do install sampleData
mydatadir <- "~/mydata"
installSampleData(mydatadir)

# 2.0   Test single contracts of each contract type before portfolios 
serverURL <- "https://dadfir3-app.zhaw.ch/"
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
evs22 <- generateEventSeries(pam22, list(rfx_falling), serverURL)
evs22$events_df
cashflowPlot(evs22)

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
# 5.0 Portfolio Testing starts here -------- 
# 5.1 Create and show portfolios
cdfn  <- paste0(mydatadir,"/AnnuityPortfolio.csv")
rfdfn <- paste0(mydatadir,"/RiskFactors.csv")

ptf   <-  samplePortfolio(cdfn = cdfn)
unlist(ptf$contracts[[1]]$contractTerms)
rfxlist <- sampleReferenceIndexList(rfdfn)
rfxlist[[1]]$marketObjectCode  # show off the first referenceIndex from the list
rfxlist[[1]]$riskFactorID
rfxlist[[1]]$data

cfls  <- generateEvents(ptf,serverURL, rfxlist)
unlist(lapply(cfls,function(x){return(x$status)}))
unlist(cfls[[1]])

#5.2  simulate and plot Portfolio Monthly data falling rates 
plotlist <- simulatePortfolio(ptf, serverURL, list(rfx_falling),
                              rfx_falling$riskFactorID)

# display the plots - fo falling rates scenario 
plotlist[["monthly income"]]
plotlist[["cumulative income"]]
plotlist[["monthly liquidity change"]]
plotlist[["accumulated liquidity"]]

# 6.0  Open latestshiny application with browser controlled review examples
runDaDFiR3demo3() 

# ------------- Testing complete
