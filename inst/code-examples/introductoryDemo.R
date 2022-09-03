#  introductoryDemo   -  documentation with example of package use
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
runDaDFiR3demo()
# 5. Developing FEMSdev support for ActusContract.rmd RShiny demo
# 5.1 create PAM bond with user friendly API
pam1 <- bond("2013-12-31", maturity = "5 years", nominal = 50000,
coupon = 0.02, couponFreq = "1 years", role = "long")
unlist(pam1$contractTerms)
#5.2 create null / uninitialized EventSeries
evs1 <- generateEventSeries(pam1, list(), serverURL)
unlist(list(contractID = evs1$contractID,
            contractType=evs1$contractType,
            statusDate= evs1$statusDate,
            riskFactors = evs1$riskFactors
          ))
evs1$events_df
cashflowPlot(evs1)
# 5.3 Extract a contract from the sample portfolio 
cids <- getContractIDs(ptf)   # find the contractID in the portfolio
cids
cntr1 <- getContract(ptf,cids[1]) # get the contract with the selected cid
evs2 <- generateEventSeries(cntr1, list(), serverURL)
evs2$events_df
cashflowPlot(evs2)   # a very simple  zero-coupon bond
# 5.4 Create sample ReferenceIndex objects 
mydatadir <- "~/mydata"
installSampleData(mydatadir)
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx_falling <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)