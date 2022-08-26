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
