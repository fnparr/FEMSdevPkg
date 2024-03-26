# ******** Unit Tests of ContractAnalysis class
# README. fnp Jan 2024
#  Fetch the package  > devtools::github_install("fnparr/FEMSdevPkg@dev")
#  In the Rstudio "Build" tab : "Build" -> "more" -> "load All"
#  .. because for testing and learning you want access to internal not
#     exported functions and definitions
#  **************************
# ******** YieldCurve tests 
# *************
# Test 0.0 create YieldCurve with valid and invalid data
# will need to clear the environment 
rm(list=ls())
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )/100
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "NONE"
yc <- YieldCurve(ycID,rd,tr,dcc,cf)
# valid case 
dcc_e <- "30/360"
yc_e <- YieldCurve(ycID,rd,tr,dcc_e,cf)
# should generate error message for invalid dcc
# could also check for cf == "NONE" 
# *************
# Test 0.1 yearFractions, InterpolateYieldCurve, ForwardRates
Tfrom <- "2024-01-01"
Tto <- "2025-07-01"
yfdcc <- "30e/360"
frac <- yearFraction(Tfrom, Tto, yfdcc)
frac
Rout <- interpolateYieldCurve(yc,0.083333)
Rout
frwdRate <- getForwardRates(yc, Tfrom,Tto)
frwdRate
# ************
# Test 0.2  Discount and Growth Factors 
riskSpread <- 0.02
discountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
discountF
growthF <- getGrowthFactor(yc,Tfrom,Tto)
growthF
# ******** Unit Tests of ContractAnalysis class 
# README. fnp Jan 2024
#  Fetch the package  > devtools::github_install("fnparr/FEMSdevPkg@dev")
#  In the Rstudio "Build" tab : "Build" -> "more" -> "load All"
#  .. because for testing and learning you want access to internal not
#     exported functions and definitions 
#  **************************
# will need to clear the environment
rm(list=ls())

# Test 0.0 : Create and unit test a timeline  
tl <- Timeline("2024-01-01", 3, 4, 8) 
tl <- Timeline( statusDate="2024-01-01",monthsPerPeriod = 3,reportCount = 4,
                periodCount = 8)
date2PeriodIndex(tl, "2025-02-01")
date2PeriodIndex(tl, "2025-04-01")
date2PeriodIndex(tl,"2027-12-31")

# Test 1.0 create ContractAnalysis - essential fields only - others class()
cfla <- ContractAnalysis( analysisID = "cfla001",
                          analysisDescription = "this_analysis_descr",
                          enterpriseID = "entp001",
                          yieldCurve = YieldCurve(),
                          portfolio =  Portfolio(),
                          currency = "USD",
                          scenario = list(),
                          actusServerURL = "https://dadfir3-app.zhaw.ch/",
                          timeline = Timeline())
cfla$analysisID

# Test 1.1 will add a meaningful portfolio into the ContractAnalysis object
#  cfla is a cashflow_Analysis with 19 Bond contracts 
#  clfa is a cashflow_Analysis with a single  riskfactor list can be empty! 

rm(list=ls())               # clear the environment and reload fndefs

mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf <- samplePortfolio(cdfn)
cfla <- ContractAnalysis( analysisID = "cfla001",
                          analysisDescription = "this_analysis_descr",
                          enterpriseID = "entp001",
                          yieldCurve = YieldCurve(),
                          portfolio =  ptf,
                          currency = "USD",
                          scenario = list(),
                          actusServerURL= "https://dadfir3-app.zhaw.ch/",
                          timeline = Timeline())
#                          actusServerURL = "https://demo.actusfrf.org:8080/",
typeof(cfla$portfolio$contracts)
length(cfla$portfolio$contracts)
cntr1<-cfla$portfolio$contracts[[1]]
typeof(cntr1)
class(cntr1)
unlist(cntr1$contractTerms)
ptf1 <- Portfolio(cntr1)
length(ptf1$contracts)
cfla1 <- ContractAnalysis( analysisID = "cfla001",
                          analysisDescription = "this_analysis_descr",
                          enterpriseID = "entp001",
                          yieldCurve = YieldCurve(),
                          portfolio =  ptf1,
                          currency = "USD",
                          scenario = list(),
                          actusServerURL = "https://dadfir3-app.zhaw.ch/",
                          timeline = Timeline())
#                         actusServerURL = "https://demo.actusfrf.org:8080/",

# Test 2.0 we have to modify the definition of generic generateEvents() to 
#        add a ContractAnalysis (cfla) parameter - so provide retesting 
#        contract and portfolio simulations from exportTestMain.R 
#.      This Test 2.0 simulates a PAM contract, no risk factors, shows IED event
rm(list=ls())
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
pam1 <- bondvr("2013-12-31", maturity = "5 years", nominal = 1000,
               coupon = 0.02, paymentFreq = "1 year", role = "long",
               rateResetFreq = "Fixed rate")
ptf2 <- Portfolio(pam1)
cfls <-generateEvents(ptf=ptf2, serverURL= serverURL, riskFactors= list())
cfls[[1]]$status
cfls[[1]]$message
cfls[[1]]$contractId
length(cfls[[1]]$events)
unlist(cfls[[1]]$events[[1]])

rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
cfls  <- generateEvents(ptf,serverURL,list(rfx))
unlist(lapply(cfls,function(x){return(x$status)}))

#Test 3.0 generateEvents(<ContractAnalysis>)  single PAM no rfs case first 
rm(list=ls())
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
pam1 <- bondvr("2013-12-31", maturity = "5 years", nominal = 1000,
               coupon = 0.02, paymentFreq = "1 year", role = "long",
               rateResetFreq = "Fixed rate")
ptf1 <- Portfolio(pam1)
cfla1 <- ContractAnalysis( analysisID = "cfla001",
                           analysisDescription = "this_analysis_descr",
                           enterpriseID = "entp001",
                           yieldCurve = YieldCurve(),
                           portfolio =  ptf1,
                           currency = "USD",
                           scenario = list(),
                           actusServerURL = serverURL,
                           timeline = Timeline()
                           )
msg <- generateEvents(cntan= cfla1)
cfla1$cashflowEventsLoL[[1]]$status

#Test 3.1 generateEvents(<ContractAnalysis>)  sample PAM portfolio
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL<- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
cfla <- ContractAnalysis( analysisID = "cfla001",
                          analysisDescription = "this_analysis_descr",
                          enterpriseID = "entp001",
                          yieldCurve = YieldCurve(),
                          portfolio =  ptf,
                          currency = "USD",
                          scenario = list(rfx),
                          actusServerURL = serverURL,
                          timeline = Timeline())
msg <- generateEvents(cntan= cfla)
unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)}))

#Test 3.2 include real Timeline in ContractAnalysis - prelim to events2dfByPeriod

# For that we need a portfolio with consistent statusDate for all contracts. The
# sample BondPortfolio does not do this .. but we can subset the sample Bond
# Portfolio to contracts with statusDate= "2015-01-01" and use the new 
# Portfolio(<contractList>) constructor to create a portfolio with just these 
# contracts. Then generateEvents(cfla2015) to simulate 
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
# display the statusDate values for contracts in ptf 
ptfsd <- unlist(lapply(ptf$contracts,
                       function(x){return(x$contractTerms["statusDate"])}))
ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
unlist(lapply(ptf2015$contracts,
              function(x){return(x$contractTerms["statusDate"])}))
# now construct a relevant Timeline with this statusDate
tl1 <- Timeline("2015-01-01",3,4,8)
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL<- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
cfla2015 <- ContractAnalysis( analysisID = "cfla001",
                          analysisDescription = "this_analysis_descr",
                          enterpriseID = "entp001",
                          yieldCurve = YieldCurve(),
                          portfolio =  ptf2015,
                          currency = "USD",
                          scenario = list(rfx),
                          actusServerURL = serverURL,
                          timeline = tl1 )
msg <- generateEvents(cntan= cfla2015)
unlist(lapply(cfla2015$cashflowEventsLoL,function(x){return(x$status)}))

# Test 3.3 now write and test events2dfByPeriod(cfla) function using timeline
# and reorganizing the events from cashflowEventsLoL set in  Test 3.2  
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
tl1 <- Timeline("2015-01-01",3,4,8)
cfla2015 <- ContractAnalysis( analysisID = "cfla001", 
                              analysisDescription = "this_analysis_descr",
                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
                              portfolio =  ptf2015, currency = "USD", 
                              scenario = list(rfx), 
                              actusServerURL = serverURL, 
                              timeline = tl1)
logMsgs1  <- generateEvents(cntan = cfla2015)
logMsgs2  <- events2dfByPeriod(cfla = cfla2015)
logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
cfla2015$contractLiquidityVectors[["102"]]

# Test 3.4 now write and test lv2liquidityReports(cfla) function using the 
# contractLiquidityVectors data from Test 3.3  
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
tl1 <- Timeline("2015-01-01",3,4,10)
cfla2015 <- ContractAnalysis( analysisID = "cfla001", 
                              analysisDescription = "this_analysis_descr",
                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
                              portfolio =  ptf2015, currency = "USD", 
                              scenario = list(rfx), 
                              actusServerURL = serverURL, 
                              timeline = tl1)
logMsgs1  <- generateEvents(cntan = cfla2015)
logMsgs2  <- events2dfByPeriod(cfla= cfla2015)
logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
logMsgs4  <- lv2LiquidityReports(cfla= cfla2015)
head (cfla2015$cashflowEventsByPeriod,15)

# Test 3.5 now write and test eventsdf2incomeReports(cfla) function using the 
# cashflowEventsByPeriod dataframe to generate income reports   
rm(list=ls())
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/BondPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
ptfsd <- unlist(lapply(ptf$contracts,
                       function(x){return(x$contractTerms["statusDate"])}))
ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://localhost:8083/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
tl1 <- Timeline("2015-01-01",3,8,16)
cfla2015 <- ContractAnalysis( analysisID = "cfla001", 
                              analysisDescription = "this_analysis_descr",
                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
                              portfolio =  ptf2015, currency = "USD", 
                              scenario = list(rfx), 
                              actusServerURL = serverURL, 
                              timeline = tl1)
logMsgs1  <- generateEvents(cntan = cfla2015)
logMsgs2  <- events2dfByPeriod(cfla= cfla2015)
logMsgs5  <- eventsdf2incomeReports(cfla= cfla2015)

logMsgs6 <- nominalValueReports(cfla2015)
