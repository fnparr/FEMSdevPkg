# FinancialModel unit test script 
# ********
# library(yaml)
# Test 1.0 : clear environment and create financial model instance 
rm(list=ls())
fm <- FinancialModel()
class(fm)
# Test 2. : create a "ready-for-analysis financial model 
#         will need: fmID, fmDescr, entprid, accounts, ptf, currency, timeline
#.                   serverURL
# Test 2.1:  set up identifier descriptor  and other scalar fields 
fmID <- "fm001"
fmDescr <- "test Financial Model logic with example"
entprID <- "modelBank01"
currency <- "USD"

serverURL <- "https://demo.actusfrf.org:8080/" 
# serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://ractus.ch:8080/"

# Test 2.2 create an accounts data tree for hierarchical reports - yaml format
#          string defining tree and contract assignment 
yamlstring <- "
name:  a Model Bank
Assets:
  Current:
     actusCIDs:
        - pam001
        - pam002
        - ann003
  ShortTerm:
     actusCIDs:
        - pam004
        - ann005
  LongTerm:
     functionIDs:
        - edf006
Liabilities:
  Debt:
     actusCIDs:
        - pam007
  Equity:
Operations:
  Cashflows:
     functionIDs:
        - ocf008
"
yamlstring
cat(yamlstring)

#  Use string above to create an AccountsTree instance 
accountsTree <- AccountsTree(yamlstring) 
print(accountsTree$root,"actusCIDs", "nodeID")

# Test 2.3 Need a sample portfolio of Actus contracts with CIDs as above
#          To view TestPortfolio.csv import into Excel with "," delimiter
cdfn  <- "~/mydata/TestPortfolio.csv"
ptf <- samplePortfolio(cdfn)
unlist(ptf$contracts[[1]]$contractTerms)

# Test 2.4 Need a timeline  
tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, reportCount=3,
               periodCount = 6)

# Test 2.4 Now create financialModel 
fm <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                         accntsTree = accountsTree, ptf = ptf, curr = currency,
                         timeline = tl, serverURL = serverURL
)

class(fm)
fm$serverURL
unlist(fm$portfolio$contracts[[1]]$contractTerms)
fm$financialModelID
fm$financialModelDescription
fm$enterpriseID
fm$currency
# ******************************
# Test 2.5  initFinancialModel( ) example ( from empty env) 
#  could be step1 in FinancialModelSteps 
rm(list=ls())
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
    "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
    "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
    "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
    "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
    "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                   accntsTree = accountsTree, ptf = ptf, curr = currency,
                   timeline = tl, serverURL = serverURL) 
# Test 3.0
# next Step is to add and then run a ScenarioAnalysis with appropriate
# marketData. These stapes wil match contents of unitTestMain.R test 3.0 
# BUT use currentScenarioAnalysis as working template 

# OK so far BUT we need a Scenario with YC_EA_AAA and a timeline 
# so NOT UnitTestMain Test 3.0 ..  this from test 3.1

mydatadir <- "~/mydata"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
# The 100 parameter is the base level for JSON 
marketData <-list(rfx)
addScenarioAnalysis(fm = fm, scnID= "UST5Y_fallingRates", rfxs = marketData,
                    yc = YieldCurve())

# Test 3.1 test example running addScenarioAnalysis()
rm(list=ls())
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
    "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
    "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
    "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
    "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
    "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                   accntsTree = accountsTree, ptf = ptf, curr = currency,
                   timeline = tl, serverURL = serverURL) 
   
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   marketData <-list(rfx)
   msg <- addScenarioAnalysis(fm = fm, scnID= "UST5Y_fallingRates", 
                              rfxs = marketData, yc = YieldCurve()) 
# Test 4.0 
# Run a FinancialModel Portfolio Simulation using its currentScenarioAnalysis
# for Risk factor data 
# 4.1 first create the ScenarioAnalysis and test generateEvents() on it
mydatadir <- "~/mydata"
installSampleData(mydatadir)
cdfn  <- "~/mydata/TestPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
serverURL <- "https://demo.actusfrf.org:8080/"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
rfxs <-list(rfx)
scnID <- "UST5Y_fallingRates"
yc<- YieldCurve()
scna <- ScenarioAnalysis(scenarioID= scnID, marketData= rfxs, 
                         yieldCurve = yc)
logMsgs  <- generateEvents(host= scna, ptf= ptf, serverURL = serverURL)
logMsgs

# Test 4.2 example test for generateEvents(FinancialModel)
rm(list=ls())   
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
    "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
    "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
    "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
    "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
    "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                   accntsTree = accountsTree, ptf = ptf, curr = currency,
                   timeline = tl, serverURL = serverURL) 
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   marketData <-list(rfx)
   msg1 <- addScenarioAnalysis(fm = fm, scnID= "UST5Y_fallingRates", 
                              rfxs = marketData, yc = YieldCurve())                
   msg2 <- generateEvents(host= fm)

# Test 5. Bucket the cash flow events in the timeline model 
# 5.0  create timeline,  scna with cashflows; test events2dfByPeriod(scna, tl)
# Run all test to (1) create fm with TImeline, (2) scna (3) generateEvents(scna) 
scna <- fm$currentScenarioAnalysis
logMsgs <- events2dfByPeriod(host= scna, tl = fm$timeline)
logMsgs
# 5.1 now with exported events2dfByPeriod(fm) 
logMsgs <- events2dfByPeriod(host = fm)
logMsgs
# Test 5.2  Example use of events2dfByPeriod(host=fm1)
rm(list=ls())
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
    "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
    "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
    "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
    "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
    "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm1 <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                   accntsTree = accountsTree, ptf = ptf, curr = currency,
                   timeline = tl, serverURL = serverURL) 
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   
   marketData <-list(rfx)
   msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                              rfxs = marketData, yc = YieldCurve())                
   msg2 <- generateEvents(host= fm1)
   msg3 <- events2dfByPeriod(host= fm1)

# *************
# Test 6.  Generate nominal value report generation 
# 6.1  test first the  nominalValueReports(host=ScenarioAnalysis, ...)   
  rm(list=ls())
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
                        "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
                        "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
                        "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
                        "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
                        "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm1 <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                             accntsTree = accountsTree, ptf = ptf, curr = currency,
                             timeline = tl, serverURL = serverURL) 
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   
   marketData <- list(rfx)
   msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                               rfxs = marketData, yc = YieldCurve())                
   msg2 <- generateEvents(host= fm1)
   msg3 <- events2dfByPeriod(host= fm1)
   msg4 <-  nominalValueReports(host = fm1)
   msg4
   fm1$currentScenarioAnalysis$nominalValueReports[[1]]
   names(fm1$currentScenarioAnalysis$nominalValueReports )
   fm1$currentScenarioAnalysis$nominalValueReports[["ann003"]]
   
# ************
# Test 7. Account Aggregated Nominal Value Reports  - tree presentation 
   # *************
# Test 7.0 startup sequence - test context 
#  create a ScenarioAnalysis - now with an AccountsTree Included  
rm(list=ls())
   fmID       <- "fm001"
   fmDescr    <- "test Financial Model logic with example"
   entprID    <- "modelBank01"
   currency   <- "USD"
   serverURL  <- "https://demo.actusfrf.org:8080/" 
   yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
                        "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
                        "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
                        "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
                        "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
                        "        - ocf008\n")
   accountsTree <- AccountsTree(yamlstring)
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/TestPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                  reportCount=3, periodCount = 6)  
   fm1 <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                             accntsTree = accountsTree, ptf = ptf, curr = currency,
                             timeline = tl, serverURL = serverURL) 
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   
# Test 7.1 empty ScenarioAnalysis
scna <- ScenarioAnalysis() 

# Test 7.2 explicit  create of ScenarioAnalysis with a cloned scenarioAccounts
rfxs <-list(rfx)
scnID <- "UST5Y_fallingRates"
yc<- YieldCurve()
scna <- ScenarioAnalysis(scenarioID= scnID, marketData= rfxs, 
                         yieldCurve = yc, accounts = accountsTree)
marketData <-list(rfx)
   msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                               rfxs = marketData, yc = YieldCurve())  
scaccnts <- fm1$currentScenarioAnalysis$scenarioAccounts
print(scaccnts$root,"actusCIDs", "nodeID")

# Test 7.3 Now through to nominal Value
rm(list=ls())
  fmID       <- "fm001"
  fmDescr    <- "test Financial Model logic with example"
  entprID    <- "modelBank01"
  currency   <- "USD"
  serverURL  <- "https://demo.actusfrf.org:8080/" 
  yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
                       "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
                       "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
                       "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
                       "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
                       "        - ocf008\n")
  accountsTree <- AccountsTree(yamlstring)
  mydatadir <- "~/mydata"
  installSampleData(mydatadir)
  cdfn  <- "~/mydata/TestPortfolio.csv"
  ptf   <-  samplePortfolio(cdfn)
  tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                 reportCount=3, periodCount = 6)  
  fm1 <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                            accntsTree = accountsTree, ptf = ptf, curr = currency,
                            timeline = tl, serverURL = serverURL) 
  rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
  rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
  marketData <-list(rfx)
  scnID <- "UST5Y_fallingRates"
  yc<- YieldCurve()  
  msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                              rfxs = marketData, yc = YieldCurve())                
  msg2 <- generateEvents(host= fm1)
  msg3 <- events2dfByPeriod(host= fm1)
  msg4 <-  nominalValueReports(host = fm1)
  msg4
  fm1$currentScenarioAnalysis$nominalValueReports[[1]]
  names(fm1$currentScenarioAnalysis$nominalValueReports )
  fm1$currentScenarioAnalysis$nominalValueReports[["ann003"]]
  
  msg5 <- accountNMVreports(host = fm1)
  msg5
  getNMVreports(fm1)
  showNMVreports(fm1) 
  
# Test 8 - Compute liquidity reports 
# Test 8.1  - non exported method on ScenarioAnalysis 
  # Test File to develop Financial Model Liquidity Reporting 
  # April 2024 
  rm(list=ls())
  fmID       <- "fm001"
  fmDescr    <- "test Financial Model logic with example"
  entprID    <- "modelBank01"
  currency   <- "USD"
  serverURL  <- "https://demo.actusfrf.org:8080/" 
  yamlstring <- paste0("\nname:  a Model Bank\nAssets:\n  Current:\n     actusCIDs:\n",
                       "        - pam001\n        - pam002\n        - ann003\n  ShortTerm:\n",
                       "     actusCIDs:\n        - pam004\n        - ann005\n  LongTerm:\n",
                       "     functionIDs:\n        - edf006\nLiabilities:\n  Debt:\n     actusCIDs:\n",
                       "        - pam007\n  Equity:\nOperations:\n  Cashflows:\n     functionIDs:\n",
                       "        - ocf008\n")
  accountsTree <- AccountsTree(yamlstring)
  mydatadir <- "~/mydata"
  installSampleData(mydatadir)
  cdfn  <- "~/mydata/TestPortfolio.csv"
  ptf   <-  samplePortfolio(cdfn)
  tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
                 reportCount=3, periodCount = 6)  
  fm1 <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                            accntsTree = accountsTree, ptf = ptf, curr = currency,
                            timeline = tl, serverURL = serverURL) 
  rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
  rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
  
  marketData <- list(rfx)
  msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                              rfxs = marketData, yc = YieldCurve())                
  msg2 <- generateEvents(host= fm1)
  msg3 <- events2dfByPeriod(host= fm1)
  msg4 <-  nominalValueReports(host = fm1)
  msg4
  #  ***** liquidity report testing from here
  msg5 <- liquidityReports(host = fm1) 
  
  
  
  msg5a <- liquidityReports(host = fm1$currentScenarioAnalysis, tl= fm1$timeline)
  msg5
  scna<- fm1$currentScenarioAnalysis
  typeof(scna$liquidityReports)
  length(scna$liquidityReports)
  scna$liquidityReports[["ann003"]]
  scna$liquidityReports[["pam007"]]
  scna$liquidityReports[["pam004"]]
  scna$liquidityReports[["pam001"]]  

   