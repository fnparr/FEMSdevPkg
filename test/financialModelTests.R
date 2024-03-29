# FinancialModel unit test script 
# ********
library(yaml)
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
serverURL <- "http://ractus.ch:8080/"

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
accounts <- treeFromYamlString(yamlstring)
print(accounts,"actusCIDs")

# Test 2.3 Need a sample portfolio of Actus contracts with CIDs as above
#          To view TestPortfolio.csv import into Excel with "," delimiter
cdfn  <- "~/mydata/TestPortfolio.csv"
ptf <- samplePortfolio(cdfn)
unlist(ptf$contracts[[1]]$contractTerms)

# Test 2.4 Need a timeline  
tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, reportCount=3,
               periodCount = 6)

# Test 2.4 Now create financialModel 
fm <- FinancialModel(fmID = fmID,fmDescr = fmDescr, entprID = entprID,
                     accounts = accounts, ptf = ptf, timeline = tl, 
                     curr = currency, serverURL = serverURL )
class(fm)

# Test 3.0
# next Step is to run a ContractAnalysis as is with the attributes of this fm 
# Use contents of unitTestMain.R test 3.0 
fm$serverURL
unlist(fm$portfolio$contracts[[1]]$contractTerms)
fm$financialModelID
fm$financialModelDescription
fm$enterpriseID
fm$currency
# OK so far BUT we need a Scenario with YC_EA_AAA and a timeline 
# so NOT UnitTestMain Test 3.0 ..  this from test 3.1
mydatadir <- "~/mydata"
rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
# The 100 parameter is the base level for JSON 
scenario <-list(rfx)

cfla <- ContractAnalysis( analysisID = fm$financialModelID,
                          analysisDescription = fm$financialModelDescription,
                          enterpriseID = fm$enterpriseID,
                          yieldCurve = YieldCurve(),
                          portfolio =  fm$portfolio,
                          currency =   fm$currency,
                          scenario =   scenario,
                          actusServerURL = fm$serverURL,
                          timeline = fm$timeline)

# check valid ContractAnalysis object created
class(cfla)

# Test 3.1:  
# simulate the portfolio, this scenario; results go into cfla$cashflowEventsLoL
logMsgs1 <- generateEvents(cntan= cfla)
logMsgs1

# check whether each contract was successfully simulated 
unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)}))

# Test 3.1 Group events by (Timeline) periods 
logMsgs2  <- events2dfByPeriod(cfla = cfla)
logMsgs2

# check on dataframe contents 
cfla$cashflowEventsByPeriod[1:10,]
rows <-nrow(cfla$cashflowEventsByPeriod)
cfla$cashflowEventsByPeriod[(rows-10):rows,]

# Test 3.2 Generate liquidity vectors But undo cumsum if there 
logMsgs3  <- liquidityByPeriod2vec(cfla= cfla)
logMsgs3

# check
typeof(cfla$contractLiquidityVectors)
length(cfla$contractLiquidityVectors)
cfla$contractLiquidityVectors$pam001
unlist(cfla$contractLiquidityVectors)
# Some of the liquidity vectors are missing elements 
# Why are there6 periods - because 3 reports and 6 periods !
cfla$timeline$periodCount
# Contract liquidity vectors is ALL periods , not rfixed length, periodID

# Test 3.3 generate income reports
logMsgs5  <- eventsdf2incomeReports(cfla= cfla)
logMsgs5

unlist(cfla$incomeReports)

logMsgs6 <- nominalValueReports(cntan=cfla)
logMsgs6
unlist(cfla$nominalValueReports)
cfla$nominalValueReports[[1]]
cfla$nominalValueReports[[3]]
typeof(cfla$nominalValueReports[[1]]$nvreps)
# somehow the values have been converted to character 
# == need to fix this 
as.numeric(cfla$nominalValueReports[[1]]$nvreps)
fixNvrs <- function(cnvrsList){
   nnvrs <- lapply(cnvrsList, function(nvr){
     return(list(cid=nvr$cid,nvreps = as.numeric(nvr$nvreps)))
   })
}
nnvrs <- fixNvrs(cfla$nominalValueReports)
nnvrs

# Test 5:  can we display aggregated nominal Values in the accounts tree 

