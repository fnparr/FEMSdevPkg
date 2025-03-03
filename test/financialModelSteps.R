# FinancialModelSteps - step through financial model analysis 
#                     - targeting exported functions from FEMSdevPkg
# ********
library(devtools)
install_github("fnparr/FEMSdevPkg")
# *********
library(FEMSdevPkg)
rm(list=ls())
# Step 1: create a string with structural information for model bank accounts
#         and ACTUS contract ids ( also function IDs) assigned to each account
yamlstring <- "
name:  modelBank
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

# Step 2: Use string above to create an accounts tree 
accountsTree <- AccountsTree(yamlstring) 
print(accountsTree$root,"actusCIDs", "nodeID")

# Step 3: Import ACTUS contract definitions for the Financial Model Portfolio
datadir <- "~/mydata"
installSampleData(datadir)
cdfn  <- "~/mydata/fmTestPortfolio.csv"
ptf   <-  samplePortfolio(cdfn)
unlist(ptf$contracts[[1]]$contractTerms)

# Step 4: Create a Timeline setting Status and report dates, period etc 
tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, reportCount=3,
               periodCount = 6)

# Step 5: create FinancialModel instance with this, ptf, accntsTree, tl
#  5.1  set up identifier, descriptors  and other scalar fields 
fmID <- "fm001"
fmDescr <- "test Financial Model logic with example"
entprID <- "modelBank01"
currency <- "USD"

serverURL <- "https://demo.actusfrf.org:8080/" 
# serverURL <- "https://dadfir3-app.zhaw.ch/"
# serverURL <- "http://ractus.ch:8080/"

# 5.2 create the financialModel
fm <- initFinancialModel(fmID=fmID, fmDescr= fmDescr, entprID = entprID,
                         accntsTree = accountsTree, ptf = ptf, curr = currency,
                         timeline = tl, serverURL = serverURL
                         )
class(fm)

# Step 6 gather scenario data and add a scenarioAnalysis to this financialModel
# 6.1 Gather reference index projections for MarketObjectCodes in this scenario
datadir <- "~/mydata"
# installSampleData(datadir)
rxdfp <- paste0(datadir,"/UST5Y_fallingRates.csv")
rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
# The 100 parameter is the base level for JSON 
marketData <-list(rfx)
# create a sample Yieldcurve 
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )/100
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "CONTINUOUS"
ycsample <- YieldCurve(ycID,rd,tr,dcc,cf)
# 6.1 addSenarioAnaysis( ) with this scnID and risk factors 
#     will set fm$currentScenarioAnalysis to be this 
addScenarioAnalysis(fm = fm, scnID= "UST5Y_fallingRates", rfxs = marketData,
                    yc = ycsample)
fm$currentScenarioAnalysis$scenarioID

# Step 7: generateEvents( ) to simulate the fm portfolio using a  risk scenario
#         set by addScenarioAnaysis()
msg1 <- generateEvents(fm)

# Step 8 events2dfByPeriod() - organize the cashflow events into period buckets 
msg2 <- events2dfByPeriod(host=fm)

# step 9 nominalValueReports(host = fm) 
msg3 <- nominalValueReports(host = fm)

# Step 10  accountsTree aggregation of NominalValue reports 
msg4 <- accountNMVreports(host = fm)
getNMVreports(fm)
showNMVreports(fm) 
showContractNMVs(fm)

#step 11
msg5 <- liquidityReports(host = fm )

#step 12
msg6 <- accountLQreports(host = fm)
getLQreports(fm)
showLQreports(fm)
showContractLQs(fm)

#step 13
msg7 <- netPresentValueReports(host = fm)

#step 14
msg6 <- accountNPVreports(host = fm)
getNPVreports(fm)
showNPVreports(fm)
showContractNPVs(fm)
print(fm$accountsTree$root,"actusCIDs")

