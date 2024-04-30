# *************
# npvTestMain.R - explore computing Net Present Value  - no buckets
# **************
# README unit testing for Net Present Value capability 
#  fnp April 2024 - including tests for YieldCurve, DiscountFactor 
# ********
# README. fnp Jan 2024 - to access these functions before they are merged intp
#   a published version of FEMSdevPkg 
#  Fetch the package  > devtools::github_install("fnparr/FEMSdevPkg@dev")
#  In the Rstudio "Build" tab : "Build" -> "more" -> "load All"
#  .. because for testing and learning you want access to internal not
#     exported functions and definitions 
#  **************************
#  YieldCurve tests 
# *************
# Test 0.0 create YieldCurve with valid and invalid data
# will need to clear the environment 
rm(list=ls())
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )/100
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "CONTINUOUS"
yc <- YieldCurve(ycID,rd,tr,dcc,cf)
# valid case is above  variable yc is  GOOD 
# invalid case is below -- skip this 
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
frwdRate <- getForwardRates(yc,Tfrom,Tfrom)
frwdRate
# ************
# Test 0.2  Discount and Growth Factors 
riskSpread <- 0.02
discountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
discountF
ndiscountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
ndiscountF
# does it return discount = 1.0 if Tfrom==Tto ?,
ndiscountF <- getDiscountFactor(yc,Tfrom,Tfrom,riskSpread)
ndiscountF 
Tto <- c("2025-07-01","2025-08-01")
ndiscountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
ndiscountF
Tto <- c(Tfrom,"2025-07-01","2025-08-01")
ndiscountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
ndiscountF
Tto <- "2025-07-01"
Tfrom <- c("2024-01-01","2024-02-01", Tto)
ndiscountF <- getDiscountFactor(yc,Tfrom,Tto,riskSpread)
ndiscountF
# no it returns NaN  => fix this 
growthF <- getGrowthFactor(yc,Tfrom,Tto)
growthF
# ************************
# Need some portfolio and cashflow data for serious testing of discounting 
# From  financialModelTest.R  Test 9/0 
# ********************************
# test context - run financialModelTests.R Test 9.0 through events2dfByPeriod
# Test 9.0  Aggregated Account Liquidity Reports 
# Test File to develop Financial Model Liquidity  and NPV reporting 
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
# create a sample Yieldcurve 
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )/100
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "CONTINUOUS"
ycsample <- YieldCurve(ycID,rd,tr,dcc,cf)

msg1 <- addScenarioAnalysis(fm = fm1, scnID= "UST5Y_fallingRates", 
                            rfxs = marketData, yc = ycsample )                
msg2 <- generateEvents(host= fm1)
msg3 <- events2dfByPeriod(host= fm1)
msg4 <- netPresentValueReports(host = fm1)
msg5 <- accountNPVreports(fm1)
msg5
getNPVreports(fm1)
showNPVreports(fm1)



# ********** Now explore results from netPresentValueReports()  
scna <- fm1$currentScenarioAnalysis
msg6 <- netPresentValueReports(host=scna,tl=fm1$timeline)
# netPresentValueReports(host = scna, tl = fm1$timeline)

scna$netPresentValueReports[[1]]
scna$netPresentValueReports[["ann005"]]
names(scna$netPresentValueReports)
scna$netPresentValueReports[["pam001"]]
scna$netPresentValueReports[["pam002"]]
scna$netPresentValueReports[["pam004"]]
scna$netPresentValueReports[["pam007"]]

print(scna$scenarioAccounts$root,"actusCIDs", "npv", "nodeID" )
scna$scenarioAccounts$root$npv

# Why is NPV for pam007 debt node7 showing + 321,000 should be approx -500,000?
scna<- fm$currentScenarioAnalysis
df <- scna$cashflowEventsByPeriod
df1<-df[df$contractId == "pam007",]
head(df1)
tail(df1)
length(df1)
npvs <- scna$netPresentValueReports
names(npvs)
npvs[["pam007"]]
# => the flows in df1 are all negative BUT we see + 320,000 NPV reports !!
# debug the netPresentValueCall() step 1 financialModel looks OK 
#  netPresentValue(host=scna)
host <- scna
dfall <- host$cashflowEventsByPeriod
df <- dfall[dfall$contractId == "pam007",]
nrow(df)
head(df)
tail(df)
tl <- Timeline(statusDate = "2023-01-01", monthsPerPeriod = 6, 
               reportCount=3, periodCount = 6)  
# -- commands into loop 
repx <- 1

# ********
nreps <- fm1$timeline$reportCount + 1
as.character(fm1$timeline$periodDateVector[1:nreps])


df2rows <- lapply(split(npvdf,npvdf$cids), function(y) as.list(y)) 
npvrVec <- unlist(df2rows[[1]][seq(2, tl$reportCount +2)])

# test list iteration
list1 <- list("a","b","c")
for (lx in list1) print(lx)
head(scna$cashflowEventsByPeriod)
# better to work with all 6 contracts not just ann003 
df <- scna$cashflowEventsByPeriod
nrow(df)              # number of rows
ncol(df)              # number of columns
names(df)

# speculate that df$time == df[,"time"] == df[["time"]] => YES 
dates <- df$time
dscntf <- getDiscountFactor(yc,sd, substr(dates,1,10),0)
payoff <- df$payoff
dscntdcf <- dscntf*payoff
# Add the discountedCashFlows as a column in df
df["discountedCashFlows"] <- dscntdcf
head(df)
df[200:210,]

# looks OK so go find aggregate discounted cashflows by contractID 
dfnpv <- aggregate(df$discountedCashFlows, 
                 by=c(cid= list(df$contractId)), FUN=sum)
head(dfnpv)

# looks good so build loop for npvrepsdf
# should be a function in ScenarioAnalysis.R 
# parameters host, tl 



# ann003 looks like a mortgage with monthly payments 
# lets filter cashFlowEvents to ann003 only 
df <- scna$cashflowEventsByPeriod
nrow(df)              # number of rows
ncol(df)              # number of columns
df[1:10,]             # df filtered by sequence or rows 
df1 <- df[df["contractId"]=="ann003",]  # data frame filtered for selected CID
df1[1:10,]
nrow(df1)
unique(df1["contractId"])
unique(df1["periodIndex"])
df[204:214,]
dates <- df1[,"time"]
dates[1:10]
length(dates)
sd <- fm1$timeline$statusDate
sd
# now inspect the discountFactor function to get
# example discount Factor call 
Tfrom <- "2023-01-01"
Tto <- "2024-01-01"
print (yc)
getDiscountFactor(yc,Tfrom,Tto,0)
Tto <- c("2024-01-01","2024-02-01")
getDiscountFactor(yc,Tfrom,Tto,0)
sd
