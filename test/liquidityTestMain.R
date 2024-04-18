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
scna <- fm1$currentScenarioAnalysis
eventsdf <- scna$cashflowEventsByPeriod
head(eventsdf)
# shows that periodIndex is present 
# need a Timeline  - parameter tl 
tl$reportCount

# code pattern from old ContractAnalysis

# subset cashflowEventsByPeriod periodIndex in 1:cfla$timeline$reportCount 
df1 <- subset(scna$cashflowEventsByPeriod, 
              periodIndex %in% 1:tl$reportCount)
df2 <- aggregate(df1$payoff, 
                 by=c(cid= list(df1$contractId), 
                      period= list(df1$periodIndex)), FUN=sum)
head(df2)
df2rows <- lapply(split(df2,df2$cid), function(y) as.list(y))
# we would like liqrows list (1) indexed by cid , then indexed by period 
# we could then build liq vector by inserting zero liquidity change for each
# missing period ( in which there was no liquidity activity ) 
cids <- unique(scna$cashflowEventsByPeriod$contractId)
ncids <- length(cids)

lqlist1 <- list()
for ( cx in 1:ncids) {
  lqlist1[[cids[cx]]] <-  list(period = df2rows[[cx]]$period, x = df2rows[[cx]]$x)
}
lqlist1[[1]]
cids[cx]  
df2rows[[cx]]$period
df2rows[[cx]]$x
l1 <- list(period = df2rows[[cx]]$period, x = df2rows[[cx]]$x)
# building a report vector for a contract
nreps <- 6
y<- rep(0,nreps)
period <- c(3,6)
x <- c( 250.0, 102239.2)
for (i in seq(1: length(period))) {
  y[period[i]]<- x[i]
}
y

    
    
    