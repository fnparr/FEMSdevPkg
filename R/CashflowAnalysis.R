# CashflowAnalysis.R  FEMS dev code by Francis Parr Jan 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class CashflowAnalysis
# Performs liquidity, income and valuation analyses on portfolio of contracts
# for specified timeline, risk scenario and yield curve 
# creates and saves a dataframe with (liquidityD,income, NPV) for each report
# date in the defined timeline 
# **************************************************
# defines: class CashflowAnalysis, CashflowAnalysis() constructor,
# defines and exports:
#    CashflowAnalysis(<enterprise inf>, <portfolio>,<timeline> ...)
#    setPortfolio(<cflana>,<portfolio>)
#    setScenario(<cflana>, <scenario>)
#    simulateContracts(cflana>) 
#    setYieldCurve(<cflana>, <yieldCurve>) )
#    cashflowEventsByPeriod(cfla)
#    doAnalysis(<cflana>)
#.   getAnalysisReports(<cflana>)
# *********************************************************************
# class CashflowAnalysis
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
setRefClass("CashflowAnalysis",
            fields = list(
              analysisID = "character",
              analysisDescription = "character",
              enterpriseID = "character",
              yieldCurve = "YieldCurve",
              portfolio = "Portfolio",
              currency = "character",
              scenario = "list",
              actusServerURL = "character",
              timeline = "Timeline",
              cashflowEventsLoL = "list",
              cashflowEventsByPeriod = "data.frame",
              contractLiquidityVectors = "list",
              liquidityReports = "list",
              incomeReports = "list",
              allReports = "data.frame"
            ))
# **************************************
# constructor CashflowAnalysis(...) for a cash flow analysis object
# *************************************
#  **** Generic CashflowAnalysis(<>) ********
# Defines generic S4 constructor method for class CashflowAnalysis
setGeneric("CashflowAnalysis",
            function(analysisID, analysisDescription, enterpriseID,
                          yieldCurve, portfolio, currency, scenario, 
                          actusServerURL, timeline
                          ) { standardGeneric("CashflowAnalysis") }
           )
#  ***** No parameters CashflowAnalysis( )
# CashflowAnalysis ( )  - no parameters instance of CashflowAnalysis()  
#   no parameters method for internal use only 
# Creates an empty CashflowAnalysis with no attributes initialized. 
# return  S4 reference with class=CashflowAnalysis no attributes initialized.
setMethod("CashflowAnalysis", c(), 
          function(){ return( new("CashflowAnalysis")) }
          )

# ************************************************************************
# cashflowAnalysis( < > ) constructor to create/initialize a CashflowAnalysis
# ************************************************************************
#' CashflowAnalysis(analysisID, analysisDescription, enterpriseID, yieldCurve, 
#'                  portfolio, currency, scenario, actusServerURL, Timeline )
#'
#'   This method is used to start a cashflow analysis of contract holdings of 
#'   an enterprise. The user supplies information specifying the analysis to 
#'   be performed. The method is a constructor returning an initialized
#'   S4 CashflowAnalysis object which is then used to step through the analysis
#'   process saving results at each step. The completed analysis will provide 
#'   projected liquidity change, income and valuation for each held contract 
#'   at a set of specified report periods for a specified risk scenario. The
#'   risk scenario is defined as a list of projected market interest and 
#'   possibly stock price movements over time. Contract cashflow behavior is 
#'   simulated by calling out to an identified ACTUS server.  Input Yield curve
#'   data is used to set discounting and risk free groth factors used to 
#'   determine a projected contract value at each defined future report time for
#'   each contract in the enterprise portfolio. 
#'   
#'   Steps in the CashflowAnalysis are: (1) Initialization (2) Portfolio 
#'   simulation generating projected cashflow behavior for each contract 
#'   (3) Bucket the cashevents for each contract to enable reporting for each
#'   period in the timeline (4) Compute contract income for each report period 
#'   based in non principal flows for each contract (5) Compute contract 
#'   liquidity for each report period based on all period cashflows for each
#'   (6) compute NetPresentValue for each contract at the start time and at each
#'   report point using aggregated bucket flows for the contract and a 
#'   correction for residual cashflow project to occur beyond the bucket horizon
#'   - applying discounting and growth factors derived from the supplied Yield 
#'   Curve Data (7) Computing some key ratios for the Enterprise based on these 
#'   results    
#'
#' @param analysisID  character: a unique ID for this Cashflow Analysis
#' @param analysisDescription character: a short text describing this analysis
#' @param enterpriseID  character: a unique ID for the enterprise being analysed 
#' @param YieldCurve S4 YieldCurve object used for NetPresentValue discounting
#' @param Portfolio S4 Portfolio object with ACTUS contract enterprise holdings
#' @param currency character label "USD", "CHF" to be used for reports
#' @param scenario list of S4 RiskFactors for  projected market index values
#' @param actusServerURL character - url of an ACTUS server simulating contracts
#' @param timeline S$ Timeline object: future times for which reports generated   
#' 
#' @return    CashflowAnalysis S4 object: initialized/ready for simulation step
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- CashflowAnalysis( analysisID = "cfla001",
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001",
#'                              yieldCurve = YieldCurve(),
#'                              portfolio =  ptf,
#'                              currency = "USD",
#'                              scenario = list(rfx),
#'                              actusServerURL = serverURL,
#'                              timeline = Timeline())
#'  }
#'
setMethod("CashflowAnalysis", 
          c ( analysisID = "character",
              analysisDescription = "character",
              enterpriseID = "character",
              yieldCurve = "YieldCurve",
              portfolio = "Portfolio",
              currency = "character",
              scenario = "list",
              actusServerURL = "character",
              timeline = "Timeline"
              ), 
           function (analysisID, analysisDescription, enterpriseID, yieldCurve, 
                     portfolio, currency, scenario, actusServerURL, timeline) {
            cfla <- CashflowAnalysis()
            cfla$analysisID <-          analysisID
            cfla$analysisDescription <- analysisDescription
            cfla$enterpriseID <-        enterpriseID
            cfla$yieldCurve <-          yieldCurve
            cfla$portfolio  <-          portfolio
            cfla$currency <-            currency
            cfla$scenario <-            scenario
            cfla$actusServerURL <-      actusServerURL
            cfla$timeline <-            timeline
            return(cfla)
          })

# ************************************************************************
# generateEvents(<CashflowAnalysis> )
# ************************************************************************
#' generateEvents(<cashflowAnalysis>)
#'
#'   The generateEvents(CashflowAnalysis) function takes as input a S4
#'   CashflowAnalysis object with the following attributes set: (1) the 
#'   portfolio of contracts held by the enterprise (2) the actusServerURL
#'   and (3) the scenario / riskFactor list to be used. The function sends a 
#'   simulation request to the designated ACTUS server with the contract and 
#'   risk data and saves the results of the simulation in the cashflowAnalysis
#'   The method return a log message with a report on which contracts were 
#'   successfully simulated
#'
#' @param cfla  CashAnalysis S4 object with portfolio, actusServer and risk data
#' @return      Log summarizing which contracts were successfully simulated 
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- CashflowAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = Timeline())
#'    logMsgs  <- generateEvents(cfla = cfla)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(ptf="missing", serverURL="missing", 
                         riskFactors="missing", cfla = "CashflowAnalysis") ,
           definition = function(cfla){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(cfla$portfolio,
                                                cfla$actusServerURL,
                                                cfla$scenario
                                                )
             if (simulationRsp$status_code == 200 ){
                cfla$cashflowEventsLoL <- content(simulationRsp)
                logmsg <- "Contract simulations were successful"
             }
             else {
                cfla$cashFlowEvents <- NULL
                logmsg <- paste0("Contract simulation error. status_code= ",
                                 simulationRsp$status_code,
                                 "Error info= ", response_content$error)
             }
             return(logmsg) 
           }
)

# ************************************************************************
# events2dfByPeriod(<CashflowAnalysis> )
# ************************************************************************
# ****** generic method first - a CashflowAnalysis is the only parameter 

setGeneric("events2dfByPeriod",
           function(cfla) 
             { standardGeneric("events2dfByPeriod") }
)
# ***** method instance   signature = (<cashflowAnalysis>)   
#' events2dfByPeriod(<cashflowAnalysis>)
#'
#'   This method reorganizes a list(by contract) of lists of cashflow events
#'   into a data frame with columns for: contractID, period, and for each 
#'   ACTUS cashflow event field. The input cashflowAnalysis object myst be in 
#'   the following state: (1) portfolio, and timeline fields must be initialized
#'   (2) statusDate of the timeline must be the same as statusdate of all 
#'   contracts in the portfolio (3) generateEvents(cfla) must have been run to 
#'   populate cfla$cashflowEventsLoL, and (4) the status of each contract 
#'   simulation must be "Success" . You can check this using: 
#'  > unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)})) 
#'  
#'   If these conditions are met, events2dfByPeriod() will reorganize the data
#'   in cfla$cashflowEventsLoL as a dataframe with columns: 
#'      TBD
#'   and save that as cfla$cashflowEventsByPeriod for use in subsequent analysis
#'   steps on the cashflowAnalysis object. 
#'   
#'   A text message is returned reporting on any issues in this processing step.
#'   
#'   Processing steps: (0) check valid cfla$cashflowEventsLoL, (1) merge 
#'   eventsLOL into eventsDF, (2) add periodIndex column,  (3) sort by 
#'   (contractID, periodIndex), (4) save as cfla$cashFlowEventsByPeriod. 
#'   
#' @param cfla  CashAnalysis S4 object with portfolio, actusServer and risk data
#' @return      Log summarizing which contracts were successfully simulated 
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
#'    ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- CashflowAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(cfla = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(cfla= cfla2015)
#' } 
setMethod (f = "events2dfByPeriod", 
           signature = c(cfla = "CashflowAnalysis") ,
           definition = function(cfla){ 
    if (! is.null(cfla$cashflowEventsLoL) && 
         all(unlist(lapply(cfla$cashflowEventsLoL,
                           function(x){return(x$status)})) == "Success" ) )
    { msg <- "OK" 
      df1 <- mergecfls(cfla$cashflowEventsLoL)
      df1["periodIndex"] <- sapply( df1$time, 
         function(x){return(date2PeriodIndex(cfla$timeline, substr(x,1,10)))})
      df2 <- df1[c("contractId","periodIndex","time","type", "payoff",
                   "currency", "nominalValue","nominalRate","nominalAccrued")]
      cfla$cashflowEventsByPeriod <- df2
    }
    else
    {  msg <- "Cannot rearrange by Period - check state of cashflowEventsLoL"}
    return(msg)
})
# **************************************
# liquidityByPeriod2vec(cfla) get liquidity change by period for each contract
# *************************************
#  **** Generic liquidityByPeriod2vec(... ) 
setGeneric("liquidityByPeriod2vec",
           function(cfla) 
           { standardGeneric("liquidityByPeriod2vec") }
)
#'  *******************************
#'   liquidityByPeriod2df(cfla= <cashflowAnalysis>). -method instance
#' *******************************
#'
#'   This method reorganizes the cashflowEventsByPeriod df to show the liquidity
#'   change from each contract for each regular period - period 999 is extracted
#'   separately for residual (far future) valuations. It saves a list of 
#'   liquidity change vectors, one for each contract. The liquidity change
#'   vectors record a net liquidity change for each period in the timeline
#'   for that contract. All eventtype payoffs contribute to 
#'   liquidity change for the period in which they occur. This liquiditByPeriod
#'   dataframe is used for: (1) liquidityReports - will be a reformatted subset
#'   using values for periodIndex in the range 1:reportCount, and (2) liquidity
#'   change for all periods is will be used in the aggregated period step of 
#'   contract valuation. 
#'  
#'   Processing steps in the method: (1) subset cashflowByPerioddf to remove"
#'   beyond the periodHorizon events; (2) Aggregate payoffs for each contract x
#'   period combination;  (3) use split on the contract id to convert the 
#'   aggregated df to a list where each element has its cid value (repeated),
#'   a vector of period indices and a vector of period net liquidity change 
#'   values. Use lapply() on this list to produce a list of 
#'   <contractId, liquidity> vector pairs. 
#'   
#' @param cfla  CashAnalysis S4 object with portfolio, actusServer and risk data
#' @return      Log summarizing whether processins was successful 
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
#'    ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- CashflowAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(cfla = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(cfla= cfla2015)
#'    logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
#' } 
setMethod(f = "liquidityByPeriod2vec",
          signature = c(cfla = "CashflowAnalysis"),
          definition = function(cfla) {
    # subset cashflowEventsByPeriod periodIndex in 1:cfla$timeline$reportCount 
    df1 <- subset(cfla$cashflowEventsByPeriod, 
                  periodIndex %in% 1:cfla$timeline$reportCount)
    df2 <- aggregate(df1$payoff, 
                     by=c(cid= list(df1$contractId), 
                          period= list(df1$periodIndex)), FUN=sum)
    df2rows <- lapply(split(df2,df2$cid), function(y) as.list(y))
    cfla$contractLiquidityVectors <- lapply( df2rows, function(z){
        lvec <- z$x     # df aggregate requires that result has name x
        names(lvec) <- z$period
        return(list(cid=z$cid[[1]],lvec= lvec))
        }      
    )
    return(msg <-"OK")
 })

# **************************************
# Method:  lv2LiquidityReports(...) 
# *************************************
#  **** Generic lv2LiquidityReports(<>) ********
# Defines generic method to map liquidity vectors list to liquidityReports
setGeneric("lv2LiquidityReports",
           function(cfla) 
             { standardGeneric("lv2LiquidityReports") }
)

#'  *******************************
#'   lv2LiquidityReports(cfla= CashflowAnalysis) -method instance
#' *******************************
#'
#'   This method generates a liquidityReport (vector) for each contract using 
#'   the data in cfla$contractLiquidityVectors and saves this as a list keyed by 
#'   contractID in cfla$liquidityReports. The method is exported
#'   
#'   Differences between liquidityReports and contractLiquidityVectors are:
#'   (1) a contractLiquidityVector can have up to cfla$timeline$periodCount 
#'   elements, (2) but it actually only has entries for periods in which at least
#'   one liquidity changing event happens ( so it can also have fewer than 
#'   cfla$timeline$reportCount entries (3) a liquidityReport vector must have 
#'   exactly reportCount elements (4) contractLiquidityVectors record the 
#'   change in liquidity for each period in which the related contract sees
#'   liquidity changing events (5) It is more convenient for liquidity
#'   reporting to state the cumulative change in enterprise liquidity caused by
#'   each contract at each report date. That way, by aggregating the 
#'   liquidIty reports for all contracts, We can immediately see if the 
#'   enterprise is solvent at each report date. 
#'   
#'   The method returns a message indicating whether processing was successful.   
#' @param cfla  CashAnalysis S4 object with portfolio, actusServer and risk data
#' @return      Log summarizing whether processing was successful 
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
#'    ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- CashflowAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(cfla = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(cfla= cfla2015)
#'    logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
#'    lofMsgs4  <- lv2LiquidityReports(cfla= cfla2015)
#' }      
setMethod(f = "lv2LiquidityReports",
          signature = c(cfla = "CashflowAnalysis"),
          definition = function(cfla) {
            rseq <- seq(1,cfla$timeline$reportCount)
            clvs <- cfla$contractLiquidityVectors
            cfla$liquidityReports  <- lapply (names(clvs), function(y){
              lv <-clvs[[y]]$lvec
              vv <- c()
              rep0 <- cumsum(unlist(sapply(rseq, function(x) {
                # not the xth element of lv but the element with name=period_x
                if ( x %in% names(lv)) vv[[x]] <- lv[[as.character(x)]]
                else  vv[[x]] <- 0
                return(vv)
              }) ) )
              names(rep0) <- rseq
              # for a list of <cid, lvec> pairs, must create a list of lists
              # use lapply() to get a list out; ok to iterate thru a base vector  
              return(list(cid= clvs[[y]]$cid, lvec=rep0))
              })
    msg <- "lv2LiquidityReport processing OK" 
    return(msg)
})