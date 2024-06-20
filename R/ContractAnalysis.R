# ContractAnalysis.R  FEMS dev code by Francis Parr Jan 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class ContractAnalysis
# Performs liquidity, income and valuation analyses on portfolio of contracts
# for specified timeline, risk scenario and yield curve 
# creates and saves a dataframe with (liquidityD,income, NPV) for each report
# date in the defined timeline 
# *********************
# as of 16 APril 2024, FEMSdevPKG V 1.0 ContractAnalysis is DEPRECATED
# ScenarioAnalysis and FnancialModel should used instead 
# **************************************************
# defines: class ContractAnalysis, ContractAnalysis() constructor,
# defines and exports:
#    ContractAnalysis(<enterprise inf>, <portfolio>,<timeline> ...)
#    setPortfolio(<cflana>,<portfolio>)
#    setScenario(<cflana>, <scenario>)
#    simulateContracts(cflana>) 
#    setYieldCurve(<cflana>, <yieldCurve>) )
#    cashflowEventsByPeriod(cfla)
#    doAnalysis(<cflana>)
#.   getAnalysisReports(<cflana>)
# *********************************************************************
# class ContractAnalysis
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
setRefClass("ContractAnalysis",
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
              nominalValueReports = "list",
              allReports = "data.frame"
            ))
# **************************************
# constructor ContractAnalysis(...) for a cash flow analysis object
# *************************************
#  **** Generic ContractAnalysis(<>) ********
# Defines generic S4 constructor method for class ContractAnalysis
setGeneric("ContractAnalysis",
            function(analysisID, analysisDescription, enterpriseID,
                          yieldCurve, portfolio, currency, scenario, 
                          actusServerURL, timeline
                          ) { standardGeneric("ContractAnalysis") }
           )
#  ***** No parameters ContractAnalysis( )
# ContractAnalysis ( )  - no parameters instance of ContractAnalysis()  
#   no parameters method for internal use only 
# Creates an empty ContractAnalysis with no attributes initialized. 
# return  S4 reference with class=ContractAnalysis no attributes initialized.
setMethod("ContractAnalysis", c(), 
          function(){ return( new("ContractAnalysis")) }
          )

# ************************************************************************
# initContractAnalysis(< >) creates/initializes a ContractAnalysis instance
# ************************************************************************
#' initContractAnalysis(analysisID, analysisDescription, enterpriseID, 
#' yieldCurve, portfolio, currency, scenario, actusServerURL, Timeline )
#'
#'   This method is used to start a cashflow analysis of contract holdings of 
#'   an enterprise. The user supplies information specifying the analysis to 
#'   be performed. The method is a constructor returning an initialized
#'   S4 ContractAnalysis object which is then used to step through the analysis
#'   process saving results at each step. The completed analysis will provide 
#'   projected liquidity change, income and valuation for each held contract 
#'   at a set of specified report periods for a specified risk scenario. The
#'   risk scenario is defined as a list of projected market interest and 
#'   possibly stock price movements over time. Contract cashflow behavior is 
#'   simulated by calling out to an identified ACTUS server.  Input Yield curve
#'   data is used to set discounting and risk free growth factors used to 
#'   determine a projected contract value at each defined future report time for
#'   each contract in the enterprise portfolio. 
#'   
#'   Steps in the ContractAnalysis are: (1) Initialization (2) Portfolio 
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
#' @return    ContractAnalysis S4 object: initialized/ready for simulation step
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- initContractAnalysis( analysisID = "cfla001",
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
initContractAnalysis <- function (
              analysisID = " ",
              analysisDescription = " ",
              enterpriseID = " ",
              yieldCurve = YieldCurve(),
              portfolio = Portfolio(),
              currency = " ",
              scenario = list(),
              actusServerURL = " ",
              timeline = Timeline()
              )
 {
            cfla <- ContractAnalysis()
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
          }

# ************************************************************************
# generateEvents(<ContractAnalysis> )
# ************************************************************************
#' generateEvents(<ContractAnalysis>)
#'
#'   The generateEvents(ContractAnalysis) function takes as input a S4
#'   ContractAnalysis object with the following attributes set: (1) the 
#'   portfolio of contracts held by the enterprise (2) the actusServerURL
#'   and (3) the scenario / riskFactor list to be used. The function sends a 
#'   simulation request to the designated ACTUS server with the contract and 
#'   risk data and saves the results of the simulation in the ContractAnalysis
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
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = Timeline())
#'    logMsgs  <- generateEvents(host = cfla)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(host = "ContractAnalysis", ptf="missing", 
                         serverURL="missing", riskFactors="missing") ,
           definition = function(host){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(host$portfolio,
                                                host$actusServerURL,
                                                host$scenario
                                                 )
             if (simulationRsp$status_code == 200 ){
                host$cashflowEventsLoL <- content(simulationRsp)
                logmsg <- "Contract simulations were successful"
             }
             else {
                host$cashflowEventsLoL <- list()
                logmsg <- paste0("Contract simulation error. status_code= ",
                                 simulationRsp$status_code)
   #                              "Error info= ", content$error)
             }
             return(logmsg) 
           }
)

# ************************************************************************
# events2dfByPeriod(<ContractAnalysis>,  )
# ************************************************************************
# ****** generic method defined in ScenarioAnalysis.R  

# ***** method instance   signature = (<ContractAnalysis>)   
#' events2dfByPeriod(<ContractAnalysis>)
#'
#'   This method reorganizes a list(by contract) of lists of cashflow events
#'   into a data frame with columns for: contractID, period, and for each 
#'   ACTUS cashflow event field. The input ContractAnalysis object myst be in 
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
#'   steps on the ContractAnalysis object. 
#'   
#'   A text message is returned reporting on any issues in this processing step.
#'   
#'   Processing steps: (0) check valid cfla$cashflowEventsLoL, (1) merge 
#'   eventsLOL into eventsDF, (2) add periodIndex column,  (3) sort by 
#'   (contractID, periodIndex), (4) save as cfla$cashFlowEventsByPeriod. 
#'   
#' @param host  ContractAnalysis S4 obj with portfolio, actusServer and risk data
#' @return      log msg reporting which contracts were successfully simulated 
#' @include   ScenarioAnalysis.R
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
#'    ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(host = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(host = cfla2015)
#' } 
setMethod (f = "events2dfByPeriod", 
           signature = c(host = "ContractAnalysis") ,
           definition = function(host){ 
    if (! is.null(host$cashflowEventsLoL) && 
         all(unlist(lapply(host$cashflowEventsLoL,
                           function(x){return(x$status)})) == "Success" ) )
    { msg <- "OK" 
      df1 <- mergecfls(host$cashflowEventsLoL)
      df1["periodIndex"] <- sapply( df1$time, 
         function(x){return(date2PeriodIndex(host$timeline, substr(x,1,10)))})
      df2 <- df1[c("contractId","periodIndex","time","type", "payoff",
                   "currency", "nominalValue","nominalRate","nominalAccrued")]
      host$cashflowEventsByPeriod <- df2
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
#'   liquidityByPeriod2vec(cfla= <ContractAnalysis>). -method instance
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
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(host = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(host = cfla2015)
#'    logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
#' } 
setMethod(f = "liquidityByPeriod2vec",
          signature = c(cfla = "ContractAnalysis"),
          definition = function(cfla) {
    # subset cashflowEventsByPeriod periodIndex in 1:cfla$timeline$reportCount 
    df1 <- subset(cfla$cashflowEventsByPeriod, 
                  periodIndex %in% 1:cfla$timeline$periodCount)
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
#'   lv2LiquidityReports(cfla= ContractAnalysis) -method instance
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
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(host = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(host = cfla2015)
#'    logMsgs3  <- liquidityByPeriod2vec(cfla= cfla2015)
#'    lofMsgs4  <- lv2LiquidityReports(cfla= cfla2015)
#' }      
setMethod(f = "lv2LiquidityReports",
          signature = c(cfla = "ContractAnalysis"),
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

# **************************************
# Method:  eventsdf2incomeReports(...) 
# *************************************
#  **** Generic eventsdf2incomeReports(<>) ********
# This method defines a generic method for incomeReports from eventsByPeriod df 
setGeneric("eventsdf2incomeReports",
           function(cfla) 
           { standardGeneric("eventsdf2incomeReports") }
)
#'  *******************************
#'   eventsdf2incomeReports(cfla= ContractAnalysis) -method instance
#' *******************************
#'
#'   This method generates an incomeReport (vector) for each contract using the
#'   data in cfla$cashflowEventsByPeriod (dataframe), and saves this as a list 
#'   keyed by contractID in cfla$incomeReports. The method is exported.
#'   
#'   The processing steps are: (1) subset the cashflowEvents data frame to 
#'   select events in the reportPeriods and with eventType in ("IP", "FP"). 
#'   (2) aggregate the event payoff amounts for each contractId x reportPeriod,  
#'   and (3) convert the resulting dataframe into a list of incomeReport vectors 
#'   indexed by contractID (4) for contracts in the portfolio with no income 
#'   create null income reports - every contract should have a report  
#'   
#'   Note that the incomeReports from this are "analytic" income; not values 
#'   that would be acceptable to an accountant. Also with current ACTUS event
#'   types, we do not record any income for zero coupon PAMS with a premium 
#'   discount at IED ( no event type available representing income separable from
#'   the principal flow events) 
#'   
#'   Method eventsdf2IncomeReport( ) generates income reports from the 
#'   cashflowsByPeriod df in a single method, while the corresponding liquidity
#'   reports do this with the sequence of method calls: liquidityByPeriod2vec( ) 
#'   AND lv2LiquidityReports(cfla). The reason is that the intermediate result,
#'   for liquidity- the list by contractId of vectors with aggregated liquidity 
#'   change for each period, needs to be save dand made available for contract
#'   valuations. There is no corresponding need for aggregated income by period
#'   for each contract to be saved. Also for liquidity cumulative reports are 
#'   the most useful, but income reports state income in the preceding period.  
#'   
#'   The method returns a message indicating whether processing was successful.
#'      
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
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"                                                          
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(host = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(host = cfla2015)
#'    logMsgs5  <- eventsdf2incomeReports(cfla= cfla2015)
#' } 
#'      
setMethod(f = "eventsdf2incomeReports",
          signature = c(cfla = "ContractAnalysis"),
          definition = function(cfla) {
            # step1 - subset
            df1 <- subset(cfla$cashflowEventsByPeriod,  
                              periodIndex %in% 1:cfla$timeline$reportCount  
                            & type %in%  c("IP", "FP")
                         )
            # step2 aggregate 
            df2 <- aggregate(df1$payoff, 
                             by=c(cid= list(df1$contractId), 
                                  period= list(df1$periodIndex)), FUN=sum 
                             )
            # step3 - convert to income vectors list
            df2rows <- lapply(split(df2,df2$cid), function(y) as.list(y))
            civs <- lapply( df2rows, function(z){
              ivec <- z$x     # df aggregate requires that result has name x
              names(ivec) <- z$period
              return(list(cid=z$cid[[1]],ivec= ivec))
            }) 
            rseq <- seq(1,cfla$timeline$reportCount)
            irs  <- lapply (names(civs), function(y){
              iv <-civs[[y]]$ivec
              vv <- c()
              rep0 <- unlist(sapply(rseq, function(x) {
                # not the xth element of lv but the element with name=period_x
                if ( x %in% names(iv)) vv[[x]] <- iv[[as.character(x)]]
                else  vv[[x]] <- 0
                return(vv)
              }) ) 
              names(rep0) <- rseq
              return(list(cid= civs[[y]]$cid, ivec=rep0))
            })
            # step4  - add in  0,0,0 ...0 reports for contracts with no income
            piks <- sapply(irs, function(x) return(x$cid))
            noir <- rep(0,cfla$timeline$reportCount)
            names(noir) <- rseq
            cfla$incomeReports <- lapply( 
              sapply(cfla$portfolio$contracts,
                     function(x) return (x$contractTerms$contractID)
              ),
              function(y){ 
                if (y %in% piks) 
                      return(list(cid= y, ivec= irs[[match(y,piks)]]$ivec ))
                else 
                      return (list(cid= y, ivec= noir))
              })
            msg <- "incomeReport processing OK" 
            return(msg)
         })

# *********************************************
# nominalValueReports( )  - generic method and instances 
# ********************************************
# functions to generate a list of <cid-nominalValueReportVector> pairs 
#  generic: nominalValueReports( ) ; 
#  nominalValueReports(<contractAnalysis,cid>) - NV report for one contract cid 
#  nominalValueReports(<contractAnalysis>) creates list of NV reports for all 
#. contracts in the contractAnalysis and saves in  
#  scna$nominalValueReports. Each NV report is a vector of numeric
#  the method returns a log msg - no known causes for error 
#
# nominalValueReports(scna, cid)
#
# This method instance uses the scna$cashflowEventsByPeriod dataframe 
# and returns a vector of nominalValue reports for the contract with
# contractId == cid 
# The returned reportVector has length= scna$timeline$reportCount+1) because a
# valuation at time 0  i.e. statusDate is included 
# **********
# Consistency of the reportMethods for liquidity, income, nominal
# shorter names are better: nominalValueReports() not eventsdf2liquidityReports
# value naming in each report vector - we should add this: sd, rep1, rep2 etc 
# ***
# **************************************
# Method:  nominalValueReports(...) 
# *************************************
#  generic method is defined in scenarioAnalysis.R 

#  **** Single contract nominalValueReports(<>) ********
#  Uses ContractsAnalysis data to build nominalValue report vector for a 
#  specific contract (cid) in the ContractsAnalysis$portfolio. Returns the
#  report with nominalValuations at statusDate and each report date

setMethod(f = "nominalValueReports",
          signature = c(host = "ContractAnalysis", ptf = "missing", 
                        tl = "missing", cid= "character"),
          definition = function(host,cid) {
 #  host <- cfla2015
 # cid <- 101
  df <- host$cashflowEventsByPeriod
  # subset df: this cid,  report horizon rows; <periodIndex, nominalValue> cols
  nreps <- host$timeline$reportCount
  df1 <- df[(df$contractId == cid) & (df$periodIndex <= nreps),]
  df2 <- df1[c("periodIndex","nominalValue")]
  # Keep the last-in-period event rows - discard earlier event rows  
  df3 <- df2[sapply(unique(df2$periodIndex), function(x)
    max(which(x == df2$periodIndex)) 
  ),]
  # get nominalValue of contract cid at statusDate from scna$portfolio
  # (contracts are in portfolio order in df3) 
  nvsd  <- host$portfolio$contracts[[match(cid,unique(df$contractId))
  ]]$contractTerms["notionalPrincipal"]
  # pass1 report:  has values for any "active period" report 
  rvals <- unlist(sapply(seq(1,nreps), function (i) {
    if ((i) %in% df3$periodIndex )  
      v<- df3$nominalValue[match(i, df3$periodIndex)]
    else                                  
      v<- NA
    return(v)
  }))
  # add statusDate nominalValue as "0th" report
  rvals <- unlist(append(rvals,nvsd,0))
  # pass2 report: forward fill inactive period NAs from preceding report value
  rvalsF <- na.locf(rvals) 
  
  return(rvalsF)  
})

#'  *******************************
#'   nominalValues(host = ContractAnalysis) - exported method instance
#' *******************************
#' 
#' nominalValueReports(host= <contractAnalysis>) creates list of NV reports for all 
#' contracts in the contractAnalysis and saves this in host$nominalValueReports.
#' Each NV report is a vector of numeric values - one for statusDate and a 
#' value for each reportDate, so host$nominalValueReports is a list of 
#' list(cid,nominalValueVector) elements. 
#' The method returns a log msg - no known causes for error 
#'
#' Method nominalValueReports(host) uses host$cashflowEventsByPeriod dataframe
#' for input data except for the statusDate value for each contract which comes 
#' from host$portfolio.
#' 
#' ************
#' We get nominalValue at each reportDate for a specific contract as follows: 
#' Step(1): subset the scna$cashflowEventsByPeriod dataframe to rows with this
#' cid and periodIndex == a report period i.e. periodIndex <= reportCount. 
#' Step(2): subset this dataframe to the columns: periodIndex and nominalValue.
#' Step(3): subset further: for each period make a list of eventrow indexes for
#' that period; keep the last-event-in-period, discard the others. Since the 
#' nominalValue of the contract cannot change after the last event,
#' the reported nominalValue at the time of this last event is the correct value
#' to report for the period. 
#' Step(4): convert dataframe nominalValues to a report vector with na for 
#' report periods in which there was no event generating nominalValue status
#' Step(5): add StatusDate nominalPrincipal from contractTerms as first 0th element
#' Step(6): use library(zoo) na.locf() to forward fill missing (na) values in 
#' the nominalValueReport vector with "the most recent" value from left. Since
#' there was no intervening event to change nominalValue this is still correct.
#'
#' The list of nominalValueReports for all contracts in the contractsAnalysis
#' is by reapplying the single contract algorithm to the events dataframe for
#' each unique cid in the events dataframe
#'      
#' @param host  ContractsAnalysis S4 object with portfolio, cashflowevents data
#' @return      Log summarizing whether processing was successful
#' @import zoo
#' @importFrom zoo na.locf  
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
#'    ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    tl1 <- Timeline("2015-01-01",3,4,8)
#'    cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf2015, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = tl1)
#'    logMsgs1  <- generateEvents(host = cfla2015)
#'    logMsgs2  <- events2dfByPeriod(host = cfla2015)
#'    logMsgs6  <- nominalValueReports(host = cfla2015)
#' } 
     
setMethod(f = "nominalValueReports",
          signature = c(host = "ContractAnalysis"),
          definition = function(host) {
  df <- host$cashflowEventsByPeriod
  host$nominalValueReports <- lapply( unique(df$contractId), function(cid) {
    list(cid= cid, nvreps= nominalValueReports(host=host,,,cid= cid))
  })
  msg <- "NominalValue reports generated"
  return(msg)
})


