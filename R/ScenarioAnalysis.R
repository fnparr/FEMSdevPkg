# ScenarioAnalysis.R  FEMS dev code by Francis Parr April 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class ScenarioAnalysis
# Performs liquidity, income and valuation analyses for a specific risk 
# scenario on the portfolio of ACTUS contracts, specified timeline, serverURL
# passed in a parameters and typically coming from a financialModel.
# It creates and saves lists of cashflow events, a dataframe with bucketized
# liquidity events, and vectors of liquidity, valuations and income reports,
# for the report dates in the input Timeline
# **************************************************
# defines: class ScenarioAnalysis, ScenarioAnalysis() constructor,
# defines ( But does not export) :
#  -- Class FinancialModel exports version of these methods - fewer parameters
#    ScenarioAnalysis( scenarioID,marketData,yieldCurve)

#    setPortfolio(<cflana>,<portfolio>)
#    setScenario(<cflana>, <scenario>)
#    simulateContracts(cflana>) 
#    setYieldCurve(<cflana>, <yieldCurve>) )
#    cashflowEventsByPeriod(cfla)
#    doAnalysis(<cflana>)
#.   getAnalysisReports(<cflana>)
# *********************************************************************
# class ScenarioAnalysis
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
setRefClass("ScenarioAnalysis",
            fields = list(
              scenarioID = "character",
              marketData = "list",
              yieldCurve = "YieldCurve",
              cashflowEventsLoL = "list",
              cashflowEventsByPeriod = "data.frame",
              contractLiquidityVectors = "list",
              liquidityReports = "list",
              incomeReports = "list",
              nominalValueReports = "list",
              logMsgs = "list"
            ))
# **************************************
# constructor ScenarioAnalysis(...) for a scenario analysis object
# *************************************
#  **** Generic ScenarioAnalysis(<>) ********
# Defines generic S4 constructor method for class ScenarioAnalysis
setGeneric("ScenarioAnalysis",
           function(scenarioID, marketData, yieldCurve)
             { standardGeneric("ScenarioAnalysis") }
)
#  ***** No parameters ScenarioAnalysis( )
# ScenarioAnalysis ( )  - no parameters instance of ScenarioAnalysis()  
#   no parameters method for internal use only 
# Creates an empty ScenarioAnalysis with no attributes initialized. 
# return  S4 reference with class=ScenarioAnalysis no attributes initialized.
setMethod("ScenarioAnalysis", c(), 
          function(){ return( new("ScenarioAnalysis")) }
)

#  ***** Initial values ScenarioAnalysis( )
#' ScenarioAnalysis (required initialization parameters)   
#' internal use only 
#' Creates and returns S4 ref ScenarioAnalysis instance ready for contract 
#' simulationized.
#' @param scenarioID  character ID for this risk Scenario
#' @param marketData  list of risk factor reference indexes
#' @param yieldCurve  a YieldCurve - related to factors for discounting 
#' @return   initialized ScenarioAnalysis object 
#' @export
setMethod("ScenarioAnalysis", c(scenarioID = "character",
                                marketData = "list",
                                yieldCurve = "YieldCurve"),
          function(scenarioID,marketData,yieldCurve) {
            scna <- ScenarioAnalysis()
            scna$scenarioID <- scenarioID
            scna$marketData <- marketData
            scna$yieldCurve <- yieldCurve
            scna$cashflowEventsLoL <- list()
            scna$contractLiquidityVectors <- list()
            scna$liquidityReports <- list()
            scna$incomeReports <- list()
            scna$nominalValueReports <-list()
            scna$logMsgs <- list() 
            return(scna)
          })

# ************************************************************************
# generateEvents(ScenarioAnalysis, Portfolio, ServerURL )
# ************************************************************************
#' generateEvents(ScenarioAnalysis, Portfolio, ServerURL )
#'
#'   The generateEvents(ScenarioAnalysis) function takes as input: an 
#'   initialized S4 ScenarioAnalysis object with RiskFactor information, (2) a 
#'   Portfolio of ACTUS contracts to be simulated and (3) the URL of an ACTUS
#'   server to compute the cashflow events. The method sends a 
#'   JSON simulation request to the designated ACTUS server with portfolio and 
#'   risk data then saves the results of the simulation in the cashflowEventsLoL
#'   attribute of the ScenarioAnalysis. A log message is saved in the logMsgs
#'   list attribute with key "generateEvents" and returned as ouput of the 
#'   method 
#'
#' @param host  ScenarioAnalysis S4 object with risk scenario data
#' @param ptf   Portfolio of ACTUS contracts to be simulated
#' @param serverURL  character string locating ACTUS server to be used  
#' @return      Log message listing which contracts were successfully simulated 
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/TestPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    rfxs <-list(rfx)
#'    scnID <- "UST5Y_fallingRates"
#'    yc <- YieldCurve()
#'    scna <- ScenarioAnalysis(scenarioID= scnID, marketData= rfxs, 
#'                             yieldCurve = yc)
#'   logMsgs  <- generateEvents(host= scna, ptf=ptf, serverURL = serverURL)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(host = "ScenarioAnalysis", ptf="Portfolio", 
                         serverURL="character", riskFactors="missing" ) ,
           definition = function( host, ptf, serverURL){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(ptf, serverURL, host$marketData)
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
             host$logMsgs["generateEvents"]<- logmsg
             return(logmsg) 
           }
)          
 
# ***** eventsdfByPeriod instance   signature = (ScenarioAnalysis, Timeline)   
#' events2dfByPeriod(host = <ScenarioAnalysis>, tl = <Timeline>)
#'
#'   This method reorganizes a list(by contract) of lists of cashflow events
#'   into a data frame with columns for: contractID, period, and for each 
#'   ACTUS cashflow event field. The input ScenarioAnalysis object must have 
#'   run generateEvents(host = scna) to populate scna$cashflowEventsLoL, and
#'   the status of each contract simulation must be "Success" . You can check 
#'   this using: 
#'  > unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)})) 
#'  
#'   If these conditions are met, events2dfByPeriod() will reorganize the data
#'   in cfla$cashflowEventsLoL as a dataframe with columns: 
#'   and save that as scna$cashflowEventsByPeriod for use in subsequent analysis
#'   steps on the ScenarioAnalysis object. 
#'   
#'   A text message is returned reporting on any issues in this processing step.
#'   
#'   Processing steps: (0) check valid host$cashflowEventsLoL, (1) merge 
#'   eventsLOL into eventsDF, (2) add periodIndex column,  (3) sort by 
#'   (contractID, periodIndex), (4) save as host$cashFlowEventsByPeriod. 
#'            
setMethod (f = "events2dfByPeriod", 
        signature = c(host = "ScenarioAnalysis", tl = "Timeline") ,
        definition = function(host,tl){ 
          if (! is.null(host$cashflowEventsLoL) && 
              all(unlist(lapply(host$cashflowEventsLoL,
                                function(x){return(x$status)})) == "Success" ) )
          { logmsg <- "OK" 
            df1 <- mergecfls(host$cashflowEventsLoL)
            df1["periodIndex"] <- sapply( df1$time, 
                                  function(x)
                                    {return(date2PeriodIndex(tl, 
                                                             substr(x,1,10)))})
            df2 <- df1[c( "contractId","periodIndex","time","type", "payoff",
                          "currency", "nominalValue","nominalRate",
                          "nominalAccrued")]
            host$cashflowEventsByPeriod <- df2
         }
         else
         { logmsg <- "Failed - check state of cashflowEventsLoL"}
        host$logMsgs["events2dfByPeriod"]<- logmsg
        return(logmsg)
       })
 
#  **** Single contract nominalValueReports(<>) ********
#  Uses ScenarioAnalysis data( eventsByPeriod),  the input parameters ptf
#  - portfolio and tl timeline and a cid ContractID to build a nominalValue 
#  report vector for a specific contract (cid) in the input ptf portfolio. It 
#  returns the report with nominalValuations at statusDate and each report date.

setMethod(f = "nominalValueReports",
          signature = c(host = "ScenarioAnalysis", ptf = "Portfolio", 
                        tl = "Timeline", cid= "character"),
          definition = function(host, ptf, tl, cid) {
            # host <- cfla2015
            # cid <- 101
            df <- host$cashflowEventsByPeriod
            # subset df: this cid, report horizon rows; <periodIndex, nominalValue> cols
            nreps <- tl$reportCount
            df1 <- df[(df$contractId == cid) & (df$periodIndex <= nreps),]
            df2 <- df1[c("periodIndex","nominalValue")]
            # Keep the last-in-period event rows - discard earlier event rows  
            df3 <- df2[sapply(unique(df2$periodIndex), function(x)
              max(which(x == df2$periodIndex)) 
            ),]
            # get nominalValue of contract cid at statusDate from input ptf
            # (contracts are in portfolio order in df3) 
            nvsd  <- ptf$contracts[[match(cid,unique(df$contractId))
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
            rvalsF <- as.numeric(na.locf(rvals))
            names(rvalsF)<- tl$periodDateVector[1:(tl$reportCount+1)]
            return(rvalsF)  
          })

#'  *******************************
#'   nominalValues(host = ScenarioAnalysis) - NOT exported method instance
#' *******************************
#' 
#' nominalValueReports(host= ScenarioAnalysis, ptf=Portfolio, tl- Timeline) 
#' creates a list of NV reports using the eventsByPeriod data in the host 
#' ScenarioAnalysis. Inputs ptf and tl are used in this process. The nominal
#' Value reports are saved in host$nominalValueReports.
#' Each NV report is a vector of numeric values - one for statusDate and a 
#' value for each reportDate, so host$nominalValueReports is a list of 
#' list(cid,nominalValueVector) elements. 
#' The method returns a log msg - no known causes for error 
#'
#' Method nominalValueReports(host) uses host$cashflowEventsByPeriod dataframe
#' for input data except for the statusDate value for each contract which comes 
#' from the ptf input parameter and the tl timeline parameter to understand 
#' the number and dates of reports .
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
#' @param host  ScenarioAnalysis S4 object with portfolio, cashflowevents data
#' @return      Log summarizing whether processing was successful
#' @import zoo
#' @importFrom zoo na.locf  

setMethod(f = "nominalValueReports",
          signature = c(host = "ScenarioAnalysis", ptf="Portfolio", 
                        tl = "Timeline"),
          definition = function(host,ptf,tl) {
            df <- host$cashflowEventsByPeriod
            host$nominalValueReports <- lapply( unique(df$contractId), 
                                                function(cid) {
              list(cid= cid, 
                   nvreps= nominalValueReports(host=host,ptf,tl,cid= cid))
            })
            msg <- "NominalValue reports generated"
            return(msg)
          })
