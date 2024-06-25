# ScenarioAnalysis.R  FEMS dev code by Francis Parr April 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class ScenarioAnalysis
# Performs liquidity, income and valuation analyses for a specific risk 
# scenario on the portfolio of ACTUS contracts, specified timeline, serverURL
# passed in a parameters and typically coming from a financialModel.
# It creates and saves lists of cashflow events, a dataframe with bucketized
# liquidity events, and vectors of liquidity, valuations and income reports,
# for the report dates in the input Timeline. It has a scenario specific clone
# of the parent FinancialModel AccountsTree to organize NominalValue, Liquidity
# and NetPresentValue reporting (for the parent Financial Model ) 
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
              netPresentValueReports = "list",
              scenarioAccounts = "AccountsTree",
              logMsgs = "list"
            ))
# **************************************
# constructor ScenarioAnalysis(...) for a scenario analysis object
# *************************************
#  **** Generic ScenarioAnalysis(<>) ********
# Defines generic S4 constructor method for class ScenarioAnalysis
setGeneric("ScenarioAnalysis",
           function(scenarioID, marketData, yieldCurve, accounts)
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
#' @param accountsTree an accountsTree (from owning financial Model)
#' @return   initialized ScenarioAnalysis object 
#' @include Accounts.R
#' @export
setMethod("ScenarioAnalysis", c(scenarioID = "character",
                                marketData = "list",
                                yieldCurve = "YieldCurve",
                                accounts   = "AccountsTree"),
          function(scenarioID,marketData,yieldCurve, accounts) {
            scna <- ScenarioAnalysis()
            scna$scenarioID <- scenarioID
            scna$marketData <- marketData
            scna$yieldCurve <- yieldCurve
            scna$cashflowEventsLoL <- list()
            scna$contractLiquidityVectors <- list()
            scna$liquidityReports <- list()
            scna$incomeReports <- list()
            scna$nominalValueReports <-list()
            scna$netPresentValueReports <- list()
            scna$scenarioAccounts <- clone(accounts)
            scna$logMsgs <- list() 
            return(scna)
          })

# ********* GENERIC METHODS with ScenarioAnalysis as lowest level host
# ************************************************************************
# events2dfByPeriod(<ContractAnalysis>,  )
# ************************************************************************
# ****** generic method first - a ContractAnalysis is the only parameter 

setGeneric("events2dfByPeriod",
           function(host, tl)  
           { standardGeneric("events2dfByPeriod") }
)

# **************************************
# Method:  nominalValueReports(...) 
# *************************************
# A generic method for creating and saving a list of nominalValueReports  
setGeneric("nominalValueReports",
           function(host, ptf, tl, cid) 
           { standardGeneric("nominalValueReports") }
)

# **************************************
# Method:  liquidityReports(...) 
# *************************************
# A generic method for creating and saving a list of liquidityReports
# replaces liquidityByPeriod2Vec() and lv2liquidityReports() in 
# in ContractAnalysis.R (now deprecated)

setGeneric("liquidityReports",
           function(host, tl) 
           { standardGeneric("liquidityReports") }
)

# **************************************
# Method:  netPresentValueReports(...) 
# *************************************
# A generic method for creating and saving a list of NetPresentValue(NPV) 
# reports indexed by contractID with each report a value vector. Instances
# in ScenarioAnalysis.R and FinancialModel.R  Parameter host is required, 
# timeline is only in ScenarioAnalysis case 

setGeneric("netPresentValueReports",
           function(host, tl) 
           { standardGeneric("netPresentValueReports") }
)

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
            df1 <- eventsLoL2DF(host$cashflowEventsLoL)
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
            # subset df: this cid, report horizon rows; 
            #            <periodIndex, nominalValue> cols
            nreps <- tl$reportCount
            df1 <- df[(df$contractId == cid) & (df$periodIndex <= nreps),]
            df2 <- df1[c("periodIndex","nominalValue")]
            # Keep the last-in-period event rows - discard earlier event rows  
            df3 <- df2[sapply(unique(df2$periodIndex), function(x)
              max(which(x == df2$periodIndex)) 
            ),]
            # get nominalValue of contract cid at statusDate from input ptf
            # (contracts are in portfolio order in df3) 
            cntr <- ptf$contracts[[ match(cid,unique(df$contractId)) ]]
            
            if (cntr$contractTerms["contractRole"] == "RPA" ) { 
               sign <- +1 
            } else {
               sign <- -1 
            }
            
            # **** add check for RPL and errors if other contractRole value
            nvsd  <- sign * as.numeric(cntr$contractTerms["notionalPrincipal"])
            
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
            # pass2 report: forward fill inactive period NAs from preceding
            #               report value; also convert numeric, add report date
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
            # iterate through cids, build keyed list of NVreport vectors 
            nvrklist <- list()
            for ( cid  in unique(df$contractId) ) {
              nvrklist[[cid]] <- nominalValueReports(host=host,ptf,tl,cid= cid)
            }
            host$nominalValueReports <- nvrklist
            msg <- "NominalValue reports generated"
            return(msg)
          })


setMethod("accountNMVreports",
          c(host = "ScenarioAnalysis", vlen = "numeric", vnames = "character"), 
          function(host, vlen, vnames ){ 
            return(accountNMVreports( host = host$scenarioAccounts$root,
                                        vlen = vlen, vnames = vnames, 
                                        cidNMVlist = host$nominalValueReports
                                        ))
          }
)
# *****************
# liquidityReports(host = ScenarioAnalysis, tl = Timeline )
# *****************
#  This method computes the liquidity reports for a ScenarioAnalysis using 
#  previously computed cashflowEventsByPeriod and timeline passed in as input 
#  parameter from the parent FinancialModel. The list of liquidityReport vectors
# is saved in the Scenario Analysis liquidtyReports attribute. It is keyed by
# contractID. Each report vector has timeline$reportCount +1  values because
# there is a value report for statusDate and reports for each period. 
#
#  Algorithm: 
#.   (1) filter cashflowevents df: event rows in reported periods only
#    (2) aggregate cashflow amounts for each contract x period combination
#    (3) split the dataframe into a list of rows by cid : each row has
#       (a) cid - repeated
#       (b)  vector of periods in which at least one cashflow occurred, 
#       (c) sum of cashflow amounts of that <contract x period> 
#    (4)  step through the cids, same order in aggregate list and in the 
#         cashflowEventsByPeriod df: function list2report creates a report
#         vector of the correct length all zeros - using timeline, then insert
#         values from periods with cashflows into correct slot in vector.
#    (5) Resulting list of liquidity vectors is saved in the host 
#        scenarioAnalysis and a log message is returned 
 
setMethod(f = "liquidityReports",
          signature = c(host = "ScenarioAnalysis", tl = "Timeline"),
          definition = function(host, tl) {
            # subset cashflowEventsByPeriod periodIndex in 1:tl$reportCount 
            df1 <- subset(host$cashflowEventsByPeriod, 
                          periodIndex %in% 1:tl$reportCount)
            df2 <- aggregate(df1$payoff, 
                             by=c(cid= list(df1$contractId), 
                                  period= list(df1$periodIndex)), FUN=sum)
            df2rows <- lapply(split(df2,df2$cid), function(y) as.list(y)) 
            cids <- unique(host$cashflowEventsByPeriod$contractId)
            # the first date in tl$periodDateVector is statusDate which is 
            # NOT a liquidity reporting date - but the next reportCount are.
            rptdates <- as.character(
              tl$periodDateVector[2:(tl$reportCount+1)]
            )
              
            lqlist1 <- list()
            for ( cx in 1:length(cids)) {
              lqlist1[[df2rows[[cx]]$cid[1]]] <- 
                list2report(df2rows[[cx]]$period,
                            df2rows[[cx]]$x,
                            tl$reportCount,
                            as.character( 
                                tl$periodDateVector[2:(tl$reportCount+1)]
                                  )
                              )  
            } 
            host$liquidityReports <- lqlist1
            logMsg <- paste0 ("Liquidity reports for scenario ", host$scenarioID, 
                      " generated - OK")
            return(logMsg)
          }
)

# ******** list2report() *******
# convert a list of values indexed by position to a report vector
# posnv - vector of positions ( periods) having a value
# valsv - vector of values - same pength as posnv
# number of reports - length of output vector 
# rptdatev - report date strings 
list2report <- function(posnv, valsv, nrpts, rptdatev){
  rptv <- rep(0,nrpts)
  for (i in seq(1: length(posnv))) {
    rptv[posnv[i]]<- valsv[i]
  }
  names(rptv) <- rptdatev
  return(rptv)
}

# ****************
# accountLQreports(host=ScenarioAnalysis, .....)
#  method instance to compute  aggregatedLiquidity reports for  each account node
#  in the scenarioAccounts tree. Generic definition is in: Accounts.R

setMethod("accountLQreports",
          c(host = "ScenarioAnalysis", vlen = "numeric", vnames = "character"), 
          function(host, vlen, vnames ){ 
            return(accountLQreports( host = host$scenarioAccounts$root,
                                      vlen = vlen, vnames = vnames, 
                                      cidLQlist = host$liquidityReports
            ))
          }
)

# *****************
# netPresentValueReports(host = ScenarioAnalysis, tl = Timeline )
# *****************
#  This method computes the NPV reports for a ScenarioAnalysis using previously
#  computed cashflowEventsByPeriod and timeline passed in as an input parameter
#  from the parent FinancialModel. The list of NPVReport vectors, keyed by cid
#  is saved in the Scenario Analysis netPresentValueReports attribute. Each 
#  report vector has timeline$reportCount +1 values: there is a value report for
#  statusDate and reports for the end of each report period in the timeline. 
#
#  Algorithm: 
#   (1) loop:  for each report 
#    (2) filter cashflowevents df: for report rx FUTURE cashflows only.
#    (3) add a column with discounted (future) cashflows
#        (a) (column of discountFactors ReportDate -> cashflow date) * payoffs
#    (4) aggregate cashflow amounts for each contract 
#    (5) put back npv = 0 for mature / terminated contracts with no FUTURE cfs
#  (6) insert npv report columns into npvsdf - row per cid, npv col each report
#  (7) split npvsdf into list of rows; convert to indexed list of report vectors
#  (8) save in host scna$netPresentValueReports; return message to caller 


setMethod(f = "netPresentValueReports",
          signature = c(host = "ScenarioAnalysis", tl = "Timeline"),
          definition = function(host, tl) {
            df <- host$cashflowEventsByPeriod
            nreps <- tl$reportCount
            cids <- unique(df$contractId)
            npvsdf <- data.frame(cids = cids)
            ncids <- length(cids)
            repdates <- as.character(tl$periodDateVector[0:nreps+1])
            # print(paste0(" *** repdates has length", length(repdates)))
            for ( repx in seq(1,nreps+1) ) {
              dfrx <- df[df$periodIndex > (repx-1),] #  repx==1 case (statusDate) no filtering
              dfrx["discountedCashflows"] <- 
                dfrx$payoff * getDiscountFactor(host$yieldCurve, repdates[repx],
                                                substr(dfrx$time,1,10),0)
              dfaggr <- aggregate(dfrx$discountedCashflows, 
                                  by= list(dfrx$contractId), FUN=sum)
              # the aggregation above does NOT preserve contractId order     
              # so do a match on the contractid to sort into cashflowslist order 
              allnpvs <- rep(0,ncids)
              allnpvs[match(dfaggr$Group.1,cids)] <- dfaggr$x
              npvsdf[repdates[[repx]]] <- allnpvs  
            }
            npvrows <- lapply(split(npvsdf,npvsdf$cids), function(y) as.list(y)) 
            host$netPresentValueReports <- list()
            for (lx in npvrows) {
              host$netPresentValueReports[[lx$cid]] <- 
                unlist(lx[seq(2, tl$reportCount +2)])
            }
            msg <- paste0("netPresentValueReports generated for scenario ", 
                          host$scenarioID)
            return(msg)
          })

setMethod("accountNPVreports",
          c(host = "ScenarioAnalysis", vlen = "numeric", vnames = "character"), 
          function(host, vlen, vnames ){ 
            return(accountNPVreports( host = host$scenarioAccounts$root,
                                      vlen = vlen, vnames = vnames, 
                                      cidNPVlist = host$netPresentValueReports
            ))
          }
)
