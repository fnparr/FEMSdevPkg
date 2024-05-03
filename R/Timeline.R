# Timeline.R FEMS dev code by Francis Parr Jan 2024
# Licensing and Copyright notices from FEMS ( TBD) 
# **************************************************
# Timeline is an internal class not exported from FEMSDevPkg 
# User access to Timeline(startDate, monthsPerBucket, repCount bktCount) 
#  **** by including these fields in a CashflowAnalysis( ) constructor 
# provides: date2bucketIndex(date), bucketHorizon, reportNames()
# *************************************************
library(lubridate)
# *******************************************
# S4 class Timeline holds values and methods defining the timeline used for 
# each of the different analysis steps ( liquidity, income, value ) in a 
# cashflow analysis of a portfolio. Methods provided by the class: as above  
# statusDate: (1) must match statusDate of Portfolio (2) must be first day in 
# month (3) character yyyy-mm-dd format; monthsPerBucket: (1) integer (2) 
# divisor or multiple of 12 
# ******************************************
setRefClass("Timeline",
            fields = list(
              statusDate = "character",
              monthsPerPeriod = "numeric",
              reportCount = "numeric",  
              periodCount = "numeric", 
              bucketHorizon = "character", 
              reportDates = "character", 
              periodDateVector = "Date",
              periodHorizon = "Date"
            )
)
# *********************
#  constructors:  Timeline() : (), (<statusDate>, <monthsPerBucket>, 
#.                                  <reportCount>, <bucketCount>  )
setGeneric(name = "Timeline",
           def = function(statusDate, monthsPerPeriod, reportCount, periodCount) 
             {   standardGeneric("Timeline")
           })

setMethod(f = "Timeline", signature = c(),
          definition = function(){
            return(new("Timeline"))
          })

# ************************************************************************
# Timeline( < > ) constructor to create/initialize a Timeline S4 object 
# ************************************************************************
#' Timeline(statusDate, monthsPerBucket, reportCount, periodCount) 
#'
#'   This method is used to create and initialize a Timeline object which 
#'   specifies: (1) the statusDate on which current state of enterprise holdings
#'   is reported. For consistency this should also be the statusDate value 
#'   specified for all ACTUS contracts in the CashflowAnalysis portlfolio; 
#'   statusDate also needs to be a "first of month" date e.g. 2024-02-01, 
#'   2024-03-01 - to simplify the mapping of cashflow event dates into periods. 
#'   (2) monthsPerPeriod is an integer specifying the number or months between 
#'   consecutive analysis reports; usually the monthsPerPeriod will be a divisor
#'   or a multiple of 12 so that generated analysis reports are for the same 
#'   months in each year.  (3) reportCount specifies how many reports  on
#'   liquidity, income and present value should be generated for each contract
#'   (4) periodCount specifies the total number of periods into which the 
#'   cashflow events for each contract will be grouped and eventually 
#'   aggregated. We expect periodCount to be appox 2x reportCount. Valuation of
#'   contract cashflow events occurring beyond the period "horizon" is handled
#'   without aggregation - in a single open ended final period with index 999.  
#'   
#'   The initialized Timeline object is used as an input to CashflowAnalysis
#'   where it controls the timing of analysis reports, and valuation of far 
#'   future cashflows. 
#'   
#'   Example Timeline(="2024-01-01",3,4,8) creates a timeline for a cashflow 
#'   portfolio with contracts having  statusDate="2024-01-01". Reports will be
#'   quarterly. Four reports on liquidity and income will be generated, dated
#'   2024-04-01, 2024-07-01, 2024-10-01, 2025-01-01. Cashflow events occuring on
#'   or after 2024-01-01 but before 2024-04-01 will be considered period 1 
#'   are included in the 2024-04-01 report. Periods 2,3, and 4 are defined
#'   similarly. Cashflow event occuring on or after "2026-01-01" are beyond the
#'   period horizon and will be discounted and valued individually ( rather 
#'   than aggregated into a period net flow before discounting). These events 
#'   have periodIndex=999. Valuations will be generated for the 
#'   timeline statusDate 2024-01-01 and for the four subsequent report times.  
#'
#' @param statusDate character yyyy-mm-01 date=statusdate of portfolio contracts
#' @param monthsPerPeriod integer length in months of period between reports
#' @param reportCount integer number of reports(income, liquidity) to produce
#' @param periodCount integer number of periods including valuation periods
#' 
#' @return Timeline S4 object - initialized, ready to use inCashflowAnalysis()
#' @export
#' @import lubridate
#' @examples {
#' tl<-Timeline("2024-01-01", 3, 4, 8)
#' }
setMethod(f = "Timeline", 
          signature = c(statusDate="character",monthsPerPeriod="numeric", 
                        reportCount= "numeric", periodCount= "numeric"),
          definition = function(statusDate, monthsPerPeriod, reportCount, 
                                periodCount){
            tl <- Timeline()
            tl$statusDate <- statusDate
            tl$monthsPerPeriod <- monthsPerPeriod
            tl$reportCount <- reportCount
            tl$periodCount <-periodCount
            tl$periodDateVector <- 
               add_with_rollback(as_date(tl$statusDate),
                  months(seq(from=0, to=tl$periodCount)* tl$monthsPerPeriod)) 
            tl$periodHorizon <- tail(tl$periodDateVector, n=1)
            tl$reportDates <- as.character(tl$periodDateVector[1:(tl$reportCount+1)])
            return(tl)
          }
) 

# ************************
# date2PeriodIndex(timeline,eventDate)
#  takes (1) Timeline object, (2) eventDate in "yyyy-mm-dd" format 
#  returns the integer bucketIndex fpr the event; aggregate on this !
# not exported but used by all analysis steps 
# Buckets are indexed from 0.. bucketCount-1 ; 
# **********************
setGeneric(name = "date2PeriodIndex",
           def = function(timeline, eventDate){
             standardGeneric("date2PeriodIndex")
           })            
setMethod (f = "date2PeriodIndex", signature = c("Timeline","character") ,
           definition = function (timeline,eventDate) {
             if (as_date(eventDate) >= timeline$periodHorizon )
                 { index <- 999 }
             else 
                 { index <- sum(as.integer(as_date(eventDate) >= 
                                             timeline$periodDateVector) ) }
             return(index) 
           }
)

             