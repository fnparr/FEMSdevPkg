# MarketIndex.R  FEMSdev version edited francis Parr March 2022
#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
# Defines S4 Ref Class MarketIndex - extends RiskFactor
#
#  Reference indices define a class of market risk factors
#  that in the ACTUS model may directly affect future cash
# flows arising from a (variable Rte ) financial instrument, e.g. Inflation-
# linked bonds, or are used for valuation of instruments,
# e.g. a Stock market index when using CAPM.

#  create an ReferenceIndex object
# ind <- Index()
#
# # define time stamps and values
# times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#            "2019-01-01")
# values <- c(100, 110, 120, 130, 140)
#
# # set the MarketObjectCode and Data
# set(ind, what = list(MarketObjectCode = "CHF_SMI",
#                      Data = list(Dates = times,
#                                        Values = values)))
#
# # get MarketObjectCode
# get(ind, "MarketObjectCode")
#
# # get values of the risk factor at certain times
# valueAt(ind, "2016-01-01")
# valueAt(ind, c("2016-01-01", "2018-07-01", "2018-07-01"))

#' @include RiskFactor.R
#' @import methods
#' @importFrom methods new
#' @import timeSeries
#'
setRefClass("ReferenceIndex", contains = "RiskFactor",
            fields = list( # riskFactorID =  "character"  In RiskFactor parent
                           marketObjectCode = "character", 
                           base  = "numeric",  
                           data =  "timeSeries" 
            ))

setGeneric(name = "Index",
           def = function(rfID, moc, base, timeSeries, 
                          dates, values, fname, ...) standardGeneric("Index") )

# constructor with rfID label, base and timeSeries data
setMethod(f = "Index", signature = c("character", "character","numeric",
                                     "timeSeries"),
          definition= function(rfID, moc, base, timeSeries) {
            rfx <- new("ReferenceIndex")
            rfx$riskFactorID      <- rfID
            rfx$marketObjectCode  <- moc
            rfx$base              <- base
            rfx$data  <- timeSeries
            return(rfx)
          })
# constructor with label, base, vector of dates, vector of values
# builds the data TimeSeries from date qnd value columns
setMethod(f = "Index", signature =
                 c("character", "character", "numeric", "missing", 
                   "character","numeric") ,
          definition= function(rfID, moc, base, dates, values) {
            rfx <- new("ReferenceIndex")
            ts1 <- timeSeries(data = values, charvec = dates, units = "value")
            rfx$riskFactorID <- rfID
            rfx$marketObjectCode <- moc
            rfx$base            <- base
            rfx$data  <- ts1
            return(rfx)
          })


# ************************************************************
# preJSONts(), preJSONrf(), preJSONrfc() these functions map
#     reference Index elements to a preJSON form where calling
#     jsonlite::toJSON(preJSONrfc(rfc) , dataframe = "rows")
#     will generate valid Actus JSON for reference Indexes;
#     R dataframes map to JSON [ ] sequences;
#     R lists map to JSON { } records but
#     need unbox( ) for singleton values
#     timeSeries optimizes times, no renaming time col etc
#     time() extracts vector of times; format() converts to chars
#     paste0 to append T00:00:00 expected by Actus; reference Index list is 
#     organized
#     as list of names with unboxed values, embedded dataframe is allowed
#     a rFConn had to be a dataframe with unnamed column of risked factors
#        FNP Apr 2022
# ************************************************************

preJSONts <- function(ts) {
  return (data.frame(time = paste0(format(timeSeries::time(ts)),"T00:00:00"),
                     value = ts$value)
  )
}
# result should convert to JSON with toJSON(preJSONrf(rf),dataframe="rows")

preJSONrfx <- function(rfx) {
  return ( list(marketObjectCode= jsonlite::unbox(rfx$marketObjectCode),
                base = jsonlite::unbox(rfx$base),
                data = preJSONts(rfx$data)
               )
         )
}

preJSONrfxs <- function(rfxs) {         # work directly on riskFactors list
  rfsl <- lapply(rfxs, preJSONrfx )
  names(rfsl) <- NULL  # must clear the list names
  return(rfsl)
}

# ************************************

#'  sampleReferenceIndex     (rxdfp,rfID, moc, base)
#'  
#'     Function to read a csv file with a column of yyyy-mm-dd dates, and a 
#'     column of interest rate values. A marketObjectCode, a riskFactorID and 
#'     a numeric base value. The function returns an S4 ref to an object with
#'     class=ReferenceIndex. The marketObjectCode is the identifier used in 
#'     variable rate contracts to specify a dependency on this ReferenceIndex
#'     for setting a nominal interest rate.  The numeric value base should have 
#'     value 100 if a 3.1% interest rate appears as 3.1 in the csv file, and 
#'     value 1.0 if that rate appears as 0.031. Input parameter rfID will be 
#'     set as the unique riskFactorID of the returned ReferenceIndex object.
#'     Examples of csv files formatted with Dates and Values for ReferenceIndex
#'     creation in the sample data are: UST5Y_fallingRates.csv,
#'     UST5Y_recoveringRates.csv, UST5Y_risingRates.csv, UST5Y_steadyRates.csv .
#'
#' @param rxdfp   character  Pathname of csv data file for the referenceIndex
#' @param rfID    character  the (unique) riskFactorID for new referenceIndex
#' @param moc     character  marketObjectCode used in varying rate contracts
#' @param base    numeric    1.0 or 100  if 3 percent  as 0.03 or 3.00
#' @return                    S4 ref to a new class=ReferenceIndex object
#' @export
#' @importFrom utils read.csv
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#' }
sampleReferenceIndex <- function(rxdfp, rfID, moc, base){
   rxddf <- utils::read.csv(file = rxdfp, header=TRUE)
   rfx <- Index(rfID,moc, base,,rxddf$Date,rxddf$Rate)
   return(rfx) 
} 

# ************************************

#'  sampleReferenceIndexList(rfxsfn)
#'  
#'     Function to read a csv file with data specifying a collection of 
#'     RiskFactorIndexes. Each record in the csv file defines a different 
#'     RiskFactorIndex. The columns in the csv file are: (1) rfType,
#'     (2) marketObjectCode (3) base (4) dataPairCount (5) <for X = 1,2,3,4>:
#'     date.X ,  value.X 
#'     
#'     Input csv file format: 
#'     rfType is the fixed string "referenceIndex"; marketObjectCode is a  
#'     character string with the MOC of the reference index; base is a numeric
#'     numeric value typically 1.00 or 100 to set whether an interest rate of  
#'     5% pa is coded as 5.00 or 0.05; columns 5-13 are used to specify a list 
#'     of up to 4 <date,value> pairs for the reference index. Dates have the  
#'     format yyyy-mm-dd. Values are numeric.  
#'      
#'     Processing: the rfID for referenceIndex with marketObjectCode="moc1" is
#'                 set to "sample$moc1";  different risk scenarios may need 
#'                 different rfid's for the same marketObjectCode. Multiple
#'                 risk scenarios are not supported using this referenceIndex
#'                 data import function/format 
#'     WARNING -   this csv input data format is different from (and not 
#'                 compatible with) the csv input file format used in 
#'                 sampleReferenceIndex() function. The format used here is 
#'                 convenient for defining a set of ReferenceIndexes with very
#'                 limited data for testing portfolio simulation using a single 
#'                 risk scenario. The csv input file format used in 
#'                 sampleReferenceIndex() data import is more scalable and must 
#'                 be used when referenceIndex data will be entered for 
#'                 multiple risk scenarios.
#'
#' @param rfxsfn   character  full path filename of csv input data file 
#' @return         list of S4 objects class=ReferenceIndex 
#' @export
#' @importFrom utils read.csv
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    rfxsfn <- paste0(mydatadir,"/RiskFactors.csv")
#'    rfxsl <- sampleReferenceIndexList(rfxsfn)
#' }
sampleReferenceIndexList <- function(rfxsfn){
  rfxlist <- riskFactors_df2list(riskFile2dataframe(rfxsfn))
  return(rfxlist) 
} 

# **********************************************
# FNP testing section Mar - Apr 2022
# **********************************************

# function to create a sample ReferenceIndex riskFactor object for
# BondPortfolio.csv ie MOC == "YC_EA_AAA"
sampleReferenceIndex_YC_EA_AAA <- function(){
  values <- c(0.02, 0.03, 0.04)   # sample interest rates base = 1.0
  dates <- c("2000-01-01","2016-01-01","2017-01-01")
  rfID  <- "sample$YC_EA_AAA"
  moc   <- "YC_EA_AAA"
  base  <- 1.0
  rfndx <- Index(rfID, moc, base,, dates, values)
  return (rfndx)
}
# function to create a sample ReferenceIndex riskFactor object for
# OptionsPortfolio.csv ie MOC == "AAPL"
sampleReferenceIndex_AAPL <- function(){
  values <- c(63.70, 91.20, 115.81 )   # sample index / stock price base = 1.0
  dates <- c("2020-03-30","2020-06-30","2020-09-30")
  rfID  <- "sample$AAPL"
  moc   <- "AAPL"
  base  <- 1.0
  rfndx <- Index(rfID, moc, base,, dates, values)
  return (rfndx)
}
# function to create a sample ReferenceIndex riskFactor object for
# IND_CPI_EA needed by BondPortfolio_dev.csv contracts 115,116
sampleReferenceIndex_IND_CPI_EA <- function(){
  values <- c(1.08, 1.07, 1.06 )   # sample index / stock price base = 1.0
  dates <- c("2000-01-02","2016-01-02","2017-01-02")
  rfID  <- "sample$IND_CPI_EA"
  moc   <- "IND_CPI_EA"
  base   <- 1.0
  rfndx <- Index(rfID,moc,base,,dates,values)
}
