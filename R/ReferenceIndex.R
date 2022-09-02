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


# setMethod(f = "Index",signature = c("numeric", "ANY", "character"),
#          definition = function(data, charvec, label, ...){
#            object <- new("ReferenceIndex")
#            object$data <- timeSeries(data = data,
#                                      charvec = charvec,
#                                      units = "Values", ...)
#            object$label <- label
#            return(object)
#          })

#setMethod(f = "Index",signature = c("matrix", "ANY", "character"),
#          definition = function(data, charvec, label, ...){
#            object <- new("ReferenceIndex")
#            object$Data <- timeSeries(data = data,
#                                      charvec = charvec,
#                                      units = "Values", ...)
#            object$label <- label
#            return(object)
#          })

#setMethod(f = "Index",signature = c("timeSeries", "missing", "character"),
#          definition = function(data, charvec, label, ...){
#            object <- new("ReferenceIndex")
#            object$Data <- data
#            object$label <- label
#            return(object)
#          })

# sst("ReferenceIndex>, <list> ) removed -introduces FEMS:: FNP March 2022
# setMethod(f = "set", signature = c("ReferenceIndex", "list"),
#          definition = function(object, what, ...){
#            par.names <- names(what)
#            for (i in par.names) {
#              if (FEMS:::is.valid.index.set.field(i)) {
#                value <- what[[i]]
#                switch(i,
#                       label = {
#                         object$label = value
#                       },
#                       Data = {
#                         object$Data = timeSeries(data = as.numeric(value$Values),
#                                                  charvec = as.character(value$Dates),
#                                                  units = "Values")
#                       } )
#              } else {
#                warning(paste("field ", i, " does not exist, cannot assign value!", sep=""))
#              }
#            }
#          })



# setMethod get(<MarketIndex>, ,,,) removed - includes FEMS:: - FNP march 2022
#get(ind, "MarketObjectCode")

# setMethod(f = "get", signature = c("ReferenceIndex", "character"),
#          definition = function(object, what, ...){
#            out <- list()
#            if (length(what) == 1 && tolower(what) == "all") {
#              what <- FEMS:::validIndexGetFields()
#            for (i in what) {
#              if (is.valid.index.get.field(i)) {
#              out[[i]] <- switch(i,
#                                   label = {
#                                     object$label
#                                     },
#                                   Dates = {
#                                     rownames(idx$Data)
#                                     },
#                                   Values = {
#                                     object$Data$Values
#                                     },
#                                   Data = object$Data
#                )
#              } else {
#                warning(paste("field ", i, " does not exist, cannot get value!", sep = ""))
#              }
#            }
#            if (length(out) == 1) {
#              out <- out[[1]]
#            }
#            return(out)
#          })

setMethod(f = "valueAt", signature = c("ReferenceIndex", "character"),
          definition = function(object, at, ...){
            datums <- sort(as.Date(unlist(rownames(object$Data))))
            bool_matrix <- t(sapply(at, function(x) datums <= as.Date(x)))
            indices <- unname(apply(bool_matrix,1,function(x) max(which(x))))
            return(object$Data[,"Values"][indices])
          })
# FNP comment out the show function below which seems to have an issue
# not aware of any use in FEMSdev01
# setMethod(f = "show", signature = c("ReferenceIndex"),
#          definition = function(object){
#            cat(paste0("Label: ", object$label,", Base: ",object$base,"\n"))
#            print("Time Series:")
#            print(object$data)
#          })


## -----------------------------------------------------------------
## helper methods
# existing fields in the java class
validIndexSetFields <- function() {
  return(c(
    "data", "label", "base"
  ))
}
is.valid.index.set.field <- function(x) {
  valid <- validIndexSetFields()
  return(x %in% valid)
}
validIndexGetFields <- function() {
  return(c(
    "label", "date", "value", "data"
  ))
}

is.valid.index.get.field <- function(x) {
  valid <- validIndexGetFields()
  return(x %in% valid)
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
