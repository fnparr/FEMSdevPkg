# *************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
# Edits/updates for FEMSdev by Francis Parr April 2022
#   fnparr@gmail.com
# *************************************************************

# *******************************************
# S4 class EventSeries holds a dataframe with the cashflow events generated for
# a single Actus Contract, along with the contractID, contractType, statusDate
# contructors: EventSeries()  EventSeries(<Contract>,<rfs>) 2B exported
# ******************************************
setRefClass("EventSeries",
            fields = list(
              events_df = "data.frame",
              contractID = "character",
              contractType = "character",  # short form e.g. 'PAM'
              statusDate = "character",    # text yyyy-mm-dd
              riskFactors = "list"
            )
)
# *********************
#  constructors:  EventSeries() : (), (<contractType>, <rf_list>, <serverURL> )
setGeneric(name = "EventSeries",
           def = function(contract, riskFactors, serverURL){
             standardGeneric("EventSeries")
           })

setMethod(f = "EventSeries", signature = c(),
          definition = function(){
            return(new("EventSeries"))
          })

setMethod(f = "EventSeries", signature = c("ContractType","list", "character"),
          definition = function(contract, riskFactors, serverURL){
            ptf <- Portfolio()
            ptf$contracts <- list(contract)  # singleContractPortfolio
            ptf$riskFactors <- riskFactors   # some number of rfs
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- generateEvents(ptf,serverURL)[[1]]
            #first cashflow from single contract ptf
            stopifnot (cshfl_rslt1$status == "Success") # possibl better info
            evs_list <- cshfl_rslt1$events
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- contract$contractTerms$contractID
            evs$contractType <- contract$contractTerms$contractType
            evs$statusDate <-  contract$contractTerms$statusDate
            evs$riskFactors <- riskFactors

            # construct the 7 columns with event list data (no long loops please)
            # initialize the data.frame with a row index evid
            evid <- 1:length(evs_list)
            events_df <-data.frame(evid)
            Event_Field_Names <- c("type","time","payoff","currency",
                                   "nominalValue","nominalRate","nominalAccrued")
            for(evfield in Event_Field_Names) {
              events_df[evfield] <- unlist(sapply(evs_list,
                                                  function(ev){ev[evfield]}))
            }
            # now remove the evid column used to size events_df
            events_df <- subset(events_df, select = -evid) #drop starter column

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })

#' generateEventSeries      Generic method definition
#'
#' Defines a generic method on S4 Class Eventseries. The instance is
#' generateEventSeries < contract riskFactors serverURL >
#'
#' @param  contract     the contract to simulate cashflows for
#' @param  riskFactors  list of riskFactors - scenario for contract simulation
#' @param  serverURL    locate the ACTUS server performing the cashflow analysis
#' @return              an EventSeries with cashflow events for the contract
#' @import  methods
#' @include RiskFactor.R
#' @include Portfolio.R
#' @include ContractType.R
#' @examples{
#'   pam1 <- bond("2013-12-31", maturity = "5 years", nominal = 50000,
#'                coupon = 0.02, couponFreq = "1 years", role = "long")
#'   serverURL <- "https://demo.actusfrf.org:8080/"
#'   evs1 <-generateEventSeries(pam1, list(), serverURL)
#'  }
setGeneric(name = "generateEventSeries",
           def = function(contract, riskFactors, serverURL){
             standardGeneric("generateEventSeries")
           })

#' generateEventSeries        <contract>, <risk-factor-list>, <ACTUS-server-URL>
#'
#' exported function to simulate a contract cashflows and create an
#'   EventSeries using  "ContractType", "list", "character" method instance.
#'   constructs an EventSeries instance including as its events_df attribute a
#'   dataframe of cashflow events for the input ACTUS contract. This cashflow
#'   is generated with a callout to the ACTUS server located at ACTUS-server-URL
#'   using a risk scenario specified as the list of risk factors. THe method
#'   works by first creating a Portfolio with this ine contract and the supplied
#'   risk factor list, then calling generateEvents on this portfolio
#'
#'   This is an internal method on EventSeries, and not exported.
#'   generateEventSeries <contract>, risk-factors>, <serverURL> is the
#'   exported wrapper function which is exported from FEMSdevPkg and calls this.
#'
#' @param  contract    S4 ref      class= ContractType
#' @param  riskFactors list        list of S4 ref Class=RiskFactor
#' @param  serverURL   character   URL of ACTUS server to simulate the contract
#' @return              S4 ref     class=EventSeries
#' @export
#'
setMethod(f = "generateEventSeries", signature = c("ContractType","list", "character"),
          definition = function(contract, riskFactors, serverURL){
              evs <- EventSeries(contract,riskFactors,serverURL)
              return(evs)
          })
