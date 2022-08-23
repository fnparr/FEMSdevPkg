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
           def = function(contract, riskFactors, serverURL ){
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
            events_df <- data.frame(evid = 1:length(evs_list))
            Event_Field_Names <- c("type","time","payoff","currency",
                                   "nominalValue","nominalRate","nominalAccrued")
            # each is actually prefixed by events.  complete field name = events.<fieldname>
            for(evfield in Event_Field_Names) {
                events_df[evfield] <- unlist(sapply(evs_list,
                                                    function(ev){ev[evfield]}))
            }
            events_df <- subset(events_df, select = -evid) #drop starter column
            events_df$time <- sapply(events_df$time,
                                    function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })
