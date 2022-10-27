# *******************************************************************************
# ZHAW Risk and Finance Lab
# author(s): Francis Parr, Nils Andri Bundi 
# This version 25 October 2022
# **************************************************
library(lubridate)
# **************************************
#' \code{mortgage}
#' 
#' mortgage()   function to construct a single annuity contract using 
#'              simplified parameters. Handles fixed and variable rates. 
#'              Variable rate is identified by a nonnull rateResetFreq. The 
#'              market object code for rate reset is by default the interest 
#'              market object code YC_EA_AAA 
#' 
#' @param start   character string yyyy-mm-dd the start date of the mortgage.
#' @param maturity   character string setting the lifetime of the mortgage.
#' @param nominal    numeric to set the notional principal of the bond,
#' @param coupon     numeric initial interest rate 0.02 = 2% pa default is 0.0.
#' @param paymentFreq character string period of payments (interest + principal)
#' @param role      character string setting whether lender or borrower role. 
#' @param rateResetFreq optional character string setting a period of RateReset
#' @param rateResetSpread optional numeric with rate spread for variable rate
#' @return    initialized Annuity contract contract with specified attributes.
#' @usage  mortgage(start, maturity, nominal, coupon, paymentFreq, role, rateResetFreq, rateResetSpread )
#' @examples {
#'     m <- mortgage("2020-12-31", maturity = "10 years", nominal = 10000,
#'               coupon = 0.07, paymentFreq = "3 months", role = "long",
#'               rateResetFreq = "1 year", rateResetSpread = 0.01 )
#'     }
#' @include Annuity.R 
#' @importFrom   lubridate period
#' @importFrom   lubridate ymd
#' @export
mortgage <- function(start, maturity, nominal, coupon, paymentFreq, role,
                   rateResetFreq=NULL, rateResetSpread=NULL){
  ann1 <- Annuity()
  
  # Set contract identifying keys 
  ann1$contractTerms[["contractType"]]  <- "ANN"
  ann1$contractTerms[["contractID"]]    <- "ann001"
  
  # set contract IED
  if (missing(start)){
    stop("Parameter 'start' must be set to yyyy-mm-dd date !!!")
  }
  # other required atomic fields 
  ann1$contractTerms[["initialExchangeDate"]] <- start # T00:00:00 to be added
  
  startDate <- lubridate::ymd(start)
  ann1$contractTerms[["contractDealDate"]] <- as.character( startDate 
                                                  - lubridate::period("1 day"))
  ann1$contractTerms[["statusDate"]] <-  as.character( startDate 
                                                  - lubridate::period("1 day"))
  ann1$contractTerms[["maturityDate"]] <-  as.character( startDate 
                                                  + lubridate::period(maturity))
  ann1$contractTerms[["notionalPrincipal"]] <- nominal
  ann1$contractTerms[["nominalInterestRate"]] <- coupon
  
  if(role=="long") {
    ann1$contractTerms[["contractRole"]]  <- "RPA"
  } else {
    ann1$contractTerms[["contractRole"]] <- "RPL"
  }
  
  ann1$contractTerms[["currency"]] <- "CHF"
  ann1$contractTerms[["calendar"]] <- "NC"
  ann1$contractTerms[["dayCountConvention"]] <- "30E360"
  
  # principal  and interest payments
  payPeriod <- lubridate::period(paymentFreq)
  payPcode <- period2code(paymentFreq)
  firstPaymentDate <-   as.character( startDate + payPeriod ) 
  ann1$contractTerms[["cycleAnchorDateOfPrincipalRedemption"]] <- 
         firstPaymentDate
  ann1$contractTerms[["cycleAnchorDateOfInterestPayment"]] <- 
         firstPaymentDate
  ann1$contractTerms[["cycleOfPrincipalRedemption"]] <- payPcode
  ann1$contractTerms[["cycleOfInterestPayment"]]     <- payPcode
  
  # if variable rate add RateReset terms 
  if ( !(is.null(rateResetFreq) || rateResetFreq=="NULL") ) {
     rrPeriod <- lubridate::period(rateResetFreq)
     rrPcode  <- period2code(rateResetFreq)
     ann1$contractTerms[["cycleAnchorDateOfRateReset"]] <-
           as.character( startDate + rrPeriod )
     ann1$contractTerms[["cycleOfRateReset"]] <- rrPcode
     ann1$contractTerms[["marketObjectCodeOfRateReset"]] <- "YC_EA_AAA"
  }
  
  return(ann1)
}

# ****************************
# period2code ( )  input is a periodString is. "2 years" "3 months" "1 day" 
#                  as accepted by lubridate period( ) converts ISO P2YL1 etc 
period2code <- function(pstring) {
  if(length(grep("year", pstring))>0) {
    pcode <- paste("P",gsub("([0-9]*).*","\\1", pstring), "Y", "L1", sep="")
#  } else if(length(grep("q", pstring))>0) {
#    pcode <- paste("P",gsub("([0-9]*).*","\\1", pstring), "Q", "L1", sep="")
# quarter durations not supported as lubridate period    
  } else if(length(grep("month", pstring))>0) {
    pcode <- paste("P",gsub("([0-9]*).*","\\1", pstring), "M", "L1", sep="")
  } else if(length(grep("week", pstring))>0) {
    pcode <- paste("P",gsub("([0-9]*).*","\\1", pstring), "W", "L1", sep="")
  } else if(length(grep("day", pstring))>0) {
    pcode <- paste("P",gsub("([0-9]*).*","\\1", pstring), "D", "L1", sep="")
  }
  return (pcode)
}
