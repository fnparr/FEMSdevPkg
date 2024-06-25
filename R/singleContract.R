# *******************************************************************************
# ZHAW Risk and Finance Lab
# author(s): Francis Parr, Nils Andri Bundi 
# This version 25 October 2022
# **************************************************
# **************************************
#' \code{loan}
#'
#'     Function loan() creates and returns a PAM or an ANN
#' 
#' @import lubridate
#' @param ctype   character eith ANN or PAM
#' @param start   character string yyyy-mm-dd the start date of the mortgage.
#' @param maturity   character string setting the lifetime of the mortgage.
#' @param nominal    numeric to set the notional principal of the bond,
#' @param coupon     numeric initial interest rate 0.02 = 2 pa default is 0.0.
#' @param paymentFreq character string period of payments (interest + principal)
#' @param role      character string setting whether lender or borrower role.
#' @param rateResetFreq optional character string setting a period of RateReset
#' @param rateResetSpread optional numeric with rate spread for variable rate
#' @return    initialized Annuity contract contract with specified attributes.
#' @usage {
#'         ctr1<-  loan(ctype, start, maturity, nominal, coupon, paymentFreq,
#'                      role, rateResetFreq, rateResetSpread )
#'        }
#' @examples {
#'     l <- loan("ANN", "2020-12-31", maturity = "10 years", nominal = 10000,
#'               coupon = 0.07, paymentFreq = "3 months", role = "long",
#'               rateResetFreq = "1 year", rateResetSpread = 0.01 )
#'     }
#' @export
loan <- function(ctype, start, maturity, nominal, coupon,
                 paymentFreq,role,
                 rateResetFreq = NULL,rateResetSpread = NULL){
  if (ctype== "PAM") {
    cntr1 <- bondvr(start, maturity, nominal, coupon, paymentFreq,role,
                    rateResetFreq=rateResetFreq,
                    rateResetSpread=rateResetSpread )
  } else if (ctype == "ANN"){
    cntr1 <- mortgage(start, maturity, nominal, coupon, paymentFreq,role,
                      rateResetFreq=rateResetFreq,
                      rateResetSpread=rateResetSpread )
  } else if (ctype == "LAM"){
    cntr1 <- lam(start, maturity, nominal, coupon, paymentFreq,role,
                      rateResetFreq=rateResetFreq,
                      rateResetSpread=rateResetSpread )
  } else {
    stop("Invalid contract type - must be PAM, ANN or LAM")
  }
  return(cntr1)
}

# **************************************
#' \code{mortgage}
#' 
#'     Function mortgage() creates and returns a fixed or variable rate ANN
#'
#'     Convenience function with a simplified parameter list to create and 
#'     initialize a single Annuity fixed or variable rate mortgage contract.
#' .
#'     Variable rate bonds  are identified by a nonnull rateResetFrequency. For 
#'     these variable rate bonds the default marketObjectCodeOfRateReset is 
#'     "YC_EA_AAA"
#'     
#'     Terms statusDate and contractDealDate are set 1 day before start date. 
#'     Term initialExchangeDate is set to start date. The cycleAnchorDates for
#'     Interest and principal payments are set at ( IED + a period ). The 
#'     maturityDate is set ( IED + Maturity period) defaults are set for 
#'     currency, dayCountConvention, Calendar. Role, notionaPrincipal and 
#'     nominalInterestRate are set as specified in the parameter list.
#'              
#' 
#' @param start   character string yyyy-mm-dd the start date of the mortgage.
#' @param maturity   character string setting the lifetime of the mortgage.
#' @param nominal    numeric to set the notional principal of the bond,
#' @param coupon     numeric initial interest rate 0.02 = 2pcpa default is 0.0.
#' @param paymentFreq character string period of payments (interest + principal)
#' @param role      character string setting whether lender or borrower role. 
#' @param rateResetFreq optional character string setting a period of RateReset
#' @param rateResetSpread optional numeric with rate spread for variable rate
#' @return    initialized Annuity contract contract with specified attributes.
#' @usage  mortgage(start, maturity, nominal, coupon, paymentFreq, role, 
#'                  rateResetFreq, rateResetSpread )
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
  firstPaymentDate <-   lubridate::add_with_rollback(startDate,
                                                     payPeriod ) 
  ann1$contractTerms[["cycleAnchorDateOfPrincipalRedemption"]] <- 
         firstPaymentDate
  ann1$contractTerms[["cycleAnchorDateOfInterestPayment"]] <- 
         firstPaymentDate
  ann1$contractTerms[["cycleOfPrincipalRedemption"]] <- payPcode
  ann1$contractTerms[["cycleOfInterestPayment"]]     <- payPcode
  
  # if variable rate add RateReset terms 
  if ( !(is.null(rateResetFreq) || rateResetFreq=="Fixed rate") ) {
     rrPeriod <- lubridate::period(rateResetFreq)
     rrPcode  <- period2code(rateResetFreq)
     ann1$contractTerms[["cycleAnchorDateOfRateReset"]] <-
                  lubridate::add_with_rollback(startDate, rrPeriod) 
     ann1$contractTerms[["cycleOfRateReset"]] <- rrPcode
     ann1$contractTerms[["marketObjectCodeOfRateReset"]] <- "YC_EA_AAA"
  }
  
  return(ann1)
}

# **************************************
# bondvr() Adding variable rate capability - does fixed and variable rates
#' \code{bondvr}
#'     Function bondvr() creates and returns a fixed or variable rate PAM
#'
#'     Convenience function with a simplified parameter list to create and 
#'     initialize a single PrincipalAtMaturity fixed or variable rate bond
#'     contract.
#' .
#'     Variable rate bonds  are identified by a nonnull rateResetFrequency. For 
#'     these variable rate bonds the default marketObjectCodeOfRateReset is 
#'     "YC_EA_AAA"
#'     
#'     Terms statusDate and contractDealDate are set 1 day before start date. 
#'     Term initialExchangeDate is set to start date. The cycleAnchorDates for
#'     Interest and principal payments are set at ( IED + a period ). The 
#'     maturityDate is set ( IED + Maturity period) defaults are set for 
#'     currency, dayCountConvention, Calendar. Role, notionaPrincipal and 
#'     nominalInterestRate are set as specified in the parameter list.
#' 
#' @param start      character string yyyy-mm-dd the start date of the bond.
#' @param maturity   character string (lubridate) period for term of the bond.
#' @param nominal    numeric to set the notional principal of the bond,
#' @param coupon    numeric the coupon/interest rate 0.02 = 2pcpa default is 0.0.
#' @param paymentFreq a character string with period of coupon payments,
#' @param role      a character string setting the contract role.
#' @param rateResetFreq optional character string setting a period of RateReset
#' @param rateResetSpread optional numeric with rate spread for variable rate
#' @return    a PrincipalAtMaturity contract with specified attributes.
#' @usage bondvr(start, maturity, nominal, coupon, paymentFreq, role, 
#'               rateResetFreq, rateResetSpread )
#' @examples {
#'     b <- bondvr("2013-12-31", maturity = "5 years", nominal = 50000,
#'               coupon = 0.02, paymentFreq = "3 months", role = "long",
#'               rateResetFreq = "1 years", rateResetSpread = 0.01 )
#'     }
#' @include PrincipalAtMaturity.R
#' @importFrom  lubridate ymd
#' @importFrom  lubridate period
#' @export
bondvr <- function(start, maturity, nominal, coupon, paymentFreq, role,
                   rateResetFreq=NULL, rateResetSpread=NULL){
  bnd1 <- Pam()
  
  # Set contract identifying keys 
  bnd1$contractTerms[["contractType"]]  <- "PAM"
  bnd1$contractTerms[["contractID"]]    <- "bndvr001"
  
  # set contract IED
  if (missing(start)){
    stop("Parameter 'start' must be set to yyyy-mm-dd date !!!")
  }
  # other required atomic fields 
  bnd1$contractTerms[["initialExchangeDate"]] <- start # T00:00:00 to be added
  
  startDate <- lubridate::ymd(start)
  bnd1$contractTerms[["contractDealDate"]] <- as.character( startDate 
                                                            - lubridate::period("1 day"))
  bnd1$contractTerms[["statusDate"]] <-  as.character( startDate 
                                                       - lubridate::period("1 day"))
  bnd1$contractTerms[["maturityDate"]] <-  as.character( startDate 
                                                         + lubridate::period(maturity))
  bnd1$contractTerms[["notionalPrincipal"]] <- nominal
  bnd1$contractTerms[["nominalInterestRate"]] <- coupon
  
  if(role=="long") {
    bnd1$contractTerms[["contractRole"]]  <- "RPA"
  } else {
    bnd1$contractTerms[["contractRole"]] <- "RPL"
  }
  
  bnd1$contractTerms[["currency"]] <- "CHF"
  bnd1$contractTerms[["calendar"]] <- "NC"
  bnd1$contractTerms[["dayCountConvention"]] <- "30E360"
  
  # principal  and interest payments
  payPcode <- period2code(paymentFreq)
  # lubridate addition of months gives NA in some cases so handle specially:
  
  firstPaymentDate <- lubridate::add_with_rollback(startDate,
                                                   period(paymentFreq )) 
  bnd1$contractTerms[["cycleAnchorDateOfPrincipalRedemption"]] <- 
    firstPaymentDate
  bnd1$contractTerms[["cycleAnchorDateOfInterestPayment"]] <- 
    firstPaymentDate
  bnd1$contractTerms[["cycleOfPrincipalRedemption"]] <- payPcode
  bnd1$contractTerms[["cycleOfInterestPayment"]]     <- payPcode
  
  # if variable rate add RateReset terms 
  if ( !(is.null(rateResetFreq) || rateResetFreq=="Fixed rate") ) {
    rrPeriod <- lubridate::period(rateResetFreq)
    rrPcode  <- period2code(rateResetFreq)
    bnd1$contractTerms[["cycleAnchorDateOfRateReset"]] <-
      lubridate::add_with_rollback(startDate,period(rateResetFreq))
    bnd1$contractTerms[["cycleOfRateReset"]] <- rrPcode
    bnd1$contractTerms[["marketObjectCodeOfRateReset"]] <- "YC_EA_AAA"
  }
  
  return(bnd1)
}

# **************************************
#' \code{lam}
#' 
#'     Function lam() creates and returns a fixed or variable rate LAM
#'
#'     Convenience function with a simplified parameter list to create and 
#'     initialize a single Annuity fixed or variable rate mortgage contract.
#' .
#'     Variable rate bonds  are identified by a nonnull rateResetFrequency. For 
#'     these variable rate bonds the default marketObjectCodeOfRateReset is 
#'     "YC_EA_AAA"
#'     
#'     Terms statusDate and contractDealDate are set 1 day before start date. 
#'     Term initialExchangeDate is set to start date. The cycleAnchorDates for
#'     Interest and principal payments are set at ( IED + a period ). The 
#'     maturityDate is set ( IED + Maturity period) defaults are set for 
#'     currency, dayCountConvention, Calendar. Role, notionaPrincipal and 
#'     nominalInterestRate are set as specified in the parameter list.
#'              
#' 
#' @param start   character string yyyy-mm-dd the start date of the mortgage.
#' @param maturity   character string setting the lifetime of the mortgage.
#' @param nominal    numeric to set the notional principal of the bond,
#' @param coupon     numeric initial interest rate 0.02 = 2pcpa default is 0.0.
#' @param paymentFreq character string period of payments (interest + principal)
#' @param role      character string setting whether lender or borrower role. 
#' @param rateResetFreq optional character string setting a period of RateReset
#' @param rateResetSpread optional numeric with rate spread for variable rate
#' @return    initialized Annuity contract contract with specified attributes.
#' @usage  lam(start, maturity, nominal, coupon, paymentFreq, role, 
#'                  rateResetFreq, rateResetSpread )
#' @examples {
#'     l <- lam("2020-12-31", maturity = "10 years", nominal = 10000,
#'               coupon = 0.07, paymentFreq = "3 months", role = "long",
#'               rateResetFreq = "1 year", rateResetSpread = 0.01 )
#'     }
#' @include LinearAmortizer.R 
#' @importFrom   lubridate period
#' @importFrom   lubridate ymd
#' @export
lam <- function(start, maturity, nominal, coupon, paymentFreq, role,
                     rateResetFreq=NULL, rateResetSpread=NULL){
  lam1 <- LinearAmortizer()
  
  # Set contract identifying keys 
  lam1$contractTerms[["contractType"]]  <- "LAM"
  lam1$contractTerms[["contractID"]]    <- "lam001"
  
  # set contract IED
  if (missing(start)){
    stop("Parameter 'start' must be set to yyyy-mm-dd date !!!")
  }
  # other required atomic fields 
  lam1$contractTerms[["initialExchangeDate"]] <- start # T00:00:00 to be added
  
  startDate <- lubridate::ymd(start)
  lam1$contractTerms[["contractDealDate"]] <- as.character( startDate 
                                                            - lubridate::period("1 day"))
  lam1$contractTerms[["statusDate"]] <-  as.character( startDate 
                                                       - lubridate::period("1 day"))
  lam1$contractTerms[["maturityDate"]] <-  as.character( startDate 
                                                         + lubridate::period(maturity))
  lam1$contractTerms[["notionalPrincipal"]] <- nominal
  lam1$contractTerms[["nominalInterestRate"]] <- coupon
  
  if(role=="long") {
    lam1$contractTerms[["contractRole"]]  <- "RPA"
  } else {
    lam1$contractTerms[["contractRole"]] <- "RPL"
  }
  
  lam1$contractTerms[["currency"]] <- "CHF"
  lam1$contractTerms[["calendar"]] <- "NC"
  lam1$contractTerms[["dayCountConvention"]] <- "30E360"
  
  # principal  and interest payments
  payPeriod <- lubridate::period(paymentFreq)
  payPcode <- period2code(paymentFreq)
  firstPaymentDate <-   lubridate::add_with_rollback(startDate,
                                                     payPeriod ) 
  lam1$contractTerms[["cycleAnchorDateOfPrincipalRedemption"]] <- 
    firstPaymentDate
  lam1$contractTerms[["cycleAnchorDateOfInterestPayment"]] <- 
    firstPaymentDate
  lam1$contractTerms[["cycleOfPrincipalRedemption"]] <- payPcode
  lam1$contractTerms[["cycleOfInterestPayment"]]     <- payPcode
  
  # if variable rate add RateReset terms 
  if ( !(is.null(rateResetFreq) || rateResetFreq=="Fixed rate") ) {
    rrPeriod <- lubridate::period(rateResetFreq)
    rrPcode  <- period2code(rateResetFreq)
    lam1$contractTerms[["cycleAnchorDateOfRateReset"]] <-
      lubridate::add_with_rollback(startDate, rrPeriod) 
    lam1$contractTerms[["cycleOfRateReset"]] <- rrPcode
    lam1$contractTerms[["marketObjectCodeOfRateReset"]] <- "YC_EA_AAA"
  }
  
  return(lam1)
}

# ****************************
# period2code ( )  input is a periodString is. "2 years" "3 months" "1 day" 
#                  as accepted by lubridate period( ) converts ISO P2YL1 etc 
period2code <- function(pstring) {
  pcode<-""
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
  } else { pcode <- "invalid period input"}
  
  return (pcode)
}

# ***************************
# addPeriod( )   lubridate returns NA for add 6 months to 2020-12-31  
#      addPeriod: parameters: start character yyyy-mm-dd , pstring character
#                 "2 years", "1 month" , "6 months" , "4 days"
#      uses lubridate + for years and days , %m+% for months 
#      returns  character yyyy-mm-dd  = start + period 
addPeriod <- function( start, pstring ){
   startDate <- lubridate::ymd(start)
   if(length(grep("month", pstring))>0) {
      ptokens<- strsplit(pstring," ")
      endDate <- startDate %m+% months(as.integer(ptokens[[1]][1]))
   }
   else {
      endDate <- startDate + lubridate::period(pstring)
   }
   return(as.character(endDate))
}
