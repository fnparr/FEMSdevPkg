#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
# edits by Francis Parr for FEMSdev april 2022
 #*******************************************************************************

##############################################################
#' \code{bond}
#'
#' Constructor method for simple (single) PrincipalAtMaturity contract.
#' @export
#'
#' @param start a character reflecting the start date of the bond.
#'
#' @param maturity a character reflecting the maturity of the bond,
#' default is "0 years".
#'
#' @param nominal a numeric to set the notional principal of the bond,
#' default is 0.0.
#'
#' @param coupon a numeric to set the coupon payment, default is 0.0.
#'
#' @param couponFreq a character reflecting the frequency of coupon payments,
#' default is "1 year".
#'
#' @param role a character reflecting the contract role. default is "long".
#'
#' @return a PrincipalAtMaturity contrat with specified attributes.
#'
#' @usage bond(start, maturity, nominal, coupon, couponFreq, role)
#'
#' @examples
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000,
#'            coupon = 0.02, couponFreq = "1 years")
#'
#' @include PrincipalAtMaturity.R
#' @export
bond <- function(start, maturity="0 years", nominal=0,
                coupon=0.0, couponFreq="1 year", role="long",
                variable.rates=FALSE, ...) {
  if (missing(start)){
    stop("Parameter 'start' must be set to yyyy-mm-dd date !!!")
  }
  args <- list(...)
  if(nchar(maturity)<10) {
    maturity <- as.character(timeSequence(timeDate(start),
                                               by=maturity, length.out=2)[2])
  }
  statusDate <- as.character(timeDate(start)-24*3600)
  contractDealDate <- as.character(timeDate(start)-24*3600)
  initialExchangeDate <- start

  couponFreq_bef <- couponFreq
  if(is.null(couponFreq) || couponFreq=="NULL") {
    couponFreq <- NULL
    coupon <- -999999999
  } else {
    if(length(grep("y", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "Y", "L1", sep="")
    } else if(length(grep("q", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "Q", "L1", sep="")
    } else if(length(grep("m", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "M", "L1", sep="")
    } else if(length(grep("w", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "W", "L1", sep="")
    } else if(length(grep("d", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "D", "L1", sep="")
    }else {
      stop("please provide couponFreq information in timeSeries 'by' format!")
    }
  }

  if(role=="long") {
    role <- "RPA"
  } else {
    role <- "RPL"
  }

  if(!"currency"%in%names(args)) {
    args[["currency"]] <- "CHF"
  }

  if(!"calendar"%in%names(args)) {
    args[["calendar"]] <- "NC"
  }

  if(!"dayCountConvention"%in%names(args)) {
    args[["dayCountConvention"]] <- "30E360"
  }

  if(variable.rates) {
    args[["cycleOfRateReset"]] <- couponFreq
  }

  contractType <- "PAM"
  contractID <-  "bond001"

  attributes <- list( contractType=contractType,
                      contractID=contractID,
                    initialExchangeDate=initialExchangeDate,
                    statusDate=statusDate,
                    contractDealDate=contractDealDate,
                    maturityDate=maturity,
                    notionalPrincipal=nominal,
                    nominalInterestRate=coupon,
                    cycleOfInterestPayment=couponFreq,
                    cycleAnchorDateOfInterestPayment =
                      as.character(timeSequence(initialExchangeDate,
                                                by=couponFreq_bef,
                                                length.out=2)[2]),
                    contractRole=role)
  attributes <- append(attributes, args)
  out <- Pam()
  set(out, what=attributes)
#  checkAttributes(out)     $ checking disabled for now
  return(out)
}
