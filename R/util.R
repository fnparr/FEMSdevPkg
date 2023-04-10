# util.R file with utilities
# *************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## -----------------------------------------------------------------
## private util methods
## get long name for ContractType from short
longName <- function(name) {
  short <- c("pam", "ann", "nam", "lam", "stk",
             "fxout", "swaps", "futur", "optns","lax")
  long <- c("principalatmaturity", "annuity", "negativeamortizer",
            "linearamortizer", "stock", "foreignexchangeoutright",
            "swap", "future", "option","exoticlinearamortizer")
  target <- c("PrincipalAtMaturity", "Annuity", "NegativeAmortizer",
              "LinearAmortizer", "Stock", "ForeignExchangeOutright",
              "Swap", "Future", "Option","ExoticLinearAmortizer")
  names.table <- data.frame(short=short, long=long, target=target)
  if(tolower(name)%in%short) {
    out <- names.table[which(short==tolower(name)), "target"]
  } else if(tolower(name)%in%long) {
    out <- names.table[which(long==tolower(name)), "target"]
  } else {
    stop(paste("ContractType", name, "does not exist!", sep=" "))
  }
  return(out)
}
#' monthlyAverageRate indf)  function
#'
#'    This function takes as input a data frame with a numeric Rate column
#'    and a charcter string Date columns. It returns as output a dataframe
#'    with  a numeric column Rate and a timeDateCOlumn Date  - but the Rate
#'    values now aggregated using
#'    FUN = mean and each months data associated with the first day of the month.
#' @param indf  input dataframe with columns named Rate and Date
#' @return outdf  Rate values from indf aggregated by month using mean()
#' @export
#' @examples {
#'  indf <- data.frame(Date = as.Date(c('2001-01-01','2001-01-08', '2001-01-15',
#'                         '2001-02-01')),
#'                   Rate = c( 3.1, 3.2, 3.3, 4.1)
#'                   )
#'  outdf <- monthlyAverageRate(indf)
#' }
#'
monthlyAverageRate <- function(indf){
  indf["Month"] <- substr(indf[,"Date"],1,7)
  dfout <- aggregate(indf$Rate, by=list(indf$Month), FUN=mean)
  colnames(dfout) <- c("Month","Rate")
  dfout["Date"]<- as.Date(paste0(dfout[,"Month"],"-01"))
  dfout <- dfout [c("Date","Rate")]
  return(dfout)
}
