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
