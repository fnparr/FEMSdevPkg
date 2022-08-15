# ContractABC.R    modified FNP Feb 2022
##############################################################
#' A Reference Class parent to all ACTUS & FEMS Contract Types
#'
#' This class is only used in an abstract sense in that it is
#' not intended to create instances of it. It's use is to
#' serve as parent to various implementations of ACTUS and FEMS CTs
#' as means for designing method-inheritage.

setRefClass("ContractABC",
            fields = list())

# omit ctnames - but might be useful
# show , summary and checkAttributes suppressed for now
