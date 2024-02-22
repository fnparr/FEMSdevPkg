# Accounts.R  FEMS dev code by Francis Parr Feb 2024. Included in FEMSdevPkg; 
# Licensing and Copyright notices from there
# Defines utilities for manipulating and aggregating accountNodes in 
# a FinancialModel accounts tree 
# ***********
# VectorSum(vlist) does element wise aggregation of a list of numeric vectors
#                  of equal length and returns the vector of sums. We expect to
#                  this in aggregating report values to higher level nodes in 
#.                 the tree
VectorSum <- function(vlist) {
  # could get more issue detection using :
  # vlen <- max(sapply(vlist,function(vl) { return(length(vl)) } )
  vlen <- length(vlist[[1]]) 
  vsum <- rep(0,vlen)
  for (i in 1:vlen){
    vsum[i] <-  sum( sapply(vlist, function(v) return(v[i])) ) 
  }
  return(vsum)
}
# **************
# Income(account) is a function which can be applied to the root of an accounts
#                 tree. It picks up $income report vectors from leaf accounts in
#                 and aggregates these vectors , saving them at $income vector
#                 values at each sub-account of the account node argument 
Income <- function(node) {
  if (isNotLeaf(node))
    node$income <- VectorSum(lapply(node$children, Income))
  return(node$income) 
}
#                 