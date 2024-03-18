# Accounts.R  FEMS dev code by Francis Parr Feb 2024. Included in FEMSdevPkg; 
# Licensing and Copyright notices from there
# Defines utilities for manipulating and aggregating accountNodes in 
# a FinancialModel accounts tree 
library(data.tree)
setOldClass("Node")   # Allows data.tree::Node to be used in S4 object slots
# ************
# class AccountsTree
#  the saved attributes for convenient identification of accountNodes
#  and navigation by path and nodeID 
#  leafList ? 

setRefClass("AccountsTree",
            fields = list(
              root         = "Node",
              height = "numeric",
              map_df = "data.frame",
              leafList = "list",
              specification = "character"
            ))

# ***********
# Function to create treeFromYamlString
treeFromYamlString <- function(yamlstring) 
  return(as.Node(yaml.load(yamlstring),interpretNullAsList = TRUE))

# ***************
# Function to assign unique NodeID at each node 
setUniqueNodeIDs <- function(accounts) {
  accounts$Set(nodeID = 1:accounts$totalCount)
  return(accounts)
}

# *******************
# Functions to relate CID to nodeID
# ********************
# Function to show nodeID of account contract is assigned to  
# we use mapPair lists: =  < cids: stringVector, nodes: intVector>   
# mapc function concatenates two mapPair lists 
mapc <- function(alist,blist) {
  return(list(cids=c(alist$cids,blist$cids),
              nodes=c(alist$nodes,blist$nodes)))
}
# maplistc(maplist) concatenates a list of mapPair lists 
# repeated twoway mapc concatenation - simple implementation 
maplistc <- function(maplist){
  outPair <- list(cids=c(),nodes=c())
  for (i in 1:length(maplist)){
    outPair <- mapc(outPair,maplist[[i]])
  }
  return(outPair)
}
# function cid2NodeIdMap(account) builds mapPair lists for the accounts tree
# recursive on children , maplist on leaf nodes with assigned cids
cid2NodeIdMap <- function(account) {
  if (isNotLeaf(account)){
    return( maplistc(lapply(account$children,
                            function(child){ return(cid2NodeIdMap(child))})))
  }
  else if ( !is.null(account$actusCIDs) ) {
    return( list(cids = account$actusCIDs,
                 nodes= rep(account$nodeID,length(account$actusCIDs))))
  }
  else { return(list(cids=c(),nodes=c()))
  }
}
# *********
# lookup function cid2NodeId(cid) using map pair above 
# Test 1.1  cid2NodeId( ) return NodeID of owner for contract with  CID = cid
cid2NodeId <- function(cid, accounts) {
  mp <- cid2NodeIdMap(accounts)
  return(mp$nodes[which(mp$cids==cid)])
}





# ************
# Functions for aggregating report vectors in the accounts tree  
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
#  **********
# clearNonleafIncome(account) will clear the income aggregate vecto in all non 
#                leaf nodes descended from account. Income(account) will 
#               recompute and reinstate aggregate income report vectors 
clearNonleafIncome <- function(account) {
accnts2Node$Set(income = NULL, filterFun = isNotLeaf)
}
