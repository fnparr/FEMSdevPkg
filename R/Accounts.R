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

# **********
# Function to set empty reports attribute at each account
setEmptyReportsList <-function(accounts){
  accounts$Set(reports = list()) 
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
#                  the tree. For each report in the vlist input, we have to 
#                  unlist it  to get the vector of values then index into that
#                  and sum the indexed values over each vectorin the list
VectorSum <- function(vlist) {
  # could get more issue detection using :
  vvlist <- Filter(Negate(is.null),vlist)
  # vlen <- max(sapply(vlist,function(vl) { return(length(vl)) } )
  if (length(vvlist)  > 0 ) { 
#     print("*** length vvlist = ", length(vvlist))
     vlen <- length(unlist(vvlist[[1]]))
     vsum <- rep(0,vlen)
     for (i in 1:vlen){
        vsum[i] <-  sum( sapply(vvlist, 
                             function(v) { if (!is.null(v)) return(unlist(v)[i])
                                          else return(NULL)
                             }))
     }
     return(vsum)
  } else
     return(NULL)
}
# *****************
#  Function to create aggregated hierarchical reports from contract reports 
# *****************
#  addAggregateReport(account,cidReports,reportName)
#     This function adds a new aggregated report reportName into the list of 
#     of reports at each node of the accounts tree. Input parameter cidReports 
#     is a list keyed by CID of report values for each contract. Each report 
#     is a numeric vector computed by contract cashflow analysis. All report 
#     vectors have the same length - determined by timeline of the analysis. 
#     Flow reports have one less element than status/value reports. Function 
#     addAggregateReport() computes account node report values recursively:
#     (1) nonleaf accounts do a vector sum of their child node reports
#     (2) leaf nodes owning at least one actusCID, do a vector sum of the
#         cidReports for their assigned contracts 
#     (3) leaf nodes with no assigned actusContracts generate a report vector 
#         of the correct length ( for this report type) BUT all NULL vallues
# ********
addAggregatedLiquidity <- function(account,cidReports){
  if (isNotLeaf(account))
    account$liquidity <- 
      VectorSum(lapply(
        account$children,
        function(x) addAggregatedLiquidity(x,cidReports))
      )
  else if ( is.null(account$actusCIDs) )  account$liquidity <- NULL
  else {
    account$liquidity <-
      VectorSum(lapply(account$actusCIDs, function(cid) cidReports[cid]))
  }  
  return(account$liquidity) # return specific report parents need 
}
