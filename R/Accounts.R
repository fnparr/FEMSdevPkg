# Accounts.R  FEMS dev code by Francis Parr Feb 2024. Included in FEMSdevPkg; 
# Licensing and Copyright notices from there
# Defines utilities for manipulating and aggregating accountNodes in 
# a FinancialModel accounts tree 
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

# **************
#  AccountsTree( ) constructors 
# **** Generic S4 constructor method for class AccountsTree
setGeneric("AccountsTree",
           function(yamlstring) { standardGeneric("AccountsTree") }
)

#  ***** No parameters AccountsTree( )
# AccountsTree ( )  - no parameters instance of AccountsTree()  
#   no parameters method for internal use only 
# Creates and returns an empty AccountsTree with no attributes initialized. 
setMethod("AccountsTree", c(), 
          function(){ return( new("AccountsTree")) }
)

# ****** exported user AccountsTree(yamlstring)
# ************************************************************************
# AccountsTree(yamlstring) creates/initializes an AccountsTree instance
# ************************************************************************
#' AccountsTree(yamlstring )
#'
#' This method creates and initializes a new data.tree AccountsTree instance.
#' Input paramter yamlstring is a character string  in yaml format describing 
#' the desired AccountsTree structure using indentation for accounts nodes at 
#' lower levels and listing zero or more ACTUS contractIDs at leaf nodes. The 
#' tree is created in the $root attribute of the AccountsTree. Its nodes are 
#' assigned unique nodeIDs. The new AccountsTree objects is returned 
#' 
#' @param yamlstring  character: string describing tree structure see examples
#' @return  AccountsTree S4 object: initialized, ready for Financial Model use
#' @export
#' @import data.tree
#' @import yaml
#' @examples {
#' yamlstring <- 
#' "
#'  name:  modelBank 
#'  Assets:
#'    Current:
#'      actusCIDs:
#'        - pam001
#'        - ann002
#'    ShortTerm:
#'      actusCIDs:
#'        - pam003
#'  Liabilities:
#'    Debt:
#' "
#' accountsTree <- AccountsTree(yamlstring)
#' }

setMethod(f = "AccountsTree", signature = c(yamlstring="character"),
          definition= function(yamlstring) {
            accountsTree <- AccountsTree()
            #check whether it is a path that was entered (are the last 5 characters ".yaml"?)
            if(endsWith(yamlstring, ".yaml")){
              yamlstring <- yaml.load_file(yamlstring)
            }
            accountsTree$root <- treeFromYamlString(yamlstring)
            accountsTree$root <- setUniqueNodeIDs (accountsTree$root)
            return(accountsTree)
          })

# ***********
# Function to create treeFromYamlString
treeFromYamlString <- function(yamlstring) 
  return(as.Node(yaml.load(yamlstring),interpretNullAsList = TRUE))

# ***************
# Function to assign unique NodeID at each node 
setUniqueNodeIDs <- function(root) {
  root$Set(nodeID = 1:root$totalCount)
  return(root)
}

# ************
# Function to return a yaml string from an AccountsTree
#' treeToYamlString("accounsTree")
#' 
#' This function returns a yaml string representation of the accounts tree, it can later
#' be used to recreate the accounts tree using the AccountsTree constructor.
#' @param  accountsTree  AccountsTree: the accounts tree to be converted to yaml 
#' @returns yaml string: describing the accounts tree structure   
#' @import data.tree
#' @import yaml
#' @export
treeToYamlString <- function(accountsTree){
  treeList <- ToListSimple(accountsTree$root, unname = TRUE, keepOnly = c("actusCIDs", "functionIDs"))
  yamlString <- as.yaml(treeList)
  return(yamlString)
}

# *********
# Clone an AccountsTree - deep copy - assumed initialized and nodeIDs set  
# *****************
setGeneric("clone",
           function(accounts) { standardGeneric("clone") }
)
setMethod("clone", c(accounts= "AccountsTree"), 
          function(accounts){ 
            accountsTree <- AccountsTree()
            accountsTree$root <- Clone(accounts$root)
            accountsTree$root <- setUniqueNodeIDs(accountsTree$root)
            return(accountsTree)
          }
)

#  Function to create aggregated hierarchical NMV reports from contract NMVs 
# *****************
#  accountNMVreports(host=Node,vlen, vnames,cidNMVreports)
#     This function saves an aggregated NMV report of length vlen with report 
#     dates as specified in vnames to every subnode in the accounts tree
#     Input parameter cidReports is a list keyed by CID of NMV report values 
#     for each contract. Each report is a numeric vector computed by scenario -
#     ScenarioAnalysis method nominalValueReports( ). All report 
#     vectors have the same length - determined by timeline of the analysis. 
#     Flow reports have one less element than status/value reports. Function 
#     addAggregateReport() computes account node report values recursively:
#     (1) nonleaf accounts do a vector sum of their child node reports
#     (2) leaf nodes owning at least one actusCID, do a vector sum of the
#         cidReports for their assigned contracts 
#     (3) leaf nodes with no assigned actusContracts generate a report vector 
#         of the correct length ( for this report type) with 0's for no values
#         allowing zeros simplifies vectorSum - keeps numeric vectors 
# ********
setGeneric("accountNMVreports",
           function(host, vlen, vnames, cidNMVlist) 
           { standardGeneric("accountNMVreports") }
)


setMethod("accountNMVreports",
          c(host = "Node", vlen = "numeric", vnames = "character",
            cidNMVlist = "list"), 
          function(host, vlen, vnames, cidNMVlist ){ 
            if (isNotLeaf(host)) 
              host$nmv <- 
                fxVectorSum(lapply( 
                  host$children, 
                  function(child) unlist(accountNMVreports(child, vlen, 
                                                             vnames,
                                                             cidNMVlist))
                  ),
                  vlen,vnames
                  )
            else if ( is.null(host$actusCIDs) )  host$nmv <- rep(0,vlen)
            else {
                   host$nmv <- 
                     fxVectorSum(lapply(host$actusCIDs, 
                                        function(cid) 
                                          unlist(cidNMVlist[cid])),
                                 vlen,vnames
                                 )
                  }  
           return(host$nmv) # return specific report parents need 
          }
)


# fxVectorSum(vlist,vlen) 
#           does element wise aggregation of a list of numeric vectors
#           of equal length vlen and returns the vector of sums. We 
#           expect to use this in aggregating report values to higher level 
#           nodes in the tree. 
fxVectorSum <- function(vlist, vlen, vnames= NULL) {
  vsum <- rep(0,vlen)
  for (i in 1:vlen)
    vsum[i] <-  sum( sapply(vlist, function(v) v[i]))
  if ((! is.null(vnames)) & (length(vnames)==vlen) )
    names(vsum) <- vnames
  return(vsum)
}

#  Function to create aggregated hierarchical LQ reports from contract LQs 
# *****************
#  accountLQreports(host=Node,vlen, vnames,cidLQreports)
#     This function saves an aggregated LQ report of length vlen with report 
#     dates as specified in vnames to every subnode in the accounts tree
#     Input parameter cidLQReports is a list keyed by CID of LQ report values 
#     for each contract. Each report is a numeric vector computed by 
#     ScenarioAnalysis method liquidityReports( ). The function is identical to 
#     account NMVreports except that results are saved in $lq attribute of each 
#     nodeAll and input is from cidLQreports  
# Instances of this generic method in: FinancialModel.R, ScenarioAnalysis.R

setGeneric("accountLQreports",
           function(host, vlen, vnames, cidLQlist) 
           { standardGeneric("accountLQreports") }
)


setMethod("accountLQreports",
          c(host = "Node", vlen = "numeric", vnames = "character",
            cidLQlist = "list"), 
          function(host, vlen, vnames, cidLQlist ){ 
            if (isNotLeaf(host)) 
              host$lq <- 
                fxVectorSum(lapply( 
                  host$children, 
                  function(child) unlist(accountLQreports(child, vlen, 
                                                           vnames,
                                                           cidLQlist))
                ),
                vlen,vnames
                )
            else if ( is.null(host$actusCIDs) )  host$lq <- rep(0,vlen)
            else {
              host$lq <- 
                fxVectorSum(lapply(host$actusCIDs, 
                                   function(cid) 
                                     unlist(cidLQlist[cid])),
                            vlen,vnames
                )
            }  
            return(host$lq) # return specific report parent node needs 
          }
)

#  Function to create aggregated hierarchical NPV reports from contract NPVs 
# *****************
#  accountNPVreports(host=Node,vlen, vnames,cidNPVreports)
#     This function saves an aggregated NPV report of length vlen with report 
#     dates as specified in vnames to every subnode in the accounts tree
#     Input parameter cidNPVReports is a list keyed by CID of NPV report values 
#     for each contract. Each report is a numeric vector computed by 
#     ScenarioAnalysis method netPresentValueReports( ). The function is 
#.    identical to accountNMVreports and accountLQreports except that results 
#     are saved in $npv attribute of each node and input is from cidNPVreports  
#  Instances of this generic method in: FinancialModel.R, ScenarioAnalysis.R

setGeneric("accountNPVreports",
           function(host, vlen, vnames, cidNPVlist) 
           { standardGeneric("accountNPVreports") }
)

setMethod("accountNPVreports",
          c(host = "Node", vlen = "numeric", vnames = "character",
            cidNPVlist = "list"), 
          function(host, vlen, vnames, cidNPVlist ){ 
            if (isNotLeaf(host)) 
              host$npv <- 
                fxVectorSum(lapply( 
                  host$children, 
                  function(child) unlist(accountNPVreports(child, vlen, 
                                                          vnames,
                                                          cidNPVlist))
                ),
                vlen,vnames
                )
            else if ( is.null(host$actusCIDs) )  host$npv <- rep(0,vlen)
            else {
              host$npv <- 
                fxVectorSum(lapply(host$actusCIDs, 
                                   function(cid) 
                                     unlist(cidNPVlist[cid])),
                            vlen,vnames
                )
            }  
            return(host$npv) # return specific report parent node needs 
          }
)




# **********************************************
# ******** AccountsTree.R file organized up to here 
# ********  material below this not yet used/tested 

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
  if (isNotLeaf(account)){  }
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

# *****************
#  Function to create aggregated hierarchical NominalValue Report vectors 
#  from contract reports using fixed all numeric vectors simple fxVectorSum()
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
#         of the correct length ( for this report type) with 0's for no values
#         allowing zeros simplifies vectorSum - keeps numeric vectors 
# ********
#aggregateNMVreports <- function(account,nmvReports,vlen, vnames){
#  if (isNotLeaf(account))
#    account$nmv <- 
#      fxVectorSum(lapply(
#        account$children,
#        function(child) unlist(aggregateNMVreports(child,nmvReports,
#                                                   vlen, vnames))),
#        vlen,vnames
#      )
#  else if ( is.null(account$actusCIDs) )  account$nmv <- rep(0,vlen)
#  else {
#    account$nmv <-
#      fxVectorSum(lapply(account$actusCIDs, function(cid) unlist(nmvReports[cid])),
#                  vlen,vnames)
#  }  
#  return(account$nmv) # return specific report parents need 
# }


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

                       
