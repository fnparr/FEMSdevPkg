library(data.tree)
library(yaml)
rm(list=ls())
# Test 1.  Create an AccountsTree using treeFromYamlString and print out tree
#          structure contract lists at leaf  nodes 
yaml2 <- "
name: Powerplant Accounts
Assets:
  Current:
     actusCIDs:
        - pam001
        - pam002
        - ann003
  ShortTerm:
     actusCIDs:
        - pam004
        - ann005
  LongTerm:
     functionIDs:
        - edf006
Liabilities:
  Debt:
     actusCIDs:
        - pam007
  Equity:
Operations:
  Cashflows:
     functionIDs:
        - ocf008
"
accounts <- treeFromYamlString(yaml2)
print(accounts)

accounts$Assets$Current$actusCIDs
accounts$Assets$ShortTerm$actusCIDs
accounts$Assets$LongTerm$functionIDs

# Test 2.  print accounts tree showing the contracts vectors at each leaf 
print(accounts, "actusCIDs","functionIDs")

# Test 3.  create path for a node as a string vector, show CIDs indexable
# tree indexing using climb and account name segments 
accounts$Climb(name="Assets")$Climb(name="ShortTerm")$path 
accounts$Climb(name="Assets")$Climb(name="ShortTerm")$path[1]

# Test 4.  generate unique nodeID for each leaf and non leaf node
accounts <- setUniqueNodeIDs(accounts)
print(accounts,"nodeID","actusCIDs", "functionIDs")

# Test  5.1  convert to data frame - row per leaf node - string of CIDs 
treemap2 <- ToDataFrameTable(accounts,"path","nodeID","actusCIDs","liquidity")
treemap2
# Test 5.2 convert to list of row prefixes ; use this for building reports
treedf <- as.data.frame(accounts)
treedf$levelName[1]
treedf$levelName[2]

#Test 6.  use cid2NodeIdMap function to relate CIDs to owning NodeID 
mp <- cid2NodeIdMap(accounts) 
mp

# Test 7.   cid2NodeId( ) return NodeID of owner for contract with  CID = cid
cid2NodeId("pam001",accounts)
cid2NodeId("ann005",accounts)

# Test 8. Assign liquidity report vectors to each contract  

# Next series of tests SHOULD be to attach liquidity reports to the contracts
# and FunctionID - aggregate contract level report vectors back to leaf node 
# aggregates then back up the tree 

# Test 8.1 Create A set of liquidity report vectors - change per period
#  arbitrary test data - no significance  
lqNull        <- c(jun24=  0.0, dec24=  0.0, jun25=  0.0)
lqSteadyInc   <- c(jun24=  1.0, dec24=  1.1, jun25=  1.2)
lqRoi         <- c(jun24= -2.0, dec24=  0.0, jun25=  3.0)
lqRandom      <- c(jun24 = 1.8, dec24= -0.2, jun25= 0.6 )
lqPulse       <- c(jun24=  0.0, dec24=  3.0, jun25= 0.0)
lqGrow        <- c(jun24=  0.4, dec24=  0.2, jun25=  1.5)
lqShrink      <- c(jun24= -4.9, dec24= -2.2, jun25= -1.0)

# Test 8.2  Create a list of liquidity reports, one per contract 
#          This is cid test data to be set at leafs for tree aggregation of 
#          reports 

lqReportList <- list( "pam001" = lqSteadyInc,
                      "pam002" = lqRoi,
                      "ann003" = lqRandom,
                      "pam004" = lqPulse,
                      "ann005" = lqGrow,
                      "pam007"= lqShrink
                      )
lqReportList["pam001"]
names(lqReportList[[1]])
print(unlist(lqReportList))
# ****************
# Test 8.3 Add(Liquidity) aggregated reports into accounts tree and print 
# *************
addAggregatedLiquidity(accounts, lqReportList)
print(accounts, "liquidity")

# ************
# Functions for aggregating report vectors in the accounts tree  
# ***********
# fxVectorSum(vlist,vlen) 
#           does element wise aggregation of a list of numeric vectors
#           of equal length vlen and returns the vector of sums. We 
#           expect to use this in aggregating report values to higher level 
#           nodes in the tree. For each report in the vlist input, we have to 
#           unlist it  to get the vector of values then index into that
#           and sum the indexed values over each vectorin the list
fxVectorSum <- function(vlist, vlen, vnames= NULL) {
    vsum <- rep(0,vlen)
    for (i in 1:vlen)
      vsum[i] <-  sum( sapply(vlist, function(v) v[i]))
    if ((! is.null(vnames)) & (length(vnames)==vlen) )
          names(vsum) <- vnames
    return(vsum)
}

# Function aggregateNMVreports() to be developed using sumFixedVectors
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
aggregateNMVreports <- function(account,nmvReports,vlen, vnames){
  if (isNotLeaf(account))
    account$nmv <- 
      fxVectorSum(lapply(
        account$children,
        function(child) unlist(aggregateNMVreports(child,nmvReports,
                                                   vlen, vnames))),
        vlen,vnames
         )
  else if ( is.null(account$actusCIDs) )  account$nmv <- rep(0,vlen)
  else {
    account$nmv <-
      fxVectorSum(lapply(account$actusCIDs, function(cid) unlist(nmvReports[cid])),
                  vlen,vnames)
  }  
  return(account$nmv) # return specific report parents need 
}
account <-accounts$Assets$Current
account$actusCIDs
vnames
vlen
fxVectorSum(lapply(account$actusCIDs,function(cid) unlist(knnvrs[cid])),
            vlen,vnames)

# ***********
# now test fxVectorSum()
vlist1 <- list(nvreps=c(10,11,12))
vlen <- 3
fxVectorSum(vlist1,vlen,)
vlist2 <- list(nvreps=c(10,11,12),nvreps= c(20,22,24))
fxVectorSum(vlist2,vlen,c("Jan", "Feb", "Mar"))

# ************************
# now test aggregateNMVreports()
# we need nodeID in each account 
accounts <- setUniqueNodeIDs(accounts)
print(accounts,"nodeID","actusCIDs", "functionIDs")

knnvrs
vlen<-4
fm$timeline$statusDate
fm$timeline$monthsPerPeriod
vnames<-c("Jan23","Jun23","Jan24","Jun24")
aggregateNMVreports(accounts,knnvrs,vlen,vnames)
aggregateNMVreports(accounts$Assets$Current, knnvrs,vlen,vnames)
aggregateNMVreports(accounts$Assets$ShortTerm,knnvrs,vlen,vnames)
aggregateNMVreports(accounts$Assets$LongTerm, knnvrs,vlen,vnames)
aggregateNMVreports(accounts$Assets,knnvrs,vlen,vnames)

# *******
# Now build a reports dataframe extracting fields from Nodes
table <- t(accounts$Get("nmv"))
table
adf<- as.data.frame(accounts)
adf["levelName"]
nmvdf <- data.frame(adf["levelName"],table)

pairs2keylist <- function(pairs,kf,vf) {
  outList <- list()
  for ( i in seq(1:length(pairs))) {
    outList[[unlist(pairs[[i]][kf])]] <-pairs[[i]][vf]
  }
  return(outList)
}
knnvrs <-pairs2keylist(nnvrs,"cid","nvreps")
# *********************
# **** INCOMPLETE ( OLDER)  WORK TOWARD THIS FOLLOWS .... 
# ***********************
# TESTS following this are still in development 
# ********
# Test 8.3  Set an empty list attribute $reports at each accounts node 
accounts <- setEmptyReportsList(accounts)
print(accounts,"reports")
print(accounts,"actusCIDs","reports","nodeID")


vlist <- list(c(1, 1, 1))
VectorSum(vlist )
vlist <- NULL
VectorSum(vlist)
vlist <- list(c(1,1,1), NULL, c( 3,4,5))
VectorSum(vlist)
# ******
addAggregateReport <- function(account,cidReports,reportName){
  if (isNotLeaf(account))
     account$reports[[reportName]] <- 
        VectorSum(lapply(
           account$children,
           function(x) addAggregateReport(x,cidReports,reportName))
        )
  else if ( is.null(account$actusCIDs) ) {
     vlen <- length(cidReports[[1]])
     account$reports[[reportName]] <- rep(NULL, vlen)
  }
  else {
     account$reports[[reportName]] <-
        VectorSum(lapply(account$actusCIDs, function(cid) cidReports[cid]))
  }  
  return(account$reports[[reportName]]) # return specific report parents need 
}


# Test 8.4 (1) valid accountsTree with $reports? (2) valid cidReports ?
print(accounts,"reports")
unlist(lqReportList)
accounts <- addAggregateReport(accounts,lqReportList,"liquidity")
getLQ <- function(node) node$reports[["liquidity"]]
getLQ(accounts$Assets$Current)
print(accounts,"getLQ")
accounts$Set(LQ= NULL)


# Test 10.4 Test aggregation of numeric vectors ( income reports) to tree 
incNull <- c(rep1= 0.0, rep2= 0.0, rep3 = 0.0)
incCurrent <- c(rep1= 1.0, rep2= 1.1, rep3=1.2)
incShortTerm <- c(rep1= 10.0, rep2= 10.1, rep3=10.2)
incLongTerm <- c(rep1 = 0.5, rep2= 0.8, rep3 = 20.9 )
incDebt <- c(rep1= - 0.1, rep2 = - 0.5 , rep3 = - 0.5)


# We would like Aggregate to aggregate these vectors up the tree 
# BUT standard aggregate  seems to refuse to work with vectors ( all of the 
# length and same name structure) So here we simplify and ask it to work 
# just with the first income report value 
# => that Aggregate CAN do but it seems not to set the aggregated value at 
#  any higher up intermediate nodes which is what we wanted 
accnts2Node$Assets$Current$income <- incCurrent
accnts2Node$Assets$ShortTerm$income <- incShortTerm
accnts2Node$Assets$Longterm$income <- incLongTerm
accnts2Node$Liabilities$Debt$income <- incDebt
accnts2Node$Liabilities$Equity$income <- incNull
accnts2Node$Operations$Revenues$income <- incNull
accnts2Node$Operations$Expenses$income <- incNull

# now write the aggregate function which propagates to all accounts in tree
# needs to be able to ignore na values in aggregation 
Aggregate( node=accnts2Node, attribute = "income",  aggFun = sum)
accnts2Node$attributesAll

# Test 10.5 Use set and a recursive function to do aggregation ( on vectors)

# 10.5.1. Test utility function VectorSum(veclist) which takes as input a list 
#         of equal lengthNumeric vectors and return the elementwise aggregte 
#         values as output. Function VectorSum( ) is defined in Accounts.R
incCurrent <- c(rep1= 1.0, rep2= 1.1, rep3=1.2)
incShortTerm <- c(rep1= 10.0, rep2= 10.1, rep3=10.2)
incLongTerm <- c(rep1 = 0.5, rep2= 0.8, rep3 = 20.9 )
VectorSum(list(incCurrent,incShortTerm))
VectorSum(list(incCurrent,incShortTerm,incLongTerm))

# Test 10.5.2 Work towards: leaf2rootAggregate( )
typeof(accnts2Node$children)
names(accnts2Node$children)
# Node$children is a list  keyed by the pathname segment values and presumably 
# in position order 

# Test 10.5.3 Test Income(account) which does vector elementwise aggregation on 
#            income Reports vectors saving aggregated $income vector  in each  
#           subaccount of the input (node) account. Income() is defined in 
#           Accounts.R 

accnts2Node$Assets$Current$income <- incCurrent
accnts2Node$Assets$ShortTerm$income <- incShortTerm
accnts2Node$Assets$Longterm$income <- incLongTerm
accnts2Node$Liabilities$Debt$income <- incDebt
accnts2Node$Liabilities$Equity$income <- incNull
accnts2Node$Operations$Revenues$income <- incNull
accnts2Node$Operations$Expenses$income <- incNull
accnts2Node$Assets$Current$income
Income(accnts2Node$Assets$Current) 
Income(accnts2Node)
accnts2Node$Assets$Current$income
accnts2Node$income
accnts2Node$Assets$income
print(accnts2Node,"income")

# Trying out a possible clear function on non leafs 
clearNonleafIncome(accnts2Node) 

print(accnts2Node,"income")
Income(accnts2Node)
print(accnts2Node,"income")

# 10.5.3  Use filterFun = is.leaf to get lists of leaf and non leaf pathnames
# result is an indexable character matrix 

leafPaths <- accnts2Node$Get('path', filterFun = isLeaf)
leafPaths

accnts2Node$Get('path',filterFun = isLeaf)[3,]
accnts2Node$Get('path', filterFun = isLeaf)[3,1]
accnts2Node$Get('path',filterFun = isNotLeaf)

# Test 10.6 function which takes a nodeId and a CIDs list, builds list
#          of nodeID, CID> pairs ( NOW DEPRECATED  replaced by cid2NodeIdMap())
nodeCIDpairs <- function(nodeID, CIDs){
     outList <- list()
     sapply (CIDs, function(CID) {
       outList.append( list(nodeID, CID), length(outList)+1)
     })
     return(outList)
}
# may be more useful just to build a vector of CIDs with nodeID names 
# and this can work on an arbitrary list of multiple children 
 cidVector <- function(account){
  cids <- c()
  if ( ! ( isLeaf(account) & (length(account$actusCIDs) == 0 ))) {
    if (isLeaf(account) ) {
      newCIDs <- account$actusCIDs
      names(newCIDs) <- rep(account$nodeID, length(newCIDs))
    } 
    else { newCIDlist <-
      sapply(account$children, 
             function(child){ return(cidVector(child))
             })  
    newCIDs <- c(newCIDlist)
    }
    cids <- c(cids,newCIDs)
  }
  return(cids)  
}

accnts2Node
cidv<- cidVector(accnts2Node)

cidv1 <- function(account) {
  cids <- c()
  if (isLeaf(account)){
    if (length(account$actusCIDs) > 0 ) {
      newCids <- account$actusCIDs
      names(newCids) <- rep(account$nodeId,length(newCIDs))
      cids <- c(cids, newCids)
    }
  } else { cids<- c(cids, 
                    sapply(account$children, 
                           function(child) { return(cidv1(child))}))
  }
  return(cids)
}
cids <- cidv1(accnts2Node)
