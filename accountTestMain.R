library(data.tree)
library(yaml)
# Test 0.1   show tree structure with contractid list at leaf  nodes 
rm(list=ls())
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
     formulaCIDs:
        - frm006
  Longterm:
Liabilities:
  Debt:
  Equity:
Operations:
  Revenues:
  Expenses:
"
accnts2List <- yaml.load(yaml2)
accnts2Node <- as.Node(accnts2List, interpretNullAsList = TRUE)
print(accnts2Node)
accnts2Node$Assets$Current$actusCIDs
accnts2Node$Assets$ShortTerm$actusCIDs
accnts2Node$Assets$ShortTerm$formulaCIDs
# Experiment 0.2  (test failed)  build index of CIDs to owning leaf account
# tree indexing using climb and account name segments 
accnts2Node$Climb(name="Assets")$Climb(name="ShortTerm")$path
# Need to generate a unique nodeID for each leaf and non leaf node for contract
# report aggregation 
accnts2Node$Set(nodeID = 1:accnts2Node$totalCount)
# convert to data frame - row per leaf node  with list - full path, last pos
treemap2 <- ToDataFrameTable(accnts2Node,"path","nodeID","actusCIDs")
# filter out any rows with no defined contracts 
tmcids <- subset(treemap2, ! is.na(actusCIDs))
# build list of contractID to leaf maps 
accountIDmap <- function (tmcids) {
   map <- list() 
  lapply(1:nrow(tmcids), function(i){
    for (j in 1:length(tmcids$actusCIDs[i])) {
      item <- list(cid= tmcids$actusCIDs[j], accountID= tmcids$nodeID[i])
      map[[length(map)+1]] <- item 
    }
  })
  return(map)
}
# => problem is that data.frame merges vector of CIDs into a string
# Test 0.3 Show that the vector of CIDs at each leaf account is indexable 
accnts2Node
length(accnts2Node$Assets$Current$actusCIDs)
accnts2Node$Assets$Current$actusCIDs[2]

# Test 0.4 Test aggregation of numeric vectors ( income reports) to tree 
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
accnts2Node$Assets$Current$income <- incCurrent[1]
accnts2Node$Assets$ShortTerm$income <- incShortTerm[1]
accnts2Node$Assets$Longterm$income <- incLongTerm[1]
accnts2Node$Liabilities$Debt$income <- incDebt[1]
accnts2Node$Liabilities$Equity$income <- incNull[1]
accnts2Node$Operations$Revenues$income <- incNull[1]
accnts2Node$Operations$Expenses$income <- incNull[1]

# now write the aggregate function which propagates to all accounts in tree
# needs to be able to ignore na values in aggregation 
Aggregate( node=accnts2Node, attribute = "income",  aggFun = sum)
accnts2Node$attributesAll

