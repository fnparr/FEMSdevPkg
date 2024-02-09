library(data.tree)
library(yaml)
# Test 0.1
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
