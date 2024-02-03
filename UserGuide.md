# FEMSdevPkg User Guide
## Specifying an Accounts tree structure for enterprise. 
It is convenient to enter the tree structure as a text string in a format which:
* starts each account name  on a new line,
* terminates each account name with a colon,
* uses indentation to express the level of the account in the accounts tree.
A text string in this format can be read as yaml, then passed as a yaml structure
to create a data.tree accountsTree. 

A sample sequence of r language commands to do this is illustrated below. 
```
library(data.tree)
library(yaml)
yaml<- "
name: Powerplant Accounts
Assets:
  Current:
  ShortTerm:
  Longterm:
Liabilities:
  Debt:
  Equity:
Operations:
  Revenues:
  Expenses:
"
accountsList <- yaml.load(yaml)
accountsNode <- as.Node(accountsList, interpretNullAsList = TRUE)
print(accountsNode)
```
This accounts tree structure can also be writtten out to a local file with: write_yaml(yaml, <filename> ) 
and then read back in  to initialize another Financial Model with the same accounts tree using: 
yaml <- read_yaml(<filename>) 



