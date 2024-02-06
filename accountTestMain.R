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
fname <-"~/DaDFiR3/FinancialModelSampleData/Powerplant.yaml"
write_yaml(accountsList,fname)
accounts1List <- read_yaml(fname)
accounts1Node <- as.Node(accounts1List, interpretNullAsList = TRUE)
print(accounts1Node)
