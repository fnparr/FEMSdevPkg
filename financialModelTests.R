# FinancialModel unit test script 
library(data.tree)
# needed for accounts 
# ********
# Test 0.1 : clear environment and create financial model instance 
rm(list=ls())
fm <- FinancialModel()
# Test 0.2 
rm(list=ls())
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
fmID <- "fm001"
fmDescr <- "Text describing the model"
entprID <-"Powerplant"
