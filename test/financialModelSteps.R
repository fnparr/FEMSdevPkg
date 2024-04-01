# FinancialModelSteps - step through financial model analysis 
#                     - targeting exported functions from FEMSdevPkg
# ********
rm(list=ls())
# Step 1: create a string with structural information for model bank accounts
#         and ACTUS contract ids ( also function IDs) assigned to each account
yamlstring <- "
name:  modelBank
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
# Step 2: Use string above to create an accounts tree 
accountsTree <- AccountsTree(yamlstring) 
print(accountsTree$root,"actusCIDs", "nodeID")

