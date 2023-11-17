# generatePortFolioData.R
# authors Donat Maier, Francis Parr
# Sept 2022 
# R utility code to build csv data files with sample Portfolios for simulating 
# static behavior of a toy bank 
# Lubridate is part of the tidyverse and OK to use - but avoid until requirement
# becomes clear ( fewer dependent packages is better ) FNP 
#' @import lubridate
#' @importFrom stats runif
#' @importFrom utils write.csv

# ***************************
# generateContractCsvData ( )
#   This function creates a csv file with data for a sample randomized collection of
#   contracts of mixed PAM, OPS,ANN contract type. It is not externalized
#   because its primary use is for FEMSdevPkg developer ( NOT the FEMSdevPkg
#   user ) to create a csv sample data file which is copied into inst/extdata
#  and then made available to the packuser by installSamplePortfolio() etc
#  The csvFilepath parameter is a character string with the full path and 
#  file name wher the generated csv data will be written 

generateContractCsvData <- function(pamCount = 20, optionCount=0, 
                                      annuityCount = 0, csvFilePath) {
## Input for Portfoliogeneration (Number of each Contracts)----------
  numberOfPams <- pamCount
  numberOfOptions <- optionCount
  numberOfAnns <- annuityCount 
   
  ## Templates for PAM;OPTNS;ANN------------------
  # Contract terms for an Option Contract:
  optionNames <- c("contractType" ,"contractID" , "contractRole", "contrStrucObj.marketObjectCode", "contrStruc.referenceType", 
                   "contrStruc.referenceRole", "currency", "calendar", "contractDealDate", "statusDate", "purchaseDate", 
                   "priceAtPurchaseDate", "maturityDate", "optionExerciseType", "optionType", "optionStrike1",                 
                   "settlementPeriod", "deliverySettlement", "description")
  # Template/Dummy Values for the Option
  optionValues <- c("OPTNS", "option01", "BUY", "AAPL", "MOC", "UDL", "USD", "NC",
                    "2020-01-01", "2020-01-01","2020-01-02","10","2020-03-30","E",
                    "C", "80", "P0D","S","European call option on underlying MOC AAPL
                          - actus-test-option.json optionTemplate;2020-03-30 maturity out of money")
#creation of an Option template
optionTemplate <- structure(optionValues,
                      .Names = optionNames)
# Contract Terms for Pam Contracts
pamNames <- c("calendar", "businessDayConvention", "endOfMonthConvention", "contractType", "statusDate", "contractRole",                             
               "legalEntityIDRecordCreator", "contractID", "legalEntityIDCounterparty", "cycleAnchorDateOfInterestPayment",         
               "cycleOfInterestPayment", "arrayCycleAnchorDateOfInterestPayment", "arrayCycleOfInterestPayment", "nominalInterestRate", 
               "dayCountConvention", "accruedInterest", "capitalizationEndDate", "cycleAnchorDateOfInterestCalculationBase", 
               "cycleOfInterestCalculationBase", "interestCalculationBase", "interestCalculationBaseAmount", "cyclePointOfInterestPayment",              
               "currency", "amortizationDate", "contractDealDate", "initialExchangeDate", "premiumDiscountAtIED", "maturityDate",                             
               "notionalPrincipal", "cycleAnchorDateOfPrincipalRedemption", "cycleOfPrincipalRedemption", "nextPrincipalRedemptionPayment",           
               "arrayCycleAnchorDateOfPrincipalRedemption", "arrayCycleOfPrincipalRedemption", "arrayNextPrincipalRedemptionPayment", 
               "arrayIncreaseDecrease", "purchaseDate", "priceAtPurchaseDate", "terminationDate", "priceAtTerminationDate",                   
               "marketObjectCodeOfScalingIndex", "scalingIndexAtStatusDate", "cycleAnchorDateOfScalingIndex", "cycleOfScalingIndex",                      
               "scalingEffect", "cycleAnchorDateOfRateReset", "cycleOfRateReset", "rateSpread", "arrayCycleAnchorDateOfRateReset", 
               "arrayCycleOfRateReset", "arrayRate", "arrayFixedVariable", "marketObjectCodeOfRateReset", "cyclePointOfRateReset",                    
               "fixingDays", "rateMultiplier", "description", "contrStrucObj.marketObjectCode", "contrStruc.referenceType", 
               "contrStruc.referenceRole" )
# Template/Dummy Values for PAM Contracts
pamValues <- c("NC", "NULL", "SD", "PAM", "2015-01-01", "RPA", "NULL", "pam01",
                "TEST_LEI_CP", "NULL", "NULL", "NULL", "NULL", "0.00", "30E360", "-999999999", "NULL",
                "NULL", "NULL", "NULL", "NULL", "E", "USD", "NULL", "2015-01-01", "2015-01-02", "0", "2015-04-02",
                "1000", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "-999999999", "NULL", "-999999999", 
                "NULL", "-999999999", "NULL", "NULL", "NULL", "NULL", "NULL", "0.00","NULL", "NULL", "NULL", "NULL", "YC_EE_AAA", "B",
                "NULL", "1", "Long position of a 3-month Zero Coupon Bond starting at 01/02/2015 with Notional of 1000", "NULL", "NULL", "NULL")
#creation of a Pam template
pamTemplate <- structure(pamValues,
                    .Names = pamNames)

annuityNames <- c("calendar","businessDayConvention","endOfMonthConvention" ,
"contractType","statusDate","contractRole" ,
"creatorID","contractID","marketObjectCode" ,
"counterpartyID","contractPerformance","seniority" ,
"nonPerformingDate","prepaymentPeriod","gracePeriod" ,
"delinquencyPeriod","delinquencyRate","cycleAnchorDateOfFee" ,
"cycleOfFee","feeBasis","feeRate" ,
"feeAccrued","cycleAnchorDateOfInterestPayment","cycleOfInterestPayment",
"nominalInterestRate","dayCountConvention","accruedInterest" ,
"capitalizationEndDate","cycleAnchorDateOfInterestCalculationBase","cycleOfInterestCalculationBase" ,
"interestCalculationBase","interestCalculationBaseAmount","currency" ,
"amortizationDate","contractDealDate","initialExchangeDate" ,
"premiumDiscountAtIED","maturityDate","notionalPrincipal",
"cycleAnchorDateOfPrincipalRedemption","cycleOfPrincipalRedemption","nextPrincipalRedemptionPayment" ,
"purchaseDate","priceAtPurchaseDate","terminationDate",
"priceAtTerminationDate","creditLineAmount","marketObjectCodeOfScalingIndex",
"scalingIndexAtContractDealDate","notionalScalingMultiplier","interestScalingMultiplier",
"cycleAnchorDateOfScalingIndex","cycleOfScalingIndex","scalingEffect",
"marketValueObserved","optionExerciseEndDate","cycleAnchorDateOfOptionality",
"cycleOfOptionality","penaltyType","penaltyRate",
"prepaymentEffect","cycleAnchorDateOfRateReset","cycleOfRateReset",
"rateSpread","marketObjectCodeOfRateReset","lifeCap",
"lifeFloor","periodCap","periodFloor",
"fixingPeriod","nextResetRate","rateMultiplier",
"settlementCurrency")

annuityValues <- c("NC","NULL","SD" ,"ANN","2020-10-29","RPA" ,"NULL","LN310003900","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL","NULL" ,
                   "NULL","NULL","NULL" ,"NULL","NULL","NULL" ,"NULL","2020-11-10","P1ML1","0.0525","30E360","NULL" ,"NULL","NULL","NULL" ,
                   "NULL","NULL","USD" ,"NULL","2020-10-29","2020-10-30" ,"0","2030-07-10","1128040.18344582" ,"2020-11-10","P1ML1","NULL",
                   "NULL","NULL","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL",
                   "NULL" ,"NULL","2024-01-09","P1YL1" ,"0.05","YC_EE_AAA","NULL" ,"NULL","NULL","NULL" ,"NULL","NULL","NULL", "NULL")

annuityTemplate <- structure(.Data = annuityValues, .Names = annuityNames)

#check how many Terms given
# allNames <- unique(c(pamNames,optionNames,annuityNames))
# length(allNames)

## Portfolio Generation and Sampling of individual Contracts-----------------

ptf1 <- as.data.frame(matrix(nrow = (numberOfAnns + numberOfOptions + numberOfPams), #creation of a dataframe with the respective rows and colums according to the portfolio size
                             ncol = length(unique(c(names(optionTemplate), names(pamTemplate), names(annuityTemplate))))
                             ))

colnames(ptf1) <- unique(c(names(optionTemplate), 
                           names(pamTemplate), 
                           names(annuityTemplate))) #set colanmes as ContractTerm names of all possible Contract Terms (a lot of them are unused!)

ptf1[1:numberOfPams, names(pamTemplate)] <- as.data.frame(t(as.data.frame(pamTemplate))) # Pam Templates added to the df

if(numberOfOptions > 1){
  ptf1[(numberOfPams+1):(numberOfPams+numberOfOptions),names(optionTemplate)] <- as.data.frame(t(as.data.frame(optionTemplate))) # Options Template added to the df
}
if(numberOfAnns > 0){
  ptf1[(numberOfPams+numberOfOptions+1):(numberOfOptions+numberOfPams+numberOfAnns),names(annuityTemplate)] <- as.data.frame(t(as.data.frame(annuityTemplate))) # ANN Template added to the df
}


for(i in 1:(numberOfAnns + numberOfOptions + numberOfPams)){
  ptf1[i,is.na(ptf1[i,])] <- "NULL" #Replace NA Values with "NULL"
}

#set seed because of random numbers in the sampling Process...
set.seed(2022)
#modify/sample the PAMs
for(i in 1:numberOfPams){
  ptf1[i,"contractID"] <- paste0("pam0",i) #numerate the contractIDs
  ptf1[i,"contractRole"] <- c("RPA","RPL")[sample(c(1,2),size = 1,prob = c(0.6,0.4))]
  ptf1[i,c("contractDealDate","statusDate")] <- as.character(sample(seq(as.Date('2019/01/01'), as.Date('2021/01/01'), by="day"), numberOfPams)[i]) #set random Issuedates between 2019 and 2021
  ptf1[i,"initialExchangeDate"] <- as.character(as.Date(ptf1[i,"contractDealDate"]) %m+% days(1)) #set different maturitys between 3 and 6 years
  ptf1[i,"maturityDate"] <- as.character(as.Date(ptf1[i,"contractDealDate"]) %m+% years(sample(3:10,1))) #set different maturitys between 3 and 6 years
  ptf1[i,"cycleAnchorDateOfInterestPayment"] <- as.character(as.Date(ptf1[i,"contractDealDate"]) %m+% years(1))
  ptf1[i,"cycleOfInterestPayment"] <- c("P1YL0","P6ML0")[sample(1:2,1)]
  ptf1[i,"nominalInterestRate"] <- as.character(round(runif(1,0,0.05),digits = 2)) #set random interest rates between 0 and 5%
  ptf1[i,"premiumDiscountAtIED"] <- "0" #here one could sample a discount on the bond at issue
  ptf1[i,"rateSpread"] <- "0" #here we could set a rate spread if we want to
  ptf1[i,"notionalPrincipal"] <- as.character(sample(1000:10000,1))
  ptf1[i,"legalEntityIDCounterparty"] <-ifelse(ptf1[i,"contractRole"] == "RPA","TEST_LEI_CP","TEST_BUY_CP")
}
#modify/sample the options
if(numberOfOptions > 0){
  for(i in (numberOfPams+1):(numberOfPams+numberOfOptions)){
    ptf1[i,"contractID"] <- paste0("option0",i-numberOfPams)
    ptf1[i,"contractRole"] <- c("BUY","SEL")[sample(1:2,1)]
    ptf1[i,c("contractDealDate","statusDate")] <- as.character(sample(seq(as.Date('2020/01/01'), as.Date('2021/01/01'), by="day"), numberOfOptions)[(i-numberOfPams)])
    ptf1[i,"purchaseDate"] <- as.character(as.Date(ptf1[i,"contractDealDate"])%m+% days(1))
    ptf1[i,"priceAtPurchaseDate"] <- as.character(sample(3:10,1))
    ptf1[i,"maturityDate"] <- as.character(as.Date(ptf1[i,"purchaseDate"]) %m+% years(3))
    ptf1[i,"optionType"] <- c("C","P")[sample(1:2,1)]
    ptf1[i,"optionStrike1"] <- as.character(ifelse(as.numeric(ptf1[i,"priceAtPurchaseDate"]) < 7 & ptf1[i,"optionType"]=="C",80+sample(1:10,1),80- sample(1:10,1)))
}

}
#modify/sample the Annuitys
if(numberOfAnns > 0){
  for(i in (numberOfPams+numberOfOptions+1):(numberOfPams+numberOfOptions+numberOfAnns)){
    ptf1[i,"contractID"] <- paste0("annuity0",i-(numberOfPams+numberOfOptions))
    ptf1[i,"contractRole"] <- c("RPA","RPL")[sample(c(1,2),size = 1,prob = c(0.7,0.3))]
    ptf1[i,c("contractDealDate","statusDate")] <- as.character(sample(seq(as.Date('2018/01/01'), as.Date('2022/01/01'), by="day"), 1))
    ptf1[i,"initialExchangeDate"] <- as.character(as.Date(ptf1[i,"contractDealDate"])%m+% days(1))
    ptf1[i,"maturityDate"] <- as.character(as.Date(ptf1[i,"initialExchangeDate"]) %m+% years(sample(5:10,1)))
    ptf1[i,c("cycleAnchorDateOfPrincipalRedemption","cycleAnchorDateOfInterestPayment")] <- as.character(as.Date(ptf1[i,"statusDate"]) %m+% days(12))
    ptf1[i,"nominalInterestRate"] <- as.character(round(runif(1,0,0.05),digits = 2))
    ptf1[i,"notionalPrincipal"] <- as.character(sample(10000:1000000,1))
    ptf1[i, "cycleAnchorDateOfRateReset"] <- as.character(as.Date(ptf1[i,"statusDate"]) %m+% years(round(as.double(difftime(as.Date(ptf1[i,"maturityDate"]),as.Date(ptf1[i,"statusDate"]),units = "days")/365)/2,digits = 0)))
    ptf1[i,"rateSpread"] <- as.character(round(runif(1,min = 0,max = 0.06),2))
  }
}

ptf1[,"description"] <- "NULL" #delete the description / would have to be set manually...

## Write the csv-----------
write.csv(ptf1, csvFilepath, row.names = FALSE) #save the CSVfile

}
