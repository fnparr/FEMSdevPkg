# import.R 
# import.R - working development version of import.R
# fnp Feb 2022, Sep 2023
library(utils)
library(timeSeries)

# ***************************************
# xxxfile2dataframe(filename) xxx=contractFile OR riskFile
#    reads named file; creates clean df
#    unify file2Contracts_df(filename)
#    and file2riskFactors_df(filename)
#       FNP 25th April 2022
#    for contracts we need to force dayCountConvention to be read
#    as character ( "30E360" would be read numeric )  BUT
#    field does not occur in riskData csv files
# ***************************************
contractFile2dataframe <- function(cdfn, sep = ",") {
   # read csv ignores and strips '"' and dayCountConvention has 30E360 value
   # which gets read as numeric = Inf.
   # we assume all contract csv files will have a dayCountConvention column
   df <- utils::read.csv(cdfn, colClasses = c(dayCountConvention = "character"))
   # convert all missing data into text null
   df[is.na(df)] <- "NULL"
   return(df)
}

riskFile2dataframe <- function(fname, sep = ","){
  df = utils::read.csv(fname)
  # convert all missing data into text null
  df[is.na(df)] <- "NULL"
  return(df)
}

# ************************************
# contracts_df2list(contracts_df)
#   build list of contracts from df
#   expanded cleaned version of df2contracts_list
#  Split df: terms, legs, descriptp (once)
#  for each row: createContract(terms, legs, irow)
#      append into returned list  fnp  10 Apr 2022
#    -- improved version df2contracts_list()
# ************************************************

contracts_df2list<- function(contracts_df){
  nonTermCols <- c("description","contrStrucObj.marketObjectCode",
                   "contrStruc.referenceType", "contrStruc.referenceRole")
  terms_df <-contracts_df[!names(contracts_df) %in% nonTermCols]
  legs_df <-data.frame(
         marketObjectCode = contracts_df["contrStrucObj.marketObjectCode"],
         referenceType = contracts_df["contrStruc.referenceType"],
         referenceRole = contracts_df["contrStruc.referenceRole"]
         )
  outlist <- list()
  for ( irow in 1:nrow(contracts_df)){
    outlist <- append (outlist, datarow2Contract(terms_df,legs_df,irow) )
  }
  return (outlist)
}

# ************************************
# riskFactors_df2list(riskFactors_df)
#   input: dataframe riskFactor data,
#   returns list of riskFactor objects
#   convert date, value pairs in risk Factor row
#   all riskFactors are referenceIndex for now
# ************************************************

riskFactors_df2list <- function(rfxsdf){
  rfxlist <- list()
  nhdrs <- 4        # rfType, moc, base, dataPairCount are " row headers"
  for ( irow in 1:nrow(rfxsdf)){
      rfRow <- rfxsdf[irow,]
      tset <- as.character(rfRow[nhdrs-1+(1:rfRow$dataPairCount)*2])
          # vector of dates
      vset <- as.numeric(rfRow[nhdrs+(1:rfRow$dataPairCount)*2])
           # vector of numeric values
      rfID <- paste0("sample$",rfRow$marketObjectCode)
      rfxlist <-append(rfxlist,
                       Index(rfID,rfRow$marketObjectCode,rfRow$base,,
                             tset,vset))
      }
  return(rfxlist)
}

# ***********************************************
# datarow2Contract ( ) -  create contract object
#    inputs:  terms_df, legs_df, descr, irow :
#    contractType; object <- new("contractType)
#    constructors will set isStructured but not populate terms or legs
#    if isStructured: insertLegs
#    insertTerms ( both simple and structured cases )
# ************************************************
datarow2Contract<- function(terms_df, legs_df,irow){
  contractTypeName <- longName(tolower(terms_df$contractType[irow]))
  contract <- CT(contractTypeName)
  #FNP  avoid validity check for now 10Apr2022; test PAM,OPTNS
  if (contractTypeName == "Option"){
     contract$contractStructure<-list(
               CLeg(legs_df$contrStrucObj.marketObjectCode[irow])
               )
     contract$isCompound <- TRUE
  } else {
     contract$isCompound <- FALSE
  }
  # insert terms - skipping term validy checks for now FNP Apr 2022
  contractTerms <- as.list(t(terms_df[irow,]))
  names(contractTerms) <- colnames(terms_df)  # reattach column names

  ## drop all NULL elements. sapply operates on a list but returns a vector
  contractTerms <- contractTerms[sapply(contractTerms, function(x) x != "NULL")]

  set(object = contract, what = contractTerms)
  return(contract)
}

# *********************************************************
# installSampleData(<directory-file-path>) 
# ****************

# ***********************************************
#' installSampleData 
#' 
#'  This function copies asample csv data files into a user selected directory 
#'  where they can be easily inspected or modified. This demonstrates the 
#'  required format for additional contract and riskFactor data files to be 
#'  used in FEMSdev Pkg requests 
#'  .  
#'  Sample file BondPortfolio.csv specifies ACTUS contract terms for  a 
#'  portfolio of PrincipalAtMaturity ( bullet ) bonds. Sample file 
#'  RiskFactors.csv specifies projected # future values for a collection of
#'  marketObjectCodes designated by contracts as their baseline for resetting 
#'  rates or, in the case of (european) stock options, market value of the 
#'  underlying 
#'  
#'  The input parameter <directory-file-path> locates a directory where the 
#'  sample data files should be copied to. ( FEMSdevPkg contains a compressed 
#'  form of these sample csv files not conveniently visible or accessible to the
#'   FEMSdevPkg package user.
#'
#' @param mydatadir  character - full path name of directory to write sample csvs
#'
#' @return NULL
#' @export
#'
#' @examples  {              # directory ~/mydata must exist and be writable 
#'   datadir <- "~/mydata"
#'   installSampleData(datadir)
#'   }
#'   
installSampleData <- function (mydatadir){
  for (fn in  c("BondPortfolio.csv","AnnuityPortfolio.csv",
                "OptionsPortfolio.csv", "RiskFactors.csv",
                "UST5Y_fallingRates.csv", "UST5Y_recoveringRates.csv",
                "UST5Y_risingRates.csv", "UST5Y_steadyRates.csv")) {
    pn <- paste0(mydatadir,"/",fn)
    file.copy(from = system.file("extdata",fn, package = "FEMSdevPkg"),
              to = pn, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  }
}

# ***********************************************
#'  installSampleCode 
#' 
#'  This function copies the code of a sample R application importing and using 
#'  the FEMSdevPkg package in a user specified directory. The copied code
#'  introductoryDemo.R provides a simple guide and introduction to using the 
#'  package. This code is documented with comments  introductoryDemo.R is copied.
#'  
#'  The input parameter <directory-file-path> of installSampleCode locates a 
#'  directory where the sample data files should be copied to. 
#'
#' @param demodir  character - full path name of folder to write demo R code 
#'
#' @return NULL
#' @export
#'
#' @examples {
#'   demodir <- "~/mycode"
#'   installSampleCode(demodir)
#'   }
#'   
installSampleCode <- function (demodir){
    pn <- paste0(demodir,"/","introductoryDemo.R")
    file.copy(from = system.file("code-examples","introductoryDemo.R", 
                                  package = "FEMSdevPkg"),
              to = pn, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    pn <- paste0(demodir,"/","workshopDemo.R")
    file.copy(from = system.file("code-examples","workshopDemo.R", 
                                 package = "FEMSdevPkg"),
              to = pn, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
}
