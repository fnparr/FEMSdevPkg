# import.R 
# import.R - working development version of import.R
# fnp Feb 2022, Sep 2023
# includes xlsx2ptf function developed for FEMSdevBase by Francis Parr
# in Dec 2023.  Cleans up Excel generated file with data specifying a set of
# ACTUS contracts and imports as a portfolio
# Licensing and Copyright notices from sibling FEMSdevBase files there
# defines:
# ************************************************************************
# csvx2ptf(<fnameIn>) - function
# ************************************************************************
#' csvx2ptf(<fnameIn>)
#'
#'   The csvx2ptf(character) function takes as input a character string
#'   which is the path name of Excel generated csv file with data specifying
#'   a collection of ACTUS contracts - one record per contract. The first
#'   record in the csv file has ACTUS term names. Each subsequent record
#'   has term values for the contract specified in that row.
#'
#'   Function csvx2ptf returns an initialized portfolio object populated with
#'   "cleaned up" data for the specified contracts, The returned portfolio can
#'   then be used in an ACTUS portfolio simulation request.
#'
#'   Specific data wrangling issues handled in xlsx2ptf include:
#'   (1) regularizing the file format as a linux csv file by removing any
#'       carriage return characters 0x'0D' inserted by Excel
#'   (2) regularizing the file format as a linux csv file by adding a linefeed
#'       0x'0A' character at the and of the final record when Excel fails to do
#'       this
#'   (3) removing a leading ' ' character from all input ACTUS term date fields
#'       -- actually removing a leading space form ALL fields in the csv data
#'
#'   Our required convention is that all yyyy-mm-dd dates are written in the
#'   Excel spreadsheet and submitted in the generated csv field with a leading
#'   ' ' character, i.e. as " yyyy-mm-dd". This format causes Excel to treat
#'   all date term values in the contract as text strings rather than dates.
#'   The mapping of text strings between Excel data cells and csv values is
#'   simpler and more predictable than the mappings and automatic conversions
#'   which Excel applies to date values.
#'
#' @param fnameIn    character string - full pathname of input csv file object
#' @return    dataframe - of contract term values, one row per contract
#'            with reformatted information from file fnameIn
#' @export
#' @import utils
#' @import timeSeries
#' @import TAF
#' @importFrom    TAF dos2unix
#' @examples {
#'   datadir <- "~/mydata"
#'   installSampleData(datadir)
#'   fname <- "~/mydata/testptf2.csv"
#'   ptf1 <- csvx2ptf(fname)
#' }
#'
csvx2ptf <- function (fnameIn) {
  TAF::dos2unix(fnameIn)
  
  # read csv ignores and strips '"' and dayCountConvention has 30E360 value
  # which gets read as numeric = Inf.
  # we assume all contract csv files will have a dayCountConvention column
  df1 <- utils::read.csv(fnameIn, colClasses = c(dayCountConvention = "character"))
  
  #  remove lead space in all fields (col ops only)
  df1 <- data.frame(lapply(df1,rmLeadSpaces))
  
  # convert all missing data into text "NULL"
  df1[is.na(df1)] <- "NULL"
  
  # convert df2 into a list of - per contract list of terms
  cntrTermLoL <-  lapply(split(df1,seq(nrow(df1))), as.list)
  ptf <- Portfolio()
  ptf$contracts <- lapply(cntrTermLoL,importContract)
  # following line seems to convert the contracts List in portfolio
  # from having named "1","2", ... elements to indexable [1], [2]. [3]
  # seems to avoid an error in running the ACTUS simulation
  # maybe using something other than seq(nrow(df1)) in split would fix ???
  names(ptf$contracts) <- NULL
  return (ptf)
}
# ***************************
# function: rmLeadSpaces(vec)
# Our required convention is that all contract dates in cells of the Excel.
# The cells will be treated as text by Excel; hence the generated csv values
# will also be " yyyy-mm-dd".
# are in " yyyy-mm-dd" format
# this function will remove the leading space from a column of dates
rmLeadSpaces <- function(vec) {
  result <- sapply(vec, function(x) {
    if ((typeof(x) == "character") && (nchar(x) > 1) 
        && (substring(x, 1, 1) == " ")) {
      return(substring(x, 2, nchar(x)))
    } else {
      return(x)
    }
  })
}


# ***************************************
# file2dataframe(filename)
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
  # this read.csv works for csv with no dayCountConvention column. Warning
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

# *********************************************
# importContract(termList)
#
# input parameter termList is list of term values with corresponding term names
# returns a populated object with the correct CT extension of ContractType
# input = termList removes NULL values - eventually other validate checks
# returns initialized contract object - T00:00:00 will be added to dates later
# replaces earlier datarow2contract - BUT no Option support yet
# expect this to be called from new contracts_df2ptf()
# ***********************************

importContract <- function(termList) {
  # delete from list all terms with value character string "NULL"
  tl1 <- termList[sapply(termList, function(x) x != "NULL")]
  return(ContractType(tl1))
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
#' installSampleData
#'
#'  This function copies sample csv data files into a user selected directory
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
                "UST5Y_risingRates.csv", "UST5Y_steadyRates.csv",
                "testptf1.csv", "testptf2.csv", "fmTestPortfolio.csv")) {
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