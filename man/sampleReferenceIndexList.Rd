% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReferenceIndex.R
\name{sampleReferenceIndexList}
\alias{sampleReferenceIndexList}
\title{sampleReferenceIndexList(rfxsfn)
 
    Function to read a csv file with data specifying a collection of 
    RiskFactorIndexes. Each record in the csv file defines a different 
    RiskFactorIndex. The columns in the csv file are: (1) rfType,
    (2) marketObjectCode (3) base (4) dataPairCount (5) <for X = 1,2,3,4>:
    date.X ,  value.X 
    
    Input csv file format: 
    rfType is the fixed string "referenceIndex"; marketObjectCode is a  
    character string with the MOC of the reference index; base is a numeric
    numeric value typically 1.00 or 100 to set whether an interest rate of  
    5% pa is coded as 5.00 or 0.05; columns 5-13 are used to specify a list 
    of up to 4 <date,value> pairs for the reference index. Dates have the  
    format yyyy-mm-dd. Values are numeric.  
     
    Processing: the rfID for referenceIndex with marketObjectCode="moc1" is
                set to "sample$moc1";  different risk scenarios may need 
                different rfid's for the same marketObjectCode. Multiple
                risk scenarios are not supported using this referenceIndex
                data import function/format 
    WARNING -   this csv input data format is different from (and not 
                compatible with) the csv input file format used in 
                sampleReferenceIndex() function. The format used here is 
                convenient for defining a set of ReferenceIndexes with very
                limited data for testing portfolio simulation using a single 
                risk scenario. The csv input file format used in 
                sampleReferenceIndex() data import is more scalable and must 
                be used when referenceIndex data will be entered for 
                multiple risk scenarios.}
\usage{
sampleReferenceIndexList(rfxsfn)
}
\arguments{
\item{rfxsfn}{character  full path filename of csv input data file}
}
\value{
list of S4 objects class=ReferenceIndex
}
\description{
sampleReferenceIndexList(rfxsfn)
 
    Function to read a csv file with data specifying a collection of 
    RiskFactorIndexes. Each record in the csv file defines a different 
    RiskFactorIndex. The columns in the csv file are: (1) rfType,
    (2) marketObjectCode (3) base (4) dataPairCount (5) <for X = 1,2,3,4>:
    date.X ,  value.X 
    
    Input csv file format: 
    rfType is the fixed string "referenceIndex"; marketObjectCode is a  
    character string with the MOC of the reference index; base is a numeric
    numeric value typically 1.00 or 100 to set whether an interest rate of  
    5% pa is coded as 5.00 or 0.05; columns 5-13 are used to specify a list 
    of up to 4 <date,value> pairs for the reference index. Dates have the  
    format yyyy-mm-dd. Values are numeric.  
     
    Processing: the rfID for referenceIndex with marketObjectCode="moc1" is
                set to "sample$moc1";  different risk scenarios may need 
                different rfid's for the same marketObjectCode. Multiple
                risk scenarios are not supported using this referenceIndex
                data import function/format 
    WARNING -   this csv input data format is different from (and not 
                compatible with) the csv input file format used in 
                sampleReferenceIndex() function. The format used here is 
                convenient for defining a set of ReferenceIndexes with very
                limited data for testing portfolio simulation using a single 
                risk scenario. The csv input file format used in 
                sampleReferenceIndex() data import is more scalable and must 
                be used when referenceIndex data will be entered for 
                multiple risk scenarios.
}
\examples{
{
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   rfxsfn <- paste0(mydatadir,"/RiskFactors.csv")
   rfxsl <- sampleReferenceIndexList(rfxsfn)
}
}
