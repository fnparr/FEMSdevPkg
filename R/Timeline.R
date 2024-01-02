# Timeline.R FEMS dev code by Francis Parr Feb 2024
# Licensing and Copyright notices from FEMS ( TBD) 
# **************************************************
# Timeline is an internal class not exported from FEMSDevPkg 
# User access to Timeline(startDate, monthsPerBucket, repCount bktCount) 
#  **** by including these fields in a CashflowAnalysis( ) constructor 
# provides: date2bucketIndex(date), bucketHorizon, reportNames()
# *************************************************
library(lubridate)
# *******************************************
# S4 class Timeline holds values and methods defining the timeline used for 
# each of the different analysis steps ( liquidity, income, value ) in a 
# cashflow analysis of a portfolio. Methods provided by the class: as above  
# statusDate: (1) must match statusDate of Portfolio (2) must be first day in 
# month (3) character yyyy-mm-dd format; monthsPerBucket: (1) integer (2) 
# divisor or multiple of 12 
# ******************************************
setRefClass("Timeline",
            fields = list(
              statusDate = "character",
              monthsPerBucket = "numeric",
              reportCount = "numeric",  
              bucketCount = "numeric", 
              bucketHorizon = "character", 
              reportDates = "character", 
              bucketDateVector = "Date"
            )
)
# *********************
#  constructors:  Timeline() : (), (<statusDate>, <monthsPerBucket>, 
#.                                  <reportCount>, <bucketCount>  )
setGeneric(name = "Timeline",
           def = function(statusDate, monthsPerBucket, reportCount, bucketCount) 
             {   standardGeneric("Timeline")
           })

setMethod(f = "Timeline", signature = c(),
          definition = function(){
            return(new("Timeline"))
          })

setMethod(f = "Timeline", 
          signature = c("character","numeric", "numeric", "numeric"),
          definition = function(statusDate, monthsPerBucket, reportCount, 
                                bucketCount){
            tl <- Timeline()
            tl$statusDate <- statusDate
            tl$monthsPerBucket <- monthsPerBucket
            tl$reportCount <- reportCount
            tl$bucketCount <-bucketCount
            tl$bucketDateVector <- 
               add_with_rollback(as_date(tl$statusDate),
                            months(seq(1:tl$bucketCount)* tl$monthsPerBucket) 
               )
            return(tl)
          }
) 
# ************************
# date2BucketIndex(timeline,eventDate)
#  takes (1) Timeline object, (2) eventDate in "yyyy-mm-dd" format 
#  returns the integer bucketIndex fpr the event; aggregate on this !
# not exported but used by all analysis steps 
# Buckets are indexed from 0.. bucketCount-1 ; 
# **********************
setGeneric(name = "date2BucketIndex",
           def = function(timeline, eventDate){
             standardGeneric("date2BucketIndex")
           })            
setMethod (f = "date2BucketIndex", signature = c("Timeline","character") ,
           definition = function(timeline,eventDate){
             return(sum(as.integer(as_date(eventDate) >= 
                                        timeline$bucketDateVector)) 
                   )
           }
)

             