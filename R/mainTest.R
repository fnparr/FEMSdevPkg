# ******** YieldCurve tests 
# will need to clear the environment 
rm(list=ls())
#  need to source files we refer to - testing YieldCurve.R functions
source("R/yearFraction.R")
source("R/YieldCurve.R")
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "NONE"
yc <- YieldCurve(ycID,rd,tr,dcc,cf)
# valid case 
dcc_e <- "30/360"
yc_e <- YieldCurve(ycID,rd,tr,dcc_e,cf)
# should generate error message for invalid dcc
# could also check for cf == "NONE" 
Tfrom <- "2024-01-01"
Tto <- "2024-07-01"
yfdcc <- "30e/360"
frac <- yearFraction(Tfrom, Tto, yfdcc)
