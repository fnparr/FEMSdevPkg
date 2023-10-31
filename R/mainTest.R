# ******** YieldCurve tests 
# will need to clear the environment 
rm(list=ls())
# will need to source files we refer to 
source("R/YieldCurve.R")
ycID <- "yc001"
rd <- "2023-10-31"
tr <-  c(1.1, 2.0, 3.5 )
names(tr) <- c("1M", "1Y", "5Y")
dcc <- "30E360"
cf <- "NONE"
yc <- YieldCurve(ycID,rd,tr,dcc,cf)
