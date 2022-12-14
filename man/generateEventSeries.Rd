% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EventSeries.R
\name{generateEventSeries}
\alias{generateEventSeries}
\title{generateEventSeries      Generic method definition}
\usage{
generateEventSeries(contract, riskFactors, serverURL)
}
\arguments{
\item{contract}{the contract to simulate cashflows for}

\item{riskFactors}{list of riskFactors - scenario for contract simulation}

\item{serverURL}{locate the ACTUS server performing the cashflow analysis}
}
\value{
an EventSeries with cashflow events for the contract
}
\description{
Defines a generic method on S4 Class Eventseries. The instance is
generateEventSeries < contract riskFactors serverURL >
}
