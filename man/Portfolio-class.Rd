% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Portfolio.R
\docType{class}
\name{Portfolio-class}
\alias{Portfolio-class}
\title{class Portfolio}
\description{
A Portfolio consists of a list of contracts and a list of riskFactors
defining a scenario, A portfolio argument can be used as the input
parameter for a generateEvents( ) request
}
\section{Fields}{

\describe{
\item{\code{contracts}}{List of contracts, class=ContractType, in the portfolio.}

\item{\code{riskFactors}}{List of class=ReferenceIndex's defining a risk Scenario.
possibly better to change to riskFactorIndexList}
}}


