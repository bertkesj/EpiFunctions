% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poi_back.R
\name{poi_back}
\alias{poi_back}
\title{Background Stratified Poisson Regression}
\usage{
poi_back(data, outcomeq, ss, lin = ~1, loglin = ~1, gcis = FALSE)
}
\arguments{
\item{data}{A tibble/data.frame like object containing the stratified person-time.
It is assumed the data contains a variable named \code{pdays} indicating the amount
of person-time in that strata (typically in days, but any unit of time can be used).}

\item{outcomeq}{a quosure naming the variable containing the outcome counts.}

\item{ss}{a quosure(s) containing the name of all variables to define the
background strata}

\item{lin}{A one sided function containing covariates in linear section of
hazard function}

\item{loglin}{A one sided function containing covariates in log-linear
section of hazard function}

\item{gcis}{A boolean (TRUE/FALSE) indicating if profile-likelihood CIs should
be calculated.}
}
\value{
A list containing:
\itemize{
\item output: A tibble containing paratemter estimates (est), standard errors (se),
lower/upper CIs, and gradients,
\item neg2LL_diff -2 * log-likelihood,
\item AIC: neg2LL_diff + 2 * number of parameters,
\item hessian: Hessian matrix o$hessian
}
}
\description{
Performs background stratified poisson regression on a tibble/data.frame containing
stratified person-time.
}
