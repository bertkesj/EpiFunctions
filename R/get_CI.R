#' Get Profile Likelihood CIs
#'
#' Gets the profile likelihood CI (either the upper or lower limit) for a parameter.
#'
#' @param .i integer. If even, gets the upper limit of the .i/2 parameter.
#' If odd, the lower limit of the .i/2 + .5 parameter.
#' @param o an optim object. The result of running optim().
#' @param ll the likelihood function.
#' @param se a vector of the standard errors. Can be all NAs if not calculated.
#' @param ... arguments passed to ll for optimization
#' @param alpha the degree of confidence (i.e. alpha = .95 returns 95% CIs)
#' @param verbose boolean (TRUE/FALSE). Should each step be presented.
#'
#' @return
#' @noRd
#'
get_CI <- function(.i, o, ll, se, ..., alpha=0.95, verbose=T) {
  #library(tidyverse)
  #library(numDeriv)

  #Define Functions for optimization
  #f3 is a wrapper around the log-likelihood (ll)
  error_fail <- function(e) return('Fail')

  f3 <- function(x, value, vi, ..., ll) {
    expn <- length(x) + 1
    b <- rep(NA, expn)
    b[-vi] <- x
    b[vi] <- value
    return(ll(b, ...))
  }

  #Define variables
  vi <- floor((.i - 1) / 2) + 1
  orig_ll <- o$value
  orig_est <- o$par[vi]
  goal <- stats::qchisq(alpha, df = 1)

  if (.i %% 2 == 0) sgn <- 1      #upper
  if (.i %% 2 == 1) sgn <- -1     #lower

  step <- stats::qnorm(1 - (1-alpha)/2) * se[vi] / 4
  if (is.na(step)) step <- abs(orig_est*.2)
  value <- orig_est + sgn*step
  lastpar <- o$par[-vi]

  ests <- c(orig_est)
  diffs <- c(0)
  for (i in 1:10) {
    ests <- c(ests, value)
    #browser()
    #For one parameter (therefore, no optim is run)
    if (length(o$par) == 1){

      ol <- tryCatch(expr = {ll(value,
                                ...)},
                     error = error_fail)
      if (is.nan(ol)) ol <- 'Fail'
      failed <- ol == 'Fail'
      if (!failed) diff <- -2*(ol - orig_ll) else diff <- NA

    } else { #For models with multiple parameters

      cont <- list(maxit = 10000, fnscale=-1)
      ol <- tryCatch(expr = {stats::optim(pmax(lastpar, 0), f3,
                                   value=value, vi=vi,
                                   ...,
                                   ll=ll,
                                   control = cont, method = "BFGS")},
                     error = error_fail)
      failed <- sum(ol == 'Fail') == 1
      if (!failed) {
        diff <- -2*(ol$value - orig_ll)
        lastpar <- ol$par
        } else diff <- NA
    }

    diffs <- c(diffs, diff)
    if (verbose) print(paste(i, value, diff))

    if (!is.na(diff) && diff > goal) break

    if (failed) {
      step <- step / 2
      value <- value - sgn*step

    } else {
      value <- value + sgn*step


    }
  }
  sp <- stats::spline(ests, diffs, n=10*length(ests))
  value <- stats::approx(sp$y, sp$x, xout=goal)$y

  return(value)
}
