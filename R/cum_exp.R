#' Calculate Cumulative Exposure
#'
#' Calculates cumulative exposure taking into account a beginning date
#'   end date and a cutoff date. This is useful when combining an exposure
#'   history file and a set of risk-sets.
#'
#' @param var numeric representing the daily exposure value.
#' @param begin_dt date representing the beginning date of an exposure period.
#' @param end_dt date representing the end date of an exposure period.
#' @param cut_dt date representing the beginning date of an exposure period.
#' @param lag numeric representing the lag, in years, to be applied when calculating cumulative exposure.
#'
#' @return
#' A numeric representing the cumulative, adjusting for a lag and cutoff date, for
#'   the exposure period.
#'
#' @export
#'
#' @examples
cum_exp <- function(var, begin_dt, end_dt, cut_dt, lag=0){
  ct <- cut_dt - floor(lag*365.25)
  cum <- ((pmin(end_dt, ct) - begin_dt + 1) * var) %>%
    as.numeric()
  cum <- if_else(ct >= begin_dt, cum, 0)
}
