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
#' library(dplyr)
#' # Typical Usage is to add time-dependent cumulative exposures
#' # to an existing data.frame of risk-sets
#'
#' # Create Risk-Sets
#' risk_sets <- example_person %>%
#'   mutate(dob = as.Date(dob),
#'          pybegin= as.Date(pybegin),
#'          dlo = as.Date(dlo),
#'
#'          sex = gender,
#'
#'          case = (lung_cancer == 'TRUE')) %>%
#'   gt_rs()
#'
#' # Add time dependent cumulative exposure, 0 yr lag and 10 yr lag
#' risk_sets <- example_history %>%
#'   mutate(begin_dt = as.Date(begin_dt),
#'          end_dt= as.Date(end_dt),
#'
#'          daily_exposure = as.numeric(daily_exposure)) %>%
#'   right_join(risk_sets,
#'              by='id',
#'              relationship = 'many-to-many') %>%
#'   mutate(cumulative_exposure_lag0 = cum_exp(daily_exposure, begin_dt, end_dt, cut_dt, 0),
#'          cumulative_exposure_lag10 = cum_exp(daily_exposure, begin_dt, end_dt, cut_dt, 10)) %>%
#'   group_by(case_id, id, case, cut_dt) %>%
#'   summarize(cumulative_exposure_lag0 = sum(cumulative_exposure_lag0),
#'             cumulative_exposure_lag10 = sum(cumulative_exposure_lag10),
#'             .groups='drop')
#'
cum_exp <- function(var, begin_dt, end_dt, cut_dt, lag=0){
  ct <- cut_dt - floor(lag*365.25)
  cum <- ((pmin(end_dt, ct) - begin_dt + 1) * var) %>%
    as.numeric()
  cum <- dplyr::if_else(ct >= begin_dt, cum, 0)
}
