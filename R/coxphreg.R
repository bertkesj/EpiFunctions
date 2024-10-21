#' Cox Proportional Regression
#'
#' Performs Cox proportional regression on a tibble of risk-sets
#'   that were outputted from `get_rs()` function. Both a linear piece and
#'   log-linear piece of the hazard function can be specified. The hazard function
#'   has the general form: h(t) = h0(t) * exp(loglin) * (1 + lin).
#'
#' @param data A tibble/data.frame like object containing risk-sets. Variables
#' `case_id` and `case` are required to be contained in the data.frame.
#' @param lin A one sided function containing covariates in linear section of
#' hazard function
#' @param loglin A one sided function containing covariates in log-linear
#' section of hazard function
#' @param gcis A boolean (TRUE/FALSE) indicating if profile-likelihood CIs should
#' be calculated.
#' @param alpha A numeric between 0 and 1, indicating the level of significance
#' of the profile likelihood confidence intervals ouptputed when gcis = TRUE.
#'
#' @return
#' A list containing:
#' *  output:      A tibble containing paratemter estimates (est), standard errors (se), and lower/upper CIs,
#' *  neg2LL_diff: -2 * log-likelihood,
#' *  AIC:         neg2LL_diff + 2 * number of parameters,
#' *  hessian:     Hessian matrix
#'
#' @export
#' @importFrom rlang .data
#' @examples
#'
#' library(dplyr)
#'
#' # Typical usage is to create a risk-set, add a time-dependent exposure,
#' # and then perform Cox regression.
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
#'   dplyr::summarize(cumulative_exposure_lag0 = sum(cumulative_exposure_lag0),
#'                    cumulative_exposure_lag10 = sum(cumulative_exposure_lag10),
#'                    .groups='drop')
#'
#' # Perform Cox-regression
#' coxphreg(risk_sets,
#'          loglin = ~ scale(cumulative_exposure_lag10, scale = 100000))


coxphreg <- function(data, lin = ~ 1, loglin = ~ 1, gcis = F, alpha = 0.95){

  ll <- function(b, data, Xloglin, Xlin) {

    hloglin <- rep(0, nrow(data))
    hlin <- rep(0, nrow(data))
    if (ncol(Xloglin) != 0) hloglin <- Xloglin %*% b[1:ncol(Xloglin)]
    if (ncol(Xlin) != 0) hlin <-  Xlin %*% b[(ncol(Xloglin) + 1):tot]
    data$h = exp(hloglin)*(1 + hlin)

    ll <- data %>%
      dplyr::group_by(.data$case_id) %>%
      dplyr::summarize(den = sum(.data$h),
                       num = sum(.data$case*.data$h)) %>%
      dplyr::mutate(L = .data$num / .data$den) %>%
      dplyr::summarize(x = sum(log(.data$L)))

    return(ll$x)
  }
  g <- function(b, data, Xloglin, Xlin) {
    hloglin <- rep(1, nrow(data))
    hlin <- rep(1, nrow(data))
    if (ncol(Xloglin) != 0) hloglin <- exp(as.numeric(Xloglin %*% b[1:ncol(Xloglin)]))
    if (ncol(Xlin) != 0) hlin <-  (1 + as.numeric(Xlin %*% b[(ncol(Xloglin) + 1):tot]))

    data$h = hloglin*hlin

    bbloglin <- purrr::map_dbl(asplit(Xloglin, 2),
                        ~ {
                          data$x <- as.numeric(.x)
                          data %>%
                            dplyr::group_by(case_id) %>%
                            dplyr::summarize(den = sum(h),
                                             num = sum(h*x),
                                             cx = sum(case*x),
                                             .groups = 'drop') %>%
                            dplyr::mutate(L = cx - num/den) %>%
                            dplyr::summarize(x = sum(L)) %>%
                            `$`(x)
                        })
    bblin <- purrr::map_dbl(asplit(Xlin, 2),
                     ~ {
                       data$x <- as.numeric(.x)
                       data %>%
                         dplyr::group_by(case_id) %>%
                         dplyr::summarize(den = sum(hloglin*hlin),
                                          num = sum(hloglin*x),
                                          cxn = sum(case*x),
                                          cxd = sum(case*hlin),
                                          .groups = 'drop') %>%
                         dplyr::mutate(L = cxn/cxd - num/den) %>%
                         dplyr::summarize(x = sum(L)) %>%
                         `$`(x)
                     })
    return(c(bbloglin, bblin))
  }

  Xloglin <- stats::model.matrix(loglin, data = data) %>%
    `[`(,-1, drop = F)
  Xlin <- stats::model.matrix(lin, data = data) %>%
    `[`(,-1, drop = F)


  tot <- ncol(Xlin) + ncol(Xloglin)
  b <- rep(0, tot)

  o <- stats::optim(b, ll, gr=g,
             data=data, Xloglin=Xloglin, Xlin=Xlin,
             hessian = TRUE, method = "BFGS",
             control=list(fnscale=-1))#
  se <- sqrt(diag(solve(-o$hessian)))

  #Confidence Interval Profile
  cis <- rep(NA, 2*length(o$par))
  if (gcis & length(o$par) > 1){
    # library(parallel)
    # library(foreach)
    # library(doParallel)
    # numCores <- min(6, detectCores())
    # cl <- makeCluster(numCores)
    # registerDoParallel(cl)
    # cis <- foreach(i=1:(2*length(o$par)), .export = c('ll', 'f3', 'error_fail',
    #                                                   'get_CI')) %dopar% get_CI(i, o=o, df=df, se=se, Xloglin=Xloglin, Xlin=Xlin, ll=ll, verbose = F, alpha = alpha)
    #
    # stopCluster(cl)
    #

    cis <- purrr::map(1:(2*length(o$par)),
               ~ get_CI(., o=o, se=se,
                        Xloglin=Xloglin, Xlin=Xlin, data=data, alpha = alpha,
                        ll=ll,
                        verbose = F)) %>%
      unlist()


    lower <- cis[2*(1:length(o$par)) - 1]
    upper <- cis[2*(1:length(o$par))]

  } else if (gcis & length(o$par) == 1){
    cis <-   purrr::map(1:(2*length(o$par)),
                 ~ get_CI(., o, se=se,
                          Xloglin=Xloglin, Xlin=Xlin, data=data, alpha = alpha,
                          ll=ll,
                          verbose = F))
    lower <- cis[[1]]
    upper <- cis[[2]]
  } else {
    #Confidence Interval Wald
    lower <- NA
    upper <- NA
  }

  out <- dplyr::tibble(var = c(colnames(Xloglin), colnames(Xlin)),
                est = o$par,
                se = se,
                lower = lower,
                upper = upper,
                gradient = g(o$par, Xloglin=Xloglin, Xlin=Xlin, data=data))
  return(list(output=out,
              neg2LL = - 2*o$value,
              AIC = - 2*o$value + 2*tot,
              hessian = o$hessian))
}
