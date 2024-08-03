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
#' be calcualted.
#'
#' @return
#' A list containing:
#' *  output: A tibble containing paratemter estimates (est), standard errors (se), and lower/upper CIs,
#' *  neg2LL_diff -2 * log-likelihood,
#' *  AIC: neg2LL_diff + 2 * number of parameters,
#' *  hessian: Hessian matrix o$hessian
#'
#' @export
#'
#' @examples
coxphreg <- function(data, lin = ~ 1, loglin = ~ 1, gcis = F){
  ll <- function(b, data, Xloglin, Xlin, ss=quos()) {
    hloglin <- rep(0, nrow(data))
    hlin <- rep(0, nrow(data))
    if (ncol(Xloglin) != 0) hloglin <- Xloglin %*% b[1:ncol(Xloglin)]
    if (ncol(Xlin) != 0) hlin <-  Xlin %*% b[(ncol(Xloglin) + 1):tot]
    data$h <- exp(hloglin)*(1 + hlin)

    ll <- data %>%
      group_by(case_id) %>%
      dplyr::summarize(den = sum(h),
                       num = sum(case*h)) %>%
      mutate(L = num / den) %>%
      dplyr::summarize(x = sum(log(L)))

    return(ll$x)
  }
  g <- function(b, data, Xloglin, Xlin, ss=quos()) {
    if (ncol(Xloglin) != 0) hloglin <- exp(as.numeric(Xloglin %*% b[1:ncol(Xloglin)])) else hloglin <- rep(1, nrow(data))
    if (ncol(Xlin) != 0) hlin <-  (1 + as.numeric(Xlin %*% b[(ncol(Xloglin) + 1):tot])) else hlin <- rep(1, nrow(data))
    data$hloglin <- hloglin
    data$hlin <- hlin

    bbloglin <- map_dbl(asplit(Xloglin, 2),
                        ~ {
                          data$x <- as.numeric(.x)
                          data %>%
                            group_by(case_id) %>%
                            mutate(h = hloglin*hlin) %>%
                            dplyr::summarize(den = sum(h),
                                             num = sum(h*x),
                                             cx = sum(case*x),
                                             .groups = 'drop') %>%
                            mutate(L = cx - num/den) %>%
                            dplyr::summarize(x = sum(L)) %>%
                            `$`(x)
                        })
    bblin <- map_dbl(asplit(Xlin, 2),
                     ~ {
                       data$x <- as.numeric(.x)
                       data %>%
                         group_by(case_id) %>%
                         dplyr::summarize(den = sum(hloglin*hlin),
                                          num = sum(hloglin*x),
                                          cxn = sum(case*x),
                                          cxd = sum(case*hlin),
                                          .groups = 'drop') %>%
                         mutate(L = cxn/cxd - num/den) %>%
                         dplyr::summarize(x = sum(L)) %>%
                         `$`(x)
                     })
    return(c(bbloglin, bblin))
  }

  Xloglin <- model.matrix(loglin, data = data) %>%
    `[`(,-1, drop = F)
  Xlin <- model.matrix(lin, data = data) %>%
    `[`(,-1, drop = F)


  tot <- ncol(Xlin) + ncol(Xloglin)
  b <- rep(0, tot)

  o <- optim(b, ll, gr=g,
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
    #                                                   'get_CI')) %dopar% get_CI(i, o=o, df=df, se=se, Xloglin=Xloglin, Xlin=Xlin, ll=ll, verbose = F)
    #
    # stopCluster(cl)
    #

    cis <- map(1:(2*length(o$par)),
               ~ get_CI(., o=o, se=se,
                        Xloglin=Xloglin, Xlin=Xlin,
                        ll=ll, data=data,
                        verbose = F)) %>%
      unlist()


    lower <- cis[2*(1:length(o$par)) - 1]
    upper <- cis[2*(1:length(o$par))]

  } else if (gcis & length(o$par) == 1){
    cis <-   map(1:(2*length(o$par)),
                 ~ get_CI(., o, se=se,
                          Xloglin=Xloglin, Xlin=Xlin,
                          ll=ll, data=data,
                          verbose = F))
    lower <- cis[[1]]
    upper <- cis[[2]]
  } else {
    #Confidence Interval Wald
    lower <- NA
    upper <- NA
  }

  out <- tibble(var = c(colnames(Xloglin), colnames(Xlin)),
                est = o$par,
                se = se,
                lower = lower,
                upper = upper,
                gradient = fp(o$par, Xloglin, Xlin, data, ss))
  return(list(output=out,
              neg2LL = - 2*o$value,
              AIC = - 2*o$value + 2*tot,
              hessian = o$hessian))
}
