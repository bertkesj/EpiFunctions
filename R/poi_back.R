#' Background Stratified Poisson Regression
#'
#' Performs background stratified poisson regression on a tibble/data.frame containing
#' stratified person-time.
#'
#' @param data A tibble/data.frame like object containing the stratified person-time.
#' It is assumed the data contains a variable named `pdays` indicating the amount
#' of person-time in that strata (typically in days, but any unit of time can be used).
#' @param outcomeq a quosure naming the variable containing the outcome counts.
#' @param ss a quosure(s) containing the name of all variables to define the
#' background strata
#' @param lin A one sided function containing covariates in linear section of
#' hazard function
#' @param loglin A one sided function containing covariates in log-linear
#' section of hazard function
#' @param gcis A boolean (TRUE/FALSE) indicating if profile-likelihood CIs should
#' be calculated.
#'
#' @return
#' A list containing:
#' *  output: A tibble containing paratemter estimates (est), standard errors (se),
#' lower/upper CIs, and gradients,
#' *  neg2LL_diff -2 * log-likelihood,
#' *  AIC: neg2LL_diff + 2 * number of parameters,
#' *  hessian: Hessian matrix o$hessian
#' @export
#'
#' @examples
poi_back <- function(data,
                    outcomeq,
                    ss,
                    lin = ~ 1,
                    loglin = ~ 1,
                    gcis=TRUE){
  if (!'pdays' %in% colnames(data)) stop("Data missing 'pdays' column")

  #################################################################
  # Optimization Function
  my_log <- function(x){
    y <- log(abs(x))
    y[x < 0] <- NaN
    return(y)
  }
  loglik <- function(b, Xloglin, Xlin, data, ss){
    tot <- ncol(Xlin) + ncol(Xloglin)

    hloglin <- rep(0, nrow(data))
    hlin <- rep(0, nrow(data))
    if (ncol(Xloglin) != 0) hloglin <- Xloglin %*% b[1:ncol(Xloglin)]
    if (ncol(Xlin) != 0) hlin <-  Xlin %*% b[(ncol(Xloglin) + 1):tot]

    data$phi <- exp(hloglin)*(1 + hlin)

    n <- data %>%
      group_by(!!!ss) %>%
      mutate(logphi = my_log(phi)) %>%
      summarize(one = sum(observed*logphi),
                two = sum(observed),
                three = sum(pdays*phi),
                .groups = 'drop') %>%
      transmute(ll = one - two*my_log(three)) %>%
      ungroup() %>%
      summarize(ll = sum(ll))

    return(as.numeric(n[1,1]))
  }
  fp <- function(b, Xloglin, Xlin, data, ss){
    tot <- ncol(Xlin) + ncol(Xloglin)

    hloglin <- rep(0, nrow(data))
    hlin <- rep(0, nrow(data))
    if (ncol(Xloglin) != 0) hloglin <- Xloglin %*% b[1:ncol(Xloglin)]
    if (ncol(Xlin) != 0) hlin <-  Xlin %*% b[(ncol(Xloglin) + 1):tot]

    data$phi <- exp(hloglin)*(1 + hlin)

    fp_helper <- function(data) {
      data %>%
        group_by(!!!ss) %>%
        mutate(logphi = my_log(phi)) %>%
        summarize(one = sum(observed*phi_p/phi),

                  two = sum(observed),
                  three = sum(pdays*phi_p),
                  four = sum(pdays*phi),
                  .groups = 'drop') %>%
        transmute(ll = (one - two*three/four)) %>%
        ungroup() %>%
        dplyr::summarize(x = sum(ll)) %>%
        `$`(x)
    }
    bbloglin <- map_dbl(asplit(Xloglin, 2),
                        ~ {
                          data$phi_p = as.numeric(.x) * data$phi
                          data %>%
                            fp_helper()
                        })
    bblin <- map_dbl(asplit(Xlin, 2),
                     ~ {
                       data$phi_p = as.numeric(.x) * exp(hloglin)
                       data %>%
                         fp_helper()
                     })
    return(c(bbloglin, bblin))
  }
  ##################################################################
  data <- data %>%
    mutate(observed = if_else(is.na(!!outcomeq), 0, !!outcomeq))

  Xloglin <- model.matrix(loglin, data = data) %>%
    `[`(,-1, drop = F)
  Xlin <- model.matrix(lin, data = data) %>%
    `[`(,-1, drop = F)

  tot <- ncol(Xlin) + ncol(Xloglin)
  b <- rep(0, tot)


  # Optimize
  cont <- list(maxit=10000,
               fnscale=-1)
  o <- optim(b, loglik, fp,
             Xloglin=Xloglin, Xlin=Xlin,
             data=data, ss=ss,
             control = cont, method="BFGS", hessian=T)#drop method and use default???????
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
                        ll=loglik, data=data, ss=ss,
                        verbose = F)) %>%
      unlist()


    lower <- cis[2*(1:length(o$par)) - 1]
    upper <- cis[2*(1:length(o$par))]

  } else if (gcis & length(o$par) == 1){
    cis <-   map(1:(2*length(o$par)),
                 ~ get_CI(., o, se=se,
                          Xloglin=Xloglin, Xlin=Xlin,
                          ll=loglik, data=data, ss=ss,
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
              neg2LL = -2*o$value,
              AIC = -2*o$value + 2*tot,
              hessian = o$hessian))
}

