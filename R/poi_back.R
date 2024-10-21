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
#'
#' library(dplyr)
#'
#' # Stratify person file.
#' # Automatically stratifies by race, sex, age and calendar period.
#' py_table <- example_person %>%
#'   mutate(dob = as.Date(dob),
#'          pybegin= as.Date(pybegin),
#'          dlo = as.Date(dlo),
#'
#'          sex = gender,
#'
#'          outcome = if_else(lung_cancer == 'TRUE',
#'                            1,
#'                            NA)) %>%
#'   get_table_rapid(break_yr = 5)
#'
#' # Background stratified poisson regression. Controlling for age, calendar period,
#' # and race. Output effect of sex being male.
#' poi_back(py_table,
#'          quo(`1`),
#'          loglin = ~ (sex=='M'),
#'          ss = vars(ageCat, CPCat, race))


poi_back <- function(data,
                    outcomeq,
                    ss,
                    lin = ~ 1,
                    loglin = ~ 1,
                    gcis=FALSE){
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
      dplyr::group_by(!!!ss) %>%
      dplyr::mutate(logphi = my_log(.data$phi)) %>%
      dplyr::summarize(one = sum(.data$observed*.data$logphi),
                two = sum(.data$observed),
                three = sum(.data$pdays*.data$phi),
                .groups = 'drop') %>%
      dplyr::transmute(ll = .data$one - .data$two*my_log(.data$three)) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(ll = sum(.data$ll))

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
      dt <- data %>%
        dplyr::group_by(!!!ss) %>%
        dplyr::mutate(logphi = my_log(.data$phi)) %>%
        dplyr::summarize(one = sum(.data$observed*.data$phi_p/.data$phi),

                  two = sum(.data$observed),
                  three = sum(.data$pdays*.data$phi_p),
                  four = sum(.data$pdays*.data$phi),
                  .groups = 'drop') %>%
        dplyr::transmute(ll = (.data$one - .data$two*.data$three/.data$four)) %>%
        dplyr::ungroup() %>%
        dplyr::summarize(x = sum(.data$ll))
      return(dt$x)
    }
    bbloglin <- purrr::map_dbl(asplit(Xloglin, 2),
                        ~ {
                          data$phi_p = as.numeric(.x) * data$phi
                          data %>%
                            fp_helper()
                        })
    bblin <- purrr::map_dbl(asplit(Xlin, 2),
                     ~ {
                       data$phi_p = as.numeric(.x) * exp(hloglin)
                       data %>%
                         fp_helper()
                     })
    return(c(bbloglin, bblin))
  }
  ##################################################################
  data <- data %>%
    dplyr::mutate(observed = dplyr::if_else(is.na(!!outcomeq), 0, !!outcomeq))

  Xloglin <- stats::model.matrix(loglin, data = data) %>%
    `[`(,-1, drop = F)
  Xlin <- stats::model.matrix(lin, data = data) %>%
    `[`(,-1, drop = F)

  tot <- ncol(Xlin) + ncol(Xloglin)
  b <- rep(0, tot)


  # Optimize
  cont <- list(maxit=10000,
               fnscale=-1)
  o <- stats::optim(b, loglik, fp,
             Xloglin=Xloglin, Xlin=Xlin, data=data, ss=ss,
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

    cis <- purrr::map(1:(2*length(o$par)),
               ~ get_CI(., o=o, se=se, ll=loglik,
                        Xloglin=Xloglin, Xlin=Xlin, data=data, alpha = alpha, ss=ss,
                        verbose = F)) %>%
      unlist()


    lower <- cis[2*(1:length(o$par)) - 1]
    upper <- cis[2*(1:length(o$par))]

  } else if (gcis & length(o$par) == 1){
    cis <-   purrr::map(1:(2*length(o$par)),
                 ~ get_CI(., o, se=se, ll=loglik,
                          Xloglin=Xloglin, Xlin=Xlin, data=data, alpha = alpha, ss=ss,
                          verbose = F))
    lower <- cis[[1]]
    upper <- cis[[2]]
  } else {
    #Confidence Interval Wald
    lower <- NA
    upper <- NA
  }


  out <- tibble::tibble(var = c(colnames(Xloglin), colnames(Xlin)),
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

