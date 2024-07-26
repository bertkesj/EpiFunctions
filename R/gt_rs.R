#' Create Risk Sets
#'
#' `gt_rs` creates risk-sets from a person data.frame/tibble with age as the time-scale.
#' It automatically matches on race, sex and date of birth (within 5 years of
#' the index case).
#'
#' The person tibble must contain the variables:
#' * id,
#' * sex,
#' * race,
#' * dob (date),
#' * pybegin (date),
#' * dlo	(date),
#' * case (boolean: TRUE/FALSE)
#'
#' @param person A data.frame like object containing one row per person with the
#' required demographic information.
#'
#' @return A tibble containing the risk sets for each case using age as the time-scale.
#' @export
#'
#' @examples
gt_rs <- function(person){
  # Expects: id, case, race, sex, dob, pybegin, dlo!!!!!
  # Calculate age risk begin and end for all people
  person <- person %>%
    mutate(age_risk_begin =
             difftime(pybegin,
                      dob,
                      units='days') %>%
             as.numeric() %>%
             `/`(365.25),
           age_risk_begin =
             difftime(dlo,
                      dob,
                      units='days') %>%
             as.numeric() %>%
             `+`(1) %>%
             `/`(365.25))

  # Create Risksets
  risk_sets <- person %>%
    dplyr::filter(case) %>%
    select(case_id = id, case_age = age_risk_end,
           sex, race, dob) %>%
    left_join(person,
              by=c('sex', 'race'),
              relationship = 'many-to-many') %>%            #Match on sex, race
    dplyr::filter(case_age >= age_risk_begin,               #Match age
                  case_age <= age_risk_end,
                  abs((dob.x - dob.y) / 365.25) <= 5) %>%   #Match DOB
    mutate(case = (case_id == id),
           cut_dt = dob.y + case_age*365.25)  %>%
    select(case_id, id, case, cut_dt) %>%
    ungroup() %>%
    return()
}
