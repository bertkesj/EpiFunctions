#' Example Person File for Testing
#'
#' A tibble containing example person file data to be used for testing and
#' demonstration of the package
#'
#' @format A tibble with 1000 observations and 7 variables:
#' \describe{
#'   \item{id}{unique identifier; character}
#'   \item{race}{Race; character 'W' or 'N'}
#'   \item{gender}{Gender/Sex; character 'M' or 'F'}
#'   \item{dob}{Date of Birth; character to be converted to date}
#'   \item{pybegin}{date to begin follow-up/at-risk accumulation, character to be converted to date}
#'   \item{dlo}{Date last observed; character to be converted to date}
#'   \item{lung_cancer}{indicator identifying case status for hypothical outcome of lung cancer}
#'   ...
#' }
#' @source {Internally Generated}
"example_person"
