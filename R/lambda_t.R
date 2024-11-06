#' lambda_t function of equation 17
#'
#' @param k A integer number between "last day - num_test day + 1" and "last day + 1"
#' @param phi A double greater than 0
#'
#' @return scalar defined by equation 17
#' @export
#'
#' @examples
#'
#' last_day = 497
#' num_test_days = 14
#'
#' lambda_t(490, 0.7)
lambda_t <- function(k, phi){
  # k in (last_day - num_test_days:1)+1
  # phi > 0
  return(1 + k*((phi-1)/(30)))
}
