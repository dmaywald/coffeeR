#' omega_t function of equation 16
#'
#' @param k A integer number between "last day - num_test day + 1" and "last day + 1"
#' @param omega A double greater than or equal to 1
#'
#' @return scalar defined by equation 16
#' @export
#'
#' @examples
#'
#' last_day = 497
#' num_test_days = 14
#'
#' omega_t(490, 1.2)
omega_t <- function(k, omega){
  # k in (last_day - num_test_days:1)+1
  # omeaga >= 1

  return(pmax(0,(1 - ((k-1)/(omega))**2)))

}
