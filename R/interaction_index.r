#' Commutative Intensity Neighbor Index  
#' 
#' @param without a vector containing non-negatives real values. 
#' @param with_nbs a vector containing non-negatives real values. 
#'
#' @usage with_nbs contains the performance of individuals with a neighbor.
#' without contains the performance of individual without a neighbor.
#'
#' @details This function computes the Commutative Intensity Neighbor Index developed by
#' Dìaz-Sierra et al. (2016). Positive values indicates a positive net effect of
#' the neighbor (facilitation) whereas negative values indicates a negative one
#' (competition). 
#'
#' @return a vector containing the index values.
#'
#' @seealso n_int_a
#'
#' @examples
#' n_int_c(without = 1, with_nbs = 4)
#' n_int_c(without = 0.4, with_nbs = 0.1)
#' 
#'
#' @export

n_int_c <- function(without, with_nbs) {
  check_int_arg(without, with_nbs)

  p_sum <- without + with_nbs
  delta_p <- with_nbs - without
  res <- 2*delta_p/(p_sum + abs(delta_p))
  return(res)
}

#' Additive Intensity Neighbor Index  
#' 
#' @param without a vector containing non-negatives real values. 
#' @param with_nbs a vector containing non-negatives real values. 
#'
#' @usage with_nbs contains the performance of individuals with a neighbor.
#' without contains the performance of individual without a neighbor.
#'
#' @details This function computes the Additive Intensity Neighbor Index developed by
#' Dìaz-Sierra et al. (2016). Positive values indicates a positive net effect of
#' the neighbor (facilitation) whereas negative values indicates a negative one
#' (competition). 
#'
#' @return a vector containing the index values.
#'
#' @seealso n_int_c
#'
#' @examples
#' n_int_a(without = 1, with_nbs = 4)
#' n_int_a(without = 0.4, with_nbs = 0.1)
#'
#' @export
n_int_a <- function(without, with_nbs) {
  check_int_arg(without, with_nbs)

  delta_p <- with_nbs - without
  res <- 2 * delta_p / (without + abs(delta_p))
  return(res)
}

#' Check interaction index data
#' 
#' Check if the arguments are in the good format to compute the index. Returns
#' NA if with_nbs or/and without is NA
#'
#' @param without data from the without
#' @param with_nbs data from the without
#'
#' @export

check_int_arg <- function(without, with_nbs) {
  #Drop if NA

  if(any(is.na(without), is.na(with_nbs))) {
   return(NA)
  }

  # Index works only with non-negative data
  stopifnot(min(without, na.rm = TRUE) >= 0, min(with_nbs, na.rm = TRUE) >= 0)
}

