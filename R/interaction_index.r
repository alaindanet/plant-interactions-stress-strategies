#' Intensity interaction indices  
#' 
#' @param open a vector containing non-negatives real values. 
#' @param patch a vector containing non-negatives real values. 
#'
#' @details Those two functions computes the intensity interaction indices developed by
#' Dìaz-Sierra et al. (2016). Those indices ensures symmetry, 
#' n_int_a provided the additive symmetry version of the intensity interaction
#' index
#' n_int_c provided the commutative symmetry version of the intensity interaction
#'
#' @return a vector containing the index values.
#'
#' @examples
#' 
#' 
#'
#' @export

n_int_c <- function(open, patch) {
    # Index works only with non-negative data  
    stopifnot(min(open, na.rm = TRUE) >= 0, min(patch, na.rm = TRUE) >= 0)
    p_sum <- open + patch
    delta_p <- patch - open
    res <- 2*delta_p/(p_sum + abs(delta_p))
    return(res)
}

n_int_a <- function(open, patch) {
    # Index works only with non-negative data  
    stopifnot(min(open, na.rm = TRUE) >= 0, min(patch, na.rm = TRUE) >= 0)
    p_sum <- open + patch
    delta_p <- patch - open
    res <- 2*delta_p/(open + abs(delta_p))
    return(res)
}
