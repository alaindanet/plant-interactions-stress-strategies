#' Commutative Intensity Neighbor Index  
#' 
#' @param open a vector containing non-negatives real values. 
#' @param patch a vector containing non-negatives real values. 
#'
#' @usage patch contains the performance of individuals with a neighbor.
#' open contains the performance of individual without a neighbor.
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
#' n_int_c(open = 1, patch = 4)
#' n_int_c(open = 0.4, patch = 0.1)
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

#' Additive Intensity Neighbor Index  
#' 
#' @param open a vector containing non-negatives real values. 
#' @param patch a vector containing non-negatives real values. 
#'
#' @usage patch contains the performance of individuals with a neighbor.
#' open contains the performance of individual without a neighbor.
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
#' n_int_a(open = 1, patch = 4)
#' n_int_a(open = 0.4, patch = 0.1)
#'
#' @export
n_int_a <- function(open, patch) {
    # Index works only with non-negative data  
    stopifnot(min(open, na.rm = TRUE) >= 0, min(patch, na.rm = TRUE) >= 0)
    p_sum <- open + patch
    delta_p <- patch - open
    res <- 2*delta_p/(open + abs(delta_p))
    return(res)
}
