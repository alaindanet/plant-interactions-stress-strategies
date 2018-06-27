#' Find the closest open/patch site pairs  
#' 
#' @param data a data.frame
#'
#' @details This function uses the Hungarian algorithm to find the closest
#' open/patch hole pair while minimizing the sum of the distance between the
#' pairs [please follow this link to have more
#' details](https://stackoverflow.com/questions/13961493/finding-the-best-matching-pairwise-points-from-2-vectors).
#'
#' @seealso solve_LSAP
#'
#' @return 
#'
#' @examples
#'
#' @export

pair_find <- function(x){
  require(fields)
  require(clue)

  # List of open holes
  open_sites <- unlist(filter(x, ms == "0")[, "label"])
  names(open_sites) <- open_sites

  # List of patch holes
  patch_sites <- unlist(filter(x, ms == "1")[, "label"])
  names(patch_sites) <- patch_sites

  # Compute the distance between open and patch algorithm
  distances <- rdist(open_sites, patch_sites)
  dimnames(distances) <- list( names(open_sites), names(patch_sites))

  # Hungarian algorithm works only with square matrix
  n_open_sites <- length(open_sites)
  names(n_open_sites)  <- "open_sites"
  n_patch_sites <- length(patch_sites)
  names(n_patch_sites)  <- "patch_sites"

  #https://s-mat-pcs.oulu.fi/~mpa/matreng/ematr1_2.htm
  if((n_open_sites != n_patch_sites)){
    deficit <- abs(n_open_sites - n_patch_sites)
    lower <- c(n_open_sites, n_patch_sites)
    which_lower <- names(lower)[which.min(lower)]

    # Dummy rows/columns
    add <- matrix(0, 
      nrow = ifelse(which_lower == "open_sites", deficit, nrow(distances)),
      ncol = ifelse(which_lower == "patch_sites", deficit, ncol(distances))
      )

    # Add the dummies
    if (which_lower == "open_sites") {
      distances  <- rbind(distances, add)
      # The vector of the same size to not recycle the vector:
      length(open_sites) <- length(patch_sites)
    } else {
      distances  <- cbind(distances, add)
    }
  }

  #Hungarian algorithm, return columns assigns to row 
  columns_order <- unlist(solve_LSAP(distances))
  temp <- as_tibble(
    cbind("Open" = open_sites, "Patch" = patch_sites[columns_order])
    )
  return(temp)
}

