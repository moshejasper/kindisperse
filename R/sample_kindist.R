#' Subsample and filter a KinPairSimulation Object
#'
#' @param kindist KinPairSimulation - KinPairSimulation Class Object
#' @param upper   numeric - upper cutoff for kin pair distances
#' @param lower   numeric - lower cutoff for kin pair distances
#' @param spacing numeric - spacing between traps (assume 1D layout)
#' @param n       numeric - number of individuals to keep after filtering (if possible)
#' @param dims    dimensions square to sample within (works with the KinPairSimulation spatial & dimension information)
#'
#' @return  returns an object of class 'KinPairSimulation' containing simulation and filtering details and a tibble (tab) of filtered simulation values
#' @export
#'
#' @examples
#' simobject <- simulate_kindist_simple(nsims = 100000, sigma = 100, category = "PO")
#'
#' sample_kindist(simobject, upper = 200, lower = 50, spacing = 15, n = 100)
sample_kindist <- function(kindist, upper = NULL, lower = NULL, spacing = NULL, n = NULL, dims = NULL){

  if (! is.null(dims)) {

    # check if dimensions smaller than original dimensions!
    simdims <- kindist@dims
    if (dims > simdims) {
      cat("Ignoring 'dims' as they are greater than original simulation dimensions")
    }
    else {

      cat(paste0("Setting central sampling area to ", dims, " by ", dims, "\n"))

      # otherwise, identify the sampling rectangle

      displacement <- (simdims - dims) / 2

      xmin <- 0 + displacement; ymin <- 0 + displacement
      xmax <- simdims - displacement; ymax <- simdims - displacement

      # now, filter final coordinates that fall out of the new sampling rectangle

      kindist@tab <- filter(kindist@tab, x1 > xmin, x1 < xmax, x2 > xmin, x2 < xmax,
                                            y1 > ymin, y1 < ymax, y2 > ymin, y2 < ymax)
    }
  }

  if (! is.null(upper)) {
    cat(paste0("Removing distances farther than ", upper, "\n"))
    kindist@tab <- dplyr::filter(kindist@tab, distance < upper)
  }

  if (! is.null(lower)) {
    cat(paste0("Removing distances closer than ", lower, "\n"))
    kindist@tab <- dplyr::filter(kindist@tab, distance > lower)
  }

  if (! is.null(spacing)){
    cat(paste0("Setting trap spacing to ", spacing, "\n"))

    binshift <- spacing / 2

    kindist@tab <- dplyr::mutate(kindist@tab, distance = ((distance %/% spacing) * spacing) + binshift) # changed equation here... (need to rerun estimates)

  }

  if (! is.null(n)){

    if (! n < nrow(kindist@tab)) {
      cat(paste0("Less than n = ", n, " kin pairs remaining (", nrow(kindist@tab), ") - skipping downsampling\n"))
    }
    else {
      cat(paste0("Down-sampling to ", n, " kin pairs\n"))
      kindist@tab <- dplyr::slice_sample(kindist@tab, n = n)
    }
  }
  cat(paste0(nrow(kindist@tab), " kin pairs remaining.\n"))
  cat("\n")
  kindist@filtertype <- "filtered"
  if (! is.null(upper)) {kindist@upper <- upper}
  if (! is.null(lower)) {kindist@lower <- lower}
  if (! is.null(spacing)) {kindist@spacing <- spacing}
  if (! is.null(n)) {kindist@samplenum <- n}
  if (! is.null(dims)) {kindist@sampledims <- dims}

  return(kindist)
}

# additional comment
