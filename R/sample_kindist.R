#' Subsample and filter a KinPairSimulation Object
#' @description This function takes a pre-existing KinPairSimulation Object with distance and coordinate data and filters it to simulate various in-field sampling schemes.
#' @param kindist KinPairSimulation - KinPairSimulation Class Object
#' @param upper   numeric - upper cutoff for kin pair distances
#' @param lower   numeric - lower cutoff for kin pair distances
#' @param spacing numeric - spacing between traps (assume 1D layout)
#' @param n       numeric - number of individuals to keep after filtering (if possible)
#' @param dims    dimensions of square to sample within (works with the KinPairSimulation spatial & dimension information). Typically use either dims or upper.
#'
#' @return  returns an object of class 'KinPairSimulation' containing simulation and filtering details and a tibble (tab) of filtered simulation values
#' @export
#'
#' @examples
#' simobject <- simulate_kindist_simple(nsims = 100000, sigma = 100, kinship = "PO")
#'
#' sample_kindist(simobject, upper = 200, lower = 50, spacing = 15, n = 100)
sample_kindist <- function(kindist, upper = NULL, lower = NULL, spacing = NULL, n = NULL, dims = NULL){

  if (! is.KinPairData(kindist)) stop("Object is not of class KinPairSimulation or KinPairData!")

  if (! is.null(dims)) {
    if (! is.KinPairSimulation(kindist)) cat("Ignoring 'dims' as object is not of class KinPairSimulation\n")
    else{

    # check if dimensions smaller than original dimensions!
    simdims <- kindist@simdims
    if (dims > simdims) {
      cat("Ignoring 'dims' as they are greater than original simulation dimensions\n")
    }
    else {

      cat(paste0("Setting central sampling area to ", dims, " by ", dims, "\n"))

      # otherwise, identify the sampling rectangle

      displacement <- (simdims - dims) / 2

      xmin <- 0 + displacement; ymin <- 0 + displacement
      xmax <- simdims - displacement; ymax <- simdims - displacement

      # now, filter final coordinates that fall out of the new sampling rectangle

      kindist@tab <- filter(kindist@tab, .data$x1 > xmin, .data$x1 < xmax, .data$x2 > xmin, .data$x2 < xmax,
                            .data$y1 > ymin, .data$y1 < ymax, .data$y2 > ymin, .data$y2 < ymax)
    }
  }}

  if (! is.null(upper)) {
    cat(paste0("Removing distances farther than ", upper, "\n"))
    kindist@tab <- filter(kindist@tab, .data$distance < upper)
  }

  if (! is.null(lower)) {
    cat(paste0("Removing distances closer than ", lower, "\n"))
    kindist@tab <- filter(kindist@tab, .data$distance > lower)
  }

  if (! is.null(spacing)){
    cat(paste0("Setting trap spacing to ", spacing, "\n"))

    binshift <- spacing / 2

    kindist@tab <- mutate(kindist@tab, distance = ((.data$distance %/% spacing) * spacing) + binshift) # changed equation here... (need to rerun estimates)

  }

  if (! is.null(n)){

    if (! n < nrow(kindist@tab)) {
      cat(paste0("Less than n = ", n, " kin pairs remaining (", nrow(kindist@tab), ") - skipping downsampling\n"))
    }
    else {
      cat(paste0("Down-sampling to ", n, " kin pairs\n"))
      kindist@tab <- slice_sample(kindist@tab, n = n)
    }
  }
  cat(paste0(nrow(kindist@tab), " kin pairs remaining.\n"))
  cat("\n")
  if (is.KinPairSimulation(kindist)){
    kindist@filtertype <- "filtered"
    if (! is.null(upper)) {kindist@upper <- upper}
    if (! is.null(lower)) {kindist@lower <- lower}
    if (! is.null(spacing)) {kindist@spacing <- spacing}
    if (! is.null(n)) {kindist@samplenum <- n}
    if (! is.null(dims)) {kindist@sampledims <- dims}
  }
  return(kindist)
}

# additional comment
