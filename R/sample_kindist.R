#' Subsample and filter a KinPairSimulation Object
#' @description This function takes a pre-existing KinPairSimulation Object with distance and coordinate data and filters it to simulate various in-field sampling schemes.
#' @param kindist KinPairSimulation - KinPairSimulation Class Object
#' @param upper   numeric - upper cutoff for kin pair distances
#' @param lower   numeric - lower cutoff for kin pair distances
#' @param spacing numeric - spacing between traps (assume 1D layout)
#' @param n       numeric - number of individuals to keep after filtering (if possible)
#' @param dims    dimensions to sample within (works with the KinPairSimulation spatial & dimension information). Either 'num' (square) or 'c(num1, num2)' (rectangle).
#'
#' @return  returns an object of class 'KinPairSimulation' containing simulation and filtering details and a tibble (tab) of filtered simulation values
#' @export
#'
#' @examples
#' simobject <- simulate_kindist_simple(nsims = 100000, sigma = 100, kinship = "PO")
#'
#' sample_kindist(simobject, upper = 200, lower = 50, spacing = 15, n = 100)
sample_kindist <- function(kindist, upper = NULL, lower = NULL, spacing = NULL, n = NULL, dims = NULL) {
  if (!is.KinPairData(kindist)) stop("Object is not of class KinPairSimulation or KinPairData!")

  if (!is.null(dims)) {
    if (!is.KinPairSimulation(kindist)) {
      cat("Ignoring 'dims' as object is not of class KinPairSimulation\n")
    } else {

      # check if dimensions smaller than original dimensions!
      simdims <- kindist@simdims
      if (length(dims) > 2){ #(dims > simdims) {
        cat("Ignoring 'dims' as vector length is greater than two\n")
      }
      else {

        # first, rebase kindist...
        if (length(dims) == 1){
          dims <- c(dims, dims)
        }

        cat(paste0("Setting central sampling area to ", dims[1], " by ", dims[2], "\n"))

        kindist <- rebase_dims(kindist, dims)

        xdim <- dims[1]
        ydim <- dims[2]
        nsims <- nrow(kindist@tab)


        # now, filter final coordinates that fall out of the new sampling rectangle

        kindist@tab <- filter(
          kindist@tab, .data$x1 >= 0, .data$x1 <= xdim, .data$x2 >= 0, .data$x2 <= xdim,
          .data$y1 >= 0, .data$y1 <= ydim, .data$y2 >= 0, .data$y2 <= ydim
        )
      }
    }
  }

  if (!is.null(upper)) {
    cat(paste0("Removing distances farther than ", upper, "\n"))
    kindist@tab <- filter(kindist@tab, .data$distance < upper)
  }

  if (!is.null(lower)) {
    cat(paste0("Removing distances closer than ", lower, "\n"))
    kindist@tab <- filter(kindist@tab, .data$distance > lower)
  }

  if (!is.null(spacing)) {
    cat(paste0("Setting trap spacing to ", spacing, "\n"))

    binshift <- spacing / 2

    kindist@tab <- mutate(kindist@tab, distance = ((.data$distance %/% spacing) * spacing) + binshift) # changed equation here... (need to rerun estimates)
  }

  if (!is.null(n)) {
    if (!n < nrow(kindist@tab)) {
      cat(paste0("Less than n = ", n, " kin pairs remaining (", nrow(kindist@tab), ") - skipping downsampling\n"))
    }
    else {
      cat(paste0("Down-sampling to ", n, " kin pairs\n"))
      kindist@tab <- slice_sample(kindist@tab, n = n)
    }
  }
  cat(paste0(nrow(kindist@tab), " kin pairs remaining.\n"))
  cat("\n")
  if (is.KinPairSimulation(kindist)) {
    kindist@filtertype <- "filtered"
    if (!is.null(upper)) {
      kindist@upper <- upper
    }
    if (!is.null(lower)) {
      kindist@lower <- lower
    }
    if (!is.null(spacing)) {
      kindist@spacing <- spacing
    }
    if (!is.null(n)) {
      kindist@samplenum <- n
    }
    if (!is.null(dims)) {
      kindist@sampledims <- dims
    }
  }
  return(kindist)
}

# additional comment

#' Change the shape of a rectangle while preserving area
#'
#' @param dims  Original rectangle dimensions - either single number (length of side of square) or length 2 numeric vector (lenghts of sides x and y of rectangle)
#' @param shape Ratio of side lengths x & y in the new rectangle
#'
#' @return Returns a numeric vector containing the side lengths c(x, y) of a transformed rectangle with preserved area
#' @export
#'
#' @examples
#' elongate(10, 100)
#' elongate(c(5, 125), 4)
elongate <- function(dims, shape = 1){
  if (length(dims) > 2){
    stop("'dims' cannot have length > 2")
  }
  else if (length(dims) == 1){
    area <- dims^2
  }
  else if (length(dims) == 2){
    area <- dims[1] * dims[2]
  }

  # calculate dimensions
  xdim <- sqrt(area / shape)
  ydim <- area / xdim

  return(c(xdim, ydim))
}

#' Change the dimensions of a KinPairSimulation Object and shift kinpairs so at least one individual is within the area
#'
#' @param kindist   KinPairSimulation - KinPairSimulation Class Object
#' @param dims      New site dimensions - either single number (length of side of square) or length 2 vector (lenghts of sides x and y of rectangle)
#'
#' @return returns a rebased object of class 'KinPairSimulation' with adjusted simulation dimensions
#' @export
#'
#' @examples
#' simobject <- simulate_kindist_simple()
#'
#' rebase_dims(simobject, c(1, 100))
#' rebase_dims(simobject, 15)
rebase_dims <- function(kindist, dims){
  if (length(dims) > 2){
    stop("'dims' cannot have length > 2")
  }
  else if (length(dims) ==1){
    dims <- c(dims, dims)
  }

  nsims <- nrow(kindist@tab)

  x0 <- runif(nsims, 0, dims[1])
  y0 <- runif(nsims, 0, dims[2])

  xdiff <- x0 - kindist@tab$x1
  ydiff <- y0 - kindist@tab$y1

  kindist@tab$x1 <- kindist@tab$x1 + xdiff
  kindist@tab$x2 <- kindist@tab$x2 + xdiff
  kindist@tab$y1 <- kindist@tab$y1 + ydiff
  kindist@tab$y2 <- kindist@tab$y2 + ydiff

  kindist@simdims <- dims
  return(kindist)
}
