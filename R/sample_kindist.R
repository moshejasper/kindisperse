#' Subsample and filter a \code{\link{KinPairSimulation}} or \code{\link{KinPairData}} Object
#'
#' This function takes a pre-existing \code{\link{KinPairSimulation}} or \code{\link{KinPairData}} Object with distance and coordinate data and filters
#' it to simulate various in-field sampling schemes.
#'
#' This function enables the testing of the impact of some basic sampling constraints that might be encountered in study design or
#' implementation on the effectiveness of the \code{kindisperse} estimation of intergenerational dispersal. It is typically paired with
#' a simulation function such as \code{\link{simulate_kindist_composite}} to generate a 'pure' dataset, then an estimation function
#' such as \code{\link{axpermute}} to examine the impact of filter settings on the 'detected' value of dispersal sigma. The filter
#' parameters \code{upper}, \code{lower}, & \code{spacing} all work on the vector of (direction-independent) distances, & the parameter
#' \code{n} enables the random subsampling of n kin dyads. The parameter \code{dims} requires 2D location information for each individual,
#' meaning it can ordinarily only be used with the \code{KinPairSimulation} object (not \code{KinPairData}). All filter parameters are
#' stackable.
#'
#' The \code{upper} parameter implements a cutoff for the \strong{maximum} distance allowable in the dataset. If set to e.g. 100m, all kin dyads
#' separated by a distance greater than 100m will be excluded from the filtered dataset. Note that this is a geometry-independent metric;
#' it is naive to the edge effects of an actual sample site. The \code{lower} parameter implements a cutoff for the \strong{minimum}
#' distance allowable in the dataset.It operates in the same manner as the previous parameter (in this case, removing results smaller
#' than a distance threshold)
#'
#' The \code{spacing} parameter as currently implemented takes all distances & alters them to lie at the midpoint of a bin with width set
#' by this parameter. So if \code{spacing} is set to 10 meters, all kin pairs with distances between 0 and 10m will have their distances
#' rest to 5m, all between 10 & 20 will be set to 15 m, etc. (quantizing the data). Note that once again this is a geometry-independent
#' action: These binwiths & 'trap spacing' are not spatially related to each other like they would be in a sample site, and there is no
#' simulated dropout of kinpairs too far from a trap. There is also no geometry-dependent profiling of possible frequency of recaptures
#' across each distance category (will be implemented in a future version). (this parameter leaves 2D spatial information intact)
#'
#' The \code{dims} parameter defines the dimensions of a rectangle within which \strong{both} individuals of a kin dyad will need to lie
#' to be included in the filtered dataset. This measure (which excludes e.g. long-distance dispersal into & out of the study site) is
#' \emph{geometry-dependent}, unlike the \code{upper} parameter. This enables the testing of (rectangular) site geometries potentially
#' corresponding to an actual site (two-dimensional estimates of dispersal such as \code{kindisperse} become unreliable as edge effects
#' significantly reduce the size of either one or both dimensions with respect to the real underlying dispersal sigma). These site
#' geometries can be entered in a few ways: (a) a single numeric value, which will be interpreted as the length of the side of a square;
#' (b) a numeric vector of length two, which will be interpreted as the length & width of the sample site; (c) either of the above passed
#' to the \code{\link{elongate}} function, which takes the rectangular site dimensions and alters their \code{aspect ratio} (ratio of
#' length to width) while preserving the underlying area the study site covers. The implementation of this filtering step permutes the
#' absolute positions of all dyads so that at least one member of the dyad is in the inial site rectangle, while preserving their relative
#' positions (and angles) with respect to each other. This means that following this step, the xy coordinate positions of each
#' individual will not match those contained in the previous round. It also means that the repeated calling of this function will
#' result in a steady reduction in retained kin dyads due to edge effects.
#'
#'
#' The \code{n} parameter randomly samples n pairs from the dataset. It is implemented after all other filtering has taken place, so
#' will only sample surviving individuals A typical strategy for the use of this functions in simulations would be to simulate an
#' extremely large (e.g. one million pairs) dataset, then pass it repeatedly to this filter function, with a final sub-sampling step of
#' 1,000 included. This enables comparisons across sampling conditions (in most cases) regardless of the amount of data filtered prior to
#' this step.
#'
#' As this function returns a \code{KinPairData} or \code{KinPairSimulation} object, the returned object can be passed back for filtering
#' an arbitrary number of times, or alternatively passed to an estimation strategy.
#'
#' This function can be used to test for bias in the results of a close-kin dispersal study that has been conducted. After the field
#' sampling, kin identification, & sigma calculation steps, use the estimated sigmas as inputs into simulation functions that are
#' then filtered for size & geometry of the actual study site (via the \code{dims} method). Then pass this filtered dataset back to the
#' sigma-determining functions. If filtering has resulted in a substantial drop in sigma, the estimate of sigma from the study site has
#' likely been biased by the site geometry (note that the impact of this is dependent on the shape of the dispersal kernel - the more
#' leptokurtic (dominated by long-distance dispersal), the more severe bias will be for a particular sigma and site geometry.
#'
#' @param kindist \code{KinPairSimulation} Class Object
#' @param upper   \code{numeric} - upper cutoff for kin pair distances
#' @param lower   \code{numeric} - lower cutoff for kin pair distances
#' @param spacing \code{numeric} - spacing between traps (location-independent)
#' @param n       \code{numeric} - number of individuals to keep after filtering (if possible)
#' @param dims    dimensions to sample within (works with the \code{KinPairSimulation} spatial & dimension information).
#' Either \code{num} (defining a square) or \code{c(num1, num2)} (defining a rectangle).
#'
#' @return  returns an object of class \code{KinPairData} or \code{KinPairSimulation} containing simulation and filtering
#' details and a filtered dataset of dispersed individuals.
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
      message("Ignoring 'dims' as object is not of class KinPairSimulation")
    } else {

      # check if dimensions smaller than original dimensions!
      simdims <- kindist@simdims
      if (length(dims) > 2){ #(dims > simdims) {
        message("Ignoring 'dims' as vector length is greater than two")
      }
      else {

        # first, rebase kindist...
        if (length(dims) == 1){
          dims <- c(dims, dims)
        }

        message(paste0("Setting central sampling area to ", dims[1], " by ", dims[2]))

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
    message(paste0("Removing distances farther than ", upper))
    kindist@tab <- filter(kindist@tab, .data$distance < upper)
  }

  if (!is.null(lower)) {
    message(paste0("Removing distances closer than ", lower))
    kindist@tab <- filter(kindist@tab, .data$distance > lower)
  }

  if (!is.null(spacing)) {
    message(paste0("Setting trap spacing to ", spacing))

    binshift <- spacing / 2

    kindist@tab <- mutate(kindist@tab, distance = ((.data$distance %/% spacing) * spacing) + binshift) # changed equation here... (need to rerun estimates)
  }

  if (!is.null(n)) {
    if (!n < nrow(kindist@tab)) {
      message(paste0("Less than n = ", n, " kin pairs remaining (", nrow(kindist@tab), ") - skipping downsampling"))
    }
    else {
      message(paste0("Down-sampling to ", n, " kin pairs"))
      kindist@tab <- slice_sample(kindist@tab, n = n)
    }
  }
  message(paste0(nrow(kindist@tab), " kin pairs remaining.\n"))
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

#' Change the shape (aspect ratio) of a rectangle while preserving area
#'
#' This function is used to manipulate the dimensions parameter in other package functions, which control site dimentions.
#' These geometries can be entered innto functions in a few ways: (a) a single numeric value, which will be interpreted as the length of the side of a square;
#' (b) a numeric vector of length two, which will be interpreted as the length & width of the sample site; (c) either of the above passed
#' to this function, which takes the rectangular site dimensions and alters their \code{aspect ratio} (ratio of
#' length to width) while preserving the underlying area the study site covers.
#' @param dims  Original rectangle dimensions - either single number (length of side of square) or length 2 numeric vector (lengths of sides x and y of rectangle)
#' @param aspect Aspect ratio of side lengths x & y (i.e. x/y) in the new rectangle
#'
#' @return Returns a numeric vector containing the side lengths c(x, y) of a transformed rectangle with preserved area
#' @export
#'
#' @examples
#' elongate(10, 100)
#' elongate(c(5, 125), 4)
elongate <- function(dims, aspect = 1){
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
  xdim <- sqrt(area * aspect)
  ydim <- area / xdim

  return(c(xdim, ydim))
}

#' Change the dimensions of a KinPairSimulation Object and shift kinpairs so at least one individual is within the area
#'
#' @param kindist   \code{KinPairSimulation} - \code{KinPairSimulation} Class Object
#' @param dims      New site dimensions - either single number (length of side of square) or length 2 vector (lengths of sides x and y of rectangle)
#'
#' @return returns a rebased object of class \code{\link{KinPairSimulation}} with adjusted simulation dimensions
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
