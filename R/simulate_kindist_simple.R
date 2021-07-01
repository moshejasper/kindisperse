
#' Simulate kin dispersal distance pairs with simple sigma
#'
#' Simulates intergenerational dispersal defined by a simple dispersal sigma (covering the entire lifecycle) and ignoring phase
#' differences between full & half sibling dispersal categories. Returns an object of class \code{\link{KinPairSimulation}}
#'
#' This function is one of a family of functions that implement the core intergenerational dispersal simulations
#' contained in the \code{kindisperse} package. Each of these functions proceeds by the following steps:
#' \enumerate{
#' \item identify the pedigree
#' relationship, dispersal phase (FS, HS & PO) and sampling stage that must be generated;
#' \item randomly assign a coordinate position
#' to the 'root' individual within the pedigree (i.e. last common ancestor of the dyad, inclusive);
#' \item 'disperse' both pathways from
#' this root position via the appropriately defined phase dispersal (additively via random draws from the underlying statistical
#' model, defined by an axial standard deviation - sigma);
#' \item further disperse both phased descendant branches according to the
#' number of realised breeding dispersal cycles contained in the defining pedigree (additively via random draws from the chosen
#' underlying statistical model);
#' \item add displacement caused by dispersal before the sampling point in a similar manner to above,
#' defining the final positions of the sampled dispersed kin dyads;
#' \item calculating geographical distances between the
#' resulting dyads.
#' }
#'
#' These simulation functions operate under an additive variance framework: all individual dispersal events are modeled as random
#' draws from a bivariate probability distribution defined by an axial standard deviation \code{sigma} and (sometimes) a shape
#' parameter. At present, three such distributions are included as options accessible with the \code{method} parameter: the
#' bivariate normal distribution '\code{Gaussian}', the bivariate Laplace distribution '\code{Laplace}', and the bivariate
#' variance-gamma distribution '\code{vgamma}'. The \code{Gaussian} (normal) distribution enables easy compatibility with the
#' framework under which much population genetic & dispersal theory (isolation by distance, neighbourhoods, etc.) have been
#' developed. The \code{Laplace} distribution is a multivariate adaptation of the (positive) exponential distribution, and
#' represents a more 'fat-tailed' (leptokurtic) disperal situation than Gaussian. The \code{vgamma} distribution is a mixture
#' distribution formed by mixing the gamma distribution with the bivariate normal distribution. The flexibility of this
#' distribution's \code{shape} parameter enables us to model arbitrarily leptokurtic dispesal kernels, providing a helpful way
#' to examine the impacts of (e.g.) long distance dispersal on the overall disperal distribution and sampling decisions. A
#' \code{vgamma} distribution with shape parameter equal to 1 reduces to the bivariate Laplace distribution. As shape approaches
#' infinity, the \code{vgamma} distribution approaches the bivariate normal distribution. As shape approaches zero, the distribution
#' becomes increasingly leptokurtic.
#'
#' The \code{simulate_kindist_simple()} function is the most basic of the simulation functions, ignoring all information about
#' dispersal phase and treating dispersal with a single sigma corresponding to the entire lifecycle to breeding of the
#' dispersed individuals. It is useful for exploring simple intergenerational dispersal in a stripped back context; for many
#' typical contexts involving complex dispersal across different phases of the breeding cycle, the other dispersal simulation
#' functions would be more suitable.
#'
#'
#'
#' Following simulation, the results are returned as an object of the specially defined package class \code{\link{KinPairSimulation}},
#' which stores the simulation results along with information about all simulation parameters, and can be further passed to
#' sample filtering & dispersal estimation functions.
#'
#' @param nsims   (integer) -   number of pairs to simulate
#' @param sigma   (numeric) -   size of simple (axial) sigma
#' @param dims    (numeric) -   length of sides of (square) simulated site area
#' @param method  (character) - kernel shape to use: either 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @param kinship (character)- kin category to simulate: one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param lifestage (lifestage) lifestage at sample collection: either 'immature' or 'ovipositional'
#' @param shape   (numeric) - value of shape parameter to use with 'vgamma' method. Default 0.5. Must be > 0. Increment towards zero for increasingly heavy-tailed (leptokurtic) dispersal
#'
#' @return      returns an object of class \code{\link{KinPairSimulation}} containing simulation details and a \code{tibble} (tab) of simulation values
#' @export
#' @family simulate_kindist
#' @importFrom tibble tibble
#' @examples
#' test <- simulate_kindist_simple(nsims = 10, sigma = 50, dims = 1000, method = "Laplace")
#' simulate_kindist_simple(nsims = 10000, sigma = 75, kinship = "PO", lifestage = "ovipositional")
simulate_kindist_simple <- function(nsims = 100, sigma = 125, dims = 100, method = "Gaussian",
                                    kinship = "PO", lifestage = "immature", shape = 0.5) {
  if (!method %in% c("Gaussian", "Laplace", "vgamma")) {
    stop("Invalid Method! - choose from 'Gaussian', 'Laplace' or 'vgamma'")
  }

  if (!kinship %in% c(
    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
    "HGAV", "H1C", "H1C1", "H2C"
  )) {
    stop("Invalid Kinship Category - choose from PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C")
  }

  if (!lifestage %in% (c("ovipositional", "immature"))) {
    stop("Invalid Lifestage - available options are 'ovipositional' and 'immature'")
  }

  if (method == "Gaussian") { # bivariate symmetric Gaussian distribution
    rdistr <- function(sig) {
      return(matrix(c(rnorm(nsims, 0, sig), rnorm(nsims, 0, sig)), ncol = 2))
    }
  }
   else if (method == "vgamma"){ # bivariate symmetric variance-gamma distribution
    rdistr <- function(sig){
      Sigma <- matrix(c(sig^2, 0, 0, sig^2), ncol = 2)
      mu <- rbind(c(0, 0))
      n <- nsims

      k <- ncol(Sigma)
      if (n > nrow(mu))
        mu <- matrix(mu, n, k, byrow = TRUE)
      e <- matrix(rgamma(n, scale = 1, shape = shape), n, k) / shape
      z <- LaplacesDemon::rmvn(n, rep(0, k), Sigma)
      x <- mu + sqrt(e) * z
      return(x)
    }
   }
  else if (method == "Laplace") { # bivariate symmetric Laplace distribution
    rdistr <- function(sig) {
      sigdiag <- matrix(c(sig^2, 0, 0, sig^2), ncol = 2)
      xyi <- LaplacesDemon::rmvl(nsims, c(0, 0), sigdiag)
      xf <- xyi[, 1]
      yf <- xyi[, 2]
      return(matrix(c(xf, yf), ncol = 2))
    }
  }

  lspan <- function(spans = 1) {
    if (spans == 0) {
      return(0)
    }
    if (spans == 1) {
      return(rdistr(sigma))
    }

    else {
      disp <- rdistr(sigma)
      s <- spans - 1
      while (s > 0) {
        disp <- disp + rdistr(sigma)
        s <- s - 1
      }
      return(disp)
    }
  }

  # initial locations
  if (length(dims) > 2){
    stop("'dims' vector can have no more than two elements")
  }
  if (length(dims) == 1){
    dims <- c(dims, dims)
  }
  x0 <- runif(nsims, 0, dims[1])
  y0 <- runif(nsims, 0, dims[2])
  xy0 <- matrix(c(x0, y0), ncol = 2)

  # test phase


  if (kinship %in% c("PO", "GG", "GGG")) {
    phase <- "PO"
  }
  if (kinship %in% c("FS", "AV", "1C", "GAV", "1C1", "2C")) {
    phase <- "FS"
  }
  if (kinship %in% c("HS", "HAV", "H1C", "HGAV", "H1C1", "H2C")) {
    phase <- "HS"
  }

  # test span1

  if (kinship %in% c("FS", "HS", "PO", "AV", "HAV", "GG", "GAV", "GHAV", "GGG")) {
    span1 <- 0
  }
  if (kinship %in% c("1C", "H1C", "1C1", "H1C1")) {
    span1 <- 1
  }
  if (kinship %in% c("2C", "H2C")) {
    span1 <- 2
  }

  if (kinship %in% c("FS", "HS")) {
    span2 <- 0
  }
  if (kinship %in% c("AV", "HAV", "1C", "H1C", "PO")) {
    span2 <- 1
  }
  if (kinship %in% c("GAV", "GHAV", "GG", "1C1", "H1C1", "2C", "H2C")) {
    span2 <- 2
  } # an issue with PO... probably gonna have to make a special relation class...
  if (kinship %in% c("GGG")) {
    span2 <- 3
  }

  # resolve phased dispersal

  if (phase == "PO") {
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  if (phase == "FS") {
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  if (phase == "HS") {
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  # resolve lifespan dispersal

  xy1_span <- xy1_phased + lspan(span1)
  xy2_span <- xy2_phased + lspan(span2)

  # resolve collection point

  if (lifestage == "immature") {
    xy1_final <- xy1_span
    xy2_final <- xy2_span
  }
  else if (lifestage == "ovipositional") {
    xy1_final <- xy1_span + lspan()
    xy2_final <- xy2_span + lspan()
  }

  # return appropriate data form...

  id1 <- paste0(1:nsims, "a")
  id2 <- paste0(1:nsims, "b")
  x1 <- xy1_final[, 1]
  y1 <- xy1_final[, 2]
  x2 <- xy2_final[, 1]
  y2 <- xy2_final[, 2]
  ls1 <- lifestage
  ls2 <- lifestage
  distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

  tab <- tibble(
    id1 = id1, id2 = id2,
    x1 = x1, y1 = y1, x2 = x2, y2 = y2,
    distance = distance,
    kinship = kinship
  )
  if (method == "vgamma") kernelshape <- shape
  else kernelshape <- NULL
  model <- dispersal_model(posigma = sigma)

  return(KinPairSimulation_simple(
    data = tab, kinship = kinship, kerneltype = method,
    posigma = sigma, simdims = dims, lifestage = lifestage,
    kernelshape = kernelshape, call = sys.call(), model = model
  ))
}
