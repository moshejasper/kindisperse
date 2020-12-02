
#' Simulate kin dispersal distance pairs with simple sigma
#'
#' @param nsims   (integer) -   number of pairs to simulate
#' @param sigma   (numeric) -   size of simple (axial) sigma
#' @param dims    (numeric) -   length of sides of (square) simulated site area
#' @param method  (character) - kernel shape to use: either 'Gaussian' or 'Laplace'
#' @param category (character)- kin category to simulate: one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param lifestage (lifestage) lifestage at sample collection: either 'larva' or 'oviposition'
#' @param shape   NULL - placeholder for future kernels
#'
#' @return      returns an object of class 'KinPairSimulation' containing simulation details and a tibble (tab) of simulation values
#' @export
#' @importFrom tibble tibble
#' @examples
#' test <- simulate_kindist_simple(nsims = 10, sigma = 50, dims = 1000, method = "Laplace")
#' simulate_kindist_simple(nsims = 100000, sigma = 75, category = "PO", lifestage = "oviposition")
simulate_kindist_simple <- function(nsims = 100, sigma = 125, dims = 100, method = "Gaussian",
                                    category = "FS", lifestage = "larva", shape = 2){


  if (! method %in% c("Gaussian", "Laplace")) {
    stop("Invalid Method! - choose from 'Gaussian' or 'Laplace'")
    stop("Invalid Method! - choose from 'Gaussian', 'Exponential', 'Gamma', 'Weibull' or 'Laplace'")
  }

  if (! category %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
                        "HGAV", "H1C", "H1C1", "H2C")) {
    stop("Invalid Kinship Category - choose from PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C")
  }

  if (! lifestage %in% (c("oviposition", "larva"))) {
    stop("Invalid Lifestage - available options are 'oviposition' and 'larva'")
  }

  if (method == "Gaussian"){
    rdistr <- function(sig){
      return(matrix(c(stats::rnorm(nsims, 0, sig), stats::rnorm(nsims, 0, sig)), ncol = 2))
    }
  }
  else if (method == "Norm"){
    rdistr <- function(sig){
      xi <- stats::rnorm(nsims, 0, sig)
      yi <- stats::rnorm(nsims, 0, sig)
      dst <- sqrt(xi^2 + yi^2)
      angle <- stats::runif(nsims, 0, 2 * pi)
      xf <- dst * cos(angle)
      yf <- dst * sin(angle)
      return(matrix(c(xf, yf), ncol = 2))
    }
  }
  else if (method == "Exponential"){
    rdistr <- function(sig){
      xi <- stats::rexp(nsims, 1/sig)
      yi <- stats::rexp(nsims, 1/sig)
      dst <- sqrt(xi^2 + yi^2)
      angle <- stats::runif(nsims, 0, 2 * pi)
      xf <- dst * cos(angle)
      yf <- dst * sin(angle)
      return(matrix(c(xf, yf), ncol = 2))
    }
  }
  else if (method == "Weibull"){
    rdistr <- function(sig){
      xi <- stats::rweibull(nsims, shape, scale = sig)
      yi <- stats::rweibull(nsims, shape, scale = sig)
      dst <- sqrt(xi^2 + yi^2)
      angle <- stats::runif(nsims, 0, 2 * pi)
      xf <- dst * cos(angle)
      yf <- dst * sin(angle)
      return(matrix(c(xf, yf), ncol = 2))
    }
  }
  else if (method == "Gamma"){
    rdistr <- function(sig){
      sigdiag <- matrix(c(1, 0, 0, 1), ncol = 2)
      xyi <- lcmix::rmvgamma(nsims, shape, sqrt(2)*sqrt(shape)/sig, sigdiag)
      xi <- xyi[,1]
      yi <- xyi[,2]
      dst <- sqrt(xi^2 + yi^2)
      #dst <- rgamma(nsims, shape, scale = sig / sqrt(shape))
      angle <- stats::runif(nsims, 0, 2 * pi)
      xf <- dst * cos(angle)
      yf <- dst * sin(angle)
      return(matrix(c(xf, yf), ncol = 2))
    }
  }
  else if (method == "Laplace"){ # bivariate symmetric laplacian
    rdistr <- function(sig){
      sigdiag <- matrix(c(sig^2, 0, 0, sig^2), ncol = 2)
      xyi <- LaplacesDemon::rmvl(nsims, c(0, 0), sigdiag)
      xf <- xyi[,1]
      yf <- xyi[,2]
      return(matrix(c(xf, yf), ncol = 2))
    }
  }

  lspan <- function(spans = 1){

    if (spans == 0){
      return(0)
    }
    if (spans == 1){
      return(rdistr(sigma))
    }

    else {
      disp <- rdistr(sigma)
      s <- spans - 1
      while(s > 0) {
        disp <- disp + rdistr(sigma)
        s <- s - 1
      }
      return(disp)
    }
  }

  # initial locations

  x0 <- stats::runif(nsims, 0, dims)
  y0 <- stats::runif(nsims, 0, dims)
  xy0 <- matrix(c(x0, y0), ncol = 2)

  # test phase


  if (category %in% c("PO", "GG", "GGG")) {phase <- "PO"}
  if (category %in% c("FS", "AV", "1C", "GAV", "1C1", "2C")) { phase <- "FS"}
  if (category %in% c("HS", "HAV", "H1C", "HGAV", "H1C1", "H2C")) {phase <- "HS"}

  # test span1

  if (category %in% c("FS", "HS", "PO", "AV", "HAV", "GG", "GAV", "GHAV", "GGG")) { span1 <- 0}
  if (category %in% c("1C", "H1C", "1C1", "H1C1")) { span1 <- 1}
  if (category %in% c("2C", "H2C")) { span1 <- 2}

  if (category %in% c("FS", "HS")) { span2 <- 0}
  if (category %in% c("AV", "HAV", "1C", "H1C", "PO")) { span2 <- 1}
  if (category %in% c("GAV", "GHAV", "GG", "1C1", "H1C1", "2C", "H2C")) { span2 <- 2} # an issue with PO... probably gonna have to make a special relation class...
  if (category %in% c("GGG")) {span2 <- 3}

  # resolve phased dispersal

  if (phase == "PO") {
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  if (phase == "FS"){
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  if (phase == "HS"){
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  # resolve lifespan dispersal

  xy1_span <- xy1_phased + lspan(span1)
  xy2_span <- xy2_phased + lspan(span2)

  # resolve collection point

  if (lifestage == "larva"){
    xy1_final <- xy1_span
    xy2_final <- xy2_span
  }
  else if (lifestage == "oviposition"){
    xy1_final <- xy1_span + lspan()
    xy2_final <- xy2_span + lspan()
  }

  # return appropriate data form...

  id1 <- paste0(1:nsims, "a")
  id2 <- paste0(1:nsims, "b")
  x1 <- xy1_final[,1]
  y1 <- xy1_final[,2]
  x2 <- xy2_final[,1]
  y2 <- xy2_final[,2]
  ls1 <- lifestage
  ls2 <- lifestage
  distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

  tab <- dplyr::tibble(id1 = id1, id2 = id2,
                x1 = x1, y1 = y1, x2 = x2, y2 = y2,
                ls1 = ls1, ls2 = ls2, distance = distance,
                category = category, dims = dims)

  return(KinPairSimulation_simple(tab, category = category, kerneltype = method,
                                       sigma = sigma, dims = dims, lifestage = lifestage,
                                       call = sys.call()))


}
