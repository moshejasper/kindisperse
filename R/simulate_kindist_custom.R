#' Simulate kin dispersal distance pairs with custom species dispersal models.
#'
#' Simulates intergenerational dispersal in a species defined by multiple dispersal components across the breeding cycle, with
#' dispersal, breeding & sampling & basic generational structure custom-defined by a \code{\link{DispersalModel}} object.
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
#' The \code{simulate_kindist_custom()} function is designed to enable modeling of the composite dispersal events that occur
#' \strong{within} the breeding cycle of an organism, and enables the separate treatment of the \code{PO, FS}, and \code{HS}
#' phases in situations where the breeding and dispersal cycle of an organism is (somewhat more complex that that
#' encountered in organisms such as mosquitoes (i.e. single-generational breeding organisms with defined sampling
#' points). This function relies on a custom dispersal model of class \code{\link{DispersalModel}} defined via parameter
#' \code{model} to supply organism-specific information about dispersal stages (with axial sigmas), FS & HS branch points, and
#' the dispersal stage at which sampling occurs. Via this model object (or overridden by the \code{cycle} parameter) you can
#' also define the number of breeding cycles each final individual within the close-kin dyad has passed through before sampling.
#' This is defined as a length one or two non-negative integer (where a length-one integer of value a is converted to a length
#' two integer of value c(a, a)), where the first integer defines the number of life cycles passed through by the 'senior'
#' pedigree member of the dyad, and the second the number passed through by the 'junior' member (so the GG phase has a grandparent
#' as senior, the grandchild as junior, etc. (in practice this distinction is unimportant for dyads). A cycle number of 0
#' references an individual that hasn't lived through an entire breeding cycle (sampling phase to sampling phase) before being
#' sampled. A value of 1 references an individual that has lived through one such cycle (e.g. a female entering her second breeding
#' season, an ovipositing mosquito (where the oviposition dispersal stage overlaps with the larval dispersal stage)). A value of
#' 2 references two such cycles, etc. As all cycles are considered equivalent in the current formulation of this model (whether
#' an individual enters the cycle as a juvenile or as an adult) care must be taken in applying this system to species where the
#' dispersal behaviour of a second cycle individual (i.e. adult) is likely to be substantially different to that of a first cycle
#' individual (often an immature individual).
#'
#' This function can only handle one kinship pairing & dispersal mode in the one simulation: where multiple dispersal pathways lead
#' to the same kinship outcome, each pathway should be simulated separately, and the resulting distributions combined subsequently.
#'
#'
#' Following simulation, the results are returned as an object of the specially defined package class \code{\link{KinPairSimulation}},
#' which stores the simulation results along with information about all simulation parameters, and can be further passed to
#' sample filtering & dispersal estimation functions.
#'
#' @param nsims    (integer) -   number of pairs to simulate
#' @param model   (object of class \code{DispersalModel}) - species-specific model of dispersal containing lifestage & phase parameters
#' @param dims     (numeric) -   length of sides of (square) simulated site area
#' @param method   (character) - kernel shape to use: either 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @param kinship  (character)- kin category to simulate: one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C H1C1 or H2C
#' @param lifestage (character) lifestage at sample collection - must match a lifestage supplied in the 'model' parameter
#' @param cycle (numeric) - breeding cycle number(s) of dispersed kin to be modelled. Must be a nonnegative integer. (0, 1, 2, ...) or vector of two nonnegative integers. Represents the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param shape    (numeric) - value of shape parameter to use with 'vgamma' method. Default 0.5. Must be > 0. Increment towards zero for increasingly heavy-tailed (leptokurtic) dispersal
#'
#' @return returns an object of class \code{KinPairSimulation} containing simulation details and a tibble (tab) of simulation values
#' @export
#' @family simulate_kindist
#'
#' @examples
#' custom_dispersal_model <- dispersal_model(a = 10, b = 25, .FS = "b", .HS = "a")
#' simulate_kindist_custom(nsims = 100, model = custom_dispersal_model, cycle = c(0, 1),
#' kinship = "FS", lifestage = "b")
simulate_kindist_custom <- function(nsims = 100, model = dispersal_model(init = 100, breed = 50, grav = 50,
                                                                         ovi = 25, .FS = "ovi", .HS = "breed"),
                                    dims = 100, method = "Gaussian", kinship = "FS",
                                       lifestage = sampling_stage(model), cycle = 0, shape = 0.5) {
  if (!method %in% c("Gaussian", "Laplace", "vgamma")) {
    stop("Invalid Method! - choose from 'Gaussian', 'Laplace' or 'vgamma'")
  }

  if (!kinship %in% c(
    "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV",
    "HGAV", "H1C", "H1C1", "H2C"
  )) {
    stop("Invalid Kinship Category")
  }

  if (!lifestage %in% model@stages & ! lifestage == 0) {
    stop("Invalid Lifestage")
  }

  if (method == "Gaussian") { # bivariate symmetric Gaussian distribution
    rdistr <- function(sig) {
      return(matrix(c(rnorm(nsims, 0, sig), rnorm(nsims, 0, sig)), ncol = 2))
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

  lspan <- function(spans = 1) {
    if (spans == 0) {
      return(0)
    }
    if (spans == 1) {
      disp <- matrix(0, nrow = nsims, ncol = 2)
      for (stage in dispersal_vector(model)){
        disp <- disp + rdistr(stage)
      }
      return(disp)
    }

    else {
      disp <- matrix(0, nrow = nsims, ncol = 2)
      for (stage in dispersal_vector(model)){
        disp <- disp + rdistr(stage)
      }
      #disp <- rdistr(initsigma) + rdistr(breedsigma) + rdistr(gravsigma) + rdistr(ovisigma)
      s <- spans - 1
      while (s > 0) {
        for (stage in dispersal_vector(model)){
          disp <- disp + rdistr(stage)
        }
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
  if (! fs(model) == 0){
  fs_phase <- dispersal_vector(model)[match(fs(model), stages(model)):length(dispersal_vector(model))]
  }
  else fs_phase <- fs(model)
  if (! hs(model) == 0){
  hs_phase <- dispersal_vector(model)[match(hs(model), stages(model)):length(dispersal_vector(model))]
  }
  else hs_phase <- hs(model)

  if (phase == "PO") {
    xy1_phased <- xy0
    xy2_phased <- xy0
  }
  if (phase == "FS") {
    xy1_phased <- xy0
    xy2_phased <- xy0
    if (! any(fs_phase == 0)){
    for (p in fs_phase){
      xy1_phased <- xy1_phased + rdistr(p)
      xy2_phased <- xy2_phased + rdistr(p)
    }
    }
  }
  if (phase == "HS") {
    xy1_phased <- xy0
    xy2_phased <- xy0
    if (! any(hs_phase == 0)){
    for (p in hs_phase){
      xy1_phased <- xy1_phased + rdistr(p)
      xy2_phased <- xy2_phased + rdistr(p)
    }
    }
  }
  # resolve lifespan dispersal

  xy1_span <- xy1_phased + lspan(span1)
  xy2_span <- xy2_phased + lspan(span2)

  # resolve collection point

  if (lifestage == 0 | lifestage == sampling_stage(model)) {
    xy1_final <- xy1_span
    xy2_final <- xy2_span
  }
  else {
    sample_span <- dispersal_vector(model)[1:match(lifestage, stages(model))]
    xy1_final <- xy1_span
    xy2_final <- xy2_span
    for (p in sample_span){
      xy1_final <- xy1_final + rdistr(p)
      xy2_final <- xy2_final + rdistr(p)
    }
  }

  if (length(cycle) > 2){
    stop("'cycle' vector can have no more than two elements")
  }
  if (length(cycle) == 1){
    cycle <- c(cycle, cycle)
  }

  if (!cycle_to_span(cycle) == 0){
    xy1_final <- xy1_final + lspan(cycle[1])
    xy2_final <- xy2_final + lspan(cycle[2])
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

  #return(df_to_kinpair(tab, kinship = kinship, lifestage = as.character(lifestage), lifecheck = FALSE))
  return(KinPairSimulation_custom(tab,
                                     kinship = kinship, kerneltype = method, customsigma = dispersal_vector(model),
                                     simdims = dims, lifestage = lifestage, kernelshape = kernelshape, cycle = cycle,
                                     call = sys.call()
  ))
}
