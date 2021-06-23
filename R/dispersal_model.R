#' Build species-specific dispersal model with custom lifestage, phase & sampling parameters
#'
#' @param ... name, value (numeric) pairs pairing custom lifestages with their corresponding axial dispersal values. MUST
#' be in chronological order across the entire breeding cycle.
#' @param .FS (character) - breeding cycle stage at which first substantial FS-phased dispersal occurs. Must correspond to a
#' previously described cycle stage name. Typically reflects the first dispersal of female gametes from the mother at (variously)
#' egg-laying, birth, weaning stages (species-dependent). Use care in adapting to situations where multiple breeding and/or dispersal
#' routes commonly lead to the FS phase
#' @param .HS (character) - breeding cycle stage at which first substantial HS-phased dispersal occurs. Must correspond to a
#' previously described cycle stage name. Typically reflects the movement of male gametes at e.g. the breeding stage (use care in adapting
#' to situations where multiple dispersal routes commonly lead to the HS phase)
#' @param .lifestage (character) - stage in the breeding cycle at which samples are to be collected for kin identification. Must correspond
#' to a previously described cycle stage name. (so collection of eggs corresponds to an egg-laying stage, as juveniles to a juvenile stage, etc.)
#' @param .cycle (non-negative integer or vector of two non-negative integers) breeding cycle numbers of dispersed kin to be modeled. Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp.
#' where there is likely to be a reasonable equivalence between breeding stages across a lifespan. As the rest of the model is compatible
#' with a variety of cycle points, this parameter will often be overridden by the 'cycle' parameter in the \code{simulate_kindist_custom} function.
#'
#' @return Returns an object of class \code{DispersalModel} containing custom lifestages and dispersal, phase & sampling parameters that
#' can be passed to simulation functions.
#' @export
#'
#' @examples
#' antechinus_model <- dispersal_model(pouch = 25, nest = 25, free_living = 250, breeding = 40,
#' gestation = 25, .FS = "nest", .HS = "breeding", .lifestage = "nest")
#' antechinus_model
dispersal_model <- function(..., .FS = 0, .HS = .FS, .lifestage = 0, .cycle = 0){
  xs <- c(...)
  if (length(.cycle) > 2){
    stop("'.cycle' vector can have no more than two elements")
  }
  if (length(.cycle) == 1){
    .cycle <- c(.cycle, .cycle)
  }
  if (! isTRUE(all.equal(.cycle, as.integer(.cycle))) | any(.cycle < 0)) stop("'.cycle' vector is not of nonnegative integers!")
  output <- list(ls = xs, fs = .FS, hs = .HS, samp = .lifestage, stages = names(xs), cycle = .cycle)
  if (! output$fs %in% output$stages & output$fs != 0) stop(".FS is not a listed dispersal stage or 0!")
  if (! output$hs %in% output$stages & output$hs != 0) stop(".HS is not a listed dispersal stage or 0!")
  if (! output$samp %in% output$stages & output$samp != 0) stop(".lifestage is not a listed dispersal stage or 0!")
  # reorganise stages to correlate with sampling point.
  if (output$fs == 0) output$fs <- output$stages[1]
  if (output$hs == 0) output$hs <- output$fs
  if (! output$samp == 0){
    output$stages <- c(output$stages[match(output$samp, output$stages):length(output$stages)],
                       output$stages[1:match(output$samp, output$stages) - 1])
    output$stages <- c(output$stages[2:length(output$stages)], output$stages[1])
    output$ls <- output$ls[output$stages]
  }
  if (output$samp == 0) output$samp <- output$stages[length(output$stages)]
  output2 <- new(
    "DispersalModel",
    stages = output$stages,
    dispersal_vector = output$ls,
    fs = output$fs,
    hs = output$hs,
    sampling_stage = output$samp,
    cycle = output$cycle
  )
  return(output2)
}
