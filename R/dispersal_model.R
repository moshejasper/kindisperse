#' Create Dispersal Model of an Organism
#'
#' The function creates an object of class \code{DispersalModel} carrying organism-specific information about dispersal stages (with axial
#' sigmas), FS & HS branch points, and the dispersal stage at which sampling occurs.It is used with the
#' \code{\link{simulate_kindist_custom}} function to enable the simulation of uniquely defined breeding & dispersal cycles.
#'
#' The original simulation functions in this package (\code{simulate_kindist_simple()} & \code{simulate_kindist_composite}) were
#' designed for an organism with a specific (& relatively simple) breeding & dispersal cycle. 'simple' corresponded to a single
#' dispersal event across a lifespan, equivalency of all dispersal phases (FS, HS, PO) and no lifetime overlaps. 'composite'
#' corresponded to many insect dispersal situations, where breeding & oviposition are the key 'phase-defining' events (i.e.,
#' they lead to the initial gamete dispersal of half siblings & full siblings from each other), where field sampling typically
#' occurs via ovitraps
#'
#' More general dispersal scenarios (e.g in mammals) require the ability to uniquely specify a variety of distinct breeding
#' ecologies & sampling schemes: the \code{\link{DispersalModel}} class paired with the \code{\link{simulate_kindist_custom}}
#' function achieves this by defining a breeding cycle with an arbitrary number of dispersal phases (the \code{dispersal_vector}
#' slot, accessed by the \code{\link{dispersal_vector}} method).
#'
#' The breeding structure of a species may also impact at which stage
#' FS and HS phase branches occur. In \emph{Ae. aegypti}, males mate with multiple females in a (single) breeding season, and a female
#' typically carried the egg of only one male. In this context the FS (full-sibling) phase would be set to correspond to the female's
#' oviposition dispersal, while the HS (half-sibling) phase would be set to correspond to the male's breeding dispersal (as its gametes
#' will then be dispersed by multiple females across their gravid & ovipositional phases). However, in e.g. some species of the marsupial
#' \emph{Antechinus}, the FS branch point would be more appropriately associated with juveniles at the time that they leave the mother's
#' pouch. The \code{.FS} and \code{.HS} parameters enable the assignment of these phase branches to any
#' defined life phase. Similarly, the \code{.lifestage} parameter allow the sampling point to be set to correspond to any
#' phase of the defined breeding cycle (this is later accessed with the \code{\link{sampling_stage}} method).
#'
#' The final parameter stored in this object is the breeding cycle number \code{.cycle}, accessed later by the \code{\link{breeding_cycle}} method.
#' This parameter enables the treatment of species that undergo multiple breeding cycles in one lifetime. This is defined as a length two
#' vector describing the number of breeding cycles undergone by the final descendant of branch 1 and branch 2 of the dispersal pedigree before
#' their sampling. (where branch one is the 'senior' and branch two the 'junior' member of the pedigree) (so uncle is branch one, nephew branch
#' two, grandmother branch one, granddaughter branch two, etc.). For each member of the resulting kin pair, the cycle number represents the
#' number of complete breeding cycles each individual has undergone before the sampling point, where the time between birth and first
#' reproduction is coded as '0', that between first and second reproduction '1', etc. This enables an application of the simulation
#' functions defined here to deal with populations with some amount of overlap between generations.
#'
#' Note that this 'breeding cycle' approach is only applicable in situations where there is an approximate equivalence between the dispersal which
#' occurs in the first 'juvenile' breeding cycle and that which occurs between later breeding cycles. This parameter is implemented here, but it
#' will often be more productive to implement it instead as a parameter of the \code{\link{simulate_kindist_custom}} function (the cycle parameter
#' there if set overrides whatever was defined within this object)
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
  if (is.null(xs)) xs <- c(s1 = 0)
  if (is.null(names(xs))) names(xs) <- paste0("s", c(1:length(xs)))
  names(xs)[names(xs) == ""] <- paste0("s", c(1:length(xs)))[names(xs) == ""]
  print(xs)
  if (length(.cycle) > 2){
    stop("'.cycle' vector can have no more than two elements")
  }
  if (length(.cycle) == 1){
    .cycle <- c(.cycle, .cycle)
  }
  if (! isTRUE(all.equal(.cycle, as.integer(.cycle))) | any(.cycle < 0)) stop("'.cycle' vector is not of nonnegative integers!")
  ls <- xs; fs <- .FS; hs <- .HS; samp <- .lifestage; stages <- names(xs); cycle <-  .cycle
  if (! fs %in% stages & fs != 0) stop(".FS is not a listed dispersal stage or 0!")
  if (! hs %in% stages & hs != 0) stop(".HS is not a listed dispersal stage or 0!")
  if (! samp %in% stages & samp != 0) stop(".lifestage is not a listed dispersal stage or 0!")
  # reorganize stages to correlate with sampling point.
  if (fs == 0) fs <- stages[1]
  if (hs == 0) hs <- fs
  if (! samp == 0){
    stages <- c(stages[match(samp, stages):length(stages)],
                       stages[1:match(samp, stages) - 1])
    stages <- c(stages[2:length(stages)], stages[1])
    ls <- ls[stages]
  }
  if (samp == 0) samp <- stages[length(stages)]
  print(samp)
  output <- new(
    "DispersalModel",
    stages = stages,
    dispersal_vector = ls,
    fs = fs,
    hs = hs,
    sampling_stage = samp,
    cycle = cycle
  )
  return(output)
}
