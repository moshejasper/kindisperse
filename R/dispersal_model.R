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
#' defined life phase. Similarly, the \code{.sampling_stage} parameter allow the sampling point to be set to correspond to any
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
#' @param .sampling_stage (character) - stage in the breeding cycle at which samples are to be collected for kin identification. Must correspond
#' to a previously described cycle stage name. (so collection of eggs corresponds to an egg-laying stage, as juveniles to a juvenile stage, etc.)
#' @param .cycle (integer >= -1 or vector of two such integers) breeding cycle numbers of dispersed kin to be modeled. Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' first dispersal and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). If .cycle
#' is specially set to '-1' this constitutes the sampling of an individual before it has differentiated (via dispersal) from the parent. Only use in spp.
#' where there is likely to be a reasonable equivalence between breeding stages across a lifespan. As the rest of the model is compatible
#' with a variety of cycle points, this parameter will often be overridden by the 'cycle' parameter in the \code{simulate_kindist_custom} function.
#' @param .breeding_stage (character) - stage in the cycle at which breeding occurs. Must correspond to a previously described cycle
#' stage name. By default, equated with the .HS stage. This stage corresponds to the \strong{generation} of next-generation individuals;
#' the .FS & .HS stages correspond to their separation. Needed for situations where individuals are sampled before they separate from the parent.
#' Modify if the modeled .HS gamete dispersal event does not correspond to the initial breeding event.
#' @param .visible_stage (character) - stage in the cycle at the \strong{beginning} of which individuals are visible to the study for
#' sampling rather than their parents (i.e. the beginning point of \code{cycle 0}). By default, equated with the .FS stage. This parameter
#' determines how many dispersal stages individuals have gone through before they are sampled - if \code{.sampling_stage} occurs just
#' \strong{after} \code{.visible_stage}, the sampled individuals will have dispersed through only a small amount of the breeding cycle. if
#' \code{.sampling_stage} occurs just \strong{before} \code{.visible_stage}, the sampled individuals will have dispersed throughout most
#' of the breeding cycle before being sampled. If \code{.cycle} is set to \code{-1}, dispersal stages between breeding & visibility
#' can be accessed.
#'
#' @return Returns an object of class \code{DispersalModel} containing custom lifestages and dispersal, phase & sampling parameters that
#' can be passed to simulation functions.
#' @export
#'
#' @examples
#' antechinus_model <- dispersal_model(pouch = 25, nest = 25, free_living = 250, breeding = 40,
#' gestation = 25, .FS = "nest", .HS = "breeding", .sampling_stage = "nest")
#' antechinus_model
dispersal_model <- function(..., .FS = 0, .HS = .FS, .sampling_stage = 0, .cycle = 0, .breeding_stage = .HS, .visible_stage = .FS){
  xs <- c(...)
  if (is.null(xs)) xs <- c(s1 = 0)
  if (is.null(names(xs))) names(xs) <- paste0("s", c(1:length(xs)))
  names(xs)[names(xs) == ""] <- paste0("s", c(1:length(xs)))[names(xs) == ""]
  if (length(.cycle) > 2){
    stop("'.cycle' vector can have no more than two elements")
  }
  if (length(.cycle) == 1){
    .cycle <- c(.cycle, .cycle)
  }
  if (! isTRUE(all.equal(.cycle, as.integer(.cycle))) | any(.cycle < -1)) stop("'.cycle' vector is not of integers >= -1!")
  ls <- xs; fs <- .FS; hs <- .HS; samp <- .sampling_stage; stages <- names(xs); cycle <-  .cycle
  breeding_stage <- .breeding_stage; visible_stage <- .visible_stage
  if (! fs %in% stages & fs != 0) stop(".FS is not a listed dispersal stage or 0!")
  if (! hs %in% stages & hs != 0) stop(".HS is not a listed dispersal stage or 0!")
  if (! samp %in% stages & samp != 0) stop(".sampling_stage is not a listed dispersal stage or 0!")
  if (! breeding_stage %in% stages & breeding_stage != 0) stop(".breeding_stage is not a listed dispersal stage or 0!")
  if (! visible_stage %in% stages & visible_stage != 0) stop(".visible_stage is not a listed dispersal stage or 0!")
  # reorganize stages to correlate with sampling point.

  if (fs == 0) fs <- stages[1]
  if (hs == 0) hs <- fs
  if (breeding_stage == 0) breeding_stage <- hs
  if (visible_stage == 0) visible_stage <- fs
  if (! samp == 0){
    stages <- c(stages[match(samp, stages):length(stages)],
                       stages[1:match(samp, stages) - 1])
    stages <- c(stages[2:length(stages)], stages[1])
    ls <- ls[stages]
  }
  if (samp == 0) samp <- stages[length(stages)]

    output <- new(
    "DispersalModel",
    stages = stages,
    dispersal_vector = ls,
    fs = fs,
    hs = hs,
    sampling_stage = samp,
    cycle = cycle,
    breeding_stage = breeding_stage,
    visible_stage = visible_stage
  )
  return(output)
}

rebase_stages <- function(stages, newbase){
  # reorders stages objects in vector so that newbase is at start, while preserving cyclic order
  newstages <- c(stages[match(newbase, stages):length(stages)],
              stages[1:match(newbase, stages) - 1])
  return(newstages)
}

get_stages_nonvisible <- function(x){
  if (! is.DispersalModel(x)) stop("Object x is not of class DispersalModel!")
  temp <- rebase_stages(stages(x), breeding_stage(x))
  temp1 <- temp[1:match(visible_stage(x), temp) - 1]
  return(temp1)
}

get_stages_predispersal <- function(x, phase = "FS"){
  if (! is.DispersalModel(x)) stop("Object x is not of class DispersalModel!")
  if (! phase %in% c("FS", "HS")) stop("Phase is not one of 'FS' or 'HS'!")
  temp <- rebase_stages(stages(x), breeding_stage(x))
  if (phase == "FS") temp1 <- temp[1:match(fs(x), temp) - 1]
  else if (phase == "HS") temp1 <- temp[1:match(hs(x), temp) - 1]
  return(temp1)
}

get_stages_sample2phase <- function(x, phase = "FS"){
  if (! is.DispersalModel(x)) stop("Object x is not of class DispersalModel!")
  if (! phase %in% c("FS", "HS")) stop("Phase is not one of 'FS' or 'HS'!")
  temp <- rebase_stages(stages(x), sampling_stage(x))
  if (phase == "FS") temp1 <- temp[1:match(fs(x), temp) - 1]
  else if (phase == "HS") temp1 <- temp[1:match(hs(x), temp) - 1]
  return(temp1)
}

stagediff <- function(stages, start = names(stages)[1], end = names(stages)[length(stages)], inclusive = TRUE){
  temp <- rebase_stages(names(stages), start)
  if (inclusive) temp1 <- temp[1:match(end, temp)] # inclusive
  else temp1 <- temp[1:match(end, temp) - 1] # noninclusive
  return(stages[temp1])
}


