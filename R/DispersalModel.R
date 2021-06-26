methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' DispersalModel Class
#'
#' The class \code{DispersalModel} is an S4 Class supplying organism-specific information about dispersal stages (with axial
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
#' ecologies & sampling schemes: the \code{DispersalModel} class paired with the \code{\link{simulate_kindist_custom}}
#' function achieves this by defining a breeding cycle with an arbitrary number of dispersal phases (the \code{dispersal_vector}
#' slot, accessed by the \code{\link{dispersal_vector}} method).
#'
#' The breeding structure of a species may also impact at which stage
#' FS and HS phase branches occur. In \emph{Ae. aegypti}, males mate with multiple females in a (single) breeding season, and a female
#' typically carried the egg of only one male. In this context the FS (full-sibling) phase would be set to correspond to the female's
#' oviposition dispersal, while the HS (half-sibling) phase would be set to correspond to the male's breeding dispersal (as its gametes
#' will then be dispersed by multiple females across their gravid & ovipositional phases). However, in e.g. some species of the marsupial
#' \emph{Antechinus}, the FS branch point would be more appropriately associated with juveniles at the time that they leave the mother's
#' pouch. The \code{\link{fs}} and \code{\link{hs}} slots & accessor functions enable the assignment of these phase branches to any
#' defined life phase. Similarly, the \code{\link{sampling_stage}} slot & method allow the sampling point to be set to correspond to any
#' phase of the defined breeding cycle.
#'
#' The final parameter stored in this object is the breeding cycle number \code{cycle}, accessed by the \code{\link{breeding_cycle}} method.
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
#' @slot dispersal_vector numeric. Named vector of custom breeding cycle stages and their corresponding axial dispersal values
#' @slot stages character.  Ordered vector of all dispersal stages across the breeding cycle of the modeled species
#' @slot fs character.  breeding cycle stage at which first substantial FS-phased dispersal occurs
#' @slot hs character. breeding cycle stage at which first substantial HS-phased dispersal occurs
#' @slot sampling_stage character. stage in the breeding cycle at which samples are to be collected for kin identification.
#' @slot cycle non-negative integer. Breeding cycle numbers of dispersed kin to be modeled.  Represents
#' the number of complete breeding cycles each individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0)
#'
#' @return returns object of class \code{DispersalModel}
#' @export
#' @family kdclasses
#'
DispersalModel <- setClass("DispersalModel",
                           slots = list(
                             dispersal_vector = "numeric", stages = "character",
                             fs = "character", hs = "character", sampling_stage = "character",
                             cycle = "numeric"
                           ))


########### Generics ##############

#' Access dispersal vector of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{numeric vector} named vector of custom lifestages & associated dispersal sigmas.
#' @export
#'
setGeneric("dispersal_vector", function(x) standardGeneric("dispersal_vector"))

#' Access breeding cycle stages of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} ordered vector of custom lifestages contained in the object
#' @export
#'
setGeneric("stages", function(x) standardGeneric("stages"))

#' Access FS phase split point of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} FS phase split
#' @export
#'
setGeneric("fs", function(x) standardGeneric("fs"))

#' Access HS phase split point of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} HS phase split
#' @export
#'
setGeneric("hs", function(x) standardGeneric("hs"))

#' Access sampling stage of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} sampling stage
#' @export
#'
setGeneric("sampling_stage", function(x) standardGeneric("sampling_stage"))

#' Access breeding cycle at sampling of \code{\link{DispersalModel}} object.
#'
#' @param x object of class \code{DispersalModel} or \code{KinPairData}
#'
#' @return \code{non-negative integer(s)} Breeding cycle numbers of modelled dispersed kin. Represents the number of complete
#' breeding cycles each indivdiual has undergone before the sampling point, where the time between birth and first
#' reproduction is coded as \code{0}, that between first and second reproduction \code{1}, etc.
#' @export
#'
setGeneric("breeding_cycle", function(x) standardGeneric("breeding_cycle"))


############ Methods ##############


#' @describeIn dispersal_vector
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
setMethod("dispersal_vector", "DispersalModel", function(x) x@dispersal_vector)

#' @describeIn stages
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
#'
setMethod("stages", "DispersalModel", function(x) x@stages)

#' @describeIn fs
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
#'
setMethod("fs", "DispersalModel", function(x) x@fs)

#' @describeIn hs
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
#'
setMethod("hs", "DispersalModel", function(x) x@hs)

#' @describeIn sampling_stage
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
#'
setMethod("sampling_stage", "DispersalModel", function(x) x@sampling_stage)

#' @describeIn breeding_cycle
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @export
#'
setMethod("breeding_cycle", "DispersalModel", function(x) x@cycle)

#' @describeIn DispersalModel print method
#' @param DispersalModel an object of class \code{DispersalModel}
#' @param object object of class \code{DispersalModel}
#'
#' @return No return value. Called for side effects
#' @export
#'
#'
setMethod(
  "show",
  "DispersalModel",
  function(object) {
    cat("KINDISPERSE INTERGENERATIONAL DISPERSAL MODEL\n")
    cat("---------------------------------------------\n")
    cat("stage:\t\t", paste(object@stages, collapse = "\t"), "\n")
    cat("dispersal:\t", paste(object@dispersal_vector, collapse = "\t"), "\n\n")
    cat("FS branch:\t", object@fs, "\n")
    cat("HS branch:\t", object@hs, "\n")
    cat("sampling stage:\t", object@sampling_stage, "\n")
    cat("cycle:\t\t", object@cycle, "\n")
    cat("---------------------------------------------\n")
  }
)

#' @describeIn DispersalModel initialization method
#'
#' @param DispersalModel an object of class DispersalModel
#' @param .Object object to be constructed into DispersalModel class
#' @param stages character.  Ordered vector of all dispersal stages across the breeding cycle of the modeled species
#' @param dispersal_vector numeric. Named vector of custom breeding cycle stages and their corresponding axial dispersal values
#' @param fs character.  breeding cycle stage at which first substantial FS-phased dispersal occurs
#' @param hs character. breeding cycle stage at which first substantial HS-phased dispersal occurs
#' @param sampling_stage character. stage in the breeding cycle at which samples are to be collected for kin identification.
#' @param cycle non-negative integer. Breeding cycle numbers of dispersed kin to be modeled.  Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0)
#'
#' @return returns an object of class \code{DispersalModel}
#' @export
setMethod(
  "initialize", "DispersalModel",
  function(
    .Object,
    stages = NULL,
    dispersal_vector = NULL,
    fs = NULL,
    hs = NULL,
    sampling_stage = NULL,
    cycle = NULL
  ) {
    if (! is.null(stages)) .Object@stages <- stages else .Object@stages <- NULL
    if (! is.null(dispersal_vector)) .Object@dispersal_vector <- dispersal_vector else .Object@dispersal_vector <- NULL
    if (! is.null(fs)) .Object@fs <- fs else .Object@fs <- NULL
    if (! is.null(hs)) .Object@hs <- hs else .Object@hs <- NULL
    if (! is.null(sampling_stage)) .Object@sampling_stage <- sampling_stage else .Object@sampling_stage <- NULL
    if (! is.null(cycle)) .Object@cycle <- cycle else .Object@cycle <- NULL
    validObject(.Object)
    return(.Object)
  }
)

#' Check if object is of class \code{DispersalModel}
#'
#' @param x object to be checked
#'
#' @return returns TRUE if of class \code{DispersalModel}, FALSE if not
#' @export
#'
is.DispersalModel <- function(x) {
  "DispersalModel" %in% is(x)
}

