methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' DispersalModel Class
#'
#' @slot dispersal_vector numeric. Named vector of custom breeding cycle stages and their corresponding axial dispersal values
#' @slot stages character.  Ordered vector of all dispersal stages across the breeding cycle of the modelled species
#' @slot fs character.  breeding cycle stage at which first substantial FS-phased dispersal occurs
#' @slot hs character. breeding cycle stage at which first substantial HS-phased dispersal occurs
#' @slot sampling_stage character. stage in the breeding cycle at which samples are to be collected for kin identification.
#' @slot cycle non-negative integer. Breeding cycle numbers of dispersed kin to be modeled.  Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0)
#'
#' @return returns object of class \code{DispersalModel}
#' @export
#'
DispersalModel <- setClass("DispersalModel",
                           slots = list(
                             dispersal_vector = "numeric", stages = "character",
                             fs = "character", hs = "character", sampling_stage = "character",
                             cycle = "numeric"
                           ))


########### Generics ##############

#' kindisperse - access dispersal vector of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{numeric vector} named vector of custom lifestages & associated dispersal sigmas.
#' @export
#'
setGeneric("dispersal_vector", function(x) standardGeneric("dispersal_vector"))

#' kindisperse - access breeding cycle stages of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} ordered vector of custom lifestages contained in the object
#' @export
#'
setGeneric("stages", function(x) standardGeneric("stages"))

#' kindisperse - access FS phase split point of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} FS phase split
#' @export
#'
setGeneric("fs", function(x) standardGeneric("fs"))

#' kindisperse - access HS phase split point of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} HS phase split
#' @export
#'
setGeneric("hs", function(x) standardGeneric("hs"))

#' kindisperse - access sampling stage of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} sampling stage
#' @export
#'
setGeneric("sampling_stage", function(x) standardGeneric("sampling_stage"))

#' kindisperse - access breeding cycle at sampling of DispersalModel object.
#'
#' @param x object of class \code{DispersalModel} or \code{KinPairData}
#'
#' @return \code{non-negative integer(s)} Breeding cycle numbers of modelled dispersed kin. Represents the number of complete
#' breeding cycles each indivdiual has undergone before the sampling point, wher ethe time between birth and first
#' reproduction is coded as \code{0}, that between first and second reproduction \code{1}, etc.
#' @export
#'
setGeneric("breeding_cycle", function(x) standardGeneric("breeding_cycle"))


############ Methods ##############


#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{numeric vector} named vector of custom lifestages & associated dispersal sigmas.
#' @export
#'
#'
#' @describeIn DispersalModel access dispersal vector
setMethod("dispersal_vector", "DispersalModel", function(x) x@dispersal_vector)

#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} ordered vector of custom lifestages contained in the object
#' @export
#'
#'
#' @describeIn DispersalModel access breeding cycle stages
setMethod("stages", "DispersalModel", function(x) x@stages)

#'
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} FS phase split
#' @export
#'
#'
#' @describeIn DispersalModel access FS phase split
setMethod("fs", "DispersalModel", function(x) x@fs)

#'
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} HS phase split
#' @export
#'
#'
#' @describeIn DispersalModel access HS phase split
setMethod("hs", "DispersalModel", function(x) x@hs)

#'
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{character} sampling stage
#' @export
#'
#'
#' @describeIn DispersalModel access sampling stage
setMethod("sampling_stage", "DispersalModel", function(x) x@sampling_stage)

#'
#'
#' @param DispersalModel object of class \code{DispersalModel}
#' @param x object of class \code{DispersalModel}
#'
#' @return \code{non-negative integer(s)} Breeding cycle numbers of modelled dispersed kin. Represents the number of complete
#' breeding cycles each indivdiual has undergone before the sampling point, wher ethe time between birth and first
#' reproduction is coded as \code{0}, that between first and second reproduction \code{1}, etc.
#' @export
#'
#' @describeIn DispersalModel access breeding cycle at sampling
setMethod("breeding_cycle", "DispersalModel", function(x) x@cycle)

#'
#' @param DispersalModel an object of class \code{DispersalModel}
#' @param object object of class \code{DispersalModel}
#'
#' @return No return value. Called for side effects
#' @export
#'
#'
#' @describeIn DispersalModel print method
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

#'
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
#' @describeIn DispersalModel initialization method
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

