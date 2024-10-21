methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' KinPairSimulation Class
#'
#' The class \code{KinPairSimulation} is a formal (S4) class for storing kinship and dispersal
#' distribution information derived from simulations in the \code{kindisperse} package.
#' It is derived from the \code{\link{KinPairData}} class.
#' The \code{KinPairSimulation} class is used to store information about the spatial distribution of kin dyads for use
#' in calculating axial sigmas of intergenerational dispersal as initially implemented in Jasper et al. 2019
#' (\doi{https://doi.org/10.1111/1755-0998.13043}).
#'
#' This class is essentially wrapped around the \code{tbl_df} class but with (a) expectations around certain columns
#' that must be present (\code{id1, id2, kinship}, & \code{distance} - three 'character' & one 'numeric' column), as
#' well as (b) additional attributes (\code{kinship}, \code{lifestage}, & \code{cycle}) characterizing the close-kin
#' dyads being stored.These attributes, as well as the embedded vector of distances, can be accessed with the methods
#' \code{\link{kinship}}, \code{\link{lifestage}}, \code{\link{breeding_cycle}} and \code{\link{distances}}.
#' In addition to the above attributes (derived from the \code{KinPairData} class), this class contains attributes
#' capturing the simulation type & parameters used to generate the final distribution of kin dyads.
#'
#' Objects from this class are returned from the \code{\link{simulate_kindist_composite}},
#' \code{\link{simulate_kindist_simple}} and \code{\link{simulate_kindist_custom}} functions
#' (& related), and are directly constructed with the namesake \code{KinPairSimulation()} function.
#' They can be passed to the \code{\link{sample_kindist}} function for filtering and subsampling, and to
#' axial functions (including \code{\link{axials_standard}} and \code{\link{axpermute_standard}}) for estimation of
#' axial dispersal.
#' @slot kinship character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @slot simtype character. - one of 'simple', 'composite' or 'custom'
#' @slot kerneltype character. - 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @slot posigma numeric.       - overall value of dispersal sigma (for simple kernel)
#' @slot initsigma numeric.    - value of pre-breeding dispersal sigma (for composite kernel)
#' @slot breedsigma numeric.  - value of breeding dispersal sigma (for composite kernel)
#' @slot gravsigma numeric.   - value of post-breeding dispersal sigma (for composite kernel)
#' @slot ovisigma numeric.    - value of oviposition dispersal sigma (for composite kernel)
#' @slot customsigma numeric  - vector of named custom dispersal sigmas (for custom kernel)
#' @slot simdims numeric.        - dimensions of sampling area (assumes 1 side of square)
#' @slot lifestage character. - lifestage at sampling - either 'immature' or 'ovipositional'
#' @slot cycle integer - number of breeding cycles sampled individuals have survived (for custom kernel)
#' @slot kernelshape numeric.   - shape parameter if vgamma kerneltype
#' @slot call call.           - call to create initial simulation
#' @slot tab tbl_df.          - tibble of simulation values
#' @slot filtertype character. - whether the initial sim has been further filtered
#' @slot upper numeric.       - FILTER: upper threshold used
#' @slot lower numeric.       - FILTER: lower threshold used
#' @slot spacing numeric.     - FILTER: spacing used
#' @slot samplenum numeric.   - FILTER: sample number used
#' @slot sampledims numeric.  - FILTER: dimensions used
#' @slot model \code{DispersalModel} - model of dispersal used to create object (with custom type)
#'
#' @return returns object of class \code{KinPairSimulation}
#'
#' @export
#' @family kdclasses
#' @include DispersalModel.R dispersal_model.R
#'
KinPairSimulation <- setClass("KinPairSimulation",
  slots = list(
    kinship = "character", simtype = "character", kerneltype = "character",
    posigma = "numeric", initsigma = "numeric", breedsigma = "numeric",
    gravsigma = "numeric", ovisigma = "numeric", customsigma = "numeric", simdims = "numeric",
    lifestage = "character", cycle = "numeric", kernelshape = "numeric", call = "call", tab = "tbl_df",
    filtertype = "character", upper = "numeric", lower = "numeric",
    spacing = "numeric", samplenum = "numeric", sampledims = "numeric", model = "DispersalModel"
  ),
  contains = "KinPairData"
)





############ Generics ################



#' Access kernel type of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the shape parameter used in kernel simulation (if \code{kerneltype} is vgamma)
#' @export
#' @family kpsmethods
#'
setGeneric("kernelshape", function(x) standardGeneric("kernelshape"))

#' Access or assign simulation type of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the kind of simulation stored in the object (simple or composite)
#' @export
#' @family kpsmethods
#'
setGeneric("simtype", function(x) standardGeneric("simtype"))
#' @rdname simtype
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return returns a modified object of relevant class
#' @export
#'
setGeneric("simtype<-", function(x, value) standardGeneric("simtype<-"))
#' Access or assign kerneltype of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the type of statistical kernel used to run the simulation (Gaussian, Laplace, vgamma)
#' @export
#' @family kpsmethods
#'
setGeneric("kerneltype", function(x) standardGeneric("kerneltype"))
#' @rdname kerneltype
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return returns a modified object of the relevant class with altered kerneltype parameter
#' @export
#'
setGeneric("kerneltype<-", function(x, value) standardGeneric("kerneltype<-"))
#' Access or assign dispersal sigmas of \code{\link{KinPairSimulation}} objects
#'
#' These generics & methods work with \code{KinPairSimulation} objects to access & modify information about the dispersal
#' sigma parameters that define the stored simulation. The \code{posigma()} method accesses the single dispersal parameter
#' stored in a simulation with \code{simtype == "simple"}. The remaining parameters access the dispersal parameters stored
#' in a simulation with \code{simtype == "composite"}. The dispersal kernel sigma parameters of \code{simtype == "custom"}
#' simulations are not yet implemented here. Assignment operations currently only exist as generics (they are not yet applied
#' to the \code{KinPairSimulation} class).
#' @name access_sigmas
#' @aliases posigma
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} value of specified sigma parameter or modified \code{KinPairSimulation} object
#' @export
#' @family kpsmethods
#'
#'
setGeneric("posigma", function(x) standardGeneric("posigma"))
#' @rdname access_sigmas
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("posigma<-", function(x, value) standardGeneric("posigma<-"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("initsigma", function(x) standardGeneric("initsigma"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("initsigma<-", function(x, value) standardGeneric("initsigma<-"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("breedsigma", function(x) standardGeneric("breedsigma"))
#' @rdname access_sigmas
#'
#' @param x object with relevant method
#' @param value new value to assign
#' @export
#'
setGeneric("breedsigma<-", function(x, value) standardGeneric("breedsigma<-"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("gravsigma", function(x) standardGeneric("gravsigma"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("gravsigma<-", function(x, value) standardGeneric("gravsigma<-"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("ovisigma", function(x) standardGeneric("ovisigma"))
#' @rdname access_sigmas
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("ovisigma<-", function(x, value) standardGeneric("ovisigma<-"))
#' Access simulation dimensions of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric vector} dimensions of simulated object
#' @export
#'
setGeneric("simdims", function(x) standardGeneric("simdims"))
#' @rdname simdims
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("simdims<-", function(x, value) standardGeneric("simdims<-"))
#' Access filtertype of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} filter status of simulation
#' @export
#'
setGeneric("filtertype", function(x) standardGeneric("filtertype"))
#' @rdname filtertype
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("filtertype<-", function(x, value) standardGeneric("filtertype<-"))


#' Access dispersal model of \code{\link{KinPairSimulation}} object
#'
#' @param x object of class \code{KinPairSimulation}
#'
#' @return returns an object of class \code{\link{DispersalModel}}
#' @export
#'
setGeneric("get_dispersal_model", function(x) standardGeneric("get_dispersal_model"))

###############################################
#' @name filter_methods
#' @aliases upper
#' @title Access or modify the \code{filter} parameters of \code{\link{KinPairSimulation}} objects
#'
#' @description These generics & methods work as an interface between \code{KinPairSimulation} objects and the
#' \code{\link{sample_kindist}} function. They either retrieve the value of pre-existing filter steps that have
#' been applied to the object (e.g. \code{upper(x)}) or assign such a filtering parameter to the \code{KinPairSimulation}
#' object (e.g. \code{sampledims(x) <- value}). In this case, the method passes the \code{KinPairSimulation} object to the
#' \code{sample_kindist()} function for subsampling or filtering, then updates the sampling parameter before returning the
#' modified object. Note that while the \code{sample_kindist} function can take \code{\link{KinPairData}} objects, the methods
#' described here are only applicable to objects of class \code{KinPairSimulation}.
#'
#' @param x object of class \code{KinPairSimulation}
#' @return either the accessed \code{numeric} filter parameter or a filtered \code{\link{KinPairSimulation}} object
#' @export
#' @family kpsmethods
#'
setGeneric("upper", function(x) standardGeneric("upper"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("upper<-", function(x, value) standardGeneric("upper<-"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("lower", function(x) standardGeneric("lower"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#'
#' @export
#'
setGeneric("lower<-", function(x, value) standardGeneric("lower<-"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("spacing", function(x) standardGeneric("spacing"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("spacing<-", function(x, value) standardGeneric("spacing<-"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("samplenum", function(x) standardGeneric("samplenum"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("samplenum<-", function(x, value) standardGeneric("samplenum<-"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @export
#'
setGeneric("sampledims", function(x) standardGeneric("sampledims"))
#' @rdname filter_methods
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @export
#'
setGeneric("sampledims<-", function(x, value) standardGeneric("sampledims<-"))


####################### Methods ####################################


#' @describeIn kernelshape
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{character} the shape parameter used in kernel simulation (if \code{kerneltype} is vgamma)
#'
#' @export
#'
setMethod("kernelshape", "KinPairSimulation", function(x) x@kernelshape)

#' @describeIn simtype
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{character} the kind of simulation stored in the object (simple or composite)
#'
#' @export
setMethod("simtype", "KinPairSimulation", function(x) x@simtype)

#' @describeIn kerneltype
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{character} the type of statistical kernel used to run the simulation (Gaussian, Laplace, vgamma)
#'
#' @export
setMethod("kerneltype", "KinPairSimulation", function(x) x@kerneltype)

#' @describeIn access_sigmas
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @export
setMethod("posigma", "KinPairSimulation", function(x) x@posigma)

#' @describeIn access_sigmas
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @export
setMethod("initsigma", "KinPairSimulation", function(x) x@initsigma)

#' @describeIn access_sigmas
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @export
setMethod("breedsigma", "KinPairSimulation", function(x) x@breedsigma)

#' @describeIn access_sigmas
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @export
setMethod("gravsigma", "KinPairSimulation", function(x) x@gravsigma)

#' @describeIn access_sigmas
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @export
setMethod("ovisigma", "KinPairSimulation", function(x) x@ovisigma)

#' @describeIn simdims

#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{numeric vector} simulation dimensions of \code{KinPairSimulation} object
#'
#' @export
setMethod("simdims", "KinPairSimulation", function(x) x@simdims)

#' @describeIn filtertype
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{character} filter status of \code{KinPairSimulation} object
#'
#' @export
setMethod("filtertype", "KinPairSimulation", function(x) x@filtertype)

#' @describeIn get_dispersal_model
#'
#' @param KinPairSimulation object of class \code{KinPairSimulation}
#'
#' @export
#'
setMethod("get_dispersal_model", "KinPairSimulation", function(x) x@model)




#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("upper", "KinPairSimulation", function(x) x@upper)

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("lower", "KinPairSimulation", function(x) x@lower)

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("spacing", "KinPairSimulation", function(x) x@spacing)

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("samplenum", "KinPairSimulation", function(x) x@samplenum)

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("sampledims", "KinPairSimulation", function(x) x@sampledims)

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("upper<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@upper)) {
    if (x@upper < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, upper = value)
})

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("lower<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@lower)) {
    if (x@lower < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, lower = value)
})

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#'
#' @export
setMethod("spacing<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@spacing)) {
    warning("Can't apply spacing twice. Skipped")
    return(x)
  }
  sample_kindist(x, spacing = value)
})

#' @describeIn filter_methods
#'
#' @param KinPairSimulation
#'
#'
#' @export
setMethod("samplenum<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@samplenum)) {
    if (x@samplenum < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, n = value)
})

#' @describeIn filter_methods
#'
#' @param KinPairSimulation object of class KinPairSimulation
#' @param x object of class KinPairSimulation
#' @param value value for parameter to be adjusted to
#'
#'
#' @export
setMethod("sampledims<-", "KinPairSimulation", function(x, value) {
  if (length(value) == 1){
    value <- c(value, value)
  }
  if (!is.na(x@sampledims[1])) {
    if (length(x@sampledims) == 1) {x@sampledims <- c(x@sampledims, x@sampledims)}
    if (x@sampledims[1] < value[1] | x@sampledims[2] < value[2]) {
      warning("Prior sampledim value was smaller. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, dims = value)
})

#' @describeIn KinPairSimulation print method
#'
#' @param KinPairSimulation object of class KinPairSimulation
#' @param object object of class KinPairSimulation
#'
#' @return No return value. Called for side effects
#'
#' @export
setMethod(
  "show",
  "KinPairSimulation",
  function(object) {
    cat("KINDISPERSE SIMULATION of KIN PAIRS\n")
    cat("-----------------------------------\n")
    cat("simtype:\t\t", object@simtype, "\n")
    cat("kerneltype:\t\t", object@kerneltype, "\n")
    if (! is.na(object@kernelshape))
      cat("kernelshape:\t\t", object@kernelshape, "\n")
    cat("kinship:\t\t", object@kinship, "\n")
    cat("simdims:\t\t", signif(object@simdims, 3), "\n")
    if (is.na(object@simtype)) {
      cat("")
    }
    else if (object@simtype == "simple") {
      cat("posigma:\t\t", object@posigma, "\n")
    }
    else if (object@simtype == "composite") {
      cat("initsigma\t\t", object@initsigma, "\nbreedsigma\t\t", object@breedsigma, "\ngravsigma\t\t", object@gravsigma, "\novisigma\t\t", object@ovisigma, "\n")
    }
    else if (object@simtype == "custom") {
      for (sig in 1:length(object@customsigma)){
        cat(names(object@customsigma)[sig], "\t\t\t", object@customsigma[sig], "\n")
      }
      cat("cycle:\t\t\t", object@cycle, "\n")
    }
    cat("lifestage:\t\t", object@lifestage, "\n\n")
    if (!is.na(object@filtertype)) {
      if (object@filtertype == "filtered") {
        cat("FILTERED\n")
        cat("--------\n")
        if (!is.na(object@upper)) {
          cat("upper:\t\t\t", object@upper, "\n")
        }
        if (!is.na(object@lower)) {
          cat("lower:\t\t\t", object@lower, "\n")
        }
        if (!is.na(object@spacing)) {
          cat("spacing:\t\t", object@spacing, "\n")
        }
        if (!is.na(object@samplenum)) {
          cat("samplenum:\t\t", object@samplenum, "\n")
        }
        if (!is.na(object@sampledims[1])) {
          cat("sampledims:\t\t", signif(object@sampledims, 3), "\n")
        }
        cat("\n")
      }
    }
    cat("tab\n")
    print(object@tab)
    cat("-----------------------------------")
  }
)


#' @describeIn KinPairSimulation initialisation method
#'
#' @param KinPairSimulation an object of class KinPairSimulation
#' @param .Object object to be constructed into KinPairSimulation class
#' @param data tbl_df. tibble  of simulation values
#' @param kinship character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @param lifestage character - one of 'unknown', 'immature' or 'ovipositional'
#' @param simtype character - simulation type
#' @param kerneltype character. - 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @param posigma numeric - overall value of dispersal sigma (for simple kernel)
#' @param initsigma numeric.    - value of pre-breeding dispersal sigma (for composite kernel)
#' @param breedsigma numeric.    - value of breeding dispersal sigma (for composite kernel)
#' @param gravsigma numeric.    - value of post-breeding dispersal sigma (for composite kernel)
#' @param ovisigma numeric.    - value of oviposition dispersal sigma (for composite kernel)
#' @param customsigma numeric.  - vector of named custom dispersal sigmas (for custom kernel)
#' @param simdims numeric. - dimensions of sampling area (assumes one side of square)
#' @param cycle integer - number of breeding cycles sampled individual has survived (for custom kernel)
#' @param kernelshape numeric. - value of kernel shape of simulation (if using kernel with shape parameter e.g. vgamma)
#' @param call call. Call to create object
#' @param filtertype character. whether the initial sim has been further filtered
#' @param upper numeric.       - FILTER: upper threshold used
#' @param lower numeric.       - FILTER: lower threshold used
#' @param spacing numeric.       - FILTER: spacing used
#' @param samplenum numeric.       - FILTER: sample number used
#' @param sampledims numeric.       - FILTER: sample dimensions used
#' @param model list - model information if custom simulation used to generate object
#'
#' @return Returns an object of class \code{KinPairSimulation}
#' @export
setMethod(
  "initialize", "KinPairSimulation",
  function(.Object,
           data = NULL,
           kinship = NULL,
           lifestage = NULL,
           simtype = NULL,
           kerneltype = NULL,
           kernelshape = NULL,
           posigma = NULL,
           initsigma = NULL,
           breedsigma = NULL,
           gravsigma = NULL,
           ovisigma = NULL,
           customsigma = NULL,
           cycle = NULL,
           simdims = NULL,
           call = NULL,
           filtertype = NULL,
           upper = NULL,
           lower = NULL,
           spacing = NULL,
           samplenum = NULL,
           sampledims = NULL,
           model = NULL) {
    if (!is.null(kinship)) .Object@kinship <- kinship else .Object@kinship <- "UN"
    if (!is.null(lifestage)) .Object@lifestage <- lifestage else .Object@lifestage <- "unknown"
    if (!is.null(simtype)) .Object@simtype <- simtype else .Object@simtype <- NA_character_
    if (!is.null(kerneltype)) .Object@kerneltype <- kerneltype else .Object@kerneltype <- NA_character_
    if (!is.null(kernelshape)) .Object@kernelshape <- kernelshape else .Object@kernelshape <- NA_real_
    if (!is.null(posigma)) .Object@posigma <- posigma else .Object@posigma <- NA_real_
    if (!is.null(initsigma)) .Object@initsigma <- initsigma else .Object@initsigma <- NA_real_
    if (!is.null(breedsigma)) .Object@breedsigma <- breedsigma else .Object@breedsigma <- NA_real_
    if (!is.null(gravsigma)) .Object@gravsigma <- gravsigma else .Object@gravsigma <- NA_real_
    if (!is.null(ovisigma)) .Object@ovisigma <- ovisigma else .Object@ovisigma <- NA_real_
    if (!is.null(customsigma)) .Object@customsigma <- customsigma else .Object@customsigma <- NA_real_
    if (!is.null(cycle)) .Object@cycle <- cycle else .Object@cycle <- NA_real_
    if (!is.null(simdims)) .Object@simdims <- simdims else .Object@simdims <- NA_real_
    if (!is.null(call)) .Object@call <- call else .Object@call <- sys.call()
    if (!is.null(filtertype)) .Object@filtertype <- filtertype else .Object@filtertype <- NA_character_
    if (!is.null(upper)) .Object@upper <- upper else .Object@upper <- NA_real_
    if (!is.null(lower)) .Object@lower <- lower else .Object@lower <- NA_real_
    if (!is.null(spacing)) .Object@spacing <- spacing else .Object@spacing <- NA_real_
    if (!is.null(samplenum)) .Object@samplenum <- samplenum else .Object@samplenum <- NA_real_
    if (!is.null(sampledims)) .Object@sampledims <- sampledims else .Object@sampledims <- NA_real_
    if (! is.null(model)) .Object@model <- model else .Object@model <- dispersal_model()

    if (!is.null(data)) {
      if (is.data.frame(data) & !is_tibble(data)) {
        data <- as_tibble(data)
      }
      if (is_tibble(data)) {
        if (ncol(data) == 1) {
          data <- data[[1]]
        }
      }
      if (is_tibble(data)) {
        if (!"distance" %in% colnames(data)) {
          if (!("x1" %in% colnames(data) & "y1" %in% colnames(data) & "x2" %in% colnames(data) & "y2" %in% colnames(data))) {
            stop("Unable to determine kin distances!")
          }
          else {
            data <- mutate(data, distance = sqrt((.data$x1 - .data$x2)^2 + (.data$y1 - .data$y2)^2))
          }
        }
        if (!"kinship" %in% colnames(data)) {
          data <- mutate(data, kinship = .Object@kinship)
        }
        if (!"id1" %in% colnames(data)) {
          data <- add_column(data, id1 = paste0(1:nrow(data), "a"))
        }
        if (!"id2" %in% colnames(data)) {
          data <- add_column(data, id2 = paste0(1:nrow(data), "b"))
        }
        data <- select(data, .data$id1, .data$id2, .data$kinship, .data$distance, everything())
        .Object@tab <- data
      }
      else { # check if just distances included
        if (is.numeric(data)) {
          message("Note: numeric vector interpreted as kin distances!")
          data <- tibble(id1 = paste0(1:length(data), "a"), id2 = paste0(1:length(data), "b"), kinship = .Object@kinship, distance = data)
          .Object@tab <- data
        }
      }
    }
    else {
      .Object@tab <- tibble(id1 = "a", id2 = "b", kinship = "UN", distance = 0, .rows = 0)
    }
    validObject(.Object)
    return(.Object)
  }
)

#' KinPairSimulation
#'
#' @param data tbl_df. tibble  of simulation values
#' @param kinship character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @param lifestage character - one of 'unknown', 'immature' or 'ovipositional'
#' @param simtype character - simulation type
#' @param kerneltype character. - 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @param posigma numeric - overall value of dispersal sigma (for simple kernel)
#' @param initsigma numeric.    - value of pre-breeding dispersal sigma (for composite kernel)
#' @param breedsigma numeric.    - value of breeding dispersal sigma (for composite kernel)
#' @param gravsigma numeric.    - value of post-breeding dispersal sigma (for composite kernel)
#' @param ovisigma numeric.    - value of oviposition dispersal sigma (for composite kernel)
#' @param customsigma numeric. - vector of named custom dispersal sigmas (for custom kernel)
#' @param simdims numeric. - dimensions of sampling area (assumes one side of square)
#' @param kernelshape numeric. - value of kernel shape of simulation (if using kernel with shape parameter e.g. vgamma)
#' @param cycle integer - number of breeding cycles sampled individual has survived (for custom kernel)
#' @param call call. Call to create object
#' @param filtertype character. whether the initial sim has been further filtered
#' @param upper numeric.       - FILTER: upper threshold used
#' @param lower numeric.       - FILTER: lower threshold used
#' @param spacing numeric.       - FILTER: spacing used
#' @param samplenum numeric.       - FILTER: sample number used
#' @param sampledims numeric.       - FILTER: sample dimensions used
#' @param model list - model information if custom simulation used to generate object
#'
#' @return returns an object of class \code{KinPairSimulation}.
#' @export
#'
#' @examples
#' kin_pair_simulation()
kin_pair_simulation <- function(data = NULL,
                              kinship = NULL,
                              lifestage = NULL,
                              simtype = NULL,
                              kerneltype = NULL,
                              posigma = NULL,
                              initsigma = NULL,
                              breedsigma = NULL,
                              gravsigma = NULL,
                              ovisigma = NULL,
                              customsigma = NULL,
                              simdims = NULL,
                              kernelshape = NULL,
                              cycle = NULL,
                              call = NULL,
                              filtertype = NULL,
                              upper = NULL,
                              lower = NULL,
                              spacing = NULL,
                              samplenum = NULL,
                              sampledims = NULL,
                              model = NULL) {
  new("KinPairSimulation",
    data = data,
    kinship = kinship,
    lifestage = lifestage,
    simtype = simtype,
    kerneltype = kerneltype,
    posigma = posigma,
    initsigma = initsigma,
    breedsigma = breedsigma,
    gravsigma = gravsigma,
    ovisigma = ovisigma,
    customsigma = customsigma,
    simdims = simdims,
    kernelshape = kernelshape,
    cycle = cycle,
    call = call,
    filtertype = filtertype,
    upper = upper,
    lower = lower,
    spacing = spacing,
    samplenum = samplenum,
    sampledims = sampledims,
    model = model
  )
}

#' Check if object is of class KinPairSimulation
#'
#' @param x object to be checked
#'
#' @return Returns TRUE if of class KinPairSimulation, FALSE if not
#' @export
#'
is.KinPairSimulation <- function(x) {
  "KinPairSimulation" %in% is(x)
}

#' Constructor for KinPairSimulation Class (simple)
#'
#' @param data tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), kinship (chr) and sims (dbl)
#' @param kinship  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian", "Laplace" or "vgamma" (variance-gamma).
#' @param posigma numeric. Axial sigma of dispersal kernel (axial standard deviation).
#' @param simdims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Either "immature" (sampled at hatching) or "ovipositional"
#' (sampled as an adult during oviposition - essentially one lifespan later than 'immature')
#' @param kernelshape numeric. Value of shape parameter for simulated kernel if kernel requires one (e.g. vgamma kernel).
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#' @param model DispersalModel - model information passed from simulation function
#'
#' @return Returns a \code{KinPairSimulation} Class object with simtype set to 'simple' and relevant fields included.
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(
#'   id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'   distance = c(50, 45, 65), kinship = c("1C", "1C", "1C")
#' )
#' KinPairSimulation_simple(kindata,
#'   kinship = "1C", kerneltype = "Gaussian",
#'   posigma = 38, lifestage = "immature"
#' )
KinPairSimulation_simple <- function(data = NULL, kinship = NULL, kerneltype = NULL, posigma = NULL,
                                     simdims = NULL, lifestage = NULL, kernelshape = NULL, call = NULL, model = NULL) {
  if (is.null(call)) {
    call <- sys.call()
  }
  return(kin_pair_simulation(data = data, kinship = kinship, simtype = "simple", kerneltype = kerneltype,
                           posigma = posigma, simdims = simdims, lifestage = lifestage, kernelshape = kernelshape, cycle = 0, call = call,
                           model = model))
}


#' Constructor for KinPairSimulation Class (composite)
#'
#' @param data tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), kinship (chr) and sims (dbl)
#' @param kinship  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian", "Laplace" or "vgamma" (variance-gamma).
#' @param initsigma  numeric. Axial sigma of prebreeding ('juvenile') dispersal kernel (axial standard deviation).
#' @param breedsigma  numeric. Axial sigma of breeding dispersal kernel (axial standard deviation).
#' @param gravsigma numeric. Axial sigma of post-breeding ('gravid') dispersal kernel (axial standard deviation).
#' @param ovisigma  numeric. Axial sigma of oviposition dispersal kernel (axial standard deviation).
#' @param simdims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Either "immature" (sampled at hatching) or "ovipositional"
#' (sampled as an adult during oviposition - essentially one lifespan later than 'immature')
#' @param kernelshape numeric. Value of shape parameter for simulated kernel if kernel requires one (e.g. vgamma kernel).
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#' @param model DispersalModel - model information passed from simulation function
#'
#' @return Returns a \code{KinPairSimulation} Class object with simtype set to 'composite' and relevant fields included.
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(
#'   id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'   distance = c(50, 45, 65), kinship = c("1C", "1C", "1C")
#' )
#' KinPairSimulation_composite(kindata,
#'   kinship = "1C", kerneltype = "Gaussian",
#'   initsigma = 15, breedsigma = 25, gravsigma = 20, ovisigma = 10, lifestage = "immature"
#' )
KinPairSimulation_composite <- function(data = NULL, kinship = NULL, kerneltype = NULL, initsigma = NULL, breedsigma = NULL,
                                        gravsigma = NULL, ovisigma = NULL, simdims = NULL, lifestage = NULL, kernelshape = NULL, call = NULL,
                                        model = NULL) {
  if (is.null(call)) {
    call <- sys.call()
  }
  return(kin_pair_simulation(
    data = data, kinship = kinship, simtype = "composite", kerneltype = kerneltype, initsigma = initsigma, breedsigma = breedsigma,
    gravsigma = gravsigma, ovisigma = ovisigma, simdims = simdims, lifestage = lifestage, kernelshape = kernelshape, cycle = 0, call = call,
    model = model
  ))
}


#' Constructor for KinPairSimulation Class (custom)
#'
#' @param data tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), kinship (chr) and sims (dbl)
#' @param kinship  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian", "Laplace" or "vgamma" (variance-gamma).
#' @param customsigma numeric. Named vector of custom breeding cycle stages and their corresponding axial dispersal values
#' @param simdims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Here, must correspond to a custom lifestage derived from 'customsigma'
#' @param kernelshape numeric. Value of shape parameter for simulated kernel if kernel requires one (e.g. vgamma kernel).
#' @param cycle non-negative integer. Breeding cycle numbers of dispersed kin to be modeled.  Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0)
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#' @param model DispersalModel - model information passed from simulation function
#'
#' @return Returns a \code{KinPairSimulation} Class object with simtype set to 'custom' and relevant fields included.
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(
#'   id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'   distance = c(50, 45, 65), kinship = c("1C", "1C", "1C")
#' )
#' KinPairSimulation_custom(kindata,
#'   kinship = "1C", kerneltype = "Gaussian",
#'   customsigma = c(initsigma = 15, breedsigma = 25, gravsigma = 20, ovisigma = 10),
#'   lifestage = "ovisigma", cycle = 0
#' )
KinPairSimulation_custom <- function(data = NULL, kinship = NULL, kerneltype = NULL, customsigma = NULL,
                                     simdims = NULL, lifestage = NULL, kernelshape = NULL, cycle = NULL,
                                     call = NULL, model = NULL) {
  if (is.null(call)) {
    call <- sys.call()
  }
  return(kin_pair_simulation(data = data, kinship = kinship, simtype = "custom", kerneltype = kerneltype,
                           customsigma = customsigma, simdims = simdims, lifestage = lifestage,
                           kernelshape = kernelshape, cycle = cycle, call = call, model = model))
}
