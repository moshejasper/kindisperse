methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' KinPairSimulation Class
#'
#' @slot kinship character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @slot simtype character.
#' @slot kerneltype character. - 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)
#' @slot posigma numeric.       - overall value of dispersal sigma (for simple kernel)
#' @slot initsigma numeric.    - value of pre-breeding dispersal sigma (for composite kernel)
#' @slot breedsigma numeric.  - value of breeding dispersal sigma (for composite kernel)
#' @slot gravsigma numeric.   - value of post-breeding dispersal sigma (for composite kernel)
#' @slot ovisigma numeric.    - value of oviposition dispersal sigma (for composite kernel)
#' @slot simdims numeric.        - dimensions of sampling area (assumes 1 side of square)
#' @slot lifestage character. - lifestage at sampling - either 'immature' or 'ovipositional'
#' @slot kernelshape numeric.   - shape parameter if vgamma kerneltype
#' @slot call call.           - call to create initial simulation
#' @slot tab tbl_df.          - tibble of simulation values
#' @slot filtertype character. - whether the initial sim has been further filtered
#' @slot upper numeric.       - FILTER: upper threshold used
#' @slot lower numeric.       - FILTER: lower threshold used
#' @slot spacing numeric.     - FILTER: spacing used
#' @slot samplenum numeric.   - FILTER: sample number used
#' @slot sampledims numeric.  - FILTER: dimensions used
#'
#' @return returns object of class \code{KinPairSimulation}
#'
#' @export
#'
KinPairSimulation <- setClass("KinPairSimulation",
  slots = list(
    kinship = "character", simtype = "character", kerneltype = "character",
    posigma = "numeric", initsigma = "numeric", breedsigma = "numeric",
    gravsigma = "numeric", ovisigma = "numeric", simdims = "numeric",
    lifestage = "character", kernelshape = "numeric", call = "call", tab = "tbl_df",
    filtertype = "character", upper = "numeric", lower = "numeric",
    spacing = "numeric", samplenum = "numeric", sampledims = "numeric"
  ),
  contains = "KinPairData"
)





############ Generics ################



#' kindisperse - access kerneltype of KinPairSimulation object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the shape parameter used in kernel simulation (if \code{kerneltype} is vgamma)
#' @export
#'
setGeneric("kernelshape", function(x) standardGeneric("kernelshape"))

#' kindisperse - access simtype of KinPairSimulation object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the kind of simulation stored in the object (simple or composite)
#' @export
#'
setGeneric("simtype", function(x) standardGeneric("simtype"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return returns a modified object of relevant class
#' @export
#'
setGeneric("simtype<-", function(x, value) standardGeneric("simtype<-"))
#' kindisperse - access kerneltype of KinPairSimulation object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} the type of statistical kernel used to run the simulation (Gaussian, Laplace, vgamma)
#' @export
#'
setGeneric("kerneltype", function(x) standardGeneric("kerneltype"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return returns a modified object of the relevant class with altered kerneltype parameter
#' @export
#'
setGeneric("kerneltype<-", function(x, value) standardGeneric("kerneltype<-"))
#' kindisperse - access sigmas of KinPairSimulation objects
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} posigma value of simple simulation
#' @export
#'
#'
setGeneric("posigma", function(x) standardGeneric("posigma"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return returns a modified object of the relevant class
#' @export
#'
setGeneric("posigma<-", function(x, value) standardGeneric("posigma<-"))
#' @rdname posigma
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} initsigma value of composite simulation
#' @export
#'
setGeneric("initsigma", function(x) standardGeneric("initsigma"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("initsigma<-", function(x, value) standardGeneric("initsigma<-"))
#' @rdname posigma
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} breedsigma value of composite simulation
#' @export
#'
setGeneric("breedsigma", function(x) standardGeneric("breedsigma"))
#' Title
#'
#' @param x object with relevant method
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("breedsigma<-", function(x, value) standardGeneric("breedsigma<-"))
#' @rdname posigma
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} gravsigma value of composite simulation
#' @export
#'
setGeneric("gravsigma", function(x) standardGeneric("gravsigma"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("gravsigma<-", function(x, value) standardGeneric("gravsigma<-"))
#' @rdname posigma
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} ovisigma value of composite simulation
#' @export
#'
setGeneric("ovisigma", function(x) standardGeneric("ovisigma"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("ovisigma<-", function(x, value) standardGeneric("ovisigma<-"))
#' Access simulation dimensions of KinPairSimulation object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric vector} dimensions of simulated object
#' @export
#'
setGeneric("simdims", function(x) standardGeneric("simdims"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("simdims<-", function(x, value) standardGeneric("simdims<-"))
#' Access filter type of KinPairSimulation object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{character} filter status of simulation
#' @export
#'
setGeneric("filtertype", function(x) standardGeneric("filtertype"))
#' Title
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("filtertype<-", function(x, value) standardGeneric("filtertype<-"))
#' Access & filter by filter parameters of KinPairSimulation Object
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} upper value of sampled object
#' @export
#'
setGeneric("upper", function(x) standardGeneric("upper"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}d
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("upper<-", function(x, value) standardGeneric("upper<-"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} lower value of sampled object
#' @export
#'
setGeneric("lower", function(x) standardGeneric("lower"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#'
#' @export
#'
setGeneric("lower<-", function(x, value) standardGeneric("lower<-"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} trap spacing of sampled object
#' @export
#'
setGeneric("spacing", function(x) standardGeneric("spacing"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("spacing<-", function(x, value) standardGeneric("spacing<-"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric} number of kin dyads in sampled object
#' @export
#'
setGeneric("samplenum", function(x) standardGeneric("samplenum"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("samplenum<-", function(x, value) standardGeneric("samplenum<-"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @return \code{numeric vector} dimensions of sampled object
#' @export
#'
setGeneric("sampledims", function(x) standardGeneric("sampledims"))
#' @rdname upper
#'
#' @param x object of class \code{KinPairSimulation}
#' @param value new value to assign
#' @return  returns a modified object of the relevant class
#' @export
#'
setGeneric("sampledims<-", function(x, value) standardGeneric("sampledims<-"))


####################### Methods ####################################


#'
#'
#' @param KinPairSimulation
#'
#' @return \code{character} the shape parameter used in kernel simulation (if \code{kerneltype} is vgamma)
#'
#' @export
#'
#' @describeIn KinPairSimulation access kernelshape
setMethod("kernelshape", "KinPairSimulation", function(x) x@kernelshape)

#'
#'
#' @param KinPairSimulation object of class KinPairSimulation
#'
#' @return \code{character} the kind of simulation stored in the object (simple or composite)
#'
#' @export
#' @describeIn KinPairSimulation access simulation type
setMethod("simtype", "KinPairSimulation", function(x) x@simtype)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{character} the type of statistical kernel used to run the simulation (Gaussian, Laplace, vgamma)
#'
#' @export
#' @describeIn KinPairSimulation access kerneltype
setMethod("kerneltype", "KinPairSimulation", function(x) x@kerneltype)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} posigma value of simple simulation
#'
#' @export
#' @describeIn KinPairSimulation access sigma
setMethod("posigma", "KinPairSimulation", function(x) x@posigma)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} initsigma value of composite simulation
#'
#' @export
#' @describeIn KinPairSimulation access initsigma
setMethod("initsigma", "KinPairSimulation", function(x) x@initsigma)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} breedsigma value of composite simulation
#'
#' @export
#' @describeIn KinPairSimulation access breedsigma
setMethod("breedsigma", "KinPairSimulation", function(x) x@breedsigma)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} gravsigma value of composite simulation
#'
#' @export
#' @describeIn KinPairSimulation access gravsigma
setMethod("gravsigma", "KinPairSimulation", function(x) x@gravsigma)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} ovisigma value of composite simulation
#'
#' @export
#' @describeIn KinPairSimulation access ovisigma
setMethod("ovisigma", "KinPairSimulation", function(x) x@ovisigma)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric vector} simulation dimensions of \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access simdims
setMethod("simdims", "KinPairSimulation", function(x) x@simdims)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{character} filter status of \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access filtertype
setMethod("filtertype", "KinPairSimulation", function(x) x@filtertype)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} upper value of sampled \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access upper filter distance
setMethod("upper", "KinPairSimulation", function(x) x@upper)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} lower value of sampled \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access lower filter distance
setMethod("lower", "KinPairSimulation", function(x) x@lower)
#' )
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} trap spacing value of sampled \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access spacing
setMethod("spacing", "KinPairSimulation", function(x) x@spacing)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric} number of kin dyads in \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access sampled samplenum
setMethod("samplenum", "KinPairSimulation", function(x) x@samplenum)
#'
#'
#' @param KinPairSimulation
#'
#' @return \code{numeric vector} sampling dimensions of \code{KinPairSimulation} object
#'
#' @export
#' @describeIn KinPairSimulation access sampled dimensions
setMethod("sampledims", "KinPairSimulation", function(x) x@sampledims)

#'
#'
#' @param KinPairSimulation
#'
#' @return returns a modified object of class \code{KinPairSimulation}
#'
#' @export
#' @describeIn KinPairSimulation assign and filter by upper distance (uses sample_kindist())
setMethod("upper<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@upper)) {
    if (x@upper < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, upper = value)
})
#'
#'
#' @param KinPairSimulation
#'
#' @return returns a modified object of class \code{KinPairSimulation}
#'
#' @export
#' @describeIn KinPairSimulation assign and filter by lower distance (uses sample_kindist())
setMethod("lower<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@lower)) {
    if (x@lower < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, lower = value)
})
#'
#'
#' @param KinPairSimulation
#'
#' @return returns a modified object of class \code{KinPairSimulation}
#'
#' @export
#' @describeIn KinPairSimulation assign kin spacing (uses sample_kindist())
setMethod("spacing<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@spacing)) {
    warning("Can't apply spacing twice. Skipped")
    return(x)
  }
  sample_kindist(x, spacing = value)
})
#'
#'
#' @param KinPairSimulation
#'
#' @return returns a modified object of class \code{KinPairSimulation}
#'
#' @export
#' @describeIn KinPairSimulation assign and downsample to samplenum (uses sample_kindist())
setMethod("samplenum<-", "KinPairSimulation", function(x, value) {
  if (!is.na(x@samplenum)) {
    if (x@samplenum < value) {
      warning("Redundant. Skipped.")
      return(x)
    }
  }
  sample_kindist(x, n = value)
})
#'
#'
#' @param KinPairSimulation object of class KinPairSimulation
#' @param x object of class KinPairSimulation
#' @param value value for parameter to be adjusted to
#'
#' @return returns a modified object of class \code{KinPairSimulation}
#'
#' @export
#' @describeIn KinPairSimulation assign and filter by sample dimensions (uses sample_kindist())
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

#'
#'
#' @param KinPairSimulation object of class KinPairSimulation
#' @param object object of class KinPairSimulation
#'
#' @return No return value, called for side effects
#'
#' @export
#' @describeIn KinPairSimulation print method
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


#'
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
#' @param simdims numeric. - dimensions of sampling area (assumes one side of square)
#' @param kernelshape numeric. - value of kernel shape of simulation (if using kernel with shape parameter e.g. vgamma)
#' @param call call. Call to create object
#' @param filtertype character. whether the initial sim has been further filtered
#' @param upper numeric.       - FILTER: upper threshold used
#' @param lower numeric.       - FILTER: lower threshold used
#' @param spacing numeric.       - FILTER: spacing used
#' @param samplenum numeric.       - FILTER: sample number used
#' @param sampledims numeric.       - FILTER: sample dimensions used
#'
#' @return Returns an object of class \code{KinPairSimulation}
#' @export
#' @describeIn KinPairSimulation initialisation method
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
           simdims = NULL,
           call = NULL,
           filtertype = NULL,
           upper = NULL,
           lower = NULL,
           spacing = NULL,
           samplenum = NULL,
           sampledims = NULL) {
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
    if (!is.null(simdims)) .Object@simdims <- simdims else .Object@simdims <- NA_real_
    if (!is.null(call)) .Object@call <- call else .Object@call <- sys.call()
    if (!is.null(filtertype)) .Object@filtertype <- filtertype else .Object@filtertype <- NA_character_
    if (!is.null(upper)) .Object@upper <- upper else .Object@upper <- NA_real_
    if (!is.null(lower)) .Object@lower <- lower else .Object@lower <- NA_real_
    if (!is.null(spacing)) .Object@spacing <- spacing else .Object@spacing <- NA_real_
    if (!is.null(samplenum)) .Object@samplenum <- samplenum else .Object@samplenum <- NA_real_
    if (!is.null(sampledims)) .Object@sampledims <- sampledims else .Object@sampledims <- NA_real_

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

#'
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
#' @param simdims numeric. - dimensions of sampling area (assumes one side of square)
#' @param kernelshape numeric. - value of kernel shape of simulation (if using kernel with shape parameter e.g. vgamma)
#' @param call call. Call to create object
#' @param filtertype character. whether the initial sim has been further filtered
#' @param upper numeric.       - FILTER: upper threshold used
#' @param lower numeric.       - FILTER: lower threshold used
#' @param spacing numeric.       - FILTER: spacing used
#' @param samplenum numeric.       - FILTER: sample number used
#' @param sampledims numeric.       - FILTER: sample dimensions used
#'
#' @return returns an object of class \code{KinPairSimulation}.
#' @export
#'
#' @examples
#' KinPairSimulation()
KinPairSimulation <- function(data = NULL,
                              kinship = NULL,
                              lifestage = NULL,
                              simtype = NULL,
                              kerneltype = NULL,
                              posigma = NULL,
                              initsigma = NULL,
                              breedsigma = NULL,
                              gravsigma = NULL,
                              ovisigma = NULL,
                              simdims = NULL,
                              kernelshape = NULL,
                              call = NULL,
                              filtertype = NULL,
                              upper = NULL,
                              lower = NULL,
                              spacing = NULL,
                              samplenum = NULL,
                              sampledims = NULL) {
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
    simdims = simdims,
    kernelshape = kernelshape,
    call = call,
    filtertype = filtertype,
    upper = upper,
    lower = lower,
    spacing = spacing,
    samplenum = samplenum,
    sampledims = sampledims
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
                                     simdims = NULL, lifestage = NULL, kernelshape = NULL, call = NULL) {
  if (is.null(call)) {
    call <- sys.call()
  }
  return(KinPairSimulation(data = data, kinship = kinship, simtype = "simple", kerneltype = kerneltype,
                           posigma = posigma, simdims = simdims, lifestage = lifestage, kernelshape = kernelshape, call = call))
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
                                        gravsigma = NULL, ovisigma = NULL, simdims = NULL, lifestage = NULL, kernelshape = NULL, call = NULL) {
  if (is.null(call)) {
    call <- sys.call()
  }
  return(KinPairSimulation(
    data = data, kinship = kinship, simtype = "composite", kerneltype = kerneltype, initsigma = initsigma, breedsigma = breedsigma,
    gravsigma = gravsigma, ovisigma = ovisigma, simdims = simdims, lifestage = lifestage, kernelshape = kernelshape, call = call
  ))
}
