methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' KinPairSimulation
#'
#' @slot category character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @slot simtype character.
#' @slot kerneltype character. - 'Gaussian' or 'Laplace'
#' @slot sigma numeric.       - overall value of dispersal sigma (for simple kernel)
#' @slot juvsigma numeric.    - value of pre-breeding dispersal sigma (for composite kernel)
#' @slot breedsigma numeric.  - value of breeding dispersal sigma (for composite kernel)
#' @slot gravsigma numeric.   - value of post-breeding dispersal sigma (for composite kernel)
#' @slot ovisigma numeric.    - value of oviposition dispersal sigma (for composite kernel)
#' @slot dims numeric.        - dimensions of sampling area (assumes 1 side of square)
#' @slot lifestage character. - lifestage at sampling - either 'larva' or 'oviposition'
#' @slot call call.           - call to create initial simulation
#' @slot tab tbl_df.          - tibble of simulation values
#' @slot filtertype character. - whether the initial sim has been further filtered
#' @slot upper numeric.       - FILTER: upper threshold used
#' @slot lower numeric.       - FILTER: lower threshold used
#' @slot spacing numeric.     - FILTER: spacing used
#' @slot samplenum numeric.   - FILTER: sample number used
#' @slot sampledims numeric.  - FILTER: dimensions used
#'
#' @return Returns an object of class 'KinPairSimulation'
#' @export
#'
KinPairSimulation <- setClass("KinPairSimulation",
         slots = list(category = "character", simtype = "character", kerneltype = "character",
                      sigma = "numeric", juvsigma = "numeric", breedsigma = "numeric",
                      gravsigma = "numeric", ovisigma = "numeric", dims = "numeric",
                      lifestage = "character", call = "call", tab = "tbl_df",
                      filtertype = "character", upper = "numeric", lower = "numeric",
                      spacing = "numeric", samplenum = "numeric", sampledims = "numeric"),
         contains = "KinPairData")

setGeneric("simtype", function(x) standardGeneric("simtype"))
setGeneric("simtype<-", function(x, value) standardGeneric("simtype<-"))
setGeneric("kerneltype", function(x) standardGeneric("kerneltype"))
setGeneric("kerneltype<-", function(x, value) standardGeneric("kerneltype<-"))
setGeneric("sigma", function(x) standardGeneric("sigma"))
setGeneric("sigma<-", function(x, value) standardGeneric("sigma<-"))
setGeneric("juvsigma", function(x) standardGeneric("juvsigma"))
setGeneric("juvsigma<-", function(x, value) standardGeneric("juvsigma<-"))
setGeneric("breedsigma", function(x) standardGeneric("breedsigma"))
setGeneric("breedsigma<-", function(x, value) standardGeneric("breedsigma<-"))
setGeneric("gravsigma", function(x) standardGeneric("gravsigma"))
setGeneric("gravsigma<-", function(x, value) standardGeneric("gravsigma<-"))
setGeneric("ovisigma", function(x) standardGeneric("ovisigma"))
setGeneric("ovisigma<-", function(x, value) standardGeneric("ovisigma<-"))
setGeneric("simdims", function(x) standardGeneric("simdims"))
setGeneric("simdims<-", function(x, value) standardGeneric("simdims<-"))
setGeneric("filtertype", function(x) standardGeneric("filtertype"))
setGeneric("filtertype<-", function(x, value) standardGeneric("filtertype<-"))
setGeneric("upper", function(x) standardGeneric("upper"))
setGeneric("upper<-", function(x, value) standardGeneric("upper<-"))
setGeneric("lower", function(x) standardGeneric("lower"))
setGeneric("lower<-", function(x, value) standardGeneric("lower<-"))
setGeneric("spacing", function(x) standardGeneric("spacing"))
setGeneric("spacing<-", function(x, value) standardGeneric("spacing<-"))
setGeneric("samplenum", function(x) standardGeneric("samplenum"))
setGeneric("samplenum<-", function(x, value) standardGeneric("samplenum<-"))
setGeneric("sampledims", function(x) standardGeneric("sampledims"))
setGeneric("sampledims<-", function(x, value) standardGeneric("sampledims<-"))

#' Access simtype (KinPairSimulation)
#' @name simtype
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("simtype", "KinPairSimulation", function(x) x@simtype)
#' Access kerneltype (KinPairSimulation method)
#' @name kerneltype
#' @param KinPairSimulation
#' @name kerneltype
#' @return
#' @export
#'
#' @examples
setMethod("kerneltype", "KinPairSimulation", function(x) x@kerneltype)
#' Access sigma (KinPairSimulation method)
#' @name sigma
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("sigma", "KinPairSimulation", function(x) x@sigma)
#' Access juvsigma (KinPairSimulation method)
#' @name juvsigma
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("juvsigma", "KinPairSimulation", function(x) x@juvsigma)
#' Access breedsigma (KinPairSimulation method)
#' @name breedsigma
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("breedsigma", "KinPairSimulation", function(x) x@breedsigma)
#' Access gravsigma (KinPairSimulation method)
#' @name gravsigma
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("gravsigma", "KinPairSimulation", function(x) x@gravsigma)
#' Access ovisigma (KinPairSimulation method)
#' @name ovisigma
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("ovisigma", "KinPairSimulation", function(x) x@ovisigma)
#' Access simulation dimensions (KinPairSimulation method)
#' @name simdims
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("simdims", "KinPairSimulation", function(x) x@simdims)
setMethod("filtertype", "KinPairSimulation", function(x) x@filtertype)
#' Access or filter by upper distance (KinPairSimulation method)
#' @name upper
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("upper", "KinPairSimulation", function(x) x@upper)
#' Access or filter by lower distance (KinPairSimulation method)
#' @name lower
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("lower", "KinPairSimulation", function(x) x@lower)
#' Access or alter kin spacing (KinPairSimulation method)
#' @name spacing
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("spacing", "KinPairSimulation", function(x) x@spacing)
#' Access or change sample number (KinPairSimulation method)
#' @name samplenum
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("samplenum", "KinPairSimulation", function(x) x@samplenum)
#' Access or alter sampling dimensions (KinPairSimulation method)
#' @name sampledims
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("sampledims", "KinPairSimulation", function(x) x@sampledims)

#'
#' @rdname upper
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("upper<-", "KinPairSimulation", function(x, value){
  if (! is.na(x@upper)){
    if (x@upper < value) {
      cat("Redundant. Skipped.\n")
      return(x)
    }
  }
  sample_kindist(x, upper = value)
})
#'
#' @rdname lower
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("lower<-", "KinPairSimulation", function(x, value){
  if (! is.na(x@lower)){
    if (x@lower < value) {
      cat("Redundant. Skipped.\n")
      return(x)
    }
  }
  sample_kindist(x, lower = value)
})
#'
#' @rdname spacing
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("spacing<-", "KinPairSimulation", function(x, value){
  if (! is.na(x@spacing)){
    cat("Can't apply spacing twice. Skipped")
    return(x)
  }
  sample_kindist(x, spacing = value)
})
#'
#' @rdname samplenum
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("samplenum<-", "KinPairSimulation", function(x, value){
  if (! is.na(x@samplenum)){
    if (x@samplenum < value) {
      cat("Redundant. Skipped.\n")
      return(x)
    }
  }
  sample_kindist(x, n = value)
})
#'
#' @rdname sampledims
#' @param KinPairSimulation
#'
#' @return
#' @export
#'
#' @examples
setMethod("sampledims<-", "KinPairSimulation", function(x, value){
  if (! is.na(x@sampledims)){
    if (x@sampledims < value) {
      cat("Redundant. Skipped.\n")
      return(x)
    }
  }
  sample_kindist(x, dims = value)
})

setMethod(
  "show",
  "KinPairSimulation",
  function(object){
    cat("KINDISPERSE SIMULATION of KIN PAIRS\n")
    cat("-----------------------------------\n")
    cat("simtype:\t\t", object@simtype, "\n")
    cat("kerneltype:\t\t", object@kerneltype, "\n")
    cat('category:\t\t', object@category, '\n')
    cat('dims:\t\t\t', object@dims, '\n')
    if (is.na(object@simtype)) {cat('')}
    else if  (object@simtype == "simple"){
      cat('sigma:\t\t\t', object@sigma, '\n')
    }
    else if (object@simtype == "composite"){
      cat('juvsigma\t\t', object@juvsigma, '\nbreedsigma\t\t', object@breedsigma, '\ngravsigma\t\t', object@gravsigma, '\novisigma\t\t', object@ovisigma, '\n')
    }
    cat('lifestage:\t\t', object@lifestage, '\n\n')
    if (! is.na(object@filtertype)) { if (object@filtertype == "filtered") {
      cat('FILTERED\n')
      cat('--------\n')
      if (! is.na(object@upper)) {cat('upper:\t\t\t', object@upper, '\n')}
      if (! is.na(object@lower)) {cat('lower:\t\t\t', object@lower, '\n')}
      if (! is.na(object@spacing)) {cat('spacing:\t\t', object@spacing, '\n')}
      if (! is.na(object@samplenum)) {cat('samplenum:\t\t', object@samplenum, '\n')}
      if (! is.na(object@sampledims)) {cat('sampledims:\t\t', object@sampledims, '\n')}
      cat('\n')
    }}
    cat('tab\n')
    print(object@tab)
    cat("-----------------------------------")
  }
)


setMethod("initialize", "KinPairSimulation",
          function(.Object,
                   data = NULL,
                   category = NULL,
                   lifestage = NULL,
                   simtype = NULL,
                   kerneltype = NULL,
                   sigma = NULL,
                   juvsigma = NULL,
                   breedsigma = NULL,
                   gravsigma = NULL,
                   ovisigma = NULL,
                   dims = NULL,
                   call = NULL,
                   filtertype = NULL,
                   upper = NULL,
                   lower = NULL,
                   spacing = NULL,
                   samplenum = NULL,
                   sampledims = NULL){
            if (! is.null(category)) .Object@category <- category else .Object@category <- "UN"
            if (! is.null(lifestage)) .Object@lifestage <- lifestage else .Object@lifestage <- "unknown"
            if (! is.null(simtype)) .Object@simtype <- simtype else .Object@simtype <- NA_character_
            if (! is.null(kerneltype)) .Object@kerneltype <- kerneltype else .Object@kerneltype <- NA_character_
            if (! is.null(sigma)) .Object@sigma <- sigma else .Object@sigma <- NA_real_
            if (! is.null(juvsigma)) .Object@juvsigma <- juvsigma else .Object@juvsigma <- NA_real_
            if (! is.null(breedsigma)) .Object@breedsigma <- breedsigma else .Object@breedsigma <- NA_real_
            if (! is.null(gravsigma)) .Object@gravsigma <- gravsigma else .Object@gravsigma <- NA_real_
            if (! is.null(ovisigma)) .Object@ovisigma <- ovisigma else .Object@ovisigma <- NA_real_
            if (! is.null(dims)) .Object@dims <- dims else .Object@dims <- NA_real_
            if (! is.null(call)) .Object@call <- call else .Object@call <- sys.call()
            if (! is.null(filtertype)) .Object@filtertype <- filtertype else .Object@filtertype <- NA_character_
            if (! is.null(upper)) .Object@upper <- upper else .Object@upper <- NA_real_
            if (! is.null(lower)) .Object@lower <- lower else .Object@lower <- NA_real_
            if (! is.null(spacing)) .Object@spacing <- spacing else .Object@spacing <- NA_real_
            if (! is.null(samplenum)) .Object@samplenum <- sigma else .Object@samplenum <- NA_real_
            if (! is.null(sampledims)) .Object@sampledims <- sigma else .Object@sampledims <- NA_real_

            if (! is.null(data)){
              if (is.data.frame(data) & ! is_tibble(data)){
                data <- as_tibble(data)
              }
              if (is_tibble(data)){
                if (ncol(data) == 1){
                  data <- data[[1]]
                }
              }
              if (is_tibble(data)){
                if (!"distance" %in% colnames(data)) {
                  if (! ("x1" %in% colnames(data) & "y1" %in% colnames(data) & "x2" %in% colnames(data) & "y2" %in% colnames(data))){
                    stop("Unable to determine kin distances!")
                  }
                  else {data <- mutate(data, distance = sqrt((.data$x1 - .data$x2)^2 + (.data$y1 - .data$y2)^2))}
                }
                if (! "category" %in% colnames(data)){
                  data <- mutate(data, category = .Object@category)
                }
                if (! "id1" %in% colnames(data)){
                  data <- add_column(data, id1 = paste0(1:nrow(data), "a"))
                }
                if (! "id2" %in% colnames(data)){
                  data <- add_column(data, id2 = paste0(1:nrow(data), "b"))
                }
                data <- select(data, .data$id1, .data$id2, .data$category, .data$distance, everything())
                .Object@tab <- data
              }
              else { # check if just distances included
                if (is.numeric(data)) {
                  cat("Note: numeric vector interpreted as kin distances!\n")
                  data <- tibble(id1 = paste0(1:length(data), "a"), id2 = paste0(1:length(data), "b"), category = .Object@category, distance = data)
                  .Object@tab <- data
                }
              }
            }
            else {
              .Object@tab <- tibble(id1 = "a", id2 = "b", category = "UN", distance = 0, .rows = 0)
            }
            validObject(.Object)
            return(.Object)
          })

#' Constructor for KinPairSimulation Class (simple)
#'
#' @param data tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), category (chr) and sims (dbl)
#' @param category  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian" or "Laplace".
#' @param sigma numeric. Axial sigma of dispersal kernel (axial standard deviation).
#' @param dims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Either "larva" (sampled at hatching) or "oviposition" (sampled as an adult during oviposition - essentially one lifespan later that 'larva')
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#'
#' @return Returns a KinPairSimulation Class object with simtype set to 'simple' and relevant fields included.
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'     distance = c(50, 45, 65), category = c("1C", "1C", "1C"))
#' KinPairSimulation_simple(kindata, category = "1C", kerneltype = "Gaussian",
#'     sigma = 38, lifestage = "larva")
KinPairSimulation_simple <- function(data=NULL, category=NULL, kerneltype=NULL, sigma=NULL, dims = NULL, lifestage=NULL, call=NULL){
  if (is.null(call)) {call <- sys.call()}
  return(KinPairSimulation(data=data, category=category, simtype="simple", kerneltype=kerneltype, sigma=sigma, dims = dims, lifestage=lifestage, call=call))
}


#' Constructor for KinPairSimulation Class (composite)
#'
#' @param data tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), category (chr) and sims (dbl)
#' @param category  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian" or "Laplace".
#' @param juvsigma  numeric. Axial sigma of prebreeding ('juvenile') dispersal kernel (axial standard deviation).
#' @param breedsigma  numeric. Axial sigma of breeding dispersal kernel (axial standard deviation).
#' @param gravsigma numeric. Axial sigma of post-breeding ('gravid') dispersal kernel (axial standard deviation).
#' @param ovisigma  numeric. Axial sigma of oviposition dispersal kernel (axial standard deviation).
#' @param dims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Either "larva" (sampled at hatching) or "oviposition" (sampled as an adult during oviposition - essentially one lifespan later that 'larva')
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#'
#' @return Returns a KinPairSimulation Class object with simtype set to 'composite' and relevant fields included.
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'                 distance = c(50, 45, 65), category = c("1C", "1C", "1C"))
#' KinPairSimulation_composite(kindata, category = "1C", kerneltype = "Gaussian",
#'     juvsigma = 15, breedsigma = 25, gravsigma = 20, ovisigma = 10, lifestage = "larva")
KinPairSimulation_composite <- function(data = NULL, category=NULL, kerneltype=NULL, juvsigma=NULL, breedsigma=NULL,
                                             gravsigma=NULL, ovisigma=NULL, dims = NULL, lifestage=NULL, call=NULL){
  if (is.null(call)) {call <- sys.call()}
  return(KinPairSimulation(data=data, category=category, simtype="composite", kerneltype=kerneltype, juvsigma=juvsigma, breedsigma=breedsigma,
                                gravsigma=gravsigma, ovisigma=ovisigma, dims = dims, lifestage=lifestage, call=call))
}
