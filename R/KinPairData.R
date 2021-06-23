methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' Formal class "KinPairData"
#'
#' @description The class \code{KinPairData} is a formal (S4) class for storing kinship and lifespan dispersal information concerning kin pairs.
#' @slot kinship character - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @slot lifestage character - lifestage at sampling - either 'immature', 'ovipositional' or a stage
#' corresponding to a \code{DispersalModel} custom stage
#' @slot cycle non-negative integer or vector of two such integers - Represents
#' the number of complete breeding cycles each  individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). If the first individual
#' was sampled as a juvenile & the second as an adult of equivalent stage, the vector c(0, 1) would be used. In most situations, the default will be appropriate
#' @slot tab tbl_df. - tibble of dispersal values
#' @return returns object of class \code{KinPairData}
#' @export
#'
#'
KinPairData <- setClass("KinPairData",
  slots = list(kinship = "character", lifestage = "character", cycle = "numeric", tab = "tbl_df")
)


######### GENERICS and METHODS#############


#' Access or assign kin category (generic for KinPairData class)
#'
#' @param x object with relevant method
#'
#' @return \code{character}. Kinship category of object
#' @export
#'
setGeneric("kinship", function(x) standardGeneric("kinship"))
#'
#' @rdname kinship
#' @param x object with relevant method
#' @param value new value to assign
#'
#' @return returns modified object
#' @export
#'
#'
setGeneric("kinship<-", function(x, value) standardGeneric("kinship<-"))
#' Access or assign lifestage (generic for KinPairData class)
#'
#' @param x object with relevant method
#' @return \code{character} life stage of object
#' @export
#'
#'
setGeneric("lifestage", function(x) standardGeneric("lifestage"))
#'
#' @rdname lifestage
#' @param x object with relevant method
#' @param value new value to assign
#'
#' @return returns modified object with altered lifestage
#' @export
#'
#'
setGeneric("lifestage<-", function(x, value) standardGeneric("lifestage<-"))

#' Access distances (generic for KinPairData class)
#'
#' @param x Object of Class KinPairData
#'
#' @return Returns a numeric vector of kin separation distances
#' @export
#'
#'
setGeneric("distances", function(x) standardGeneric("distances"))




#'
#'
#' @param KinPairData object of class \code{KinPairData}
#' @param x object of class KinPairData
#'
#' @return numeric vector of kin separation distances
#' @export
#'
#' @describeIn KinPairData access distances
setMethod("distances", "KinPairData", function(x) x@tab$distance)

#'
#'
#' @param KinPairData object of class \code{KinPairData}
#'
#' @return \code{character} kinship of \code{KinPairData} object
#' @export
#'
#' @describeIn KinPairData access kin category
setMethod("kinship", "KinPairData", function(x) x@kinship)

#'
#'
#' @param KinPairData object of class \code{KinPairData}
#' @param x object of class \code{KinPairData}
#' @param value value to assign to slot
#'
#'@return modified object of class \code{KinPairData}
#'
#' @export
#'
#' @describeIn KinPairData assign kin category
setMethod("kinship<-", "KinPairData", function(x, value) {
  x@kinship <- value
  validObject(x)
  x
})


#'
#'
#' @param KinPairData
#'
#' @return \code{character} lifestage of \code{KinPairData} object
#'
#' @export
#'
#' @describeIn KinPairData access lifestage
setMethod("lifestage", "KinPairData", function(x) x@lifestage)

#'
#'
#'
#' @param KinPairData
#'
#' @return modified object of class \code{KinPairData}
#'
#' @export
#'
#' @describeIn KinPairData assign lifestage
setMethod("lifestage<-", "KinPairData", function(x, value) {
  x@lifestage <- value
  validObject(x)
  x
})

#'
#'
#' @param KinPairData
#'
#' @return \code{numeric} number of complete breeding cycles sampled individuals have progressed through
#' @export
#' @describeIn KinPairData access breeding cycle
setMethod("breeding_cycle", "KinPairData", function(x) x@cycle)

#'
#'
#' @param KinPairData object of class KinPairData
#' @param object an object of class KinpairData
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @describeIn KinPairData standard print method
setMethod(
  "show",
  "KinPairData",
  function(object) {
    cat("KINDISPERSE RECORD OF KIN PAIRS\n")
    cat("-------------------------------\n")
    cat("kinship:\t\t", object@kinship, "\n")
    cat("lifestage:\t\t", object@lifestage, "\n")
    cat("cycle:\t\t\t", object@cycle, "\n")
    cat("\ntab\n")
    print(object@tab)
    cat("-------------------------------")
  }
)

# Constructor method of KinPairData

#' Constructor method for \code{KinPairData} objects.
#'
#' @param KinPairData object of class KinPairData
#' @param .Object the KinPairData object to be constructed
#' @param data  data about kinship to be used to construct object (tibble, data.frame, or numeric vector of distances)
#' @param kinship character. Kinship category value for object. - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, and H2C.
#' @param lifestage character. Lifestage value for object. - one of 'immature', 'ovipositional' or 'unknown'
#' @param cycle non-negative integer or vector of two such integers - Represents
#' the number of complete breeding cycles each simulated individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). If the first individual
#' was sampled as a juvenile & the second as an adult of equivalent stage, the vector c(0, 1) would be used. In most situations, defualt will be appropriate
#' @param ... additional argument to pass to downstream functions in future
#'
#' @export
#'
#' @return Returns an object of class \code{KinPairData}
#'
#' @describeIn KinPairData initialize method
setMethod(
  "initialize", "KinPairData",
  function(.Object,
           data = NULL,
           kinship = NULL,
           lifestage = NULL,
           cycle = NULL,
           ...) {
    if (!is.null(kinship)) {
      .Object@kinship <- kinship
    }
    else {
      .Object@kinship <- "UN"
    }
    if (!is.null(lifestage)) {
      .Object@lifestage <- lifestage
    }
    else {
      .Object@lifestage <- "unknown"
    }
    if (!is.null(cycle)) .Object@cycle <- cycle else .Object@cycle <- 0
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
          message("Note: numeric vector interpreted as kin distances")
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

#' Make new KinPairData object
#'
#' @param data tlb_df. Tibble of kinpair distances
#' @param kinship character. - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, H2C & UN.
#' @param lifestage character. - one of 'unknown', 'immature' or 'ovipositional', or alternatively a custom
#' stage that corresponds to a dispersal stage contained in a \code{DispersalModel} object.
#' @param cycle non-negative integer of length one or two (here, 1 is equivalent to c(1, 1)). Represents
#' the number of complete breeding cycles each individual has undergone before the sampling point, where the time between
#' birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). If the first individual
#' was sampled as a juvenile & the second as an adult of equivalent stage, the vector c(0, 1) would be used.
#' In most situations, the default will be appropriate
#'
#' @return returns an object of class \code{KinPairData}
#' @export
#'
#' @examples
#' KinPairData()
KinPairData <- function(data = NULL, kinship = NULL, lifestage = NULL, cycle = NULL) {
  new("KinPairData", data = data, kinship = kinship, lifestage = lifestage)
}


setValidity("KinPairData", function(object) {
  if (!object@kinship %in% c("UN", "PO", "GG", "GGG", "FS", "AV", "GAV", "1C", "1C1", "2C", "HS", "HAV", "HGAV", "H1C", "H1C1", "H2C")) {
    "@kinship must be one of UN PO GG GGG FS AV GAV 1C 1C1 2C HS HAV HGAV H1C H1C1 H2C"
  }
  #else if (!object@lifestage %in% c("unknown", "immature", "ovipositional")) {
  #  "@lifestage must currently be set to 'unknown', 'immature', or 'ovipositional'"
  #}
  else {
    TRUE
  }
})

#' Check if object is of class KinPairData
#'
#' @param x object to be checked
#'
#' @return Returns TRUE if of class \code{KinPairData}, FALSE if not.
#' @export
#'
#'
is.KinPairData <- function(x) {
  "KinPairData" %in% is(x)
}
