methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

#' Formal class "KinPairData"
#'
#' @description The class \code{KinPairData} is a formal (S4) class for storing kinship and lifespan dispersal information concerning kin pairs.
#' @slot category character.
#' @slot lifestage character.
#' @slot tab tbl_df.
#' @return
#' @export
#'
#'
KinPairData <- setClass("KinPairData",
                        slots = list(category = "character", lifestage = "character", tab = "tbl_df"))


######### GENERICS and METHODS#############

#' Extract KinPairData to tibble (generic)
#'
#' @param x object of class \code{KinPairData}
#'
#' @return tibble (class \code{tbl_df})
#' @export
#'
#'
setGeneric("to_tibble", function(x) standardGeneric("to_tibble"))

#'
#'
#' @param KinPairData
#'
#' @return
#' @export
#'
#' @describeIn KinPairData extract to tibble
setMethod("to_tibble", "KinPairData", function(x) x@tab)

#' Access or assign kin category (generic for KinPairData class)
#'
#' @param x object with relevant method
#'
#' @return \code{character}. Kinship category of object
#' @export
#'
setGeneric("category", function(x) standardGeneric("category"))
#'
#' @rdname category
#' @param x object with relevant method
#' @param value new value to assign
#'
#' @return returns modified object
#' @export
#'
#'
setGeneric("category<-", function(x, value) standardGeneric("category<-"))
#' Access or assign lifestage (generic for KinPairData class)
#'
#' @param x object with relevant method
#'
#' @return
#' @export
#'
#'
setGeneric("lifestage", function(x) standardGeneric("lifestage"))
#'
#' @rdname lifestage
#' @param x object with relevant method
#' @param value new value to assign
#'
#' @return
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
#'
#' @return
#' @export
#'
#' @describeIn KinPairData access distances
setMethod("distances", "KinPairData", function(x) x@tab$distance)

#'
#'
#' @param KinPairData object of class \code{KinPairData}
#'
#' @return
#' @export
#'
#' @describeIn KinPairData access kin category
setMethod("category", "KinPairData", function(x) x@category)

#'
#'
#' @param KinPairData object of class \code{KinPairData}
#'
#' @return
#' @export
#'
#' @describeIn KinPairData assign kin category
setMethod("category<-", "KinPairData", function(x, value){
  x@category <- value
  validObject(x)
  x
})


#'
#'
#' @param KinPairData
#'
#' @return
#' @export
#'
#' @describeIn KinPairData access lifestage
setMethod("lifestage", "KinPairData", function(x) x@lifestage)

#'
#'
#'
#' @param KinPairData
#'
#' @return
#' @export
#'
#' @describeIn KinPairData assign lifestage
setMethod("lifestage<-", "KinPairData", function(x, value){
  x@lifestage <- value
  validObject(x)
  x
})

#'
#'
#' @param KinPairData
#'
#' @return
#' @export
#'
#' @describeIn KinPairData standard print method
setMethod(
  "show",
  "KinPairData",
  function(object){
    cat("KINDISPERSE RECORD OF KIN PAIRS\n")
    cat("-------------------------------\n")
    cat('category:\t\t', object@category, '\n')
    cat('lifestage:\t\t', object@lifestage, '\n\n')
    cat('tab\n')
    print(object@tab)
    cat("-------------------------------")
  }
)

# Constructor method of KinPairData

#' Title
#'
#' @param KinPairData
#'
#' @return
#' @export
#'
#' @describeIn KinPairData initialize method
setMethod("initialize", "KinPairData",
          function(.Object,
          data,
          category,
          lifestage,
          ...){

  if (! missing(category)){
    .Object@category <- category
  }
  else {.Object@category <- "UN"}
  if (! missing(lifestage)){
    .Object@lifestage <- lifestage
  }
  else {.Object@lifestage <- "unknown"}
  if (! missing(data)){
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
  return(.Object
         )
}
)


setValidity("KinPairData", function(object){
  if (! object@category %in% c("UN", "PO", "GG", "GGG", "FS", "AV", "GAV", "1C", "1C1", "2C", "HS", "HAV", "HGAV", "H1C", "H1C1", "H2C")) {
    "@category must be one of UN PO GG GGG FS AV GAV 1C 1C1 2C HS HAV HGAV H1C H1C1 H2C"
  }
  else if (! object@lifestage %in% c("unknown", "larva", "oviposition")){
    "@lifestage must currently be set to 'unknown', 'larva', or 'oviposition'"
  } else{
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
is.KinPairData <- function(x){
  "KinPairData" %in% is(x)
}
