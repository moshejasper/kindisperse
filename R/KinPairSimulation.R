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
#' @return
#' @export
#'
#' @examples
KinPairSimulation <- setClass("KinPairSimulation",
         slots = list(category = "character", simtype = "character", kerneltype = "character",
                      sigma = "numeric", juvsigma = "numeric", breedsigma = "numeric",
                      gravsigma = "numeric", ovisigma = "numeric", dims = "numeric",
                      lifestage = "character", call = "call", tab = "tbl_df",
                      filtertype = "character", upper = "numeric", lower = "numeric",
                      spacing = "numeric", samplenum = "numeric", sampledims = "numeric"))




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
    if (length(object@simtype) == 0) {cat('')}
    else if  (object@simtype == "simple"){
      cat('sigma:\t\t\t', object@sigma, '\n')
    }
    else if (object@simtype == "composite"){
      cat('juvsigma\t\t', object@juvsigma, '\nbreedsigma\t\t', object@breedsigma, '\ngravsigma\t\t', object@gravsigma, '\novisigma\t\t', object@ovisigma, '\n')
    }
    cat('lifestage:\t\t', object@lifestage, '\n\n')
    if (length(object@filtertype) > 0) { if (object@filtertype == "filtered") {
      cat('FILTERED\n')
      cat('--------\n')
      if (! length(object@upper) == 0) {cat('upper:\t\t\t', object@upper, '\n')}
      if (! length(object@lower) == 0) {cat('lower:\t\t\t', object@lower, '\n')}
      if (! length(object@spacing) == 0) {cat('spacing:\t\t', object@spacing, '\n')}
      if (! length(object@samplenum) == 0) {cat('samplenum:\t\t', object@samplenum, '\n')}
      if (! length(object@sampledims) == 0) {cat('sampledims:\t\t', object@sampledims, '\n')}
      cat('\n')
    }}
    cat('tab\n')
    print(object@tab)
    cat("-----------------------------------")
  }
)
