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

#' Constructor for KinPairSimulation Class
#'
#' @param tab tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), category (chr) and sims (dbl)
#' @param category  character. Code for kinship category of simulation. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C
#' @param simtype character. Type of simulation (currently 'simple' or 'composite')
#' @param kerneltype  character. Statistical model for simulated dispersal kernel. Currently either "Gaussian" or "Laplace".
#' @param sigma numeric. Axial sigma of dispersal kernel (axial standard deviation). Used with 'simple' simtype
#' @param juvsigma  numeric. Axial sigma of prebreeding ('juvenile') dispersal kernel (axial standard deviation). Used with 'composite' simtype
#' @param breedsigma  numeric. Axial sigma of breeding dispersal kernel (axial standard deviation). Used with 'composite' simtype
#' @param gravsigma numeric. Axial sigma of post-breeding ('gravid') dispersal kernel (axial standard deviation). Used with 'composite' simtype
#' @param ovisigma  numeric. Axial sigma of oviposition dispersal kernel (axial standard deviation). Used with 'composite' simtype
#' @param dims  numeric. Length of side of simulated area square.
#' @param lifestage character. Simulated lifestage of sampling. Either "larva" (sampled at hatching) or "oviposition" (sampled as an adult during oviposition - essentially one lifespan later that 'larva')
#' @param call  call object. Use to pass the system call that led to the generation of this class. (via sys.call)
#' @param filtertype character. Has a value of 'filtered' if object has been passed through the sample_kindist() function or equivalent. Blank otherwise.
#' @param upper numeric. upper cutoff for kin pair distances (if filtered)
#' @param lower numeric. lower cutoff for kin pair distances (if filtered)
#' @param spacing numeric. spacing between traps (assume 1D layout) (if filtered)
#' @param samplenum numeric. number of individuals retained within data (if filtered)
#' @param sampledims numeric. Dimensions used in filtering process (side of square) (if filtered)
#'
#' @return Returns an object of class 'KinPairSimulation'
#' @export
#'
#' @examples
#' kindata <- tibble::tibble(id1 = c("a", "b", "c"), id2 = c("x", "y", "z"),
#'     distance = c(50, 45, 65), category = c("1C", "1C", "1C"))
#' make_KinPairSimulation(kindata, "1C", "simple", "Gaussian", sigma = 38, lifestage = "larva")
#' @importFrom methods "new"
make_KinPairSimulation <- function(tab, category=character(), simtype=character(), kerneltype=character(), sigma=numeric(), juvsigma=numeric(), breedsigma=numeric(), gravsigma=numeric(), ovisigma=numeric(),
                                   dims=numeric(), lifestage=character(), call=NULL, filtertype=character(), upper=numeric(), lower=numeric(),
                                   spacing=numeric(), samplenum=numeric(), sampledims=numeric()){

  if (is.null(call)) {call <- sys.call()}
  return(new("KinPairSimulation",
         category = category, simtype = simtype, kerneltype = kerneltype, sigma = sigma, juvsigma = juvsigma, breedsigma = breedsigma,
         gravsigma = gravsigma, ovisigma = ovisigma, dims = dims, lifestage = lifestage, call = call, tab = tab, filtertype = filtertype,
         upper = upper, lower = lower, spacing = spacing, samplenum = samplenum, sampledims = sampledims))
}


#' Constructor for KinPairSimulation Class (simple)
#'
#' @param tab tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), category (chr) and sims (dbl)
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
KinPairSimulation_simple <- function(tab, category=character(), kerneltype=character(), sigma=numeric(), dims = numeric(), lifestage=character(), call=NULL){
  if (is.null(call)) {call <- sys.call()}
  return(make_KinPairSimulation(tab=tab, category=category, simtype="simple", kerneltype=kerneltype, sigma=sigma, dims = dims, lifestage=lifestage, call=call))
}


#' Constructor for KinPairSimulation Class (composite)
#'
#' @param tab tibble of pairwise kin classes & distances. Ideally contains fields id1 & id2 (chr) an distance (dbl) optionally includes coords (x1, y1, x2, y2), lifestage (ls1 & ls2), category (chr) and sims (dbl)
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
KinPairSimulation_composite <- function(tab, category=character(), kerneltype=character(), juvsigma=numeric(), breedsigma=numeric(),
                                             gravsigma=numeric(), ovisigma=numeric(), dims = numeric(), lifestage=character(), call=NULL){
  if (is.null(call)) {call <- sys.call()}
  return(make_KinPairSimulation(tab=tab, category=category, simtype="composite", kerneltype=kerneltype, juvsigma=juvsigma, breedsigma=breedsigma,
                                gravsigma=gravsigma, ovisigma=ovisigma, dims = dims, lifestage=lifestage, call=call))
}
