#' Takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring (intergenerational) kinship dispersal kernel
#' Further tests
#' @param avect     vector a of kin dispersal distances for the less closely related kinship category
#' @param bvect     vector b of kin dispersal distances for the more closely related kinship category
#' @param nreps     number of permutations to run for confidence intervals (default 1000)
#' @param nsamp     number of kin pairs to subsample for each permutation. Either "std" or an integer. If "std" will be computed as equal to the sample size. (default "std")
#' @param acat      kinship category of kin dispersal vector avect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcat      kinship category of kin dispersal vector bvect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param amix      logical describing whether vector a is a mixture of two kinship categories. Used with amixcat. Default FALSE.
#' @param bmix      logical describing whether vector b is a mixture of two kinship categories. Used with bmixcat. Default FALSE.
#' @param amixcat   mixture kinship category of vector a. Must be set if amix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bmixcat   mixture kinship category of vector b. Must be set if bmix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acomp     logical denoting whether vector a should be composited with an additional kinship category vector. Used with acompvect and acompcat. Default FALSE.
#' @param bcomp     logical denoting whether vector b should be composited with an additional kinship category vector. Used with bcompvect and bcompcat. Default FALSE.
#' @param acompvect vector acomp of kin dispersal distances for compositing with vector a. Must be set if acomp == TRUE.
#' @param bcompvect vector bcomp of kin dispersal distances for compositing with vector b. Must be set if bcomp == TRUE.
#' @param acompcat  kinship category of kin dispersal vector acompvect. Must be set if acomp == TRUE.  Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcompcat  kinship category of kin dispersal vector bcompvect. Must be set if bcomp == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param output    string denoting what kind of output to return. If 'confs', a vector of 95% confidence intervals. if 'vect', a vector of all permutated axial value results
#'
#' @return
#' @export
#'
#' @examples
axials_standard <- function(avect, bvect, nreps = 1000, nsamp = "std", acat, bcat,
                            amix = F, bmix = F, amixcat = NULL, bmixcat = NULL, acomp = F, bcomp = F,
                            acompvect = NULL, bcompvect = NULL, acompcat = NULL, bcompcat = NULL, output = "confs"){

  # Run tests - these check basic pairings between categories
  if (is.null(avect)) {stop("Please supply kin dispersal distance vector a!")}
  if (is.null(bvect)) {stop("Please supply kin dispersal distance vector b!")}
  if (is.null(acat)) {stop("Please supply kin category a!")}
  if (is.null(bcat)) {stop("Please supply kin category b!")}

  aphase <- phase_assigner(acat)
  bphase <- phase_assigner(bcat)
  aphasetest <- aphase
  bphasetest <- bphase

  aspan <- span_assigner(acat)
  bspan <- span_assigner(bcat)
  aspantest <- aspan
  bspantest <- bspan
  a_ax <- axials(avect)
  b_ax <- axials(bvect)

  if (amix == T) {
    if (is.null(amixcat)) {stop("please supply mixture kin category for a!")}
    if (bcomp == F & bmix == F) {stop("Mixed category a must be paired with a mixed category or composite b!")}
    if (acomp ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both == T)")}
    amixphase <- phase_assigner(amixcat)
    aphasetest <- c(aphasetest, amixphase)
    amixspan <- span_assigner(amixcat)
    aspantest <- mean(c(aspantest, amixspan))
  }
  if (bmix == T) {
    if (is.null(bmixcat)) {stop("please supply mixture kin category for b!")}
    if (acomp == F & amix == F) {stop("Mixed category b must be paired with a mixed category or composite a!")}
    if (bcomp ==T) {stop("Estimate b cannot be both composite & mixed ('bmix' & 'bcomp' both == T)")}
    bmixphase <- phase_assigner(bmixcat)
    bphasetest <- c(bphasetest, bmixphase)
    bmixspan <- span_assigner(bmixcat)
    bspantest <- mean(c(bspantest, bmixspan))
  }

  if (acomp == T) {
    if (is.null(acompvect)) {stop("please supply composite vector for a!")}
    if (bcomp == F & bmix == F) {stop("Composite estimate a must be paired with a mixed category or composite b!")}
    if (is.null(acompcat)) {stop("Please supply kin category for composite a")}
    if (amix ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both T)")}
    acompphase <- phase_assigner(acompcat)
    aphasetest <- c(aphasetest, acompphase)
    acompspan <- span_assigner(acompcat)
    aspantest <- mean(c(aspantest, acompspan))
    acomp_ax <- axials(acompvect)
    a_ax <- axials_combine(c(a_ax, acomp_ax))
  }

  if (bcomp == T) {
    if (is.null(bcompvect)) {stop("please supply composite vector for b!")}
    if (acomp == F & amix == F) {stop("Composite estimate b must be paired with a mixed category or composite a!")}
    if (is.null(bcompcat)) {stop("Please supply kin category for composite b")}
    if (bmix ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both T)")}
    bcompphase <- phase_assigner(bcompcat)
    bphasetest <- c(bphasetest, bcompphase)
    bcompspan <- span_assigner(bcompcat)
    bspantest <- mean(c(bspantest, bcompspan))
    bcomp_ax <- axials(bcompvect)
    b_ax <- axials_combine(c(b_ax, bcomp_ax))
  }

  if (! identical(sort(aphasetest), sort(bphasetest))) {stop(paste0("A and B phases are mismatched! A: ", sort(aphasetest), " B: ", sort(bphasetest)))}
  if (aspantest <= bspantest) {stop(paste0("A categories should contain more dispersed kin categories than B categories: A spans: ", aspantest, " B spans: ", bspantest))}

  spandiff <- aspantest - bspantest
  print(aspantest)
  print(spandiff)
  lifeax_prelim <- axials_subtract(a_ax, b_ax)
  lifeax_final <- axials_decompose(lifeax_prelim, spandiff)
  return(lifeax_final)

}


#' Takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring (intergenerational) kinship dispersal kernel with bootstrap-based confidence intervals.
#' Further tests
#' @param avect     vector a of kin dispersal distances for the less closely related kinship category
#' @param bvect     vector b of kin dispersal distances for the more closely related kinship category
#' @param nreps     number of permutations to run for confidence intervals (default 1000)
#' @param nsamp     number of kin pairs to subsample for each permutation. Either "std" or an integer. If "std" will be computed as equal to the sample size. (default "std")
#' @param acat      kinship category of kin dispersal vector avect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcat      kinship category of kin dispersal vector bvect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param amix      logical describing whether vector a is a mixture of two kinship categories. Used with amixcat. Default FALSE.
#' @param bmix      logical describing whether vector b is a mixture of two kinship categories. Used with bmixcat. Default FALSE.
#' @param amixcat   mixture kinship category of vector a. Must be set if amix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bmixcat   mixture kinship category of vector b. Must be set if bmix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acomp     logical denoting whether vector a should be composited with an additional kinship category vector. Used with acompvect and acompcat. Default FALSE.
#' @param bcomp     logical denoting whether vector b should be composited with an additional kinship category vector. Used with bcompvect and bcompcat. Default FALSE.
#' @param acompvect vector acomp of kin dispersal distances for compositing with vector a. Must be set if acomp == TRUE.
#' @param bcompvect vector bcomp of kin dispersal distances for compositing with vector b. Must be set if bcomp == TRUE.
#' @param acompcat  kinship category of kin dispersal vector acompvect. Must be set if acomp == TRUE.  Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcompcat  kinship category of kin dispersal vector bcompvect. Must be set if bcomp == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param output    string denoting what kind of output to return. If 'confs', a vector of 95% confidence intervals. if 'vect', a vector of all permutated axial value results
#'
#' @return
#' @export
#'
#' @examples
axpermute_standard <- function(avect, bvect, nreps = 1000, nsamp = "std", acat, bcat,
                               amix = F, bmix = F, amixcat = NULL, bmixcat = NULL, acomp = F, bcomp = F,
                               acompvect = NULL, bcompvect = NULL, acompcat = NULL, bcompcat = NULL, output = "confs"){

  # Run tests - these check basic pairings between categories
  if (is.null(avect)) {stop("Please supply kin dispersal distance vector a!")}
  if (is.null(bvect)) {stop("Please supply kin dispersal distance vector b!")}
  if (is.null(acat)) {stop("Please supply kin category a!")}
  if (is.null(bcat)) {stop("Please supply kin category b!")}

  aphase <- phase_assigner(acat)
  bphase <- phase_assigner(bcat)
  aphasetest <- aphase
  bphasetest <- bphase

  aspan <- span_assigner(acat)
  bspan <- span_assigner(bcat)
  aspantest <- aspan
  bspantest <- bspan

  if (nsamp == "std") {anum <- length(avect); bnum <- length(bvect)}
  else {anum <- bnum <- nsamp}

  if (amix == T) {
    if (is.null(amixcat)) {stop("please supply mixture kin category for a!")}
    if (bcomp == F & bmix == F) {stop("Mixed category a must be paired with a mixed category or composite b!")}
    if (acomp ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both == T)")}
    amixphase <- phase_assigner(amixcat)
    aphasetest <- c(aphasetest, amixphase)
    amixspan <- span_assigner(amixcat)
    aspantest <- mean(c(aspantest, amixspan))
  }
  if (bmix == T) {
    if (is.null(bmixcat)) {stop("please supply mixture kin category for b!")}
    if (acomp == F & amix == F) {stop("Mixed category b must be paired with a mixed category or composite a!")}
    if (bcomp ==T) {stop("Estimate b cannot be both composite & mixed ('bmix' & 'bcomp' both == T)")}
    bmixphase <- phase_assigner(bmixcat)
    bphasetest <- c(bphasetest, bmixphase)
    bmixspan <- span_assigner(bmixcat)
    bspantest <- mean(c(bspantest, bmixspan))
  }

  if (acomp == T) {
    if (is.null(acompvect)) {stop("please supply composite vector for a!")}
    if (bcomp == F & bmix == F) {stop("Composite estimate a must be paired with a mixed category or composite b!")}
    if (is.null(acompcat)) {stop("Please supply kin category for composite a")}
    if (amix ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both T)")}
    acompphase <- phase_assigner(acompcat)
    aphasetest <- c(aphasetest, acompphase)
    acompspan <- span_assigner(acompcat)
    aspantest <- mean(c(aspantest, acompspan))
    if (nsamp == "std") {acompnum <- length(acompvect)}
    else {acompnum <- nsamp}
  }

  if (bcomp == T) {
    if (is.null(bcompvect)) {stop("please supply composite vector for b!")}
    if (acomp == F & amix == F) {stop("Composite estimate b must be paired with a mixed category or composite a!")}
    if (is.null(bcompcat)) {stop("Please supply kin category for composite b")}
    if (bmix ==T) {stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both T)")}
    bcompphase <- phase_assigner(bcompcat)
    bphasetest <- c(bphasetest, bcompphase)
    bcompspan <- span_assigner(bcompcat)
    bspantest <- mean(c(bspantest, bcompspan))
    if (nsamp == "std") {bcompnum <- length(bcompvect)}
    else {bcompnum <- nsamp}
  }

  if (! identical(sort(aphasetest), sort(bphasetest))) {stop(paste0("A and B phases are mismatched! A: ", sort(aphasetest), " B: ", sort(bphasetest)))}
  if (aspantest <= bspantest) {stop(paste0("A categories should contain more dispersed kin categories than B categories: A spans: ", aspantest, " B spans: ", bspantest))}

  if (! output %in% c("confs", "vect")) {stop("'output' must be set to either confidence intervals 'confs' or vector 'vect'")}

  spandiff <- aspantest - bspantest

  # set up permutations...

  container <- tibble::tibble(ax = 0.0, .rows = 0)

  for (val in 1:nreps){

    asub <- sample(avect, anum, replace = TRUE)
    bsub <- sample(bvect, bnum, replace = TRUE)

    a_ax <- axials(asub)
    b_ax <- axials(bsub)

    if (acomp == T) {
      acompsub <- sample(acompvect, acompnum, replace = TRUE)
      acomp_ax <- axials(acompsub)
      a_ax <- axials_combine(c(a_ax, acomp_ax))
    }

    if (bcomp == T) {
      bcompsub <- sample(bcompvect, bcompnum, replace = TRUE)
      bcomp_ax <- axials(bcompsub)
      b_ax <- axials_combine(c(b_ax, bcomp_ax))
    }

    if (a_ax <= b_ax) {
      lifeax_prelim <- NA
      lifeax_final <- -1
    }
    else {
      lifeax_prelim <- axials_subtract(a_ax, b_ax)
      lifeax_final <- axials_decompose(lifeax_prelim, spandiff)
    }


    container <- tibble::add_row(container, ax = lifeax_final)

  }

  # return values

  if (output == "confs") {
    return(stats::quantile(container$ax, c(0.025, 0.5, 0.975)))
  }
  else if (output == "vect") {
    return(sort(container$ax))
  }

}
