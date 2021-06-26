#' Calculate the intergenerational (PO) dispersal kernel from the distributions of close kin
#'
#' This function takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring
#' (intergenerational) kinship dispersal kernel. Dispersal distances can be inputted as numeric vectors, or alternatively as objects of classes \code{\link{KinPairData}}
#' or \code{\link{KinPairSimulation}}.
#'
#' This (with its paired function \code{\link{axpermute_standard}}) are the core functions implemented in the \code{kindisperse} package. They enable the decomposition of the
#' pedigree & dispersal information contained in the sampled distributions of close kin dyads (full siblings, first cousins, etc.) & its leveraging within an additive dispersal
#' framework to estimate the key intergenerational (parent-offspring) dispersal parameter of a population. Four key ideas underpin the approach in this function: (a) tracing
#' dispersal pedigrees to determine the number of complete intergenerational (breeding-cycle-spanning) dispersal events separating the sampled close-kin dyads; (b) using kin categories
#' that share the same overarching kinship 'phase' to control for residual 'phased' (non-intergenerational) disperal events that occur at the pedigree branch point (e.g. ovipositional
#' dispersal for full sibling mosquitoes), and (c) using synced or equivalent sampling points to eliminate non-intergenerational dispersal at the branch-tips of the pedigrees, then
#' finally (d) decomposing the 'pure' pedigree-associated (intergenerational) dispersal into an estimae of the single-generation intergenerational dispersal parameter.
#'
#' At its most basic, this function requires information about two dispersal vectors, a & b - both of a phased kinship category, & \code{vector a} having a more dispersed pedigree
#' than \code{vector b}. In addition to this initial pair of dispersed kin categories, either one or another matched pair of kin categories can be added:
#'
#' 1. A mixture category. This redefines the vector it is paired with (either a or b) so that rather than being considered as a 'pure' pedigree variant, it is considered as mixed with a
#' different kin category, often of a differing pedigree phase. If used, the other initial vector must also be paired with a related mixture category or composite vector.
#'
#' 2. A composite dispersal vector. This is defined exactly as the initial dispersal vectors. After calculation, the axial value found is composited with that of the matched initial vector, and
#' its kinship category redefined as a mixture category as above. If used, the other initial vector must also be paired with a related mixture category or composite vector. These can be paired so
#' that a mixture category (e.g. first & half-first cousins where these could not be separated with available genetic data) can be counterbalanced with the composition of full sibling & half-sibling
#' dyads, which (assuming equal mixture) approximately controls for the phasing of the mixed kin categories, enabling an estimate of intergenerational dispersal without exact knowledge of the
#' composition of the cousins distribution.
#'
#' Each vector or \code{KinPairData} / \code{KinPairSimulation} object is paired with several other parameters: (1) a logical (e.g. \code{amix} delineating whether the category is being
#' used in the calculation, (2) a category parameter (.e.g \code{acat}) defining what kin relationship is being measured, (3) an optional breeding cycle number (e.g. \code{acycle})
#' showing the number of breeding cycles each member of the kin pair has passed through before being sampled (the cycle vector \code{c(1, 0)} corresponds to an adult & a juvenile
#' being sampled at the same point in the breeding cycle; \code{c(1, 1)} represents two adults (i.e. after their first breeding), etc.) . If a \code{\link{KinPairData}} or
#' \code{\link{KinPairSimulation}} object is inputted, all paired parameters that are not explicitly set will default to those contained in the objects (using KinPair objects
#' is the ideal way to deploy this function).
#'
#' For further information on this function, package & the dispersal estimation method it represents, see the paper by Jasper et al. - "A genomic approach to inferring kinship reveals
#' limited intergenerational dispersal in the yellow fever mosquito", \doi{10.1111/1755-0998.13043}.
#'
#' @param avect     vector a of kin dispersal distances for the less closely related kinship category OR object of class \code{KinPairData}.
#' @param bvect     vector b of kin dispersal distances for the more closely related kinship category OR object of class \code{KinPairData}.
#' @param acat      kinship category of kin dispersal vector avect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcat      kinship category of kin dispersal vector bvect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param amix      logical describing whether vector a is a mixture of two kinship categories. Used with amixcat. Default FALSE.
#' @param bmix      logical describing whether vector b is a mixture of two kinship categories. Used with bmixcat. Default FALSE.
#' @param amixcat   mixture kinship category of vector a. Must be set if amix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bmixcat   mixture kinship category of vector b. Must be set if bmix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acomp     logical denoting whether vector a should be composited with an additional kinship category vector. Used with acompvect and acompcat. Default FALSE.
#' @param bcomp     logical denoting whether vector b should be composited with an additional kinship category vector. Used with bcompvect and bcompcat. Default FALSE.
#' @param acompvect vector acomp of kin dispersal distances for compositing with vector a OR object of class KinPairData. Must be set if acomp == TRUE.
#' @param bcompvect vector bcomp of kin dispersal distances for compositing with vector b OR object of class KinPairData. Must be set if bcomp == TRUE.
#' @param acompcat  kinship category of kin dispersal vector acompvect. Must be set if acomp == TRUE.  Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcompcat  kinship category of kin dispersal vector bcompvect. Must be set if bcomp == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acycle    breeding cycle number of kin dispersal vector avect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param bcycle    breeding cycle number of kin dispersal vector bvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan
#' @param amixcycle breeding cycle number of kin dispersal vector amixvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan
#' @param bmixcycle breeding cycle number of kin dispersal vector bmixvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param acompcycle  breeding cycle number of kin dispersal vector acompvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param bcompcycle  breeding cycle number of kin dispersal vector bcompvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#'
#' @return Returns a numeric estimate of PO (intergenerational) dispersal kernel axial distribution.
#' @export
#' @family axstandard
#'
#' @examples
#' cous <- rexp(100, 1 / 100)
#' fullsibs <- rexp(50, 1 / 50)
#' axials_standard(cous, fullsibs, acat = "1C", bcat = "FS")
axials_standard <- function(avect, bvect, acat = NULL, bcat = NULL,
                            amix = FALSE, bmix = FALSE, amixcat = NULL, bmixcat = NULL, acomp = FALSE, bcomp = FALSE,
                            acompvect = NULL, bcompvect = NULL, acompcat = NULL, bcompcat = NULL, acycle = NULL,
                            bcycle = NULL, amixcycle = NULL, bmixcycle = NULL, acompcycle = NULL, bcompcycle = NULL) {
  if (is.KinPairData(avect)) {
    if (is.null(acat)) acat <- kinship(avect)
    if (is.null(acycle)) acycle <- breeding_cycle(avect)
    avect <- distances(avect)
  }
  if (is.KinPairData(bvect)) {
    if (is.null(bcat)) bcat <- kinship(bvect)
    if (is.null(bcycle)) bcycle <- breeding_cycle(bvect)
    bvect <- distances(bvect)
  }
  if (is.KinPairData(acompvect)) {
    if (is.null(acompcat)) acompcat <- kinship(acompvect)
    if (is.null(acompcycle)) acompcycle <- breeding_cycle(acompvect)
    acompvect <- distances(acompvect)
  }
  if (is.KinPairData(bcompvect)) {
    if (is.null(bcompcat)) bcompcat <- kinship(bcompvect)
    if (is.null(bcompcycle)) bcompcycle <- breeding_cycle(bcompvect)
    bcompvect <- distances(bcompvect)
  }
  # Run tests - these check basic pairings between categories
  if (is.null(avect)) {
    stop("Please supply kin dispersal distance vector a!")
  }
  if (is.null(bvect)) {
    stop("Please supply kin dispersal distance vector b!")
  }
  if (is.null(acat)) {
    stop("Please supply kin category a!")
  }
  if (is.null(bcat)) {
    stop("Please supply kin category b!")
  }
  if (is.null(acycle)) acycle <- 0
  if (is.null(bcycle)) bcycle <- 0

  aphase <- phase_assigner(acat)
  bphase <- phase_assigner(bcat)
  aphasetest <- aphase
  bphasetest <- bphase

  aspan <- span_assigner(acat) + cycle_to_span(acycle)
  bspan <- span_assigner(bcat) + cycle_to_span(bcycle)
  aspantest <- aspan
  bspantest <- bspan
  a_ax <- axials(avect)
  b_ax <- axials(bvect)

  if (amix == TRUE) {
    if (is.null(amixcat)) {
      stop("please supply mixture kin category for a!")
    }
    if (is.null(amixcycle)) amixcycle <- 0
    if (bcomp == FALSE & bmix == FALSE) {
      stop("Mixed category a must be paired with a mixed category or composite b!")
    }
    if (acomp == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both == TRUE)")
    }
    amixphase <- phase_assigner(amixcat)
    aphasetest <- c(aphasetest, amixphase)
    amixspan <- span_assigner(amixcat) + cycle_to_span(amixcycle)
    aspantest <- mean(c(aspantest, amixspan))
  }
  if (bmix == TRUE) {
    if (is.null(bmixcat)) {
      stop("please supply mixture kin category for b!")
    }
    if (is.null(bmixcycle)) bmixcycle <- 0
    if (acomp == FALSE & amix == FALSE) {
      stop("Mixed category b must be paired with a mixed category or composite a!")
    }
    if (bcomp == TRUE) {
      stop("Estimate b cannot be both composite & mixed ('bmix' & 'bcomp' both == TRUE)")
    }
    bmixphase <- phase_assigner(bmixcat)
    bphasetest <- c(bphasetest, bmixphase)
    bmixspan <- span_assigner(bmixcat) + cycle_to_span(bmixcycle)
    bspantest <- mean(c(bspantest, bmixspan))
  }

  if (acomp == TRUE) {
    if (is.null(acompvect)) {
      stop("please supply composite vector for a!")
    }
    if (is.null(acompcycle)) acompcycle <- 0
    if (bcomp == FALSE & bmix == FALSE) {
      stop("Composite estimate a must be paired with a mixed category or composite b!")
    }
    if (is.null(acompcat)) {
      stop("Please supply kin category for composite a")
    }
    if (amix == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both TRUE)")
    }
    acompphase <- phase_assigner(acompcat)
    aphasetest <- c(aphasetest, acompphase)
    acompspan <- span_assigner(acompcat) + cycle_to_span(acompcycle)
    aspantest <- mean(c(aspantest, acompspan))
    acomp_ax <- axials(acompvect)
    a_ax <- axials_combine(c(a_ax, acomp_ax))
  }

  if (bcomp == TRUE) {
    if (is.null(bcompvect)) {
      stop("please supply composite vector for b!")
    }
    if (is.null(bcompcycle)) bcompcycle <- 0
    if (acomp == FALSE & amix == FALSE) {
      stop("Composite estimate b must be paired with a mixed category or composite a!")
    }
    if (is.null(bcompcat)) {
      stop("Please supply kin category for composite b")
    }
    if (bmix == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both TRUE)")
    }
    bcompphase <- phase_assigner(bcompcat)
    bphasetest <- c(bphasetest, bcompphase)
    bcompspan <- span_assigner(bcompcat) + cycle_to_span(bcompcycle)
    bspantest <- mean(c(bspantest, bcompspan))
    bcomp_ax <- axials(bcompvect)
    b_ax <- axials_combine(c(b_ax, bcomp_ax))
  }

  if (!identical(sort(aphasetest), sort(bphasetest))) {
    stop(paste0("A and B phases are mismatched! A: ", sort(aphasetest), " B: ", sort(bphasetest)))
  }
  if (aspantest <= bspantest) {
    stop(paste0("A categories should contain more dispersed kin categories than B categories: A spans: ", aspantest, " B spans: ", bspantest))
  }

  spandiff <- aspantest - bspantest
  lifeax_prelim <- axials_subtract(a_ax, b_ax)
  lifeax_final <- axials_decompose(lifeax_prelim, spandiff)
  return(lifeax_final)
}


#' Calculate the intergenerational (PO) dispersal kernel from the distributions of close kin (bootstrapped)
#'
#' This function takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring
#' (intergenerational) kinship dispersal kernel with bootstrapped confidence intervals. Dispersal distances can be inputted as numeric vectors, or alternatively as objects of
#' classes \code{\link{KinPairData}} or \code{\link{KinPairSimulation}}.
#'
#' This (with its paired function \code{\link{axials_standard}}) are the core functions implemented in the \code{kindisperse} package. They enable the decomposition of the
#' pedigree & dispersal information contained in the sampled distributions of close kin dyads (full siblings, first cousins, etc.) & its leveraging within an additive dispersal
#' framework to estimate the key intergenerational (parent-offspring) dispersal parameter of a population. Four key ideas underpin the approach in this function: (a) tracing
#' dispersal pedigrees to determine the number of complete intergenerational (breeding-cycle-spanning) dispersal events separating the sampled close-kin dyads; (b) using kin categories
#' that share the same overarching kinship 'phase' to control for residual 'phased' (non-intergenerational) disperal events that occur at the pedigree branch point (e.g. ovipositional
#' dispersal for full sibling mosquitoes), and (c) using synced or equivalent sampling points to eliminate non-intergenerational dispersal at the branch-tips of the pedigrees, then
#' finally (d) decomposing the 'pure' pedigree-associated (intergenerational) dispersal into an estimae of the single-generation intergenerational dispersal parameter.
#'
#' At its most basic, this function requires information about two dispersal vectors, a & b - both of a phased kinship category, & \code{vector a} having a more dispersed pedigree
#' than \code{vector b}. In addition to this initial pair of dispersed kin categories, either one or another matched pair of kin categories can be added:
#'
#' 1. A mixture category. This redefines the vector it is paired with (either a or b) so that rather than being considered as a 'pure' pedigree variant, it is considered as mixed with a
#' different kin category, often of a differing pedigree phase. If used, the other initial vector must also be paired with a related mixture category or composite vector.
#'
#' 2. A composite dispersal vector. This is defined exactly as the initial dispersal vectors. After calculation, the axial value found is composited with that of the matched initial vector, and
#' its kinship category redefined as a mixture category as above. If used, the other initial vector must also be paired with a related mixture category or composite vector. These can be paired so
#' that a mixture category (e.g. first & half-first cousins where these could not be separated with available genetic data) can be counterbalanced with the composition of full sibling & half-sibling
#' dyads, which (assuming equal mixture) approximately controls for the phasing of the mixed kin categories, enabling an estimate of intergenerational dispersal without exact knowledge of the
#' composition of the cousins distribution.
#'
#' Each vector or \code{KinPairData} / \code{KinPairSimulation} object is paired with several other parameters: (1) a logical (e.g. \code{amix} delineating whether the category is being
#' used in the calculation, (2) a category parameter (.e.g \code{acat}) defining what kin relationship is being measured, (3) an optional breeding cycle number (e.g. \code{acycle})
#' showing the number of breeding cycles each member of the kin pair has passed through before being sampled (the cycle vector \code{c(1, 0)} corresponds to an adult & a juvenile
#' being sampled at the same point in the breeding cycle; \code{c(1, 1)} represents two adults (i.e. after their first breeding), etc.) . If a \code{\link{KinPairData}} or
#' \code{\link{KinPairSimulation}} object is inputted, all paired parameters that are not explicitly set will default to those contained in the objects (using KinPair objects
#' is the ideal way to deploy this function).
#'
#' Confidence intervals are assigned via bootstrapping, or optionally the
#' vector of all bootstrapped results can be outputted by setting \code{output} to \code{'vect'}, enabling its passing to other
#' functions or external statistical analysis.
#'
#' For further information on this function, package & the dispersal estimation method it represents, see the paper by Jasper et al. - "A genomic approach to inferring kinship reveals
#' limited intergenerational dispersal in the yellow fever mosquito", \doi{10.1111/1755-0998.13043}.
#' @param avect     vector a of kin dispersal distances for the less closely related kinship category OR object of class KinPairData.
#' @param bvect     vector b of kin dispersal distances for the more closely related kinship category OR object of class KinPairData.
#' @param acat      kinship category of kin dispersal vector avect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcat      kinship category of kin dispersal vector bvect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param nreps     number of permutations to run for confidence intervals (default 1000)
#' @param nsamp     number of kin pairs to subsample for each permutation. Either "std" or an integer. If "std" will be computed as equal to the sample size. (default "std")
#' @param amix      logical describing whether vector a is a mixture of two kinship categories. Used with amixcat. Default FALSE.
#' @param bmix      logical describing whether vector b is a mixture of two kinship categories. Used with bmixcat. Default FALSE.
#' @param amixcat   mixture kinship category of vector a. Must be set if amix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bmixcat   mixture kinship category of vector b. Must be set if bmix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acomp     logical denoting whether vector a should be composited with an additional kinship category vector. Used with acompvect and acompcat. Default FALSE.
#' @param bcomp     logical denoting whether vector b should be composited with an additional kinship category vector. Used with bcompvect and bcompcat. Default FALSE.
#' @param acompvect vector acomp of kin dispersal distances for compositing with vector a OR object of class KinPairData. Must be set if acomp == TRUE.
#' @param bcompvect vector bcomp of kin dispersal distances for compositing with vector b OR object of class KinPairData. Must be set if bcomp == TRUE.
#' @param acompcat  kinship category of kin dispersal vector acompvect. Must be set if acomp == TRUE.  Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param bcompcat  kinship category of kin dispersal vector bcompvect. Must be set if bcomp == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"
#' @param acycle    breeding cycle number of kin dispersal vector avect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param bcycle    breeding cycle number of kin dispersal vector bvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param amixcycle breeding cycle number of kin dispersal vector amixvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param bmixcycle breeding cycle number of kin dispersal vector bmixvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param acompcycle  breeding cycle number of kin dispersal vector acompvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param bcompcycle  breeding cycle number of kin dispersal vector bcompvect. Must be a nonnegative integer. (0, 1, 2, ...). Represents the number of complete breeding cycles the sampled individual has undergone before the checkpoint, where the time between birth and first reproduction is coded as '0', that between first and second reproduction '1', etc. (default 0). Only use in spp. where there is likely to be a reasonable equivalence between breeding stages across a lifespan.
#' @param output    string denoting what kind of output to return. If 'confs', a vector of 95% confidence intervals. if 'vect', a vector of all permutated axial value results
#'
#' @return If output = 'confs' returns vector of 95% confidence intervals (with mean).
#' If output = 'vect' returns vector of individual axial estimates from each permutation
#' @export
#' @family axstandard
#'
#' @examples
#' cous <- rexp(100, 1 / 100)
#' fullsibs <- rexp(50, 1 / 50)
#' axpermute_standard(cous, fullsibs, acat = "1C", bcat = "FS")
axpermute_standard <- function(avect = NULL, bvect = NULL, acat = NULL, bcat = NULL, nreps = 1000, nsamp = "std",
                               amix = FALSE, bmix = FALSE, amixcat = NULL, bmixcat = NULL, acomp = FALSE, bcomp = FALSE,
                               acompvect = NULL, bcompvect = NULL, acompcat = NULL, bcompcat = NULL, acycle = NULL,
                               bcycle = NULL, amixcycle = NULL, bmixcycle = NULL, acompcycle = NULL, bcompcycle = NULL,
                               output = "confs") {
  if (is.KinPairData(avect)) {
    if (is.null(acat)) acat <- kinship(avect)
    if (is.null(acycle)) acycle <- breeding_cycle(avect)
    avect <- distances(avect)
  }
  if (is.KinPairData(bvect)) {
    if (is.null(bcat)) bcat <- kinship(bvect)
    if (is.null(bcycle)) bcycle <- breeding_cycle(bvect)
    bvect <- distances(bvect)
  }
  if (is.KinPairData(acompvect)) {
    if (is.null(acompcat)) acompcat <- kinship(acompvect)
    if (is.null(acompcycle)) acompcycle <- breeding_cycle(acompvect)
    acompvect <- distances(acompvect)
  }
  if (is.KinPairData(bcompvect)) {
    if (is.null(bcompcat)) bcompcat <- kinship(bcompvect)
    if (is.null(bcompcycle)) bcompcycle <- breeding_cycle(bcompvect)
    bcompvect <- distances(bcompvect)
  }

  # Run tests - these check basic pairings between categories
  if (is.null(avect)) {
    stop("Please supply kin dispersal distance vector a!")
  }
  if (is.null(bvect)) {
    stop("Please supply kin dispersal distance vector b!")
  }
  if (is.null(acat)) {
    stop("Please supply kin category a!")
  }
  if (is.null(bcat)) {
    stop("Please supply kin category b!")
  }
  if (is.null(acycle)) acycle <- 0
  if (is.null(bcycle)) bcycle <- 0

  aphase <- phase_assigner(acat)
  bphase <- phase_assigner(bcat)
  aphasetest <- aphase
  bphasetest <- bphase

  aspan <- span_assigner(acat) + cycle_to_span(acycle)
  bspan <- span_assigner(bcat) + cycle_to_span(bcycle)
  aspantest <- aspan
  bspantest <- bspan

  if (nsamp == "std") {
    anum <- length(avect)
    bnum <- length(bvect)
    if (anum > 1000) {
      message("More than 1,000 kinpairs in vector avect: setting permutation sample number to 1,000")
      anum <- 1000
    }
    if (bnum > 1000) {
      message("More than 1,000 kinpairs in vector bvect: setting permutation sample number to 1,000")
      bnum <- 1000
    }
  }
  else {
    anum <- bnum <- nsamp
  }

  if (amix == TRUE) {
    if (is.null(amixcat)) {
      stop("please supply mixture kin category for a!")
    }
    if (is.null(amixcycle)) amixcycle <- 0
    if (bcomp == FALSE & bmix == FALSE) {
      stop("Mixed category a must be paired with a mixed category or composite b!")
    }
    if (acomp == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both == TRUE)")
    }
    amixphase <- phase_assigner(amixcat)
    aphasetest <- c(aphasetest, amixphase)
    amixspan <- span_assigner(amixcat) + cycle_to_span(amixcycle)
    aspantest <- mean(c(aspantest, amixspan))
  }
  if (bmix == TRUE) {
    if (is.null(bmixcat)) {
      stop("please supply mixture kin category for b!")
    }
    if (is.null(bmixcycle)) bmixcycle <- 0
    if (acomp == FALSE & amix == FALSE) {
      stop("Mixed category b must be paired with a mixed category or composite a!")
    }
    if (bcomp == TRUE) {
      stop("Estimate b cannot be both composite & mixed ('bmix' & 'bcomp' both == TRUE)")
    }
    bmixphase <- phase_assigner(bmixcat)
    bphasetest <- c(bphasetest, bmixphase)
    bmixspan <- span_assigner(bmixcat) + cycle_to_span(bmixcycle)
    bspantest <- mean(c(bspantest, bmixspan))
  }

  if (acomp == TRUE) {
    if (is.null(acompvect)) {
      stop("please supply composite vector for a!")
    }
    if (is.null(acompcycle)) acompcycle <- 0
    if (bcomp == FALSE & bmix == FALSE) {
      stop("Composite estimate a must be paired with a mixed category or composite b!")
    }
    if (is.null(acompcat)) {
      stop("Please supply kin category for composite a")
    }
    if (amix == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both TRUE)")
    }
    acompphase <- phase_assigner(acompcat)
    aphasetest <- c(aphasetest, acompphase)
    acompspan <- span_assigner(acompcat) + cycle_to_span(acompcycle)
    aspantest <- mean(c(aspantest, acompspan))
    if (nsamp == "std") {
      acompnum <- length(acompvect)
      if (acompnum > 1000) {
        message("More than 1,000 kinpairs in vector acompvect: setting permutation sample number to 1,000")
        acompnum <- 1000
      }
    }
    else {
      acompnum <- nsamp
    }
  }

  if (bcomp == TRUE) {
    if (is.null(bcompvect)) {
      stop("please supply composite vector for b!")
    }
    if (is.null(bcompcycle)) bcompcycle <- 0
    if (acomp == FALSE & amix == FALSE) {
      stop("Composite estimate b must be paired with a mixed category or composite a!")
    }
    if (is.null(bcompcat)) {
      stop("Please supply kin category for composite b")
    }
    if (bmix == TRUE) {
      stop("Estimate a cannot be both composite & mixed ('amix' & 'acomp' both TRUE)")
    }
    bcompphase <- phase_assigner(bcompcat)
    bphasetest <- c(bphasetest, bcompphase)
    bcompspan <- span_assigner(bcompcat) + cycle_to_span(bcompcycle)
    bspantest <- mean(c(bspantest, bcompspan))
    if (nsamp == "std") {
      bcompnum <- length(bcompvect)
      if (bcompnum > 1000) {
        message("More than 1,000 kinpairs in vector bcompvect: setting permutation sample number to 1,000")
        bcompnum <- 1000
      }
    }
    else {
      bcompnum <- nsamp
    }
  }

  if (!identical(sort(aphasetest), sort(bphasetest))) {
    stop(paste0("A and B phases are mismatched! A: ", sort(aphasetest), " B: ", sort(bphasetest)))
  }
  if (aspantest <= bspantest) {
    stop(paste0("A categories should contain more dispersed kin categories than B categories: A spans: ", aspantest, " B spans: ", bspantest))
  }

  if (!output %in% c("confs", "vect")) {
    stop("'output' must be set to either confidence intervals 'confs' or vector 'vect'")
  }

  spandiff <- aspantest - bspantest

  # set up permutations...

  container <- tibble(ax = 0.0, .rows = 0)

  for (val in 1:nreps) {
    asub <- sample(avect, anum, replace = TRUE)
    bsub <- sample(bvect, bnum, replace = TRUE)

    a_ax <- axials(asub)
    b_ax <- axials(bsub)

    if (acomp == TRUE) {
      acompsub <- sample(acompvect, acompnum, replace = TRUE)
      acomp_ax <- axials(acompsub)
      a_ax <- axials_combine(c(a_ax, acomp_ax))
    }

    if (bcomp == TRUE) {
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
    a_ax <- axials(avect)
    b_ax <- axials(bvect)

    if (acomp == TRUE){
      acomp_ax <- axials(acompvect)
      a_ax <- axials_combine(c(a_ax, acomp_ax))
    }

    if (bcomp == TRUE){
      bcomp_ax <- axials(bcompvect)
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

    ci <- stats::quantile(container$ax, c(0.025, 0.975))
    return(c(ci[1], mean = lifeax_final, ci[2]))
  }
  else if (output == "vect") {
    return(sort(container$ax))
  }
}
