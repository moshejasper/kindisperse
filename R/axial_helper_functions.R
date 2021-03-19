#' Estimate the axial dispersal distance of a kernel
#'
#' @param valvect A numeric vector of distances between close kin OR an object of class KinPairData
#' @param composite numeric. The number of separate 'draws' (dispersal events)
#' from the kernel required to produce the final positions of the measured individuals.
#' For example, the displacement of a child from parent at the same lifestage would involve 1 draw and thus be composite = 1.
#' Two full siblings would be two draws (composite = 2) from the FS kernel. Non-symmetric relationships (e.g. AV, 1C)
#' should not be decomposed using this method, nor should any assumptions be made about different kernels
#' (e.g. the 1C relationship would appropriately be given the value 2, but not 4)
#'
#'
#' @return Returns the value of the estimated axial dispersal distance of the kernel producing the dispersal distances measured. (numeric)
#' @export
#'
#' @examples
#' po_dists <- c(5, 6, 7.5)
#' axials(po_dists) # one 'draw' (dispersal event) goes into the parent offspring category
#' # so composite is left to its default of 1
#'
#' fs_dists <- c(2, 3, 3)
#' axials(fs_dists, composite = 2) # two 'draws' (symmetric dispersal events)
#' # go into the full sibling category so composite is set to 2
axials <- function(valvect, composite = 1) { #  computes axial distance for set... make better name...
  if (is.KinPairData(valvect)) valvect <- distances(valvect)
  vals <- (valvect^2) / (2 * composite)
  return(sqrt(mean(vals)))
}

# axials_kurtosis <- function(valvect){
#  nl <- length(valvect)
#  return(kurtosis(valvect * cos(runif(nl*100, 0, 2 * pi))))
# }

axials_norm <- function(valvect) { # wrapper for axials, but assumes distribution of two sampling outcomes  (e.g. FS) rather than one (e.g. PO) - norm = normal
  return(axials(valvect, 2))
}

#' Decompose an axial distribution into simple components
#'
#' @description Decomposes an axial distribution into simple components. Note that this should only be used in the simplest situations.
#'
#' @param ax  numeric. The axial value to be decomposed.
#' @param n_composites numeric. The number of separate 'draws' (dispersal events)
#' from the kernel required to produce the final positions of the measured individuals.
#' For example, the displacement of a child from parent at the same life stage would involve 1 draw and thus be composite = 1.
#' Two full siblings would be two draws (composite = 2) from the FS kernel. Non-symmetric relationships (e.g. AV, 1C)
#' should not be decomposed using this method, nor should any assumptions be made about different kernels
#' (e.g. the 1C relationship would appropriately be given the value 2, but not 4)
#'
#' @return Returns the (numeric) axial distribution value of the underlying dispersal kernel from which the composite kernel was (or could be) created.
#' @export
#'
#' @examples
#' fs_vect <- c(10, 11, 12)
#' fs_axial_raw <- axials(fs_vect, composite = 1) # composite hasn't corrected for two dispersal events
#' # inherent to this kin category!
#' fs_axial_final <- axials_decompose(fs_axial_raw, n_composites = 2)
axials_decompose <- function(ax, n_composites = 2) { # adjusts for number of (equal) combinations led to this value. (need to think of examples)...
  return(ax / sqrt(n_composites))
}

#' Combine axial distributions to produce a mixed distribution
#'
#' @description combines axial distributions to produce a mixed distribution.
#' This is useful in settings where you have two separate distributions (e.g. FS & HS) with their own axial values,
#' but you want to average them appropriately so that they can be compared to e.g.
#' a mixed distribution of full & half cousins which cannot be distinguished via kinship determination methods
#' and thus are best treated as an even mixture of the two categories. Different to adding dispersal events.
#'
#' @param axvals numeric. vector of axial distribution values from different kernels that are to be combined
#'
#' @return \code{numeric} Returns the axial value that results from combining the input axial values under an additive variance framework.
#' @export
#'
#' @examples
#' fullax <- axials(c(2, 4, 5), composite = 2)
#' halfax <- axials(c(6, 5, 7), composite = 2)
#' sibax <- axials_combine(c(fullax, halfax))
axials_combine <- function(axvals) { # for when your data is an even mix of two different dispersal types... (e.g. H1C & 1C). - a blunt instrument...
  n <- length(axvals)
  return(sqrt(sum(axvals^2) / n))
}

#' Add axial distributions
#'
#' @description Add axial distributions. Useful to construct an overall distribution that results from multiple 'draws'
#' from smaller distributions. E.g. The pathway between first cousins which can be found by adding
#' each of the component distributions of their respective lifespans along with the relevant offspring producing
#' (e.g. oviposition) of the parent.
#'
#' @param axvals numeric. vector of axial distribution values from different kernels that are to be added.
#'
#' @return \code{numeric} Returns the axial value that results from adding the input axial values under an additive variance framework.
#' @export
#'
#' @examples
#' fullsibs_ax <- 5
#' parent_offspring_ax <- 25
#' cousin_ax <- axials_add(c(fullsibs_ax, parent_offspring_ax))
axials_add <- function(axvals) { # for when there are multiple components summing together... (e.g. PO + PO, etc.)...
  return(sqrt(sum(axvals^2)))
}

#' Subtract axial distributions
#'
#' @description Subtract axial distributions, finding the difference (under an additive variance framework).
#' This is most useful when one distribution subsumes another and includes a unique dispersal event that needs to be extracted.
#' For example, the FS category is subsumed by the 1C category, which can be written 'FS + PO'.
#' In this circumstance, subtracting FS from 1C will yield an estimate of the PO kernel (the basic intergenerational dispersal kernel)
#'
#' @param abig  numeric. The axial dispersal distance of the larger (subsuming) distribution (e.g. 1C).
#' @param asmall numeric. The axial dispersal distance of the smaller (subsumed) distribution (e.g. FS).
#'
#' @return \code{numeric} Returns an estimate of the axial dispersal distance of those dispersal elements that are unique to the larger dispersal distribution (e.g. PO).
#' @export
#'
#' @examples
#' axials_subtract(100, 70)
axials_subtract <- function(abig, asmall) { # this is standard for our estimates... returns the non-sharecd component between then
  return(sqrt(abig^2 - asmall^2))
}
axmed <- function(ax) { # returns median distance of this distribution (at least, under normal assumptions?) - need to brush up on the stats.
  return(ax * 5 / (3 * sqrt(2)))
}


#' Estimate the axial dispersal distance of a kernel with confidence intervals
#'
#' @param vals numeric. Vector of distances between close kin OR object of class KinPairData.
#' @param nreps numeric. Number of permutations to run for confidence intervals (default 1000)
#' @param nsamp numeric. Number of kin pairs to subsample for each permutation.
#' Either "std" or an integer. If "std" will be computed as equal to the sample size. (default "std")
#' @param composite numeric. The number of separate 'draws' (dispersal events)
#' from the kernel required to produce the final positions of the measured individuals.
#' For example, the displacement of a child from parent at the same lifestage would involve 1 draw and thus be composite = 1.
#' Two full siblings would be two draws (composite = 2) from the FS kernel. Non-symmetric relationships (e.g. AV, 1C)
#' should not be decomposed using this method, nor should any assumptions be made about different kernels
#' (e.g. the 1C relationship would appropriately be given the value 2, but not 4)
#' @param output character. Denotes what kind of output to return.
#' If 'confs', a vector of 95% confidence intervals. if 'vect', a vector of all permuted axial value results
#'
#' @return If ouput = 'confs', returns a \code{numeric vector} of 95% confidence intervals and mean axial value.
#' if output = 'vect', returns a \code{numeric vector} of all permuted axial value results
#' @export
#'
#' @examples
#' po_dists <- rexp(100, 1 / 50)
#' axpermute(po_dists, composite = 1)
axpermute <- function(vals, nreps = 1000, nsamp = "std", composite = 1, output = "confs") {
  if (is.KinPairData(vals)) vals <- distances(vals)
  container <- tibble(ax = 0.0, .rows = 0)
  if (nsamp == "std") {
    sampnum <- length(vals)
    if (sampnum > 1000) {
      message("More than 1,000 kinpairs in vector vals: setting permutation sample number to 1,000")
      sampnum <- 1000
    }
  }
  else {
    sampnum <- nsamp
  }
  for (val in 1:nreps) {
    subvals <- sample(vals, sampnum, replace = TRUE)
    newax <- axials(subvals, composite)
    container <- add_row(container, ax = newax)
  }
  if (output == "confs") {
    meanval <- axials(vals, composite)
    ci <- quantile(container$ax, c(0.025, 0.975))
    return(c(ci[1], mean = meanval, ci[2]))
  }
  else if (output == "vect") {
    return(container$ax)
  }
}

#' Subtract axial distributions with confidence intervals
#'
#' @description Find the difference between two different empirical axial distributions with confidence intervals.
#' This is most useful when one distribution subsumes another and includes a unique dispersal event that needs to be extracted.
#' For example, the FS category is subsumed by the 1C category, which can be written 'FS + PO'.
#' In this circumstance, subtracting FS from 1C will yield an estimate of the PO kernel (the basic intergenerational dispersal kernel)
#'
#' @param bigvals numeric. Vector of distance distributions of the larger (subsuming) distribution (e.g. 1C)  OR object of class KinPairData.
#' @param smallvals numeric. Vector of distance distributions of the smaller (subsumed) distribution (e.g. FS)  OR object of class KinPairData.
#' @param nreps numeric. Number of permutations to perform when generating confidence intervals.
#' @param nsamp numeric. number of kin pairs to subsample for each permutation. Either "std" or an integer.
#' If "std" will be computed as equal to the sample size. (default "std")
#' @param composite numeric. The number of separate 'draws' (dispersal events)
#' from the kernel required to produce the final positions of the measured individuals.
#' For example, the displacement of a child from parent at the same lifestage would involve 1 draw and thus be composite = 1.
#' Two full siblings would be two draws (composite = 2) from the FS kernel. Non-symmetric relationships (e.g. AV, 1C)
#' should not be decomposed using this method, nor should any assumptions be made about different kernels
#' (e.g. the 1C relationship would appropriately be given the value 2, but not 4)
#' @param output character. What kind of output to return.
#' Either 'confs' (default -> confidence intervals) or 'vect -> vector of axial distances
#'
#' @return If output = 'confs' returns \code{numeric vector} of 95% confidence intervals and mean axial value.
#' If output = 'vect' returns \code{numeric vector} of individual axial estimates from each permutation
#' @export
#'
#' @examples
#' firstcous <- rexp(100, 1 / 80)
#' fullsibs <- rexp(100, 1 / 50)
#' axpermute_subtract(firstcous, fullsibs)
axpermute_subtract <- function(bigvals, smallvals, nreps = 1000, nsamp = "std", composite = 2, output = "confs") { # the workhorse function - need to extend for more complex form!.

  container <- tibble(ax = 0.0, .rows = 0)

  if (is.KinPairData(bigvals)) bigvals <- distances(bigvals)
  if (is.KinPairData(smallvals)) smallvals <- distances(smallvals)
  if (nsamp == "std") {
    anum <- length(bigvals)
    bnum <- length(smallvals)
    if (anum > 1000) {
      message("More than 1,000 kinpairs in vector bigvals: setting permutation sample number to 1,000")
      anum <- 1000
    }
    if (bnum > 1000) {
      message("More than 1,000 kinpairs in vector smallvals: setting permutation sample number to 1,000")
      bnum <- 1000
    }
  }
  else {
    anum <- bnum <- nsamp
  }
  for (val in 1:nreps) {
    sub1 <- sample(bigvals, anum, replace = TRUE)
    sub2 <- sample(smallvals, bnum, replace = TRUE)
    bigax <- axials(sub1, composite)
    smallax <- axials(sub2, composite)
    if (bigax < smallax) { # here, -1 substitutes for NaN to allow function to continue... needs to be worked into the piece
      newax <- -1
    }
    else {
      newax <- axials_subtract(bigax, smallax)
    }
    container <- add_row(container, ax = newax)
  }
  if (output == "confs") {
    bigax <- axials(bigvals)
    smallax <- axials(smallvals)
    if (bigax < smallax) {
      newax <- -1
    }
    else {
      finax <- axials_subtract(bigax, smallax)
    }

    ci <- quantile(container$ax, c(0.025, 0.975))
    return(c(ci[1], mean = finax, ci[2]))
  }
  else if (output == "vect") {
    return(container$ax)
  }
}

phase_assigner <- function(category) {
  if (category %in% c("PO", "GG", "GGG")) {
    phase <- "PO"
  }
  else if (category %in% c("FS", "AV", "1C", "GAV", "1C1", "2C")) {
    phase <- "FS"
  }
  else if (category %in% c("HS", "HAV", "H1C", "HGAV", "H1C1", "H2C")) {
    phase <- "HS"
  }
  else {
    stop("Invalid category!")
  }
  return(phase)
}

span_assigner <- function(category) {
  if (category %in% c("FS", "HS", "PO", "AV", "HAV", "GG", "GAV", "HGAV", "GGG")) {
    span1 <- 0
  }
  if (category %in% c("1C", "H1C", "1C1", "H1C1")) {
    span1 <- 1
  }
  if (category %in% c("2C", "H2C")) {
    span1 <- 2
  }

  if (category %in% c("FS", "HS", "PO")) {
    span2 <- 0
  }
  if (category %in% c("AV", "HAV", "1C", "H1C", "GG")) {
    span2 <- 1
  }
  if (category %in% c("GAV", "HGAV", "GGG", "1C1", "H1C1", "2C", "H2C")) {
    span2 <- 2
  }

  return(span1 + span2)
}

# axkconf <- function(axvals, nreps = 1000){
#  container <- tibble(kurt = 0.0, .rows = 0)
##  sampnum <- length(axvals)
#  for (val in 1:nreps){
#    subvals <- sample(axvals, sampnum, replace = TRUE)
#    newkurt <- axials_kurtosis(subvals)
#    container <- container %>% add_row(kurt = newkurt)
#  }
#  return(quantile(container$kurt, c(0.025, 0.5, 0.975)))
# }
