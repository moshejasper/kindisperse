#' Title
#'
#' @param valvect
#' @param composite
#'
#' @return
#' @export
#'
#' @examples
axials <- function(valvect, composite = 1){ #  computes axial distance for set... make better name...
  vals <- (valvect^2)/(2 * composite)
  return(sqrt(mean(vals)))
}

#axials_kurtosis <- function(valvect){
#  nl <- length(valvect)
#  return(kurtosis(valvect * cos(runif(nl*100, 0, 2 * pi))))
#}

axials_norm <- function(valvect){ # wrapper for axials, but assumes distribution of two sampling outcomes  (e.g. FS) rather than one (e.g. PO) - norm = normal
  return(axials(valvect, 2))
}

#' Title
#'
#' @param sd
#' @param n_composites
#'
#' @return
#' @export
#'
#' @examples
axials_decompose <- function(sd, n_composites = 2){ # adjusts for number of (equal) combinations led to this value. (need to think of examples)...
  return(sd/ sqrt(n_composites))
}

#' Title
#'
#' @param axvals
#'
#' @return
#' @export
#'
#' @examples
axials_combine <- function(axvals){ # for when your data is an even mix of two different dispersal types... (e.g. H1C & 1C). - a blunt instrument...
  n <- length(axvals)
  return(sqrt(sum(axvals^2)/n))
}

#' Title
#'
#' @param axvals
#'
#' @return
#' @export
#'
#' @examples
axials_add <- function(axvals){ # for when there are multiple components summing together... (e.g. PO + PO, etc.)...
  return(sqrt(sum(axvals)))
}

#' Title
#'
#' @param abig
#' @param asmall
#'
#' @return
#' @export
#'
#' @examples
axials_subtract <- function(abig, asmall){ # this is standard for our estimates... returns the non-sharecd component between then
  return(sqrt(abig^2 - asmall^2))
}
axmed <- function(ax){ # returns median distance of this distribution (at least, under normal assumptions?) - need to brush up on the stats.
  return(ax * 5 / (3 * sqrt(2)))
}

axconfs <- function(axvals, nreps=1000, composite = 2, output = "confs"){ # this is the process we used in the paper... (better than individual confs). - less likely to fail! (if it fails, then overlap with zero).
  container <- tibble(ax = 0.0, .rows = 0)
  sampnum <- length(axvals)
  for (val in 1:nreps){
    subvals <- sample(axvals, sampnum, replace = TRUE)
    newax <- axials(subvals, composite)
    container <- add_row(container, ax = newax)
  }
  if (output == "confs"){
    return(quantile(container$ax, c(0.025, 0.5, 0.975)))
  }
  else if (output == "vect") {
    return(container$ax)
  }
}

axpermute <- function(axvals, nreps=1000, num = 50, composite = 2, output = "confs"){ # a more customisable version of the above (primarily for simulated distributions)... [distinguish between them? ] -or make above special case of this one?
  container <- tibble(ax = 0.0, .rows = 0)
  sampnum <- num
  for (val in 1:nreps){
    subvals <- sample(axvals, sampnum, replace = TRUE)
    newax <- axials(subvals, composite)
    container <- add_row(container, ax = newax)
  }
  if (output == "confs"){
    return(quantile(container$ax, c(0.025, 0.5, 0.975)))
  }
  else if (output == "vect") {
    return(container$ax)
  }
}

#' Decompose distributions with confidence intervals
#' @description Find the difference between two different axial distributions with confidence intervals.
#' @param bigvals
#' @param smallvals
#' @param nreps
#' @param num
#' @param composite
#' @param output
#'
#' @return
#' @export
#'
#' @examples
axpermute_subtract <- function(bigvals, smallvals, nreps = 1000, num = 50, composite = 2, output = "confs"){ # the workhorse function - need to extend for more complex form!.

  container <- tibble(ax = 0.0, .rows = 0)
  for (val in 1:nreps){
    sub1 <- sample(bigvals, num, replace = T)
    sub2 <- sample(smallvals, num, replace = T)
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
    return(quantile(container$ax, c(0.025, 0.5, 0.975)))
  }
  else if (output == "vect") {
    return(container$ax)
  }
}

phase_assigner <- function(category){
  if (category %in% c("PO", "GG", "GGG")) {phase <- "PO"}
  else if (category %in% c("FS", "AV", "1C", "GAV", "1C1", "2C")) { phase <- "FS"}
  else if (category %in% c("HS", "HAV", "H1C", "HGAV", "H1C1", "H2C")) {phase <- "HS"}
  else {stop("Invalid category!")}
  return(phase)
}

span_assigner <- function(category){

  if (category %in% c("FS", "HS", "PO", "AV", "HAV", "GG", "GAV", "GHAV", "GGG")) { span1 <- 0}
  if (category %in% c("1C", "H1C", "1C1", "H1C1")) { span1 <- 1}
  if (category %in% c("2C", "H2C")) { span1 <- 2}

  if (category %in% c("FS", "HS", "PO")) { span2 <- 0}
  if (category %in% c("AV", "HAV", "1C", "H1C", "GG")) { span2 <- 1}
  if (category %in% c("GAV", "GHAV", "GGG", "1C1", "H1C1", "2C", "H2C")) { span2 <- 2}

  return(span1 + span2)
}

#axkconf <- function(axvals, nreps = 1000){
#  container <- tibble(kurt = 0.0, .rows = 0)
##  sampnum <- length(axvals)
#  for (val in 1:nreps){
#    subvals <- sample(axvals, sampnum, replace = TRUE)
#    newkurt <- axials_kurtosis(subvals)
#    container <- container %>% add_row(kurt = newkurt)
#  }
#  return(quantile(container$kurt, c(0.025, 0.5, 0.975)))
#}
