
#' Convert dataframe or tibble to \code{\link{KinPairData}} class
#'
#' @description This function at minimum requires the dataframe to contain a column titled 'distance' containing distances
#' between kin pairs. It can optionally contain a column of kinship values 'kinship' as well as a column of lifestage values 'lifestage'.
#' If the file contains more than one value in the kinship or lifestage columns (e.g. bot 'FS' and 'HS') - the corresponding function
#' parameter must be set to pick a corresponding subset of dispersed pairs. where parameters are set in the absence of file columns,
#' these values are assigned to the returned \code{KinPairData} object.
#' @param data data.frame or tibble of kin distances - can contain $distance (kin distances), $kinship (kin cats) & $lifestage columns
#' @param kinship character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'immature' or 'ovipositional'.
#' @param lifecheck logical. If TRUE (default) tests if lifestage is valid, if FALSE, ignores this test. Set to FALSE when using custom lifestages.
#'
#' @return returns valid \code{KinPairData} object
#' @export
#' @family import_functions
#'
#' @examples
#' mydata <- tibble::tibble(
#'   distance = 1:10, lifestage = "immature",
#'   kinship = c("FS", "FS", "FS", "FS", "FS", "FS", "HS", "HS", "HS", "HS")
#' )
#' df_to_kinpair(mydata, kinship = "FS")
df_to_kinpair <- function(data, kinship = NULL, lifestage = NULL, lifecheck = TRUE) {
  tib <- as_tibble(data)
  if (is.null(kinship)) {
    if ("kinship" %in% colnames(tib)) {
      cats <- unique(tib$kinship)
      if (length(cats) > 1) {
        stop("More than one kin category present in data!")
      } else {
        kinship <- cats
      }
      check_valid_kinship(tib$kinship)
    }
    else {
      kinship <- "UN"
    }
  }
  else {
    check_valid_kinship(kinship)
    ct <- kinship
    if ("kinship" %in% colnames(tib)) {
      check_valid_kinship(tib$kinship)
      tib <- filter(tib, .data$kinship == ct)
    }
  }
  if (is.null(lifestage)) {
    if ("lifestage" %in% colnames(tib)) {
      stages <- unique(tib$lifestage)
      if (length(stages) > 1) {
        stop("More than one lifestage present in data!")
      } else {
        lifestage <- stages
      }
      if (lifecheck) check_valid_lifestage(tib$lifestage)
    }
  }
  else {
    if (lifecheck) check_valid_lifestage(lifestage)
    lf <- lifestage
    if ("lifestage" %in% colnames(tib)) {
      if (lifecheck) check_valid_lifestage(tib$lifestage)
      tib <- filter(tib, .data$lifestage == lf)
    }
  }
  return(kin_pair_data(data = tib, kinship = kinship, lifestage = lifestage))
}

#' Convert vector of kin separation distances to \code{\link{KinPairData}} class
#'
#' @description Function takes at minimum a (numeric) vector of distances between related kinpairs, and
#' returns a \code{KinPairData} object. Optional parameters can assign kinship and lifestage values to the
#' returned object.
#' @param vect vector of kinpair distances
#' @param kinship character or character vector containing kinship categories of kinpairs
#' @param lifestage character or character vector containing lifestages of kinpairs
#'
#' @return returns valid \code{KinPairData} object.
#' @export
#' @family import_functions
#'
#' @examples
#' vector_to_kinpair(1:10, "FS", "immature")
vector_to_kinpair <- function(vect, kinship = NULL, lifestage = NULL) {
  vlength <- length(vect)
  if (is.null(kinship) & is.null(lifestage)) {
    return(KinPairData(data = vect))
  }
  if (!is.null(kinship)) {
    check_valid_kinship(kinship)
    if (!length(kinship) == 1 | length(kinship) == vlength) stop("Invalid kinship category vector length!")
  }
  else {
    kinship <- "UN"
  }
  if (!is.null(lifestage)) {
    check_valid_lifestage(lifestage)
    if (!length(lifestage) == 1 | length(lifestage) == vlength) stop("Invalid lifestage vector length!")
  }
  else {
    lifestage <- "unknown"
  }
  tib <- tibble(distance = vect, kinship = kinship)
  return(df_to_kinpair(data = tib, lifestage = lifestage))
}


#' Check valid kinship
#'
#' @description Checks if vector of kinship categories contains all valid entries
#' @param vect vector of kinship categories
#'
#' @return TRUE if valid. Error otherwise.
#'
check_valid_kinship <- function(vect) {
  kinvector <- c("PO", "GG", "GGG", "FS", "AV", "GAV", "1C", "1C1", "2C", "HS", "HAV", "HGAV", "H1C", "H1C1", "H2C", "UN")
  for (cat in unique(vect)) {
    if (!cat %in% kinvector) stop("Invalid kinship category!")
  }
  TRUE
}

#' Check valid lifestage
#'
#' @description Checks if vector of lifestages contains all valid entries
#' @param vect vector of lifestages
#'
#' @return TRUE if valid. Error otherwise
#'
#'
check_valid_lifestage <- function(vect) {
  lifevector <- c("unknown", "immature", "ovipositional")
  for (life in unique(vect)) {
    if (!life %in% lifevector) stop("Invalid lifestage!")
  }
  TRUE
}

#' Extract KinPairData class object to tibble
#'
#' @description Extract \code{KinPairData} class object to tibble. Strips out most class metadata leaving a
#' dataframe of disersal simulation data with a column added covering lifestage at sampling.
#' @param x object of class \code{KinPairData}
#'
#' @return tibble (class \code{tbl_df})
#' @export
#' @family export_functions
#'
kinpair_to_tibble <- function(x) {
  x_out <- x@tab
  if (!"lifestage" %in% colnames(x_out)) {
    if (!is.null(x@lifestage)) x_out <- add_column(x_out, lifestage = x@lifestage)
  }
  x_out
}
