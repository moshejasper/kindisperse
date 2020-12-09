
#' Convert dataframe or tibble to KinPairData class
#'
#' @param df data.frame or tibble of kin distances - can contain $distance (kin distances), $category (kin cats) & $lifestage columns
#' @param category character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'larva' or 'oviposition'.
#'
#' @return returns valid \code{KinPairData} object
#' @export
#'
#' @examples
#' mydata <- tibble(distance = 1:10, lifestage = "larva", category = c("FS", "FS", "FS", "FS", "FS", "FS", "HS", "HS", "HS", "HS"))
#' df_to_kinpair(mydata, category = "FS")
df_to_kinpair <- function(data, category = NULL, lifestage = NULL){
  tib <- as_tibble(data)
  if (is.null(category)){
    if ("category" %in% colnames(tib)){
      cats <- unique(tib$category)
      if (length(cats) > 1) stop("More than one kin category present in data!")
      else category <- cats
      check_valid_kinship(tib$category)
    }
  }
  else {
    check_valid_kinship(category)
    ct <- category
    if ("category" %in% colnames(tib)){
      check_valid_kinship(tib$category)
      tib <- filter(tib, .data$category == ct)
    }
  }
  if (is.null(lifestage)){
    if ("lifestage" %in% colnames(tib)){
      stages <- unique(tib$lifestage)
      if (length(stages) > 1) stop("More than one lifestage present in data!")
      else lifestage <- stages
      check_valid_lifestage(tib$lifestage)
    }
  }
  else {
    check_valid_lifestage(lifestage)
    lf <- lifestage
    if ("lifestage" %in% colnames(tib)){
      check_valid_lifestage(tib$lifestage)
      tib <- filter(tib, .data$lifestage == lf)
    }
  }
  KinPairData(data = tib, category = category, lifestage = lifestage)
}

#' Convert vector of kin separation distances to KinPairData class
#'
#' @param vect vector of kinpair distances
#' @param category character or character vector containing kinship categories of kinpairs
#' @param lifestage character or character vector containing lifestages of kinpairs
#'
#' @return returns valid \code{KinPairData} object.
#' @export
#'
#' @examples
#' vector_to_kinpair(1:10, "FS", "larva")
vector_to_kinpair <- function(vect, category=NULL, lifestage=NULL){
  vlength <- length(vect)
  if (is.null(category) & is.null(lifestage)) return(KinPairData(data = vect))
  if (! is.null(category)){
    check_valid_kinship(category)
    if (! length(category) == 1 | length(category) == vlength) stop("Invalid kinship category vector length!")
  }
  else category <- "UN"
  if (! is.null(lifestage)){
    check_valid_lifestage(lifestage)
    if (! length(lifestage) == 1 | length(lifestage) == vlength) stop("Invalid lifestage vector length!")
  }
  else lifestage <- "unknown"
  tib <- tibble(distance = vect, category = category)
  return(df_to_kinpair(data = tib, lifestage = lifestage))
}


#' Check if vector of kinship categories contains all valid entries
#'
#' @param vect vector of kinship categories
#'
#' @return TRUE if valid. Error otherwise.
#'
check_valid_kinship <- function(vect){
  kinvector <- c("PO", "GG", "GGG", "FS", "AV", "GAV", "1C", "1C1", "2C", "HS", "HAV", "HGAV", "H1C", "H1C1", "H2C", "UN")
  for (cat in unique(vect)){
    if (! cat %in% kinvector) stop("Invalid kinship category!")
  }
  TRUE
}

#' Check if vector of lifestages contains all valid entries
#'
#' @param vect vector of lifestages
#'
#' @return TRUE if valid. Error otherwise
#'
#'
check_valid_lifestage <- function(vect){
  lifevector <- c("unknown", "larva", "oviposition")
  for (life in unique(vect)){
    if (! life %in% lifevector) stop("Invalid lifestage!")
  }
  TRUE
}

#' Extract KinPairData class object to tibble
#'
#' @param x object of class \code{KinPairData}
#'
#' @return tibble (class \code{tbl_df})
#' @export
#'
kinpair_to_tibble <- function(x){
  x@tab
}
