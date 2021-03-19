### File writing functions ###

#' Write KinPairData object in .csv format (strips extra data)
#'
#' @param x Oject of class KinPairData or KinPairSimulation
#' @param file The file path to write to
#' @param ... Additional arguments to pass to write_csv
#'
#' @return Invisibly returns the initial object
#' @export
#'
kinpair_to_csv <- function(x, file, ...) {
  if (!is.KinPairData(x)) stop("x is not an object of class KinPairData!")
  write_csv(kinpair_to_tibble(x), file, ...)
  invisible(x)
}

#' Write KinPairData object in .tsv format (strips extra data)
#'
#' @param x Object of class KinPairData or KinPairSimulation
#' @param file The file path to write to
#' @param ... Additional arguments to pass to write_tsv
#'
#' @return Invisibly returns the initial object
#' @export
#'
kinpair_to_tsv <- function(x, file, ...) {
  if (!is.KinPairData(x)) stop("x is not an object of class KinPairData!")
  write_tsv(kinpair_to_tibble(x), file, ...)
  invisible(x)
}

#' Write KinPairData or KinPairSimulation object in .kindata format (no information lost)
#'
#' @param x Object of class KinPairData or KinPairSimulation
#' @param file The file path to write to. If is doesn't end it '.kindata', this will be added.
#'
#' @return Invisibly returns the initial object
#' @export
#'
write_kindata <- function(x, file) {
  if (!is.KinPairData(x)) stop("x is not an object of class KinPairData!")
  if (!str_ends(file, ".kindata")) {
    file <- paste0(file, ".kindata")
    message(paste0("Adding extension '.kindata': final filename '", file))
  }
  write_rds(x, file)
  invisible(x)
}

### File reading functions ###

#' Reads .csv and converts to KinPairData object
#'
#' @param file The file path to read from
#' @param kinship character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'larva' or 'oviposition'.
#' @param ... additional arguments to pass to read_csv
#'
#' @return returns an object of class \code{KinPairData}
#' @export
#'
csv_to_kinpair <- function(file, kinship = NULL, lifestage = NULL, ...) {
  tib <- read_csv(file)
  return(df_to_kinpair(tib, kinship = kinship, lifestage = lifestage))
}

#' Reads .tsv and converts to KinPairData object
#'
#' @param file The file path to read from
#' @param kinship character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'larva' or 'oviposition'.
#' @param ... additional arguments to pass to read_tsv
#'
#' @return Returns object of class \code{KinPairData}
#' @export
#'
tsv_to_kinpair <- function(file, kinship = NULL, lifestage = NULL, ...) {
  tib <- read_tsv(file)
  return(df_to_kinpair(tib, kinship = kinship, lifestage = lifestage))
}

#' Reads .kindata filetype back to KinPairData or KinPairSimulation object.
#'
#' @param file Character giving path reference to file with extension .kinpair
#'
#' @return Returns either \code{KinPairData} or \code{KinPairSimulation} object.
#' @export
#'
read_kindata <- function(file) {
  if (str_ends(file, ".kindata")) {
    kinpair <- read_rds(file)
  } else if (file.exists(paste0(file, ".kindata"))) {
    kinpair <- read_rds(paste0(file, ".kindata"))
  } else {
    stop("Invalid file extension!")
  }
  if (!is.KinPairData(kinpair)) stop("Could not retrieve object of class KinPairData!")
  return(kinpair)
}
