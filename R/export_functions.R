### File writing functions ###

#' Write \code{KinPairData} object to \code{.csv} format
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects. Writing
#' to .csv or .tsv formats strips most \code{KinPairData} & \code{KinPairSimulation} class metadata and leaves a delimited file
#' containing ids, kinship category, geographical distance, & x & y coordinates for each simulated pair. (removes class attributes)
#' @param x Object of class \code{KinPairData} or \code{KinPairSimulation}
#' @param file The file path to write to
#' @param ... Additional arguments to pass to \code{write_csv}
#'
#' @return Invisibly returns the initial object
#' @export
#' @family export_functions
#'
kinpair_to_csv <- function(x, file, ...) {
  if (!is.KinPairData(x)) stop("x is not an object of class KinPairData!")
  write_csv(kinpair_to_tibble(x), file, ...)
  invisible(x)
}

#' Write \code{KinPairData} object to \code{.tsv} format
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects. Writing
#' to .csv or .tsv formats strips most \code{KinPairData} & \code{KinPairSimulation} class metadata and leaves a delimited file
#' containing ids, kinship category, geographical distance, & x & y coordinates for each simulated pair. (removes class attributes)
#' @param x Object of class \code{KinPairData} or \code{KinPairSimulation}
#' @param file The file path to write to
#' @param ... Additional arguments to pass to \code{write_tsv}
#'
#' @return Invisibly returns the initial object
#' @export
#' @family export_functions
#'
kinpair_to_tsv <- function(x, file, ...) {
  if (!is.KinPairData(x)) stop("x is not an object of class KinPairData!")
  write_tsv(kinpair_to_tibble(x), file, ...)
  invisible(x)
}

#' Write \code{KinPairData} or \code{KinPairSimulation} object in \code{.kindata} format
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects.
#' Writing to the custom \code{.kindata} format enables complete preservation of \code{KinPairData} & \code{KinPairSimulation} formats
#' without any loss of class attributes or metadata - ideal for saving simulation data that is intended for further in-package
#' processing with kindisperse.
#' @param x Object of class \code{KinPairData} or \code{KinPairSimulation}
#' @param file The file path to write to. If is doesn't end it '\code{.kindata}', this will be added.
#'
#' @return Invisibly returns the initial object
#' @export
#' @family export_functions
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

#' Reads \code{.csv} and converts to \code{KinPairData} object
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects.
#'
#' \code{.csv} & \code{.tsv} reading functions at minimum require the .delim file to contain a column titled 'distance' containing distances
#' between kin pairs. It can optionally contain a column of kinship values 'kinship' as well as a column of lifestage values 'lifestage'.
#' If the file contains more than one value in the kinship or lifestage columns (e.g. bot 'FS' and 'HS') - the corresponding function
#' parameter must be set to pick a corresponding subset of dispersed pairs. where parameters are set in the absence of file columns,
#' these values are assigned to the returned \code{KinPairData} object.
#' @param file The file path to read from
#' @param kinship character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'immature' or 'ovipositional'.
#' @param ... additional arguments to pass to \code{read_csv}
#'
#' @return returns an object of class \code{KinPairData}
#' @export
#' @family import_functions
#'
csv_to_kinpair <- function(file, kinship = NULL, lifestage = NULL, ...) {
  tib <- read_csv(file)
  return(df_to_kinpair(tib, kinship = kinship, lifestage = lifestage))
}

#' Reads \code{.tsv} and converts to \code{KinPairData} object
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects.
#'
#' \code{.csv} & \code{.tsv} reading functions at minimum require the .delim file to contain a column titled 'distance' containing distances
#' between kin pairs. It can optionally contain a column of kinship values 'kinship' as well as a column of lifestage values 'lifestage'.
#' If the file contains more than one value in the kinship or lifestage columns (e.g. bot 'FS' and 'HS') - the corresponding function
#' parameter must be set to pick a corresponding subset of dispersed pairs. where parameters are set in the absence of file columns,
#' these values are assigned to the returned \code{KinPairData} object.
#' @param file The file path to read from
#' @param kinship character. kin category to assign or extract from data. one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C , H1C1 or H2C
#' @param lifestage character. lifestage to assign or extract from data. one of 'unknown', 'immature' or 'ovipositional'.
#' @param ... additional arguments to pass to \code{read_tsv}
#'
#' @return Returns object of class \code{KinPairData}
#' @export
#' @family import_functions
#'
tsv_to_kinpair <- function(file, kinship = NULL, lifestage = NULL, ...) {
  tib <- read_tsv(file)
  return(df_to_kinpair(tib, kinship = kinship, lifestage = lifestage))
}

#' Reads \code{.kindata} filetype back to \code{KinPairData} or \code{KinPairSimulation} object.
#'
#' @description This function is part of suite of functions handling file import/export for kinship dispersal objects.
#'
#' The custom \code{.kindata} format enables complete preservation of \code{KinPairData} & \code{KinPairSimulation} formats
#' without any loss of class attributes or metadata - ideal for saving and retrieving simulation data that is intended for further in-package
#' processing with kindisperse. This function loads a previously stored object into its original class format.
#' @param file Character giving path reference to file with extension \code{.kinpair}
#'
#' @return Returns either \code{KinPairData} or \code{KinPairSimulation} object.
#' @export
#' @family import_functions
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
