% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_functions.R
\name{write_kindata}
\alias{write_kindata}
\title{Write \code{KinPairData} or \code{KinPairSimulation} object in \code{.kindata} format}
\usage{
write_kindata(x, file)
}
\arguments{
\item{x}{Object of class \code{KinPairData} or \code{KinPairSimulation}}

\item{file}{The file path to write to. If is doesn't end it '\code{.kindata}', this will be added.}
}
\value{
Invisibly returns the initial object
}
\description{
This function is part of suite of functions handling file import/export for kinship dispersal objects.
Writing to the custom \code{.kindata} format enables complete preservation of \code{KinPairData} & \code{KinPairSimulation} formats
without any loss of class attributes or metadata - ideal for saving simulation data that is intended for further in-package
processing with kindisperse.
}
\seealso{
Other export_functions: 
\code{\link{kinpair_to_csv}()},
\code{\link{kinpair_to_tibble}()},
\code{\link{kinpair_to_tsv}()}
}
\concept{export_functions}
