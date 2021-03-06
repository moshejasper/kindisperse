% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_kinpairs.R
\name{vector_to_kinpair}
\alias{vector_to_kinpair}
\title{Convert vector of kin separation distances to KinPairData class}
\usage{
vector_to_kinpair(vect, kinship = NULL, lifestage = NULL)
}
\arguments{
\item{vect}{vector of kinpair distances}

\item{kinship}{character or character vector containing kinship categories of kinpairs}

\item{lifestage}{character or character vector containing lifestages of kinpairs}
}
\value{
returns valid \code{KinPairData} object.
}
\description{
Convert vector of kin separation distances to KinPairData class
}
\examples{
vector_to_kinpair(1:10, "FS", "larva")
}
