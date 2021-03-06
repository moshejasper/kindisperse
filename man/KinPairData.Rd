% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KinPairData.R
\name{KinPairData}
\alias{KinPairData}
\title{Formal class "KinPairData"}
\usage{
KinPairData(data = NULL, kinship = NULL, lifestage = NULL)

KinPairData(data = NULL, kinship = NULL, lifestage = NULL)
}
\arguments{
\item{data}{tlb_df. Tibble of kinpair distances}

\item{kinship}{character. - one of PO, FS, HS, AV, HAV, GG, 1C, H1C, GAV, HGAV, 1C1, H1C1, GGG, 2C, H2C & UN.}

\item{lifestage}{character. - one of 'unknown', 'larva' or 'oviposition'}
}
\value{


returns an object of class \code{KinPairData}
}
\description{
The class \code{KinPairData} is a formal (S4) class for storing kinship and lifespan dispersal information concerning kin pairs.
}
\section{Slots}{

\describe{
\item{\code{kinship}}{character.}

\item{\code{lifestage}}{character.}

\item{\code{tab}}{tbl_df.}
}}

\examples{
KinPairData()
}
