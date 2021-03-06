% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axials_standard.R
\name{axials_standard}
\alias{axials_standard}
\title{Takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring (intergenerational) kinship dispersal kernel
Further tests}
\usage{
axials_standard(
  avect,
  bvect,
  acat = NULL,
  bcat = NULL,
  amix = F,
  bmix = F,
  amixcat = NULL,
  bmixcat = NULL,
  acomp = F,
  bcomp = F,
  acompvect = NULL,
  bcompvect = NULL,
  acompcat = NULL,
  bcompcat = NULL
)
}
\arguments{
\item{avect}{vector a of kin dispersal distances for the less closely related kinship category OR object of class KinPairData.}

\item{bvect}{vector b of kin dispersal distances for the more closely related kinship category OR object of class KinPairData.}

\item{acat}{kinship category of kin dispersal vector avect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}

\item{bcat}{kinship category of kin dispersal vector bvect. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}

\item{amix}{logical describing whether vector a is a mixture of two kinship categories. Used with amixcat. Default FALSE.}

\item{bmix}{logical describing whether vector b is a mixture of two kinship categories. Used with bmixcat. Default FALSE.}

\item{amixcat}{mixture kinship category of vector a. Must be set if amix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}

\item{bmixcat}{mixture kinship category of vector b. Must be set if bmix == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}

\item{acomp}{logical denoting whether vector a should be composited with an additional kinship category vector. Used with acompvect and acompcat. Default FALSE.}

\item{bcomp}{logical denoting whether vector b should be composited with an additional kinship category vector. Used with bcompvect and bcompcat. Default FALSE.}

\item{acompvect}{vector acomp of kin dispersal distances for compositing with vector a OR object of class KinPairData. Must be set if acomp == TRUE.}

\item{bcompvect}{vector bcomp of kin dispersal distances for compositing with vector b OR object of class KinPairData. Must be set if bcomp == TRUE.}

\item{acompcat}{kinship category of kin dispersal vector acompvect. Must be set if acomp == TRUE.  Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}

\item{bcompcat}{kinship category of kin dispersal vector bcompvect. Must be set if bcomp == TRUE. Must be one of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV", "HGAV", "H1C", "H1C1", "H2C"}
}
\value{
Returns a numeric estimate of PO (intergenerational) dispersal kernel axial distribution.
}
\description{
Takes (at least) two vectors of kinship dispersal distances from defined kinship categories, and returns a resulting calculation of the parent-offspring (intergenerational) kinship dispersal kernel
Further tests
}
\examples{
cous <- rexp(100, 1 / 100)
fullsibs <- rexp(50, 1 / 50)
axials_standard(cous, fullsibs, acat = "1C", bcat = "FS")
}
