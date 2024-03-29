% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axial_helper_functions.R
\name{axpermute_subtract}
\alias{axpermute_subtract}
\title{Subtract axial distributions with confidence intervals}
\usage{
axpermute_subtract(
  bigvals,
  smallvals,
  nreps = 1000,
  nsamp = "std",
  composite = 2,
  output = "confs"
)
}
\arguments{
\item{bigvals}{numeric. Vector of distance distributions of the larger (subsuming) distribution (e.g. 1C)  OR object of class KinPairData.}

\item{smallvals}{numeric. Vector of distance distributions of the smaller (subsumed) distribution (e.g. FS)  OR object of class KinPairData.}

\item{nreps}{numeric. Number of permutations to perform when generating confidence intervals.}

\item{nsamp}{numeric. number of kin pairs to subsample for each permutation. Either "std" or an integer.
If "std" will be computed as equal to the sample size. (default "std")}

\item{composite}{numeric. The number of separate 'draws' (dispersal events)
from the kernel required to produce the final positions of the measured individuals.
For example, the displacement of a child from parent at the same lifestage would involve 1 draw and thus be composite = 1.
Two full siblings would be two draws (composite = 2) from the FS kernel. Non-symmetric relationships (e.g. AV, 1C)
should not be decomposed using this method, nor should any assumptions be made about different kernels
(e.g. the 1C relationship would appropriately be given the value 2, but not 4)}

\item{output}{character. What kind of output to return.
Either 'confs' (default -> confidence intervals) or 'vect -> vector of axial distances}
}
\value{
If output = 'confs' returns \code{numeric vector} of 95\% confidence intervals and mean axial value.
If output = 'vect' returns \code{numeric vector} of individual axial estimates from each permutation
}
\description{
Finds the difference between two different empirical axial distributions with confidence intervals.
This is most useful when one distribution subsumes another and includes a unique dispersal event that needs to be extracted.
For example, the FS category is subsumed by the 1C category, which can be written 'FS + PO'.
In this circumstance, subtracting FS from 1C will yield an estimate of the PO kernel (the basic intergenerational dispersal kernel).
Confidence intervals are assigned via bootstrapping, or optionally the
vector of all bootstrapped results can be outputted by setting \code{output} to \code{'vect'}, enabling its passing to other
functions or external statistical analysis.
}
\examples{
firstcous <- rexp(100, 1 / 80)
fullsibs <- rexp(100, 1 / 50)
axpermute_subtract(firstcous, fullsibs)
}
\seealso{
Other axial_helpers: 
\code{\link{axials_add}()},
\code{\link{axials_decompose}()},
\code{\link{axials_subtract}()},
\code{\link{axials}()},
\code{\link{axpermute}()}
}
\concept{axial_helpers}
