% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_kindist_simple.R
\name{simulate_kindist_simple}
\alias{simulate_kindist_simple}
\title{Simulate kin dispersal distance pairs with simple sigma}
\usage{
simulate_kindist_simple(
  nsims = 100,
  sigma = 125,
  dims = 100,
  method = "Gaussian",
  kinship = "PO",
  lifestage = "immature",
  shape = 0.5
)
}
\arguments{
\item{nsims}{(integer) -   number of pairs to simulate}

\item{sigma}{(numeric) -   size of simple (axial) sigma}

\item{dims}{(numeric) -   length of sides of (square) simulated site area}

\item{method}{(character) - kernel shape to use: either 'Gaussian', 'Laplace' or 'vgamma' (variance-gamma)}

\item{kinship}{(character)- kin category to simulate: one of PO, FS, HS, AV, GG, HAV, GGG, 1C, 1C1, 2C, GAV, HGAV, H1C or H2C}

\item{lifestage}{(lifestage) lifestage at sample collection: either 'immature' or 'ovipositional'}

\item{shape}{(numeric) - value of shape parameter to use with 'vgamma' method. Default 0.5. Must be > 0. Increment towards zero for increasingly heavy-tailed (leptokurtic) dispersal}
}
\value{
\preformatted{ returns an object of class \code{\link{KinPairSimulation}} containing simulation details and a \code{tibble} (tab) of simulation values
}
}
\description{
Simulates intergenerational dispersal defined by a simple dispersal sigma (covering the entire lifecycle) and ignoring phase
differences between full & half sibling dispersal categories. Returns an object of class \code{\link{KinPairSimulation}}
}
\details{
This function is one of a family of functions that implement the core intergenerational dispersal simulations
contained in the \code{kindisperse} package. Each of these functions proceeds by the following steps:
\enumerate{
\item identify the pedigree
relationship, dispersal phase (FS, HS & PO) and sampling stage that must be generated;
\item randomly assign a coordinate position
to the 'root' individual within the pedigree (i.e. last common ancestor of the dyad, inclusive);
\item 'disperse' both pathways from
this root position via the appropriately defined phase dispersal (additively via random draws from the underlying statistical
model, defined by an axial standard deviation - sigma);
\item further disperse both phased descendant branches according to the
number of realised breeding dispersal cycles contained in the defining pedigree (additively via random draws from the chosen
underlying statistical model);
\item add displacement caused by dispersal before the sampling point in a similar manner to above,
defining the final positions of the sampled dispersed kin dyads;
\item calculating geographical distances between the
resulting dyads.
}

These simulation functions operate under an additive variance framework: all individual dispersal events are modeled as random
draws from a bivariate probability distribution defined by an axial standard deviation \code{sigma} and (sometimes) a shape
parameter. At present, three such distributions are included as options accessible with the \code{method} parameter: the
bivariate normal distribution '\code{Gaussian}', the bivariate Laplace distribution '\code{Laplace}', and the bivariate
variance-gamma distribution '\code{vgamma}'. The \code{Gaussian} (normal) distribution enables easy compatibility with the
framework under which much population genetic & dispersal theory (isolation by distance, neighbourhoods, etc.) have been
developed. The \code{Laplace} distribution is a multivariate adaptation of the (positive) exponential distribution, and
represents a more 'fat-tailed' (leptokurtic) disperal situation than Gaussian. The \code{vgamma} distribution is a mixture
distribution formed by mixing the gamma distribution with the bivariate normal distribution. The flexibility of this
distribution's \code{shape} parameter enables us to model arbitrarily leptokurtic dispesal kernels, providing a helpful way
to examine the impacts of (e.g.) long distance dispersal on the overall disperal distribution and sampling decisions. A
\code{vgamma} distribution with shape parameter equal to 1 reduces to the bivariate Laplace distribution. As shape approaches
infinity, the \code{vgamma} distribution approaches the bivariate normal distribution. As shape approaches zero, the distribution
becomes increasingly leptokurtic.

The \code{simulate_kindist_simple()} function is the most basic of the simulation functions, ignoring all information about
dispersal phase and treating dispersal with a single sigma corresponding to the entire lifecycle to breeding of the
dispersed individuals. It is useful for exploring simple intergenerational dispersal in a stripped back context; for many
typical contexts involving complex dispersal across different phases of the breeding cycle, the other dispersal simulation
functions would be more suitable.

Following simulation, the results are returned as an object of the specially defined package class \code{\link{KinPairSimulation}},
which stores the simulation results along with information about all simulation parameters, and can be further passed to
sample filtering & dispersal estimation functions.
}
\examples{
test <- simulate_kindist_simple(nsims = 10, sigma = 50, dims = 1000, method = "Laplace")
simulate_kindist_simple(nsims = 10000, sigma = 75, kinship = "PO", lifestage = "ovipositional")
}
\seealso{
Other simulate_kindist: 
\code{\link{simulate_kindist_composite}()},
\code{\link{simulate_kindist_custom}()}
}
\concept{simulate_kindist}
