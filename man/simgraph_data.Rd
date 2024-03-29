% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simgraph_data.R
\name{simgraph_data}
\alias{simgraph_data}
\title{Simple kin dispersal simulation for graphical display. (returns the data side as a tibble).}
\usage{
simgraph_data(nsims = 1000, posigma = 50, dims = 250, kinship = "2C")
}
\arguments{
\item{nsims}{Integer. The number of kin dispersal families to simulate.}

\item{posigma}{Integer. The axial deviation of the (simple) parent-offspring dispersal kernel governing this simulation.}

\item{dims}{Integer. Lays out the length of the sides of a square within which parent individuals are seeded.}

\item{kinship}{Character. Lists the kin category the simulation is reconstructing. One of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV" (no half-categories included)}
}
\value{
Returns a tibble containing the coordinates of the f0 to f2 generations, as well as coordinates and distances relative to the 'focus' kinship categories. (kindist, kinmid, k1 & k2)
}
\description{
Simple kin dispersal simulation for graphical display. (returns the data side as a tibble).
}
\examples{
simgraph_data(nsims = 100, dims = 1000, kinship = "GAV")
}
\seealso{
Other simgraph: 
\code{\link{simgraph_graph}()}
}
\concept{simgraph}
