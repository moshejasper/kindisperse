% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_ports.R
\name{mount_appdata}
\alias{mount_appdata}
\title{Mount KinPairData Objects for use in kindisperse app}
\usage{
mount_appdata(x, nm)
}
\arguments{
\item{x}{An object of class \code{KinPairData} or {KinPairSimulation}}

\item{nm}{character. A name to store the object as}
}
\value{
invisibly returns x.
}
\description{
Mount KinPairData Objects for use in kindisperse app
}
\examples{
mount_appdata(KinPairData(), "mydata")
}
