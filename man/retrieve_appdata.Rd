% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_ports.R
\name{retrieve_appdata}
\alias{retrieve_appdata}
\title{Retrieve KinPairData object from appdata (single)}
\usage{
retrieve_appdata(nm)
}
\arguments{
\item{nm}{character. Name of item as stored in appdata}
}
\value{
Returns KinPairData object accessible by name nm
}
\description{
Retrieve KinPairData object from appdata (single)
}
\examples{
mount_appdata(KinPairData(), "mydata")

retrieve_appdata("mydata")
}
