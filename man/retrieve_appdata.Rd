% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_ports.R
\name{retrieve_appdata}
\alias{retrieve_appdata}
\title{Retrieve \code{KinPairData} object from appdata (single)}
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
This function is part of a suite of functions handling the interface between the kindisperse app & R
package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
accessible within the app and outside of it.

This function accesses the app interface environment and retrieves an object (typically of class \code{KinPairData} or
\code{KinPairSimulation}) with the name \code{nm}, making it accessible from within our outside the app. This can be
used to load simulation objects that were saved from the interface while using the app into the regular R environment
(after closing the app). (The app uses this function internally to load objects from the interface into its own internal
environment for display & processing.)
}
\examples{
mount_appdata(kin_pair_data(), "mydata")

retrieve_appdata("mydata")
}
\seealso{
Other app_ports: 
\code{\link{display_appdata}()},
\code{\link{mount_appdata}()},
\code{\link{reset_appdata}()},
\code{\link{reset_tempdata}()},
\code{\link{retrieve_tempdata}()},
\code{\link{retrieveall_appdata}()},
\code{\link{unmount_appdata}()}
}
\concept{app_ports}
