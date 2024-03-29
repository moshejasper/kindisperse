% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_kinpairs.R
\name{kinpair_to_tibble}
\alias{kinpair_to_tibble}
\title{Extract KinPairData class object to tibble}
\usage{
kinpair_to_tibble(x)
}
\arguments{
\item{x}{object of class \code{KinPairData}}
}
\value{
tibble (class \code{tbl_df})
}
\description{
Extract \code{KinPairData} class object to tibble. Strips out most class metadata leaving a
dataframe of disersal simulation data with a column added covering lifestage at sampling.
}
\seealso{
Other export_functions: 
\code{\link{kinpair_to_csv}()},
\code{\link{kinpair_to_tsv}()},
\code{\link{write_kindata}()}
}
\concept{export_functions}
