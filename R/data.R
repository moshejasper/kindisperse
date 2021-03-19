#' Position & kinship information of \emph{Aedes aegypti} from Mentari Court, Malaysia
#'
#' A data file containing the positions & kinship values of 98 \emph{Ae. aegypti} larval
#' kin pairs collected between September 19 & October 10, 2017 in Mentari Court
#' (Petaling Jaya), Malaysia.
#'
#' 162 individuals were sourced as larvae from ovitraps placed in eight apartment
#' buildings (in floors three or four for each), collected over three weeks. Entire
#' larval bodies were extracted and sequenced using the double-digest restriction-site-
#' associated DNA sequencing protocol for \emph{Ae. aegypti}
#' (\doi{10.1186/1471-2164-15-275}. After sequencing & genotyping,
#' Loiselle's \emph{k} was used as an initial estimate of genetic kinship. The program
#' ML-Relate (\doi{10.1111/j.1471-8286.2006.01256.x}) was then used to
#' estimate the pedigree kinships for the FS and HS categories. Following simulation
#' work described in \doi{10.1111/1755-0998.13043} the
#' 1C category was assigned to all remaining unassigned individuals with a Loiselle's
#' \emph{k} of less than 0.06.
#'
#' @format A data frame with 98 rows and 10 variables
#' \describe{
#' \item{id1}{id of first individual of kinpair}
#' \item{id2}{id of second individual of kinpair}
#' \item{kinship}{kinship category of the pairing}
#' \item{distance}{geographical distance between kinpair}
#' \item{x1}{relative x coordinate of first individual in metres}
#' \item{y1}{relative y coordinate of first individual in metres}
#' \item{x2}{relative x coordinate of second individual in metres}
#' \item{y2}{relative y coordinate of second individual in metres}
#' \item{lifestage}{lifestage at time of sampling of kinpair}
#' \item{k_loiselle}{calculated Loiselle's \emph{k} value for kinpair}
#' }
#'
#' @source \doi{10.1111/1755-0998.13043}
#' @return returns an object of class \code{tbl_df}
"mentari"
