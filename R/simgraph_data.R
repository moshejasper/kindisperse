#' Simple kin dispersal simulation for graphical display. (returns the data side as a tibble).
#'
#' @param nsims     Integer. The number of kin dispersal families to simulate.
#' @param dsigma    Integer. The axial deviation of the (simple) parent-offspring dispersal kernel governing this simulation.
#' @param dims      Integer. Lays out the length of the sides of a square within which parent individuals are seeded.
#' @param category  Character. Lists the kin category the simulation is reconstructing. One of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV" (no half-categoris included)
#' @param centred   Logical. Whether all parents are treated as originating in the same point (for pinwheel, scatter graphs, etc.)
#' @param pinwheel  Logical. Whether data is being prepared as if for a pinwheel graph.
#' @param scattered Locigal. Whether data is being prepared as if for a scatter graph.
#'
#' @return  Returns a tibble containing the coordinates of the f0 to f2 generations, as well as coordinates and distances relative to the 'focus' kinship categories. (kindist, kinmid, k1 & k2)
#' @export
#' @importFrom stats runif rnorm
#' @examples
#' simgraph_data(nsims = 100, dims = 1000, category = "GAV", centred = TRUE)
simgraph_data <- function(nsims = 10, dsigma = 50, dims = 250, category = "2C",
                          centred = FALSE, pinwheel = FALSE, scattered = FALSE){

  if (! category %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
    stop("Invalid Kinship Category")
  }

  nsims <- nsims
  dsigma <- dsigma
  dims <- dims
  if (pinwheel == TRUE | scattered == TRUE) {centred <- TRUE}
  if (centred == TRUE){
    f0x <- runif(nsims, 0, 0)
    f0y <- runif(nsims, 0, 0)
  }
  if (centred == FALSE){
    f0x <- runif(nsims, 0, dims)
    f0y <- runif(nsims, 0, dims)
    # sample = 10...
  }
  f1ax <- rnorm(nsims, 0, dsigma) + f0x
  f1ay <- rnorm(nsims, 0, dsigma) + f0y
  f1bx <- rnorm(nsims, 0, dsigma) + f0x
  f1by <- rnorm(nsims, 0, dsigma) + f0y
  f1cx <- rnorm(nsims, 0, dsigma) + f0x
  f1cy <- rnorm(nsims, 0, dsigma) + f0y

  f2ax <- rnorm(nsims, 0, dsigma) + f1ax
  f2ay <- rnorm(nsims, 0, dsigma) + f1ay
  f2bx <- rnorm(nsims, 0, dsigma) + f1bx
  f2by <- rnorm(nsims, 0, dsigma) + f1by
  f3ax <- rnorm(nsims, 0, dsigma) + f2ax
  f3ay <- rnorm(nsims, 0, dsigma) + f2ay
  f3bx <- rnorm(nsims, 0, dsigma) + f2bx
  f3by <- rnorm(nsims, 0, dsigma) + f2by


  if (category == "PO"){k1x <- f0x ; k1y <- f0y ; k2x <- f1ax; k2y <- f1ay}
  if (category == "FS" | category == "HS") {k1x <- f1ax ; k1y <- f1ay ; k2x <- f1bx; k2y <- f1by}
  if (category == "AV" | category == "HAV") {k1x <- f2ax ; k1y <- f2ay ; k2x <- f1bx; k2y <- f1by}
  if (category == "GG") {k1x <- f0x ; k1y <- f0y ; k2x <- f2ax; k2y <- f2ay}
  if (category == "1C") {k1x <- f2ax ; k1y <- f2ay ; k2x <- f2bx; k2y <- f2by}
  if (category == "GGG") {k1x <- f0x ; k1y <- f0y ; k2x <- f3ax; k2y <- f3ay}
  if (category == "GAV") {k1x <- f3ax; k1y <- f3ay; k2x <- f1bx; k2y <- f1by}
  if (category == "1C1") {k1x <- f3ax ; k1y <- f3ay ; k2x <- f2bx; k2y <- f2by}
  if (category == "2C") {k1x <- f3ax ; k1y <- f3ay ; k2x <- f3bx; k2y <- f3by}

  if (pinwheel == TRUE | scattered == TRUE) {k1x <- k1x - k1x; k1y <- k1y - k1y; k2x <- k2x - k1x; k2y <- k2y - k1y}
  kindist <- round(sqrt((k1x - k2x)^2 + (k1y - k2y)^2), digits = 1)
  kinmidx <- (k1x + k2x) / 2
  kinmidy <- (k1y + k2y) / 2


  result <- tibble(f0x = f0x, f0y = f0y, f1ax = f1ax, f1ay = f1ay,
                   f1bx = f1bx, f1by = f1by, f1cx = f1cx, f1cy = f1cy,
                   f2ax = f2ax, f2ay = f2ay, f2bx = f2bx, f2by = f2by,
                   f3ax = f3ax, f3ay = f3ay, f3bx = f3bx, f3by = f3by,
                   kindist = kindist, kinmidx = kinmidx, kinmidy = kinmidy,
                   k1x = k1x, k1y = k1y, k2x = k2x, k2y = k2y)

  return(result)
}
