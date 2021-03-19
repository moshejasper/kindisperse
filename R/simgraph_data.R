#' Simple kin dispersal simulation for graphical display. (returns the data side as a tibble).
#'
#' @param nsims     Integer. The number of kin dispersal families to simulate.
#' @param posigma    Integer. The axial deviation of the (simple) parent-offspring dispersal kernel governing this simulation.
#' @param dims      Integer. Lays out the length of the sides of a square within which parent individuals are seeded.
#' @param kinship  Character. Lists the kin category the simulation is reconstructing. One of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV" (no half-categories included)
#'
#' @return  Returns a tibble containing the coordinates of the f0 to f2 generations, as well as coordinates and distances relative to the 'focus' kinship categories. (kindist, kinmid, k1 & k2)
#' @export
#' @importFrom stats runif rnorm
#' @examples
#' simgraph_data(nsims = 100, dims = 1000, kinship = "GAV")
simgraph_data <- function(nsims = 1000, posigma = 50, dims = 250, kinship = "2C") {

  if (!kinship %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
    stop("Invalid Kinship Category")
  }

  nsims <- nsims
  posigma <- posigma
  dims <- dims

  f0x <- runif(nsims, 0, dims)
  f0y <- runif(nsims, 0, dims)

  f1ax <- rnorm(nsims, 0, posigma) + f0x
  f1ay <- rnorm(nsims, 0, posigma) + f0y
  f1bx <- rnorm(nsims, 0, posigma) + f0x
  f1by <- rnorm(nsims, 0, posigma) + f0y
  f1cx <- rnorm(nsims, 0, posigma) + f0x
  f1cy <- rnorm(nsims, 0, posigma) + f0y

  f2ax <- rnorm(nsims, 0, posigma) + f1ax
  f2ay <- rnorm(nsims, 0, posigma) + f1ay
  f2bx <- rnorm(nsims, 0, posigma) + f1bx
  f2by <- rnorm(nsims, 0, posigma) + f1by
  f3ax <- rnorm(nsims, 0, posigma) + f2ax
  f3ay <- rnorm(nsims, 0, posigma) + f2ay
  f3bx <- rnorm(nsims, 0, posigma) + f2bx
  f3by <- rnorm(nsims, 0, posigma) + f2by


  if (kinship == "PO") {
    k1x <- f0x
    k1y <- f0y
    k2x <- f1ax
    k2y <- f1ay
  }
  if (kinship == "FS" | kinship == "HS") {
    k1x <- f1ax
    k1y <- f1ay
    k2x <- f1bx
    k2y <- f1by
  }
  if (kinship == "AV" | kinship == "HAV") {
    k1x <- f2ax
    k1y <- f2ay
    k2x <- f1bx
    k2y <- f1by
  }
  if (kinship == "GG") {
    k1x <- f0x
    k1y <- f0y
    k2x <- f2ax
    k2y <- f2ay
  }
  if (kinship == "1C") {
    k1x <- f2ax
    k1y <- f2ay
    k2x <- f2bx
    k2y <- f2by
  }
  if (kinship == "GGG") {
    k1x <- f0x
    k1y <- f0y
    k2x <- f3ax
    k2y <- f3ay
  }
  if (kinship == "GAV") {
    k1x <- f3ax
    k1y <- f3ay
    k2x <- f1bx
    k2y <- f1by
  }
  if (kinship == "1C1") {
    k1x <- f3ax
    k1y <- f3ay
    k2x <- f2bx
    k2y <- f2by
  }
  if (kinship == "2C") {
    k1x <- f3ax
    k1y <- f3ay
    k2x <- f3bx
    k2y <- f3by
  }

  kindist <- round(sqrt((k1x - k2x)^2 + (k1y - k2y)^2), digits = 1)
  kinmidx <- (k1x + k2x) / 2
  kinmidy <- (k1y + k2y) / 2


  result <- tibble(
    f0x = f0x, f0y = f0y, f1ax = f1ax, f1ay = f1ay,
    f1bx = f1bx, f1by = f1by, f1cx = f1cx, f1cy = f1cy,
    f2ax = f2ax, f2ay = f2ay, f2bx = f2bx, f2by = f2by,
    f3ax = f3ax, f3ay = f3ay, f3bx = f3bx, f3by = f3by,
    kindist = kindist, kinmidx = kinmidx, kinmidy = kinmidy,
    k1x = k1x, k1y = k1y, k2x = k2x, k2y = k2y,
    posigma = posigma, dims = dims, kinship = kinship #stopgap before we roll this out as family simulation functions.
  )

  return(result)
}
