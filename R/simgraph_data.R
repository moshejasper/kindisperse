simgraph_data <- function(nsims = 10, dsigma = 50, dims = 250, labls = TRUE, steps = TRUE,
                          moves = TRUE, shadows = TRUE, category = "2C", show_area = TRUE,
                          centred = FALSE, pinwheel = FALSE, scattered = FALSE,
                          lengths = TRUE, lengthlabs = TRUE, histogram = FALSE,
                          binwidth = dsigma / 5, freqpoly = FALSE){

  if (! category %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
    stop("Invalid Kinship Category")
  }

  nsims <- nsims
  dsigma <- dsigma
  dims <- dims
  if (pinwheel == TRUE | scattered == TRUE) {centred <- TRUE}
  if (centred == TRUE){
    show_area <- FALSE
    f0x <- stats::runif(nsims, 0, 0)
    f0y <- stats::runif(nsims, 0, 0)
  }
  if (centred == FALSE){
    f0x <- stats::runif(nsims, 0, dims)
    f0y <- stats::runif(nsims, 0, dims)
    # sample = 10...
  }
  f1ax <- stats::rnorm(nsims, 0, dsigma) + f0x
  f1ay <- stats::rnorm(nsims, 0, dsigma) + f0y
  f1bx <- stats::rnorm(nsims, 0, dsigma) + f0x
  f1by <- stats::rnorm(nsims, 0, dsigma) + f0y
  f1cx <- stats::rnorm(nsims, 0, dsigma) + f0x
  f1cy <- stats::rnorm(nsims, 0, dsigma) + f0y

  f2ax <- stats::rnorm(nsims, 0, dsigma) + f1ax
  f2ay <- stats::rnorm(nsims, 0, dsigma) + f1ay
  f2bx <- stats::rnorm(nsims, 0, dsigma) + f1bx
  f2by <- stats::rnorm(nsims, 0, dsigma) + f1by
  f3ax <- stats::rnorm(nsims, 0, dsigma) + f2ax
  f3ay <- stats::rnorm(nsims, 0, dsigma) + f2ay
  f3bx <- stats::rnorm(nsims, 0, dsigma) + f2bx
  f3by <- stats::rnorm(nsims, 0, dsigma) + f2by


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
  if (pinwheel == TRUE & nsims > 50) { labls <- FALSE}
  kindist <- round(sqrt((k1x - k2x)^2 + (k1y - k2y)^2), digits = 1)
  kinmidx <- (k1x + k2x) / 2
  kinmidy <- (k1y + k2y) / 2


  result <- tibble::tibble(f0x = f0x, f0y = f0y, f1ax = f1ax, f1ay = f1ay,
                   f1bx = f1bx, f1by = f1by, f1cx = f1cx, f1cy = f1cy,
                   f2ax = f2ax, f2ay = f2ay, f2bx = f2bx, f2by = f2by,
                   f3ax = f3ax, f3ay = f3ay, f3bx = f3bx, f3by = f3by,
                   kindist = kindist, kinmidx = kinmidx, kinmidy = kinmidy,
                   k1x = k1x, k1y = k1y, k2x = k2x, k2y = k2y)

  return(result)
}
