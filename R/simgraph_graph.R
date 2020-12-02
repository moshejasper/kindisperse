#' Simple kin dispersal simulation for graphical display. (graphs the pre-existing simulation).
#'
#' @param result      simulation supplied from simgraph_data() function (tibble)
#' @param nsims       number of families to graph
#' @param dsigma      Integer. The axial deviation of the (simple) parent-offspring dispersal kernel governing this simulation.
#' @param dims        Integer. Lays out the length of the sides of a square within which parent individuals are seeded.
#' @param labls       Logical. Displays labels.
#' @param steps       Logical. Whether or not to show any details of dispersal movement
#' @param moves       Logical. Whether or not to show (curved) lines denoting dispersal movement
#' @param shadows     Logical. Whether or not to show (dashed) shadows tracing dispersal movement.
#' @param category    Character. Lists the kin category the simulation is reconstructing. One of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV" (no half-categoris included)
#' @param show_area   Logical. Whether or not to show the parental seed area as defined in dims.
#' @param centred     Logical. Whether or not to centre the coordinates on one individual.
#' @param pinwheel    Logical. Whether the final graph should be of the pinwheel form.
#' @param scattered   Logical. Whether the final graph should be of the scatter form.
#' @param lengths     Logical. Whether or not to show a dashed line connecting the 'focus' kin to illustrate overall distance of dispersal.
#' @param lengthlabs  Logical. Whether to show labels denoting distance of dispersal between focus kin.
#' @param histogram   Logical. Whether the final graph should be of the histogram form.
#' @param binwidth    Numeric. Binwidth for histogram or freqpoly.
#' @param freqpoly    Logical. Whether the final graph should be of the freqpoly form.
#'
#' @return      Returns a ggplot object for graphing.
#' @export
#'
#' @examples
#' @import ggplot2
#' @import tibble
#' @import dplyr
#' @importFrom magrittr %>%
#'
simgraph_graph <- function(result, nsims = 10, dsigma = 50, dims = 250, labls = TRUE, steps = TRUE,
                           moves = TRUE, shadows = TRUE, category = "2C", show_area = TRUE,
                           centred = FALSE, pinwheel = FALSE, scattered = FALSE,
                           lengths = TRUE, lengthlabs = TRUE, histogram = FALSE,
                           binwidth = dsigma / 5, freqpoly = FALSE){

  element_text <- ggplot2::element_text
  element_rect <- ggplot2::element_rect
  unit <- grid::unit
  guide_legend <- ggplot2::guide_legend
  coord_fixed <- ggplot2::coord_fixed
  aes <- ggplot2::aes
  x <- ggplot2::x
  y <- ggplot2::y
  xend <- ggplot2::xend
  yend <- ggplot2::yend


  if (pinwheel == TRUE | scattered == TRUE) {centred <- TRUE}
  if (centred == TRUE){
    show_area <- FALSE
  }
  requireNamespace("ggplot2")
  requireNamespace("ggrepel")
  if (pinwheel == TRUE & nsims > 50) { labls <- FALSE}

  result <- dplyr::slice_head(result, n = nsims)

  # rescale the key relationships within the graph...

  f0x <- result$f0x
  f0y <- result$f0y

  f1ax <- result$f1ax
  f1ay <- result$f1ay
  f1bx <- result$f1bx
  f1by <- result$f1by
  f1cx <- result$f1cx
  f1cy <- result$f1cy

  f2ax <- result$f2ax
  f2ay <- result$f2ay
  f2bx <- result$f2bx
  f2by <- result$f2by
  f3ax <- result$f3ax
  f3ay <- result$f3ay
  f3bx <- result$f3bx
  f3by <- result$f3by

  if (centred == TRUE) {

    f1ax <- f1ax - f0x
    f1ay <- f1ay - f0y
    f1bx <- f1bx - f0x
    f1by <- f1by - f0y
    f1cx <- f1cx - f0x
    f1cy <- f1cy - f0y

    f2ax <- f2ax - f0x
    f2ay <- f2ay - f0y
    f2bx <- f2bx - f0x
    f2by <- f2by - f0y
    f3ax <- f3ax - f0x
    f3ay <- f3ay - f0y
    f3bx <- f3bx - f0x
    f3by <- f3by - f0y

    f0x <- f0x - f0x
    f0y <- f0y - f0y
  }

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

  # Continue with new code!

  arr = grid::arrow(length = unit(0.05, "inches"), type = "closed")

  rectgrid <- tibble::tibble(x = c(0, 0, dims, dims), xend = c(0, dims, dims, 0), y = c(0, dims, dims, 0), yend = c(dims, dims, 0, 0))

  ggp <- ggplot2::ggplot(result[1:nsims,]) + aes(x = f0x, y = f0y, xend = f1ax, yend = f1ay)

  if (histogram == TRUE){

    ggp <- ggplot2::ggplot(result[1:nsims,]) + ggplot2::aes(x = kindist)
    if (freqpoly == FALSE){
      ggp <- ggp + ggplot2::geom_histogram(fill = "white", colour = "black", binwidth = binwidth)}
    if (freqpoly == TRUE){
      ggp <- ggp + ggplot2::geom_freqpoly(colour = "grey10", size = 0.75, binwidth = binwidth)}
    ggp <- ggp + ggplot2::theme_bw() + ggplot2::xlab("Separation (metres)") + ggplot2::ylab("Count") +
      ggplot2::ggtitle("Kin Dispersal Histogram", subtitle = paste0("S=", dsigma, " N=", nsims, " Category: ", category))
    ggp <- ggp + ggplot2::theme(axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       legend.box.background = element_rect(color = "black", linetype = 1, colour = "black"))
    return(ggp)
  }

  if (pinwheel == TRUE){
    ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = k1x, y = k1y, xend = k2x, yend = k2y), colour = "black",
                              linetype = 1, alpha = 0.7)
    if (labls != FALSE & lengthlabs != FALSE){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = k2x, y = k2y, label = paste(kindist, "m")), colour = "black",
                                    size = 3, hjust = 0.5, vjust = 0.5, alpha = 1, box.padding = unit(0.01, "lines"),
                                    label.padding = unit(0.1, "line"), fontface = "bold")
    }
    ggp <- ggp + ggplot2::coord_fixed() + ggplot2::theme_bw() +
      ggplot2::xlab("Metres (x)") + ggplot2::ylab("Metres (y)") +
      ggplot2::ggtitle(paste0("Kin Dispersal: sigma ", dsigma, "m (pinwheel)"), subtitle = paste("Kin Category:", category))

    ggp <- ggp + ggplot2::theme(axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       legend.box.background = element_rect(color = "black", linetype = 1, colour = "black"))

    return(ggp)
  }

  if (scattered == TRUE){
    ggp <- ggp + ggplot2::geom_point(mapping = aes(x = k2x, y = k2y), colour = "black",
                            alpha = 0.7)

    ggp <- ggp + ggplot2::coord_fixed() + ggplot2::theme_bw() +
      ggplot2::xlab("Metres (x)") + ggplot2::ylab("Metres (y)") +
      ggplot2::ggtitle(paste0("Kin Dispersal: sigma ", dsigma, "m (scatter)"), subtitle = paste("Kin Category:", category))

    ggp <- ggp + ggplot2::theme(axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
                       legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                       legend.box.background = element_rect(color = "black", linetype = 1, colour = "black"))

    return(ggp)
  }

  if (show_area == TRUE){
    ggp <- ggp + ggplot2::geom_segment(data = rectgrid, mapping = aes(x = x, y = y, xend = xend, yend = yend), linetype = 1, alpha = 0.5)}

  if (steps == TRUE) {
    #geom_segment(mapping = aes(x = f1bx, y = f1by), colour = "blue", size = 1, alpha = 0.8)+
    #geom_segment(mapping = aes(x = f1cx, y = f1cy), colour = "blue", size = 1, alpha = 0.7)+
    #geom_segment(mapping = aes(x = f1bx, y = f1by, xend = f1cx, yend = f1cy), colour = "blue", size = 1, alpha = 0.7)+
    if (shadows == TRUE){
      ggp <- ggp + ggplot2::geom_segment(colour = "black", alpha = 0.5, linetype = 3)
      if (category %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_segment(mapping = aes(xend = f1bx, yend = f1by), colour = "black", alpha = 0.5, linetype = 3)}
      if (category %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = f1ax, y = f1ay, xend = f2ax, yend = f2ay),
                                  alpha = 0.5, linetype = 3)}
      if (category %in% c("1C", "1C1", "2C")){
        ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = f1bx, y = f1by, xend = f2bx, yend = f2by),
                                  alpha = 0.5, linetype = 3)}
      if (category %in% c("GGG", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = f2ax, y = f2ay, xend = f3ax, yend = f3ay),
                                  alpha = 0.5, linetype = 3)}
      if (category %in% c("2C")){
        ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = f2bx, y = f2by, xend = f3bx, yend = f3by),
                                  linetype = 3, alpha = 0.5)}}

    if (moves == TRUE) {
      ggp <- ggp + ggplot2::geom_curve(mapping = aes(colour = "black"), alpha = 0.7, linetype = 1, arrow = arr)
      if (category %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_curve(mapping = aes(xend = f1bx, yend = f1by, colour = "black"), alpha = 0.7, linetype = 1, arrow = arr)}
      #geom_segment(mapping = aes(xend = f1cx, yend = f1cy), alpha = 0.8, linetype = 1, arrow = arr)+
      if (category %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_curve(mapping = aes(x = f1ax, y = f1ay, xend = f2ax, yend = f2ay, colour = "green4"),
                                alpha = 0.9, arrow = arr, linetype = 1)}
      if (category %in% c("1C", "1C1", "2C")){
        ggp <- ggp + ggplot2::geom_curve(mapping = aes(x = f1bx, y = f1by, xend = f2bx, yend = f2by, colour = "green4"),
                                alpha = 0.9, linetype = 1, arrow = arr)}
      #geom_segment(mapping = aes(x = f2ax, y = f2ay, xend = f2bx, yend = f2by), colour = "red", size = 1) +
      if (category %in% c("GGG", "1C1", "2C", "GAV")){
        ggp <- ggp + ggplot2::geom_curve(mapping = aes(x = f2ax, y = f2ay, xend = f3ax, yend = f3ay, colour = "purple"),
                                arrow = arr, linetype = 1, alpha = 0.9)}
      if (category %in% c("2C")){
        ggp <- ggp + ggplot2::geom_curve(mapping = aes(x = f2bx, y = f2by, xend = f3bx, yend = f3by, colour = "purple"),
                                linetype = 1, alpha = 0.9, arrow = arr)}}

    ggp <- ggp + ggplot2::geom_point(alpha = 1, size = 2)}

  if (lengths == TRUE) {
    if (centred == TRUE) {kls <-  0.75; klt <- 5}
    if (centred == FALSE) {kls <- 0.5; klt <- 2}
    ggp <- ggp + ggplot2::geom_segment(mapping = aes(x = k1x, y = k1y, xend = k2x, yend = k2y), colour = "black", linetype = klt,
                              size = kls)}

  if (labls == TRUE & steps == TRUE){
    if (centred != TRUE){
      ggp <- ggp + ggrepel::geom_label_repel(label = "0",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}
    if (centred == TRUE){
      ggp <- ggp + ggrepel::geom_label_repel(data = result[1,], label = "0",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))
    }
    if (category %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f1ax, y = f1ay), alpha = 0.7, colour = "black", label = "1",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}
    if (category %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f1bx, y = f1by), alpha = 0.7, colour = "black", label = "1",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}
    if (category %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f2ax, y = f2ay), alpha = 0.7, colour = "green4", label = "2",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}
    if (category %in% c("1C", "1C1", "2C")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f2bx, y = f2by), alpha = 0.7, colour = "green4", label = "2",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}
    if (category %in% c("GGG", "1C1", "2C", "GAV")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f3ax, y = f3ay), alpha = 0.7, colour = "purple", label = "3",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.1, "lines"))}
    if (category %in% c("2C")){
      ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = f3bx, y = f3by), alpha = 0.7, colour = "purple", label = "3",
                                    size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines"))}}

  if (lengths == TRUE & lengthlabs == TRUE){
    ggp <- ggp + ggrepel::geom_label_repel(mapping = aes(x = kinmidx, y = kinmidy, label = paste(kindist, "m")), colour = "black",
                                  size = 3, hjust = 0.5, vjust = 0.5, alpha = 1, box.padding = unit(0.01, "lines"),
                                  label.padding = unit(0.1, "line"), fontface = "bold")}
  #geom_point(mapping = aes(x = f1ax, y = f1ay), alpha = 0.7, colour = "blue")+
  #geom_point(mapping = aes(x = f1bx, y = f1by), alpha = 0.7, colour = "blue")+
  #geom_point(mapping = aes(x = f1cx, y = f1cy), alpha = 0.7, colour = "blue")+
  ggp <- ggp + ggplot2::coord_fixed() + ggplot2::theme_bw() +
    ggplot2::xlab("Metres (x)") + ggplot2::ylab("Metres (y)") + ggplot2::ggtitle(paste0("Kin Dispersal: sigma ", dsigma, "m"), subtitle = paste("Kin Category:", category))
  if (steps == TRUE & (labls == TRUE | moves == TRUE)) {
    ggp <- ggp + ggplot2::scale_colour_identity(labels = c("F0 -> F1", "F1 -> F2", "F2 -> F3"), breaks = c("black", "green4", "purple"),
                                       guide = ggplot2::guide_legend(title = "Dispersal Generation", override.aes = list(size = 1)))}

  ggp <- ggp + ggplot2::theme(axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
                     legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                     legend.box.background = element_rect(color = "black", linetype = 1, colour = "black"))

  return(ggp)
}


simgraph_basic <- function(result, nsims = 10, dsigma = 25, dims = 250,
                           labls = F, steps = T, moves = T, shadows = F,
                           show_area = T, category = "2C",
                           lengths = T, lengthlabs = T) {
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, dims = dims, labls = labls, steps = steps, moves = moves, shadows = shadows,
                  show_area = show_area, category = category, lengths = lengths, lengthlabs = lengthlabs))
}

simgraph_centred_graph <- function(result, nsims = 5, dsigma = 25, dims = 250,
                                   labls = F, steps = T, moves = T, shadows = F,
                                   show_area = T, category = "2C",
                                   lengths = T, lengthlabs = F) {
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, dims = dims, labls = labls, steps = steps, moves = moves, shadows = shadows,
                  show_area = show_area, category = category, lengths = lengths, lengthlabs = lengthlabs, centred = T)) }

simgraph_pinwheel <- function(result, nsims = 200, dsigma = 25, labls = F, category = "2C"){
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, labls = labls, category = category, pinwheel = TRUE))
}

simgraph_scatter <- function(result, nsims = 500, dsigma = 25, category = "2C"){
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, category = category, scattered = TRUE))
}

simgraph_indv <- function(result, dsigma = 25, dims = 250, scalefactor = 4, scaled = T,
                          labls = F, steps = T, moves = T, shadows = F,
                          show_area = T, category = "2C",
                          lengths = T, lengthlabs = T) {
  newtitle = paste0(category, " Dispersal: sigma ", dsigma, "m")
  if (scaled == TRUE){
    return(simgraph_graph(result, nsims = 1, dsigma = dsigma, dims = dims, labls = labls, steps = steps, moves = moves, shadows = shadows,
                    show_area = show_area, category = category, lengths = lengths, lengthlabs = lengthlabs, centred = T) +
             ggplot2::coord_fixed(xlim = c(-scalefactor * dsigma, scalefactor * dsigma),
                         ylim = c(-scalefactor * dsigma, scalefactor * dsigma)) + ggplot2::ggtitle(newtitle))}
  return(simgraph_graph(result, nsims = 1, dsigma = dsigma, dims = dims, labls = labls, steps = steps, moves = moves, shadows = shadows,
                  show_area = show_area, category = category, lengths = lengths, lengthlabs = lengthlabs, centred = T) +
           ggplot2::ggtitle(newtitle))}

simgraph_histogram <- function(result, nsims = 500, dsigma = 25, category = "2C", binwidth = dsigma / 3){
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, category = category, histogram = TRUE, binwidth = binwidth))
}

simgraph_freqpoly <- function(result, nsims = 5000, dsigma = 25, category = "2C", binwidth = dsigma / 3){
  return(simgraph_graph(result, nsims = nsims, dsigma = dsigma, category = category, histogram = TRUE, binwidth = binwidth, freqpoly = TRUE))
}
