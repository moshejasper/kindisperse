#' Simple kin dispersal simulation for graphical display. (graphs the pre-existing simulation).
#'
#' @param result      simulation supplied from simgraph_data() function (tibble)
#' @param nsims       number of families to graph
#' @param labls       Logical. Displays labels.
#' @param steps       Logical. Whether or not to show any details of dispersal movement
#' @param moves       Logical. Whether or not to show (curved) lines denoting dispersal movement
#' @param shadows     Logical. Whether or not to show (dashed) shadows tracing dispersal movement.
#' @param kinship    Character. Lists the kin category the simulation is reconstructing. One of "PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV" (no half-categoris included)
#' @param show_area   Logical. Whether or not to show the parental seed area as defined in data$dims
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
#' @family simgraph
#' @examples
#' simdata <- simgraph_data()
#' simgraph_graph(simdata)
simgraph_graph <- function(result, nsims = 10, labls = TRUE, steps = TRUE,
                           moves = TRUE, shadows = TRUE, kinship = NULL, show_area = TRUE,
                           centred = FALSE, pinwheel = FALSE, scattered = FALSE,
                           lengths = TRUE, lengthlabs = TRUE, histogram = FALSE,
                           binwidth = posigma / 5, freqpoly = FALSE) {

  posigma <- result[1,]$posigma
  dims <- result[1,]$dims
  if (is.null(kinship))
    kinship <- result[1,]$kinship

  if (nsims > nrow(result)){
    message(paste0("Too few families in datasource; reducing nsims to datasource maximum (", nrow(result), ")."))
    nsims <- nrow(result)
  }
  if (pinwheel == TRUE | scattered == TRUE) {
    centred <- TRUE
  }
  if (centred == TRUE) {
    show_area <- FALSE
  }

  if (pinwheel == TRUE & nsims > 50) {
    labls <- FALSE
  }

  result <- slice_head(result, n = nsims)

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

  if (pinwheel == TRUE | scattered == TRUE) {
    k1x <- k1x - k1x
    k1y <- k1y - k1y
    k2x <- k2x - k1x
    k2y <- k2y - k1y
  }
  if (pinwheel == TRUE & nsims > 50) {
    labls <- FALSE
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
    k1x = k1x, k1y = k1y, k2x = k2x, k2y = k2y
  )

  arr <- arrow(length = unit(0.05, "inches"), type = "closed")

  rectgrid <- tibble(x = c(0, 0, dims, dims), xend = c(0, dims, dims, 0), y = c(0, dims, dims, 0), yend = c(dims, dims, 0, 0))

  ggp <- ggplot(result[1:nsims, ]) +
    aes(x = .data$f0x, y = .data$f0y, xend = .data$f1ax, yend = .data$f1ay)

  if (histogram == TRUE) {
    ggp <- ggplot(result[1:nsims, ]) +
      aes(x = .data$kindist)
    if (freqpoly == FALSE) {
      ggp <- ggp + geom_histogram(fill = "white", colour = "black", binwidth = binwidth)
    }
    if (freqpoly == TRUE) {
      ggp <- ggp + geom_freqpoly(colour = "grey10", size = 0.75, binwidth = binwidth)
    }
    ggp <- ggp + theme_bw() + xlab("Separation (metres)") + ylab("Count") +
      ggtitle("Kin Dispersal Histogram", subtitle = paste0("S=", posigma, " N=", nsims, " Category: ", kinship))
    ggp <- ggp + theme(
      axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
      legend.title = element_text(size = 14), legend.text = element_text(size = 12),
      legend.box.background = element_rect(color = "black", linetype = 1, colour = "black")
    )
    return(ggp)
  }

  if (pinwheel == TRUE) {
    ggp <- ggp + geom_segment(
      mapping = aes(x = .data$k1x, y = .data$k1y, xend = .data$k2x, yend = .data$k2y), colour = "black",
      linetype = 1, alpha = 0.7
    )
    if (labls != FALSE & lengthlabs != FALSE) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$k2x, y = .data$k2y, label = paste(.data$kindist, "m")), colour = "black",
        size = 3, hjust = 0.5, vjust = 0.5, alpha = 1, box.padding = unit(0.01, "lines"),
        label.padding = unit(0.1, "line"), fontface = "bold"
      )
    }
    ggp <- ggp + coord_fixed() + theme_bw() +
      xlab("Metres (x)") + ylab("Metres (y)") +
      ggtitle(paste0("Kin Dispersal: sigma ", posigma, "m (pinwheel)"), subtitle = paste("Kin Category:", kinship))

    ggp <- ggp + theme(
      axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
      legend.title = element_text(size = 14), legend.text = element_text(size = 12),
      legend.box.background = element_rect(color = "black", linetype = 1, colour = "black")
    )

    return(ggp)
  }

  if (scattered == TRUE) {
    ggp <- ggp + geom_point(
      mapping = aes(x = .data$k2x, y = .data$k2y), colour = "black",
      alpha = 0.7
    )

    ggp <- ggp + coord_fixed() + theme_bw() +
      xlab("Metres (x)") + ylab("Metres (y)") +
      ggtitle(paste0("Kin Dispersal: sigma ", posigma, "m (scatter)"), subtitle = paste("Kin Category:", kinship))

    ggp <- ggp + theme(
      axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
      legend.title = element_text(size = 14), legend.text = element_text(size = 12),
      legend.box.background = element_rect(color = "black", linetype = 1, colour = "black")
    )

    return(ggp)
  }

  if (show_area == TRUE) {
    ggp <- ggp + geom_segment(data = rectgrid, mapping = aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend), linetype = 1, alpha = 0.5)
  }

  if (steps == TRUE) {
    # geom_segment(mapping = aes(x = f1bx, y = f1by), colour = "blue", size = 1, alpha = 0.8)+
    # geom_segment(mapping = aes(x = f1cx, y = f1cy), colour = "blue", size = 1, alpha = 0.7)+
    # geom_segment(mapping = aes(x = f1bx, y = f1by, xend = f1cx, yend = f1cy), colour = "blue", size = 1, alpha = 0.7)+
    if (shadows == TRUE) {
      ggp <- ggp + geom_segment(colour = "black", alpha = 0.5, linetype = 3)
      if (kinship %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_segment(mapping = aes(xend = .data$f1bx, yend = .data$f1by), colour = "black", alpha = 0.5, linetype = 3)
      }
      if (kinship %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_segment(
          mapping = aes(x = .data$f1ax, y = .data$f1ay, xend = .data$f2ax, yend = .data$f2ay),
          alpha = 0.5, linetype = 3
        )
      }
      if (kinship %in% c("1C", "1C1", "2C")) {
        ggp <- ggp + geom_segment(
          mapping = aes(x = .data$f1bx, y = .data$f1by, xend = .data$f2bx, yend = .data$f2by),
          alpha = 0.5, linetype = 3
        )
      }
      if (kinship %in% c("GGG", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_segment(
          mapping = aes(x = .data$f2ax, y = .data$f2ay, xend = .data$f3ax, yend = .data$f3ay),
          alpha = 0.5, linetype = 3
        )
      }
      if (kinship %in% c("2C")) {
        ggp <- ggp + geom_segment(
          mapping = aes(x = .data$f2bx, y = .data$f2by, xend = .data$f3bx, yend = .data$f3by),
          linetype = 3, alpha = 0.5
        )
      }
    }

    if (moves == TRUE) {
      ggp <- ggp + geom_curve(mapping = aes(colour = "black"), alpha = 0.7, linetype = 1, arrow = arr)
      if (kinship %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_curve(mapping = aes(xend = .data$f1bx, yend = .data$f1by, colour = "black"), alpha = 0.7, linetype = 1, arrow = arr)
      }
      # geom_segment(mapping = aes(xend = f1cx, yend = f1cy), alpha = 0.8, linetype = 1, arrow = arr)+
      if (kinship %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_curve(
          mapping = aes(x = .data$f1ax, y = .data$f1ay, xend = .data$f2ax, yend = .data$f2ay, colour = "green4"),
          alpha = 0.9, arrow = arr, linetype = 1
        )
      }
      if (kinship %in% c("1C", "1C1", "2C")) {
        ggp <- ggp + geom_curve(
          mapping = aes(x = .data$f1bx, y = .data$f1by, xend = .data$f2bx, yend = .data$f2by, colour = "green4"),
          alpha = 0.9, linetype = 1, arrow = arr
        )
      }
      # geom_segment(mapping = aes(x = f2ax, y = f2ay, xend = f2bx, yend = f2by), colour = "red", size = 1) +
      if (kinship %in% c("GGG", "1C1", "2C", "GAV")) {
        ggp <- ggp + geom_curve(
          mapping = aes(x = .data$f2ax, y = .data$f2ay, xend = .data$f3ax, yend = .data$f3ay, colour = "purple"),
          arrow = arr, linetype = 1, alpha = 0.9
        )
      }
      if (kinship %in% c("2C")) {
        ggp <- ggp + geom_curve(
          mapping = aes(x = .data$f2bx, y = .data$f2by, xend = .data$f3bx, yend = .data$f3by, colour = "purple"),
          linetype = 1, alpha = 0.9, arrow = arr
        )
      }
    }

    ggp <- ggp + geom_point(alpha = 1, size = 2)
  }

  if (lengths == TRUE) {
    if (centred == TRUE) {
      kls <- 0.75
      klt <- 5
    }
    if (centred == FALSE) {
      kls <- 0.5
      klt <- 2
    }
    ggp <- ggp + geom_segment(
      mapping = aes(x = .data$k1x, y = .data$k1y, xend = .data$k2x, yend = .data$k2y), colour = "black", linetype = klt,
      size = kls
    )
  }

  if (labls == TRUE & steps == TRUE) {
    if (centred != TRUE) {
      ggp <- ggp + geom_label_repel(
        label = "0",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (centred == TRUE) {
      ggp <- ggp + geom_label_repel(
        data = result[1, ], label = "0",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (kinship %in% c("PO", "FS", "HS", "AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f1ax, y = .data$f1ay), alpha = 0.7, colour = "black", label = "1",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (kinship %in% c("FS", "HS", "AV", "HAV", "1C", "1C1", "2C", "GAV")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f1bx, y = .data$f1by), alpha = 0.7, colour = "black", label = "1",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (kinship %in% c("AV", "GG", "HAV", "GGG", "1C", "1C1", "2C", "GAV")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f2ax, y = .data$f2ay), alpha = 0.7, colour = "green4", label = "2",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (kinship %in% c("1C", "1C1", "2C")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f2bx, y = .data$f2by), alpha = 0.7, colour = "green4", label = "2",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
    if (kinship %in% c("GGG", "1C1", "2C", "GAV")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f3ax, y = .data$f3ay), alpha = 0.7, colour = "purple", label = "3",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.1, "lines")
      )
    }
    if (kinship %in% c("2C")) {
      ggp <- ggp + geom_label_repel(
        mapping = aes(x = .data$f3bx, y = .data$f3by), alpha = 0.7, colour = "purple", label = "3",
        size = 2, label.padding = unit(0.1, "lines"), box.padding = unit(0.01, "lines")
      )
    }
  }

  if (lengths == TRUE & lengthlabs == TRUE) {
    ggp <- ggp + geom_label_repel(
      mapping = aes(x = .data$kinmidx, y = .data$kinmidy, label = paste(.data$kindist, "m")), colour = "black",
      size = 3, hjust = 0.5, vjust = 0.5, alpha = 1, box.padding = unit(0.01, "lines"),
      label.padding = unit(0.1, "line"), fontface = "bold"
    )
  }
  # geom_point(mapping = aes(x = f1ax, y = f1ay), alpha = 0.7, colour = "blue")+
  # geom_point(mapping = aes(x = f1bx, y = f1by), alpha = 0.7, colour = "blue")+
  # geom_point(mapping = aes(x = f1cx, y = f1cy), alpha = 0.7, colour = "blue")+
  ggp <- ggp + coord_fixed() + theme_bw() +
    xlab("Metres (x)") + ylab("Metres (y)") + ggtitle(paste0("Kin Dispersal: sigma ", posigma, "m"), subtitle = paste("Kin Category:", kinship))
  if (steps == TRUE & (labls == TRUE | moves == TRUE)) {
    ggp <- ggp + scale_colour_identity(
      labels = c("F0 -> F1", "F1 -> F2", "F2 -> F3"), breaks = c("black", "green4", "purple"),
      guide = guide_legend(title = "Dispersal Generation", override.aes = list(size = 1))
    )
  }

  ggp <- ggp + theme(
    axis.title = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20),
    legend.title = element_text(size = 14), legend.text = element_text(size = 12),
    legend.box.background = element_rect(color = "black", linetype = 1, colour = "black")
  )

  return(ggp)
}


simgraph_basic <- function(result, nsims = 10, labls = F, steps = T,
                           moves = T, shadows = F, show_area = T, kinship = "2C",
                           lengths = T, lengthlabs = T) {
  return(simgraph_graph(result,
    nsims = nsims, labls = labls, steps = steps, moves = moves, shadows = shadows,
    show_area = show_area, kinship = kinship, lengths = lengths, lengthlabs = lengthlabs
  ))
}

simgraph_centred_graph <- function(result, nsims = 5,
                                   labls = F, steps = T, moves = T, shadows = F,
                                   show_area = T, kinship = "2C",
                                   lengths = T, lengthlabs = F) {
  return(simgraph_graph(result,
    nsims = nsims, labls = labls, steps = steps, moves = moves, shadows = shadows,
    show_area = show_area, kinship = kinship, lengths = lengths, lengthlabs = lengthlabs, centred = T
  ))
}

simgraph_pinwheel <- function(result, nsims = 25, labls = T, kinship = "2C") {
  return(simgraph_graph(result, nsims = nsims, labls = labls, kinship = kinship, pinwheel = TRUE))
}

simgraph_scatter <- function(result, nsims = 500, kinship = "2C") {
  return(simgraph_graph(result, nsims = nsims, kinship = kinship, scattered = TRUE))
}

simgraph_indv <- function(result, scalefactor = 4, scaled = T,
                          labls = F, steps = T, moves = T, shadows = F,
                          show_area = T, kinship = "2C",
                          lengths = T, lengthlabs = T) {
  posigma <- result[1,]$posigma
  newtitle <- paste0(kinship, " Dispersal")
  newsubtitle <- paste0("Sigma: ", posigma, "m")
  if (scaled == TRUE) {
    return(simgraph_graph(result,
      nsims = 1, labls = labls, steps = steps, moves = moves, shadows = shadows,
      show_area = show_area, kinship = kinship, lengths = lengths, lengthlabs = lengthlabs, centred = T
    ) +
      coord_fixed(
        xlim = c(-scalefactor * posigma, scalefactor * posigma),
        ylim = c(-scalefactor * posigma, scalefactor * posigma)
      ) + ggtitle(newtitle, newsubtitle))
  }
  return(simgraph_graph(result,
    nsims = 1, labls = labls, steps = steps, moves = moves, shadows = shadows,
    show_area = show_area, kinship = kinship, lengths = lengths, lengthlabs = lengthlabs, centred = T
  ) +
    ggtitle(newtitle, newsubtitle))
}

simgraph_histogram <- function(result, nsims = 500, kinship = "2C", binwidth = posigma / 3) {
  return(simgraph_graph(result, nsims = nsims, kinship = kinship, histogram = TRUE, binwidth = binwidth))
}

simgraph_freqpoly <- function(result, nsims = 5000, kinship = "2C", binwidth = posigma / 3) {
  return(simgraph_graph(result, nsims = nsims, kinship = kinship, histogram = TRUE, binwidth = binwidth, freqpoly = TRUE))
}
