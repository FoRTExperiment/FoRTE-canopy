#' Complete list of palettes
#'
#' Use \code{\link{np_palette}} to construct palettes of desired length.
#'
#' @export
forte_colors <- list(
  arches = c("#3B1105", "#FC7500", "#ADB5BE", "#5487C8", "#FFD707"),
  forte1 = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
  
  forte2 = c("#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  
  forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")
)

#' A National Parks of the United States palette generator
#'
#' These are a handful of color palettes pulled from photographs of US National Parks.
#'
#' @param n Number of colors desired. Unfortunately most palettes now only
#'   have 5 colors. These palettes are picked using color.adobe.com
#'   from photographs provided by J.W. Atkins, E. Agee, A. R. Woleslagle.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{badlands}, \code{deathvalley}, \code{smokies},
#'   \code{picturedrocks}, \code{lakesuperior}, \code{tallgrass},
#'   \code{rockymtn}, \code{flatirons}, \code{shenandoah},
#'   \code{shenandoah2}, \code{kingscanyon}, \code{kingscanyon2},
#'   \code{smokies2}, \code{smokies3}, \code{arches},
#'   \code{bryce}, \code{grandtetons}, \code{conagree},
#'   \code{zion}, \code{yellowstone}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' np_palette("badlands")
#' np_palette("deathvalley")
#' np_palette("deathvalley", 3)
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- np_palette(21, name = "badlands", type = "continuous")
#' image(volcano, col = pal)
forte_color <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- forte_colors[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

#' heatmap
#'
#' A heatmap example
"heatmap"