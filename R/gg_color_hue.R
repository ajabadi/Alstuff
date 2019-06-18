#' Custom color hue
#'
#' Create\code{n} equally spaced hues around the color wheel (ggplot style)
#' @author Al J Abadi, \email{aljalabadi@gmail.com}
#' @title gg colors hue
#' @keywords hue
#'
#' @param n Number of calolrs
#' @return A character vector of \code{n} HEX colors
#' @importFrom grDevices hcl
#' @import ggplot2
#' @export
#' @example ./examples/gg_color_hue_example.R
#' @family plot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
