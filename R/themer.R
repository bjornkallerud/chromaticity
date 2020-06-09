#' Theme for ggplot2
#'
#' This function automates and standardizes a ggplot2 theme
#'
#' @param font_size adjust plot font size
#' @param box determines whether or not there is a box around the plot
#' @param flipped set to TRUE if you are using coord_flip
#'
#' @return This function returns a \code{vector} of hex color codes
#'
#' @author Bjorn Kallerud
#'
#' @export
themer <- function(font_size = 12, box = T, flipped = F) {

  thm <- theme(
    text = element_text(size = font_size),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(hjust = 0.1),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.spacing = unit(1.0,"cm"),
    legend.key.size = unit(1.5,"lines"),
    plot.margin=grid::unit(c(10, 10, 5, 5), "mm"),
    panel.grid.major.y = if (flipped == F) {element_line(size = 0.25, color="grey70")} else {element_blank()},
    panel.grid.minor.y = if (flipped == F) {element_line(size = 0.25, color="grey70")} else {element_blank()},
    panel.grid.major.x = if (flipped == F) {element_blank()} else {element_line(size = 0.25, color="grey70")},
    panel.grid.minor.x = if (flipped == F) {element_blank()} else {element_line(size = 0.25, color="grey70")},
    panel.background = element_blank(),
    plot.background = element_rect(size = 1, colour = ifelse(box == T, "grey50", rgb(1, 0, 0, alpha = 0))),
    strip.background = element_blank())

  return(thm)

}
