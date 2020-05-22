#' Color Picker
#'
#' This function returns a palette of colors optimized for the requested number of colors
#'
#' @param n the number of colors needed for the palette
#' @param palette string name of the palette to draw colors from, "Jalama", "Refugio", "Rincon", "Naples", or "El Capitan"
#'
#' @return This function returns a \code{vector} of hex color codes
#'
#' @author Bjorn Kallerud
#'
#' @export
color_pickr <- function(n, palette = "Jalama") {

  if (palette == "Jalama") {

    if (n == 1 ) {p = "#006175"}
    if (n == 2 ) {p = c("#006175", "#E3B505")}
    if (n == 3 ) {p = c("#006175", "#DB504A", "#E3B505")}
    if (n == 4 ) {p = c("#006175", "#E3B505", "#FF934F", "#A9CBB7")}
    if (n == 5 ) {p = c("#006175", "#FF934F", "#DB504A", "#A3BCF9", "#E3B505")}
    if (n == 6 ) {p = c("#006175", "#FF934F", "#EFECCA", "#E3B505", "#A9CBB7", "#AA78A6")}
    if (n == 7 ) {p = c("#006175", "#DB504A", "#E3B505", "#FF934F", "#EFECCA", "#A9CBB7", "#A3BCF9")}
    if (n == 8 ) {p = c("#006175", "#DB504A", "#E3B505", "#FF934F", "#EFECCA", "#A9CBB7", "#A3BCF9", "#7D6D61")}
    if (n == 9 ) {p = c("#006175", "#DB504A", "#E3B505", "#EFECCA", "#A9CBB7", "#A3BCF9", "#7D6D61", "#157145",  "#AA78A6")}
    if (n == 10) {p = c("#006175", "#DB504A", "#E3B505", "#FF934F", "#EFECCA", "#A9CBB7", "#A3BCF9", "#7D6D61", "#157145",  "#AA78A6")}

  }

  if (palette == "Naples") {

    if (n == 1 ) {p = "#D25C37"}
    if (n == 2 ) {p = c("#D25C37", "#4D4E5E")}
    if (n == 3 ) {p = c("#D25C37", "#4D4E5E", "#BACF4A")}
    if (n == 4 ) {p = c("#D25C37", "#4D4E5E", "#BACF4A", "#326985")}
    if (n == 5 ) {p = c("#D25C37", "#4D4E5E", "#BACF4A", "#326985", "#C8A66A")}

  }

  if (palette == "Refugio") {

    if (n == 1 ) {p = "#e8ae61"}
    if (n == 2 ) {p = c("#e8ae61", "#cCC7B9")}
    if (n == 3 ) {p = c("#e8ae61", "#cCC7B9", "#653239")}
    if (n == 4 ) {p = c("#e8ae61", "#cCC7B9", "#653239", "#6A7186")}
    if (n == 5 ) {p = c("#e8ae61", "#cCC7B9", "#AF7A6D", "#6A7186", "#AA8F66")}
    if (n == 6 ) {p = c("#e8ae61", "#cCC7B9", "#AF7A6D", "#6A7186", "#AA8F66", "#AF7A6D" )}

  }

  if (palette == "Rincon") {
    if (n == 1 ) {p = "#029084"}
    if (n == 2 ) {p = c("#029084", "#6D6E73")}
    if (n == 3 ) {p = c("#029084", "#6D6E73", "#e4a438")}
    if (n == 4 ) {p = c("#029084", "#6D6E73", "#e4a438", "#a0ab66")}
    if (n == 5 ) {p = c("#029084", "#6D6E73", "#e4a438", "#a0ab66", "#785539")}
    if (n == 6 ) {p = c("#029084", "#6D6E73", "#e4a438", "#a0ab66", "#785539", "#c3b090")}
    if (n == 7 ) {p = c("#029084", "#6D6E73", "#e4a438", "#a0ab66", "#785539", "#c3b090", "#e05625")}

  }

  if (palette == "El Capitan") {
    if (n == 1 ) {p = "#4E8085"}
    if (n == 2 ) {p = c("#759CA0", "#4E8085")}
    if (n == 3 ) {p = c("#759CA0", "#4E8085", "#27646B")}
    if (n == 4 ) {p = c("#9CB8BB", "#759CA0", "#4E8085", "#27646B")}
    if (n == 5 ) {p = c("#9CB8BB", "#759CA0", "#4E8085", "#27646B", "#205258")}
    if (n == 6 ) {p = c("#C4D4D6", "#9CB8BB", "#759CA0", "#4E8085", "#27646B", "#205258")}
    if (n == 7 ) {p = c("#C4D4D6", "#9CB8BB", "#759CA0", "#4E8085", "#27646B", "#205258", "#194045")}
    if (n == 8 ) {p = c("#EBF0F1", "#C4D4D6", "#9CB8BB", "#759CA0", "#4E8085", "#27646B", "#205258", "#194045")}
    if (n == 9 ) {p = c("#EBF0F1", "#C4D4D6", "#9CB8BB", "#759CA0", "#4E8085", "#27646B", "#205258", "#194045", "#122E31")}
  }

  if ((palette == "Jalama" & n > 10) | (palette == "Refugio" & n > 6) |(palette == "Rincon" & n > 7) |(palette == "El Capitan" & n > 9) |(palette == "Naples" & n > 5)) {
    stop("This palette does not support this many color options.")
  }

  return(p)

}
