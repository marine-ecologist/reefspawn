#' Generate Species Color Palette
#'
#' Returns a named vector of hex color codes for coral species, with an option
#' to lighten the palette for alternate display modes (e.g., backgrounds or emphasis).
#'
#' @param type Character. Type of palette to return. `"base"` for the default palette,
#' or any other value to return a paler variant.
#'
#' @return A named character vector where names are species names and values are hex color codes.
#'
#' @examples
#' sp_pal()                        # Returns base palette
#' sp_pal("pale")                 # Returns lighter palette
#' barplot(rep(1, 11), col = sp_pal())  # Visualize palette
#'
#' @export
sp_pal <- function(type="base") {

  if(type=="base"){
    sp_pal <- c(
      "Acropora hyacinthus" = "#50676c",
      "Acropora cytherea" = "#3a6c8e",
      "Acropora intermedia" = "#2c3687",
      "Acropora robusta" = "#7b8ca8",
      "Acropora spathulata" = "#98a062",
      "Acropora millepora" = "#665a43",
      "Acropora nasuta" = "#48642f",
      "Acropora humilis" = "#824b3b",
      "Acropora cf. digitifera" = "#a47c73",
      "Goniastrea pectinata" = "#999999",
      "Goniastrea retiformis" = "#bfbfbf"
    )
  } else {
    make_paler <- function(color) {
      grDevices::adjustcolor(color, alpha.f = 1, red.f = 1.2, green.f = 1.2, blue.f = 1.2)
    }

    sp_pal <- sapply(sp_pal("base"), make_paler)
  }

  return(sp_pal)
}
