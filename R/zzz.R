#' @encoding UTF-8
#' @title Package Startup Functions
#' @description This script runs when the hassediagram package is loaded.
#' It ensures that the showtext package and Noto Sans font are registered properly to support Unicode fonts.
#' @keywords internal
#' @noRd

.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("showtext", quietly = TRUE)) {
    warning("showtext package is not installed. Unicode fonts may not render correctly.")
    return()
  }

  showtext::showtext_auto()

  # if (!"Noto Sans Math" %in% names(sysfonts::font_families())) {
  #   sysfonts::font_add_google("Noto Sans Math", "noto")
  # }
}



