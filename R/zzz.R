#' @encoding UTF-8
#' @title Package Startup Functions
#' @description This script runs when the hassediagram package is loaded.
#' It ensures that the showtext package and Noto Sans font are registered properly to support Unicode fonts.
#' @keywords internal
#' @noRd
#' 


.onLoad <- function(libname, pkgname) {
  font_path <- system.file("fonts", "NotoSansMath-Regular.ttf", package = "hassediagram")
  
  if (file.exists(font_path)) {
    sysfonts::font_add("noto", font_path)
    showtext::showtext_auto()
  } else {
    warning("Noto Sans Math font is missing. Please install manually with sysfonts::font_add_google().")
  }
}


# .onLoad <- function(libname, pkgname) {
#   font_path <- system.file("fonts/NotoSansMath-Regular.ttf", package = "hassediagram")
#   if (file.exists(font_path)) {
#     sysfonts::font_add("noto", font_path)
#   } else {
#     warning("Noto Sans Math font is missing. Install manually with sysfonts::font_add_google().")
#   }
# }


# .onLoad <- function(libname, pkgname) {
#   if (!requireNamespace("showtext", quietly = TRUE)) {
#     warning("showtext package is not installed. Unicode fonts may not render correctly.")
#     return()
#   }
#   
#   showtext::showtext_auto()
#   
#   if (!"Noto Sans Math" %in% names(sysfonts::font_families())) {
#     sysfonts::font_add_google("Noto Sans Math", "noto")
#   }
# }



