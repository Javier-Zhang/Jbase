#' Quick Save Images
#'
#' @param dir Dir to save figure.
#' @param name Figure name
#' @param width Figure width
#' @param height Figure height
#' @param save_as Figure format: png, pdf, svg.
#'
#' @return Image in the local folder.
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' save_image(dir = "./", name = "test", width = 5, height = 5, save_as = "pdf")
save_image <- function(dir = "./", name = "Test", width = 5, height = 5, save_as = "pdf") {
  if (save_as == "png") {
    ggsave(file.path(dir, paste0(name, ".png")), width = width, height = height)
  } else if (save_as == "pdf") {
    ggsave(file.path(dir, paste0(name, ".pdf")), width = width, height = height, device = grDevices::cairo_pdf)
  } else if (save_as == "svg") {
    ggsave(file.path(dir, paste0(name, ".svg")), width = width, height = height)
  }
}
