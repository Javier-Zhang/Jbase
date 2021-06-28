#' Quick Save Images
#'
#' @param dir Dir to save figure.
#' @param name Figure name
#' @param width Figure width
#' @param height Figure height
#' @param save_as Figure format: png, pdf, svg.
#'
#' @return
#' @export
#'
#' @examples
#'
save_image <- function(dir = "./", name = "Test", width = 5, height = 5, save_as = "pdf") {
  if (save_as == 'png') {
    ggsave(file.path(fig_dir, paste0(fname, '.png')), width = width, height = height)
  } else if (save_as == 'pdf') {
    ggsave(file.path(fig_dir, paste0(fname, '.pdf')), width = width, height = height, device = grDevices::cairo_pdf)
  } else if (save_as == 'svg') {
    ggsave(file.path(fig_dir, paste0(fname, '.svg')), width = width, height = height)
  }
}
