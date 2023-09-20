#' Check whether a layout fits in tik-tok safe spaces
#'
#' Takes your plot, saves it out at 1080x1920px,
#' overlays a tiktok view of buttons etc. on top of that
#' and then previews the resultant image.
#'
#' @param plot a ggplot object, fully styled and ready to go.
#' @return (Invisibly) the pathname of the generated png.
#' @export
#' @examples
#'
#' library(ggplot2)
#'
#' plot <- ggplot(uk_gdp, aes(x = date, y = value)) +
#'   geom_line(colour = tvcols["news"], lineend = "round") +
#'   labs(
#'     title = "Quarterly UK GDP",
#'     caption = "Source: Office for National Statistics"
#'   ) +
#'   portrait_style(
#'     plot_margins = c(252, 240, 400, 80),
#'     bottom = 250
#'   ) +
#'   scale_y_continuous(
#'     label = scales::label_dollar(
#'       prefix = "£",
#'       scale_cut = c(" " = 0, "bn" = 1e3, "tn" = 1e6)
#'     )
#'   )
#'
#' \dontrun{
#'
#' tiktok_preview(plot)
#'
#' tiktok_preview(plot + portrait_style(plot_margins = c(150, 20, 20, 20)))
#' }
tiktok_preview <- function(plot) {
  fn1 <- tempfile(fileext = ".png")
  fn2 <- tempfile(fileext = ".png")
  ggplot2::ggsave(fn1, plot, device = ragg::agg_png, width = 3.6, height = 6.4, dpi = 300)
  new_image <- magick::image_read(fn1)
  tiktok_safe_area <-
    ## tiktok_safe_area taken from
    ## https://rayhollister.com/tiktok-safe-area-templates/
    system.file("extdata", "tiktok_safe_area.png", package = "bbtv") |>
    safe_fn() |>
    magick::image_read()
  c(new_image, tiktok_safe_area) |>
    magick::image_flatten() |>
    magick::image_write(path = fn2)
  utils::browseURL(fn2)
  invisible(fn2)
}
