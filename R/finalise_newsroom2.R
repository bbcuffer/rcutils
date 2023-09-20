#' Adjust a graphic for fish-eye cameras
#'
#' Squeezes the right-hand-side of an image to make it look
#' normal on telly when filmed from a fish-eye camera.
#'
#' You need to have Magick installed for this to work
#' (the software, not the R package)
#'
#' @param infile the png you want to squeeze
#' @param outfile where you want to save the output
#' @param across how many pixels you want to shove it to the right
#' @param squeeze how many pixels you want to squeeze the RHS in by. Half
#'   will be taken off the top and half off the bottom.
#' @param split how much of the `squeeze` pixels are taken at the top. Default
#'   makes for a 50:50 split. Defined in the range 0-1.
#' @param bg the background colour you want to use to fill in any blank space
#'   created.
#' @export
#'
#' @examples
#' \dontrun{
#' ## shift everything 100 pixels left, take 100 off the top
#' ## and 100 off the bottom.
#' skew("file1.png", "file2.png")
#' ## shift everything 100 pixels left, take 150 off the top
#' ## and 50 off the bottom.
#' skew("file1.png", "file2.png", split = .75)
#' ## shift everything 200 pixels left, take 75 off the top and 75
#' ## off the bottom.
#' skew("file1.png", "file2.png", across = 200, squeeze = 150)
#' }
skew <- function(infile, outfile, across = 100, squeeze = 200, split = 0.5,
                 bg = "#f5f5f5") {
  ## Aiming for something like
  ## convert anim01_0001.png -fill none -stroke "#f5f5f5" -strokewidth 10 -draw "rectangle 0,0 1920,1080" -virtual-pixel Background -background "#f5f5f5" -distort Perspective '0,0,200,0 0,1080,200,1080 1920,0,1920,200 1920,1080,1920,880' anim02_skew.png # nolint: line_length_linter.

  tr <- paste(0, 0, across, 0, sep = ",")
  br <- paste(0, 1080, across, 1080, sep = ",")
  tl <- paste(1920, 0, 1920, squeeze * split, sep = ",")
  bl <- paste(1920, 1080, 1920, 1080 - squeeze * (1 - split), sep = ",")

  new_dims <- paste(tr, br, tl, bl)

  args <- paste0(
    infile,
    ## to get rid of the border around ggplot pngs
    ' -fill none -stroke "', bg,
    '" -strokewidth 10 -draw "rectangle 0,0, 1920,1080" ',
    ## to do the skewing
    '-virtual-pixel Background -background "', bg, '" ',
    '-distort Perspective "', new_dims, '" ',
    outfile
  )

  system2("convert", args)
}

#' Finalise a chart for newsroom camera 2
#'
#' Tweaks the margins and adjusts for fisheye distortion to suit
#' newsroom camera 2. The parameters have been set by trial
#' and error
#'
#' @inheritParams bbtv::finalise_tv
#' @param ... other arguments passed through to [finalise_tv()]
#'
#' @export
#'
#' @examples
#'
#' library(bbplot2)
#' library(ggplot2)
#'
#' plot <- ggplot(uk_gdp, aes(x = date, y = value)) +
#'   geom_line(colour = tvcols["news"], lineend = "round", linewidth = 1.2) +
#'   labs(
#'     title = "Quarterly UK GDP",
#'     caption = "Source: Office for National Statistics"
#'   ) +
#'   plasma_style() +
#'   scale_y_continuous(
#'     label = scales::label_dollar(
#'       prefix = "£",
#'       scale_cut = c(" " = 0, "bn" = 1e3, "tn" = 1e6)
#'     )
#'   ) +
#'   annotate("label", lubridate::ymd("1980-01-01"),
#'     450000,
#'     label = "The eighties"
#'   )
#'
#' finalise_newsroom2(plot, "fisheye_plot.png")
#'
#' ## delete files with base::unlink()
#' unlink("fisheye_plot.png")
finalise_newsroom2 <- function(plot, save_filepath, ...) {
  plot <- plot +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 54, r = 250, b = 54, l = 54) * bbtv::.tvpx2pt
    )

  fn1 <- tempfile(fileext = ".png")

  bbtv::finalise_tv(plot, save_filepath = fn1, ...)

  skew(
    infile = fn1, outfile = safe_fn(save_filepath),
    across = 240, squeeze = 270, split = 0.4444
  )
}
