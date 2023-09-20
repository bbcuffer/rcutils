#' Work out breaks for an axis of decimal dates
#'
#' gganimate can't zoom across dates.
#' So, if zooming, a chart will need to be based on decimal dates
#' (e.g. 2022.01, 2022.7).
#'
#' That causes problems for picking sensible dates for breaks
#' (e.g. why prefer Jan 1 over Jan 6 when they're decimals?)
#' This function calculates those breaks.
#'
#' The break width changes depends on the width of the data. Rather than using
#' R's pretty algorithm, it manually specifies whether it's month, year etc.
#'
#'
#' @param x the range over which the dates should be calculated
#' @export
ddate_breaks <- function(x) {
  the_range <- diff(x) # nolint: object_usage_linter.
  the_dates <- lubridate::date_decimal(x) |> lubridate::as_date()
  spaces <- dplyr::case_when(
    the_range > 5 ~ "2 years",
    the_range > 1 ~ "1 year",
    the_range > 0.5 ~ "3 months",
    TRUE ~ "1 month"
  )
  date_breaks <- scales::fullseq(the_dates, spaces) + lubridate::minutes(2)
  decimal_breaks <- lubridate::decimal_date(date_breaks)
  return(decimal_breaks)
}

#' Add a duplicate final row
#'
#' Takes the final row (ie with the highest value of `reveal_index`) of the
#' dataframe `df`, adds +1 to `reveal_index` and then binds that duplicate
#' final row onto the bottom of `df`.
#'
#'
#' @param df the dataframe
#' @param reveal_index the name of the variable being used
#' @export
add_reveal_row <- function(df, reveal_index) {
  the_row <- dplyr::slice_max(df, {{ reveal_index }}) |>
    dplyr::mutate({{ reveal_index }} := {{ reveal_index }} + 1)
  dplyr::bind_rows(df, the_row)
}
#' Duplicate dataset with zero values for one variable
#'
#' This function duplicates a dataset, using a variable `state` to
#' distinguish the two copies:
#' the original version has `state=1`, the copy has `state=0`.
#'
#' The copy also sets `value = 0` throughout.
#' Useful in conjunction with `transition_state()`.
#' Eg for a bar plot, state = 1, the actual values, where an animation ends.
#' state = 0, all plotted values set to 0, where an animation starts.
#'
#' @param df the dataframe
#' @param value the variable that will be reset as zero in the copy dataset.
#' @importFrom rlang :=
#' @export
#'
add_zero_state <- function(df, value) {
  extra_df <- df |>
    dplyr::mutate({{ value }} := 0)
  dplyr::bind_rows("0" = extra_df, "1" = df, .id = "state")
}
#' Thin a vector
#'
#' Take every (say) fifth element from a vector (like a list of
#' filenames, keeping the first and final ones.
#'
#' If you've got 200 frames for a video and want to thin it to around 40,
#' then use this.
#' @param x the vector to be thinned
#' @param jump_size pick every `jump_size`th element
#' @param n pick (roughly) length elements, ie set `jumps = length(x) / n.`
#' @param neg Return the elements picked (default behaviour) or
#' the complement (ie all the other elements).
#' @export
#' @examples
#' thin(1:19, jump = 5)
#' thin(1:20, n = 6)
thin <- function(x, jump_size = NULL, n = NULL, neg = FALSE) {
  l <- length(x)

  if (is.null(jump_size) && is.null(n)) {
    stop("Specify either jump_size or n")
  } else if (is.null(jump_size)) {
    jump_size <- round(l / n, digits = 0)
  }

  bottom <- seq(1, l / 2, jump_size) ## count halfway up the sequence
  top <- seq(l, l / 2, -jump_size) ## count halfway down the sequence
  full_sequence <- c(bottom, top) ## combine them

  ## If you end up with a big gap between the bottom sequence
  ## and the top...
  ## for example, if you're thinning 1:19 in jumps of 5,
  ## bottom is 1, 6 and top is 14, 19. So...
  if (min(top) - max(bottom) > jump_size) {
    ## ...add in a value halfway between the two
    full_sequence <- c(
      full_sequence,
      round((min(top) + max(bottom)) / 2, digits = 0)
    )
  }

  full_sequence <- sort(full_sequence)

  if (neg) {
    x[-full_sequence] ## If neg, strip out the selected entries
  } else {
    x[full_sequence] ## Otherwise (the default) return the selected entries
  }
}
