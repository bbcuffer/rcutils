#' Length of the Trues
#'
#' @param x a logical vector
#' @return the proportion of \code{x} that is T
#' @export
lt <- function(x) sum(x)/length(x)

#' A point between two others
#' @param x The points between which the function should find a value
#' @param p How far between the two points
#' @param y Alternative way to specify the second point we should look between
#' @return a single value, 100*p% of the way between x[1] and x[2] or between x and y.
#' @export
between <- function(x, p, y=NULL){
   if(length(x)==2) x[1] + p*diff(x)
   else x + p*(y-x)
}

#' A smaller range
#' @param rng The original range
#' @param x How much to shrink it by
#' @return A new, smaller range
#' @export
shrink <- function(rng, x)
   c( sum( c(1-x/2, x/2)*rng ), sum(c(x/2, 1-x/2)*rng) )


#' show the current repo on github
#' @export
ghme <- function(){
  .url <- function(x) system2("open", x)

  file.path(here::here(), ".git/config") |>
    readLines() |>
    stringr::str_subset("url") |>
    stringr::str_replace("\\\turl = ", "") |>
    utils::browseURL()
}

#' Write an xls sheet to xlsx so you can open it with tidyxl
#' @param fn the filename of a .xls spreadsheet
#' @param sheet the worksheet to be xlsx_cells'ed. 
#' @export
tidyxls_sheet <- function(fn, sheet){
  tmp_fn <- tempfile(fileext=".xlsx")
  tmp <- readxl::read_excel(fn, sheet=sheet)
  writexl::write_xlsx(tmp, tmp_fn)
  tidyxl::xlsx_cells(tmp_fn)
}

#' A plot preview window that shows if your title fits
#'
#' Plotting windows that match default dimensions for BBC outputs
#' @param form template for the sizing of the output
#' @param width,height,title finer control over sizing/title for plot window.
#'   In inches. To convert from pixel size, divide by 72. 
#'
#' @examples
#'
#' plot_window("online")
#' plot_window("tv")
#' plot_window("online", height=800/72)
#' 
#' @export
plot_window <- function(form=c("online", "tv", "portrait"),
                        width=NULL, height=NULL, title=NULL){
  form <- match.arg(form)

  widths <- c(online = 640/72, tv=6.4, portrait=3.6)
  heights <- c(online = 450/72, tv=3.6, portrait=6.4)

  the_height <- heights[form]
  the_width <- widths[form]

  if(!is.null(width)) the_width <- width
  if(!is.null(height)) the_height <- height
  the_title <- ifelse(is.null(title), form, title)


  dev.new(width=the_width, height=the_height,
    noRStudioGD=TRUE, title=the_title)
}


                        
