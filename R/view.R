#' Look at a dataframe using a spreadsheet package. Mac only.
#'
#' You can't edit the dataframe - it just writes the dataframe to a tempfile which it then opens. 
#'
#' @param df the data frame
#' @param app the program used to show it. Defaults to Apple's Numbers. Excel also an option.
#'
#' @return nothing, just opens a spreadsheet showing the dataframe. 
#'
#' @examples
#'
#' demo <- tibble(alpha=letters, num=rnorm(26))
#' views(demo)
#' views(demo, "Excel")
#' views(demo, "E")
#' @export
views <- function(df, app=c("Numbers", "Excel")){
  app01 <- match.arg(app)
  app02 <- gsub("Excel", "Microsoft Excel", app01)
  fn <- tempfile(fileext=".csv")
  write_csv(df, fn)
  arg <- paste0('-a "', app02, '" ', fn)
  system2("open", arg)
}
